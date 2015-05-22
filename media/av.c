#include <jni.h>
#include <pthread.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>

#define PKG	"media/AV$"
static struct construct {
	jclass class;
	jmethodID init;
} Object, String, Error, Probe;
static JNIEnv *Env; /* for lockmgr only */

static inline int construct_init(JNIEnv *env, struct construct *c, const char *name, const char *type)
{
	jclass cl;
	if (!((cl = (*env)->FindClass(env, name))
	   && (c->class = (*env)->NewGlobalRef(env, cl))))
		return -1;
	if (type && !(c->init = (*env)->GetMethodID(env, c->class, "<init>", type)))
		return -1;
	return 0;
}

static inline void construct_fini(JNIEnv *env, struct construct *c)
{
	(*env)->DeleteGlobalRef(env, c->class);
	c->class = NULL;
}

#define CONSTRUCT(NAME, ARGS...) (*env)->NewObject(env, NAME.class, NAME.init, ##ARGS)

static int lockmgr(void **mtx, enum AVLockOp op)
{
#ifdef USE_JAVA_LOCKMGR // seems to be broken
	JNIEnv *env = Env;
	if (!env)
		return -1;
	jobject *obj = (jobject *)mtx;
	switch (op) {
		case AV_LOCK_CREATE: {
			jobject o;
			if (!(o = CONSTRUCT(Object)))
				goto error;
			if (!(*obj = (*env)->NewGlobalRef(env, o)))
				goto error;
			break;
		}
		case AV_LOCK_OBTAIN:
			if ((*env)->MonitorEnter(env, *obj))
				goto error;
			break;
		case AV_LOCK_RELEASE:
			if ((*env)->MonitorExit(env, *obj))
				goto error;
			break;
		case AV_LOCK_DESTROY:
			(*env)->DeleteGlobalRef(env, *obj);
			*obj = NULL;
			break;
	}
#else
	pthread_mutex_t **pmtx = (pthread_mutex_t **)mtx;
	switch (op) {
		case AV_LOCK_CREATE: {
			if (!(*pmtx = malloc(sizeof(pthread_mutex_t))))
				goto error;
			if (pthread_mutex_init(*pmtx, NULL))
				goto error;
			break;
		}
		case AV_LOCK_OBTAIN:
			if (pthread_mutex_lock(*pmtx))
				goto error;
			break;
		case AV_LOCK_RELEASE:
			if (pthread_mutex_unlock(*pmtx))
				goto error;
			break;
		case AV_LOCK_DESTROY:
			if (pthread_mutex_destroy(*pmtx))
				goto error;
			free(*pmtx);
			*pmtx = NULL;
			break;
	}
#endif
	return 0;
error:
	fprintf(stderr, "lockmgr %d failed\n", op);
	return -1;
}

JNIEXPORT jint JNICALL
JNI_OnLoad(JavaVM *jvm, void *reserved)
{
	if ((*jvm)->GetEnv(jvm, (void **)&Env, JNI_VERSION_1_6) != 0)
		return -1;

#define CONSTRUCT_INIT(NAME, TYPE) \
	if (construct_init(Env, &NAME, PKG #NAME, TYPE) < 0) \
		return -1

	if (construct_init(Env, &Object, "java/lang/Object", "()V") < 0)
		return -1;
	if (construct_init(Env, &String, "java/lang/String", NULL) < 0)
		return -1;
	CONSTRUCT_INIT(Error, "(Ljava/lang/String;Ljava/lang/String;I)V");
	CONSTRUCT_INIT(Probe, "(Ljava/lang/String;D[Ljava/lang/String;)V");

#undef CONSTRUCT_INIT

	av_register_all();

	if (av_lockmgr_register(lockmgr))
		return -1;

	return JNI_VERSION_1_6;
}

JNIEXPORT void JNICALL
JNI_OnUnload(JavaVM *jvm, void *reserved)
{
	JNIEnv *env;
	av_lockmgr_register(NULL);
	(*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_6);
	construct_fini(env, &String);
	construct_fini(env, &Error);
	construct_fini(env, &Probe);
	if (Env == env)
		Env = NULL;
}

static void throw(JNIEnv *env, int r, const jstring jfile, const char *fmt, ...) __attribute__((format(printf, 4, 5)));
static void throw(JNIEnv *env, int r, const jstring jfile, const char *fmt, ...)
{
	va_list args;
	char buf[256];
	
	va_start(args, fmt);
	int l = vsnprintf(buf, sizeof(buf), fmt, args);
	va_end(args);

	if (r) {
		if (l < 0)
			l = 0;
		if (l < sizeof(buf)-4) {
			buf[l++] = ':';
			buf[l++] = ' ';
		}
		av_strerror(r, &buf[l], sizeof(buf)-l);
	}

	jstring jstr = (*env)->NewStringUTF(env, buf);
	(*env)->Throw(env, CONSTRUCT(Error, jfile, jstr, r));
}

#define CHECK(F, MSG, ARGS...) ({ \
		int _r = (F); \
		if (_r < 0) { \
			throw(env, _r, jinfile, MSG, ##ARGS); \
			goto error; \
		} \
		_r; \
	})

JNIEXPORT jobject JNICALL
Java_media_AV_00024__1probe(
		JNIEnv *env,
		jobject this,
		jstring jinfile)
{
	const char *infile = (*env)->GetStringUTFChars(env, jinfile, 0);
	AVFormatContext *in = NULL;
	jobject probe = NULL;
	jobjectArray jstreams = NULL;
	int i;

	CHECK(avformat_open_input(&in, infile, NULL, NULL), "opening %s", infile);
	CHECK(avformat_find_stream_info(in, NULL), "reading stream info");

	if (!(jstreams = (*env)->NewObjectArray(env, in->nb_streams, String.class, NULL)))
		goto error;
	for (i = 0; i < in->nb_streams; i ++)
		(*env)->SetObjectArrayElement(env, jstreams, i,
				(*env)->NewStringUTF(env, avcodec_get_name(in->streams[i]->codec->codec_id)));

	probe = CONSTRUCT(Probe, 
			(*env)->NewStringUTF(env, in->iformat->name),
			(double)in->duration/(double)AV_TIME_BASE,
			jstreams);

error:
	if (in)
		avformat_close_input(&in);
	(*env)->ReleaseStringUTFChars(env, jinfile, infile);
	return probe;
}

/* This does the equivalent of:
 *   ffmpeg -loglevel error -accurate_seek -ss $offset -i $infile -f:v image2 -frames:v 1 $outfile
 */
JNIEXPORT jobject JNICALL
Java_media_AV_00024__1frame(
		JNIEnv *env,
		jobject this,
		jstring jinfile, jdouble offset,
		jstring joutfile,
		jint width, jint height)
{
	const char *infile = (*env)->GetStringUTFChars(env, jinfile, 0);
	const char *outfile = joutfile ? (*env)->GetStringUTFChars(env, joutfile, 0) : NULL;
	AVFormatContext *in = NULL, *out = NULL;
	AVDictionary *opts = NULL;
	AVCodec *codec = NULL;
	AVStream *is = NULL, *os = NULL;
	AVPacket pkt;
	AVFrame *frame = NULL;
	int i;
	jbyteArray jimg = NULL;
	int gpp = 0;

	av_init_packet(&pkt);

	CHECK(avformat_open_input(&in, infile, isnan(offset) ? av_find_input_format("image2") : NULL, NULL), "opening %s", infile);
	if (isnan(offset))
		in->video_codec_id = AV_CODEC_ID_MJPEG;
	CHECK(avformat_find_stream_info(in, NULL), "reading stream info");

	int si = CHECK(av_find_best_stream(in, AVMEDIA_TYPE_VIDEO, -1, -1, &codec, 0), "finding video stream");
	is = in->streams[si];
	for (i = 0; i < in->nb_streams; i ++)
		if (i != si)
			in->streams[i]->discard = AVDISCARD_ALL;

	av_dict_set(&opts, "threads", "1", 0);
	CHECK(avcodec_open2(is->codec, codec, &opts), "opening input codec %s", codec->name);

	int64_t off;
	if (isnan(offset)) {
		off = INT64_MIN;
	} else {
		off = offset*is->time_base.den/is->time_base.num;
		CHECK(avformat_seek_file(in, 0, INT64_MIN, off, off, 0), "seeking to %ld", off);
	}

	frame = av_frame_alloc();
	uint64_t pts = 0;
	do {
		av_frame_unref(frame);
		CHECK(av_read_frame(in, &pkt), "reading frame");
		if (pkt.stream_index == si)
		{
			CHECK(avcodec_decode_video2(is->codec, frame, &gpp, &pkt), "decoding video");
			if (gpp)
				pts = frame->pts = av_frame_get_best_effort_timestamp(frame);
		}
		else
			gpp = 0;
		av_free_packet(&pkt);
	} while (!gpp || (int64_t)pts < off);

	CHECK(avformat_alloc_output_context2(&out, NULL, outfile ? "image2" : "image2pipe", outfile), "opening '%s'", outfile);
	if (!outfile)
		CHECK(avio_open_dyn_buf(&out->pb), "opening buffer");

	if (!((codec = avcodec_find_encoder(AV_CODEC_ID_MJPEG)) &&
	      (os = avformat_new_stream(out, codec)))) {
		throw(env, 0, jinfile, "cannot find JPEG codec or create output stream");
		goto error;
	}
	os->codec->time_base = is->codec->time_base;
	os->time_base = is->time_base;
	AVRational sar = av_guess_sample_aspect_ratio(in, is, frame);
	AVRational dsr = av_make_q(frame->width, frame->height);
	if (sar.num)
		dsr = av_mul_q(dsr, sar);
	else
		sar.num = sar.den = 1;
	if (frame->height > (unsigned)height || sar.num > sar.den) {
		os->codec->height = (unsigned)height < frame->height ? height : frame->height;
		os->codec->width = os->codec->height * dsr.num / dsr.den;
	}
	if (!os->codec->width || os->codec->width > (unsigned)width) {
		os->codec->width = (unsigned)width < frame->width ? width : frame->width;
		os->codec->height = os->codec->width * dsr.den / dsr.num;
	}
	os->codec->colorspace = av_frame_get_colorspace(frame);
	os->codec->color_range = av_frame_get_color_range(frame);
	os->codec->pix_fmt = CHECK(avcodec_find_best_pix_fmt_of_list(codec->pix_fmts, frame->format, 0, NULL), "finding pixel format");

	if (os->codec->pix_fmt != frame->format || os->codec->width != frame->width || os->codec->height != frame->height)
	{
		AVFrame tmp;
		memset(&tmp, 0, sizeof(tmp));
		tmp.format = os->codec->pix_fmt;
		tmp.width = os->codec->width;
		tmp.height = os->codec->height;
		av_frame_copy_props(&tmp, frame);
		CHECK(av_frame_get_buffer(&tmp, 32), "allocating conversion buffers");
		struct SwsContext *sws = sws_getContext(
				frame->width, frame->height, frame->format,
				tmp.width, tmp.height, tmp.format,
				SWS_POINT, NULL, NULL, NULL);
		sws_scale(sws, (const uint8_t *const *)frame->data, frame->linesize, 0, frame->height, tmp.data, tmp.linesize);
		sws_freeContext(sws);
		av_frame_unref(frame);
		av_frame_move_ref(frame, &tmp);
	}

	av_dict_set(&opts, "threads", "1", 0);
	CHECK(avcodec_open2(os->codec, codec, &opts), "opening output codec %s", codec->name);
	CHECK(avformat_write_header(out, NULL), "writing header to '%s'", outfile);
	CHECK(avcodec_encode_video2(os->codec, &pkt, frame, &gpp), "encoding frame");
	if (!gpp) {
		throw(env, 0, jinfile, "did not encode JPEG frame");
		goto error;
	}
	CHECK(av_write_frame(out, &pkt), "writing frame to '%s'", outfile);
	CHECK(av_write_trailer(out), "writing trailer to '%s'", outfile);

	if (!outfile)
	{
		uint8_t *buf = NULL;
		int l = avio_close_dyn_buf(out->pb, &buf);
		/* assuming jbyte === uint8_t */
		jbyte *img;
		if (!((jimg = (*env)->NewByteArray(env, l)) &&
		      (img = (*env)->GetByteArrayElements(env, jimg, NULL))))
		{
			jimg = NULL;
			av_free(buf);
			goto error;
		}
		memcpy(img, buf, l);
		(*env)->ReleaseByteArrayElements(env, jimg, img, 0);
		av_free(buf);
	}

error:
	av_dict_free(&opts);
	av_frame_free(&frame);
	av_free_packet(&pkt);
	if (out) {
		if (os && os->codec)
			avcodec_close(os->codec);
		avformat_free_context(out);
	}
	if (in) {
		if (is && is->codec)
			avcodec_close(is->codec);
		avformat_close_input(&in);
	}

	if (joutfile)
		(*env)->ReleaseStringUTFChars(env, joutfile, outfile);
	(*env)->ReleaseStringUTFChars(env, jinfile, infile);
	return jimg;
}
