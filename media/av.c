#include <jni.h>
#include <libavformat/avformat.h>

#define PKG	"media/AV$"
static struct construct {
	jclass class;
	jmethodID init;
} String, Error, Probe;

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

JNIEXPORT jint JNICALL
JNI_OnLoad(JavaVM *jvm, void *reserved)
{
	JNIEnv *env;

	if ((*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_6) != 0)
		return -1;

#define CONSTRUCT_INIT(NAME, TYPE) \
	if (construct_init(env, &NAME, PKG #NAME, TYPE) < 0) \
		return -1

	if (construct_init(env, &String, "java/lang/String", NULL) < 0)
		return -1;
	CONSTRUCT_INIT(Error, "(Ljava/lang/String;I)V");
	CONSTRUCT_INIT(Probe, "(Ljava/lang/String;D[Ljava/lang/String;)V");

	av_register_all();

	return JNI_VERSION_1_6;
}

JNIEXPORT void JNICALL
JNI_OnUnload(JavaVM *jvm, void *reserved)
{
	JNIEnv *env;
	(*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_6);
	construct_fini(env, &String);
	construct_fini(env, &Error);
	construct_fini(env, &Probe);
}

static void throw(JNIEnv *env, int r, const char *fmt, ...) __attribute__((format(printf, 3, 4)));
static void throw(JNIEnv *env, int r, const char *fmt, ...)
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
	(*env)->Throw(env, CONSTRUCT(Error, jstr, r));
}

#define CHECK(F, MSG, ARGS...) ({ \
		int _r = (F); \
		if (_r < 0) { \
			throw(env, _r, "%s: " MSG, infile, ##ARGS); \
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

	CHECK(avformat_open_input(&in, infile, NULL, NULL), "opening");
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
		jstring jinfile,
		jdouble offset,
		jstring joutfile)
{
	const char *infile = (*env)->GetStringUTFChars(env, jinfile, 0);
	const char *outfile = joutfile ? (*env)->GetStringUTFChars(env, joutfile, 0) : NULL;
	AVFormatContext *in = NULL, *out = NULL;
	static AVDictionary *opts = NULL;
	AVCodec *codec = NULL;
	AVStream *is = NULL, *os = NULL;
	AVPacket pkt;
	AVFrame *frame = NULL;
	int i;
	jbyteArray jimg = NULL;

	av_init_packet(&pkt);

	CHECK(avformat_open_input(&in, infile, NULL, NULL), "opening");
	CHECK(avformat_find_stream_info(in, NULL), "reading stream info");

	int si = CHECK(av_find_best_stream(in, AVMEDIA_TYPE_VIDEO, -1, -1, &codec, 0), "finding video stream");
	is = in->streams[si];
	for (i = 0; i < in->nb_streams; i ++)
		if (i != si)
			in->streams[i]->discard = AVDISCARD_ALL;

	av_dict_set(&opts, "threads", "1", 0);
	CHECK(avcodec_open2(is->codec, codec, &opts), "opening input codec %s", codec->name);

	int64_t off = offset*is->time_base.den/is->time_base.num;
	CHECK(avformat_seek_file(in, 0, INT64_MIN, off, off, 0), "seeking to %ld", off);

	frame = av_frame_alloc();
	int gpp = 0;
	do {
		av_frame_unref(frame);
		CHECK(av_read_frame(in, &pkt), "reading frame");
		CHECK(avcodec_decode_video2(is->codec, frame, &gpp, &pkt), "decoding video");
		frame->pts = av_frame_get_best_effort_timestamp(frame);
		av_free_packet(&pkt);
	} while (!gpp || frame->pts < off);

	CHECK(avformat_alloc_output_context2(&out, NULL, outfile ? "image2" : "image2pipe", outfile), "opening '%s'", outfile);
	if (!outfile)
		CHECK(avio_open_dyn_buf(&out->pb), "opening buffer");

	if (!((codec = avcodec_find_encoder(AV_CODEC_ID_MJPEG)) &&
	      (os = avformat_new_stream(out, codec)))) {
		throw(env, 0, "cannot find JPEG codec or create output stream");
		goto error;
	}
	os->codec->time_base = is->codec->time_base;
	os->codec->width = frame->width;
	os->codec->height = frame->height;
	/* These are too new and should also use av_frame_get_color* instead:
	os->codec->colorspace = frame->colorspace;
	os->codec->color_range = frame->color_range;
	*/
	os->codec->pix_fmt = CHECK(avcodec_find_best_pix_fmt_of_list(codec->pix_fmts, frame->format, 0, NULL), "finding pixel format");

	if (os->codec->pix_fmt != frame->format)
	{
		/* This will only be necessary to support certain video formats */
		throw(env, 0, "pixel format conversion not supported");
		goto error;

		/*
		AVPicture pict;
		CHECK(avpicture_alloc(&pict, os->codec->pix_fmt, os->codec->width, os->codec->height));
		struct SwsContext *sws = sws_getCachedContext(NULL,
				frame->width, frame->height, frame->format,
				os->codec->width, os->codec->height, os->codec->pix_fmt,
				SWS_POINT, NULL, NULL, NULL);
		int h = sws_scale(sws, frame->data, frame->linesize, 0, frame->height, pict.data, pict.linesize);
		sws_freeContext(sws);
		*/
	}

	CHECK(avformat_write_header(out, NULL), "writing header to '%s'", outfile);
	av_dict_set(&opts, "threads", "1", 0);
	CHECK(avcodec_open2(os->codec, codec, &opts), "opening output codec %s", codec->name);
	CHECK(avcodec_encode_video2(os->codec, &pkt, frame, &gpp), "encoding frame");
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
