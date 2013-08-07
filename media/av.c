#include <jni.h>
#include <libavformat/avformat.h>

#define PKG	"media/AV$"
static struct construct {
	jclass class;
	jmethodID init;
} Error, Probe;

static inline int construct_init(JNIEnv *env, struct construct *c, const char *name, const char *type)
{
	jclass cl;
	if (!((cl = (*env)->FindClass(env, name))
	   && (c->class = (*env)->NewGlobalRef(env, cl))
	   && (c->init = (*env)->GetMethodID(env, c->class, "<init>", type))))
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

	CONSTRUCT_INIT(Error, "(Ljava/lang/String;I)V");
	CONSTRUCT_INIT(Probe, "(Ljava/lang/String;D)V");

	av_register_all();

	return JNI_VERSION_1_6;
}

JNIEXPORT void JNICALL
JNI_OnUnload(JavaVM *jvm, void *reserved)
{
	JNIEnv *env;
	(*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_6);
	construct_fini(env, &Error);
	construct_fini(env, &Probe);
}

static void throw(JNIEnv *env, int r)
{
	char msg[AV_ERROR_MAX_STRING_SIZE];
	jstring str = (*env)->NewStringUTF(env,
			av_strerror(r, msg, sizeof(msg)) == 0
			? msg : strerror(AVUNERROR(r)));
	(*env)->Throw(env, CONSTRUCT(Error, str, r));
}

JNIEXPORT jobject JNICALL
Java_media_AV_00024_probe(
		JNIEnv *env,
		jobject this,
		jstring sfilename)
{
	AVFormatContext *fmt = NULL;

	const char *filename = (*env)->GetStringUTFChars(env, sfilename, 0);
	int r = avformat_open_input(&fmt, filename, NULL, NULL);
	(*env)->ReleaseStringUTFChars(env, sfilename, filename);
	if (r < 0) {
		throw(env, r);
		return NULL;
	}

	jobject probe = CONSTRUCT(Probe, 
			(*env)->NewStringUTF(env, fmt->iformat->name),
			(double)fmt->duration/(double)AV_TIME_BASE);

	avformat_close_input(&fmt);
	return probe;
}
