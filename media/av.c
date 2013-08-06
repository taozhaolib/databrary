#include <jni.h>
#include <libavformat/avformat.h>

#define PKG	"media/AV"
static jclass AVError, AVProbe;
static jmethodID AVErrorInit, AVProbeInit;

JNIEXPORT jint JNICALL
JNI_OnLoad(JavaVM *jvm, void *reserved)
{
	JNIEnv *env;
	if (!(((*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_6) == 0)
	   && (AVError = (*env)->FindClass(env, PKG "$AVError"))
	   && (AVProbe = (*env)->FindClass(env, PKG "$AVProbe"))
	   && (AVErrorInit = (*env)->GetMethodID(env, AVError, "<init>", "(Ljava/lang/String;I)V"))
	   && (AVProbeInit = (*env)->GetMethodID(env, AVProbe, "<init>", "(I)V"))
	   ))
		return -1;

	AVError = (*env)->NewGlobalRef(env, AVError);
	AVProbe = (*env)->NewGlobalRef(env, AVProbe);

	av_register_all();

	return JNI_VERSION_1_6;
}

JNIEXPORT void JNICALL
JNI_OnUnload(JavaVM *jvm, void *reserved)
{
	JNIEnv *env;
	(*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION_1_6);
	(*env)->DeleteGlobalRef(env, AVError);
	(*env)->DeleteGlobalRef(env, AVProbe);
}

static void throw(JNIEnv *env, int r)
{
	char msg[AV_ERROR_MAX_STRING_SIZE];
	jstring str = (*env)->NewStringUTF(env,
			av_strerror(r, msg, sizeof(msg)) == 0
			? msg : strerror(AVUNERROR(r)));
	(*env)->Throw(env, (*env)->NewObject(env, AVError, AVErrorInit, str, r));
}

JNIEXPORT jobject JNICALL
Java_media_AV_00024_probe(
		JNIEnv *env,
		jobject this,
		jstring sfilename)
{
	AVFormatContext *fmt_ctx = NULL;

	const char *filename = (*env)->GetStringUTFChars(env, sfilename, 0);
	int r = avformat_open_input(&fmt_ctx, filename, NULL, NULL);
	(*env)->ReleaseStringUTFChars(env, sfilename, filename);
	if (r < 0) {
		throw(env, r);
		return NULL;
	}

	jobject probe = (*env)->NewObject(env, AVProbe, AVProbeInit, fmt_ctx->nb_streams);

	avformat_close_input(&fmt_ctx);
	return probe;
}
