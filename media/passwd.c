#include <jni.h>
#include <crack.h>

JNIEXPORT jobject JNICALL
Java_media_Passwd_00024__1check(
		JNIEnv *env,
		jobject this,
		jstring jpasswd,
		jstring juser,
		jstring jname)
{
	const char *passwd = (*env)->GetStringUTFChars(env, jpasswd, 0);
	const char *user = (*env)->GetStringUTFChars(env, juser, 0);
	const char *name = jname ? (*env)->GetStringUTFChars(env, jname, 0) : NULL;

	const char *r = FascistCheckUser(passwd, NULL, user, name);
	jstring jr = r ? (*env)->NewStringUTF(env, r) : NULL;

	if (name)
		(*env)->ReleaseStringUTFChars(env, jname, name);
	(*env)->ReleaseStringUTFChars(env, juser, user);
	(*env)->ReleaseStringUTFChars(env, jpasswd, passwd);

	return jr;
}
