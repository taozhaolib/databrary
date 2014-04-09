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

#ifdef FascistCheckUser // doesn't actually work, but you get the idea. need 2.9
	const char *r = FascistCheckUser(passwd, NULL, user, name);
#else // currently have 2.8 on production
	const char *r = FascistCheck(passwd, NULL);
#endif
	jstring jr = r ? (*env)->NewStringUTF(env, r) : NULL;

	if (name)
		(*env)->ReleaseStringUTFChars(env, jname, name);
	(*env)->ReleaseStringUTFChars(env, juser, user);
	(*env)->ReleaseStringUTFChars(env, jpasswd, passwd);

	return jr;
}
