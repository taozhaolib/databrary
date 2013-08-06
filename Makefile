# This is not the makefile.  This is me being lazy until I figure out how to do this within sbt.
JAVA_HOME?=/usr/lib/j2sdk
PKGS=libavformat
media/src/main/resources/libav.so: media/av.c
	gcc -Wall -fPIC -shared -o $@ -I$(JAVA_HOME)/include `pkg-config --cflags --libs $(PKGS)` $<
