#!/bin/bash -e
# Control interface for transcode jobs.
# This is run directly from the application, on the webserver.
# It calls tools/transcode on transcode.host, which must be in the path.

cmd=`dirname $0`/transcode

if [[ ! -f $cmd ]] ; then
	echo "$cmd: not found" >&2
	exit 2
fi

while getopts 'i:h:d:v:c:k:f:r:' opt ; do case "$opt" in
	i) id=$OPTARG ;;
	h) host=$OPTARG ;;
	d) dir=$OPTARG ;;
	v) version=$OPTARG ;;

	c) collect=$OPTARG ;;
	k) kill=$OPTARG ;;
	f) src=$OPTARG ;;
	r) url=$OPTARG ;;

	?) exit 1 ;;
esac ; done

if [[ -z $id || -z $dir || -z $collect$kill && ( -z $src || -z $url ) ]] ; then
	echo "$0: usage error: $*" >&2
	exit 1
fi

if [[ -n $host ]] ; then
	hcmd=`basename "$cmd"`
	if [[ -n $version ]] ; then
		hcmd=$hcmd-$version
		rsync "$cmd" "$host:$hcmd"
	fi
	cmd=./$hcmd
fi

if [[ -n $collect ]] ; then
	if [[ -n $host ]] ; then
		rsync "$host:$dir/$id.mp4" "$collect"
		ssh "$host" rm -f "$dir/$id" "$dir/$id.mp4"
	else
		mv "$dir/$id.mp4" "$collect"
		rm -f "$dir/$id"
	fi
elif [[ -n $host ]] ; then
	if [[ -z $kill ]] ; then
		rsync "$src" "$host:$dir/$id"
	fi
	ssh "$host" "$cmd" "$@" | sed 's/^\([0-9]\+\)\.[.a-z0-9-]*$/\1/'
elif [[ -n $kill ]] ; then
	"$cmd" "$@"
else
	ln -sfT "$src" "$dir/$id"
	"$cmd" "$@" &
	echo $!
fi
