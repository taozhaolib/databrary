#!/bin/bash -e
# Control interface for transcode jobs.
# This is run directly from the application, on the webserver.
# It calls tools/transcode on transcode.host, which must be in the path.

cmd=transcode

while getopts 'a:c:d:f:h:k:r:' opt ; do case "$opt" in
	a) aid=$OPTARG ;;
	c) collect=$OPTARG ;;
	d) dir=$OPTARG ;;
	f) src=$OPTARG ;;
	h) host=$OPTARG ;;
	k) kill=$OPTARG ;;
	r) url=$OPTARG ;;
esac ; done

if [[ $# -ge $OPTIND || -z $aid || -z $dir || -z $collect$kill && ( -z $src || -z $url ) ]] ; then
	echo "$0: usage error: $*"
	exit 1
fi

set -- ${aid:+-a "$aid"} ${dir:+-d "$dir"} ${url:+-r "$url"} ${kill:+-k "$kill"}
if [[ -n $collect ]] ; then
	if [[ -n $host ]] ; then
		rsync "$host:$dir/${aid}.mp4" "$collect"
		ssh "$host" rm "$dir/$aid" "$dir/${aid}.mp4"
	else
		mv "$dir/${aid}.mp4" "$collect"
		rm "$dir/$aid"
	fi
elif [[ -n $host ]] ; then
	if [[ -z $kill ]] ; then
		rsync "$src" "$host:$dir/$aid"
	fi
	exec ssh "$host" "$cmd" -h "$@"
elif [[ -n $kill ]] ; then
	"$cmd" "$@"
else
	ln -f $src $dir/$aid
	"$cmd" "$@" &
	echo $!
fi
