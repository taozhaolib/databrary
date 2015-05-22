#! /bin/bash
#Report on the usage of different conf/messages entries
#WARNING: Will not indicate computed strings


if [ $# -eq 0 ]; then
	long=0
else
	long=$1
fi

while read l; do
	i=`expr index "$l" =`
	if [ $i -ne 0 ]; then
		i=$i-1
		p=${l:0:i}
		result=$(grep -r --exclude-dir=target --exclude-dir=conf "$p" | wc -l)
		if [ $long = 0 ]; then
			outl=$p
		else
			outl=$l
		fi
		if [ $result -eq 0 ]; then
			echo -e "\e[0;31m"$outl":" $result
		else
			echo -e "\e[0m"$outl":" $result
		fi
	fi
done < ../conf/messages
