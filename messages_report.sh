#! /bin/bash
while read l; do
	i=`expr index "$l" =`
	if [ $i -ne 0 ]; then
		i=$i-1
		p=${l:0:i}
		result=$(grep -r --exclude-dir=target --exclude-dir=conf "$p" | wc -l)
		if [ $result -eq 0 ]; then
			echo -e "\e[0;31m"$p":" $result
		else
			echo -e "\e[0m"$p":" $result
		fi
	fi
done < conf/messages
