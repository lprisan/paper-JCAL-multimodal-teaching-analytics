#!/bin/bash

BASESTRING='docker run -p 8888:8888 -p 6006:6006 -v /media/sf_shared:/root/sharedfolder lprisan/dl-docker-loadcaffe:cpu /root/sharedfolder/paper-JLA-deep-teaching-analytics/src/models/'
COUNT=($(wc -l $1))
echo "Running python scripts in docker...." $COUNT $BASESTRING

#IFS=$'\r\n' GLOBIGNORE='*' command eval  'XYZ=($(cat $1))'

counter=0
while read p; do
  COMMAND=$BASESTRING$p
  echo $counter "Executing" $COMMAND
  eval $COMMAND
  counter=$((counter+1))
done <$1
