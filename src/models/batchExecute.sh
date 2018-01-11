#!/bin/bash

BASESTRING='Rscript '
COUNT=($(wc -l $1))
echo "Running Rscript in docker...." $COUNT $BASESTRING

#IFS=$'\r\n' GLOBIGNORE='*' command eval  'XYZ=($(cat $1))'

counter=0
while read p; do
  COMMAND=$BASESTRING$p
  echo $counter "Executing" $COMMAND
  eval $COMMAND
  counter=$((counter+1))
done <$1

