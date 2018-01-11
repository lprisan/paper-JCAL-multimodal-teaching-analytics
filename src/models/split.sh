IFS=$'\n'       # make newlines the only separator
COUNTER=0
PERFILE=30
INTERPRETER="/homes/kidzik/anaconda2/bin/Rscript"
INTERPRETER="/homes/kidzik/anaconda2/bin/python"

for line in $(cat $1)    
do
    JOBID=$(($COUNTER / ${PERFILE}))
    if (( $COUNTER % ${PERFILE} == 0 ))           
    then
	echo "#PBS -l select=1:ncpus=1" >> jobs/job$JOBID.sh
	echo "#PBS -l walltime=60:00:00" >> jobs/job$JOBID.sh
    fi
    echo "$INTERPRETER /homes/kidzik/paper-JLA-deep-teaching-analytics/src/models/$line" >> jobs/job$JOBID.sh
    let COUNTER=COUNTER+1
done
