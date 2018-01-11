FILES=`qstat -H | grep kidzik | grep "F 24:00" | cut -d' ' -f10 | cut -c4-5`
echo $FILES
for FILE in $FILES
do
    cp jobs-tmp/job$((FILE * 3 - 2)).sh jobs/
    cp jobs-tmp/job$((FILE * 3 - 1)).sh jobs/
    cp jobs-tmp/job$((FILE * 3)).sh jobs/
done
