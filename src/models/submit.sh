for line in $(ls jobs)
do
    qsub -v EXPERIMENT=$line ./jobs/$line
done
