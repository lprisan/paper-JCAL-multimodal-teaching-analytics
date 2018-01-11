#!/bin/bash

echo "Running Rscript in docker...."

# In linux
sudo docker run -it -p 8888:8888 -p 6006:6006 -v /media/sf_shared:/root/sharedfolder kaggle/rstats Rscript /root/sharedfolder/paper-JLA-deep-teaching-analytics/src/models/Train_Evaluate_SVMBest.R 'RF_et_GM_LOSO_Activity_1' 'Activity' 'et' 'case1-day1-session2-teacher1,case1-day1-session3-teacher1,case1-day1-session4-teacher1,case2-day1-session1-teacher2,case2-day1-session2-teacher2,case2-day2-session1-teacher2,case2-day2-session2-teacher2,case2-day3-session1-teacher2,case2-day3-session2-teacher2,case2-day4-session1-teacher2,case2-day4-session2-teacher2' 'case1-day1-session1-teacher1'

# Variant for windows
#docker run -it -p 8888:8888 -p 6006:6006 -v C:\Users\Luis\Downloads:/root/sharedfolder kaggle/rstats Rscript /root/sharedfolder/paper-JLA-deep-teaching-analytics/src/models/Train_Evaluate_RF.R 'RF_et_GM_LOSO_Activity_1' 'Activity' 'et' 'case1-day1-session2-teacher1,case1-day1-session3-teacher1,case1-day1-session4-teacher1,case2-day1-session1-teacher2,case2-day1-session2-teacher2,case2-day2-session1-teacher2,case2-day2-session2-teacher2,case2-day3-session1-teacher2,case2-day3-session2-teacher2,case2-day4-session1-teacher2,case2-day4-session2-teacher2' 'case1-day1-session1-teacher1'