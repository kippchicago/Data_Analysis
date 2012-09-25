#!/bin/bash

#shell script that cleans NWEA provided comprehensive data files (CDFs) for loading into a MySQL Database

#USAGE: dataprep foo.csv
#input: foo.csv, which is any CFD csv
#output: foo_loaddata.csv, a csv ready to load into a MySQL data base. 

#partion and save file name into twoparts (for foo.csv, first = foo and rest = .csv)
first=`echo $1 | sed 's:\(.*\)\(\..*\):\1:'`
rest=`echo $1 | sed 's:\(.*\)\(\..*\):\2:'`

#long sed using regexp to remove spaces and slashes from column headers and replaces cells that are empty or contain ", BR, * with \N
sed -E -e 's:,NumSense,:_NumSense_:g' -e 's:,Stat/Prob,:_Stat_Prob_:g' -e 's:,Phonic,:_Phonic_:g'  -e 's:,"",:,\\N,:g' -e 's:"::g' -e 's:BR:\\N:g' -e 's:*:\N:g'  -e '1d'  <$1 >$first'_loaddata'$rest

#messsage upon completion
echo 'Data from '$1' cleaned and saved to '$first'_loaddata'$rest'. BOOYAH. Time Saved! CHANGED?'


