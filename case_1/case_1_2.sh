#!/bin/bash
#$ -N case_1_2
#$ -cwd
#$ -V
#$ -P fraser.prjc
#$ -q jeeves.q,gromit.q,short.qc
#$ -t 1-20

module load R/3.4.0-openblas-0.2.18-omp-gcc5.4.0

# Print time stamp of when IBM is started
printf '%-17.17s%-20.20s\n' 'DATE_START' "$(date +"%d/%m/%y %T")"

Rscript ABC_rejection_2.R

# Save end time stamp
printf '%-17.17s%-20.20s\n' 'DATE_END' "$(date +"%d/%m/%y %T")"
