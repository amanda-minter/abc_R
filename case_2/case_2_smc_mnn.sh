#!/bin/bash
#$ -N case_2_smc_mnn
#$ -cwd
#$ -V
#$ -P fraser.prjc
#$ -q jeeves.q,gromit.q,short.qc
#$ -t 1-20

module load R/3.4.3

# Print time stamp of when IBM is started
printf '%-17.17s%-20.20s\n' 'DATE_START' "$(date +"%d/%m/%y %T")"

Rscript case_2_ABC_SMC_MNN.R

# Save end time stamp
printf '%-17.17s%-20.20s\n' 'DATE_END' "$(date +"%d/%m/%y %T")"
