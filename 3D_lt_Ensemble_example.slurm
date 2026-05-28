#!/bin/bash
#SBATCH --partition=sapphire
#SBATCH --nodes=1
#SBATCH --job-name="3D_lt_Ensemble_example"
#SBATCH --account="punim2365"
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mail-user=XX
#SBATCH --mail-type=FAIL
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --time=4-00:00:00
#SBATCH --array=1-5  # Run an array of 5 jobs

# Load the Intel compiler module
module load intel-compilers

# Define and create a unique directory for each job in the array
JOB_DIR="JOB${SLURM_ARRAY_TASK_ID}"
mkdir -p "$JOB_DIR/Data"

# Copy only the specified files into the job-specific directory
cp par.fi com.fi fft4f2d.f90 mlb_wave3_new.f90 makefile 3D_lt_Ensemble.slurm "$JOB_DIR"

# Navigate to the job-specific directory
cd "$JOB_DIR"

# Compile the program using the Makefile
make -f makefile_mlb

# Ensure the binary is executable
chmod +x mlb_wave3_nw

# Run the program, directing any output to the Data directory within the job-specific directory
./mlb_wave3_nw
##DO NOT ADD/EDIT BEYOND THIS LINE##
my-job-stats -a -n -s