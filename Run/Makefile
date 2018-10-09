#------------------------------------------------------------------------------
#                  GEOS-Chem Global Chemical Transport Model                  !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: Makefile
#
# !DESCRIPTION: Utility Makefile for the GCHP run directory. It is stored
#  in the Unit Tester with name 'Makefile_GCHP' but is copied to the 
#  GCHP run directory as 'Makefile'.
#\\
#\\
# !REMARKS:
# 
# !REVISION HISTORY: 
#  Navigate to your unit tester directory and type 'gitk' at the prompt
#  to browse the revision history.
#EOP
#------------------------------------------------------------------------------
#BOC

# Unix shell (we'll assume Bash, which is on every Linux system)
SHELL        :=/bin/bash

###############################################################################
#####                                                                     #####
#####   CONFIGURABLE TOKENS: You can modify these for your environment.   #####
#####                                                                     #####
###############################################################################

# MPI implementation (openmpi or mvapich2) and number of cores. Currently 
# you can only run interactively using the Makefile using one node.
# NUM_THREADS must be a multiple of six.
MPI=mvapich2
NUM_THREADS=6

###############################################################################
#####                                                                     #####
#####  DEFAULT OPTIONS
#####                                                                     #####
###############################################################################

# Source code location
ifndef GC_DIR
 CODEDIR_GC :=$(shell readlink -f ./CodeDir)
endif

# GCHP code directory path
CODEDIR_GCHP :=$(CODEDIR_GC)/GCHP

# Run directory path
RUN_DIR :=$(shell pwd)

# Log files that will be written to the log directory
BUILD_LOG  :="$(RUN_DIR)/compile.log"
GCHP_LOG   :="$(RUN_DIR)/GCHP.log"
LAST_BUILD :="$(RUN_DIR)/lastbuild"

# Executables
EXE :=geos

###############################################################################
#####                                                                     #####
#####                          MAKEFILE TARGETS                           #####
#####                                                                     #####
###############################################################################

# PHONY targets don't actually compile anything. They either are
# synonyms for other targets, they remove files, or they print output.
.PHONY: cleanup_data      cleanup_logs   cleanup_output  cleanup_exe
.PHONY: clean_gc          clean_mapl     clean_all       clean_nuclear
.PHONY: compile_standard  compile_mapl   compile_debug   compile_clean
.PHONY: run_interactive   superclean

#%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Build Only             %
#%%%%%%%%%%%%%%%%%%%%%%%%%%

# Beware! Destroys ESMF, MAPL, and FVdycore compilations
compile_clean:
	date > $(BUILD_LOG)
	./build.sh compile_clean 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

compile_standard:
	date >> $(BUILD_LOG)
	./build.sh compile_standard 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

compile_debug:
	date >> ${BUILD_LOG}
	./build.sh compile_debug 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

compile_mapl:
	date >> ${BUILD_LOG}
	./build.sh compile_mapl 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

#%%%%%%%%%%%%%%%%%%%%%%%
#  Run Interactively   %
#%%%%%%%%%%%%%%%%%%%%%%%

run_interactive:
	if [[ $(MPI) == openmpi ]]; then mpirun -n $(NUM_THREADS) ./$(EXE) 2>&1 | tee $(GCHP_LOG); fi;
	if [[ $(MPI) == mvapich2 ]]; then srun -n $(NUM_THREADS) --mpi=pmi2 ./$(EXE) 2>&1 | tee $(GCHP_LOG); fi;

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Clean up, Build, and Run Interactively  %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Beware! Destroys ESMF, MAPL, and FVdycore compilations
gchp_clean:
	@$(MAKE) cleanup_output
	@$(MAKE) compile_clean
	@$(MAKE) run_interactive

gchp_standard:
	@$(MAKE) cleanup_output
	@$(MAKE) compile_standard
	@$(MAKE) run_interactive

gchp_debug:
	@$(MAKE) cleanup_output
	@$(MAKE) compile_debug
	@$(MAKE) run_interactive

gchp_mapl:
	@$(MAKE) cleanup_output
	@$(MAKE) compile_mapl
	@$(MAKE) run_interactive

#%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Clean up Run Directory  %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%

cleanup_output: cleanup_data cleanup_logs

cleanup_data: 
	rm -f $(RUN_DIR)/OutputDir/*.nc4
	rm -f trac_avg.*
	rm -f tracerinfo.dat
	rm -f diaginfo.dat
	rm -f cap_restart
	rm -f gcchem*
	rm -f *.rcx
	rm -f *~

cleanup_logs: 
	rm -f gchp.log
	rm -f HEMCO.log
	rm -f PET*.log
	rm -f runConfig*log
	rm -f multirun.log
	rm -f logfile.000000.out
	rm -f slurm-*
	rm -f 1
	rm -f EGRESS

cleanup_exe: 
	rm -f geos
	rm -f $(BUILD_LOG)
	rm -f $(LAST_BUILD)

#%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Clean Source Code      %
#   (BE CAREFUL!!!)       %
#%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_all:
	./build.sh clean_all

clean_gc:
	./build.sh clean_gc

clean_mapl:
	./build.sh clean_mapl

clean_nuclear:
	./build.sh clean_nuclear

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Delete and kill Everything  %
#   (BE CAREFUL!!!)            %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

superclean:
	@$(MAKE) cleanup_output
	@$(MAKE) cleanup_exe
	@$(MAKE) clean_all

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Print information                      %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printbuildinfo:
	$(eval CODE_BRANCH_GC :=$(shell git -C $(CODEDIR_GC) rev-parse --abbrev-ref HEAD))
	$(eval LAST_COMMIT_GC :=$(shell git -C $(CODEDIR_GC) log -n 1 --pretty=format:"%s")) 
	$(eval COMMIT_DATE_GC :=$(shell git -C $(CODEDIR_GC) log -n 1 --pretty=format:"%cd")) 
	$(eval COMMIT_USER_GC :=$(shell git -C $(CODEDIR_GC) log -n 1 --pretty=format:"%cn")) 
	$(eval COMMIT_HASH_GC :=$(shell git -C $(CODEDIR_GC) log -n 1 --pretty=format:"%h")) 
	$(eval CODE_STATUS_GC :=$(shell git -C $(CODEDIR_GC) status -s)) 
	$(eval CODE_BRANCH_GCHP:=$(shell git -C $(CODEDIR_GCHP) rev-parse --abbrev-ref HEAD))
	$(eval LAST_COMMIT_GCHP:=$(shell git -C $(CODEDIR_GCHP) log -n 1 --pretty=format:"%s"))
	$(eval COMMIT_DATE_GCHP:=$(shell git -C $(CODEDIR_GCHP) log -n 1 --pretty=format:"%cd"))
	$(eval COMMIT_USER_GCHP:=$(shell git -C $(CODEDIR_GCHP) log -n 1 --pretty=format:"%cn"))
	$(eval COMMIT_HASH_GCHP:=$(shell git -C $(CODEDIR_GCHP) log -n 1 --pretty=format:"%h")) 
	$(eval CODE_STATUS_GCHP:=$(shell git -C $(CODEDIR_GCHP) status -s)) 
	@echo "GEOS-Chem repository"
	@echo "   Path        : $(CODEDIR_GC)"
	@echo "   Branch      : $(CODE_BRANCH_GC)"
	@echo "   Last commit : $(LAST_COMMIT_GC)"
	@echo "   Date        : $(COMMIT_DATE_GC)"
	@echo "   User        : $(COMMIT_USER_GC)"
	@echo "   Hash        : $(COMMIT_HASH_GC)"
	@echo "   Git status  : $(CODE_STATUS_GC)"
	@echo "GCHP repository"
	@echo "   Path        : $(CODEDIR_GCHP)"
	@echo "   Branch      : $(CODE_BRANCH_GCHP)"
	@echo "   Last commit : $(LAST_COMMIT_GCHP)"
	@echo "   Date        : $(COMMIT_DATE_GCHP)"
	@echo "   User        : $(COMMIT_USER_GCHP)"
	@echo "   Hash        : $(COMMIT_HASH_GCHP)"
	@echo "   Git status  : $(CODE_STATUS_GCHP)"

help:
	@echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
	@echo '%%%    GCHP Run Directory Makefile Options    %%%'
	@echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
	@echo ' '
	@echo 'Usage: make TARGET [OPTIONAL-FLAGS]'
	@echo ' '
	@echo 'Default threads for compiling is hard-coded as 6'
	@echo ' '
	@echo '----------------------------------------------------------'
	@echo 'TARGET may be one of the following:'
	@echo '----------------------------------------------------------'
	@echo ' '      
	@echo 'To print settings to screen:' 
	@echo '  printbuildinfo   Print out git info for GC and GCHP repos'
	@echo '  help             If you got this far, you know this one'
	@echo ' '
	@echo 'To clean source code:'
	@echo '  clean_all        Clean all source code using build.sh'
	@echo '  clean_gc         Clean GEOS-Chem classic only using build.sh'
	@echo '  clean_mapl       Clean MAPL and FVdyCore using build.sh'
	@echo '  clean_nuclear    Clean everything using build.sh'
	@echo '  superclean       Remove exe, logs, output, and clean ALL code'
	@echo ' '
	@echo 'To compile (appropriate clean included):'
	@echo '  compile_clean     Include everything'
	@echo '  compile_standard  Skip MAPL, FvDyCore, and ESMF; debug off'
	@echo '  compile_debug     Skip MAPL, FvDyCore, and ESMF; debug on'
	@echo '  compile_mapl      Skip ESMF'
	@echo ' '
	@echo 'To remove run directory files:' 
	@echo '  cleanup_output   Remove output data, log files, and executable'
	@echo '  cleanup_data     Remove output data' 
	@echo '  cleanup_logs     Remove log files' 
	@echo '  cleanup_exe      Remove executable'
	@echo ' ' 
	@echo 'To run interactively:'
	@echo '  run_interactive'
	@echo ' '
	@echo 'To clean, build, and run interactively:'
	@echo '  gchp_clean       Include everything'
	@echo '  gchp_standard    Skip MAPL, FvDyCore, and ESMF; debug off'
	@echo '  gchp_debug       Skip MAPL, FvDyCore, and ESMF; debug on'
	@echo '  gchp_mapl        Skip ESMF'
	@echo ' '
#EOC


