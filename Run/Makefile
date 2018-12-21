#------------------------------------------------------------------------------
#                  GEOS-Chem Global Chemical Transport Model                  !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: Makefile
#
# !DESCRIPTION: Utility Makefile for the GCHP run directory.
#\\
#\\
# !REMARKS:
# 
# !REVISION HISTORY: 
#  See git history in the GCHP repository for version 12.1.0 and beyond.
#  For prior history, see the git history in geos-chem-unittest
#EOP
#------------------------------------------------------------------------------
#BOC

# Unix shell (we'll assume Bash, which is on every Linux system)
SHELL :=/bin/bash

###############################################################################
#####                                                                     #####
#####                              VARIABLES                              #####
#####                                                                     #####
###############################################################################

# Source code location
ifndef CODEDIR_GC
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
#####                              TARGETS                                #####
#####                                                                     #####
###############################################################################

# PHONY targets don't actually compile anything. They either are
# synonyms for other targets, they remove files, or they print output.
.PHONY: cleanup_data      cleanup_logs     cleanup_output  cleanup_exe
.PHONY: clean             clean_mapl       clean_gc        superclean
.PHONY: build             build_debug      rebuild         rebuild_debug
.PHONY: build_mapl        build_mapl_debug rebuild_mapl    rebuild_maple_debug
.PHONY: build_gc          build_gc_debug   rebuild_gc      rebuild_gc_debug
.PHONY: help              printbuildinfo

#Legacy (will be removed in a future version)
.PHONY: compile_clean     compile_standard compile_debug   compile_mapl
.PHONY: make_clean_nuclear

#%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Clean Source Code      %
#%%%%%%%%%%%%%%%%%%%%%%%%%%

# Clean all source code
clean:
	./build.sh clean_all

# Clean all source code and the run directory
superclean:
	@$(MAKE) clean
	@$(MAKE) cleanup_exe
	@$(MAKE) cleanup_output

# Clean only MAPL, FVdycore, and GEOS-Chem
clean_mapl:
	./build.sh clean_mapl

# Clean only GEOS-Chem
clean_gc:
	./build.sh clean_gc

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Compile ESMF, MAPL, FVdycore, and GEOS-Chem  %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Clean and build
build:
	@$(MAKE) build_all

# Clean and build with debug flags
build_debug:
	date > ${BUILD_LOG}
	./build.sh clean_all
	./build.sh build --debug 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

# Build without cleaning
rebuild:
	date > ${BUILD_LOG}
	rm -f $(CODEDIR_GCHP)/ESMF/esmf.install
	rm -f $(CODEDIR_GCHP)/Shared/mapl.install
	rm -f $(CODEDIR_GCHP)/FVdycoreCubed_GridComp/fvdycore.install
	./build.sh build 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

# Build with debug flags and without cleaning
rebuild_debug:
	date > ${BUILD_LOG}
	rm -f $(CODEDIR_GCHP)/ESMF/esmf.install
	rm -f $(CODEDIR_GCHP)/Shared/mapl.install
	rm -f $(CODEDIR_GCHP)/FVdycoreCubed_GridComp/fvdycore.install
	./build.sh build --debug 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Compile MAPL, FVdycore, and GEOS-Chem but not ESMF %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Clean and build
build_mapl:
	date > ${BUILD_LOG}
	./build.sh clean_mapl
	./build.sh build 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

# Clean and build with debug flags
build_mapl_debug:
	date > ${BUILD_LOG}
	./build.sh clean_mapl
	./build.sh build --debug 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

# Build without cleaning
rebuild_mapl:
	date > ${BUILD_LOG}
	rm -f $(CODEDIR_GCHP)/Shared/mapl.install
	rm -f $(CODEDIR_GCHP)/FVdycoreCubed_GridComp/fvdycore.install
	./build.sh build 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

# Build with debug flags and without cleaning
rebuild_mapl_debug:
	date > ${BUILD_LOG}
	rm -f $(CODEDIR_GCHP)/Shared/mapl.install
	rm -f $(CODEDIR_GCHP)/FVdycoreCubed_GridComp/fvdycore.install
	./build.sh build --debug 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Compile GEOS-Chem but not ESMF, MAPL, and FVdycore %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Clean and build
build_gc:
	date > ${BUILD_LOG}
	@$(MAKE) clean_gc
	./build.sh build 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

# Clean and build with debug flags
build_gc_debug:
	date > ${BUILD_LOG}
	./build.sh clean_gc
	./build.sh build --debug 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

# Build without cleaning
rebuild_gc:
	date > ${BUILD_LOG}
	./build.sh build 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

# Build with debug flags and without cleaning
rebuild_gc_debug:
	date > ${BUILD_LOG}
	./build.sh build --debug 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo 2>&1 | tee -a $(BUILD_LOG)
	@$(MAKE) printbuildinfo > $(LAST_BUILD)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Clean up Run Directory  %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

cleanup_output: cleanup_data cleanup_logs

cleanup_exe: 
	rm -f geos
	rm -f $(BUILD_LOG)
	rm -f $(LAST_BUILD)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Legacy options
#%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Legacy, will be deleted in a future version
clean_nuclear:
	@$(MAKE) clean
	@echo "WARNING: 'make clean_nuclear' will be deprecated in a future version."
	@echo "Use 'make clean' instead."
	@echo "Type 'make help' for complete list of expanded options."

compile_clean:
	@$(MAKE) build
	@echo "WARNING: 'make compile_clean' will be deprecated in a future version."
	@echo "Use 'make build' instead."
	@echo "Type 'make help' for complete list of expanded options."

compile_standard:
	@$(MAKE) rebuild_gc
	@echo "WARNING: 'make compile_standard' will be deprecated in a future version."
	@echo "Use 'make rebuild_gc' instead."
	@echo "Type 'make help' for complete list of expanded options."

compile_debug:
	@$(MAKE) rebuild_gc_debug
	@echo "WARNING: 'make compile_debug' will be deprecated in a future version."
	@echo "Use 'make rebuild_gc_debug' instead."
	@echo "Type 'make help' for complete list of expanded options."

compile_mapl:
	@$(MAKE) build_mapl
	@echo "WARNING: 'make compile_mapl' will be deprecated in a future version."
	@echo "Use targets build_mapl, build_mapl_debug, rebuild_mapl, or rebuild_mapl_debug instead."
	@echo "Type 'make help' for complete list of expanded options."

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
	@echo '----------------------------------------------------------'
	@echo 'TARGET may be one of the following:'
	@echo '----------------------------------------------------------'
	@echo ' '      
	@echo 'Print make options:' 
	@echo '  printbuildinfo     Print out git info for GC and GCHP repos'
	@echo '  help               If you got this far, you know this one'
	@echo ' '
	@echo 'Clean source code:'
	@echo '  clean              Clean ESMF, MAPL, FVdycore, and GEOS-Chem (all)'
	@echo '  clean_mapl         Clean MAPL, FVdycore, and GEOS-Chem (not ESMF)'
	@echo '  clean_gc           Clean GEOS-Chem only'
	@echo '  superclean         Clean all source code and the run directory'
	@echo ' '
	@echo 'Clean/compile all (ESMF, MAPL, FVdycore, and GEOS-Chem):'
	@echo '  build              Clean and compile; GC debug off'
	@echo '  build_debug        Clean and compile; GC debug on'
	@echo '  rebuild            Compile without cleaning; GC debug off'
	@echo '  rebuild_debug      Compile without cleaning; GC debug on'
	@echo ' '
	@echo 'Clean/compile MAPL, FVdycore, and GEOS-Chem (not ESMF):'
	@echo '  build_mapl         Clean and compile; GC debug off'
	@echo '  build_mapl_debug   Clean and compile; GC debug on'
	@echo '  rebuild_mapl       Compile without cleaning; GC debug off'
	@echo '  rebuild_mapl_debug Compile without cleaning; GC debug on'
	@echo ' '
	@echo 'Clean/compile only GEOS-Chem (not ESMF, MAPL, or FVdycore):'
	@echo '  build_gc           Clean and compile; GC debug off'
	@echo '  build_debug_gc     Clean and compile; GC debug on'
	@echo '  rebuild_gc         Compile without cleaning; GC debug off'
	@echo '  rebuild_debug_gc   Compile without cleaning; GC debug on'
	@echo ' '
	@echo 'To remove run directory files:' 
	@echo '  cleanup_output     Remove output data, log files, and executable'
	@echo '  cleanup_data       Remove output data' 
	@echo '  cleanup_logs       Remove log files' 
	@echo '  cleanup_exe        Remove executable'
	@echo '  superclean         All of the above and clean all source code'
	@echo ' ' 
	@echo 'Legacy options (will be deprecated in a future version) ' 
	@echo '  compile_clean      Clean and build everything'
	@echo '  compile_standard   Skip MAPL, FvDyCore, and ESMF; debug off'
	@echo '  compile_debug      Skip MAPL, FvDyCore, and ESMF; debug on'
	@echo '  compile_mapl       Clean and build everything except ESMF'
	@echo '  clean_nuclear      Clean everything using build.sh'
	@echo ' ' 
	@echo 'Print source code info:' 
	@echo '  printbuildinfo     Print out git info for GC and GCHP repos'
#EOC


