#
# Earth System Modeling Applications (ESMA) PILGRIM makefile fragment.
# This fragment customize GNUmakefile for each architecture. 
#
# REVISION HISTORY:
#
# 01May2006  Todling   Cannot run w/ option -fpe0 (ana5sfc crashes)
#
#--------------------------------------------------------------------------


#   -----
#   LINUX
#   -----

ifeq ($(ARCH),Linux)

FPE =
LATEX = pdflatex

endif  #    Linux
