#!/bin/csh -f
#
# Test function aliases. Unit test for Functions.csh.
#
# arlindo.dasilva@nasa.gov, November 2006
#---------------------------------------------------------------------------
#

# Must do this first
# ------------------
  source `which Functions.csh`

# Functions can be defined in this file or in a module file 
# The Use command appends the extension .csm and looks for
# the file utFunctions.csm in your path
# -------------------------------------------------------
  Use utFunctions

#                           ---

  echo ""
  echo "               Testing CSH Functions"
  echo "               ---------------------"
  echo ""


# Regular call: function ( arg1, arg2, .. )
# Limitation: arguments must be simple words, no strings
# ------------------------------------------------------
  echo "-------> Calling Say..."
  set first = "arlindo"
  set times = 7
  Call Say ( $first, maryland, 301-592-0303, dasilva ) 
  echo "Variable times now has value $times"

# Naked call: no parenthesis, no commas (more robust)
# This illustrates how to send command line args without
# having the function declaring named parameters
# ------------------------------------------------------
  echo ""
  echo "-------> Calling Hello..."
  Call_ Hello 'World' 
  if ( $status ) echo "Function <Hello> returned $status"

# Another naked call: function has named parameters
# -------------------------------------------------
  echo ""
  echo "Calling Entry..."
  set something = "set in <main>" # will be changed by Entry
  Call_ Entry 'Arlindo Moraes da Silva'  '212 Whitmoor Terrace' '29'
  echo "Variable 'something' $something"

# Execute example as a separate script
# ------------------------------------ 
  echo ""
  echo "-------> Running Entry..."
  set something = "set by <main>" # will NOT be changed by Entry
  Run Entry 'Arlindo Moraes da Silva'  '212 Whitmoor Terrace' '29'
  echo "Variable 'something' $something"

# Add to numbers, return by reference
# -----------------------------------
  echo ""
  echo "-------> Calling Add..."
  Call Add ( 3, 5, result )
  echo "Result is $result"

# Exit on non-zero $status from Function call
# -------------------------------------------
  set Verify

# Simulating an error
# ------------------
  echo ""
  echo "-------> Calling an undefined function: "
  Call Aloha

# Call this at the end to cleanup your mess in $TMPDIR
# ----------------------------------------------------
  Clean

  echo ""
  echo "All done"

