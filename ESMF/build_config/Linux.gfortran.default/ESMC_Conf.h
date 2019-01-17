#ifdef ESMC_RCS_HEADER
"$Id$"
"Defines the configuration for this machine"
#endif

#if 0
Earth System Modeling Framework
Copyright 2002-2018, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA License.
#endif

#if !defined(INCLUDED_CONF_H)
#define INCLUDED_CONF_H

#define PARCH_linux

#ifdef ESMF_LOWERCASE_SINGLEUNDERSCORE 
#define FTN_X(func) func##_
#define FTNX(func) func##_
#endif
#ifdef ESMF_LOWERCASE_DOUBLEUNDERSCORE 
#define FTN_X(func) func##__
#define FTNX(func) func##_
#endif

#if defined (__cplusplus)
// Typedef to match the data type of the 'hidden' string length
// argument that Fortran uses when passing CHARACTER strings.
// For GCHP (ewl, 1/17/19):
// Modify handling of GNUC > 7 to use int instaed of size_t to avoid
// compilation errors when compiling on the Harvard Odyssey cluster.
// This issue should be revisited in the future since it implies 
// a problem with access to the C library.
//typedef size_t ESMCI_FortranStrLenArg;
typedef int ESMCI_FortranStrLenArg;
#endif

#define ESMC_PRESENT(arg) ( (arg) != 0 )

#ifdef S32
#define ESMC_POINTER_SIZE 4
#endif
#ifdef Sia64_64
#define ESMC_POINTER_SIZE 8
#endif
#ifdef Sx86_64_32
#define ESMC_POINTER_SIZE 4
#endif
#ifdef Sx86_64_small
#define ESMC_POINTER_SIZE 8
#endif
#ifdef Sx86_64_medium
#define ESMC_POINTER_SIZE 8
#endif

#endif
