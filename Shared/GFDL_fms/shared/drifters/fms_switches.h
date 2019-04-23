#if !defined(use_libMPI)
#define _SERIAL
#ifndef LRB_CLEANER_BUILD_LOG
!DEC$ MESSAGE:'Compiling in serial mode'
#endif
#else
#undef _SERIAL
#ifndef LRB_CLEANER_BUILD_LOG
!DEC$ MESSAGE:'Compiling in MPI mode (with or without MPP) '
#endif
#endif
