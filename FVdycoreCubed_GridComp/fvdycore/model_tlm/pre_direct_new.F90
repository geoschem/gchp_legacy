module pre_direct_new

implicit none
private

#ifdef TEST2
logical :: TEST2_kim=.true.       
#else
logical :: TEST2_kim=.false.       
#endif
#ifdef INTEL_OPT
logical :: INTEL_OPT_kim=.true.  
#else
logical :: INTEL_OPT_kim=.false.  
#endif

public TEST2_kim,INTEL_OPT_kim

end module pre_direct_new

