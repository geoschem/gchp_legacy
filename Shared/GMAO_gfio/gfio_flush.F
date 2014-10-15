#ifdef sysAIX
      subroutine flush(lu)
      integer lu
      call flush_(lu)
      end
#else
      subroutine dum_flush
      end
#endif

