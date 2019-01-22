c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c ... file shaec.f
c
c     this file contains code and documentation for subroutines
c     shaec and shaeci
c
c ... files which must be loaded with shaec.f
c
c     sphcom.f, hrfft.f
c
c     subroutine shaec(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                 wshaec,lshaec,work,lwork,ierror)
c
c     subroutine shaec performs the spherical harmonic analysis
c     on the array g and stores the result in the arrays a and b.
c     the analysis is performed on an equally spaced grid.  the
c     associated legendre functions are recomputed rather than stored
c     as they are in subroutine shaes.  the analysis is described
c     below at output parameters a,b.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     isym   = 0  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the analysis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the analysis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of analyses.  in the program that calls shaec,
c            the arrays g,a and b can be three dimensional in which
c            case multiple analyses will be performed.  the third
c            index is the analysis index which assumes the values
c            k=1,...,nt.  for a single analysis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     g      a two or three dimensional array (see input parameter
c            nt) that contains the discrete function to be analyzed.
c            g(i,j) contains the value of the function at the colatitude
c            point theta(i) = (i-1)*pi/(nlat-1) and longitude point
c            phi(j) = (j-1)*2*pi/nlon. the index ranges are defined
c            above at the input parameter isym.
c
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shaec.  if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg
c            must be at least nlat/2 if nlat is even or at least
c            (nlat+1)/2 if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shaec.  jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shaec. mdab must be at least
c            min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shaec. ndab must be at least nlat
c
c     wshaec an array which must be initialized by subroutine shaeci.
c            once initialized, wshaec can be used repeatedly by shaec
c            as long as nlon and nlat remain unchanged.  wshaec must
c            not be altered between calls of shaec.
c
c     lshaec the dimension of the array wshaec as it appears in the
c            program that calls shaec. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshaec must be at least
c
c            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shaec. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c                    nlat*(nt*nlon+max0(3*l2,nlon))
c
c            if isym is not zero then lwork must be at least
c
c                    l2*(nt*nlon+max0(3*nlat,nlon))
c
c     **************************************************************
c
c     output parameters
c
c     a,b    both a,b are two or three dimensional arrays (see input
c            parameter nt) that contain the spherical harmonic
c            coefficients in the representation of g(i,j) given in the
c            discription of subroutine shsec. for isym=0, a(m,n) and
c            b(m,n) are given by the equations listed below. symmetric
c            versions are used when isym is greater than zero.
c
c
c
c     definitions
c
c     1. the normalized associated legendre functions
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta)
c
c     2. the normalized z functions for m even
c
c     zbar(m,n,theta) = 2/(nlat-1) times the sum from k=0 to k=nlat-1 of
c                       the integral from tau = 0 to tau = pi of
c                       cos(k*theta)*cos(k*tau)*pbar(m,n,tau)*sin(tau)
c                       (first and last terms in this sum are divided
c                       by 2)
c
c     3. the normalized z functions for m odd
c
c     zbar(m,n,theta) = 2/(nlat-1) times the sum from k=0 to k=nlat-1 of
c                       of the integral from tau = 0 to tau = pi of
c                       sin(k*theta)*sin(k*tau)*pbar(m,n,tau)*sin(tau)
c
c     4. the fourier transform of g(i,j).
c
c     c(m,i)          = 2/nlon times the sum from j=1 to j=nlon
c                       of g(i,j)*cos((m-1)*(j-1)*2*pi/nlon)
c                       (the first and last terms in this sum
c                       are divided by 2)
c
c     s(m,i)          = 2/nlon times the sum from j=2 to j=nlon
c                       of g(i,j)*sin((m-1)*(j-1)*2*pi/nlon)
c
c     5. the maximum (plus one) longitudinal wave number
c
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c
c     then for m=0,...,mmax-1 and n=m,...,nlat-1 the arrays a,b
c     are given by
c
c     a(m+1,n+1)      = the sum from i=1 to i=nlat of
c                       c(m+1,i)*zbar(m,n,theta(i))
c                       (first and last terms in this sum are
c                       divided by 2)
c
c     b(m+1,n+1)      = the sum from i=1 to i=nlat of
c                       s(m+1,i)*zbar(m,n,theta(i))
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of nt
c            = 5  error in the specification of idg
c            = 6  error in the specification of jdg
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lshaec
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c     subroutine shaeci(nlat,nlon,wshaec,lshaec,dwork,ldwork,ierror)
c
c     subroutine shaeci initializes the array wshaec which can then
c     be used repeatedly by subroutine shaec.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     lshaec the dimension of the array wshaec as it appears in the
c            program that calls shaeci. the array wshaec is an output
c            parameter which is described below. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshaec must be at least
c
c            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c     dwork  a double precision dwork array that does not have to be saved.
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls shaeci.  ldwork  must be at least
c            nlat+1.
c
c
c     output parameters
c
c     wshaec an array which is initialized for use by subroutine shaec.
c            once initialized, wshaec can be used repeatedly by shaec
c            as long as nlon and nlat remain unchanged.  wshaec must
c            not be altered between calls of shaec.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshaec
c            = 4  error in the specification of ldwork
c
c
c *******************************************************************
      subroutine shaec(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
     1                    wshaec,lshaec,work,lwork,ierror)
      real g(idg,jdg,*),a(mdab,ndab,*),b(mdab,ndab,*),wshaec(*),
     1          work(*)
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
      ierror = 3
      if(isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      if((isym.eq.0 .and. idg.lt.nlat) .or.
     1   (isym.ne.0 .and. idg.lt.(nlat+1)/2)) return
      ierror = 6
      if(jdg .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,nlon/2+1)
      if(mdab .lt. mmax) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
      imid = (nlat+1)/2
      lzz1 = 2*nlat*imid
      labc = 3*((mmax-2)*(nlat+nlat-mmax-1))/2
      if(lshaec .lt. lzz1+labc+nlon+15) return
      ierror = 10
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
      if(lwork .lt. nln+max0(ls*nlon,3*nlat*imid)) return
      ierror = 0
      ist = 0
      if(isym .eq. 0) ist = imid
      iw1 = lzz1+labc+1
      call shaec1(nlat,isym,nt,g,idg,jdg,a,b,mdab,ndab,imid,ls,nlon,
     1     work,work(ist+1),work(nln+1),work(nln+1),wshaec,wshaec(iw1))
      return
      end
      subroutine shaec1(nlat,isym,nt,g,idgs,jdgs,a,b,mdab,ndab,imid,
     1                idg,jdg,ge,go,work,zb,wzfin,whrfft)
c
c     whrfft must have at least nlon+15 locations
c     wzfin must have 2*l*(nlat+1)/2 + ((l-3)*l+2)/2 locations
c     zb must have 3*l*(nlat+1)/2 locations
c     work must have ls*nlon locations
c
      real g(idgs,jdgs,1),a(mdab,ndab,1),b(mdab,ndab,1),
     1          ge(idg,jdg,1),go(idg,jdg,1),zb(imid,nlat,3),wzfin(1),
     3          whrfft(1),work(1)
      ls = idg
      nlon = jdg
      mmax = min0(nlat,nlon/2+1)
      mdo = mmax
      if(mdo+mdo-1 .gt. nlon) mdo = mmax-1
      nlp1 = nlat+1
      tsn = 2./nlon
      fsn = 4./nlon
      modl = mod(nlat,2)
      imm1 = imid
      if(modl .ne. 0) imm1 = imid-1
      if(isym .ne. 0) go to 15
      do 5 k=1,nt
      do 5 i=1,imm1
      do 5 j=1,nlon
      ge(i,j,k) = tsn*(g(i,j,k)+g(nlp1-i,j,k))
      go(i,j,k) = tsn*(g(i,j,k)-g(nlp1-i,j,k))
    5 continue
      go to 30
   15 do 20 k=1,nt
      do 20 i=1,imm1
      do 20 j=1,nlon
      ge(i,j,k) = fsn*g(i,j,k)
   20 continue
      if(isym .eq. 1) go to 27
   30 if(modl .eq. 0) go to 27
      do 25 k=1,nt
      do 25 j=1,nlon
      ge(imid,j,k) = tsn*g(imid,j,k)
   25 continue
   27 do 35 k=1,nt
      call hrfftf(ls,nlon,ge(1,1,k),ls,whrfft,work)
      if(mod(nlon,2) .ne. 0) go to 35
      do 36 i=1,ls
      ge(i,nlon,k) = .5*ge(i,nlon,k)
   36 continue
   35 continue
      do 40 k=1,nt
      do 40 mp1=1,mmax
      do 40 np1=mp1,nlat
      a(mp1,np1,k) = 0.
      b(mp1,np1,k) = 0.
   40 continue
      if(isym .eq. 1) go to 145
      call zfin (2,nlat,nlon,0,zb,i3,wzfin)
      do 110 k=1,nt
      do 110 i=1,imid
      do 110 np1=1,nlat,2
      a(1,np1,k) = a(1,np1,k)+zb(i,np1,i3)*ge(i,1,k)
  110 continue
      ndo = nlat
      if(mod(nlat,2) .eq. 0) ndo = nlat-1
      do 120 mp1=2,mdo
      m = mp1-1
      call zfin (2,nlat,nlon,m,zb,i3,wzfin)
      do 120 k=1,nt
      do 120 i=1,imid
      do 120 np1=mp1,ndo,2
      a(mp1,np1,k) = a(mp1,np1,k)+zb(i,np1,i3)*ge(i,2*mp1-2,k)
      b(mp1,np1,k) = b(mp1,np1,k)+zb(i,np1,i3)*ge(i,2*mp1-1,k)
  120 continue
      if(mdo .eq. mmax .or. mmax .gt. ndo) go to 135
      call zfin (2,nlat,nlon,mdo,zb,i3,wzfin)
      do 130 k=1,nt
      do 130 i=1,imid
      do 130 np1=mmax,ndo,2
      a(mmax,np1,k) = a(mmax,np1,k)+zb(i,np1,i3)*ge(i,2*mmax-2,k)
  130 continue
  135 if(isym .eq. 2) return
  145 call zfin (1,nlat,nlon,0,zb,i3,wzfin)
      do 150 k=1,nt
      do 150 i=1,imm1
      do 150 np1=2,nlat,2
      a(1,np1,k) = a(1,np1,k)+zb(i,np1,i3)*go(i,1,k)
  150 continue
      ndo = nlat
      if(mod(nlat,2) .ne. 0) ndo = nlat-1
      do 160 mp1=2,mdo
      m = mp1-1
      mp2 = mp1+1
      call zfin (1,nlat,nlon,m,zb,i3,wzfin)
      do 160 k=1,nt
      do 160 i=1,imm1
      do 160 np1=mp2,ndo,2
      a(mp1,np1,k) = a(mp1,np1,k)+zb(i,np1,i3)*go(i,2*mp1-2,k)
      b(mp1,np1,k) = b(mp1,np1,k)+zb(i,np1,i3)*go(i,2*mp1-1,k)
  160 continue
      mp2 = mmax+1
      if(mdo .eq. mmax .or. mp2 .gt. ndo) return
      call zfin (1,nlat,nlon,mdo,zb,i3,wzfin)
      do 170 k=1,nt
      do 170 i=1,imm1
      do 170 np1=mp2,ndo,2
      a(mmax,np1,k) = a(mmax,np1,k)+zb(i,np1,i3)*go(i,2*mmax-2,k)
  170 continue
      return
      end

      subroutine shaeci(nlat,nlon,wshaec,lshaec,dwork,ldwork,ierror)
      real wshaec(lshaec)
      double precision dwork(ldwork)
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
      ierror = 3
      imid = (nlat+1)/2
      mmax = min0(nlat,nlon/2+1)
      lzz1 = 2*nlat*imid
      labc = 3*((mmax-2)*(nlat+nlat-mmax-1))/2
      if(lshaec .lt. lzz1+labc+nlon+15) return
      ierror = 4
      if(ldwork .lt. nlat+1) return
      ierror = 0
      call zfinit (nlat,nlon,wshaec,dwork)
      iw1 = lzz1+labc+1
      call hrffti(nlon,wshaec(iw1))
      return
      end
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file shaes.f
c
c     this file contains code and documentation for subroutines
c     shaes and shaesi
c
c ... files which must be loaded with shaes.f
c
c     sphcom.f, hrfft.f
c
c     subroutine shaes(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                 wshaes,lshaes,work,lwork,ierror)
c
c     subroutine shaes performs the spherical harmonic analysis
c     on the array g and stores the result in the arrays a and b.
c     the analysis is performed on an equally spaced grid.  the
c     associated legendre functions are stored rather than recomputed
c     as they are in subroutine shaec.  the analysis is described
c     below at output parameters a,b.
c
c     sphcom.f, hrfft.f
c
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     isym   = 0  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the analysis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the analysis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of analyses.  in the program that calls shaes,
c            the arrays g,a and b can be three dimensional in which
c            case multiple analyses will be performed.  the third
c            index is the analysis index which assumes the values
c            k=1,...,nt.  for a single analysis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     g      a two or three dimensional array (see input parameter
c            nt) that contains the discrete function to be analyzed.
c            g(i,j) contains the value of the function at the colatitude
c            point theta(i) = (i-1)*pi/(nlat-1) and longitude point
c            phi(j) = (j-1)*2*pi/nlon. the index ranges are defined
c            above at the input parameter isym.
c
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shaes.  if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg
c            must be at least nlat/2 if nlat is even or at least
c            (nlat+1)/2 if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shaes.  jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shaes. mdab must be at least
c            min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shaes. ndab must be at least nlat
c
c     wshaes an array which must be initialized by subroutine shaesi.
c            once initialized, wshaes can be used repeatedly by shaes
c            as long as nlon and nlat remain unchanged.  wshaes must
c            not be altered between calls of shaes.
c
c     lshaes the dimension of the array wshaes as it appears in the
c            program that calls shaes. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshaes must be at least
c
c               (l1*l2*(nlat+nlat-l1+1))/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shaes.  define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if isym is zero then lwork must be at least
c            (nt+1)*nlat*nlon. if isym is not zero then
c            lwork must be at least (nt+1)*l2*nlon.
c
c
c     **************************************************************
c
c     output parameters
c
c     a,b    both a,b are two or three dimensional arrays (see input
c            parameter nt) that contain the spherical harmonic
c            coefficients in the representation of g(i,j) given in the
c            discription of subroutine shses. for isym=0, a(m,n) and
c            b(m,n) are given by the equations listed below. symmetric
c            versions are used when isym is greater than zero.
c
c
c
c     definitions
c
c     1. the normalized associated legendre functions
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta)
c
c     2. the normalized z functions for m even
c
c     zbar(m,n,theta) = 2/(nlat-1) times the sum from k=0 to k=nlat-1 of
c                       the integral from tau = 0 to tau = pi of
c                       cos(k*theta)*cos(k*tau)*pbar(m,n,tau)*sin(tau)
c                       (first and last terms in this sum are divided
c                       by 2)
c
c     3. the normalized z functions for m odd
c
c     zbar(m,n,theta) = 2/(nlat-1) times the sum from k=0 to k=nlat-1 of
c                       of the integral from tau = 0 to tau = pi of
c                       sin(k*theta)*sin(k*tau)*pbar(m,n,tau)*sin(tau)
c
c     4. the fourier transform of g(i,j).
c
c     c(m,i)          = 2/nlon times the sum from j=1 to j=nlon
c                       of g(i,j)*cos((m-1)*(j-1)*2*pi/nlon)
c                       (the first and last terms in this sum
c                       are divided by 2)
c
c     s(m,i)          = 2/nlon times the sum from j=2 to j=nlon
c                       of g(i,j)*sin((m-1)*(j-1)*2*pi/nlon)
c
c     5. the maximum (plus one) longitudinal wave number
c
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     then for m=0,...,mmax-1 and  n=m,...,nlat-1  the arrays a,b are
c     given by
c
c     a(m+1,n+1)      = the sum from i=1 to i=nlat of
c                       c(m+1,i)*zbar(m,n,theta(i))
c                       (first and last terms in this sum are
c                       divided by 2)
c
c     b(m+1,n+1)      = the sum from i=1 to i=nlat of
c                       s(m+1,i)*zbar(m,n,theta(i))
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of nt
c            = 5  error in the specification of idg
c            = 6  error in the specification of jdg
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lshaes
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c     subroutine shaesi(nlat,nlon,wshaes,lshaes,work,lwork,dwork,
c    +                  ldwork,ierror)
c
c     subroutine shaesi initializes the array wshaes which can then
c     be used repeatedly by subroutine shaes
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     lshaes the dimension of the array wshaes as it appears in the
c            program that calls shaesi. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshaes must be at least
c
c               (l1*l2*(nlat+nlat-l1+1))/2+nlon+15
c
c     work   a real   work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shaesi.  define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lwork must be at least
c
c               5*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2
c
c
c     dwork  a double precision work array that does not have to be saved.
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls shaesi.  ldwork must be at least nlat+1
c
c
c     output parameters
c
c     wshaes an array which is initialized for use by subroutine shaes.
c            once initialized, wshaes can be used repeatedly by shaes
c            as long as nlon and nlat remain unchanged.  wshaes must
c            not be altered between calls of shaes.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshaes
c            = 4  error in the specification of lwork
c            = 5  error in the specification of ldwork
c
c
c ****************************************************************
      subroutine shaes(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
     1                    wshaes,lshaes,work,lwork,ierror)
      real g(idg,jdg,1),a(mdab,ndab,1),b(mdab,ndab,1),wshaes(1),
     1          work(1)
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
      ierror = 3
      if(isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      if((isym.eq.0 .and. idg.lt.nlat) .or.
     1   (isym.ne.0 .and. idg.lt.(nlat+1)/2)) return
      ierror = 6
      if(jdg .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,nlon/2+1)
      if(mdab .lt. mmax) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
      imid = (nlat+1)/2
      idz = (mmax*(nlat+nlat-mmax+1))/2
      lzimn = idz*imid
      if(lshaes .lt. lzimn+nlon+15) return
      ierror = 10
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
      if(lwork .lt. nln+ls*nlon) return
      ierror = 0
      ist = 0
      if(isym .eq. 0) ist = imid
      call shaes1(nlat,isym,nt,g,idg,jdg,a,b,mdab,ndab,wshaes,idz,
     1         ls,nlon,work,work(ist+1),work(nln+1),wshaes(lzimn+1))
      return
      end
      subroutine shaes1(nlat,isym,nt,g,idgs,jdgs,a,b,mdab,ndab,z,idz,
     1                  idg,jdg,ge,go,work,whrfft)
      real g(idgs,jdgs,1),a(mdab,ndab,1),b(mdab,ndab,1),z(idz,1),
     1          ge(idg,jdg,1),go(idg,jdg,1),work(1),whrfft(1)
      ls = idg
      nlon = jdg
      mmax = min0(nlat,nlon/2+1)
      mdo = mmax
      if(mdo+mdo-1 .gt. nlon) mdo = mmax-1
      nlp1 = nlat+1
      tsn = 2./nlon
      fsn = 4./nlon
      imid = (nlat+1)/2
      modl = mod(nlat,2)
      imm1 = imid
      if(modl .ne. 0) imm1 = imid-1
      if(isym .ne. 0) go to 15
      do 5 k=1,nt
      do 5 i=1,imm1
      do 5 j=1,nlon
      ge(i,j,k) = tsn*(g(i,j,k)+g(nlp1-i,j,k))
      go(i,j,k) = tsn*(g(i,j,k)-g(nlp1-i,j,k))
    5 continue
      go to 30
   15 do 20 k=1,nt
      do 20 i=1,imm1
      do 20 j=1,nlon
      ge(i,j,k) = fsn*g(i,j,k)
   20 continue
      if(isym .eq. 1) go to 27
   30 if(modl .eq. 0) go to 27
      do 25 k=1,nt
      do 25 j=1,nlon
      ge(imid,j,k) = tsn*g(imid,j,k)
   25 continue
   27 do 35 k=1,nt
      call hrfftf(ls,nlon,ge(1,1,k),ls,whrfft,work)
      if(mod(nlon,2) .ne. 0) go to 35
      do 36 i=1,ls
      ge(i,nlon,k) = .5*ge(i,nlon,k)
   36 continue
   35 continue
      do 40 k=1,nt
      do 40 mp1=1,mmax
      do 40 np1=mp1,nlat
      a(mp1,np1,k) = 0.
      b(mp1,np1,k) = 0.
   40 continue
      if(isym .eq. 1) go to 145
      do 110 k=1,nt
      do 110 i=1,imid
      do 110 np1=1,nlat,2
      a(1,np1,k) = a(1,np1,k)+z(np1,i)*ge(i,1,k)
  110 continue
      ndo = nlat
      if(mod(nlat,2) .eq. 0) ndo = nlat-1
      do 120 mp1=2,mdo
      m = mp1-1
      mb = m*(nlat-1)-(m*(m-1))/2
      do 120 k=1,nt
      do 120 i=1,imid
      do 120 np1=mp1,ndo,2
      a(mp1,np1,k) = a(mp1,np1,k)+z(np1+mb,i)*ge(i,2*mp1-2,k)
      b(mp1,np1,k) = b(mp1,np1,k)+z(np1+mb,i)*ge(i,2*mp1-1,k)
  120 continue
      if(mdo .eq. mmax .or. mmax .gt. ndo) go to 135
      mb = mdo*(nlat-1)-(mdo*(mdo-1))/2
      do 130 k=1,nt
      do 130 i=1,imid
      do 130 np1=mmax,ndo,2
      a(mmax,np1,k) = a(mmax,np1,k)+z(np1+mb,i)*ge(i,2*mmax-2,k)
  130 continue
  135 if(isym .eq. 2) return
  145 do 150 k=1,nt
      do 150 i=1,imm1
      do 150 np1=2,nlat,2
      a(1,np1,k) = a(1,np1,k)+z(np1,i)*go(i,1,k)
  150 continue
      ndo = nlat
      if(mod(nlat,2) .ne. 0) ndo = nlat-1
      do 160 mp1=2,mdo
      m = mp1-1
      mp2 = mp1+1
      mb = m*(nlat-1)-(m*(m-1))/2
      do 160 k=1,nt
      do 160 i=1,imm1
      do 160 np1=mp2,ndo,2
      a(mp1,np1,k) = a(mp1,np1,k)+z(np1+mb,i)*go(i,2*mp1-2,k)
      b(mp1,np1,k) = b(mp1,np1,k)+z(np1+mb,i)*go(i,2*mp1-1,k)
  160 continue
      mp2 = mmax+1
      if(mdo .eq. mmax .or. mp2 .gt. ndo) return
      mb = mdo*(nlat-1)-(mdo*(mdo-1))/2
      do 170 k=1,nt
      do 170 i=1,imm1
      do 170 np1=mp2,ndo,2
      a(mmax,np1,k) = a(mmax,np1,k)+z(np1+mb,i)*go(i,2*mmax-2,k)
  170 continue
      return
      end

      subroutine shaesi(nlat,nlon,wshaes,lshaes,work,lwork,dwork,
     +                  ldwork,ierror)
      real wshaes(*),work(*)
      double precision dwork(*)
c
c     length of wshaes is (l*(l+1)*imid)/2+nlon+15
c     length of work is 5*l*imid + 3*((l-3)*l+2)/2
c
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
      ierror = 3
      mmax = min0(nlat,nlon/2+1)
      imid = (nlat+1)/2
      lzimn = (imid*mmax*(nlat+nlat-mmax+1))/2
      if(lshaes .lt. lzimn+nlon+15) return
      ierror = 4
      labc = 3*((mmax-2)*(nlat+nlat-mmax-1))/2
      if(lwork .lt. 5*nlat*imid + labc) return
      ierror = 5
      if (ldwork .lt. nlat+1) return
      ierror = 0
      iw1 = 3*nlat*imid+1
      idz = (mmax*(nlat+nlat-mmax+1))/2
      call sea1(nlat,nlon,imid,wshaes,idz,work,work(iw1),dwork)
      call hrffti(nlon,wshaes(lzimn+1))
      return
      end
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c ... file sphcom.f
c
c     this file must be loaded with all driver level files
c     in spherepack3.0.  it includes undocumented subroutines
c     called by some or all of the drivers
c
      subroutine dnlfk (m,n,cp)
c
c     cp requires n/2+1 double precision locations
c
      double precision cp(1),fnum,fden,fnmh,a1,b1,c1,cp2,fnnp1,fnmsq,fk,
     1       t1,t2,pm1,sc10,sc20,sc40
!      real       cp(1) ! bma
      parameter (sc10=1024.d0)
      parameter (sc20=sc10*sc10)
      parameter (sc40=sc20*sc20)
c
      cp(1) = 0.
      ma = iabs(m)
      if(ma .gt. n) return
      if(n-1) 2,3,5
    2 cp(1) = dsqrt(2.d0)
      return
    3 if(ma .ne. 0) go to 4
      cp(1) = dsqrt(1.5d0)
      return
    4 cp(1) = dsqrt(.75d0)
      if(m .eq. -1) cp(1) = -cp(1)
      return
    5 if(mod(n+ma,2) .ne. 0) go to 10
      nmms2 = (n-ma)/2
      fnum = n+ma+1
      fnmh = n-ma+1
      pm1 = 1.d0
      go to 15
   10 nmms2 = (n-ma-1)/2
      fnum = n+ma+2
      fnmh = n-ma+2
      pm1 = -1.d0
c      t1 = 1.
c      t1 = 2.d0**(n-1)
c      t1 = 1.d0/t1
 15   t1 = 1.d0/sc20
      nex = 20
      fden = 2.d0
      if(nmms2 .lt. 1) go to 20
      do 18 i=1,nmms2
      t1 = fnum*t1/fden
      if(t1 .gt. sc20) then
      t1 = t1/sc40
      nex = nex+40
      end if
      fnum = fnum+2.
      fden = fden+2.
   18 continue
   20 t1 = t1/2.d0**(n-1-nex)
      if(mod(ma/2,2) .ne. 0) t1 = -t1
      t2 = 1. 
      if(ma .eq. 0) go to 26
      do 25 i=1,ma
      t2 = fnmh*t2/(fnmh+pm1)
      fnmh = fnmh+2.
   25 continue
   26 cp2 = t1*dsqrt((n+.5d0)*t2)
      fnnp1 = n*(n+1)
      fnmsq = fnnp1-2.d0*ma*ma
      l = (n+1)/2
      if(mod(n,2) .eq. 0 .and. mod(ma,2) .eq. 0) l = l+1
      cp(l) = cp2
      if(m .ge. 0) go to 29
      if(mod(ma,2) .ne. 0) cp(l) = -cp(l)
   29 if(l .le. 1) return
      fk = n
      a1 = (fk-2.)*(fk-1.)-fnnp1
      b1 = 2.*(fk*fk-fnmsq)
      cp(l-1) = b1*cp(l)/a1
   30 l = l-1
      if(l .le. 1) return
      fk = fk-2.
      a1 = (fk-2.)*(fk-1.)-fnnp1
      b1 = -2.*(fk*fk-fnmsq)
      c1 = (fk+1.)*(fk+2.)-fnnp1
      cp(l-1) = -(b1*cp(l)+c1*cp(l+1))/a1
      go to 30
      end
      subroutine dnlft (m,n,theta,cp,pb)
      double precision cp(*),pb,theta,cdt,sdt,cth,sth,chh
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      nmod=mod(n,2)
      mmod=mod(m,2)
      if(nmod)1,1,2
1     if(mmod)3,3,4
c
c     n even, m even
c
3     kdo=n/2
      pb = .5*cp(1)
      if(n .eq. 0) return
      cth = cdt
      sth = sdt
      do 170 k=1,kdo
c     pb = pb+cp(k+1)*dcos(2*k*theta)
      pb = pb+cp(k+1)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  170 continue
      return
c
c     n even, m odd
c
    4 kdo = n/2
      pb = 0.
      cth = cdt
      sth = sdt
      do 180 k=1,kdo
c     pb = pb+cp(k)*dsin(2*k*theta)
      pb = pb+cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  180 continue
      return
2     if(mmod)13,13,14
c
c     n odd, m even
c
13    kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 190 k=1,kdo
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
      pb = pb+cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  190 continue
      return
c
c     n odd, m odd
c
   14 kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 200 k=1,kdo
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
      pb = pb+cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  200 continue
      return
      end
      subroutine dnlftd (m,n,theta,cp,pb)
c
c     computes the derivative of pmn(theta) with respect to theta
c
c      real cp(1) ! bma
      double precision cp(1),pb,theta,cdt,sdt,cth,sth,chh
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      nmod=mod(n,2)
      mmod=mod(abs(m),2)
      if(nmod)1,1,2
1     if(mmod)3,3,4
c
c     n even, m even
c
3     kdo=n/2
      pb = 0.d0
      if(n .eq. 0) return
      cth = cdt
      sth = sdt
      do 170 k=1,kdo
c     pb = pb+cp(k+1)*dcos(2*k*theta)
      pb = pb-2.d0*k*cp(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  170 continue
      return
c
c     n even, m odd
c
    4 kdo = n/2
      pb = 0.
      cth = cdt
      sth = sdt
      do 180 k=1,kdo
c     pb = pb+cp(k)*dsin(2*k*theta)
      pb = pb+2.d0*k*cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  180 continue
      return
2     if(mmod)13,13,14
c
c     n odd, m even
c
13    kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 190 k=1,kdo
c     pb = pb+cp(k)*dcos((2*k-1)*theta)
      pb = pb-(2.d0*k-1)*cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  190 continue
      return
c
c     n odd, m odd
c
   14 kdo = (n+1)/2
      pb = 0.
      cth = dcos(theta)
      sth = dsin(theta)
      do 200 k=1,kdo
c     pb = pb+cp(k)*dsin((2*k-1)*theta)
      pb = pb+(2.d0*k-1)*cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  200 continue
      return
      end
      subroutine legin(mode,l,nlat,m,w,pmn,km)
c     this subroutine computes legendre polynomials for n=m,...,l-1
c     and  i=1,...,late (late=((nlat+mod(nlat,2))/2)gaussian grid
c     in pmn(n+1,i,km) using swarztrauber's recursion formula.
c     the vector w contains quantities precomputed in shigc.
c     legin must be called in the order m=0,1,...,l-1
c     (e.g., if m=10 is sought it must be preceded by calls with
c     m=0,1,2,...,9 in that order)
      real w(1),pmn(1)
c     set size of pole to equator gaussian grid
      late = (nlat+mod(nlat,2))/2
c     partition w (set pointers for p0n,p1n,abel,bbel,cbel,pmn)
      i1 = 1+nlat
      i2 = i1+nlat*late
      i3 = i2+nlat*late
      i4 = i3+(2*nlat-l)*(l-1)/2
      i5 = i4+(2*nlat-l)*(l-1)/2
      call legin1(mode,l,nlat,late,m,w(i1),w(i2),w(i3),w(i4),
     1            w(i5),pmn,km)
      return
      end
      subroutine legin1(mode,l,nlat,late,m,p0n,p1n,abel,bbel,cbel,
     1                  pmn,km)
      real p0n(nlat,late),p1n(nlat,late)
      real abel(1),bbel(1),cbel(1),pmn(nlat,late,3)
      data km0,km1,km2/ 1,2,3/
      save km0,km1,km2
c     define index function used in storing triangular
c     arrays for recursion coefficients (functions of (m,n))
c     for 2.le.m.le.n-1 and 2.le.n.le.l-1
      indx(m,n) = (n-1)*(n-2)/2+m-1
c     for l.le.n.le.nlat and 2.le.m.le.l
      imndx(m,n) = l*(l-1)/2+(n-l-1)*(l-1)+m-1

c     set do loop indices for full or half sphere
      ms = m+1
      ninc = 1
      if (mode.eq.1) then
c     only compute pmn for n-m odd
      ms = m+2
      ninc = 2
      else if (mode.eq.2) then
c     only compute pmn for n-m even
      ms = m+1
      ninc = 2
      end if


      if (m.gt.1) then
      do 100 np1=ms,nlat,ninc
      n = np1-1
      imn = indx(m,n)
      if (n.ge.l) imn = imndx(m,n)
      do 100 i=1,late
      pmn(np1,i,km0) = abel(imn)*pmn(n-1,i,km2)
     1            +bbel(imn)*pmn(n-1,i,km0)
     2            -cbel(imn)*pmn(np1,i,km2)
  100 continue

      else if (m.eq.0) then
      do 101 np1=ms,nlat,ninc
      do 101 i=1,late
      pmn(np1,i,km0) = p0n(np1,i)
  101 continue

      else if (m.eq.1) then
      do 102 np1=ms,nlat,ninc
      do 102 i=1,late
      pmn(np1,i,km0) = p1n(np1,i)
  102 continue
      end if

c     permute column indices
c     km0,km1,km2 store m,m-1,m-2 columns
      kmt = km0
      km0 = km2
      km2 = km1
      km1 = kmt
c     set current m index in output param km
      km = kmt
      return
      end


      subroutine zfin (isym,nlat,nlon,m,z,i3,wzfin)
      real       z(1)        ,wzfin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,nlon/2+1)
      labc = ((mmax-2)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of wzfin is 2*lim+3*labc
c
      call zfin1 (isym,nlat,m,z,imid,i3,wzfin,wzfin(iw1),wzfin(iw2),
     1            wzfin(iw3),wzfin(iw4))
      return
      end
      subroutine zfin1 (isym,nlat,m,z,imid,i3,zz,z1,a,b,c)
      real       z(imid,nlat,3),zz(imid,1),z1(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-1)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=1,nlat
      do 45 i=1,imid
      z(i,np1,i3) = zz(i,np1)
   45 continue
      return
   30 do 50 np1=2,nlat
      do 50 i=1,imid
      z(i,np1,i3) = z1(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(isym .eq. 1) go to 36
      do 85 i=1,imid
      z(i,m+1,i3) = a(ns)*z(i,m-1,i1)-c(ns)*z(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(isym .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      z(i,m+2,i3) = a(ns)*z(i,m,i1)-c(ns)*z(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(isym .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(isym .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      z(i,np1,i3) = a(ns)*z(i,np1-2,i1)+b(ns)*z(i,np1-2,i3)
     1                              -c(ns)*z(i,np1,i1)
   75 continue
   80 return
      end
      subroutine zfinit (nlat,nlon,wzfin,dwork)
      real       wzfin(*)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wzfin is 3*((l-3)*l+2)/2 + 2*l*imid
c     the length of dwork is nlat+2
c
      call zfini1 (nlat,nlon,imid,wzfin,wzfin(iw1),dwork,
     1                                       dwork(nlat/2+1))
      return
      end
      subroutine zfini1 (nlat,nlon,imid,z,abc,cz,work)
c
c     abc must have 3*((mmax-2)*(nlat+nlat-mmax-1))/2 locations
c     where mmax = min0(nlat,nlon/2+1)
c     cz and work must each have nlat+1 locations
c
      real z(imid,nlat,2),abc(1)
      double precision pi,dt,th,zh,cz(*),work(*)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      do 160 mp1=1,2
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dnzfk(nlat,m,n,cz,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dnzft(nlat,m,n,th,cz,zh)
      z(i,np1,mp1) = zh
  165 continue
      z(1,np1,mp1) = .5*z(1,np1,mp1)
  160 continue
      call rabcp(nlat,nlon,abc)
      return
      end
      subroutine dnzfk(nlat,m,n,cz,work)
c
c     dnzfk computes the coefficients in the trigonometric
c     expansion of the z functions that are used in spherical
c     harmonic analysis.
c
c      dimension  cz(1),work(1) ! bma
c
c     cz and work must both have nlat/2+1 locations
c
      double precision sum,sc1,t1,t2,work(1),cz(1)
      lc = (nlat+1)/2
      sc1 = 2.d0/float(nlat-1)
      call dnlfk(m,n,work)
      nmod = mod(n,2)
      mmod = mod(m,2)
      if(nmod)1,1,2
1     if(mmod)3,3,4
c
c     n even, m even
c
3     kdo = n/2+1
      do 5 idx=1,lc
      i = idx+idx-2
      sum = work(1)/(1.d0-i*i)
      if(kdo.lt.2) go to 29
      do 6 kp1=2,kdo
      k = kp1-1
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
8     sum = sum+work(kp1)*(t1+t2)/(t1*t2)
6     continue
29    cz(idx) = sc1*sum
5     continue
      return
c
c     n even, m odd
c
4     kdo = n/2
      do 9 idx=1,lc
      i = idx+idx-2
      sum = 0.
      do 101 k=1,kdo
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
12    sum=sum+work(k)*(t1-t2)/(t1*t2)
101   continue
      cz(idx) = sc1*sum
9     continue
      return
2     if(mmod)13,13,14
c
c     n odd, m even
c
13    kdo = (n+1)/2
      do 15 idx=1,lc
      i = idx+idx-1
      sum = 0.
      do 16 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
18    sum=sum+work(k)*(t1+t2)/(t1*t2)
16    continue
      cz(idx)=sc1*sum
15    continue
      return
c
c     n odd, m odd
c
14    kdo = (n+1)/2
      do 19 idx=1,lc
      i = idx+idx-3
      sum=0.
      do 20 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
22    sum=sum+work(k)*(t1-t2)/(t1*t2)
20    continue
      cz(idx)=sc1*sum
19    continue
      return
      end
      subroutine dnzft(nlat,m,n,th,cz,zh)
c      dimension cz(1) ! bma
      double precision cz(1),zh,th,cdt,sdt,cth,sth,chh
      zh = 0.
      cdt = dcos(th+th)
      sdt = dsin(th+th)
      lmod = mod(nlat,2)
      mmod = mod(m,2)
      nmod = mod(n,2)
      if(lmod)20,20,10
   10 lc = (nlat+1)/2
      lq = lc-1
      ls = lc-2
      if(nmod)1,1,2
    1 if(mmod)3,3,4
c
c     nlat odd n even m even
c
    3 zh = .5*(cz(1)+cz(lc)*dcos(2*lq*th))
      cth = cdt
      sth = sdt
      do 201 k=2,lq
c     zh = zh+cz(k)*dcos(2*(k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  201 continue
      return
c
c     nlat odd n even m odd
c
    4 cth = cdt
      sth = sdt
      do 202 k=1,ls
c     zh = zh+cz(k+1)*dsin(2*k*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  202 continue
      return
c
c     nlat odd n odd, m even
c
    2 if(mmod)5,5,6
    5 cth = dcos(th)
      sth = dsin(th)
      do 203 k=1,lq
c     zh = zh+cz(k)*dcos((2*k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  203 continue
      return
c
c     nlat odd n odd m odd
c
    6 cth = dcos(th)
      sth = dsin(th)
      do 204 k=1,lq
c     zh = zh+cz(k+1)*dsin((2*k-1)*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  204 continue
      return
   20 lc = nlat/2
      lq = lc-1
      if(nmod)30,30,80
   30 if(mmod)40,40,60
c
c     nlat even n even m even
c
   40 zh = .5*cz(1)
      cth = cdt
      sth = sdt
      do 50 k=2,lc
c     zh = zh+cz(k)*dcos(2*(k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   50 continue
      return
c
c     nlat even n even m odd
c
   60 cth = cdt
      sth = sdt
      do 70 k=1,lq
c     zh = zh+cz(k+1)*dsin(2*k*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   70 continue
      return
c
c     nlat even n odd m even
c
   80 if(mmod)90,90,110
   90 zh = .5*cz(lc)*dcos((nlat-1)*th)
      cth = dcos(th)
      sth = dsin(th)
      do 100 k=1,lq
c     zh = zh+cz(k)*dcos((2*k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  100 continue
      return
c
c     nlat even n odd m odd
c
  110 cth = dcos(th)
      sth = dsin(th)
      do 120 k=1,lq
c     zh = zh+cz(k+1)*dsin((2*k-1)*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  120 continue
      return
      end
      subroutine alin (isym,nlat,nlon,m,p,i3,walin)
      real       p(1)        ,walin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,nlon/2+1)
      labc = ((mmax-2)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of walin is ((5*l-7)*l+6)/2
c
      call alin1 (isym,nlat,m,p,imid,i3,walin,walin(iw1),walin(iw2),
     1            walin(iw3),walin(iw4))
      return
      end
      subroutine alin1 (isym,nlat,m,p,imid,i3,pz,p1,a,b,c)
      real       p(imid,nlat,3),pz(imid,1),p1(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-1)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=1,nlat
      do 45 i=1,imid
      p(i,np1,i3) = pz(i,np1)
   45 continue
      return
   30 do 50 np1=2,nlat
      do 50 i=1,imid
      p(i,np1,i3) = p1(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(isym .eq. 1) go to 36
      do 85 i=1,imid
      p(i,m+1,i3) = a(ns)*p(i,m-1,i1)-c(ns)*p(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(isym .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      p(i,m+2,i3) = a(ns)*p(i,m,i1)-c(ns)*p(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(isym .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(isym .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      p(i,np1,i3) = a(ns)*p(i,np1-2,i1)+b(ns)*p(i,np1-2,i3)
     1                              -c(ns)*p(i,np1,i1)
   75 continue
   80 return
      end
      subroutine alinit (nlat,nlon,walin,dwork)
      real       walin(*)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of walin is 3*((l-3)*l+2)/2 + 2*l*imid
c     the length of work is nlat+1
c
      call alini1 (nlat,nlon,imid,walin,walin(iw1),dwork)
      return
      end
      subroutine alini1 (nlat,nlon,imid,p,abc,cp)
      real p(imid,nlat,2),abc(1)
      double precision pi,dt,th,cp(1),ph
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      do 160 mp1=1,2
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dnlfk (m,n,cp)
      do 160 i=1,imid
      th = (i-1)*dt
      call dnlft (m,n,th,cp,ph)
      p(i,np1,mp1) = ph
  160 continue
      call rabcp(nlat,nlon,abc)
      return
      end
      subroutine rabcp(nlat,nlon,abc)
c
c     subroutine rabcp computes the coefficients in the recurrence
c     relation for the associated legendre fuctions. array abc
c     must have 3*((mmax-2)*(nlat+nlat-mmax-1))/2 locations.
c
      real abc(1)
      mmax = min0(nlat,nlon/2+1)
      labc = ((mmax-2)*(nlat+nlat-mmax-1))/2
      iw1 = labc+1
      iw2 = iw1+labc
      call rabcp1(nlat,nlon,abc,abc(iw1),abc(iw2))
      return
      end
      subroutine rabcp1(nlat,nlon,a,b,c)
c
c     coefficients a, b, and c for computing pbar(m,n,theta) are
c     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
c
      real a(1),b(1),c(1)
      mmax = min0(nlat,nlon/2+1)
      do 215 mp1=3,mmax
      m = mp1-1
      ns = ((m-2)*(nlat+nlat-m-1))/2+1
      fm = float(m)
      tm = fm+fm
      temp = tm*(tm-1.)
      a(ns) = sqrt((tm+1.)*(tm-2.)/temp)
      c(ns) = sqrt(2./temp)
      if(m .eq. nlat-1) go to 215
      ns = ns+1
      temp = tm*(tm+1.)
      a(ns) = sqrt((tm+3.)*(tm-2.)/temp)
      c(ns) = sqrt(6./temp)
      mp3 = m+3
      if(mp3 .gt. nlat) go to 215
      do 210 np1=mp3,nlat
      n = np1-1
      ns = ns+1
      fn = float(n)
      tn = fn+fn
      cn = (tn+1.)/(tn-3.)
      fnpm = fn+fm
      fnmm = fn-fm
      temp = fnpm*(fnpm-1.)
      a(ns) = sqrt(cn*(fnpm-3.)*(fnpm-2.)/temp)
      b(ns) = sqrt(cn*fnmm*(fnmm-1.)/temp)
      c(ns) = sqrt((fnmm+1.)*(fnmm+2.)/temp)
  210 continue
  215 continue
      return
      end
      subroutine sea1(nlat,nlon,imid,z,idz,zin,wzfin,dwork)
      real z(idz,*),zin(imid,nlat,3),wzfin(*)
      double precision dwork(*)
      call zfinit(nlat,nlon,wzfin,dwork)
      mmax = min0(nlat,nlon/2+1)
      do 33 mp1=1,mmax
      m = mp1-1
      call zfin (0,nlat,nlon,m,zin,i3,wzfin)
      do 33 np1=mp1,nlat
      mn = m*(nlat-1)-(m*(m-1))/2+np1
      do 33 i=1,imid
      z(mn,i) = zin(i,np1,i3)
   33 continue
      return
      end
      subroutine ses1(nlat,nlon,imid,p,pin,walin,dwork)
      real p(imid,*),pin(imid,nlat,3),walin(*)
      double precision dwork(*)
      call alinit (nlat,nlon,walin,dwork)
      mmax = min0(nlat,nlon/2+1)
      do 10 mp1=1,mmax
      m = mp1-1
      call alin(0,nlat,nlon,m,pin,i3,walin)
      do 10 np1=mp1,nlat
      mn = m*(nlat-1)-(m*(m-1))/2+np1
      do 10 i=1,imid
      p(i,mn) = pin(i,np1,i3)
   10 continue
      return
      end
      subroutine zvinit (nlat,nlon,wzvin,dwork)
      real       wzvin(1)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wzvin is 
c         2*nlat*imid +3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     the length of dwork is nlat+2
c
      call zvini1 (nlat,nlon,imid,wzvin,wzvin(iw1),dwork,
     1                                    dwork(nlat/2+2))
      return
      end
      subroutine zvini1 (nlat,nlon,imid,zv,abc,czv,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     czv and work must each have nlat/2+1  locations
c
      real zv(imid,nlat,2),abc(1)
      double precision pi,dt,czv(1),zvh,th,work(1)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(2,nlat,(nlon+1)/2)
      do 160 mp1=1,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dzvk(nlat,m,n,czv,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dzvt(nlat,m,n,th,czv,zvh)
      zv(i,np1,mp1) = zvh
  165 continue
      zv(1,np1,mp1) = .5*zv(1,np1,mp1)
  160 continue
      call rabcv(nlat,nlon,abc)
      return
      end
      subroutine zwinit (nlat,nlon,wzwin,dwork)
      real       wzwin(1)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wzvin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      call zwini1 (nlat,nlon,imid,wzwin,wzwin(iw1),dwork,
     1                                        dwork(nlat/2+2))
      return
      end
      subroutine zwini1 (nlat,nlon,imid,zw,abc,czw,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     czw and work must each have nlat+1 locations
c
      real zw(imid,nlat,2),abc(1)
      double precision  pi,dt,czw(1),zwh,th,work(1)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(3,nlat,(nlon+1)/2)
      if(mdo .lt. 2) return
      do 160 mp1=2,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dzwk(nlat,m,n,czw,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dzwt(nlat,m,n,th,czw,zwh)
      zw(i,np1,m) = zwh
  165 continue
      zw(1,np1,m) = .5*zw(1,np1,m)
  160 continue
      call rabcw(nlat,nlon,abc)
      return
      end
      subroutine zvin (ityp,nlat,nlon,m,zv,i3,wzvin)
      real       zv(1)        ,wzvin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of wzvin is 2*lim+3*labc
c
      call zvin1 (ityp,nlat,m,zv,imid,i3,wzvin,wzvin(iw1),wzvin(iw2),
     1            wzvin(iw3),wzvin(iw4))
      return
      end
      subroutine zvin1 (ityp,nlat,m,zv,imid,i3,zvz,zv1,a,b,c)
      real       zv(imid,nlat,3),zvz(imid,1),zv1(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-1)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=1,nlat
      do 45 i=1,imid
      zv(i,np1,i3) = zvz(i,np1)
   45 continue
      return
   30 do 50 np1=2,nlat
      do 50 i=1,imid
      zv(i,np1,i3) = zv1(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(ityp .eq. 1) go to 36
      do 85 i=1,imid
      zv(i,m+1,i3) = a(ns)*zv(i,m-1,i1)-c(ns)*zv(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(ityp .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      zv(i,m+2,i3) = a(ns)*zv(i,m,i1)-c(ns)*zv(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(ityp .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(ityp .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      zv(i,np1,i3) = a(ns)*zv(i,np1-2,i1)+b(ns)*zv(i,np1-2,i3)
     1                              -c(ns)*zv(i,np1,i1)
   75 continue
   80 return
      end
      subroutine zwin (ityp,nlat,nlon,m,zw,i3,wzwin)
      real       zw(1)        ,wzwin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of wzwin is 2*lim+3*labc
c
      call zwin1 (ityp,nlat,m,zw,imid,i3,wzwin,wzwin(iw1),wzwin(iw2),
     1            wzwin(iw3),wzwin(iw4))
      return
      end
      subroutine zwin1 (ityp,nlat,m,zw,imid,i3,zw1,zw2,a,b,c)
      real       zw(imid,nlat,3),zw1(imid,1),zw2(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-2)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=2,nlat
      do 45 i=1,imid
      zw(i,np1,i3) = zw1(i,np1)
   45 continue
      return
   30 do 50 np1=3,nlat
      do 50 i=1,imid
      zw(i,np1,i3) = zw2(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(ityp .eq. 1) go to 36
      do 85 i=1,imid
      zw(i,m+1,i3) = a(ns)*zw(i,m-1,i1)-c(ns)*zw(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(ityp .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      zw(i,m+2,i3) = a(ns)*zw(i,m,i1)-c(ns)*zw(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(ityp .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(ityp .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      zw(i,np1,i3) = a(ns)*zw(i,np1-2,i1)+b(ns)*zw(i,np1-2,i3)
     1                              -c(ns)*zw(i,np1,i1)
   75 continue
   80 return
      end
      subroutine vbinit (nlat,nlon,wvbin,dwork)
      real wvbin(1)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      call vbini1 (nlat,nlon,imid,wvbin,wvbin(iw1),dwork,
     1                                       dwork(nlat/2+2))
      return
      end
      subroutine vbini1 (nlat,nlon,imid,vb,abc,cvb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat+1 locations
c
      real vb(imid,nlat,2),abc(1)
      double precision pi,dt,cvb(1),th,vbh,work(1)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(2,nlat,(nlon+1)/2)
      do 160 mp1=1,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dvbk(m,n,cvb,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dvbt(m,n,th,cvb,vbh)
      vb(i,np1,mp1) = vbh
  165 continue
  160 continue
      call rabcv(nlat,nlon,abc)
      return
      end
      subroutine wbinit (nlat,nlon,wwbin,dwork)
      real       wwbin(1)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      call wbini1 (nlat,nlon,imid,wwbin,wwbin(iw1),dwork,
     1                                        dwork(nlat/2+2))
      return
      end
      subroutine wbini1 (nlat,nlon,imid,wb,abc,cwb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cwb and work must each have nlat/2+1 locations
c
      real wb(imid,nlat,2),abc(1)
      double precision pi,dt,cwb(1),wbh,th,work(1)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(3,nlat,(nlon+1)/2)
      if(mdo .lt. 2) return
      do 160 mp1=2,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dwbk(m,n,cwb,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dwbt(m,n,th,cwb,wbh)
      wb(i,np1,m) = wbh
  165 continue
  160 continue
      call rabcw(nlat,nlon,abc)
      return
      end
      subroutine vbin (ityp,nlat,nlon,m,vb,i3,wvbin)
      real       vb(1)        ,wvbin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of wvbin is 2*lim+3*labc
c
      call vbin1 (ityp,nlat,m,vb,imid,i3,wvbin,wvbin(iw1),wvbin(iw2),
     1            wvbin(iw3),wvbin(iw4))
      return
      end
      subroutine vbin1 (ityp,nlat,m,vb,imid,i3,vbz,vb1,a,b,c)
      real       vb(imid,nlat,3),vbz(imid,1),vb1(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-1)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=1,nlat
      do 45 i=1,imid
      vb(i,np1,i3) = vbz(i,np1)
   45 continue
      return
   30 do 50 np1=2,nlat
      do 50 i=1,imid
      vb(i,np1,i3) = vb1(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(ityp .eq. 1) go to 36
      do 85 i=1,imid
      vb(i,m+1,i3) = a(ns)*vb(i,m-1,i1)-c(ns)*vb(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(ityp .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      vb(i,m+2,i3) = a(ns)*vb(i,m,i1)-c(ns)*vb(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(ityp .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(ityp .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      vb(i,np1,i3) = a(ns)*vb(i,np1-2,i1)+b(ns)*vb(i,np1-2,i3)
     1                              -c(ns)*vb(i,np1,i1)
   75 continue
   80 return
      end
      subroutine wbin (ityp,nlat,nlon,m,wb,i3,wwbin)
      real       wb(1)        ,wwbin(1)
      imid = (nlat+1)/2
      lim = nlat*imid
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = lim+1
      iw2 = iw1+lim
      iw3 = iw2+labc
      iw4 = iw3+labc
c
c     the length of wwbin is 2*lim+3*labc
c
      call wbin1 (ityp,nlat,m,wb,imid,i3,wwbin,wwbin(iw1),wwbin(iw2),
     1            wwbin(iw3),wwbin(iw4))
      return
      end
      subroutine wbin1 (ityp,nlat,m,wb,imid,i3,wb1,wb2,a,b,c)
      real       wb(imid,nlat,3),wb1(imid,1),wb2(imid,1),
     1                a(1),b(1),c(1)
      save i1,i2
      ihold = i1
      i1 = i2
      i2 = i3
      i3 = ihold
      if(m-2)25,30,35
   25 i1 = 1
      i2 = 2
      i3 = 3
      do 45 np1=2,nlat
      do 45 i=1,imid
      wb(i,np1,i3) = wb1(i,np1)
   45 continue
      return
   30 do 50 np1=3,nlat
      do 50 i=1,imid
      wb(i,np1,i3) = wb2(i,np1)
   50 continue
      return
   35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
      if(ityp .eq. 1) go to 36
      do 85 i=1,imid
      wb(i,m+1,i3) = a(ns)*wb(i,m-1,i1)-c(ns)*wb(i,m+1,i1)
   85 continue
   36 if(m .eq. nlat-1) return
      if(ityp .eq. 2) go to 71
      ns = ns+1
      do 70 i=1,imid
      wb(i,m+2,i3) = a(ns)*wb(i,m,i1)-c(ns)*wb(i,m+2,i1)
   70 continue
   71 nstrt = m+3
      if(ityp .eq. 1) nstrt = m+4
      if(nstrt .gt. nlat) go to 80
      nstp = 2
      if(ityp .eq. 0) nstp = 1
      do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
      wb(i,np1,i3) = a(ns)*wb(i,np1-2,i1)+b(ns)*wb(i,np1-2,i3)
     1                              -c(ns)*wb(i,np1,i1)
   75 continue
   80 return
      end
       subroutine dzvk(nlat,m,n,czv,work)
c
c     subroutine dzvk computes the coefficients in the trigonometric
c     expansion of the quadrature function zvbar(n,m,theta)
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c
c     n      the degree (subscript) of wbarv(n,m,theta)
c
c     m      the order (superscript) of wbarv(n,m,theta)
c
c     work   a work array with at least nlat/2+1 locations
c
c     output parameter
c
c     czv     the fourier coefficients of zvbar(n,m,theta).
c
c      dimension czv(1),work(1) ! bma
      double precision czv(1),sc1,sum,work(1),t1,t2
      if(n .le. 0) return
      lc = (nlat+1)/2
      sc1 = 2.d0/float(nlat-1)
      call dvbk(m,n,work,czv)
      nmod = mod(n,2)
      mmod = mod(m,2)
      if(nmod .ne. 0) go to 1
      if(mmod .ne. 0) go to 2
c
c     n even, m even
c
      kdo = n/2
      do 9 id=1,lc
      i = id+id-2
      sum = 0.
      do 10 k=1,kdo
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
      sum = sum+work(k)*(t1-t2)/(t1*t2)
   10 continue
      czv(id) = sc1*sum
    9 continue
      return
c
c     n even, m odd
c
    2 kdo = n/2
      do 5 id=1,lc
      i = id+id-2
      sum = 0.
      do 6 k=1,kdo
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
      sum = sum+work(k)*(t1+t2)/(t1*t2)
    6 continue
      czv(id) = sc1*sum
    5 continue
      return
    1 if(mmod .ne. 0) go to 3
c
c     n odd, m even
c
      kdo = (n+1)/2
      do 19 id=1,lc
      i = id+id-3
      sum = 0.
      do 20 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
      sum = sum+work(k)*(t1-t2)/(t1*t2)
   20 continue
      czv(id) = sc1*sum
   19 continue
      return
c
c     n odd, m odd
c
    3 kdo = (n+1)/2
      do 15 id=1,lc
      i = id+id-1
      sum = 0.
      do 16 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
      sum = sum+work(k)*(t1+t2)/(t1*t2)
   16 continue
      czv(id) = sc1*sum
   15 continue
      return
      end
      subroutine dzvt(nlat,m,n,th,czv,zvh)
c
c     subroutine dzvt tabulates the function zvbar(n,m,theta)
c     at theta = th in double precision
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c
c     n      the degree (subscript) of zvbar(n,m,theta)
c
c     m      the order (superscript) of zvbar(n,m,theta)
c
c     czv     the fourier coefficients of zvbar(n,m,theta)
c             as computed by subroutine zwk.
c
c     output parameter
c
c     zvh     zvbar(m,n,theta) evaluated at theta = th
c
c      dimension czv(1) ! bma
      double precision th,czv(1),zvh,cth,sth,cdt,sdt,chh
      zvh = 0.
      if(n .le. 0) return
      lc = (nlat+1)/2
      lq = lc-1
      ls = lc-2
      cth = dcos(th)
      sth = dsin(th)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      lmod = mod(nlat,2)
      mmod = mod(m,2)
      nmod = mod(n,2)
      if(lmod .eq. 0) go to 50
      if(nmod .ne. 0) go to 1
      cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 2
c
c     nlat odd  n even  m even
c
      do 10 k=1,ls
      zvh = zvh+czv(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     nlat odd  n even  m odd
c
    2 zvh = .5*czv(1)
      do 20 k=2,lq
      zvh = zvh+czv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      zvh = zvh+.5*czv(lc)*dcos((nlat-1)*th)
      return
    1 if(mmod .ne. 0) go to 3
c
c     nlat odd  n odd  m even
c
      do 30 k=1,lq
      zvh = zvh+czv(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   30 continue
      return
c
c     nlat odd  n odd  m odd
c
    3 do 40 k=1,lq
      zvh = zvh+czv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   40 continue
      return
   50 if(nmod .ne. 0) go to 51
      cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 52
c
c     nlat even  n even  m even
c
      do 55 k=1,lq
      zvh = zvh+czv(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   55 continue
      return
c
c     nlat even  n even  m odd
c
   52 zvh = .5*czv(1)
      do 57 k=2,lc
      zvh = zvh+czv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   57 continue
      return
   51 if(mmod .ne. 0) go to 53
c
c     nlat even  n odd  m even
c
      do 58 k=1,lq
      zvh = zvh+czv(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   58 continue
      return
c
c     nlat even  n odd  m odd
c
   53 zvh = .5*czv(lc)*dcos((nlat-1)*th)
      do 60 k=1,lq
      zvh = zvh+czv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   60 continue
      return
      end
      subroutine dzwk(nlat,m,n,czw,work)
c
c     subroutine dzwk computes the coefficients in the trigonometric
c     expansion of the quadrature function zwbar(n,m,theta)
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c
c     n      the degree (subscript) of zwbar(n,m,theta)
c
c     m      the order (superscript) of zwbar(n,m,theta)
c
c     work   a work array with at least nlat/2+1 locations
c
c     output parameter
c
c     czw     the fourier coefficients of zwbar(n,m,theta).
c
c      dimension czw(1),work(1) ! bma
      double precision czw(1),work(1),sc1,sum,t1,t2
      if(n .le. 0) return
      lc = (nlat+1)/2
      sc1 = 2.d0/float(nlat-1)
      call dwbk(m,n,work,czw)
      nmod = mod(n,2)
      mmod = mod(m,2)
      if(nmod .ne. 0) go to 1
      if(mmod .ne. 0) go to 2
c
c     n even, m even
c
      kdo = n/2
      do 19 id=1,lc
      i = id+id-3
      sum = 0.
      do 20 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
      sum = sum+work(k)*(t1-t2)/(t1*t2)
   20 continue
      czw(id) = sc1*sum
   19 continue
      return
c
c     n even, m odd
c
    2 kdo = n/2
      do 15 id=1,lc
      i = id+id-1
      sum = 0.
      do 16 k=1,kdo
      t1 = 1.d0-(k+k-1+i)**2
      t2 = 1.d0-(k+k-1-i)**2
      sum = sum+work(k)*(t1+t2)/(t1*t2)
   16 continue
      czw(id) = sc1*sum
   15 continue
      return
    1 if(mmod .ne. 0) go to 3
c
c     n odd, m even
c
      kdo = (n-1)/2
      do 9 id=1,lc
      i = id+id-2
      sum = 0.
      do 10 k=1,kdo
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
      sum = sum+work(k)*(t1-t2)/(t1*t2)
   10 continue
      czw(id) = sc1*sum
    9 continue
      return
c
c     n odd, m odd
c
    3 kdo = (n+1)/2
      do 5 id=1,lc
      i = id+id-2
      sum = work(1)/(1.d0-i*i)
      if(kdo .lt. 2) go to 29
      do 6 kp1=2,kdo
      k = kp1-1
      t1 = 1.d0-(k+k+i)**2
      t2 = 1.d0-(k+k-i)**2
      sum = sum+work(kp1)*(t1+t2)/(t1*t2)
    6 continue
   29 czw(id) = sc1*sum
    5 continue
      return
      end
      subroutine dzwt(nlat,m,n,th,czw,zwh)
c
c     subroutine dzwt tabulates the function zwbar(n,m,theta)
c     at theta = th in double precision
c
c     input parameters
c
c     nlat      the number of colatitudes including the poles.
c            nlat must be an odd integer
c
c     n      the degree (subscript) of zwbar(n,m,theta)
c
c     m      the order (superscript) of zwbar(n,m,theta)
c
c     czw     the fourier coefficients of zwbar(n,m,theta)
c             as computed by subroutine zwk.
c
c     output parameter
c
c     zwh     zwbar(m,n,theta) evaluated at theta = th
c
c      dimension czw(1) ! bma
      double precision czw(1),zwh,th,cth,sth,cdt,sdt,chh
      zwh = 0.
      if(n .le. 0) return
      lc = (nlat+1)/2
      lq = lc-1
      ls = lc-2
      cth = dcos(th)
      sth = dsin(th)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      lmod = mod(nlat,2)
      mmod = mod(m,2)
      nmod = mod(n,2)
      if(lmod .eq. 0) go to 50
      if(nmod .ne. 0) go to 1
      if(mmod .ne. 0) go to 2
c
c     nlat odd  n even  m even
c
      do 30 k=1,lq
      zwh = zwh+czw(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   30 continue
      return
c
c     nlat odd  n even  m odd
c
    2 do 40 k=1,lq
      zwh = zwh+czw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   40 continue
      return
    1 cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 3
c
c     nlat odd  n odd  m even
c
      do 10 k=1,ls
      zwh = zwh+czw(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     nlat odd  n odd  m odd
c
    3 zwh = .5*czw(1)
      do 20 k=2,lq
      zwh = zwh+czw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      zwh = zwh+.5*czw(lc)*dcos((nlat-1)*th)
      return
   50 if(nmod .ne. 0) go to 51
      if(mmod .ne. 0) go to 52
c
c     nlat even  n even  m even
c
      do 55 k=1,lq
      zwh = zwh+czw(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   55 continue
      return
c
c     nlat even  n even  m odd
c
   52 zwh = .5*czw(lc)*dcos((nlat-1)*th)
      do 60 k=1,lq
      zwh = zwh+czw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   60 continue
      return
   51 cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 53
c
c     nlat even  n odd  m even
c
      do 65 k=1,lq
      zwh = zwh+czw(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   65 continue
      return
c
c     nlat even  n odd  m odd
c
   53 zwh = .5*czw(1)
      do 70 k=2,lc
      zwh = zwh+czw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   70 continue
      return
      end
      subroutine dvbk(m,n,cv,work)
      double precision cv(1),work(1),fn,fk,cf
      cv(1) = 0.
      if(n .le. 0) return
      fn = n
      srnp1 = dsqrt(fn*(fn+1.))
      cf = 2.*m/srnp1
      modn = mod(n,2)
      modm = mod(m,2)
      call dnlfk(m,n,work)
      if(modn .ne. 0) go to 70
      ncv = n/2
      if(ncv .eq. 0) return
      fk = 0.
      if(modm .ne. 0) go to 60
c
c     n even m even
c
      do 55 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*work(l+1)/srnp1
   55 continue
      return
c
c     n even m odd
c
   60 do 65 l=1,ncv
      fk = fk+2.
      cv(l) = fk*work(l)/srnp1
   65 continue
      return
   70 ncv = (n+1)/2
      fk = -1.
      if(modm .ne. 0) go to 80
c
c     n odd m even
c
      do 75 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*work(l)/srnp1
   75 continue
      return
c
c     n odd m odd
c
   80 do 85 l=1,ncv
      fk = fk+2.
      cv(l) = fk*work(l)/srnp1
   85 continue
      return
      end
      subroutine dwbk(m,n,cw,work)
      double precision cw(1),work(1),fn,cf,srnp1
      cw(1) = 0.
      if(n.le.0 .or. m.le.0) return
      fn = n
      srnp1 = dsqrt(fn*(fn+1.))
      cf = 2.*m/srnp1
      modn = mod(n,2)
      modm = mod(m,2)
      call dnlfk(m,n,work)
      if(m .eq. 0) go to 50
      if(modn .ne. 0) go to 30
      l = n/2
      if(l .eq. 0) go to 50
      if(modm .ne. 0) go to 20
c
c     n even m even
c
      cw(l) = -cf*work(l+1)
   10 l = l-1
      if(l .le. 0) go to 50
      cw(l) = cw(l+1)-cf*work(l+1)
      go to 10
c
c     n even m odd
c
   20 cw(l) = cf*work(l)
   25 l = l-1
      if(l .le. 0) go to 50
      cw(l) = cw(l+1)+cf*work(l)
      go to 25
   30 if(modm .ne. 0) go to 40
      l = (n-1)/2
      if(l .eq. 0) go to 50
c
c     n odd m even
c
      cw(l) = -cf*work(l+1)
   35 l = l-1
      if(l .le. 0) go to 50
      cw(l) = cw(l+1)-cf*work(l+1)
      go to 35
c
c     n odd m odd
c
   40 l = (n+1)/2
      cw(l) = cf*work(l)
   45 l = l-1
      if(l .le. 0) go to 50
      cw(l) = cw(l+1)+cf*work(l)
      go to 45
   50 return
      end
      subroutine dvbt(m,n,theta,cv,vh)
c      dimension cv(1) ! bma
      double precision cv(1),vh,theta,cth,sth,cdt,sdt,chh
      vh = 0.
      if(n.eq.0) return
      cth = dcos(theta)
      sth = dsin(theta)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      mmod = mod(m,2)
      nmod = mod(n,2)
      if(nmod .ne. 0) go to 1
      cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 2
c
c     n even  m even
c
      ncv = n/2
      do 10 k=1,ncv
      vh = vh+cv(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     n even  m odd
c
    2 ncv = n/2
      do 15 k=1,ncv
      vh = vh+cv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   15 continue
      return
    1 if(mmod .ne. 0) go to 3
c
c     n odd m even
c
      ncv = (n+1)/2
      do 20 k=1,ncv
      vh = vh+cv(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      return
c
c case m odd and n odd
c
    3 ncv = (n+1)/2
      do 25 k=1,ncv
      vh = vh+cv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   25 continue
      return
      end
      subroutine dwbt(m,n,theta,cw,wh)
c      dimension cw(1) ! bma
      double precision theta,cw(1),wh,cth,sth,cdt,sdt,chh
      wh = 0.
      if(n.le.0 .or. m.le.0) return
      cth = dcos(theta)
      sth = dsin(theta)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      mmod=mod(m,2)
      nmod=mod(n,2)
      if(nmod .ne. 0) go to 1
      if(mmod .ne. 0) go to 2
c
c     n even  m even
c
      ncw = n/2
      do 10 k=1,ncw
      wh = wh+cw(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     n even  m odd
c
    2 ncw = n/2
      do 8 k=1,ncw
      wh = wh+cw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
    8 continue
      return
    1 cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 3
c
c     n odd m even
c
      ncw = (n-1)/2
      do 20 k=1,ncw
      wh = wh+cw(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      return
c
c case m odd and n odd
c
    3 ncw = (n+1)/2
      wh = .5*cw(1)
      if(ncw.lt.2) return
      do 25 k=2,ncw
      wh = wh+cw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   25 continue
      return
      end
      subroutine rabcv(nlat,nlon,abc)
c
c     subroutine rabcp computes the coefficients in the recurrence
c     relation for the functions vbar(m,n,theta). array abc
c     must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2 locations.
c
      real abc(1)
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = labc+1
      iw2 = iw1+labc
      call rabcv1(nlat,nlon,abc,abc(iw1),abc(iw2))
      return
      end
      subroutine rabcv1(nlat,nlon,a,b,c)
c
c     coefficients a, b, and c for computing vbar(m,n,theta) are
c     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
c
      real a(1),b(1),c(1)
      mmax = min0(nlat,(nlon+1)/2)
      if(mmax .lt. 3) return
      do 215 mp1=3,mmax
      m = mp1-1
      ns = ((m-2)*(nlat+nlat-m-1))/2+1
      fm = float(m)
      tm = fm+fm
      temp = tm*(tm-1.)
      tpn = (fm-2.)*(fm-1.)/(fm*(fm+1.))
      a(ns) = sqrt(tpn*(tm+1.)*(tm-2.)/temp)
      c(ns) = sqrt(2./temp)
      if(m .eq. nlat-1) go to 215
      ns = ns+1
      temp = tm*(tm+1.)
      tpn = (fm-1.)*fm/((fm+1.)*(fm+2.))
      a(ns) = sqrt(tpn*(tm+3.)*(tm-2.)/temp)
      c(ns) = sqrt(6./temp)
      mp3 = m+3
      if(mp3 .gt. nlat) go to 215
      do 210 np1=mp3,nlat
      n = np1-1
      ns = ns+1
      fn = float(n)
      tn = fn+fn
      cn = (tn+1.)/(tn-3.)
      tpn = (fn-2.)*(fn-1.)/(fn*(fn+1.))
      fnpm = fn+fm
      fnmm = fn-fm
      temp = fnpm*(fnpm-1.)
      a(ns) = sqrt(tpn*cn*(fnpm-3.)*(fnpm-2.)/temp)
      b(ns) = sqrt(tpn*cn*fnmm*(fnmm-1.)/temp)
      c(ns) = sqrt((fnmm+1.)*(fnmm+2.)/temp)
  210 continue
  215 continue
      return
      end
      subroutine rabcw(nlat,nlon,abc)
c
c     subroutine rabcw computes the coefficients in the recurrence
c     relation for the functions wbar(m,n,theta). array abc
c     must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2 locations.
c
      real abc(1)
      mmax = min0(nlat,(nlon+1)/2)
      labc = (max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      iw1 = labc+1
      iw2 = iw1+labc
      call rabcw1(nlat,nlon,abc,abc(iw1),abc(iw2))
      return
      end
      subroutine rabcw1(nlat,nlon,a,b,c)
c
c     coefficients a, b, and c for computing wbar(m,n,theta) are
c     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
c
      real a(1),b(1),c(1)
      mmax = min0(nlat,(nlon+1)/2)
      if(mmax .lt. 4) return
      do 215 mp1=4,mmax
      m = mp1-1
      ns = ((m-2)*(nlat+nlat-m-1))/2+1
      fm = float(m)
      tm = fm+fm
      temp = tm*(tm-1.)
      tpn = (fm-2.)*(fm-1.)/(fm*(fm+1.))
      tph = fm/(fm-2.)
      a(ns) = tph*sqrt(tpn*(tm+1.)*(tm-2.)/temp)
      c(ns) = tph*sqrt(2./temp)
      if(m .eq. nlat-1) go to 215
      ns = ns+1
      temp = tm*(tm+1.)
      tpn = (fm-1.)*fm/((fm+1.)*(fm+2.))
      tph = fm/(fm-2.)
      a(ns) = tph*sqrt(tpn*(tm+3.)*(tm-2.)/temp)
      c(ns) = tph*sqrt(6./temp)
      mp3 = m+3
      if(mp3 .gt. nlat) go to 215
      do 210 np1=mp3,nlat
      n = np1-1
      ns = ns+1
      fn = float(n)
      tn = fn+fn
      cn = (tn+1.)/(tn-3.)
      fnpm = fn+fm
      fnmm = fn-fm
      temp = fnpm*(fnpm-1.)
      tpn = (fn-2.)*(fn-1.)/(fn*(fn+1.))
      tph = fm/(fm-2.)
      a(ns) = tph*sqrt(tpn*cn*(fnpm-3.)*(fnpm-2.)/temp)
      b(ns) = sqrt(tpn*cn*fnmm*(fnmm-1.)/temp)
      c(ns) = tph*sqrt((fnmm+1.)*(fnmm+2.)/temp)
  210 continue
  215 continue
      return
      end
      subroutine vtinit (nlat,nlon,wvbin,dwork)
      real       wvbin(*)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      call vtini1 (nlat,nlon,imid,wvbin,wvbin(iw1),dwork,
     1                                       dwork(nlat/2+2))
      return
      end
      subroutine vtini1 (nlat,nlon,imid,vb,abc,cvb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat/2+1 locations
c
      real vb(imid,nlat,2),abc(1)
      double precision pi,dt,cvb(1),th,vbh,work(*)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(2,nlat,(nlon+1)/2)
      do 160 mp1=1,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dvtk(m,n,cvb,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dvtt(m,n,th,cvb,vbh)
      vb(i,np1,mp1) = vbh
  165 continue
  160 continue
      call rabcv(nlat,nlon,abc)
      return
      end
      subroutine wtinit (nlat,nlon,wwbin,dwork)
      real       wwbin(1)
      double precision dwork(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of dwork is nlat+2
c
      call wtini1 (nlat,nlon,imid,wwbin,wwbin(iw1),dwork,
     1                                       dwork(nlat/2+2))
      return
      end
      subroutine wtini1 (nlat,nlon,imid,wb,abc,cwb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cwb and work must each have nlat/2+1 locations
c
      real wb(imid,nlat,2),abc(1)
      double precision pi,dt,cwb(*),wbh,th,work(*)
      pi = 4.*datan(1.d0)
      dt = pi/(nlat-1)
      mdo = min0(3,nlat,(nlon+1)/2)
      if(mdo .lt. 2) return
      do 160 mp1=2,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dwtk(m,n,cwb,work)
      do 165 i=1,imid
      th = (i-1)*dt
      call dwtt(m,n,th,cwb,wbh)
      wb(i,np1,m) = wbh
  165 continue
  160 continue
      call rabcw(nlat,nlon,abc)
      return
      end
      subroutine vtgint (nlat,nlon,theta,wvbin,work)
      real       wvbin(*)
      double precision theta(*), work(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      call vtgit1 (nlat,nlon,imid,theta,wvbin,wvbin(iw1),
     +                        work,work(nlat/2+2))
      return
      end
      subroutine vtgit1 (nlat,nlon,imid,theta,vb,abc,cvb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat/2+1   locations
c
      real vb(imid,nlat,2),abc(*)
      double precision theta(*),cvb(*),work(*),vbh
      mdo = min0(2,nlat,(nlon+1)/2)
      do 160 mp1=1,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dvtk(m,n,cvb,work)
      do 165 i=1,imid
      call dvtt(m,n,theta(i),cvb,vbh)
      vb(i,np1,mp1) = vbh
  165 continue
  160 continue
      call rabcv(nlat,nlon,abc)
      return
      end
      subroutine wtgint (nlat,nlon,theta,wwbin,work)
      real       wwbin(*)
      double precision theta(*), work(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      call wtgit1 (nlat,nlon,imid,theta,wwbin,wwbin(iw1),
     1                        work,work(nlat/2+2))
      return
      end
      subroutine wtgit1 (nlat,nlon,imid,theta,wb,abc,cwb,work)
c
c     abc must have 3*((nlat-3)*nlat+2)/2 locations
c     cwb and work must each have nlat/2+1 locations
c
      real wb(imid,nlat,2),abc(1)
      double precision theta(*), cwb(*), work(*), wbh
      mdo = min0(3,nlat,(nlon+1)/2)
      if(mdo .lt. 2) return
      do 160 mp1=2,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dwtk(m,n,cwb,work)
      do 165 i=1,imid
      call dwtt(m,n,theta(i),cwb,wbh)
      wb(i,np1,m) = wbh
  165 continue
  160 continue
      call rabcw(nlat,nlon,abc)
      return
      end
      subroutine dvtk(m,n,cv,work)
      double precision cv(*),work(*),fn,fk,cf,srnp1
      cv(1) = 0.
      if(n .le. 0) return
      fn = n
      srnp1 = dsqrt(fn*(fn+1.))
      cf = 2.*m/srnp1
      modn = mod(n,2)
      modm = mod(m,2)
      call dnlfk(m,n,work)
      if(modn .ne. 0) go to 70
      ncv = n/2
      if(ncv .eq. 0) return
      fk = 0.
      if(modm .ne. 0) go to 60
c
c     n even m even
c
      do 55 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*fk*work(l+1)/srnp1
   55 continue
      return
c
c     n even m odd
c
   60 do 65 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*fk*work(l)/srnp1
   65 continue
      return
   70 ncv = (n+1)/2
      fk = -1.
      if(modm .ne. 0) go to 80
c
c     n odd m even
c
      do 75 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*fk*work(l)/srnp1
   75 continue
      return
c
c     n odd m odd
c
   80 do 85 l=1,ncv
      fk = fk+2.
      cv(l) = -fk*fk*work(l)/srnp1
   85 continue
      return
      end
      subroutine dwtk(m,n,cw,work)
      double precision cw(*),work(*),fn,cf,srnp1
      cw(1) = 0.
      if(n.le.0 .or. m.le.0) return
      fn = n
      srnp1 = dsqrt(fn*(fn+1.))
      cf = 2.*m/srnp1
      modn = mod(n,2)
      modm = mod(m,2)
      call dnlfk(m,n,work)
      if(m .eq. 0) go to 50
      if(modn .ne. 0) go to 30
      l = n/2
      if(l .eq. 0) go to 50
      if(modm .ne. 0) go to 20
c
c     n even m even
c
      cw(l) = -cf*work(l+1)
   10 l = l-1
      if(l .le. 0) go to 50
      cw(l) = cw(l+1)-cf*work(l+1)
      cw(l+1) = (l+l+1)*cw(l+1)
      go to 10
c
c     n even m odd
c
   20 cw(l) = cf*work(l)
   25 l = l-1
      if(l) 50,27,26
   26 cw(l) = cw(l+1)+cf*work(l)
   27 cw(l+1) = -(l+l+1)*cw(l+1)
      go to 25
   30 if(modm .ne. 0) go to 40
      l = (n-1)/2
      if(l .eq. 0) go to 50
c
c     n odd m even
c
      cw(l) = -cf*work(l+1)
   35 l = l-1
      if(l) 50,37,36
   36 cw(l) = cw(l+1)-cf*work(l+1)
   37 cw(l+1) = (l+l+2)*cw(l+1)
      go to 35
c
c     n odd m odd
c
   40 l = (n+1)/2
      cw(l) = cf*work(l)
   45 l = l-1
      if(l) 50,47,46
   46 cw(l) = cw(l+1)+cf*work(l)
   47 cw(l+1) = -(l+l)*cw(l+1)
      go to 45
   50 return
      end
      subroutine dvtt(m,n,theta,cv,vh)
c      dimension cv(1) ! bma
      double precision cv(1),vh,theta,cth,sth,cdt,sdt,chh
      vh = 0.
      if(n.eq.0) return
      cth = dcos(theta)
      sth = dsin(theta)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      mmod = mod(m,2)
      nmod = mod(n,2)
      if(nmod .ne. 0) go to 1
      cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 2
c
c     n even  m even
c
      ncv = n/2
      do 10 k=1,ncv
      vh = vh+cv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     n even  m odd
c
    2 ncv = n/2
      do 15 k=1,ncv
      vh = vh+cv(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   15 continue
      return
    1 if(mmod .ne. 0) go to 3
c
c     n odd m even
c
      ncv = (n+1)/2
      do 20 k=1,ncv
      vh = vh+cv(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      return
c
c case m odd and n odd
c
    3 ncv = (n+1)/2
      do 25 k=1,ncv
      vh = vh+cv(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   25 continue
      return
      end
      subroutine dwtt(m,n,theta,cw,wh)
c      dimension cw(1) ! bma
      double precision theta,cw(1),wh,cth,sth,cdt,sdt,chh
      wh = 0.
      if(n.le.0 .or. m.le.0) return
      cth = dcos(theta)
      sth = dsin(theta)
      cdt = cth*cth-sth*sth
      sdt = 2.*sth*cth
      mmod=mod(m,2)
      nmod=mod(n,2)
      if(nmod .ne. 0) go to 1
      if(mmod .ne. 0) go to 2
c
c     n even  m even
c
      ncw = n/2
      do 10 k=1,ncw
      wh = wh+cw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   10 continue
      return
c
c     n even  m odd
c
    2 ncw = n/2
      do 8 k=1,ncw
      wh = wh+cw(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
    8 continue
      return
    1 cth = cdt
      sth = sdt
      if(mmod .ne. 0) go to 3
c
c     n odd m even
c
      ncw = (n-1)/2
      do 20 k=1,ncw
      wh = wh+cw(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   20 continue
      return
c
c case m odd and n odd
c
    3 ncw = (n+1)/2
      wh = 0.
      if(ncw.lt.2) return
      do 25 k=2,ncw
      wh = wh+cw(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
   25 continue
      return
      end
      subroutine vbgint (nlat,nlon,theta,wvbin,work)
      real       wvbin(1)
      double precision theta(*),work(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wvbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      call vbgit1 (nlat,nlon,imid,theta,wvbin,wvbin(iw1),
     +                        work,work(nlat/2+2))
      return
      end
      subroutine vbgit1 (nlat,nlon,imid,theta,vb,abc,cvb,work)
c
c     abc must have 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
c     locations where mmax = min0(nlat,(nlon+1)/2)
c     cvb and work must each have nlat/2+1 locations
c
      real vb(imid,nlat,2),abc(1)
      double precision cvb(1),theta(1),vbh,work(1)
      mdo = min0(2,nlat,(nlon+1)/2)
      do 160 mp1=1,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dvbk(m,n,cvb,work)
      do 165 i=1,imid
      call dvbt(m,n,theta(i),cvb,vbh)
      vb(i,np1,mp1) = vbh
  165 continue
  160 continue
      call rabcv(nlat,nlon,abc)
      return
      end
      subroutine wbgint (nlat,nlon,theta,wwbin,work)
      real       wwbin(1)
      double precision work(*),theta(*)
      imid = (nlat+1)/2
      iw1 = 2*nlat*imid+1
c
c     theta is a double precision array with (nlat+1)/2 locations
c     nlat is the maximum value of n+1
c     the length of wwbin is 2*nlat*imid+3*((nlat-3)*nlat+2)/2
c     the length of work is nlat+2
c
      call wbgit1 (nlat,nlon,imid,theta,wwbin,wwbin(iw1),
     +                        work,work(nlat/2+2))
      return
      end
      subroutine wbgit1 (nlat,nlon,imid,theta,wb,abc,cwb,work)
c
c     abc must have 3*((nlat-3)*nlat+2)/2 locations
c     cwb and work must each have nlat/2+1 locations
c
      real wb(imid,nlat,2),abc(1)
      double precision cwb(1),theta(1),wbh,work(1)
      mdo = min0(3,nlat,(nlon+1)/2)
      if(mdo .lt. 2) return
      do 160 mp1=2,mdo
      m = mp1-1
      do 160 np1=mp1,nlat
      n = np1-1
      call dwbk(m,n,cwb,work)
      do 165 i=1,imid
      call dwbt(m,n,theta(i),cwb,wbh)
      wb(i,np1,m) = wbh
  165 continue
  160 continue
      call rabcw(nlat,nlon,abc)
      return
      end

c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c ... file hrfft.f
c
c     this file contains a multiple fft package for spherepack3.0.
c     it includes code and documentation for performing fast fourier
c     transforms (see subroutines hrffti,hrfftf and hrfftb)
c
c **********************************************************************
c
c     subroutine hrffti(n,wsave)
c
c     subroutine hrffti initializes the array wsave which is used in
c     both hrfftf and hrfftb. the prime factorization of n together 
c     with a tabulation of the trigonometric functions are computed and
c     stored in wsave.
c
c     input parameter
c
c     n       the length of the sequence to be transformed.
c
c     output parameter
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             the same work array can be used for both hrfftf and 
c             hrfftb as long as n remains unchanged. different wsave 
c             arrays are required for different values of n. the 
c             contents of wsave must not be changed between calls 
c             of hrfftf or hrfftb.
c
c **********************************************************************
c
c     subroutine hrfftf(m,n,r,mdimr,wsave,work)
c
c     subroutine hrfftf computes the fourier coefficients of m real
c     perodic sequences (fourier analysis); i.e. hrfftf computes the
c     real fft of m sequences each with length n. the transform is 
c     defined below at output parameter r.
c
c     input parameters
c
c     m       the number of sequences.
c
c     n       the length of all m sequences.  the method is most
c             efficient when n is a product of small primes. n may
c             change as long as different work arrays are provided
c
c     r       r(m,n) is a two dimensional real array that contains m
c             sequences each with length n.
c
c     mdimr   the first dimension of the r array as it appears
c             in the program that calls hrfftf. mdimr must be
c             greater than or equal to m.
c
c
c     wsave   a work array with at least least 2*n+15 locations
c             in the program that calls hrfftf. the wsave array must be
c             initialized by calling subroutine hrffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by hrfftf and hrfftb.
c
c     work    a real work array with m*n locations.
c
c
c     output parameters
c
c     r      for all j=1,...,m
c  
c             r(j,1) = the sum from i=1 to i=n of r(j,i)
c
c             if n is even set l =n/2   , if n is odd set l = (n+1)/2
c
c               then for k = 2,...,l
c
c                  r(j,2*k-2) = the sum from i = 1 to i = n of
c
c                       r(j,i)*cos((k-1)*(i-1)*2*pi/n)
c
c                  r(j,2*k-1) = the sum from i = 1 to i = n of
c
c                      -r(j,i)*sin((k-1)*(i-1)*2*pi/n)
c
c             if n is even
c
c                  r(j,n) = the sum from i = 1 to i = n of
c
c                       (-1)**(i-1)*r(j,i)
c
c      *****  note
c                  this transform is unnormalized since a call of hrfftf
c                  followed by a call of hrfftb will multiply the input
c                  sequence by n.
c
c     wsave   contains results which must not be destroyed between
c             calls of hrfftf or hrfftb.
c
c     work    a real work array with m*n locations that does
c             not have to be saved.
c
c **********************************************************************
c
c     subroutine hrfftb(m,n,r,mdimr,wsave,work)
c
c     subroutine hrfftb computes the real perodic sequence of m
c     sequences from their fourier coefficients (fourier synthesis). 
c     the transform is defined below at output parameter r.
c
c     input parameters
c
c     m       the number of sequences.
c
c     n       the length of all m sequences.  the method is most
c             efficient when n is a product of small primes. n may
c             change as long as different work arrays are provided
c
c     r       r(m,n) is a two dimensional real array that contains
c             the fourier coefficients of m sequences each with 
c             length n.
c
c     mdimr   the first dimension of the r array as it appears
c             in the program that calls hrfftb. mdimr must be
c             greater than or equal to m.
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             in the program that calls hrfftb. the wsave array must be
c             initialized by calling subroutine hrffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by hrfftf and hrfftb.
c
c     work    a real work array with m*n locations.
c
c
c     output parameters
c
c     r      for all j=1,...,m
c  
c             for n even and for i = 1,...,n
c
c                  r(j,i) = r(j,1)+(-1)**(i-1)*r(j,n)
c
c                       plus the sum from k=2 to k=n/2 of
c
c                        2.*r(j,2*k-2)*cos((k-1)*(i-1)*2*pi/n)
c
c                       -2.*r(j,2*k-1)*sin((k-1)*(i-1)*2*pi/n)
c
c             for n odd and for i = 1,...,n
c
c                  r(j,i) = r(j,1) plus the sum from k=2 to k=(n+1)/2 of
c
c                       2.*r(j,2*k-2)*cos((k-1)*(i-1)*2*pi/n)
c
c                      -2.*r(j,2*k-1)*sin((k-1)*(i-1)*2*pi/n)
c
c      *****  note
c                  this transform is unnormalized since a call of hrfftf
c                  followed by a call of hrfftb will multiply the input
c                  sequence by n.
c
c     wsave   contains results which must not be destroyed between
c             calls of hrfftb or hrfftf.
c
c     work    a real work array with m*n locations that does not
c             have to be saved
c
c **********************************************************************
c
c
c
      subroutine hrffti (n,wsave)
      real       wsave(n+15)                                              
!ams      common /hrf/ tfft
!ams      tfft = 0.
      if (n .eq. 1) return                                                     
      call hrfti1 (n,wsave(1),wsave(n+1))
      return                                                                   
      end                                                                      
      subroutine hrfti1 (n,wa,fac)
c                                                                              
c     a multiple fft package for spherepack
c                                                                              
      real       wa(n)      ,fac(15)    ,ntryh(4)                         
      double precision tpi,argh,argld,arg
      data ntryh(1),ntryh(2),ntryh(3),ntryh(4)/4,2,3,5/                        
      nl = n                                                                   
      nf = 0           
      j = 0            
  101 j = j+1          
      if (j-4) 102,102,103                
  102 ntry = ntryh(j)                     
      go to 104        
  103 ntry = ntry+2    
  104 nq = nl/ntry     
      nr = nl-ntry*nq                     
      if (nr) 101,105,101                 
  105 nf = nf+1        
      fac(nf+2) = ntry                    
      nl = nq          
      if (ntry .ne. 2) go to 107          
      if (nf .eq. 1) go to 107            
      do 106 i=2,nf    
         ib = nf-i+2   
         fac(ib+2) = fac(ib+1)            
  106 continue         
      fac(3) = 2       
  107 if (nl .ne. 1) go to 104            
      fac(1) = n       
      fac(2) = nf      
      tpi = 8.d0*datan(1.d0)
      argh = tpi/float(n)                 
      is = 0           
      nfm1 = nf-1      
      l1 = 1           
      if (nfm1 .eq. 0) return             
      do 110 k1=1,nfm1                    
         ip = fac(k1+2)                   
         ld = 0        
         l2 = l1*ip    
         ido = n/l2    
         ipm = ip-1    
         do 109 j=1,ipm                   
            ld = ld+l1                    
            i = is     
            argld = float(ld)*argh        
            fi = 0.    
            do 108 ii=3,ido,2             
               i = i+2                    
               fi = fi+1.                 
               arg = fi*argld             
	       wa(i-1) = dcos(arg)
	       wa(i) = dsin(arg)
  108       continue   
            is = is+ido                   
  109    continue      
         l1 = l2       
  110 continue         
      return           
      end              
      subroutine hrfftf (m,n,r,mdimr,whrfft,work)
c                      
c     a multiple fft package for spherepack
c                      
      real       r(mdimr,n)  ,work(1)    ,whrfft(n+15)
!ams      common /hrf/ tfft
      if (n .eq. 1) return                
c     tstart = second(dum)
      call hrftf1 (m,n,r,mdimr,work,whrfft,whrfft(n+1))
c     tfft = tfft+second(dum)-tstart
      return           
      end              
      subroutine hrftf1 (m,n,c,mdimc,ch,wa,fac)
c                      
c     a multiple fft package for spherepack
c                      
      real       ch(m,n) ,c(mdimc,n)  ,wa(n)   ,fac(15)
      nf = fac(2)      
      na = 1           
      l2 = n           
      iw = n           
      do 111 k1=1,nf   
         kh = nf-k1    
         ip = fac(kh+3)                   
         l1 = l2/ip    
         ido = n/l2    
         idl1 = ido*l1                    
         iw = iw-(ip-1)*ido               
         na = 1-na     
         if (ip .ne. 4) go to 102         
         ix2 = iw+ido                     
         ix3 = ix2+ido                    
         if (na .ne. 0) go to 101         
	 call hradf4 (m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2),wa(ix3))
         go to 110     
  101    call hradf4 (m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2),wa(ix3))
         go to 110     
  102    if (ip .ne. 2) go to 104         
         if (na .ne. 0) go to 103         
	 call hradf2 (m,ido,l1,c,mdimc,ch,m,wa(iw))
         go to 110     
  103    call hradf2 (m,ido,l1,ch,m,c,mdimc,wa(iw))
         go to 110     
  104    if (ip .ne. 3) go to 106         
         ix2 = iw+ido                     
         if (na .ne. 0) go to 105         
	 call hradf3 (m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2))
         go to 110     
  105    call hradf3 (m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2))
         go to 110     
  106    if (ip .ne. 5) go to 108         
         ix2 = iw+ido                     
         ix3 = ix2+ido                    
         ix4 = ix3+ido                    
         if (na .ne. 0) go to 107         
      call hradf5(m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 110     
  107 call hradf5(m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 110     
  108    if (ido .eq. 1) na = 1-na        
         if (na .ne. 0) go to 109         
	 call hradfg (m,ido,ip,l1,idl1,c,c,c,mdimc,ch,ch,m,wa(iw))
         na = 1        
         go to 110     
  109    call hradfg (m,ido,ip,l1,idl1,ch,ch,ch,m,c,c,mdimc,wa(iw))
         na = 0        
  110    l2 = l1       
  111 continue         
      if (na .eq. 1) return
      do 112 j=1,n     
      do 112 i=1,m     
	 c(i,j) = ch(i,j)
  112 continue         
      return           
      end              
      subroutine hradf4 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1,wa2,wa3)
c                      
c     a multiple fft package for spherepack
c                      
      real    cc(mdimcc,ido,l1,4)   ,ch(mdimch,ido,4,l1)     ,
     1             wa1(ido)     ,wa2(ido)     ,wa3(ido)
      hsqt2=sqrt(2.)/2.                   
      do 101 k=1,l1    
         do 1001 m=1,mp                   
         ch(m,1,1,k) = (cc(m,1,k,2)+cc(m,1,k,4))             
     1      +(cc(m,1,k,1)+cc(m,1,k,3))    
         ch(m,ido,4,k) = (cc(m,1,k,1)+cc(m,1,k,3))           
     1      -(cc(m,1,k,2)+cc(m,1,k,4))    
         ch(m,ido,2,k) = cc(m,1,k,1)-cc(m,1,k,3)             
         ch(m,1,3,k) = cc(m,1,k,4)-cc(m,1,k,2)               
 1001    continue      
  101 continue         
      if (ido-2) 107,105,102              
  102 idp2 = ido+2     
      do 104 k=1,l1    
         do 103 i=3,ido,2                 
            ic = idp2-i                   
            do 1003 m=1,mp                
            ch(m,i-1,1,k) = ((wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*                 
     1       cc(m,i,k,2))+(wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*  
     1       cc(m,i,k,4)))+(cc(m,i-1,k,1)+(wa2(i-2)*cc(m,i-1,k,3)+             
     1       wa2(i-1)*cc(m,i,k,3)))       
            ch(m,ic-1,4,k) = (cc(m,i-1,k,1)+(wa2(i-2)*cc(m,i-1,k,3)+           
     1       wa2(i-1)*cc(m,i,k,3)))-((wa1(i-2)*cc(m,i-1,k,2)+                  
     1       wa1(i-1)*cc(m,i,k,2))+(wa3(i-2)*cc(m,i-1,k,4)+  
     1       wa3(i-1)*cc(m,i,k,4)))       
            ch(m,i,1,k) = ((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*   
     1       cc(m,i-1,k,2))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4)))+(cc(m,i,k,1)+(wa2(i-2)*cc(m,i,k,3)-               
     1       wa2(i-1)*cc(m,i-1,k,3)))     
            ch(m,ic,4,k) = ((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*  
     1       cc(m,i-1,k,2))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4)))-(cc(m,i,k,1)+(wa2(i-2)*cc(m,i,k,3)-               
     1       wa2(i-1)*cc(m,i-1,k,3)))     
            ch(m,i-1,3,k) = ((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)* 
     1       cc(m,i-1,k,2))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4)))+(cc(m,i-1,k,1)-(wa2(i-2)*cc(m,i-1,k,3)+           
     1       wa2(i-1)*cc(m,i,k,3)))       
            ch(m,ic-1,2,k) = (cc(m,i-1,k,1)-(wa2(i-2)*cc(m,i-1,k,3)+           
     1       wa2(i-1)*cc(m,i,k,3)))-((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*           
     1       cc(m,i-1,k,2))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4)))              
            ch(m,i,3,k) = ((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)* 
     1       cc(m,i,k,4))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2)))+(cc(m,i,k,1)-(wa2(i-2)*cc(m,i,k,3)-                 
     1       wa2(i-1)*cc(m,i-1,k,3)))     
            ch(m,ic,2,k) = ((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*                  
     1       cc(m,i,k,4))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2)))-(cc(m,i,k,1)-(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*        
     1       cc(m,i-1,k,3)))              
 1003       continue   
  103    continue      
  104 continue         
      if (mod(ido,2) .eq. 1) return       
  105 continue         
      do 106 k=1,l1    
         do 1006 m=1,mp                   
            ch(m,ido,1,k) = (hsqt2*(cc(m,ido,k,2)-cc(m,ido,k,4)))+              
     1       cc(m,ido,k,1)                
            ch(m,ido,3,k) = cc(m,ido,k,1)-(hsqt2*(cc(m,ido,k,2)-                
     1       cc(m,ido,k,4)))              
            ch(m,1,2,k) = (-hsqt2*(cc(m,ido,k,2)+cc(m,ido,k,4)))-               
     1       cc(m,ido,k,3)                
            ch(m,1,4,k) = (-hsqt2*(cc(m,ido,k,2)+cc(m,ido,k,4)))+               
     1       cc(m,ido,k,3)                
 1006    continue      
  106 continue         
  107 return           
      end              
      subroutine hradf2 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1)
c                      
c     a multiple fft package for spherepack
c                      
      real   ch(mdimch,ido,2,l1)  ,cc(mdimcc,ido,l1,2)     ,
     1                wa1(ido)            
      do 101 k=1,l1    
         do 1001 m=1,mp                   
         ch(m,1,1,k) = cc(m,1,k,1)+cc(m,1,k,2)               
         ch(m,ido,2,k) = cc(m,1,k,1)-cc(m,1,k,2)             
 1001    continue      
  101 continue         
      if (ido-2) 107,105,102              
  102 idp2 = ido+2     
      do 104 k=1,l1    
         do 103 i=3,ido,2                 
            ic = idp2-i                   
            do 1003 m=1,mp                
            ch(m,i,1,k) = cc(m,i,k,1)+(wa1(i-2)*cc(m,i,k,2)- 
     1       wa1(i-1)*cc(m,i-1,k,2))      
            ch(m,ic,2,k) = (wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*   
     1       cc(m,i-1,k,2))-cc(m,i,k,1)   
            ch(m,i-1,1,k) = cc(m,i-1,k,1)+(wa1(i-2)*cc(m,i-1,k,2)+              
     1       wa1(i-1)*cc(m,i,k,2))        
            ch(m,ic-1,2,k) = cc(m,i-1,k,1)-(wa1(i-2)*cc(m,i-1,k,2)+             
     1       wa1(i-1)*cc(m,i,k,2))        
 1003       continue   
  103    continue      
  104 continue         
      if (mod(ido,2) .eq. 1) return       
  105 do 106 k=1,l1    
         do 1006 m=1,mp                   
         ch(m,1,2,k) = -cc(m,ido,k,2)     
         ch(m,ido,1,k) = cc(m,ido,k,1)    
 1006    continue      
  106 continue         
  107 return           
      end              
      subroutine hradf3 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1,wa2)
c                      
c     a multiple fft package for spherepack
c                      
      real   ch(mdimch,ido,3,l1)  ,cc(mdimcc,ido,l1,3)     ,
     1                wa1(ido)     ,wa2(ido)                 
      arg=2.*pimach()/3.                  
      taur=cos(arg)    
      taui=sin(arg)    
      do 101 k=1,l1    
         do 1001 m=1,mp                   
         ch(m,1,1,k) = cc(m,1,k,1)+(cc(m,1,k,2)+cc(m,1,k,3)) 
         ch(m,1,3,k) = taui*(cc(m,1,k,3)-cc(m,1,k,2))        
         ch(m,ido,2,k) = cc(m,1,k,1)+taur*                   
     1      (cc(m,1,k,2)+cc(m,1,k,3))     
 1001    continue      
  101 continue         
      if (ido .eq. 1) return              
      idp2 = ido+2     
      do 103 k=1,l1    
         do 102 i=3,ido,2                 
            ic = idp2-i                   
            do 1002 m=1,mp                
            ch(m,i-1,1,k) = cc(m,i-1,k,1)+((wa1(i-2)*cc(m,i-1,k,2)+             
     1       wa1(i-1)*cc(m,i,k,2))+(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*            
     1       cc(m,i,k,3)))                
            ch(m,i,1,k) = cc(m,i,k,1)+((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*          
     1       cc(m,i-1,k,2))+(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*  
     1       cc(m,i-1,k,3)))              
            ch(m,i-1,3,k) = (cc(m,i-1,k,1)+taur*((wa1(i-2)*  
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa2(i-2)*  
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))))+(taui*((wa1(i-2)*            
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa2(i-2)*  
     1       cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3))))           
            ch(m,ic-1,2,k) = (cc(m,i-1,k,1)+taur*((wa1(i-2)* 
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa2(i-2)*  
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))))-(taui*((wa1(i-2)*            
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa2(i-2)*  
     1       cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3))))           
            ch(m,i,3,k) = (cc(m,i,k,1)+taur*((wa1(i-2)*cc(m,i,k,2)-             
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*            
     1       cc(m,i-1,k,3))))+(taui*((wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*          
     1       cc(m,i,k,3))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2))))               
            ch(m,ic,2,k) = (taui*((wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*             
     1       cc(m,i,k,3))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2))))-(cc(m,i,k,1)+taur*((wa1(i-2)*cc(m,i,k,2)-           
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*            
     1       cc(m,i-1,k,3))))             
 1002       continue   
  102    continue      
  103 continue         
      return           
      end              
      subroutine hradf5 (mp,ido,l1,cc,mdimcc,ch,mdimch,
     1                   wa1,wa2,wa3,wa4)
c                      
c     a multiple fft package for spherepack
c                      
      real  cc(mdimcc,ido,l1,5)    ,ch(mdimch,ido,5,l1)     ,
     1           wa1(ido)     ,wa2(ido)     ,wa3(ido)     ,wa4(ido)             
      arg=2.*pimach()/5.                  
      tr11=cos(arg)    
      ti11=sin(arg)    
      tr12=cos(2.*arg)                    
      ti12=sin(2.*arg)                    
      do 101 k=1,l1    
         do 1001 m=1,mp                   
         ch(m,1,1,k) = cc(m,1,k,1)+(cc(m,1,k,5)+cc(m,1,k,2))+                   
     1    (cc(m,1,k,4)+cc(m,1,k,3))       
         ch(m,ido,2,k) = cc(m,1,k,1)+tr11*(cc(m,1,k,5)+cc(m,1,k,2))+            
     1    tr12*(cc(m,1,k,4)+cc(m,1,k,3))  
         ch(m,1,3,k) = ti11*(cc(m,1,k,5)-cc(m,1,k,2))+ti12*  
     1    (cc(m,1,k,4)-cc(m,1,k,3))       
         ch(m,ido,4,k) = cc(m,1,k,1)+tr12*(cc(m,1,k,5)+cc(m,1,k,2))+            
     1    tr11*(cc(m,1,k,4)+cc(m,1,k,3))  
         ch(m,1,5,k) = ti12*(cc(m,1,k,5)-cc(m,1,k,2))-ti11*  
     1    (cc(m,1,k,4)-cc(m,1,k,3))       
 1001    continue      
  101 continue         
      if (ido .eq. 1) return              
      idp2 = ido+2     
      do 103 k=1,l1    
         do 102 i=3,ido,2                 
            ic = idp2-i                   
            do 1002 m=1,mp                
            ch(m,i-1,1,k) = cc(m,i-1,k,1)+((wa1(i-2)*cc(m,i-1,k,2)+             
     1       wa1(i-1)*cc(m,i,k,2))+(wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*            
     1       cc(m,i,k,5)))+((wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*                   
     1       cc(m,i,k,3))+(wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4)))        
            ch(m,i,1,k) = cc(m,i,k,1)+((wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*          
     1       cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*  
     1       cc(m,i-1,k,5)))+((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*                   
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4)))              
            ch(m,i-1,3,k) = cc(m,i-1,k,1)+tr11*              
     1      ( wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2)    
     1       +wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5))+tr12*                
     1      ( wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3)    
     1       +wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))+ti11*                
     1      ( wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2)    
     1       -(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*cc(m,i-1,k,5)))+ti12*              
     1      ( wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3)    
     1       -(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*cc(m,i-1,k,4))) 
            ch(m,ic-1,2,k) = cc(m,i-1,k,1)+tr11*             
     1      ( wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2)    
     1       +wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5))+tr12*                
     1     ( wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3)     
     1      +wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))-(ti11*                
     1      ( wa1(i-2)*cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2)    
     1       -(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*cc(m,i-1,k,5)))+ti12*              
     1      ( wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*cc(m,i-1,k,3)    
     1       -(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*cc(m,i-1,k,4))))                   
            ch(m,i,3,k) = (cc(m,i,k,1)+tr11*((wa1(i-2)*cc(m,i,k,2)-             
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*            
     1       cc(m,i-1,k,5)))+tr12*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*              
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4))))+(ti11*((wa4(i-2)*cc(m,i-1,k,5)+                   
     1       wa4(i-1)*cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*            
     1       cc(m,i,k,2)))+ti12*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*              
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*  
     1       cc(m,i,k,3))))               
            ch(m,ic,2,k) = (ti11*((wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*             
     1       cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2)))+ti12*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*              
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*  
     1       cc(m,i,k,3))))-(cc(m,i,k,1)+tr11*((wa1(i-2)*cc(m,i,k,2)-           
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*            
     1       cc(m,i-1,k,5)))+tr12*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*              
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4))))             
            ch(m,i-1,5,k) = (cc(m,i-1,k,1)+tr12*((wa1(i-2)*  
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa4(i-2)*  
     1       cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5)))+tr11*((wa2(i-2)*              
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))+(wa3(i-2)*  
     1       cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))))+(ti12*((wa1(i-2)*            
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa4(i-2)*cc(m,i,k,5)-         
     1       wa4(i-1)*cc(m,i-1,k,5)))-ti11*((wa2(i-2)*cc(m,i,k,3)-              
     1       wa2(i-1)*cc(m,i-1,k,3))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*            
     1       cc(m,i-1,k,4))))             
            ch(m,ic-1,4,k) = (cc(m,i-1,k,1)+tr12*((wa1(i-2)* 
     1       cc(m,i-1,k,2)+wa1(i-1)*cc(m,i,k,2))+(wa4(i-2)*  
     1       cc(m,i-1,k,5)+wa4(i-1)*cc(m,i,k,5)))+tr11*((wa2(i-2)*              
     1       cc(m,i-1,k,3)+wa2(i-1)*cc(m,i,k,3))+(wa3(i-2)*  
     1       cc(m,i-1,k,4)+wa3(i-1)*cc(m,i,k,4))))-(ti12*((wa1(i-2)*            
     1       cc(m,i,k,2)-wa1(i-1)*cc(m,i-1,k,2))-(wa4(i-2)*cc(m,i,k,5)-         
     1       wa4(i-1)*cc(m,i-1,k,5)))-ti11*((wa2(i-2)*cc(m,i,k,3)-              
     1       wa2(i-1)*cc(m,i-1,k,3))-(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*            
     1       cc(m,i-1,k,4))))             
            ch(m,i,5,k) = (cc(m,i,k,1)+tr12*((wa1(i-2)*cc(m,i,k,2)-             
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*            
     1       cc(m,i-1,k,5)))+tr11*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*              
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4))))+(ti12*((wa4(i-2)*cc(m,i-1,k,5)+                   
     1       wa4(i-1)*cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*            
     1       cc(m,i,k,2)))-ti11*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*              
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*  
     1       cc(m,i,k,3))))               
            ch(m,ic,4,k) = (ti12*((wa4(i-2)*cc(m,i-1,k,5)+wa4(i-1)*             
     1       cc(m,i,k,5))-(wa1(i-2)*cc(m,i-1,k,2)+wa1(i-1)*  
     1       cc(m,i,k,2)))-ti11*((wa3(i-2)*cc(m,i-1,k,4)+wa3(i-1)*              
     1       cc(m,i,k,4))-(wa2(i-2)*cc(m,i-1,k,3)+wa2(i-1)*  
     1       cc(m,i,k,3))))-(cc(m,i,k,1)+tr12*((wa1(i-2)*cc(m,i,k,2)-           
     1       wa1(i-1)*cc(m,i-1,k,2))+(wa4(i-2)*cc(m,i,k,5)-wa4(i-1)*            
     1       cc(m,i-1,k,5)))+tr11*((wa2(i-2)*cc(m,i,k,3)-wa2(i-1)*              
     1       cc(m,i-1,k,3))+(wa3(i-2)*cc(m,i,k,4)-wa3(i-1)*  
     1       cc(m,i-1,k,4))))             
 1002       continue   
  102    continue      
  103 continue         
      return           
      end              
      subroutine hradfg (mp,ido,ip,l1,idl1,cc,c1,c2,mdimcc,
     1              ch,ch2,mdimch,wa)
c                      
c     a multiple fft package for spherepack
c                      
      real     ch(mdimch,ido,l1,ip)   ,cc(mdimcc,ido,ip,l1)  ,
     1            c1(mdimcc,ido,l1,ip)    ,c2(mdimcc,idl1,ip),
     2                ch2(mdimch,idl1,ip)           ,wa(ido)
      tpi=2.*pimach()                     
      arg = tpi/float(ip)                 
      dcp = cos(arg)   
      dsp = sin(arg)   
      ipph = (ip+1)/2                     
      ipp2 = ip+2      
      idp2 = ido+2     
      nbd = (ido-1)/2                     
      if (ido .eq. 1) go to 119           
      do 101 ik=1,idl1                    
         do 1001 m=1,mp                   
         ch2(m,ik,1) = c2(m,ik,1)         
 1001    continue      
  101 continue         
      do 103 j=2,ip    
         do 102 k=1,l1                    
            do 1002 m=1,mp                
            ch(m,1,k,j) = c1(m,1,k,j)     
 1002       continue   
  102    continue      
  103 continue         
      if (nbd .gt. l1) go to 107          
      is = -ido        
      do 106 j=2,ip    
         is = is+ido   
         idij = is     
         do 105 i=3,ido,2                 
            idij = idij+2                 
            do 104 k=1,l1                 
               do 1004 m=1,mp             
               ch(m,i-1,k,j) = wa(idij-1)*c1(m,i-1,k,j)+wa(idij)                
     1           *c1(m,i,k,j)             
               ch(m,i,k,j) = wa(idij-1)*c1(m,i,k,j)-wa(idij) 
     1           *c1(m,i-1,k,j)           
 1004          continue                   
  104       continue   
  105    continue      
  106 continue         
      go to 111        
  107 is = -ido        
      do 110 j=2,ip    
         is = is+ido   
         do 109 k=1,l1                    
            idij = is                     
            do 108 i=3,ido,2              
               idij = idij+2              
               do 1008 m=1,mp             
               ch(m,i-1,k,j) = wa(idij-1)*c1(m,i-1,k,j)+wa(idij)                
     1           *c1(m,i,k,j)             
               ch(m,i,k,j) = wa(idij-1)*c1(m,i,k,j)-wa(idij) 
     1           *c1(m,i-1,k,j)           
 1008          continue                   
  108       continue   
  109    continue      
  110 continue         
  111 if (nbd .lt. l1) go to 115          
      do 114 j=2,ipph                     
         jc = ipp2-j   
         do 113 k=1,l1                    
            do 112 i=3,ido,2              
               do 1012 m=1,mp             
               c1(m,i-1,k,j) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)  
               c1(m,i-1,k,jc) = ch(m,i,k,j)-ch(m,i,k,jc)     
               c1(m,i,k,j) = ch(m,i,k,j)+ch(m,i,k,jc)        
               c1(m,i,k,jc) = ch(m,i-1,k,jc)-ch(m,i-1,k,j)   
 1012          continue                   
  112       continue   
  113    continue      
  114 continue         
      go to 121        
  115 do 118 j=2,ipph                     
         jc = ipp2-j   
         do 117 i=3,ido,2                 
            do 116 k=1,l1                 
               do 1016 m=1,mp             
               c1(m,i-1,k,j) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)  
               c1(m,i-1,k,jc) = ch(m,i,k,j)-ch(m,i,k,jc)     
               c1(m,i,k,j) = ch(m,i,k,j)+ch(m,i,k,jc)        
               c1(m,i,k,jc) = ch(m,i-1,k,jc)-ch(m,i-1,k,j)   
 1016          continue                   
  116       continue   
  117    continue      
  118 continue         
      go to 121        
  119 do 120 ik=1,idl1                    
         do 1020 m=1,mp                   
         c2(m,ik,1) = ch2(m,ik,1)         
 1020    continue      
  120 continue         
  121 do 123 j=2,ipph                     
         jc = ipp2-j   
         do 122 k=1,l1                    
            do 1022 m=1,mp                
            c1(m,1,k,j) = ch(m,1,k,j)+ch(m,1,k,jc)           
            c1(m,1,k,jc) = ch(m,1,k,jc)-ch(m,1,k,j)          
 1022       continue   
  122    continue      
  123 continue         
c                      
      ar1 = 1.         
      ai1 = 0.         
      do 127 l=2,ipph                     
         lc = ipp2-l   
         ar1h = dcp*ar1-dsp*ai1           
         ai1 = dcp*ai1+dsp*ar1            
         ar1 = ar1h    
         do 124 ik=1,idl1                 
            do 1024 m=1,mp                
            ch2(m,ik,l) = c2(m,ik,1)+ar1*c2(m,ik,2)          
            ch2(m,ik,lc) = ai1*c2(m,ik,ip)                   
 1024       continue   
  124    continue      
         dc2 = ar1     
         ds2 = ai1     
         ar2 = ar1     
         ai2 = ai1     
         do 126 j=3,ipph                  
            jc = ipp2-j                   
            ar2h = dc2*ar2-ds2*ai2        
            ai2 = dc2*ai2+ds2*ar2         
            ar2 = ar2h                    
            do 125 ik=1,idl1              
               do 1025 m=1,mp             
               ch2(m,ik,l) = ch2(m,ik,l)+ar2*c2(m,ik,j)      
               ch2(m,ik,lc) = ch2(m,ik,lc)+ai2*c2(m,ik,jc)   
 1025          continue                   
  125       continue   
  126    continue      
  127 continue         
      do 129 j=2,ipph                     
         do 128 ik=1,idl1                 
            do 1028 m=1,mp                
            ch2(m,ik,1) = ch2(m,ik,1)+c2(m,ik,j)             
 1028       continue   
  128    continue      
  129 continue         
c                      
      if (ido .lt. l1) go to 132          
      do 131 k=1,l1    
         do 130 i=1,ido                   
            do 1030 m=1,mp                
            cc(m,i,1,k) = ch(m,i,k,1)     
 1030       continue   
  130    continue      
  131 continue         
      go to 135        
  132 do 134 i=1,ido   
         do 133 k=1,l1                    
            do 1033 m=1,mp                
            cc(m,i,1,k) = ch(m,i,k,1)     
 1033       continue   
  133    continue      
  134 continue         
  135 do 137 j=2,ipph                     
         jc = ipp2-j   
         j2 = j+j      
         do 136 k=1,l1                    
            do 1036 m=1,mp                
            cc(m,ido,j2-2,k) = ch(m,1,k,j)                   
            cc(m,1,j2-1,k) = ch(m,1,k,jc)                    
 1036       continue   
  136    continue      
  137 continue         
      if (ido .eq. 1) return              
      if (nbd .lt. l1) go to 141          
      do 140 j=2,ipph                     
         jc = ipp2-j   
         j2 = j+j      
         do 139 k=1,l1                    
            do 138 i=3,ido,2              
               ic = idp2-i                
               do 1038 m=1,mp             
               cc(m,i-1,j2-1,k) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)                  
               cc(m,ic-1,j2-2,k) = ch(m,i-1,k,j)-ch(m,i-1,k,jc)                 
               cc(m,i,j2-1,k) = ch(m,i,k,j)+ch(m,i,k,jc)     
               cc(m,ic,j2-2,k) = ch(m,i,k,jc)-ch(m,i,k,j)    
 1038          continue                   
  138       continue   
  139    continue      
  140 continue         
      return           
  141 do 144 j=2,ipph                     
         jc = ipp2-j   
         j2 = j+j      
         do 143 i=3,ido,2                 
            ic = idp2-i                   
            do 142 k=1,l1                 
               do 1042 m=1,mp             
               cc(m,i-1,j2-1,k) = ch(m,i-1,k,j)+ch(m,i-1,k,jc)                  
               cc(m,ic-1,j2-2,k) = ch(m,i-1,k,j)-ch(m,i-1,k,jc)                 
               cc(m,i,j2-1,k) = ch(m,i,k,j)+ch(m,i,k,jc)     
               cc(m,ic,j2-2,k) = ch(m,i,k,jc)-ch(m,i,k,j)    
 1042          continue                   
  142       continue   
  143    continue      
  144 continue         
      return           
      end              
      function pimach()                   
      pimach=3.14159265358979             
      return           
      end              
      subroutine hrfftb(m,n,r,mdimr,whrfft,work)
c                      
c     a multiple fft package for spherepack
c                      
      real     r(mdimr,n)  ,work(1)    ,whrfft(n+15)
!ams      common /hrf/ tfft
      if (n .eq. 1) return                
c     tstart = second(dum)
      call hrftb1 (m,n,r,mdimr,work,whrfft,whrfft(n+1))
c     tfft = tfft+second(dum)-tstart
      return           
      end              
      subroutine hrftb1 (m,n,c,mdimc,ch,wa,fac)
c                      
c     a multiple fft package for spherepack
c                      
      real       ch(m,n), c(mdimc,n), wa(n) ,fac(15)
      nf = fac(2)      
      na = 0           
      l1 = 1           
      iw = 1           
      do 116 k1=1,nf   
         ip = fac(k1+2)                   
         l2 = ip*l1    
         ido = n/l2    
         idl1 = ido*l1                    
         if (ip .ne. 4) go to 103         
         ix2 = iw+ido                     
         ix3 = ix2+ido                    
         if (na .ne. 0) go to 101         
	 call hradb4 (m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2),wa(ix3))
         go to 102     
  101    call hradb4 (m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2),wa(ix3))
  102    na = 1-na     
         go to 115     
  103    if (ip .ne. 2) go to 106         
         if (na .ne. 0) go to 104         
	 call hradb2 (m,ido,l1,c,mdimc,ch,m,wa(iw))
         go to 105     
  104    call hradb2 (m,ido,l1,ch,m,c,mdimc,wa(iw))
  105    na = 1-na     
         go to 115     
  106    if (ip .ne. 3) go to 109         
         ix2 = iw+ido                     
         if (na .ne. 0) go to 107         
	 call hradb3 (m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2))
         go to 108     
  107    call hradb3 (m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2))
  108    na = 1-na     
         go to 115     
  109    if (ip .ne. 5) go to 112         
         ix2 = iw+ido                     
         ix3 = ix2+ido                    
         ix4 = ix3+ido                    
         if (na .ne. 0) go to 110         
      call hradb5 (m,ido,l1,c,mdimc,ch,m,wa(iw),wa(ix2),wa(ix3),wa(ix4))
         go to 111     
  110 call hradb5 (m,ido,l1,ch,m,c,mdimc,wa(iw),wa(ix2),wa(ix3),wa(ix4))
  111    na = 1-na     
         go to 115     
  112    if (na .ne. 0) go to 113         
	 call hradbg (m,ido,ip,l1,idl1,c,c,c,mdimc,ch,ch,m,wa(iw))
         go to 114     
  113    call hradbg (m,ido,ip,l1,idl1,ch,ch,ch,m,c,c,mdimc,wa(iw))
  114    if (ido .eq. 1) na = 1-na        
  115    l1 = l2       
         iw = iw+(ip-1)*ido               
  116 continue         
      if (na .eq. 0) return
      do 117 j=1,n     
      do 117 i=1,m     
	 c(i,j) = ch(i,j)
  117 continue         
      return           
      end              
      subroutine hradbg (mp,ido,ip,l1,idl1,cc,c1,c2,mdimcc,
     1          ch,ch2,mdimch,wa)
c                      
c     a multiple fft package for spherepack
c                      
      real    ch(mdimch,ido,l1,ip)    ,cc(mdimcc,ido,ip,l1) ,
     1           c1(mdimcc,ido,l1,ip)     ,c2(mdimcc,idl1,ip),
     2                ch2(mdimch,idl1,ip)       ,wa(ido)
      tpi=2.*pimach()                     
      arg = tpi/float(ip)                 
      dcp = cos(arg)   
      dsp = sin(arg)   
      idp2 = ido+2     
      nbd = (ido-1)/2                     
      ipp2 = ip+2      
      ipph = (ip+1)/2                     
      if (ido .lt. l1) go to 103          
      do 102 k=1,l1    
         do 101 i=1,ido                   
            do 1001 m=1,mp                
            ch(m,i,k,1) = cc(m,i,1,k)     
 1001       continue   
  101    continue      
  102 continue         
      go to 106        
  103 do 105 i=1,ido   
         do 104 k=1,l1                    
            do 1004 m=1,mp                
            ch(m,i,k,1) = cc(m,i,1,k)     
 1004       continue   
  104    continue      
  105 continue         
  106 do 108 j=2,ipph                     
         jc = ipp2-j   
         j2 = j+j      
         do 107 k=1,l1                    
            do 1007 m=1,mp                
            ch(m,1,k,j) = cc(m,ido,j2-2,k)+cc(m,ido,j2-2,k)  
            ch(m,1,k,jc) = cc(m,1,j2-1,k)+cc(m,1,j2-1,k)     
 1007       continue   
  107    continue      
  108 continue         
      if (ido .eq. 1) go to 116           
      if (nbd .lt. l1) go to 112          
      do 111 j=2,ipph                     
         jc = ipp2-j   
         do 110 k=1,l1                    
            do 109 i=3,ido,2              
               ic = idp2-i                
               do 1009 m=1,mp             
               ch(m,i-1,k,j) = cc(m,i-1,2*j-1,k)+cc(m,ic-1,2*j-2,k)             
               ch(m,i-1,k,jc) = cc(m,i-1,2*j-1,k)-cc(m,ic-1,2*j-2,k)            
               ch(m,i,k,j) = cc(m,i,2*j-1,k)-cc(m,ic,2*j-2,k)                   
               ch(m,i,k,jc) = cc(m,i,2*j-1,k)+cc(m,ic,2*j-2,k)                  
 1009          continue                   
  109       continue   
  110    continue      
  111 continue         
      go to 116        
  112 do 115 j=2,ipph                     
         jc = ipp2-j   
         do 114 i=3,ido,2                 
            ic = idp2-i                   
            do 113 k=1,l1                 
               do 1013 m=1,mp             
               ch(m,i-1,k,j) = cc(m,i-1,2*j-1,k)+cc(m,ic-1,2*j-2,k)             
               ch(m,i-1,k,jc) = cc(m,i-1,2*j-1,k)-cc(m,ic-1,2*j-2,k)            
               ch(m,i,k,j) = cc(m,i,2*j-1,k)-cc(m,ic,2*j-2,k)                   
               ch(m,i,k,jc) = cc(m,i,2*j-1,k)+cc(m,ic,2*j-2,k)                  
 1013          continue                   
  113       continue   
  114    continue      
  115 continue         
  116 ar1 = 1.         
      ai1 = 0.         
      do 120 l=2,ipph                     
         lc = ipp2-l   
         ar1h = dcp*ar1-dsp*ai1           
         ai1 = dcp*ai1+dsp*ar1            
         ar1 = ar1h    
         do 117 ik=1,idl1                 
            do 1017 m=1,mp                
            c2(m,ik,l) = ch2(m,ik,1)+ar1*ch2(m,ik,2)         
            c2(m,ik,lc) = ai1*ch2(m,ik,ip)                   
 1017       continue   
  117    continue      
         dc2 = ar1     
         ds2 = ai1     
         ar2 = ar1     
         ai2 = ai1     
         do 119 j=3,ipph                  
            jc = ipp2-j                   
            ar2h = dc2*ar2-ds2*ai2        
            ai2 = dc2*ai2+ds2*ar2         
            ar2 = ar2h                    
            do 118 ik=1,idl1              
               do 1018 m=1,mp             
               c2(m,ik,l) = c2(m,ik,l)+ar2*ch2(m,ik,j)       
               c2(m,ik,lc) = c2(m,ik,lc)+ai2*ch2(m,ik,jc)    
 1018          continue                   
  118       continue   
  119    continue      
  120 continue         
      do 122 j=2,ipph                     
         do 121 ik=1,idl1                 
            do 1021 m=1,mp                
            ch2(m,ik,1) = ch2(m,ik,1)+ch2(m,ik,j)            
 1021       continue   
  121    continue      
  122 continue         
      do 124 j=2,ipph                     
         jc = ipp2-j   
         do 123 k=1,l1                    
            do 1023 m=1,mp                
            ch(m,1,k,j) = c1(m,1,k,j)-c1(m,1,k,jc)           
            ch(m,1,k,jc) = c1(m,1,k,j)+c1(m,1,k,jc)          
 1023       continue   
  123    continue      
  124 continue         
      if (ido .eq. 1) go to 132           
      if (nbd .lt. l1) go to 128          
      do 127 j=2,ipph                     
         jc = ipp2-j   
         do 126 k=1,l1                    
            do 125 i=3,ido,2              
               do 1025 m=1,mp             
               ch(m,i-1,k,j) = c1(m,i-1,k,j)-c1(m,i,k,jc)    
               ch(m,i-1,k,jc) = c1(m,i-1,k,j)+c1(m,i,k,jc)   
               ch(m,i,k,j) = c1(m,i,k,j)+c1(m,i-1,k,jc)      
               ch(m,i,k,jc) = c1(m,i,k,j)-c1(m,i-1,k,jc)     
 1025          continue                   
  125       continue   
  126    continue      
  127 continue         
      go to 132        
  128 do 131 j=2,ipph                     
         jc = ipp2-j   
         do 130 i=3,ido,2                 
            do 129 k=1,l1                 
               do 1029 m=1,mp             
               ch(m,i-1,k,j) = c1(m,i-1,k,j)-c1(m,i,k,jc)    
               ch(m,i-1,k,jc) = c1(m,i-1,k,j)+c1(m,i,k,jc)   
               ch(m,i,k,j) = c1(m,i,k,j)+c1(m,i-1,k,jc)      
               ch(m,i,k,jc) = c1(m,i,k,j)-c1(m,i-1,k,jc)     
 1029          continue                   
  129       continue   
  130    continue      
  131 continue         
  132 continue         
      if (ido .eq. 1) return              
      do 133 ik=1,idl1                    
         do 1033 m=1,mp                   
         c2(m,ik,1) = ch2(m,ik,1)         
 1033    continue      
  133 continue         
      do 135 j=2,ip    
         do 134 k=1,l1                    
            do 1034 m=1,mp                
            c1(m,1,k,j) = ch(m,1,k,j)     
 1034       continue   
  134    continue      
  135 continue         
      if (nbd .gt. l1) go to 139          
      is = -ido        
      do 138 j=2,ip    
         is = is+ido   
         idij = is     
         do 137 i=3,ido,2                 
            idij = idij+2                 
            do 136 k=1,l1                 
               do 1036 m=1,mp             
               c1(m,i-1,k,j) = wa(idij-1)*ch(m,i-1,k,j)-wa(idij)*               
     1          ch(m,i,k,j)               
               c1(m,i,k,j) = wa(idij-1)*ch(m,i,k,j)+wa(idij)*                   
     1          ch(m,i-1,k,j)             
 1036          continue                   
  136       continue   
  137    continue      
  138 continue         
      go to 143        
  139 is = -ido        
      do 142 j=2,ip    
         is = is+ido   
         do 141 k=1,l1                    
            idij = is                     
            do 140 i=3,ido,2              
               idij = idij+2              
               do 1040 m=1,mp             
               c1(m,i-1,k,j) = wa(idij-1)*ch(m,i-1,k,j)-wa(idij)*               
     1          ch(m,i,k,j)               
               c1(m,i,k,j) = wa(idij-1)*ch(m,i,k,j)+wa(idij)*                   
     1          ch(m,i-1,k,j)             
 1040          continue                   
  140       continue   
  141    continue      
  142 continue         
  143 return           
      end              
      subroutine hradb4 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1,wa2,wa3)
c                      
c     a multiple fft package for spherepack
c                      
      real  cc(mdimcc,ido,4,l1)  ,ch(mdimch,ido,l1,4)    ,
     1                wa1(ido)  ,wa2(ido)  ,wa3(ido)         
      sqrt2=sqrt(2.)   
      do 101 k=1,l1    
          do 1001 m=1,mp                  
         ch(m,1,k,3) = (cc(m,1,1,k)+cc(m,ido,4,k))           
     1   -(cc(m,ido,2,k)+cc(m,ido,2,k))   
         ch(m,1,k,1) = (cc(m,1,1,k)+cc(m,ido,4,k))           
     1   +(cc(m,ido,2,k)+cc(m,ido,2,k))   
         ch(m,1,k,4) = (cc(m,1,1,k)-cc(m,ido,4,k))           
     1   +(cc(m,1,3,k)+cc(m,1,3,k))       
         ch(m,1,k,2) = (cc(m,1,1,k)-cc(m,ido,4,k))           
     1   -(cc(m,1,3,k)+cc(m,1,3,k))       
 1001     continue     
  101 continue         
      if (ido-2) 107,105,102              
  102 idp2 = ido+2     
      do 104 k=1,l1    
         do 103 i=3,ido,2                 
            ic = idp2-i                   
               do 1002 m=1,mp             
            ch(m,i-1,k,1) = (cc(m,i-1,1,k)+cc(m,ic-1,4,k))   
     1      +(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                  
            ch(m,i,k,1) = (cc(m,i,1,k)-cc(m,ic,4,k))         
     1      +(cc(m,i,3,k)-cc(m,ic,2,k))   
            ch(m,i-1,k,2)=wa1(i-2)*((cc(m,i-1,1,k)-cc(m,ic-1,4,k))              
     1      -(cc(m,i,3,k)+cc(m,ic,2,k)))-wa1(i-1)            
     1      *((cc(m,i,1,k)+cc(m,ic,4,k))+(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))        
            ch(m,i,k,2)=wa1(i-2)*((cc(m,i,1,k)+cc(m,ic,4,k)) 
     1      +(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))+wa1(i-1)        
     1      *((cc(m,i-1,1,k)-cc(m,ic-1,4,k))-(cc(m,i,3,k)+cc(m,ic,2,k)))        
            ch(m,i-1,k,3)=wa2(i-2)*((cc(m,i-1,1,k)+cc(m,ic-1,4,k))              
     1      -(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))-wa2(i-1)        
     1      *((cc(m,i,1,k)-cc(m,ic,4,k))-(cc(m,i,3,k)-cc(m,ic,2,k)))            
            ch(m,i,k,3)=wa2(i-2)*((cc(m,i,1,k)-cc(m,ic,4,k)) 
     1      -(cc(m,i,3,k)-cc(m,ic,2,k)))+wa2(i-1)            
     1      *((cc(m,i-1,1,k)+cc(m,ic-1,4,k))-(cc(m,i-1,3,k)  
     1      +cc(m,ic-1,2,k)))             
            ch(m,i-1,k,4)=wa3(i-2)*((cc(m,i-1,1,k)-cc(m,ic-1,4,k))              
     1      +(cc(m,i,3,k)+cc(m,ic,2,k)))-wa3(i-1)            
     1     *((cc(m,i,1,k)+cc(m,ic,4,k))-(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))         
            ch(m,i,k,4)=wa3(i-2)*((cc(m,i,1,k)+cc(m,ic,4,k)) 
     1      -(cc(m,i-1,3,k)-cc(m,ic-1,2,k)))+wa3(i-1)        
     1      *((cc(m,i-1,1,k)-cc(m,ic-1,4,k))+(cc(m,i,3,k)+cc(m,ic,2,k)))        
 1002          continue                   
  103    continue      
  104 continue         
      if (mod(ido,2) .eq. 1) return       
  105 continue         
      do 106 k=1,l1    
               do 1003 m=1,mp             
         ch(m,ido,k,1) = (cc(m,ido,1,k)+cc(m,ido,3,k))       
     1   +(cc(m,ido,1,k)+cc(m,ido,3,k))   
         ch(m,ido,k,2) = sqrt2*((cc(m,ido,1,k)-cc(m,ido,3,k))                   
     1   -(cc(m,1,2,k)+cc(m,1,4,k)))      
         ch(m,ido,k,3) = (cc(m,1,4,k)-cc(m,1,2,k))           
     1   +(cc(m,1,4,k)-cc(m,1,2,k))       
         ch(m,ido,k,4) = -sqrt2*((cc(m,ido,1,k)-cc(m,ido,3,k))                  
     1   +(cc(m,1,2,k)+cc(m,1,4,k)))      
 1003          continue                   
  106 continue         
  107 return           
      end              
      subroutine hradb2 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1)
c                      
c     a multiple fft package for spherepack
c                      
      real  cc(mdimcc,ido,2,l1)    ,ch(mdimch,ido,l1,2),
     1                wa1(ido)            
      do 101 k=1,l1    
          do 1001 m=1,mp                  
         ch(m,1,k,1) = cc(m,1,1,k)+cc(m,ido,2,k)             
         ch(m,1,k,2) = cc(m,1,1,k)-cc(m,ido,2,k)             
 1001     continue     
  101 continue         
      if (ido-2) 107,105,102              
  102 idp2 = ido+2     
      do 104 k=1,l1    
         do 103 i=3,ido,2                 
            ic = idp2-i                   
               do 1002 m=1,mp             
            ch(m,i-1,k,1) = cc(m,i-1,1,k)+cc(m,ic-1,2,k)     
            ch(m,i,k,1) = cc(m,i,1,k)-cc(m,ic,2,k)           
            ch(m,i-1,k,2) = wa1(i-2)*(cc(m,i-1,1,k)-cc(m,ic-1,2,k))             
     1      -wa1(i-1)*(cc(m,i,1,k)+cc(m,ic,2,k))             
            ch(m,i,k,2) = wa1(i-2)*(cc(m,i,1,k)+cc(m,ic,2,k))+wa1(i-1)          
     1      *(cc(m,i-1,1,k)-cc(m,ic-1,2,k))                  
 1002          continue                   
  103    continue      
  104 continue         
      if (mod(ido,2) .eq. 1) return       
  105 do 106 k=1,l1    
          do 1003 m=1,mp                  
         ch(m,ido,k,1) = cc(m,ido,1,k)+cc(m,ido,1,k)         
         ch(m,ido,k,2) = -(cc(m,1,2,k)+cc(m,1,2,k))          
 1003     continue     
  106 continue         
  107 return           
      end              
      subroutine hradb3 (mp,ido,l1,cc,mdimcc,ch,mdimch,wa1,wa2)
c                      
c     a multiple fft package for spherepack
c                      
      real  cc(mdimcc,ido,3,l1)    ,ch(mdimch,ido,l1,3),
     1                wa1(ido)   ,wa2(ido)                   
      arg=2.*pimach()/3.                  
      taur=cos(arg)    
      taui=sin(arg)    
      do 101 k=1,l1    
          do 1001 m=1,mp                  
         ch(m,1,k,1) = cc(m,1,1,k)+2.*cc(m,ido,2,k)          
         ch(m,1,k,2) = cc(m,1,1,k)+(2.*taur)*cc(m,ido,2,k)   
     1   -(2.*taui)*cc(m,1,3,k)           
         ch(m,1,k,3) = cc(m,1,1,k)+(2.*taur)*cc(m,ido,2,k)   
     1   +2.*taui*cc(m,1,3,k)             
 1001     continue     
  101 continue         
      if (ido .eq. 1) return              
      idp2 = ido+2     
      do 103 k=1,l1    
         do 102 i=3,ido,2                 
            ic = idp2-i                   
               do 1002 m=1,mp             
            ch(m,i-1,k,1) = cc(m,i-1,1,k)+(cc(m,i-1,3,k)+cc(m,ic-1,2,k))        
            ch(m,i,k,1) = cc(m,i,1,k)+(cc(m,i,3,k)-cc(m,ic,2,k))                
            ch(m,i-1,k,2) = wa1(i-2)*     
     1 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))- 
     * (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))                    
     2                   -wa1(i-1)*       
     3 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))+       
     * (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))                
            ch(m,i,k,2) = wa1(i-2)*       
     4 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))+       
     8 (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))                
     5                  +wa1(i-1)*        
     6 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))- 
     8 (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))                    
              ch(m,i-1,k,3) = wa2(i-2)*   
     7 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))+ 
     8 (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))                    
     8   -wa2(i-1)*    
     9 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))-       
     8 (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))                
            ch(m,i,k,3) = wa2(i-2)*       
     1 ((cc(m,i,1,k)+taur*(cc(m,i,3,k)-cc(m,ic,2,k)))-       
     8 (taui*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))))                
     2                 +wa2(i-1)*         
     3 ((cc(m,i-1,1,k)+taur*(cc(m,i-1,3,k)+cc(m,ic-1,2,k)))+ 
     8 (taui*(cc(m,i,3,k)+cc(m,ic,2,k))))                    
 1002          continue                   
  102    continue      
  103 continue         
      return           
      end              
      subroutine hradb5 (mp,ido,l1,cc,mdimcc,ch,mdimch,
     1       wa1,wa2,wa3,wa4)
c                      
c     a multiple fft package for spherepack
c                      
      real  cc(mdimcc,ido,5,l1)    ,ch(mdimch,ido,l1,5),
     1             wa1(ido)     ,wa2(ido)     ,wa3(ido)     ,wa4(ido)           
      arg=2.*pimach()/5.                  
      tr11=cos(arg)    
      ti11=sin(arg)    
      tr12=cos(2.*arg)                    
      ti12=sin(2.*arg)                    
      do 101 k=1,l1    
      do 1001 m=1,mp   
         ch(m,1,k,1) = cc(m,1,1,k)+2.*cc(m,ido,2,k)+2.*cc(m,ido,4,k)            
         ch(m,1,k,2) = (cc(m,1,1,k)+tr11*2.*cc(m,ido,2,k)    
     1   +tr12*2.*cc(m,ido,4,k))-(ti11*2.*cc(m,1,3,k)        
     1   +ti12*2.*cc(m,1,5,k))            
         ch(m,1,k,3) = (cc(m,1,1,k)+tr12*2.*cc(m,ido,2,k)    
     1   +tr11*2.*cc(m,ido,4,k))-(ti12*2.*cc(m,1,3,k)        
     1   -ti11*2.*cc(m,1,5,k))            
         ch(m,1,k,4) = (cc(m,1,1,k)+tr12*2.*cc(m,ido,2,k)    
     1   +tr11*2.*cc(m,ido,4,k))+(ti12*2.*cc(m,1,3,k)        
     1   -ti11*2.*cc(m,1,5,k))            
         ch(m,1,k,5) = (cc(m,1,1,k)+tr11*2.*cc(m,ido,2,k)    
     1   +tr12*2.*cc(m,ido,4,k))+(ti11*2.*cc(m,1,3,k)        
     1   +ti12*2.*cc(m,1,5,k))            
 1001          continue                   
  101 continue         
      if (ido .eq. 1) return              
      idp2 = ido+2     
      do 103 k=1,l1    
         do 102 i=3,ido,2                 
            ic = idp2-i                   
      do 1002 m=1,mp   
            ch(m,i-1,k,1) = cc(m,i-1,1,k)+(cc(m,i-1,3,k)+cc(m,ic-1,2,k))        
     1      +(cc(m,i-1,5,k)+cc(m,ic-1,4,k))                  
            ch(m,i,k,1) = cc(m,i,1,k)+(cc(m,i,3,k)-cc(m,ic,2,k))                
     1      +(cc(m,i,5,k)-cc(m,ic,4,k))   
            ch(m,i-1,k,2) = wa1(i-2)*((cc(m,i-1,1,k)+tr11*   
     1      (cc(m,i-1,3,k)+cc(m,ic-1,2,k))+tr12              
     1      *(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))-(ti11*(cc(m,i,3,k)                 
     1      +cc(m,ic,2,k))+ti12*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
     1      -wa1(i-1)*((cc(m,i,1,k)+tr11*(cc(m,i,3,k)-cc(m,ic,2,k))             
     1      +tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))+(ti11*(cc(m,i-1,3,k)              
     1      -cc(m,ic-1,2,k))+ti12*(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))              
            ch(m,i,k,2) = wa1(i-2)*((cc(m,i,1,k)+tr11*(cc(m,i,3,k)              
     1      -cc(m,ic,2,k))+tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))  
     1      +(ti11*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))+ti12       
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))+wa1(i-1)       
     1      *((cc(m,i-1,1,k)+tr11*(cc(m,i-1,3,k)             
     1      +cc(m,ic-1,2,k))+tr12*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))               
     1      -(ti11*(cc(m,i,3,k)+cc(m,ic,2,k))+ti12           
     1      *(cc(m,i,5,k)+cc(m,ic,4,k))))                    
            ch(m,i-1,k,3) = wa2(i-2)      
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))-(ti12*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
     1     -wa2(i-1)   
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-                 
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))   
     1      +(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11       
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))                
            ch(m,i,k,3) = wa2(i-2)        
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-                 
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))   
     1      +(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11       
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))                
     1      +wa2(i-1)                     
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))-(ti12*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
            ch(m,i-1,k,4) = wa3(i-2)      
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti12*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
     1      -wa3(i-1)                     
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-                 
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))   
     1      -(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11       
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))                
            ch(m,i,k,4) = wa3(i-2)        
     1     *((cc(m,i,1,k)+tr12*(cc(m,i,3,k)-                 
     1      cc(m,ic,2,k))+tr11*(cc(m,i,5,k)-cc(m,ic,4,k)))   
     1      -(ti12*(cc(m,i-1,3,k)-cc(m,ic-1,2,k))-ti11       
     1      *(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))                
     1      +wa3(i-1)                     
     1      *((cc(m,i-1,1,k)+tr12*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr11*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti12*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))-ti11*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
            ch(m,i-1,k,5) = wa4(i-2)      
     1      *((cc(m,i-1,1,k)+tr11*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr12*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti11*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))+ti12*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
     1      -wa4(i-1)                     
     1      *((cc(m,i,1,k)+tr11*(cc(m,i,3,k)-cc(m,ic,2,k))   
     1      +tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))-(ti11*(cc(m,i-1,3,k)              
     1      -cc(m,ic-1,2,k))+ti12*(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))              
            ch(m,i,k,5) = wa4(i-2)        
     1      *((cc(m,i,1,k)+tr11*(cc(m,i,3,k)-cc(m,ic,2,k))   
     1      +tr12*(cc(m,i,5,k)-cc(m,ic,4,k)))-(ti11*(cc(m,i-1,3,k)              
     1      -cc(m,ic-1,2,k))+ti12*(cc(m,i-1,5,k)-cc(m,ic-1,4,k))))              
     1      +wa4(i-1)                     
     1      *((cc(m,i-1,1,k)+tr11*(cc(m,i-1,3,k)+cc(m,ic-1,2,k))                
     1      +tr12*(cc(m,i-1,5,k)+cc(m,ic-1,4,k)))+(ti11*(cc(m,i,3,k)            
     1      +cc(m,ic,2,k))+ti12*(cc(m,i,5,k)+cc(m,ic,4,k)))) 
 1002          continue                   
  102    continue      
  103 continue         
      return           
      end              
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c ... file shsec.f
c
c     this file contains code and documentation for subroutines
c     shsec and shseci
c
c ... files which must be loaded with shsec.f
c
c     sphcom.f, hrfft.f
c
c     subroutine shsec(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                    wshsec,lshsec,work,lwork,ierror)
c
c     subroutine shsec performs the spherical harmonic synthesis
c     on the arrays a and b and stores the result in the array g.
c     the synthesis is performed on an equally spaced grid.  the
c     associated legendre functions are recomputed rather than stored
c     as they are in subroutine shses.  the synthesis is described
c     below at output parameter g.
c
c     required files from spherepack2
c
c     sphcom.f, hrfft.f
c
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     isym   = 0  no symmetries exist about the equator. the synthesis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the synthesis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the synthesis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the synthesis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the synthesis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the synthesis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the synthesis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of syntheses.  in the program that calls shsec,
c            the arrays g,a and b can be three dimensional in which
c            case multiple syntheses will be performed.  the third
c            index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shsec.  if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg
c            must be at least nlat/2 if nlat is even or at least
c            (nlat+1)/2 if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shsec.  jdg must be at least nlon.
c
c     a,b    two or three dimensional arrays (see the input parameter
c            nt) that contain the coefficients in the spherical harmonic
c            expansion of g(i,j) given below at the definition of the
c            output parameter g.  a(m,n) and b(m,n) are defined for
c            indices m=1,...,mmax and n=m,...,nlat where mmax is the
c            maximum (plus one) longitudinal wave number given by
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shsec. mdab must be at least
c            min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shsec. ndab must be at least nlat
c
c     wshsec an array which must be initialized by subroutine shseci.
c            once initialized, wshsec can be used repeatedly by shsec
c            as long as nlon and nlat remain unchanged.  wshsec must
c            not be altered between calls of shsec.
c
c     lshsec the dimension of the array wshsec as it appears in the
c            program that calls shsec. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsec must be at least
c
c            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shsec. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c                    nlat*(nt*nlon+max0(3*l2,nlon))
c
c            if isym is not zero then lwork must be at least
c
c                    l2*(nt*nlon+max0(3*nlat,nlon))
c
c     **************************************************************
c
c     output parameters
c
c     g      a two or three dimensional array (see input parameter
c            nt) that contains the spherical harmonic synthesis of
c            the arrays a and b at the colatitude point theta(i) =
c            (i-1)*pi/(nlat-1) and longitude point phi(j) =
c            (j-1)*2*pi/nlon. the index ranges are defined above at
c            at the input parameter isym.  for isym=0, g(i,j) is
c            given by the the equations listed below.  symmetric
c            versions are used when isym is greater than zero.
c
c     the normalized associated legendre functions are given by
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta)
c
c     define the maximum (plus one) longitudinal wave number
c     as   mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c          mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     then g(i,j) = the sum from n=0 to n=nlat-1 of
c
c                   .5*pbar(0,n,theta(i))*a(1,n+1)
c
c              plus the sum from m=1 to m=mmax-1 of
c
c                   the sum from n=m to n=nlat-1 of
c
c              pbar(m,n,theta(i))*(a(m+1,n+1)*cos(m*phi(j))
c                                    -b(m+1,n+1)*sin(m*phi(j)))
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of nt
c            = 5  error in the specification of idg
c            = 6  error in the specification of jdg
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lshsec
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c     subroutine shseci(nlat,nlon,wshsec,lshsec,dwork,ldwork,ierror)
c
c     subroutine shseci initializes the array wshsec which can then
c     be used repeatedly by subroutine shsec.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     lshsec the dimension of the array wshsec as it appears in the
c            program that calls shseci. the array wshsec is an output
c            parameter which is described below. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsec must be at least
c
c            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c     dwork  a double precision work array that does not have to be
c            saved.
c
c     ldwork the dimension of array dwork as it appears in the program
c            that calls shseci.  ldwork must be at least nlat+1.
c
c     output parameters
c
c     wshsec an array which is initialized for use by subroutine shsec.
c            once initialized, wshsec can be used repeatedly by shsec
c            as long as nlon and nlat remain unchanged.  wshsec must
c            not be altered between calls of shsec.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshsec
c            = 4  error in the specification of ldwork
c
c
c ****************************************************************
      subroutine shsec(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
     1                    wshsec,lshsec,work,lwork,ierror)
      real g(idg,jdg,1),a(mdab,ndab,1),b(mdab,ndab,1),wshsec(1),
     1          work(1)
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
      ierror = 3
      if(isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      if((isym.eq.0 .and. idg.lt.nlat) .or.
     1   (isym.ne.0 .and. idg.lt.(nlat+1)/2)) return
      ierror = 6
      if(jdg .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,nlon/2+1)
      if(mdab .lt. mmax) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
      imid = (nlat+1)/2
      lzz1 = 2*nlat*imid
      labc = 3*((mmax-2)*(nlat+nlat-mmax-1))/2
      if(lshsec .lt. lzz1+labc+nlon+15) return
      ierror = 10
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
      if(lwork .lt. nln+max0(ls*nlon,3*nlat*imid)) return
      ierror = 0
      ist = 0
      if(isym .eq. 0) ist = imid
      iw1 = lzz1+labc+1
      call shsec1(nlat,isym,nt,g,idg,jdg,a,b,mdab,ndab,imid,ls,nlon,
     1     work,work(ist+1),work(nln+1),work(nln+1),wshsec,wshsec(iw1))
      return
      end
      subroutine shsec1(nlat,isym,nt,g,idgs,jdgs,a,b,mdab,ndab,imid,
     1                idg,jdg,ge,go,work,pb,walin,whrfft)
c
c     whrfft must have at least nlon+15 locations
c     walin must have 3*l*imid + 3*((l-3)*l+2)/2 locations
c     zb must have 3*l*imid locations
c
      real g(idgs,jdgs,1),a(mdab,ndab,1),b(mdab,ndab,1),
     1          ge(idg,jdg,1),go(idg,jdg,1),pb(imid,nlat,3),walin(1),
     3          whrfft(1),work(1)
      ls = idg
      nlon = jdg
      mmax = min0(nlat,nlon/2+1)
      mdo = mmax
      if(mdo+mdo-1 .gt. nlon) mdo = mmax-1
      nlp1 = nlat+1
      modl = mod(nlat,2)
      imm1 = imid
      if(modl .ne. 0) imm1 = imid-1
      do 80 k=1,nt
      do 80 j=1,nlon
      do 80 i=1,ls
      ge(i,j,k)=0.
   80 continue
      if(isym .eq. 1) go to 125
      call alin (2,nlat,nlon,0,pb,i3,walin)
      do 100 k=1,nt
      do 100 np1=1,nlat,2
      do 100 i=1,imid
      ge(i,1,k)=ge(i,1,k)+a(1,np1,k)*pb(i,np1,i3)
  100 continue
      ndo = nlat
      if(mod(nlat,2) .eq. 0) ndo = nlat-1
      do 110 mp1=2,mdo
      m = mp1-1
      call alin (2,nlat,nlon,m,pb,i3,walin)
      do 110 np1=mp1,ndo,2
      do 110 k=1,nt
      do 110 i=1,imid
      ge(i,2*mp1-2,k) = ge(i,2*mp1-2,k)+a(mp1,np1,k)*pb(i,np1,i3)
      ge(i,2*mp1-1,k) = ge(i,2*mp1-1,k)+b(mp1,np1,k)*pb(i,np1,i3)
  110 continue
      if(mdo .eq. mmax .or. mmax .gt. ndo) go to 122
      call alin (2,nlat,nlon,mdo,pb,i3,walin)
      do 120 np1=mmax,ndo,2
      do 120 k=1,nt
      do 120 i=1,imid
      ge(i,2*mmax-2,k) = ge(i,2*mmax-2,k)+a(mmax,np1,k)*pb(i,np1,i3)
  120 continue
  122 if(isym .eq. 2) go to 155
  125 call alin(1,nlat,nlon,0,pb,i3,walin)
      do 140 k=1,nt
      do 140 np1=2,nlat,2
      do 140 i=1,imm1
      go(i,1,k)=go(i,1,k)+a(1,np1,k)*pb(i,np1,i3)
  140 continue
      ndo = nlat
      if(mod(nlat,2) .ne. 0) ndo = nlat-1
      do 150 mp1=2,mdo
      mp2 = mp1+1
      m = mp1-1
      call alin(1,nlat,nlon,m,pb,i3,walin)
      do 150 np1=mp2,ndo,2
      do 150 k=1,nt
      do 150 i=1,imm1
      go(i,2*mp1-2,k) = go(i,2*mp1-2,k)+a(mp1,np1,k)*pb(i,np1,i3)
      go(i,2*mp1-1,k) = go(i,2*mp1-1,k)+b(mp1,np1,k)*pb(i,np1,i3)
  150 continue
      mp2 = mmax+1
      if(mdo .eq. mmax .or. mp2 .gt. ndo) go to 155
      call alin(1,nlat,nlon,mdo,pb,i3,walin)
      do 152 np1=mp2,ndo,2
      do 152 k=1,nt
      do 152 i=1,imm1
      go(i,2*mmax-2,k) = go(i,2*mmax-2,k)+a(mmax,np1,k)*pb(i,np1,i3)
  152 continue
  155 do 160 k=1,nt
      if(mod(nlon,2) .ne. 0) go to 157
      do 156 i=1,ls
      ge(i,nlon,k) = 2.*ge(i,nlon,k)
  156 continue
  157 call hrfftb(ls,nlon,ge(1,1,k),ls,whrfft,work)
  160 continue
      if(isym .ne. 0) go to 180
      do 170 k=1,nt
      do 170 j=1,nlon
      do 175 i=1,imm1
      g(i,j,k) = .5*(ge(i,j,k)+go(i,j,k))
      g(nlp1-i,j,k) = .5*(ge(i,j,k)-go(i,j,k))
  175 continue
      if(modl .eq. 0) go to 170
      g(imid,j,k) = .5*ge(imid,j,k)
  170 continue
      return
  180 do 185 k=1,nt
      do 185 i=1,imid
      do 185 j=1,nlon
      g(i,j,k) = .5*ge(i,j,k)
  185 continue
      return
      end
c     subroutine shseci(nlat,nlon,wshsec,lshsec,dwork,ldwork,ierror)
c
c     subroutine shseci initializes the array wshsec which can then
c     be used repeatedly by subroutine shsec.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     lshsec the dimension of the array wshsec as it appears in the
c            program that calls shseci. the array wshsec is an output
c            parameter which is described below. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsec must be at least
c
c            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c     dwork  a double precision work array that does not have to be
c            saved.
c
c     ldwork the dimension of array dwork as it appears in the program
c            that calls shseci.  ldwork must be at least nlat+1.
c
c     output parameters
c
c     wshsec an array which is initialized for use by subroutine shsec.
c            once initialized, wshsec can be used repeatedly by shsec
c            as long as nlon and nlat remain unchanged.  wshsec must
c            not be altered between calls of shsec.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshsec
c            = 4  error in the specification of ldwork
c
c
c ****************************************************************
      subroutine shseci(nlat,nlon,wshsec,lshsec,dwork,ldwork,ierror)
      dimension wshsec(*)
      double precision dwork(ldwork)
      ierror = 1
      if(nlat.lt.3) return
      ierror = 2
      if(nlon.lt.4) return
      ierror = 3
      imid = (nlat+1)/2
      mmax = min0(nlat,nlon/2+1)
      lzz1 = 2*nlat*imid
      labc = 3*((mmax-2)*(nlat+nlat-mmax-1))/2
      if(lshsec .lt. lzz1+labc+nlon+15) return
      ierror = 4
      if(ldwork .lt. nlat+1) return
      ierror = 0
      call alinit(nlat,nlon,wshsec,dwork)
      iw1 = lzz1+labc+1
      call hrffti(nlon,wshsec(iw1))
      return
      end
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                          SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c                             August 2003
c
c
c     This version of gaqd implements the method presented in:
c     P. N. swarztrauber, Computing the points and weights for 
c     Gauss-Legendre quadrature, SIAM J. Sci. Comput.,
c     24(2002) pp. 945-954.
c     
c     It the version that is new to spherepack 3.1
c     The w and lwork arrays are dummy and included only to
c     permit a simple pluggable exchange with the
c     old gaqd in spherepack 3.0. 
c
c
c
      subroutine gaqd(nlat,theta,wts,w,lwork,ierror)
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 2001 by ucar                 .
c  .                                                             .
c  .       university corporation for atmospheric research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c                             February 2002
c                        
c     gauss points and weights are computed using the fourier-newton
c     described in "on computing the points and weights for 
c     gauss-legendre quadrature", paul n. swarztrauber, siam journal 
c     on scientific computing that has been accepted for publication.
c     This routine is faster and more accurate than older program 
c     with the same name.
c
c     subroutine gaqd computes the nlat gaussian colatitudes and weights
c     in double precision. the colatitudes are in radians and lie in the
c     in the interval (0,pi).
c
c     input parameters
c
c     nlat    the number of gaussian colatitudes in the interval (0,pi)
c             (between the two poles).  nlat must be greater than zero.
c
c     w       unused double precision variable that permits a simple 
c             exchange with the old routine with the same name 
c             in spherepack.
c
c     lwork   unused variable that permits a simple exchange with the
c             old routine with the same name in spherepack.
c
c     output parameters
c
c     theta   a double precision array with length nlat
c             containing the gaussian colatitudes in
c             increasing radians on the interval (0,pi).
c
c     wts     a double precision array with lenght nlat
c             containing the gaussian weights.
c
c     ierror = 0 no errors
c            = 1 if nlat.le.0
c
c  *****************************************************************
c
      double precision theta(nlat),wts(nlat),w,
     1 x,pi,pis2,dtheta,dthalf,cmax,zprev,zlast,zero,
     2 zhold,pb,dpb,dcor,sum,cz

      call unused_dummy(w)
      call unused_dummy(lwork)

c
c     check work space length
c
      ierror = 1
      if (nlat.le.0) return
      ierror = 0
c
c     compute weights and points analytically when nlat=1,2
c
      if (nlat.eq.1) then
      theta(1) = dacos(0.0d0)
      wts(1) = 2.0d0
      return
      end if
      if (nlat.eq.2) then
      x = dsqrt(1.0d0/3.0d0)
      theta(1) = dacos(x)
      theta(2) = dacos(-x)
      wts(1) = 1.0d0
      wts(2) = 1.0d0
      return
      end if
      eps = sqrt(dzeps(1.0d0))
      eps = eps*sqrt(eps)
      pis2 = 2.0d0*datan(1.0d0)
      pi = pis2+pis2 
      mnlat = mod(nlat,2)
      ns2 = nlat/2
      nhalf = (nlat+1)/2
      idx = ns2+2
c
      call cpdp (nlat,cz,theta(ns2+1),wts(ns2+1))
c
      dtheta = pis2/nhalf
      dthalf = dtheta/2.0d0
      cmax = .2d0*dtheta
c
c     estimate first point next to theta = pi/2
c
      if(mnlat.ne.0) then
      zero = pis2-dtheta
      zprev = pis2
      nix = nhalf-1
      else
      zero = pis2-dthalf
      nix = nhalf
      end if
 9    it = 0
 10   it = it+1
      zlast = zero
c
c     newton iterations
c
      call tpdp (nlat,zero,cz,theta(ns2+1),wts(ns2+1),pb,dpb)
      dcor = pb/dpb
      sgnd = 1.0
      if(dcor .ne. 0.0d0) sgnd = dcor/dabs(dcor)
      dcor = sgnd*min(dabs(dcor),cmax)
      zero = zero-dcor
      if(dabs(zero-zlast).gt.eps*dabs(zero)) go to 10
      theta(nix) = zero
      zhold = zero
c      wts(nix) = (nlat+nlat+1)/(dpb*dpb)
c    
c     yakimiw's formula permits using old pb and dpb
c
      wts(nix) = (nlat+nlat+1)/(dpb+pb*dcos(zlast)/dsin(zlast))**2
      nix = nix-1
      if(nix.eq.0) go to 30
      if(nix.eq.nhalf-1)  zero = 3.0*zero-pi
      if(nix.lt.nhalf-1)  zero = zero+zero-zprev
      zprev = zhold
      go to 9
c
c     extend points and weights via symmetries
c
 30   if(mnlat.ne.0) then
      theta(nhalf) = pis2
      call tpdp (nlat,pis2,cz,theta(ns2+1),wts(ns2+1),pb,dpb)
      wts(nhalf) = (nlat+nlat+1)/(dpb*dpb)
      end if
      do i=1,ns2
      wts(nlat-i+1) = wts(i)
      theta(nlat-i+1) = pi-theta(i)
      end do
      sum = 0.0d0
      do i=1,nlat
      sum = sum+wts(i)
      end do
      do i=1,nlat
      wts(i) = 2.0d0*wts(i)/sum
      end do

      return
      contains
      subroutine unused_dummy(x)
      class (*) :: x
      if (.false.) print*, shape(x)
      end
      end
      subroutine cpdp(n,cz,cp,dcp)
c
c     computes the fourier coefficients of the legendre
c     polynomial p_n^0 and its derivative. 
c     n is the degree and n/2 or (n+1)/2
c     coefficients are returned in cp depending on whether
c     n is even or odd. The same number of coefficients
c     are returned in dcp. For n even the constant 
c     coefficient is returned in cz. 
c
      double precision cp(n/2+1),dcp(n/2+1),
     1 t1,t2,t3,t4,cz
      ncp = (n+1)/2
      t1 = -1.0d0
      t2 = n+1.0d0
      t3 = 0.0d0
      t4 = n+n+1.0d0
      if(mod(n,2).eq.0) then
      cp(ncp) = 1.0d0
      do j = ncp,2,-1
      t1 = t1+2.0d0
      t2 = t2-1.0d0
      t3 = t3+1.0d0
      t4 = t4-2.0d0
      cp(j-1) = (t1*t2)/(t3*t4)*cp(j)
      end do
      t1 = t1+2.0d0
      t2 = t2-1.0d0
      t3 = t3+1.0d0
      t4 = t4-2.0d0
      cz = (t1*t2)/(t3*t4)*cp(1)
      do j=1,ncp
      dcp(j) = (j+j)*cp(j)
      end do
      else
      cp(ncp) = 1.0d0
      do j = ncp-1,1,-1
      t1 = t1+2.0d0
      t2 = t2-1.0d0
      t3 = t3+1.0d0
      t4 = t4-2.0d0
      cp(j) = (t1*t2)/(t3*t4)*cp(j+1)
      end do
      do j=1,ncp
      dcp(j) = (j+j-1)*cp(j)
      end do
      end if
      return
      end
      subroutine tpdp (n,theta,cz,cp,dcp,pb,dpb)
c
c     computes pn(theta) and its derivative dpb(theta) with 
c     respect to theta
c
      double precision cp(n/2+1),dcp(n/2+1),cz,
     1  pb,dpb,fn,theta,cdt,sdt,cth,sth,chh
c
      fn = n
      cdt = dcos(theta+theta)
      sdt = dsin(theta+theta)
      if(mod(n,2) .eq.0) then
c
c     n even
c
      kdo = n/2
      pb = .5d0*cz
      dpb = 0.0d0
      if(n .gt. 0) then
      cth = cdt
      sth = sdt
      do 170 k=1,kdo
c      pb = pb+cp(k)*cos(2*k*theta)
      pb = pb+cp(k)*cth
c      dpb = dpb-(k+k)*cp(k)*sin(2*k*theta)
      dpb = dpb-dcp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  170 continue
      end if
      else
c
c     n odd
c
      kdo = (n+1)/2
      pb = 0.0d0
      dpb = 0.0d0
      cth = dcos(theta)
      sth = dsin(theta)
      do 190 k=1,kdo
c      pb = pb+cp(k)*cos((2*k-1)*theta)
      pb = pb+cp(k)*cth
c      dpb = dpb-(k+k-1)*cp(k)*sin((2*k-1)*theta)
      dpb = dpb-dcp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
  190 continue
      end if
      return
      end
      real function dzeps (x)
      double precision  x
c
c     estimate unit roundoff in quantities of size x.
c
      double precision a,b,c,eps
c
c     this program should function properly on all systems
c     satisfying the following two assumptions,
c        1.  the base used in representing floating point
c            numbers is not a power of three.
c        2.  the quantity  a  in statement 10 is represented to 
c            the accuracy used in floating point variables
c            that are stored in memory.
c     the statement number 10 and the go to 10 are intended to
c     force optimizing compilers to generate code satisfying 
c     assumption 2.
c     under these assumptions, it should be true that,
c            a  is not exactly equal to four-thirds,
c            b  has a zero for its last bit or digit,
c            c  is not exactly equal to one,
c            eps  measures the separation of 1.0 from
c                 the next larger floating point number.
c     the developers of eispack would appreciate being informed
c     about any systems where these assumptions do not hold.
c
c     this version dated 4/6/83.
c
      a = 4.0d0/3.0d0
   10 b = a - 1.0d0
      c = b + b + b
      eps = abs(c-1.0d0)
      if (eps .eq. 0.0d0) go to 10
      dzeps = eps*dabs(x)
      return
      end
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file shagc.f
c
c     this file contains code and documentation for subroutines
c     shagc and shagci
c
c ... files which must be loaded with shagc.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c
c     subroutine shagc(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                 wshagc,lshagc,work,lwork,ierror)
c
c     subroutine shagc performs the spherical harmonic analysis
c     on the array g and stores the result in the arrays a and b.
c     the analysis is performed on a gaussian grid in colatitude
c     and an equally spaced grid in longitude.  the associated
c     legendre functions are recomputed rather than stored as they
c     are in subroutine shags.  the analysis is described below
c     at output parameters a,b.
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
c            full sphere. these lie in the interval (0,pi) and are compu
c            in radians in theta(1),...,theta(nlat) by subroutine gaqd.
c            if nlat is odd the equator will be included as the grid poi
c            theta((nlat+1)/2).  if nlat is even the equator will be
c            excluded as a grid point and will lie half way between
c            theta(nlat/2) and theta(nlat/2+1). nlat must be at least 3.
c            note: on the half sphere, the number of grid points in the
c            colatitudinal direction is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     isym   = 0  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the analysis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the analysis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the analysis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the analysis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of analyses.  in the program that calls shagc,
c            the arrays g,a and b can be three dimensional in which
c            case multiple analyses will be performed.  the third
c            index is the analysis index which assumes the values
c            k=1,...,nt.  for a single analysis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     g      a two or three dimensional array (see input parameter
c            nt) that contains the discrete function to be analyzed.
c            g(i,j) contains the value of the function at the gaussian
c            point theta(i) and longitude point phi(j) = (j-1)*2*pi/nlon
c            the index ranges are defined above at the input parameter
c            isym.
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shagc. if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg must
c            be at least nlat/2 if nlat is even or at least (nlat+1)/2
c            if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shagc. jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shagc. mdab must be at least
c            min0((nlon+2)/2,nlat) if nlon is even or at least
c            min0((nlon+1)/2,nlat) if nlon is odd
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shaec. ndab must be at least nlat
c
c     wshagc an array which must be initialized by subroutine shagci.
c            once initialized, wshagc can be used repeatedly by shagc.
c            as long as nlat and nlon remain unchanged.  wshagc must
c            not be altered between calls of shagc.
c
c     lshagc the dimension of the array wshagc as it appears in the
c            program that calls shagc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshagc must be at least
c
c                  nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shagc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c                      nlat*(nlon*nt+max0(3*l2,nlon))
c
c            if isym is not zero then lwork must be at least
c
c                      l2*(nlon*nt+max0(3*nlat,nlon))
c
c     **************************************************************
c
c     output parameters
c
c     a,b    both a,b are two or three dimensional arrays (see input
c            parameter nt) that contain the spherical harmonic
c            coefficients in the representation of g(i,j) given in the
c            discription of subroutine shagc. for isym=0, a(m,n) and
c            b(m,n) are given by the equations listed below. symmetric
c            versions are used when isym is greater than zero.
c
c     definitions
c
c     1. the normalized associated legendre functions
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta).
c
c     2. the fourier transform of g(i,j).
c
c     c(m,i)          = 2/nlon times the sum from j=1 to j=nlon of
c                       g(i,j)*cos((m-1)*(j-1)*2*pi/nlon)
c                       (the first and last terms in this sum
c                       are divided by 2)
c
c     s(m,i)          = 2/nlon times the sum from j=2 to j=nlon of
c                       g(i,j)*sin((m-1)*(j-1)*2*pi/nlon)
c
c
c     3. the gaussian points and weights on the sphere
c        (computed by subroutine gaqd).
c
c        theta(1),...,theta(nlat) (gaussian pts in radians)
c        wts(1),...,wts(nlat) (corresponding gaussian weights)
c
c     4. the maximum (plus one) longitudinal wave number
c
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c
c     then for m=0,...,mmax-1 and n=m,...,nlat-1 the arrays a,b
c     are given by
c
c     a(m+1,n+1)     =  the sum from i=1 to i=nlat of
c                       c(m+1,i)*wts(i)*pbar(m,n,theta(i))
c
c     b(m+1,n+1)      = the sum from i=1 to nlat of
c                       s(m+1,i)*wts(i)*pbar(m,n,theta(i))
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of nt
c            = 5  error in the specification of idg
c            = 6  error in the specification of jdg
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lshagc
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c
c     subroutine shagci(nlat,nlon,wshagc,lshagc,dwork,ldwork,ierror)
c
c     subroutine shagci initializes the array wshagc which can then
c     be used repeatedly by subroutines shagc. it precomputes
c     and stores in wshagc quantities such as gaussian weights,
c     legendre polynomial coefficients, and fft trigonometric tables.
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
c            full sphere. these lie in the interval (0,pi) and are compu
c            in radians in theta(1),...,theta(nlat) by subroutine gaqd.
c            if nlat is odd the equator will be included as the grid poi
c            theta((nlat+1)/2).  if nlat is even the equator will be
c            excluded as a grid point and will lie half way between
c            theta(nlat/2) and theta(nlat/2+1). nlat must be at least 3.
c            note: on the half sphere, the number of grid points in the
c            colatitudinal direction is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     wshagc an array which must be initialized by subroutine shagci.
c            once initialized, wshagc can be used repeatedly by shagc
c            as long as nlat and nlon remain unchanged.  wshagc must
c            not be altered between calls of shagc.
c
c     lshagc the dimension of the array wshagc as it appears in the
c            program that calls shagc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshagc must be at least
c
c                  nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c     dwork   a double precision work array that does not have to be saved.
c
c     ldwork  the dimension of the array dwork as it appears in the
c            program that calls shagci. ldwork must be at least
c
c                nlat*(nlat+4)
c
c     output parameter
c
c     wshagc an array which must be initialized before calling shagc or
c            once initialized, wshagc can be used repeatedly by shagc or
c            as long as nlat and nlon remain unchanged.  wshagc must not
c            altered between calls of shagc.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshagc
c            = 4  error in the specification of ldwork
c            = 5  failure in gaqd to compute gaussian points
c                 (due to failure in eigenvalue routine)
c
c
c ****************************************************************
      subroutine shagc(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
     1                    wshagc,lshagc,work,lwork,ierror)
c     subroutine shagc performs the spherical harmonic analysis on
c     a gaussian grid on the array(s) in g and returns the coefficients
c     in array(s) a,b. the necessary legendre polynomials are computed
c     as needed in this version.
c
      real g(idg,jdg,1),a(mdab,ndab,1),b(mdab,ndab,1),
     1          wshagc(lshagc),work(lwork)
c     check input parameters
      ierror = 1
      if (nlat.lt.3) return
      ierror = 2
      if (nlon.lt.4) return
      ierror = 3
      if (isym.lt.0 .or.isym.gt.2) return
      ierror = 4
      if (nt.lt.1) return
c     set upper limit on m for spherical harmonic basis
      l = min0((nlon+2)/2,nlat)
c     set gaussian point nearest equator pointer
      late = (nlat+mod(nlat,2))/2
c     set number of grid points for analysis/synthesis
      lat = nlat
      if (isym.ne.0) lat = late
      ierror = 5
      if (idg.lt.lat) return
      ierror = 6
      if (jdg.lt.nlon) return
      ierror = 7
      if(mdab .lt. l) return
      ierror = 8
      if(ndab .lt. nlat) return
      l1 = l
      l2 = late
      ierror = 9
c     check permanent work space length
      if (lshagc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 10
c     check temporary work space length
      if (isym.eq.0) then
      if(lwork.lt.nlat*(nlon*nt+max0(3*l2,nlon))) return
      else
c     isym.ne.0
      if(lwork.lt.l2*(nlon*nt+max0(3*nlat,nlon))) return
      end if
      ierror = 0
c     starting address for gaussian wts in shigc and fft values
      iwts = 1
      ifft = nlat+2*nlat*late+3*(l*(l-1)/2+(nlat-l)*(l-1))+1
c     set pointers for internal storage of g and legendre polys
      ipmn = lat*nlon*nt+1
      call shagc1(nlat,nlon,l,lat,isym,g,idg,jdg,nt,a,b,mdab,ndab,
     1wshagc,wshagc(iwts),wshagc(ifft),late,work(ipmn),work)
      return
      end
      subroutine shagc1(nlat,nlon,l,lat,mode,gs,idg,jdg,nt,a,b,mdab,
     1                  ndab,w,wts,wfft,late,pmn,g)
      real gs(idg,jdg,nt),a(mdab,ndab,nt),
     1          b(mdab,ndab,nt),g(lat,nlon,nt)
      real w(1),wts(nlat),wfft(1),pmn(nlat,late,3)
c     set gs array internally in shagc1
      do 100 k=1,nt
      do 100 j=1,nlon
      do 100 i=1,lat
      g(i,j,k) = gs(i,j,k)
  100 continue
c     do fourier transform
      do 101 k=1,nt
      call hrfftf(lat,nlon,g(1,1,k),lat,wfft,pmn)
  101 continue
c     scale result
      sfn = 2.0/float(nlon)
      do 102 k=1,nt
      do 102 j=1,nlon
      do 102 i=1,lat
      g(i,j,k) = sfn*g(i,j,k)
  102 continue
c     compute using gaussian quadrature
c     a(n,m) = s (ga(theta,m)*pnm(theta)*sin(theta)*dtheta)
c     b(n,m) = s (gb(theta,m)*pnm(theta)*sin(theta)*dtheta)
c     here ga,gb are the cos(phi),sin(phi) coefficients of
c     the fourier expansion of g(theta,phi) in phi.  as a result
c     of the above fourier transform they are stored in array
c     g as follows:
c     for each theta(i) and k= l-1
c     ga(0),ga(1),gb(1),ga(2),gb(2),...,ga(k-1),gb(k-1),ga(k)
c     correspond to (in the case nlon=l+l-2)
c     g(i,1),g(i,2),g(i,3),g(i,4),g(i,5),...,g(i,2l-4),g(i,2l-3),g(i,2l-
c     initialize coefficients to zero
      do 103 k=1,nt
      do 103 np1=1,nlat
      do 103 mp1=1,l
      a(mp1,np1,k) = 0.0
      b(mp1,np1,k) = 0.0
  103 continue
c     set m+1 limit on b(m+1) calculation
      lm1 = l
      if (nlon .eq. l+l-2) lm1 = l-1
      if (mode.eq.0) then
c     for full sphere (mode=0) and even/odd reduction:
c     overwrite g(i) with (g(i)+g(nlat-i+1))*wts(i)
c     overwrite g(nlat-i+1) with (g(i)-g(nlat-i+1))*wts(i)
      nl2 = nlat/2
      do 104 k=1,nt
      do 104 j=1,nlon
      do 105 i=1,nl2
      is = nlat-i+1
      t1 = g(i,j,k)
      t2 = g(is,j,k)
      g(i,j,k) = wts(i)*(t1+t2)
      g(is,j,k) = wts(i)*(t1-t2)
  105 continue
c     adjust equator if necessary(nlat odd)
      if (mod(nlat,2).ne.0) g(late,j,k) = wts(late)*g(late,j,k)
  104 continue
c     set m = 0 coefficients first
      m = 0
      call legin(mode,l,nlat,m,w,pmn,km)
      do 106 k=1,nt
      do 106 i=1,late
      is = nlat-i+1
      do 107 np1=1,nlat,2
c     n even
      a(1,np1,k) = a(1,np1,k)+g(i,1,k)*pmn(np1,i,km)
  107 continue
      do 108 np1=2,nlat,2
c     n odd
      a(1,np1,k) = a(1,np1,k)+g(is,1,k)*pmn(np1,i,km)
  108 continue
  106 continue
c     compute coefficients for which b(m,n) is available
      do 109 mp1=2,lm1
      m = mp1-1
      mp2 = m+2
c     compute pmn for all i and n=m,...,l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 110 k=1,nt
      do 111 i=1,late
      is = nlat-i+1
c     n-m even
      do 112 np1=mp1,nlat,2
      a(mp1,np1,k) = a(mp1,np1,k)+g(i,2*m,k)*pmn(np1,i,km)
      b(mp1,np1,k) = b(mp1,np1,k)+g(i,2*m+1,k)*pmn(np1,i,km)
  112 continue
c     n-m odd
      do 113 np1=mp2,nlat,2
      a(mp1,np1,k) = a(mp1,np1,k)+g(is,2*m,k)*pmn(np1,i,km)
      b(mp1,np1,k) = b(mp1,np1,k)+g(is,2*m+1,k)*pmn(np1,i,km)
  113 continue
  111 continue
  110 continue
  109 continue
      if (nlon .eq. l+l-2) then
c     compute a(l,np1) coefficients only
      m = l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 114 k=1,nt
      do 114 i=1,late
      is = nlat-i+1
c     n-m even
      do 124 np1=l,nlat,2
      a(l,np1,k) = a(l,np1,k)+0.5*g(i,nlon,k)*pmn(np1,i,km)
  124 continue
      lp1 = l+1
c     n-m odd
      do 125 np1=lp1,nlat,2
      a(l,np1,k) = a(l,np1,k)+0.5*g(is,nlon,k)*pmn(np1,i,km)
  125 continue
  114 continue
      end if
      else
c     half sphere
c     overwrite g(i) with wts(i)*(g(i)+g(i)) for i=1,...,nlate/2
      nl2 = nlat/2
      do 116  k=1,nt
      do 116 j=1,nlon
      do 115 i=1,nl2
      g(i,j,k) = wts(i)*(g(i,j,k)+g(i,j,k))
  115 continue
c     adjust equator separately if a grid point
      if (nl2.lt.late) g(late,j,k) = wts(late)*g(late,j,k)
  116 continue
c     set m = 0 coefficients first
      m = 0
      call legin(mode,l,nlat,m,w,pmn,km)
      ms = 1
      if (mode.eq.1) ms = 2
      do 117 k=1,nt
      do 117 i=1,late
      do 117 np1=ms,nlat,2
      a(1,np1,k) = a(1,np1,k)+g(i,1,k)*pmn(np1,i,km)
  117 continue
c     compute coefficients for which b(m,n) is available
      do 118 mp1=2,lm1
      m = mp1-1
      ms = mp1
      if (mode.eq.1) ms = mp1+1
c     compute pmn for all i and n=m,...,nlat-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 119 k=1,nt
      do 119 i=1,late
      do 119 np1=ms,nlat,2
      a(mp1,np1,k) = a(mp1,np1,k)+g(i,2*m,k)*pmn(np1,i,km)
      b(mp1,np1,k) = b(mp1,np1,k)+g(i,2*m+1,k)*pmn(np1,i,km)
  119 continue
  118 continue
      if (nlon.eq.l+l-2) then
c     compute coefficient a(l,np1) only
      m = l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      ns = l
      if (mode.eq.1) ns = l+1
      do 120 k=1,nt
      do 120 i=1,late
      do 120 np1=ns,nlat,2
      a(l,np1,k) = a(l,np1,k)+0.5*g(i,nlon,k)*pmn(np1,i,km)
  120 continue
      end if
      end if
      return
      end
      subroutine shagci(nlat,nlon,wshagc,lshagc,dwork,ldwork,ierror)
c     this subroutine must be called before calling shagc with
c     fixed nlat,nlon. it precomputes quantites such as the gaussian
c     points and weights, m=0,m=1 legendre polynomials, recursion
c     recursion coefficients.
      real wshagc(lshagc)
      double precision dwork(ldwork)
      ierror = 1
      if (nlat.lt.3) return
      ierror = 2
      if (nlon.lt.4) return
c     set triangular truncation limit for spherical harmonic basis
      l = min0((nlon+2)/2,nlat)
c     set equator or nearest point (if excluded) pointer
      late = (nlat+mod(nlat,2))/2
      l1 = l
      l2 = late
      ierror = 3
c     check permanent work space length
      if (lshagc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 4
      if (ldwork.lt.nlat*(nlat+4))return
      ierror = 0
c     set pointers
      i1 = 1
      i2 = i1+nlat
      i3 = i2+nlat*late
      i4 = i3+nlat*late
      i5 = i4+l*(l-1)/2 +(nlat-l)*(l-1)
      i6 = i5+l*(l-1)/2 +(nlat-l)*(l-1)
      i7 = i6+l*(l-1)/2 +(nlat-l)*(l-1)
c     set indices in temp work for double precision gaussian wts and pts
      idth = 1
      idwts = idth+nlat
      iw = idwts+nlat
      call shagci1(nlat,nlon,l,late,wshagc(i1),wshagc(i2),wshagc(i3),
     1wshagc(i4),wshagc(i5),wshagc(i6),wshagc(i7),dwork(idth),
     2dwork(idwts),dwork(iw),ierror)
      if (ierror.ne.0) ierror = 5
      return
      end
      subroutine shagci1(nlat,nlon,l,late,wts,p0n,p1n,abel,bbel,cbel,
     1                  wfft,dtheta,dwts,work,ier)
      real wts(nlat),p0n(nlat,late),p1n(nlat,late),abel(1),bbel(1),
     1 cbel(1),wfft(1)
      double precision pb,dtheta(nlat),dwts(nlat),work(*)
c     compute the nlat  gaussian points and weights, the
c     m=0,1 legendre polys for gaussian points and all n,
c     and the legendre recursion coefficients
c     define index function used in storing
c     arrays for recursion coefficients (functions of (m,n))
c     the index function indx(m,n) is defined so that
c     the pairs (m,n) map to [1,2,...,indx(l-1,l-1)] with no
c     "holes" as m varies from 2 to n and n varies from 2 to l-1.
c     (m=0,1 are set from p0n,p1n for all n)
c     define for 2.le.n.le.l-1
      indx(m,n) = (n-1)*(n-2)/2+m-1
c     define index function for l.le.n.le.nlat
      imndx(m,n) = l*(l-1)/2+(n-l-1)*(l-1)+m-1
c     preset quantites for fourier transform
      call hrffti(nlon,wfft)
c     compute double precision gaussian points and weights
c     lw = 4*nlat*(nlat+1)+2
      lw = nlat*(nlat+2)
      call gaqd(nlat,dtheta,dwts,work(1),lw,ier)
      if (ier.ne.0) return
c     store gaussian weights single precision to save computation
c     in inner loops in analysis
      do 100 i=1,nlat
      wts(i) = dwts(i)
  100 continue
c     initialize p0n,p1n using double precision dnlfk,dnlft
      do 101 np1=1,nlat
      do 101 i=1,late
      p0n(np1,i) = 0.0
      p1n(np1,i) = 0.0
  101 continue
c     compute m=n=0 legendre polynomials for all theta(i)
      np1 = 1
      n = 0
      m = 0
      call dnlfk(m,n,work)
      do 103 i=1,late
      call dnlft(m,n,dtheta(i),work,pb)
      p0n(1,i) = pb
  103 continue
c     compute p0n,p1n for all theta(i) when n.gt.0
      do 104 np1=2,nlat
      n = np1-1
      m = 0
      call dnlfk(m,n,work)
      do 105 i=1,late
      call dnlft(m,n,dtheta(i),work,pb)
      p0n(np1,i) = pb
  105 continue
c     compute m=1 legendre polynomials for all n and theta(i)
      m = 1
      call dnlfk(m,n,work)
      do 106 i=1,late
      call dnlft(m,n,dtheta(i),work,pb)
      p1n(np1,i) = pb
  106 continue
  104 continue
c     compute and store swarztrauber recursion coefficients
c     for 2.le.m.le.n and 2.le.n.le.nlat in abel,bbel,cbel
      do 107 n=2,nlat
      mlim = min0(n,l)
      do 107 m=2,mlim
      imn = indx(m,n)
      if (n.ge.l) imn = imndx(m,n)
      abel(imn)=sqrt(float((2*n+1)*(m+n-2)*(m+n-3))/
     1               float(((2*n-3)*(m+n-1)*(m+n))))
      bbel(imn)=sqrt(float((2*n+1)*(n-m-1)*(n-m))/
     1               float(((2*n-3)*(m+n-1)*(m+n))))
      cbel(imn)=sqrt(float((n-m+1)*(n-m+2))/
     1               float(((n+m-1)*(n+m))))
  107 continue
      return
      end
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c ... file shsgc.f
c
c     this file contains code and documentation for subroutines
c     shsgc and shsgci
c
c ... files which must be loaded with shsgc.f
c
c     sphcom.f, hrfft.f, gaqd.f
c
c     subroutine shsgc(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
c    +                 wshsgc,lshsgc,work,lwork,ierror)
c
c     subroutine shsgc performs the spherical harmonic synthesis
c     on the arrays a and b and stores the result in the array g.
c     the synthesis is performed on an equally spaced longitude grid
c     and a gaussian colatitude grid.  the associated legendre functions
c     are recomputed rather than stored as they are in subroutine
c     shsgs.  the synthesis is described below at output parameter
c     g.
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
c            full sphere. these lie in the interval (0,pi) and are compu
c            in radians in theta(1),...,theta(nlat) by subroutine gaqd.
c            if nlat is odd the equator will be included as the grid poi
c            theta((nlat+1)/2).  if nlat is even the equator will be
c            excluded as a grid point and will lie half way between
c            theta(nlat/2) and theta(nlat/2+1). nlat must be at least 3.
c            note: on the half sphere, the number of grid points in the
c            colatitudinal direction is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     isym   = 0  no symmetries exist about the equator. the synthesis
c                 is performed on the entire sphere.  i.e. on the
c                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
c                 (see description of g below)
c
c            = 1  g is antisymmetric about the equator. the synthesis
c                 is performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the synthesis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the synthesis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c
c            = 2  g is symmetric about the equator. the synthesis is
c                 performed on the northern hemisphere only.  i.e.
c                 if nlat is odd the synthesis is performed on the
c                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
c                 if nlat is even the synthesis is performed on the
c                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c     nt     the number of syntheses.  in the program that calls shsgc,
c            the arrays g,a and b can be three dimensional in which
c            case multiple synthesis will be performed.  the third
c            index is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that the arrays g,a and b
c            have only two dimensions.
c
c     idg    the first dimension of the array g as it appears in the
c            program that calls shsgc. if isym equals zero then idg
c            must be at least nlat.  if isym is nonzero then idg must
c            be at least nlat/2 if nlat is even or at least (nlat+1)/2
c            if nlat is odd.
c
c     jdg    the second dimension of the array g as it appears in the
c            program that calls shsgc. jdg must be at least nlon.
c
c     mdab   the first dimension of the arrays a and b as it appears
c            in the program that calls shsgc. mdab must be at least
c            min0((nlon+2)/2,nlat) if nlon is even or at least
c            min0((nlon+1)/2,nlat) if nlon is odd
c
c     ndab   the second dimension of the arrays a and b as it appears
c            in the program that calls shsgc. ndab must be at least nlat
c
c     a,b    two or three dimensional arrays (see the input parameter
c            nt) that contain the coefficients in the spherical harmonic
c            expansion of g(i,j) given below at the definition of the
c            output parameter g.  a(m,n) and b(m,n) are defined for
c            indices m=1,...,mmax and n=m,...,nlat where mmax is the
c            maximum (plus one) longitudinal wave number given by
c            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     wshsgc an array which must be initialized by subroutine shsgci.
c            once initialized, wshsgc can be used repeatedly by shsgc
c            as long as nlat and nlon remain unchanged.  wshsgc must
c            not be altered between calls of shsgc.
c
c     lshsgc the dimension of the array wshsgc as it appears in the
c            program that calls shsgc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgc must be at least
c
c               nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls shsgc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if isym is zero then lwork must be at least
c
c                      nlat*(nlon*nt+max0(3*l2,nlon))
c
c            if isym is not zero then lwork must be at least
c
c                      l2*(nlon*nt+max0(3*nlat,nlon))
c
c     **************************************************************
c
c     output parameters
c
c     g      a two or three dimensional array (see input parameter nt)
c            that contains the discrete function which is synthesized.
c            g(i,j) contains the value of the function at the gaussian
c            colatitude point theta(i) and longitude point
c            phi(j) = (j-1)*2*pi/nlon. the index ranges are defined
c            above at the input parameter isym.  for isym=0, g(i,j)
c            is given by the the equations listed below.  symmetric
c            versions are used when isym is greater than zero.
c
c     the normalized associated legendre functions are given by
c
c     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
c                       *sin(theta)**m/(2**n*factorial(n)) times the
c                       (n+m)th derivative of (x**2-1)**n with respect
c                       to x=cos(theta)
c
c
c     define the maximum (plus one) longitudinal wave number
c     as   mmax = min0(nlat,(nlon+2)/2) if nlon is even or
c          mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     then g(i,j) = the sum from n=0 to n=nlat-1 of
c
c                   .5*pbar(0,n,theta(i))*a(1,n+1)
c
c              plus the sum from m=1 to m=mmax-1 of
c
c                   the sum from n=m to n=nlat-1 of
c
c              pbar(m,n,theta(i))*(a(m+1,n+1)*cos(m*phi(j))
c                                    -b(m+1,n+1)*sin(m*phi(j)))
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of isym
c            = 4  error in the specification of nt
c            = 5  error in the specification of idg
c            = 6  error in the specification of jdg
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lwshig
c            = 10 error in the specification of lwork
c
c
c ****************************************************************
c
c     subroutine shsgci(nlat,nlon,wshsgc,lshsgc,dwork,ldwork,ierror)
c
c     subroutine shsgci initializes the array wshsgc which can then
c     be used repeatedly by subroutines shsgc. it precomputes
c     and stores in wshsgc quantities such as gaussian weights,
c     legendre polynomial coefficients, and fft trigonometric tables.
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
c            full sphere. these lie in the interval (0,pi) and are compu
c            in radians in theta(1),...,theta(nlat) by subroutine gaqd.
c            if nlat is odd the equator will be included as the grid poi
c            theta((nlat+1)/2).  if nlat is even the equator will be
c            excluded as a grid point and will lie half way between
c            theta(nlat/2) and theta(nlat/2+1). nlat must be at least 3.
c            note: on the half sphere, the number of grid points in the
c            colatitudinal direction is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than or equal to 4. the efficiency of the computation is
c            improved when nlon is a product of small prime numbers.
c
c     wshsgc an array which must be initialized by subroutine shsgci.
c            once initialized, wshsgc can be used repeatedly by shsgc
c            as long as nlat and nlon remain unchanged.  wshsgc must
c            not be altered between calls of shsgc.
c
c     lshsgc the dimension of the array wshsgc as it appears in the
c            program that calls shsgc. define
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgc must be at least
c
c                  nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c     dwork  a double precision work array that does not have to be saved.
c
c     ldwork the dimension of the array dwork as it appears in the
c            program that calls shsgci. ldwork must be at least
c
c                 nlat*(nlat+4)
c
c     output parameter
c
c     wshsgc an array which must be initialized before calling shsgc.
c            once initialized, wshsgc can be used repeatedly by shsgc
c            as long as nlat and nlon remain unchanged.  wshsgc must not
c            altered between calls of shsgc.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lshsgc
c            = 4  error in the specification of ldwork
c            = 5  failure in gaqd to compute gaussian points
c                 (due to failure in eigenvalue routine)
c
c
c ****************************************************************
      subroutine shsgc(nlat,nlon,mode,nt,g,idg,jdg,a,b,mdab,ndab,
     1                    wshsgc,lshsgc,work,lwork,ierror)
c     subroutine shsgc performs the spherical harmonic synthesis on
c     a gaussian grid using the coefficients in array(s) a,b and returns
c     the results in array(s) g.  the legendre polynomials are computed
c     as needed in this version.
c
      real g(idg,jdg,1),a(mdab,ndab,1),b(mdab,ndab,1),
     1          wshsgc(lshsgc),work(lwork)
c     check input parameters
      ierror = 1
      if (nlat.lt.3) return
      ierror = 2
      if (nlon.lt.4) return
      ierror = 3
      if (mode.lt.0 .or.mode.gt.2) return
      ierror = 4
      if (nt.lt.1) return
c     set limit for m iin a(m,n),b(m,n) computation
      l = min0((nlon+2)/2,nlat)
c     set gaussian point nearest equator pointer
      late = (nlat+mod(nlat,2))/2
c     set number of grid points for analysis/synthesis
      lat = nlat
      if (mode.ne.0) lat = late
      ierror = 5
      if (idg.lt.lat) return
      ierror = 6
      if (jdg.lt.nlon) return
      ierror = 7
      if(mdab .lt. l) return
      ierror = 8
      if(ndab .lt. nlat) return
      l1 = l
      l2 = late
      ierror = 9
c     check permanent work space length
      if (lshsgc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 10
c     check temporary work space length
      if (mode.eq.0) then
      if(lwork.lt.nlat*(nlon*nt+max0(3*l2,nlon)))return
      else
c     mode.ne.0
      if(lwork.lt.l2*(nlon*nt+max0(3*nlat,nlon))) return
      end if
      ierror = 0
c     starting address  fft values
      ifft = nlat+2*nlat*late+3*(l*(l-1)/2+(nlat-l)*(l-1))+1
c     set pointers for internal storage of g and legendre polys
      ipmn = lat*nlon*nt+1
      call shsgc1(nlat,nlon,l,lat,mode,g,idg,jdg,nt,a,b,mdab,ndab,
     1wshsgc,wshsgc(ifft),late,work(ipmn),work)
      return
      end
      subroutine shsgc1(nlat,nlon,l,lat,mode,gs,idg,jdg,nt,a,b,mdab,
     1                  ndab,w,wfft,late,pmn,g)
      real gs(idg,jdg,nt),a(mdab,ndab,nt),b(mdab,ndab,nt)
      real w(1),pmn(nlat,late,3),g(lat,nlon,nt),wfft(1)
c     reconstruct fourier coefficients in g on gaussian grid
c     using coefficients in a,b
c     set m+1 limit for b coefficient calculation
      lm1 = l
      if (nlon .eq. l+l-2) lm1 = l-1
c     initialize to zero
      do 100 k=1,nt
      do 100 j=1,nlon
      do 100 i=1,lat
      g(i,j,k) = 0.0
  100 continue
      if (mode.eq.0) then
c     set first column in g
      m = 0
c     compute pmn for all i and n=m,...,l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 101 k=1,nt
c     n even
      do 102 np1=1,nlat,2
      do 102 i=1,late
      g(i,1,k) = g(i,1,k)+a(1,np1,k)*pmn(np1,i,km)
  102 continue
c     n odd
      nl2 = nlat/2
      do 103 np1=2,nlat,2
      do 103 i=1,nl2
      is = nlat-i+1
      g(is,1,k) = g(is,1,k)+a(1,np1,k)*pmn(np1,i,km)
  103 continue
c     restore m=0 coefficents (reverse implicit even/odd reduction)
      do 112 i=1,nl2
      is = nlat-i+1
      t1 = g(i,1,k)
      t3 = g(is,1,k)
      g(i,1,k) = t1+t3
      g(is,1,k) = t1-t3
  112 continue
  101 continue
c     sweep  columns of g for which b is available
      do 104 mp1=2,lm1
      m = mp1-1
      mp2 = m+2
c     compute pmn for all i and n=m,...,l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 105 k=1,nt
c     for n-m even store (g(i,p,k)+g(nlat-i+1,p,k))/2 in g(i,p,k) p=2*m,
c     for i=1,...,late
      do 106 np1=mp1,nlat,2
      do 107 i=1,late
      g(i,2*m,k) = g(i,2*m,k)+a(mp1,np1,k)*pmn(np1,i,km)
      g(i,2*m+1,k) = g(i,2*m+1,k)+b(mp1,np1,k)*pmn(np1,i,km)
  107 continue
  106 continue
c     for n-m odd store g(i,p,k)-g(nlat-i+1,p,k) in g(nlat-i+1,p,k)
c     for i=1,...,nlat/2 (p=2*m,p=2*m+1)
      do 108 np1=mp2,nlat,2
      do 109 i=1,nl2
      is = nlat-i+1
      g(is,2*m,k) = g(is,2*m,k)+a(mp1,np1,k)*pmn(np1,i,km)
      g(is,2*m+1,k) = g(is,2*m+1,k)+b(mp1,np1,k)*pmn(np1,i,km)
  109 continue
  108 continue
c     now set fourier coefficients using even-odd reduction above
      do 110 i=1,nl2
      is = nlat-i+1
      t1 = g(i,2*m,k)
      t2 = g(i,2*m+1,k)
      t3 = g(is,2*m,k)
      t4 = g(is,2*m+1,k)
      g(i,2*m,k) = t1+t3
      g(i,2*m+1,k) = t2+t4
      g(is,2*m,k) = t1-t3
      g(is,2*m+1,k) = t2-t4
  110 continue
  105 continue
  104 continue
c     set last column (using a only)
      if (nlon.eq. l+l-2) then
      m = l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 111 k=1,nt
c     n-m even
      do 131 np1=l,nlat,2
      do 131 i=1,late
      g(i,nlon,k) = g(i,nlon,k)+2.0*a(l,np1,k)*pmn(np1,i,km)
  131 continue
      lp1 = l+1
c     n-m odd
      do 132 np1=lp1,nlat,2
      do 132 i=1,nl2
      is = nlat-i+1
      g(is,nlon,k) = g(is,nlon,k)+2.0*a(l,np1,k)*pmn(np1,i,km)
  132 continue
      do 133 i=1,nl2
      is = nlat-i+1
      t1 = g(i,nlon,k)
      t3 = g(is,nlon,k)
      g(i,nlon,k)= t1+t3
      g(is,nlon,k)= t1-t3
  133 continue
  111 continue
      end if
      else
c     half sphere (mode.ne.0)
c     set first column in g
      m = 0
      meo = 1
      if (mode.eq.1) meo = 2
      ms = m+meo
c     compute pmn for all i and n=m,...,l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 113 k=1,nt
      do 113 np1=ms,nlat,2
      do 113 i=1,late
      g(i,1,k) = g(i,1,k)+a(1,np1,k)*pmn(np1,i,km)
  113 continue
c     sweep interior columns of g
      do 114 mp1=2,lm1
      m = mp1-1
      ms = m+meo
c     compute pmn for all i and n=m,...,l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      do 115 k=1,nt
      do 115 np1=ms,nlat,2
      do 115 i=1,late
      g(i,2*m,k) = g(i,2*m,k)+a(mp1,np1,k)*pmn(np1,i,km)
      g(i,2*m+1,k) = g(i,2*m+1,k)+b(mp1,np1,k)*pmn(np1,i,km)
  115 continue
  114 continue
      if (nlon.eq.l+l-2) then
c     set last column
      m = l-1
      call legin(mode,l,nlat,m,w,pmn,km)
      ns = l
      if (mode.eq.1) ns = l+1
      do 116 k=1,nt
      do 116 i=1,late
      do 116 np1=ns,nlat,2
      g(i,nlon,k) = g(i,nlon,k)+2.0*a(l,np1,k)*pmn(np1,i,km)
  116 continue
      end if
      end if
c     do inverse fourier transform
      do 120 k=1,nt
      call hrfftb(lat,nlon,g(1,1,k),lat,wfft,pmn)
  120 continue
c     scale output in gs
      do 122 k=1,nt
      do 122 j=1,nlon
      do 122 i=1,lat
      gs(i,j,k) = 0.5*g(i,j,k)
  122 continue
      return
      end
      subroutine shsgci(nlat,nlon,wshsgc,lshsgc,dwork,ldwork,ierror)
c     this subroutine must be called before calling shsgc with
c     fixed nlat,nlon. it precomputes quantites such as the gaussian
c     points and weights, m=0,m=1 legendre polynomials, recursion
c     recursion coefficients.
      real wshsgc(lshsgc)
      double precision dwork(ldwork)
      ierror = 1
      if (nlat.lt.3) return
      ierror = 2
      if (nlon.lt.4) return
c     set triangular truncation limit for spherical harmonic basis
      l = min0((nlon+2)/2,nlat)
c     set equator or nearest point (if excluded) pointer
      late = (nlat+mod(nlat,2))/2
      l1 = l
      l2 = late
      ierror = 3
c     check permanent work space length
      if (lshsgc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 4
      if (ldwork .lt. nlat*(nlat+4)) return
      ierror = 0
c     set pointers
      i1 = 1
      i2 = i1+nlat
      i3 = i2+nlat*late
      i4 = i3+nlat*late
      i5 = i4+l*(l-1)/2 +(nlat-l)*(l-1)
      i6 = i5+l*(l-1)/2 +(nlat-l)*(l-1)
      i7 = i6+l*(l-1)/2 +(nlat-l)*(l-1)
c     set indices in temp work for double precision gaussian wts and pts
      idth = 1
      idwts = idth+nlat
      iw = idwts+nlat
      call shsgci1(nlat,nlon,l,late,wshsgc(i1),wshsgc(i2),wshsgc(i3),
     1wshsgc(i4),wshsgc(i5),wshsgc(i6),wshsgc(i7),dwork(idth),
     2dwork(idwts),dwork(iw),ierror)
      if (ierror.ne.0) ierror = 5
      return
      end
      subroutine shsgci1(nlat,nlon,l,late,wts,p0n,p1n,abel,bbel,cbel,
     1                  wfft,dtheta,dwts,work,ier)
      real wts(nlat),p0n(nlat,late),p1n(nlat,late),abel(1),bbel(1),
     1 cbel(1),wfft(1)
      double precision pb,dtheta(nlat),dwts(nlat),work(*)
c     compute the nlat  gaussian points and weights, the
c     m=0,1 legendre polys for gaussian points and all n,
c     and the legendre recursion coefficients
c     define index function used in storing
c     arrays for recursion coefficients (functions of (m,n))
c     the index function indx(m,n) is defined so that
c     the pairs (m,n) map to [1,2,...,indx(l-1,l-1)] with no
c     "holes" as m varies from 2 to n and n varies from 2 to l-1.
c     (m=0,1 are set from p0n,p1n for all n)
c     define for 2.le.n.le.l-1
      indx(m,n) = (n-1)*(n-2)/2+m-1
c     define index function for l.le.n.le.nlat
      imndx(m,n) = l*(l-1)/2+(n-l-1)*(l-1)+m-1
c     preset quantites for fourier transform
      call hrffti(nlon,wfft)
c     compute double precision gaussian points and weights
c     lw = 4*nlat*(nlat+1)+2
      lw = nlat*(nlat+2)
      call gaqd(nlat,dtheta,dwts,work(1),lw,ier)
      if (ier.ne.0) return
c     store gaussian weights single precision to save computation
c     in inner loops in analysis
      do 100 i=1,nlat
      wts(i) = dwts(i)
  100 continue
c     initialize p0n,p1n using double precision dnlfk,dnlft
      do 101 np1=1,nlat
      do 101 i=1,late
      p0n(np1,i) = 0.0
      p1n(np1,i) = 0.0
  101 continue
c     compute m=n=0 legendre polynomials for all theta(i)
      np1 = 1
      n = 0
      m = 0
      call dnlfk(m,n,work)
      do 103 i=1,late
      call dnlft(m,n,dtheta(i),work,pb)
      p0n(1,i) = pb
  103 continue
c     compute p0n,p1n for all theta(i) when n.gt.0
      do 104 np1=2,nlat
      n = np1-1
      m = 0
      call dnlfk(m,n,work)
      do 105 i=1,late
      call dnlft(m,n,dtheta(i),work,pb)
      p0n(np1,i) = pb
  105 continue
c     compute m=1 legendre polynomials for all n and theta(i)
      m = 1
      call dnlfk(m,n,work)
      do 106 i=1,late
      call dnlft(m,n,dtheta(i),work,pb)
      p1n(np1,i) = pb
  106 continue
  104 continue
c     compute and store swarztrauber recursion coefficients
c     for 2.le.m.le.n and 2.le.n.le.nlat in abel,bbel,cbel
      do 107 n=2,nlat
      mlim = min0(n,l)
      do 107 m=2,mlim
      imn = indx(m,n)
      if (n.ge.l) imn = imndx(m,n)
      abel(imn)=sqrt(float((2*n+1)*(m+n-2)*(m+n-3))/
     1               float(((2*n-3)*(m+n-1)*(m+n))))
      bbel(imn)=sqrt(float((2*n+1)*(n-m-1)*(n-m))/
     1               float(((2*n-3)*(m+n-1)*(m+n))))
      cbel(imn)=sqrt(float((n-m+1)*(n-m+2))/
     1               float(((n+m-1)*(n+m))))
  107 continue
      return
      end
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                       .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file islapec.f
c
c     this file includes documentation and code for
c     subroutine islapec         i
c
c ... files which must be loaded with islapec.f
c
c     sphcom.f, hrfft.f, shaec.f, shsec.f
c
c     subroutine islapec(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,
c    +mdab,ndab,wshsec,lshsec,work,lwork,pertrb,ierror)
c
c     islapec inverts the laplace or helmholz operator on an equally
c     spaced latitudinal grid using o(n**2) storage. given the
c     spherical harmonic coefficients a(m,n) and b(m,n) of the right
c     hand side slap(i,j), islapec computes a solution sf(i,j) to
c     the following helmhotz equation :
c
c           2                2
c     [d(sf(i,j))/dlambda /sint + d(sint*d(sf(i,j))/dtheta)/dtheta]/sint
c
c                   - xlmbda * sf(i,j) = slap(i,j)
c
c      where sf(i,j) is computed at colatitude
c
c                 theta(i) = (i-1)*pi/(nlat-1)
c
c            and longitude
c
c                 lambda(j) = (j-1)*2*pi/nlon
c
c            for i=1,...,nlat and j=1,...,nlon.
c
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct longitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c     isym   this parameter should have the same value input to subroutine
c            shaec to compute the coefficients a and b for the scalar field
c            slap.  isym is set as follows:
c
c            = 0  no symmetries exist in slap about the equator. scalar
c                 synthesis is used to compute sf on the entire sphere.
c                 i.e., in the array sf(i,j) for i=1,...,nlat and
c                 j=1,...,nlon.
c
c           = 1  sf and slap are antisymmetric about the equator. the
c                synthesis used to compute sf is performed on the
c                northern hemisphere only.  if nlat is odd, sf(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, sf(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c           = 2  sf and slap are symmetric about the equator. the
c                synthesis used to compute sf is performed on the
c                northern hemisphere only.  if nlat is odd, sf(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, sf(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c   nt       the number of solutions. in the program that calls islapec
c            the arrays sf,a, and b can be three dimensional in which
c            case multiple solutions are computed. the third index
c            is the solution index with values k=1,...,nt.
c            for a single solution set nt=1. the description of the
c            remaining parameters is simplified by assuming that nt=1
c            and sf,a,b are two dimensional.
c
c   xlmbda   a one dimensional array with nt elements. if xlmbda is
c            is identically zero islapec solves poisson's equation.
c            if xlmbda > 0.0 islapec solves the helmholtz equation.
c            if xlmbda < 0.0 the nonfatal error flag ierror=-1 is
c            returned. negative xlambda could result in a division
c            by zero.
c
c   ids      the first dimension of the array sf as it appears in the
c            program that calls islapec.  if isym = 0 then ids must be at
c            least nlat.  if isym > 0 and nlat is even then ids must be
c            at least nlat/2. if isym > 0 and nlat is odd then ids must
c            be at least (nlat+1)/2.
c
c   jds      the second dimension of the array sf as it appears in the
c            program that calls islapec. jds must be at least nlon.
c
c
c   a,b      two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the scalar field slap. a,b must be computed by shaec
c            prior to calling islapec.
c
c
c   mdab     the first dimension of the arrays a and b as it appears
c            in the program that calls islapec.  mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c   ndab     the second dimension of the arrays a and b as it appears
c            in the program that calls islapec. ndab must be at least
c            least nlat.
c
c            mdab,ndab should have the same values input to shaec to
c            compute the coefficients a and b.
c
c
c   wshsec   an array which must be initialized by subroutine shseci.
c            once initialized, wshsec can be used repeatedly by
c            islapec as long as nlat and nlon  remain unchanged.
c            wshsec must not be altered between calls of islapec.
c
c   lshsec   the dimension of the array wshsec as it appears in the
c            program that calls islapec.  let
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lsave must be greater than or equal to
c
c               2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls islapec. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            if isym = 0 let
c
c               lwkmin = nlat*(2*nt*nlon+max0(6*l2,nlon)+2*nt*l1+1).
c
c            if isym > 0 let
c
c               lwkmin = l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(2*nt*l1+1)
c
c
c     then lwork must be greater than or equal to lwkmin (see ierror=10)
c
c     **************************************************************
c
c     output parameters
c
c
c    sf      two or three dimensional arrays (see input parameter nt)
c            that contain the solution to either the helmholtz 
c            (xlmbda>0.0) or poisson's equation. sf(i,j) is computed
c            at colatitude
c
c                 theta(i) = (i-1)*pi/(nlat-1)
c
c            and longitude
c
c                 lambda(j) = (j-1)*2*pi/nlon
c
c            for i=1,...,nlat and j=1,...,nlon.
c
c   pertrb  a one dimensional array with nt elements (see input 
c           parameter nt). in the discription that follows we assume 
c           that nt=1. if xlmbda > 0.0 then pertrb=0.0 is always 
c           returned because the helmholtz operator is invertible.
c           if xlmbda = 0.0 then a solution exists only if a(1,1)
c           is zero. islapec sets a(1,1) to zero. the resulting
c           solution sf(i,j) solves poisson's equation with
c           pertrb = a(1,1)/(2.*sqrt(2.)) subtracted from the
c           right side slap(i,j).
c
c
c  ierror   a parameter which flags errors in input parameters as follows:
c
c            =-1  xlmbda is input negative (nonfatal error)
c
c            = 0  no errors detected
c
c            = 1  error in the specification of nlat
c
c            = 2  error in the specification of nlon
c
c            = 3  error in the specification of ityp
c
c            = 4  error in the specification of nt
c
c            = 5  error in the specification of ids
c
c            = 6  error in the specification of jds
c
c            = 7  error in the specification of mdbc
c
c            = 8  error in the specification of ndbc
c
c            = 9  error in the specification of lsave
c
c            = 10 error in the specification of lwork
c
c
c **********************************************************************
c                                                                              
c     end of documentation for islapec
c
c **********************************************************************
c


      subroutine islapec(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,
     +mdab,ndab,wshsec,lshsec,work,lwork,pertrb,ierror)
      real sf(ids,jds,nt),a(mdab,ndab,nt),b(mdab,ndab,nt)
      real wshsec(lshsec),work(lwork),pertrb(nt),xlmbda(nt)
c
c     check input parameters
c
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 4) return
      ierror = 3
      if (isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      imid = (nlat+1)/2
      if((isym.eq.0 .and. ids.lt.nlat) .or.
     1   (isym.gt.0 .and. ids.lt.imid)) return
      ierror = 6
      if(jds .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,nlon/2+1)
      if(mdab .lt. mmax) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
c
c     set and verify saved work space length
c
c
      l1 = min0(nlat,(nlon+2)/2)
      l2 = (nlat+1)/2
      lwmin = 2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
      if(lshsec .lt. lwmin) return
      ierror = 10
c
c     set and verify unsaved work space length
c
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
      mn = mmax*nlat*nt
c     lwmin = nln+ls*nlon+2*mn+nlat
c     if (lwork .lt. lwmin) return
      l2 = (nlat+1)/2
      l1 = min0(nlat,nlon/2+1)
      if (isym .eq. 0) then
	lwkmin = nlat*(2*nt*nlon+max0(6*l2,nlon)+2*nt*l1+1)
      else
	lwkmin = l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(2*nt*l1+1)
      end if
      if (lwork .lt. lwkmin) return
      ierror = 0
c
c     check sign of xlmbda
c
      do  k=1,nt
	if (xlmbda(k).lt.0.0) then
	  ierror = -1
	end if
      end do
c
c     set work space pointers
c
      ia = 1
      ib = ia+mn
      ifn = ib+mn
      iwk = ifn+nlat
      lwk = lwork-2*mn-nlat
      call islpec1(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,mdab,ndab,
     +work(ia),work(ib),mmax,work(ifn),wshsec,lshsec,work(iwk),lwk,
     +pertrb,ierror)
      return
      end

      subroutine islpec1(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,
     +mdab,ndab,as,bs,mmax,fnn,wshsec,lshsec,wk,lwk,pertrb,ierror)
      real sf(ids,jds,nt),a(mdab,ndab,nt),b(mdab,ndab,nt)
      real as(mmax,nlat,nt),bs(mmax,nlat,nt),fnn(nlat)
      real wshsec(lshsec),wk(lwk),pertrb(nt),xlmbda(nt)
c
c     set multipliers and preset synthesis coefficients to zero
c
      do n=1,nlat
	fn = float(n-1)
	fnn(n) = fn*(fn+1.0)
	do m=1,mmax
	  do k=1,nt
	    as(m,n,k) = 0.0
	    bs(m,n,k) = 0.0
	  end do
	end do
      end do

      do k=1,nt
c
c     compute synthesis coefficients for xlmbda zero or nonzero
c
	if (xlmbda(k) .eq. 0.0) then
	  do n=2,nlat
	    as(1,n,k) = -a(1,n,k)/fnn(n)
	    bs(1,n,k) = -b(1,n,k)/fnn(n)
	  end do
	  do m=2,mmax
	    do n=m,nlat
	      as(m,n,k) = -a(m,n,k)/fnn(n)
	      bs(m,n,k) = -b(m,n,k)/fnn(n)
	    end do
	  end do
	else
c
c     xlmbda nonzero so operator invertible unless
c     -n*(n-1) = xlmbda(k) < 0.0  for some n
c
	  pertrb(k) = 0.0
	  do n=1,nlat
	    as(1,n,k) = -a(1,n,k)/(fnn(n)+xlmbda(k))
	    bs(1,n,k) = -b(1,n,k)/(fnn(n)+xlmbda(k))
	  end do
	  do m=2,mmax
	    do n=m,nlat
	      as(m,n,k) = -a(m,n,k)/(fnn(n)+xlmbda(k))
	      bs(m,n,k) = -b(m,n,k)/(fnn(n)+xlmbda(k))
	    end do
	  end do
	end if
      end do
c
c     synthesize as,bs into sf
c
      call shsec(nlat,nlon,isym,nt,sf,ids,jds,as,bs,mmax,nlat,
     +           wshsec,lshsec,wk,lwk,ierror)
      return
      end
c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file islapgc.f
c
c     this file includes documentation and code for
c     subroutine islapgc         i
c
c ... files which must be loaded with islapgc.f
c
c     sphcom.f, hrfft.f, shagc.f, shsgc.f
c
c     subroutine islapgc(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,
c    +mdab,ndab,wshsgc,lshsgc,work,lwork,pertrb,ierror)
c
c     islapgc inverts the laplace or helmholz operator on a Gaussian
c     grid using o(n**2) storage. given the
c     spherical harmonic coefficients a(m,n) and b(m,n) of the right
c     hand side slap(i,j), islapgc computes a solution sf(i,j) to
c     the following helmhotz equation :
c
c           2                2
c     [d(sf(i,j))/dlambda /sint + d(sint*d(sf(i,j))/dtheta)/dtheta]/sint
c
c                   - xlmbda * sf(i,j) = slap(i,j)
c
c      where sf(i,j) is computed at the Gaussian colatitude point theta(i)
c      (see nlat as an input argument) and longitude
c
c                 lambda(j) = (j-1)*2*pi/nlon
c
c            for i=1,...,nlat and j=1,...,nlon.
c
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
c            full sphere. these lie in the interval (0,pi) and are computed
c            in radians in theta(1) <...< theta(nlat) by subroutine gaqd.
c            if nlat is odd the equator will be included as the grid point
c            theta((nlat+1)/2).  if nlat is even the equator will be
c            excluded as a grid point and will lie half way between
c            theta(nlat/2) and theta(nlat/2+1). nlat must be at least 3.
c            note: on the half sphere, the number of grid points in the
c            colatitudinal direction is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct longitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c     isym   this parameter should have the same value input to subroutine
c            shagc to compute the coefficients a and b for the scalar field
c            slap.  isym is set as follows:
c
c            = 0  no symmetries exist in slap about the equator. scalar
c                 synthesis is used to compute sf on the entire sphere.
c                 i.e., in the array sf(i,j) for i=1,...,nlat and
c                 j=1,...,nlon.
c
c           = 1  sf and slap are antisymmetric about the equator. the
c                synthesis used to compute sf is performed on the
c                northern hemisphere only.  if nlat is odd, sf(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, sf(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c           = 2  sf and slap are symmetric about the equator. the
c                synthesis used to compute sf is performed on the
c                northern hemisphere only.  if nlat is odd, sf(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, sf(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c   nt       the number of solutions. in the program that calls islapgc
c            the arrays sf,a, and b can be three dimensional in which
c            case multiple solutions are computed. the third index
c            is the solution index with values k=1,...,nt.
c            for a single solution set nt=1. the description of the
c            remaining parameters is simplified by assuming that nt=1
c            and sf,a,b are two dimensional.
c
c   xlmbda   a one dimensional array with nt elements. if xlmbda is
c            is identically zero islapgc solves poisson's equation.
c            if xlmbda > 0.0 islapgc solves the helmholtz equation.
c            if xlmbda < 0.0 the nonfatal error flag ierror=-1 is
c            returned. negative xlambda could result in a division
c            by zero.
c
c   ids      the first dimension of the array sf as it appears in the
c            program that calls islapgc.  if isym = 0 then ids must be at
c            least nlat.  if isym > 0 and nlat is even then ids must be
c            at least nlat/2. if isym > 0 and nlat is odd then ids must
c            be at least (nlat+1)/2.
c
c   jds      the second dimension of the array sf as it appears in the
c            program that calls islapgc. jds must be at least nlon.
c
c
c   a,b      two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the scalar field slap. a,b must be computed by shagc
c            prior to calling islapgc.
c
c
c   mdab     the first dimension of the arrays a and b as it appears
c            in the program that calls islapgc.  mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c   ndab     the second dimension of the arrays a and b as it appears
c            in the program that calls islapgc. ndbc must be at least
c            least nlat.
c
c            mdab,ndab should have the same values input to shagc to
c            compute the coefficients a and b.
c
c     wshsgc an array which must be initialized by subroutine shsgci
c            once initialized, wshsgc can be used repeatedly by islapgc
c            as long as nlon and nlat remain unchanged.  wshsgc must
c            not be altered between calls of islapgc.
c
c
c    lshsgc  the dimension of the array wshsgc as it appears in the
c            program that calls islapgc.  let
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgc must be at least
c
c               nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls islapgc. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            if isym = 0 let
c
c               lwkmin = nlat*(2*nt*nlon+max0(6*l2,nlon)+2*nt*l1+1).
c
c            if isym > 0 let
c
c               lwkmin = l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(2*nt*l1+1)
c
c
c     then lwork must be greater than or equal to lwkmin (see ierror=10)
c
c     **************************************************************
c
c     output parameters
c
c
c    sf      a two or three dimensional arrays (see input parameter nt) that
c            inverts the scalar laplacian in slap.  sf(i,j) is given at
c            the colatitude
c
c                 theta(i) = (i-1)*pi/(nlat-1)
c
c            and longitude
c
c                 lambda(j) = (j-1)*2*pi/nlon
c
c            for i=1,...,nlat and j=1,...,nlon.
c
c   pertrb  a one dimensional array with nt elements (see input 
c           parameter nt). in the discription that follows we assume 
c           that nt=1. if xlmbda > 0.0 then pertrb=0.0 is always 
c           returned because the helmholtz operator is invertible.
c           if xlmbda = 0.0 then a solution exists only if a(1,1)
c           is zero. islapgc sets a(1,1) to zero. the resulting
c           solution sf(i,j) solves poisson's equation with
c           pertrb = a(1,1)/(2.*sqrt(2.)) subtracted from the
c           right side slap(i,j).
c
c  ierror    a parameter which flags errors in input parameters as follows:
c
c            = 0  no errors detected
c
c            = 1  error in the specification of nlat
c
c            = 2  error in the specification of nlon
c
c            = 3  error in the specification of ityp
c
c            = 4  error in the specification of nt
c
c            = 5  error in the specification of ids
c
c            = 6  error in the specification of jds
c
c            = 7  error in the specification of mdbc
c
c            = 8  error in the specification of ndbc
c
c            = 9  error in the specification of lshsgc
c
c            = 10 error in the specification of lwork
c
c
c **********************************************************************
c                                                                              
c     end of documentation for islapgc
c
c **********************************************************************
c
      subroutine islapgc(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,
     +mdab,ndab,wshsgc,lshsgc,work,lwork,pertrb,ierror)
      dimension sf(ids,jds,nt),a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension wshsgc(lshsgc),work(lwork),xlmbda(nt),pertrb(nt)
c
c     check input parameters
c
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 4) return
      ierror = 3
      if (isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      imid = (nlat+1)/2
      if((isym.eq.0 .and. ids.lt.nlat) .or.
     1   (isym.gt.0 .and. ids.lt.imid)) return
      ierror = 6
      if(jds .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,nlon/2+1)
      if(mdab .lt. mmax) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
c
c     set and verify saved work space length
c
c
      l1 = min0(nlat,(nlon+2)/2)
      l2 = (nlat+1)/2
      lwmin = nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
      if(lshsgc .lt. lwmin) return
      ierror = 10
c
c     set and verify unsaved work space length
c
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
      mn = mmax*nlat*nt
c     lwmin = nln+ls*nlon+2*mn+nlat
c     if (lwork .lt. lwmin) return
      if (isym .eq. 0) then
	lwmin = nlat*(2*nt*nlon+max0(6*l2,nlon)+2*l1*nt+1)
      else
	lwmin = l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(2*l1*nt+1)
      end if
      if (lwork .lt. lwmin) return
      ierror = 0
c
c     check sign of xlmbda
c
      do  k=1,nt
	if (xlmbda(k).lt.0.0) then
	  ierror = -1
	end if
      end do
c
c     set work space pointers
c
      ia = 1
      ib = ia+mn
      ifn = ib+mn
      iwk = ifn+nlat
      lwk = lwork-2*mn-nlat
      call islpgc1(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,mdab,ndab,
     +work(ia),work(ib),mmax,work(ifn),wshsgc,lshsgc,work(iwk),lwk,
     +pertrb,ierror)
      return
      end

      subroutine islpgc1(nlat,nlon,isym,nt,xlmbda,sf,ids,jds,a,b,
     +mdab,ndab,as,bs,mmax,fnn,wsav,lsav,wk,lwk,pertrb,ierror)
      dimension sf(ids,jds,nt),a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension as(mmax,nlat,nt),bs(mmax,nlat,nt),fnn(nlat)
      dimension wsav(lsav),wk(lwk),xlmbda(nt),pertrb(nt)
c
c     set multipliers and preset synthesis coefficients to zero
c
      do n=1,nlat
	fn = float(n-1)
	fnn(n) = fn*(fn+1.0)
	do m=1,mmax
	  do k=1,nt
	    as(m,n,k) = 0.0
	    bs(m,n,k) = 0.0
	  end do
	end do
      end do

      do k=1,nt
c
c     compute synthesis coefficients for xlmbda zero or nonzero
c
	if (xlmbda(k) .eq. 0.0) then
	  do n=2,nlat
	    as(1,n,k) = -a(1,n,k)/fnn(n)
	    bs(1,n,k) = -b(1,n,k)/fnn(n)
	  end do
	  do m=2,mmax
	    do n=m,nlat
	      as(m,n,k) = -a(m,n,k)/fnn(n)
	      bs(m,n,k) = -b(m,n,k)/fnn(n)
	    end do
	  end do
	else
c
c     xlmbda nonzero so operator invertible unless
c     -n*(n-1) = xlmbda(k) < 0.0  for some n
c
	  pertrb(k) = 0.0
	  do n=1,nlat
	    as(1,n,k) = -a(1,n,k)/(fnn(n)+xlmbda(k))
	    bs(1,n,k) = -b(1,n,k)/(fnn(n)+xlmbda(k))
	  end do
	  do m=2,mmax
	    do n=m,nlat
	      as(m,n,k) = -a(m,n,k)/(fnn(n)+xlmbda(k))
	      bs(m,n,k) = -b(m,n,k)/(fnn(n)+xlmbda(k))
	    end do
	  end do
	end if
      end do
c
c     synthesize as,bs into sf
c
      call shsgc(nlat,nlon,isym,nt,sf,ids,jds,as,bs,mmax,nlat,
     +           wsav,lsav,wk,lwk,ierror)
      return
      end
