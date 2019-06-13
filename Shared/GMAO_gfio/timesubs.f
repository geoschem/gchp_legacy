!  The following function was taken from the book "Numerical Recipes in 
!  FORTRAN, the art of scientific computing (2nd Ed.), by William H. Press,
!  Saul A. Teukolsky, William T. Vetterling, and Brian P. Flannery (Cambridge 
!  University Press, 1992). 

      FUNCTION julday(mm,id,iyyy)
      INTEGER julday,id,iyyy,mm,IGREG
      PARAMETER (IGREG=15+31*(10+12*1582))
      INTEGER ja,jm,jy
      jy=iyyy
      if (jy.eq.0) then
        print *, 'julday: there is no year zero'
        stop
      endif
      if (jy.lt.0) jy=jy+1
      if (mm.gt.2) then
        jm=mm+1
      else
        jy=jy-1
        jm=mm+13
      endif
      julday=int(365.25*jy)+int(30.6001*jm)+id+1720995
      if (id+31*(mm+12*iyyy).ge.IGREG) then
        ja=int(0.01*jy)
        julday=julday+2-ja+int(0.25*ja)
      endif
      return
      END

      SUBROUTINE CALDAT (JULIAN,MM,ID,IYYY)                        
C                                                                   
C   ROUTINE CONVERTS JULIAN DAY TO MONTH, DAY, & YEAR.               
C   THIS CODE IS LIFTED FROM THE BOOK:                                
C   W.H. PRESS ET AL., NUMERICAL RECIPES, CAMBRIDGE UNIV. PRESS, 1986.  
C   THE ONLY MODIFICATION IS THAT REAL ARITHMETIC IS DONE IN R*8.
C                                                                 
C   THE ROUTINE OUTPUTS THE MONTH, DAY, AND YEAR ON WHICH THE      
C   SPECIFIED JULIAN DAY STARTED AT NOON.                           
C                                                                     
C   TO CONVERT MODIFIED JULIAN DAY, CALL THIS ROUTINE WITH              
C     JULIAN = MJD + 2400001                              
C                                                          
      PARAMETER (IGREG=2299161)                             
      IF (JULIAN.GE.IGREG) THEN                              
         JALPHA=INT((DBLE(JULIAN-1867216)-0.25D0)/36524.25D0) 
         JA=JULIAN+1+JALPHA-INT(0.25D0*DBLE(JALPHA))           
      ELSE                                                      
         JA=JULIAN                                               
      ENDIF                                               
      JB=JA+1524                                           
      JC=INT(6680.D0+(DBLE(JB-2439870)-122.1D0)/365.25D0)   
      JD=365*JC+INT(0.25D0*DBLE(JC))                         
      JE=INT(DBLE(JB-JD)/30.6001D0)                           
      ID=JB-JD-INT(30.6001D0*DBLE(JE))                         
      MM=JE-1                                                   
      IF (MM.GT.12) MM=MM-12                                     
      IYYY=JC-4715                                                
      IF (MM.GT.2) IYYY=IYYY-1                                     
      IF (IYYY.LE.0) IYYY=IYYY-1                                    
      RETURN                                                         
      END                                                             

      integer function err ( outstring, iret, ec ) 
      character *(*) outstring
      integer ec, iret

      if (iret .EQ. 0) then
        err = iret
      else
        print *,"GFIO_",outstring
        iret = ec
        err = ec
      endif
 
      return
      end
