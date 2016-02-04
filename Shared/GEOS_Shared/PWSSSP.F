      SUBROUTINE PWSSSP (INTL,TS,TF,M,MBDCND,BDTS,BDTF,PS,PF,N,NBDCND,
     >                   BDPS,BDPF,ELMBDA,F,IDIMF,PERTRB,IERROR,W)
C***********************************************************************
C
C          VERSION  2  OCTOBER 1976  INCLUDING ERRATA  OCTOBER 1976
C
C         DOCUMENTATION FOR THIS PROGRAM IS GIVEN IN
C
C        EFFICIENT FORTRAN SUBPROGRAMS FOR THE SOLUTION OF
C            ELLIPTIC PARTIAL DIFFERENTIAL EQUATIONS
C
C                              BY
C
C          PAUL SWARZTRAUBER   AND  ROLAND SWEET
C
C             TECHNICAL NOTE TN/IA-109   JULY 1975
C
C       NATIONAL CENTER FOR ATMOSPHERIC RESEARCH  BOULDER,COLORADO 80307
C
C        WHICH IS SPONSORED BY THE NATIONAL SCIENCE FOUNDATION
C
C***********************************************************************
C
C
C
C     SUBROUTINE PWSSSP SOLVES A FINITE DIFFERENCE APPROXIMATION TO THE
C     HELMHOLTZ EQUATION IN SPHERICAL COORDINATES AND ON THE SURFACE OF
C     THE UNIT SPHERE (RADIUS OF 1)0
C
C          (1/SIN(THETA))(D/DTHETA)(SIN(THETA)(D/DTHETA)U)
C
C             + (1/SIN(THETA)**2)(D/DPHI)(D/DPHI)U
C
C             + LAMBDA*U = F(THETA,PHI)
C
C     WHERE THETA IS COLATITUDE AND PHI IS LONGITUDE.
C
C     THE ARGUMENTS ARE DEFINED AS0
C
C
C     * * * * * * * * * *     ON INPUT     * * * * * * * * * *
C
C     INTL
C       = 0  ON INITIAL ENTRY TO PWSSSP OR IF N,NBDCND,PS OR PF ARE
C            CHANGED  FROM A PREVIOUS CALL
C       = 1  IF PS,PF,N AND NBDCND ARE UNCHANGED FROM A PREVIOUS CALL
C
C       NOTE0  A CALL WITH INTL = 1 IS ABOUT 1 PERCENT FASTER THAN A
C              CALL WITH INTL = 0  .
C
C     TS,TF
C       THE RANGE OF THETA (COLATITUDE), I.E., TS .LE. THETA .LE. TF.
C       TS MUST BE LESS THAN TF.  TS AND TF ARE IN RADIANS.  A TS OF
C       ZERO CORRESPONDS TO THE NORTH POLE AND A TF OF PI CORRESPONDS TO
C       THE SOUTH POLE.
C
C     M
C       THE NUMBER OF PANELS INTO WHICH THE INTERVAL (TS,TF) IS
C       SUBDIVIDED.  HENCE, THERE WILL BE M+1 GRID POINTS IN THE
C       THETA-DIRECTION GIVEN BY THETA(I) = (I-1)DTHETA+TS FOR
C       I = 1,2,...,M+1, WHERE DTHETA = (TF-TS)/M IS THE PANEL WIDTH.
C
C     MBDCND
C       INDICATES THE TYPE OF BOUNDARY CONDITION AT THETA = TS AND
C       THETA = TF.
C
C       = 1  IF THE SOLUTION IS SPECIFIED AT THETA = TS AND THETA = TF.
C       = 2  IF THE SOLUTION IS SPECIFIED AT THETA = TS AND THE
C            DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA IS
C            SPECIFIED AT THETA = TF (SEE NOTE 2 BELOW).
C       = 3  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA IS
C            SPECIFIED AT THETA = TS AND THETA = TF (SEE NOTES 1,2
C            BELOW).
C       = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA IS
C            SPECIFIED AT THETA = TS (SEE NOTE 1 BELOW) AND THE
C            SOLUTION IS SPECIFIED AT THETA = TF.
C       = 5  IF THE SOLUTION IS UNSPECIFIED AT THETA = TS = 0 AND THE
C            SOLUTION IS SPECIFIED AT THETA = TF.
C       = 6  IF THE SOLUTION IS UNSPECIFIED AT THETA = TS = 0 AND THE
C            DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA IS
C            SPECIFIED AT THETA = TF (SEE NOTE 2 BELOW).
C       = 7  IF THE SOLUTION IS SPECIFIED AT THETA = TS AND THE
C            SOLUTION IS UNSPECIFIED AT THETA = TF = PI.
C       = 8  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA IS
C            SPECIFIED AT THETA = TS (SEE NOTE 1 BELOW) AND THE
C            SOLUTION IS UNSPECIFIED AT THETA = TF = PI.
C       = 9  IF THE SOLUTION IS UNSPECIFIED AT THETA = TS = 0 AND
C            THETA = TF = PI.
C
C       NOTES0  1.  IF TS = 0, DO NOT USE MBDCND = 3,4, OR 8, BUT
C                   INSTEAD USE MBDCND = 5,6, OR 9  .
C               2.  IF TF = PI, DO NOT USE MBDCND = 2,3, OR 6, BUT
C                   INSTEAD USE MBDCND = 7,8, OR 9  .
C
C     BDTS
C       A ONE-DIMENSIONAL ARRAY OF LENGTH N+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA AT
C       THETA = TS.  WHEN MBDCND = 3,4, OR 8,
C
C            BDTS(J) = (D/DTHETA)U(TS,PHI(J)), J = 1,2,...,N+1  .
C
C       WHEN MBDCND HAS ANY OTHER VALUE, BDTS IS A DUMMY VARIABLE.
C
C     BDTF
C       A ONE-DIMENSIONAL ARRAY OF LENGTH N+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO THETA AT
C       THETA = TF.  WHEN MBDCND = 2,3, OR 6,
C
C            BDTF(J) = (D/DTHETA)U(TF,PHI(J)), J = 1,2,...,N+1  .
C
C       WHEN MBDCND HAS ANY OTHER VALUE, BDTF IS A DUMMY VARIABLE.
C
C     PS,PF
C       THE RANGE OF PHI (LONGITUDE), I.E., PS .LE. PHI .LE. PF.  PS
C       MUST BE LESS THAN PF.  PS AND PF ARE IN RADIANS.  IF PS = 0 AND
C       PF = 2*PI, PERIODIC BOUNDARY CONDITIONS ARE USUALLY PRESCRIBED.
C
C     N
C       THE NUMBER OF PANELS INTO WHICH THE INTERVAL (PS,PF) IS
C       SUBDIVIDED.  HENCE, THERE WILL BE N+1 GRID POINTS IN THE
C       PHI-DIRECTION GIVEN BY PHI(J) = (J-1)DPHI+PS  FOR
C       J = 1,2,...,N+1, WHERE DPHI = (PF-PS)/N IS THE PANEL WIDTH.  N
C       MUST BE OF THE FORM (2**P)(3**Q)(5**R) WHERE P, Q, AND R ARE ANY
C       NON-NEGATIVE INTEGERS.  N MUST BE GREATER THAN 2  .
C
C     NBDCND
C       INDICATES THE TYPE OF BOUNDARY CONDITION AT PHI = PS AND
C       PHI = PF.
C
C       = 0  IF THE SOLUTION IS PERIODIC IN PHI, I.E.,
C            U(I,1) = U(I,N+1).
C       = 1  IF THE SOLUTION IS SPECIFIED AT PHI = PS AND PHI = PF
C            (SEE NOTE BELOW).
C       = 2  IF THE SOLUTION IS SPECIFIED AT PHI = PS (SEE NOTE BELOW)
C            AND THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO PHI IS
C            SPECIFIED AT PHI = PF.
C       = 3  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO PHI IS
C            SPECIFIED AT PHI = PS AND PHI = PF.
C       = 4  IF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO PHI IS
C            SPECIFIED AT PS AND THE SOLUTION IS SPECIFIED AT PHI = PF
C            (SEE NOTE BELOW).
C
C       NOTE0  NBDCND = 1,2, OR 4 CANNOT BE USED WITH
C              MBDCND = 5,6,7,8, OR 9 (THE FORMER INDICATES THAT THE
C                       SOLUTION IS SPECIFIED AT A POLE, THE LATTER
C                       INDICATES THAT THE SOLUTION IS UNSPECIFIED).
C                       USE INSTEAD
C              MBDCND = 1 OR 2  .
C
C     BDPS
C       A ONE-DIMENSIONAL ARRAY OF LENGTH M+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO PHI AT
C       PHI = PS.  WHEN NBDCND = 3 OR 4,
C
C            BDPS(I) = (D/DPHI)U(THETA(I),PS), I = 1,2,...,M+1  .
C
C       WHEN NBDCND HAS ANY OTHER VALUE, BDPS IS A DUMMY VARIABLE.
C
C     BDPF
C       A ONE-DIMENSIONAL ARRAY OF LENGTH M+1 THAT SPECIFIES THE VALUES
C       OF THE DERIVATIVE OF THE SOLUTION WITH RESPECT TO PHI AT
C       PHI = PF.  WHEN NBDCND = 2 OR 3,
C
C            BDPF(I) = (D/DPHI)U(THETA(I),PF), I = 1,2,...,M+1  .
C
C       WHEN NBDCND HAS ANY OTHER VALUE, BDPF IS A DUMMY VARIABLE.
C
C     ELMBDA
C       THE CONSTANT LAMBDA IN THE HELMHOLTZ EQUATION.  IF
C       LAMBDA .GT. 0, A SOLUTION MAY NOT EXIST.  HOWEVER, PWSSSP WILL
C       ATTEMPT TO FIND A SOLUTION.
C
C     F
C       A TWO-DIMENSIONAL ARRAY THAT SPECIFIES THE VALUE OF THE RIGHT
C       SIDE OF THE HELMHOLTZ EQUATION AND BOUNDARY VALUES (IF ANY).
C       FOR I = 2,3,...,M  AND  J = 2,3,...,N
C
C            F(I,J) = F(THETA(I),PHI(J)).
C
C       ON THE BOUNDARIES F IS DEFINED BY
C
C            MBDCND   F(1,J)            F(M+1,J)
C            ------   ------------      ------------
C
C              1      U(TS,PHI(J))      U(TF,PHI(J))
C              2      U(TS,PHI(J))      F(TF,PHI(J))
C              3      F(TS,PHI(J))      F(TF,PHI(J))
C              4      F(TS,PHI(J))      U(TF,PHI(J))
C              5      F(0,PS)           U(TF,PHI(J))   J = 1,2,...,N+1
C              6      F(0,PS)           F(TF,PHI(J))
C              7      U(TS,PHI(J))      F(PI,PS)
C              8      F(TS,PHI(J))      F(PI,PS)
C              9      F(0,PS)           F(PI,PS)
C
C            NBDCND   F(I,1)            F(I,N+1)
C            ------   --------------    --------------
C
C              0      F(THETA(I),PS)    F(THETA(I),PS)
C              1      U(THETA(I),PS)    U(THETA(I),PF)
C              2      U(THETA(I),PS)    F(THETA(I),PF)   I = 1,2,...,M+1
C              3      F(THETA(I),PS)    F(THETA(I),PF)
C              4      F(THETA(I),PS)    U(THETA(I),PF)
C
C       F MUST BE DIMENSIONED AT LEAST (M+1)*(N+1).
C
C       NOTE
C
C       IF THE TABLE CALLS FOR BOTH THE SOLUTION U AND THE RIGHT SIDE F
C       AT  A CORNER THEN THE SOLUTION MUST BE SPECIFIED.
C
C     IDIMF
C       THE ROW (OR FIRST) DIMENSION OF THE ARRAY F AS IT APPEARS IN THE
C       PROGRAM CALLING PWSSSP.  THIS PARAMETER IS USED TO SPECIFY THE
C       VARIABLE DIMENSION OF F.  IDIMF MUST BE AT LEAST M+1  .
C
C     W
C       A ONE-DIMENSIONAL ARRAY THAT MUST BE PROVIDED BY THE USER FOR
C       WORK SPACE.  THE LENGTH OF W MUST BE AT LEAST 11(M+1)+6(N+1).
C
C
C
C     F
C       CONTAINS THE SOLUTION U(I,J) OF THE FINITE DIFFERENCE
C       APPROXIMATION FOR THE GRID POINT (THETA(I),PHI(J)),
C       I = 1,2,...,M+1,   J = 1,2,...,N+1  .
C
C     PERTRB
C       IF ONE SPECIFIES A COMBINATION OF PERIODIC, DERIVATIVE OR
C       UNSPECIFIED BOUNDARY CONDITIONS FOR A POISSON EQUATION
C       (LAMBDA = 0), A SOLUTION MAY NOT EXIST.  PERTRB IS A CONSTANT,
C       CALCULATED AND SUBTRACTED FROM F, WHICH ENSURES THAT A SOLUTION
C       EXISTS.  PWSSSP THEN COMPUTES THIS SOLUTION, WHICH IS A LEAST
C       SQUARES SOLUTION TO THE ORIGINAL APPROXIMATION.  THIS SOLUTION
C       IS NOT UNIQUE AND IS UNNORMALIZED. THE VALUE OF PERTRB SHOULD
C       BE SMALL COMPARED TO THE RIGHT SIDE F. OTHERWISE , A SOLUTION
C       IS OBTAINED TO AN ESSENTIALLY DIFFERENT PROBLEM. THIS COMPARISON
C       SHOULD ALWAYS BE MADE TO INSURE THAT A MEANINGFUL SOLUTION HAS
C       BEEN OBTAINED
C
C     IERROR
C       AN ERROR FLAG THAT INDICATES INVALID INPUT PARAMETERS.  EXCEPT
C       FOR NUMBERS 0 AND 8, A SOLUTION IS NOT ATTEMPTED.
C
C       = 0  NO ERROR.
C       = 1  TS .LT. 0  OR  TF .GT. PI
C       = 2  TS .GE. TF.
C       = 3  MBDCND .LT. 1 OR MBDCND .GT. 9  .
C       = 4  PS .GE. PF.
C       = 5  N IS NOT OF THE FORM (2**P)(3**Q)(5**R) OR N .LE. 2  .
C       = 6  NBDCND .LT. 0 OR NBDCND .GT. 4  .
C       = 7  AN NBDCND OF 1,2, OR 4 IS USED WITH AN
C            MBDCND OF 5,6,7,8, OR 9  .
C       = 8  ELMBDA .GT. 0  .
C       = 9  IDIMF .LT. M+1  .
C       = 10 TS=0 AND MBDCND=3,4 OR 8  OR  TF=PI AND MBDCND=2,3 OR 6
C
C       SINCE THIS IS THE ONLY MEANS OF INDICATING A POSSIBLY INCORRECT
C       CALL TO PWSSSP, THE USER SHOULD TEST IERROR AFTER A CALL.
C
C     W
C       CONTAINS INTERMEDIATE VALUES THAT MUST NOT BE DESTROYED IF
C       PWSSSP WILL BE CALLED AGAIN WITH INTL = 1  .
C
C
C
      REAL       F(IDIMF,1) ,BDTS(1)    ,BDTF(1)    ,BDPS(1)    ,
     1                BDPF(1)    ,W(1)
      NBR = NBDCND+1
      PI = 3.14159265358979
      IF(INTL.EQ.0) EPS = APXEPS(DUM)
                    eps = 1e-15
      IERROR = 9
      IF (IDIMF-M) 119,119,101
  101 IERROR = 1
      IF (TS) 119,102,102
  102 IF (PI-TF+EPS) 119,119,103
  103 IERROR = 2
      IF (TF-TS) 119,119,104
  104 IERROR = 3
      IF (MBDCND) 119,119,105
  105 IF (MBDCND-9) 106,106,119
  106 IERROR = 4
      IF (PF-PS) 119,119,107
  107 IERROR = 5
CCCCC IF (NCHECK(N)-2) 108,119,108
  108 IERROR = 6
      IF (NBDCND) 119,109,109
  109 IF (NBDCND-4) 110,110,119
  110 IERROR = 7
      IF (MBDCND-4) 112,112,111
  111 GO TO (112,119,119,112,119),NBR
  112 IERROR = 8
      IF (ELMBDA) 113,113,118
  113 IERROR = 10
      IF (TS) 114,114,115
  114 GO TO (115,115,118,118,115,115,115,118,115),MBDCND
  115 IF (PI-TF-EPS) 116,116,117
  116 GO TO (117,118,118,117,117,118,117,117,117),MBDCND
  117 IERROR = 0
  118 MP1 = M+1
      IW1 = 6*(N+1)+5*MP1+1
      IW2 = IW1+MP1
      IW3 = IW2+MP1
      IW4 = IW3+MP1
      IW5 = IW4+MP1
      IW6 = IW5+MP1

      CALL PWSSS1 (INTL,TS,TF,M,MBDCND,BDTS,BDTF,PS,PF,N,NBDCND,BDPS,
     .             BDPF,ELMBDA,F,IDIMF,PERTRB,W(IW1),W(IW2),W(IW3),
     .             W(IW4),W(IW5),W(IW6),W)
  119 RETURN
      END

      SUBROUTINE PWSSS1 (INTL,TS,TF,M,MBDCND,BDTS,BDTF,PS,PF,N,NBDCND,
     1                   BDPS,BDPF,ELMBDA,F,IDIMF,PERTRB,AM,BM,CM,SN,
     2                   SS,SINT,D)
      REAL       F(IDIMF,1) ,BDTS(*)    ,BDTF(*)    ,BDPS(*)    ,
     1                BDPF(*)    ,AM(*)      ,BM(*)      ,CM(*)      ,
     2                SS(*)      ,SN(*)      ,D(*)       ,SINT(*)
C
C     MACHINE DEPENDENT CONSTANT
C
C     PI=3.1415926535897932384626433832795028841971693993751058209749446
C
      PI = 3.14159265358979
      TPI = PI+PI
      HPI = PI/2.
      MP1 = M+1
      NP1 = N+1
      FN = N
      FM = M
      DTH = (TF-TS)/FM
      HDTH = DTH/2.
      TDT = DTH+DTH
      DPHI = (PF-PS)/FN
      TDP = DPHI+DPHI
      DPHI2 = DPHI*DPHI
      EDP2 = ELMBDA*DPHI2
      DTH2 = DTH*DTH
      CP = 4./(FN*DTH2)
      WP = FN*SIN(HDTH)/4.
      DO 102 I=1,MP1
         FIM1 = I-1
         THETA = FIM1*DTH+TS
         SINT(I) = SIN(THETA)
         IF (SINT(I)) 101,102,101
  101    T1 = 1./(DTH2*SINT(I))
         AM(I) = T1*SIN(THETA-HDTH)
         CM(I) = T1*SIN(THETA+HDTH)
         BM(I) = -AM(I)-CM(I)+ELMBDA
  102 CONTINUE
      INP = 0
      ISP = 0
C
C BOUNDARY CONDITION AT THETA=TS
C
      MBR = MBDCND+1
      GO TO (103,104,104,105,105,106,106,104,105,106),MBR
  103 ITS = 1
      GO TO 107
  104 AT = AM(2)
      ITS = 2
      GO TO 107
  105 AT = AM(1)
      ITS = 1
      CM(1) = AM(1)+CM(1)
      GO TO 107
  106 AT = AM(2)
      INP = 1
      ITS = 2
C
C BOUNDARY CONDITION THETA=TF
C
  107 GO TO (108,109,110,110,109,109,110,111,111,111),MBR
  108 ITF = M
      GO TO 112
  109 CT = CM(M)
      ITF = M
      GO TO 112
  110 CT = CM(M+1)
      AM(M+1) = AM(M+1)+CM(M+1)
      ITF = M+1
      GO TO 112
  111 ITF = M
      ISP = 1
      CT = CM(M)
C
C COMPUTE HOMOGENEOUS SOLUTION WITH SOLUTION AT POLE EQUAL TO ONE
C
  112 ITSP = ITS+1
      ITFM = ITF-1
      WTS = SINT(ITS+1)*AM(ITS+1)/CM(ITS)
      WTF = SINT(ITF-1)*CM(ITF-1)/AM(ITF)
      MUNK = ITF-ITS+1
      IF (ISP) 116,116,113
  113 D(ITS) = CM(ITS)/BM(ITS)
      DO 114 I=ITSP,M
         D(I) = CM(I)/(BM(I)-AM(I)*D(I-1))
  114 CONTINUE
      SS(M) = -D(M)
      IID = M-ITS
      DO 115 II=1,IID
         I = M-II
         SS(I) = -D(I)*SS(I+1)
  115 CONTINUE
      SS(M+1) = 1.
  116 IF (INP) 120,120,117
  117 SN(1) = 1.
      D(ITF) = AM(ITF)/BM(ITF)
      IID = ITF-2
      DO 118 II=1,IID
         I = ITF-II
         D(I) = AM(I)/(BM(I)-CM(I)*D(I+1))
  118 CONTINUE
      SN(2) = -D(2)
      DO 119 I=3,ITF
         SN(I) = -D(I)*SN(I-1)
  119 CONTINUE
C
C BOUNDARY CONDITIONS AT PHI=PS
C
  120 NBR = NBDCND+1
      WPS = 1.
      WPF = 1.
      GO TO (121,122,122,123,123),NBR
  121 JPS = 1
      GO TO 124
  122 JPS = 2
      GO TO 124
  123 JPS = 1
      WPS = .5
C
C BOUNDARY CONDITION AT PHI=PF
C
  124 GO TO (125,126,127,127,126),NBR
  125 JPF = N
      GO TO 128
  126 JPF = N
      GO TO 128
  127 WPF = .5
      JPF = N+1
  128 JPSP = JPS+1
      JPFM = JPF-1
      NUNK = JPF-JPS+1
      FJJ = JPFM-JPSP+1
C
C SCALE COEFFICIENTS FOR SUBROUTINE POIS
C
      DO 129 I=ITS,ITF
         CF = DPHI2*SINT(I)*SINT(I)
         AM(I) = CF*AM(I)
         BM(I) = CF*BM(I)
         CM(I) = CF*CM(I)
  129 CONTINUE
      ISING = 0
      GO TO (130,138,138,130,138,138,130,138,130,130),MBR
  130 GO TO (131,138,138,131,138),NBR
  131 IF (ELMBDA) 138,132,132
  132 ISING = 1
      SUM = WTS*WPS+WTS*WPF+WTF*WPS+WTF*WPF
      IF (INP) 134,134,133
  133 SUM = SUM+WP
  134 IF (ISP) 136,136,135
  135 SUM = SUM+WP
  136 SUM1 = 0.
      DO 137 I=ITSP,ITFM
         SUM1 = SUM1+SINT(I)
  137 CONTINUE
      SUM = SUM+FJJ*(SUM1+WTS+WTF)
      SUM = SUM+(WPS+WPF)*SUM1
      HNE = SUM
  138 GO TO (146,142,142,144,144,139,139,142,144,139),MBR
  139 IF (NBDCND-3) 146,140,146
  140 YHLD = F(1,JPS)-4./(FN*DPHI*DTH2)*(BDPF(2)-BDPS(2))
      DO 141 J=1,NP1
         F(1,J) = YHLD
  141 CONTINUE
      GO TO 146
  142 DO 143 J=JPS,JPF
         F(2,J) = F(2,J)-AT*F(1,J)
  143 CONTINUE
      GO TO 146
  144 DO 145 J=JPS,JPF
         F(1,J) = F(1,J)+TDT*BDTS(J)*AT
  145 CONTINUE
  146 GO TO (154,150,152,152,150,150,152,147,147,147),MBR
  147 IF (NBDCND-3) 154,148,154
  148 YHLD = F(M+1,JPS)-4./(FN*DPHI*DTH2)*(BDPF(M)-BDPS(M))
      DO 149 J=1,NP1
         F(M+1,J) = YHLD
  149 CONTINUE
      GO TO 154
  150 DO 151 J=JPS,JPF
         F(M,J) = F(M,J)-CT*F(M+1,J)
  151 CONTINUE
      GO TO 154
  152 DO 153 J=JPS,JPF
         F(M+1,J) = F(M+1,J)-TDT*BDTF(J)*CT
  153 CONTINUE
  154 GO TO (159,155,155,157,157),NBR
  155 DO 156 I=ITS,ITF
         F(I,2) = F(I,2)-F(I,1)/(DPHI2*SINT(I)*SINT(I))
  156 CONTINUE
      GO TO 159
  157 DO 158 I=ITS,ITF
         F(I,1) = F(I,1)+TDP*BDPS(I)/(DPHI2*SINT(I)*SINT(I))
  158 CONTINUE
  159 GO TO (164,160,162,162,160),NBR
  160 DO 161 I=ITS,ITF
         F(I,N) = F(I,N)-F(I,N+1)/(DPHI2*SINT(I)*SINT(I))
  161 CONTINUE
      GO TO 164
  162 DO 163 I=ITS,ITF
         F(I,N+1) = F(I,N+1)-TDP*BDPF(I)/(DPHI2*SINT(I)*SINT(I))
  163 CONTINUE
  164 CONTINUE
      PERTRB = 0.
      IF (ISING) 165,176,165
  165 continue
      SUM = WTS*WPS*F(ITS,JPS)+WTS*WPF*F(ITS,JPF)+WTF*WPS*F(ITF,JPS)+
     1      WTF*WPF*F(ITF,JPF)
      IF (INP) 167,167,166
  166 SUM = SUM+WP*F(1,JPS)
  167 IF (ISP) 169,169,168
  168 SUM = SUM+WP*F(M+1,JPS)
  169 DO 171 I=ITSP,ITFM
         SUM1 = 0.
         DO 170 J=JPSP,JPFM
            SUM1 = SUM1+F(I,J)
  170    CONTINUE
         SUM = SUM+SINT(I)*SUM1
  171 CONTINUE
      SUM1 = 0.
      SUM2 = 0.
      DO 172 J=JPSP,JPFM
         SUM1 = SUM1+F(ITS,J)
         SUM2 = SUM2+F(ITF,J)
  172 CONTINUE
      SUM = SUM+WTS*SUM1+WTF*SUM2
      SUM1 = 0.
      SUM2 = 0.
      DO 173 I=ITSP,ITFM
         SUM1 = SUM1+SINT(I)*F(I,JPS)
         SUM2 = SUM2+SINT(I)*F(I,JPF)
  173 CONTINUE
      SUM = SUM+WPS*SUM1+WPF*SUM2
      PERTRB = SUM/HNE
      DO 175 J=1,NP1
         DO 174 I=1,MP1
            F(I,J) = F(I,J)-PERTRB
  174    CONTINUE
  175 CONTINUE
C
C SCALE RIGHT SIDE FOR SUBROUTINE POIS
C
  176 DO 178 I=ITS,ITF
         CF = DPHI2*SINT(I)*SINT(I)
         DO 177 J=JPS,JPF
            F(I,J) = CF*F(I,J)
  177    CONTINUE
  178 CONTINUE
      NBDC = NBDCND
      IF (NBDCND-4) 182,179,182
  179 JFN = NUNK/2
      DO 181 J=1,JFN
         JFRD = J+JPS-1
         JBRD = JPF-J+1
         DO 180 I=ITS,ITF
            HLD = F(I,JFRD)
            F(I,JFRD) = F(I,JBRD)
            F(I,JBRD) = HLD
  180    CONTINUE
  181 CONTINUE
      NBDC = 2
  182 CALL POIS (INTL,NBDC,NUNK,MBDCND,MUNK,AM(ITS),BM(ITS),CM(ITS),
     1           IDIMF,F(ITS,JPS),IERROR,D)
      IF (NBDCND-4) 186,183,186
  183 DO 185 J=1,JFN
         JFRD = J+JPS-1
         JBRD = JPF-J+1
         DO 184 I=ITS,ITF
            HLD = F(I,JFRD)
            F(I,JFRD) = F(I,JBRD)
            F(I,JBRD) = HLD
  184    CONTINUE
  185 CONTINUE
  186 IF (ISING) 194,194,187
  187 IF (INP) 191,191,188
  188 IF (ISP) 189,189,194
  189 DO 190 J=1,NP1
         F(1,J) = 0.
  190 CONTINUE
      GO TO 217
  191 IF (ISP) 194,194,192
  192 DO 193 J=1,NP1
         F(M+1,J) = 0.
  193 CONTINUE
      GO TO 217
  194 IF (INP) 201,201,195
  195 SUM = WPS*F(ITS,JPS)+WPF*F(ITS,JPF)
      DO 196 J=JPSP,JPFM
         SUM = SUM+F(ITS,J)
  196 CONTINUE
      DFN = CP*SUM
      DNN = CP*((WPS+WPF+FJJ)*(SN(2)-1.))+ELMBDA
      DSN = CP*(WPS+WPF+FJJ)*SN(M)
      IF (ISP) 197,197,202
  197 CNP = (F(1,1)-DFN)/DNN
      DO 199 I=ITS,ITF
         HLD = CNP*SN(I)
         DO 198 J=JPS,JPF
            F(I,J) = F(I,J)+HLD
  198    CONTINUE
  199 CONTINUE
      DO 200 J=1,NP1
         F(1,J) = CNP
  200 CONTINUE
      GO TO 217
  201 IF (ISP) 217,217,202
  202 SUM = WPS*F(ITF,JPS)+WPF*F(ITF,JPF)
      DO 203 J=JPSP,JPFM
         SUM = SUM+F(ITF,J)
  203 CONTINUE
      DFS = CP*SUM
      DSS = CP*((WPS+WPF+FJJ)*(SS(M)-1.))+ELMBDA
      DNS = CP*(WPS+WPF+FJJ)*SS(2)
      IF (INP) 204,204,208
  204 CSP = (F(M+1,1)-DFS)/DSS
      DO 206 I=ITS,ITF
         HLD = CSP*SS(I)
         DO 205 J=JPS,JPF
            F(I,J) = F(I,J)+HLD
  205    CONTINUE
  206 CONTINUE
      DO 207 J=1,NP1
         F(M+1,J) = CSP
  207 CONTINUE
      GO TO 217
  208 RTN = F(1,1)-DFN
      RTS = F(M+1,1)-DFS
      IF (ISING) 210,210,209
  209 CSP = 0.
      CNP = RTN/DNN
      GO TO 213
  210 IF (ABS(DNN)-ABS(DSN)) 212,212,211
  211 DEN = DSS-DNS*DSN/DNN
      RTS = RTS-RTN*DSN/DNN
      CSP = RTS/DEN
      CNP = (RTN-CSP*DNS)/DNN
      GO TO 213
  212 DEN = DNS-DSS*DNN/DSN
      RTN = RTN-RTS*DNN/DSN
      CSP = RTN/DEN
      CNP = (RTS-DSS*CSP)/DSN
  213 DO 215 I=ITS,ITF
         HLD = CNP*SN(I)+CSP*SS(I)
         DO 214 J=JPS,JPF
            F(I,J) = F(I,J)+HLD
  214    CONTINUE
  215 CONTINUE
      DO 216 J=1,NP1
         F(1,J) = CNP
         F(M+1,J) = CSP
  216 CONTINUE
  217 IF (NBDCND) 220,218,220
  218 DO 219 I=1,MP1
         F(I,JPF+1) = F(I,JPS)
  219 CONTINUE
  220 RETURN
      END
C     LAST UPDATE SEPT 16 1981    RAMESH C.BALGOVIND
C POIS       FROM PORTLIB             VERSION   5          10/20/77
      SUBROUTINE POIS (IFLG,NPEROD,N,MPEROD,M,A,B,C,IDIMY,Y,IERROR,W)
C
C
C***********************************************************************
C
C          VERSION  2  OCTOBER 1976  INCLUDING ERRATA  OCTOBER 1976
C
C         DOCUMENTATION FOR THIS PROGRAM IS GIVEN IN
C
C        EFFICIENT FORTRAN SUBPROGRAMS FOR THE SOLUTION OF
C            ELLIPTIC PARTIAL DIFFERENTIAL EQUATIONS
C
C                              BY
C
C          PAUL SWARZTRAUBER   AND  ROLAND SWEET
C
C             TECHNICAL NOTE TN/IA-109   JULY 1975
C
C       NATIONAL CENTER FOR ATMOSPHERIC RESEARCH  BOULDER,COLORADO 80307
C
C        WHICH IS SPONSORED BY THE NATIONAL SCIENCE FOUNDATION
C
C***********************************************************************
C
C
C
C     SUBROUTINE POIS SOLVES THE LINEAR SYSTEM OF EQUATIONS
C
C          A(I)*X(I-1,J) + B(I)*X(I,J) + C(I)*X(I+1,J)
C
C          + X(I,J-1) - 2.*X(I,J) + X(I,J+1) = Y(I,J)
C
C               FOR I = 1,2,...,M  AND  J = 1,2,...,N.
C
C     THE INDICES I+1 AND I-1 ARE EVALUATED MODULO M, I.E.,
C     X(0,J) = X(M,J) AND X(M+1,J) = X(1,J), AND X(I,0) MAY BE EQUAL TO
C     0, X(I,2), OR X(I,N) AND X(I,N+1) MAY BE EQUAL TO 0, X(I,N-1), OR
C     X(I,1) DEPENDING ON AN INPUT PARAMETER.
C
C
C     * * * * * * * * * *     ON INPUT     * * * * * * * * * *
C
C     IFLG
C       = 0  ON INITIAL ENTRY TO POIS OR IF N AND NPEROD ARE CHANGED
C            FROM PREVIOUS CALL.
C       = 1  IF N AND NPEROD ARE UNCHANGED FROM PREVIOUS CALL TO POIS.
C
C       NOTE0  A CALL WITH IFLG = 1 IS ABOUT 1 PERCENT FASTER THAN A
C              CALL WITH IFLG = 0  .
C
C     NPEROD
C       INDICATES THE VALUES WHICH X(I,0) AND X(I,N+1) ARE ASSUMED TO
C       HAVE.
C
C       = 0  IF X(I,0) = X(I,N) AND X(I,N+1) = X(I,1).
C       = 1  IF X(I,0) = X(I,N+1) = 0  .
C       = 2  IF X(I,0) = 0 AND X(I,N+1) = X(I,N-1).
C       = 3  IF X(I,0) = X(I,2) AND X(I,N+1) = X(I,N-1).
C
C     N
C       THE NUMBER OF UNKNOWNS IN THE J-DIRECTION.  N IS DEPENDENT ON
C       NPEROD AND MUST HAVE THE FOLLOWING FORM0
C
C            NPEROD
C              = 0 OR 2, THEN N = (2**P)(3**Q)(5**R)
C              = 1, THEN N = (2**P)(3**Q)(5**R)-1
C              = 3, THEN N = (2**P)(3**Q)(5**R)+1
C
C            WHERE P, Q, AND R MAY BE ANY NON-NEGATIVE INTEGERS.  N MUST
C            BE GREATER THAN 2  .
C
C     MPEROD
C       = 0 IF A(1) AND C(M) ARE NOT ZERO
C       = 1 IF A(1) = C(M) = 0
C
C     M
C       THE NUMBER OF UNKNOWNS IN THE I-DIRECTION.  M MAY BE ANY INTEGER
C       GREATER THAN 1  .
C
C     A,B,C
C       ONE-DIMENSIONAL ARRAYS OF LENGTH M WHICH SPECIFY THE
C       COEFFICIENTS IN THE LINEAR EQUATIONS GIVEN ABOVE.
C
C     IDIMY
C       THE ROW (OR FIRST) DIMENSION OF THE TWO-DIMENSIONAL ARRAY Y AS
C       IT APPEARS IN THE PROGRAM CALLING POIS.  THIS PARAMETER IS USED
C       TO SPECIFY THE VARIABLE DIMENSION OF Y.  IDIMY MUST BE AT LEAST
C       M.
C
C     Y
C       A TWO-DIMENSIONAL ARRAY WHICH SPECIFIES THE VALUES OF THE RIGHT
C       SIDE OF THE LINEAR SYSTEM OF EQUATIONS GIVEN ABOVE.  Y MUST BE
C       DIMENSIONED AT LEAST M*N.
C
C     W
C       A ONE-DIMENSIONAL ARRAY-WHICH MUST BE PROVIDED BY THE USER FOR
C       WORK SPACE.  THE LENGTH OF W MUST BE AT LEAST 6N+5M.
C
C
C     * * * * * * * * * *     ON OUTPUT     * * * * * * * * * *
C
C     Y
C       CONTAINS THE SOLUTION X.
C
C     IERROR
C       AN ERROR FLAG WHICH INDICATES INVALID INPUT PARAMETERS  EXCEPT
C       FOR NUMBER ZERO, A SOLUTION IS NOT ATTEMPTED.
C
C       = 0  NO ERROR.
C       = 1  M .LE. 2  .
C       = 2  N .LE. 2 OR NOT OF THE RIGHT FORM.
C       = 3  IDIMY .LT. M.
C       = 4  NPEROD .LT. 0 OR NPEROD .GT. 3  .
C
C     W
C       CONTAINS INTERMEDIATE VALUES THAT MUST NOT BE DESTROYED IF
C       POIS WILL BE CALLED AGAIN WITH IFLAG = 1  .
C
C
C
      EXTERNAL        TRIDD       ,TRIDP
      REAL       Y(IDIMY,1)
      REAL       W(1)       ,B(1)       ,A(1)       ,C(1)
      IERROR = 0
      IF (M .LE. 2) IERROR = 1
C     I = NCHECK(NPEROD-2*((2*NPEROD)/3)+N)
C     IF (N.LE.2 .OR. I.GT.1) IERROR = 2
      IF (IDIMY .LT. M) IERROR = 3
      IF (NPEROD.LT.0 .OR. NPEROD.GT.3) IERROR = 4
      IF (IERROR .NE. 0) GO TO 105
      IWDIM1 = 6*N+1
      IWDIM2 = IWDIM1+M
      IWDIM3 = IWDIM2+M
      IWDIM4 = IWDIM3+M
      IWDIM5 = IWDIM4+M
      DO 101 I=1,M
         A(I) = -A(I)
         C(I) = -C(I)
         K = IWDIM5+I-1
         W(K) = -B(I)+2.
  101 CONTINUE
      IF (MPEROD .EQ. 0) GO TO 102
      CALL POISGN (NPEROD,N,IFLG,M,A,W(IWDIM5),C,IDIMY,Y,W(1),
     1             W(IWDIM1),W(IWDIM2),W(IWDIM3),W(IWDIM4),TRIDD)
      GO TO 103
  102 CALL POISGN (NPEROD,N,IFLG,M,A,W(IWDIM5),C,IDIMY,Y,W(1),
     1             W(IWDIM1),W(IWDIM2),W(IWDIM3),W(IWDIM4),TRIDP)
  103 DO 104 I=1,M
         A(I) = -A(I)
         C(I) = -C(I)
  104 CONTINUE
  105 RETURN
      END
      SUBROUTINE POISGN (NPEROD,N,IFLG,M,BA,BB,BC,IDIMQ,Q,TWOCOS,B,T,D,
     1                   W,TRI)
      REAL       Q(IDIMQ,1)
      REAL       BA(1)      ,BB(1)      ,BC(1)      ,TWOCOS(1)  ,
     1                B(1)       ,T(1)       ,D(1)       ,W(1)
      EXTERNAL   TRI

      IF (IFLG .NE. 0) GO TO 101
C
C     DO INITIALIZATION IF FIRST TIME THROUGH.
C
      CALL POINIT (NPEROD,N,IEX2,IEX3,IEX5,N2PW,N2P3P,N2P3P5,KS3,KS5,
     1             NPSTOP,TWOCOS)
  101 MM1 = M-1
      DO 104 J=1,N
         DO 102 I=1,M
            B(I) = -Q(I,J)
  102    CONTINUE
         CALL TRI (1,1,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
         DO 103 I=1,M
            Q(I,J) = B(I)
  103    CONTINUE
  104 CONTINUE
      NP = 1
C
C     START REDUCTION FOR POWERS OF TWO
C
      IF (IEX2 .EQ. 0) GO TO 112
      L = IEX2
      IF (IEX3+IEX5 .NE. 0) GO TO 105
      IF (NPEROD .EQ. 1) L = L-1
      IF (L .EQ. 0) GO TO 138
  105 DO 111 KOUNT=1,L
         K = NP
         NP = 2*NP
         K2 = NP-1
         K3 = NP
         K4 = 2*NP-1
         JSTART = NP
         IF (NPEROD .EQ. 3) JSTART = 1
         DO 110 J=JSTART,N,NP
            JM1 = J-K
            JP1 = J+K
            IF (J .EQ. 1) JM1 = JP1
            IF (J .NE. N) GO TO 106
            JP1 = JM1
            IF (NPEROD .EQ. 0) JP1 = K
  106       DO 107 I=1,M
               B(I) = Q(I,JM1)+Q(I,JP1)
  107       CONTINUE
            CALL TRI (K,K2,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 108 I=1,M
               T(I) = Q(I,J)+B(I)
               B(I) = T(I)
  108       CONTINUE
            CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 109 I=1,M
               Q(I,J) = T(I)+2.*B(I)
  109       CONTINUE
  110    CONTINUE
  111 CONTINUE
C
C     START REDUCTION FOR POWERS OF THREE
C
  112 IF (IEX3 .EQ. 0) GO TO 124
      L = IEX3
      IF (IEX5 .NE. 0) GO TO 113
      IF (NPEROD .EQ. 1) L = L-1
      IF (L .EQ. 0) GO TO 138
  113 K2 = NP-1
      DO 123 KOUNT=1,L
         K = NP
         NP = 3*NP
         K1 = K2+1
         K2 = K2+K
         K3 = K2+1
         K4 = K2+NP
         JSTART = NP
         IF (NPEROD .EQ. 3) JSTART = 1
         DO 122 J=JSTART,N,NP
            IF (J .NE. 1) GO TO 114
            JM1 = J+K
            JM2 = JM1+K
            GO TO 116
  114       JM1 = J-K
            JM2 = JM1-K
            IF (J .NE. N) GO TO 116
            IF (NPEROD .EQ. 0) GO TO 115
            JP1 = JM1
            JP2 = JM2
            GO TO 117
  115       JP1 = K
            JP2 = JP1+K
            GO TO 117
  116       JP1 = J+K
            JP2 = JP1+K
  117       DO 118 I=1,M
               B(I) = 2.*Q(I,J)+Q(I,JM2)+Q(I,JP2)
  118       CONTINUE
            CALL TRI (K1,K2,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 119 I=1,M
               T(I) = B(I)+Q(I,JM1)+Q(I,JP1)
               B(I) = T(I)
  119       CONTINUE
            CALL TRI (K1,K2,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 120 I=1,M
               Q(I,J) = Q(I,J)+B(I)
               B(I) = T(I)
  120       CONTINUE
            CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 121 I=1,M
               Q(I,J) = Q(I,J)+3.*B(I)
  121       CONTINUE
  122    CONTINUE
  123 CONTINUE
C
C     START REDUCTION FOR POWERS OF FIVE
C
  124 L = IEX5
      IF (NPEROD .EQ. 1) L = L-1
      IF (L .LE. 0) GO TO 138
      K2 = (N2PW+N2P3P)/2-1
      DO 137 KOUNT=1,L
         K = NP
         NP = 5*NP
         K1 = K2+1
         K2 = K2+K
         K3 = K2+1
         K4 = K2+NP
         JSTART = NP
         IF (NPEROD .EQ. 3) JSTART = 1
         DO 136 J=JSTART,N,NP
            IF (J .NE. 1) GO TO 125
            JM1 = J+K
            JM2 = JM1+K
            JM3 = JM2+K
            JM4 = JM3+K
            GO TO 127
  125       JM1 = J-K
            JM2 = JM1-K
            JM3 = JM2-K
            JM4 = JM3-K
            IF (J .NE. N) GO TO 127
            IF (NPEROD .EQ. 0) GO TO 126
            JP1 = JM1
            JP2 = JM2
            JP3 = JM3
            JP4 = JM4
            GO TO 128
  126       JP1 = K
            JP2 = JP1+K
            JP3 = JP2+K
            JP4 = JP3+K
            GO TO 128
  127       JP1 = J+K
            JP2 = JP1+K
            JP3 = JP2+K
            JP4 = JP3+K
  128       DO 129 I=1,M
               B(I) = 6.*Q(I,J)+4.*(Q(I,JM2)+Q(I,JP2))+Q(I,JM4)+Q(I,JP4)
  129       CONTINUE
            CALL TRI (K1,K2,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 130 I=1,M
               B(I) = B(I)+3.*(Q(I,JM1)+Q(I,JP1))+Q(I,JM3)+Q(I,JP3)
  130       CONTINUE
            CALL TRI (K1,K2,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 131 I=1,M
               T(I) = B(I)
               B(I) = 2.*Q(I,J)+Q(I,JM2)+Q(I,JP2)+B(I)
  131       CONTINUE
            CALL TRI (K1,K2,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 132 I=1,M
               B(I) = B(I)+Q(I,JM1)+Q(I,JP1)
  132       CONTINUE
            CALL TRI (K1,K2,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 133 I=1,M
               TEMP = B(I)+Q(I,J)
               B(I) = 4.*Q(I,J)+3.*(Q(I,JM2)+Q(I,JP2))+Q(I,JM4)+
     1                Q(I,JP4)-T(I)
               Q(I,J) = TEMP
  133       CONTINUE
            CALL TRI (K1,K2,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 134 I=1,M
               B(I) = B(I)+2.*(Q(I,JM1)+Q(I,JP1))+Q(I,JM3)+Q(I,JP3)
  134       CONTINUE
            CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 135 I=1,M
               Q(I,J) = Q(I,J)+5.*B(I)
  135       CONTINUE
  136    CONTINUE
  137 CONTINUE
C
C     INITIAL PHASE OF BACK SUBSTITUTION
C
  138 IF (NPEROD.EQ.1 .OR. NPEROD.EQ.2) GO TO 147
      IF (NPEROD .EQ. 0) GO TO 141
      DO 139 I=1,M
         B(I) = 2.*Q(I,1)
  139 CONTINUE
      CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
      DO 140 I=1,M
         Q(I,N) = Q(I,N)+B(I)
         B(I) = 4.*Q(I,N)
  140 CONTINUE
      CALL TRI (K4+1,K4+2*NP-1,2,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
      GO TO 143
  141 DO 142 I=1,M
         B(I) = 2.*Q(I,N)
  142 CONTINUE
      CALL TRI (K4+1,K4+NP-1,2,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
  143 DO 144 I=1,M
         Q(I,N) = Q(I,N)+B(I)
  144 CONTINUE
      IF (NPEROD .EQ. 0) GO TO 147
      DO 145 I=1,M
         B(I) = 2.*Q(I,N)
  145 CONTINUE
      CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
      DO 146 I=1,M
         Q(I,1) = Q(I,1)+B(I)
  146 CONTINUE
C
C     REGULAR BACK SUBSTITUTION FOR POWERS OF FIVE
C
  147 CONTINUE
      IF (IEX5 .EQ. 0) GO TO 171
      NP = N2P3P5
      K8 = KS5
      IF (NPEROD .EQ. 1) K3 = K3+NP/5
      DO 170 KOUNT=1,IEX5
         K = NP
         NP = NP/5
         K4 = K3-1
         K3 = K3-NP
         K1 = K8+1
         K2 = K8+2*NP
         K5 = K2+1
         K6 = K2+4*NP
         K7 = K6+1
         K8 = K6+2*NP
         JSTART = NP
         IF (NPEROD .EQ. 3) JSTART = 1+NP
         DO 169 J=JSTART,N,K
            JM1 = J-NP
            JP1 = J+NP
            JP2 = JP1+NP
            JP3 = JP2+NP
            JP4 = JP3+NP
            IF (JM1 .NE. 0) GO TO 151
            IF (NPEROD .EQ. 0) GO TO 149
            DO 148 I=1,M
               B(I) = Q(I,JP1)
  148       CONTINUE
            GO TO 153
  149       DO 150 I=1,M
               B(I) = Q(I,JP1)+Q(I,N)
  150       CONTINUE
            GO TO 153
  151       DO 152 I=1,M
               B(I) = Q(I,JP1)+Q(I,JM1)
  152       CONTINUE
  153       CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 154 I=1,M
               Q(I,J) = Q(I,J)+B(I)
               B(I) = Q(I,JP1)+Q(I,JP3)
  154       CONTINUE
            CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 155 I=1,M
               Q(I,JP2) = Q(I,JP2)+B(I)
  155       CONTINUE
            IF (JP4 .GT. N) GO TO 157
            DO 156 I=1,M
               B(I) = 2.*Q(I,JP2)+Q(I,J)+Q(I,JP4)
  156       CONTINUE
            GO TO 159
  157       DO 158 I=1,M
               B(I) = 2.*Q(I,JP2)+Q(I,J)
  158       CONTINUE
  159       CALL TRI (K1,K2,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 160 I=1,M
               Q(I,JP2) = Q(I,JP2)+B(I)
               B(I) = Q(I,JP2)+Q(I,J)
  160       CONTINUE
            CALL TRI (K5,K6,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 161 I=1,M
               Q(I,JP2) = Q(I,JP2)+B(I)
               B(I) = Q(I,J)+Q(I,JP2)
  161       CONTINUE
            CALL TRI (K7,K8,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 162 I=1,M
               Q(I,J) = Q(I,J)+B(I)
               B(I) = Q(I,J)+Q(I,JP2)
  162       CONTINUE
            CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 163 I=1,M
               Q(I,JP1) = Q(I,JP1)+B(I)
  163       CONTINUE
            IF (JP4 .GT. N) GO TO 165
            DO 164 I=1,M
               B(I) = Q(I,JP2)+Q(I,JP4)
  164       CONTINUE
            GO TO 167
  165       DO 166 I=1,M
               B(I) = Q(I,JP2)
  166       CONTINUE
  167       CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 168 I=1,M
               Q(I,JP3) = Q(I,JP3)+B(I)
  168       CONTINUE
  169    CONTINUE
  170 CONTINUE
C
C     REGULAR BACK SUBSTITUTION FOR POWERS OF THREE
C
  171 IF (IEX3 .EQ. 0) GO TO 191
      NP = N2P3P
      K2 = KS3
      IF (NPEROD.EQ.1 .AND. IEX5.EQ.0) K3 = K3+NP/3
      DO 190 KOUNT=1,IEX3
         K = NP
         NP = NP/3
         K4 = K3-1
         K3 = K3-NP
         K1 = K2+1
         K2 = K2+2*NP
         JSTART = NP
         IF (NPEROD .EQ. 3) JSTART = NP+1
         DO 189 J=JSTART,N,K
            JM1 = J-NP
            JP1 = J+NP
            JP2 = JP1+NP
            IF (JM1 .EQ. 0) GO TO 173
            DO 172 I=1,M
               B(I) = Q(I,JP1)+Q(I,JM1)
  172       CONTINUE
            GO TO 177
  173       IF (NPEROD .EQ. 0) GO TO 175
            DO 174 I=1,M
               B(I) = Q(I,JP1)
  174       CONTINUE
            GO TO 177
  175       DO 176 I=1,M
               B(I) = Q(I,JP1)+Q(I,N)
  176       CONTINUE
  177       CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 178 I=1,M
               Q(I,J) = Q(I,J)+B(I)
  178       CONTINUE
            IF (JP2 .GT. N) GO TO 180
            DO 179 I=1,M
               B(I) = Q(I,J)+Q(I,JP2)
  179       CONTINUE
            GO TO 182
  180       DO 181 I=1,M
               B(I) = Q(I,J)
  181       CONTINUE
  182       CALL TRI (K1,K2,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 183 I=1,M
               Q(I,J) = Q(I,J)+B(I)
  183       CONTINUE
            IF (JP2 .GT. N) GO TO 185
            DO 184 I=1,M
               B(I) = Q(I,J)+Q(I,JP2)
  184       CONTINUE
            GO TO 187
  185       DO 186 I=1,M
               B(I) = Q(I,J)
  186       CONTINUE
  187       CALL TRI (K3,K4,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 188 I=1,M
               Q(I,JP1) = Q(I,JP1)+B(I)
  188       CONTINUE
  189    CONTINUE
  190 CONTINUE
C
C     REGULAR BACK SUBSTITUTION FOR POWERS OF TWO
C
  191 IF (IEX2 .EQ. 0) GO TO 202
      NP = N2PW
      DO 201 KOUNT=1,IEX2
         K = NP
         NP = NP/2
         K3 = K-1
         JSTART = NP
         IF (NPEROD .EQ. 3) JSTART = 1+NP
         DO 200 J=JSTART,N,K
            JM1 = J-NP
            JP1 = J+NP
            IF (JM1 .NE. 0) GO TO 194
            IF (JP1 .GT. N) GO TO 200
            IF (NPEROD.EQ.1 .OR. NPEROD.EQ.2) GO TO 192
            JM1 = N
            GO TO 196
  192       DO 193 I=1,M
               B(I) = Q(I,JP1)
  193       CONTINUE
            GO TO 198
  194       IF (JP1 .LE. N) GO TO 196
            DO 195 I=1,M
               B(I) = Q(I,JM1)
  195       CONTINUE
            GO TO 198
  196       DO 197 I=1,M
               B(I) = Q(I,JM1)+Q(I,JP1)
  197       CONTINUE
  198       CALL TRI (NP,K3,1,M,MM1,BA,BB,BC,B,TWOCOS,D,W)
            DO 199 I=1,M
               Q(I,J) = Q(I,J)+B(I)
  199       CONTINUE
  200    CONTINUE
  201 CONTINUE
  202 RETURN
      END
      SUBROUTINE POINIT (NPEROD,N,IEX2,IEX3,IEX5,N2PW,N2P3P,N2P3P5,KS3,
     1                   KS5,NPSTOP,TWOCOS)
      REAL       TWOCOS(1)
C
C     PARAMETER NPSTOP IS NOT USED IN THIS SUBROUTINE.
C
C
C     MACHINE DEPENDENT CONSTANT
C
C     PI=3.1415926535897932384626433832795028841971693993751058209749446
C
      PI = 3.14159265358979
C
C     COMPUTE EXPONENTS OF 2,3, AND 5 IN N.
C
      NP = N
      IF (NPEROD .EQ. 1) NP = NP+1
      IF (NPEROD .EQ. 3) NP = NP-1
      IEX2 = 0
  101 K = NP/2
      IF (2*K .NE. NP) GO TO 102
      IEX2 = IEX2+1
      NP = K
      GO TO 101
  102 IEX3 = 0
  103 K = NP/3
      IF (3*K .NE. NP) GO TO 104
      IEX3 = IEX3+1
      NP = K
      GO TO 103
  104 IEX5 = 0
  105 K = NP/5
      IF (5*K .NE. NP) GO TO 106
      IEX5 = IEX5+1
      NP = K
      GO TO 105
  106 CONTINUE
      N2PW = 2**IEX2
      N2P3P = N2PW*(3**IEX3)
      N2P3P5 = N2P3P*(5**IEX5)
C
C     COMPUTE NECESSARY VALUES OF 2*COS(X)
C
      NP = 1
      TWOCOS(1) = 0.
      K = 1
      IF (IEX2 .EQ. 0) GO TO 110
      L = IEX2
      IF (IEX3+IEX5 .NE. 0) GO TO 107
      IF (NPEROD .EQ. 1) L = L-1
      IF (L .EQ. 0) GO TO 129
  107 DO 109 KOUNT=1,L
         NP = 2*NP
         DO 108 I=1,NP
            J = K+I
            TWOCOS(J) = 2.*COS((FLOAT(I)-.5)*PI/FLOAT(NP))
  108    CONTINUE
         K = K+NP
  109 CONTINUE
  110 IF (IEX3 .EQ. 0) GO TO 114
      L = IEX3
      IF (IEX5 .NE. 0) GO TO 111
      IF (NPEROD .EQ. 1) L = L-1
      IF (L .EQ. 0) GO TO 117
  111 DO 113 KOUNT=1,L
         NP = 3*NP
         DO 112 I=1,NP
            J = K+I
            TWOCOS(J) = 2.*COS((FLOAT(I)-.5)*PI/FLOAT(NP))
  112    CONTINUE
         K = K+NP
  113 CONTINUE
  114 L = IEX5
      IF (NPEROD .EQ. 1) L = L-1
      IF (L .LE. 0) GO TO 117
      DO 116 KOUNT=1,L
         NP = 5*NP
         DO 115 I=1,NP
            J = K+I
            TWOCOS(J) = 2.*COS((FLOAT(I)-.5)*PI/FLOAT(NP))
  115    CONTINUE
         K = K+NP
  116 CONTINUE
  117 IF (NPEROD.EQ.1 .OR. NPEROD.EQ.2) GO TO 121
      IF (NPEROD .EQ. 0) GO TO 119
      NPT2 = 2*NP
      DO 118 I=1,NPT2
         J = K+I
         TWOCOS(J) = 2.*COS(FLOAT(I)*PI/FLOAT(NP))
  118 CONTINUE
      K = K+NPT2
      GO TO 121
  119 DO 120 I=1,NP
         J = K+I
         TWOCOS(J) = 2.*COS(2.*FLOAT(I)*PI/FLOAT(NP))
  120 CONTINUE
      K = K+NP
  121 NP = N2P3P5
      IF (IEX5 .EQ. 0) GO TO 126
      KS5 = K
      DO 125 KOUNT=1,IEX5
         NP = NP/5
         NPT2 = 2*NP
         DO 122 I=1,NPT2
            J = K+I
            TWOCOS(J) = 2.*COS((FLOAT(I)-.5)*PI/FLOAT(NPT2))
  122    CONTINUE
         K = K+NPT2
         DO 123 I=1,NP
            J = K+4*I
            TWOCOS(J-3) = 2.*COS((FLOAT(I)-.8)*PI/FLOAT(NP))
            TWOCOS(J-2) = 2.*COS((FLOAT(I)-.6)*PI/FLOAT(NP))
            TWOCOS(J-1) = 2.*COS((FLOAT(I)-.4)*PI/FLOAT(NP))
            TWOCOS(J) = 2.*COS((FLOAT(I)-.2)*PI/FLOAT(NP))
  123    CONTINUE
         K = K+4*NP
         DO 124 I=1,NP
            J = K+2*I
            TWOCOS(J-1) = 2.*COS(FLOAT(3*I-2)*PI/FLOAT(3*NP))
            TWOCOS(J) = 2.*COS(FLOAT(3*I-1)*PI/FLOAT(3*NP))
  124    CONTINUE
         K = K+2*NP
  125 CONTINUE
  126 IF (IEX3 .EQ. 0) GO TO 129
      KS3 = K
      DO 128 KOUNT=1,IEX3
         NP = NP/3
         DO 127 I=1,NP
            J = K+2*I
            TWOCOS(J-1) = 2.*COS(FLOAT(3*I-2)*PI/FLOAT(3*NP))
            TWOCOS(J) = 2.*COS(FLOAT(3*I-1)*PI/FLOAT(3*NP))
  127    CONTINUE
         K = K+2*NP
  128 CONTINUE
  129 RETURN
      END
      SUBROUTINE TRIDD (KSTART,KSTOP,ISING,M,MM1,A,B,C,Y,TWOCOS,D,W)
      REAL       A(1)       ,B(1)       ,C(1)       ,Y(1)       ,
     1                TWOCOS(1)  ,D(1)       ,W(1)
C
C     SUBROUTINE TO SOLVE TRIDIAGONAL SYSTEMS
C
C
C     PARAMETER W NOT USED IN THIS SUBROUTINE.
C
      DO 103 K=KSTART,KSTOP
         X = -TWOCOS(K)
         D(1) = C(1)/(B(1)+X)
         Y(1) = Y(1)/(B(1)+X)
         DO 101 I=2,M
            Z = B(I)+X-A(I)*D(I-1)
            D(I) = C(I)/Z
            Y(I) = (Y(I)-A(I)*Y(I-1))/Z
  101    CONTINUE
         DO 102 IP=1,MM1
            I = M-IP
            Y(I) = Y(I)-D(I)*Y(I+1)
  102    CONTINUE
  103 CONTINUE
      IF (ISING .EQ. 1) RETURN
      D(1) = C(1)/(B(1)-2.)
      Y(1) = Y(1)/(B(1)-2.)
      DO 104 I=2,MM1
         Z = B(I)-2.-A(I)*D(I-1)
         D(I) = C(I)/Z
         Y(I) = (Y(I)-A(I)*Y(I-1))/Z
  104 CONTINUE
      Z = B(M)-2.-A(M)*D(M-1)
      X = Y(M)-A(M)*Y(M-1)
      IF (Z .NE. 0.) GO TO 105
      Y(M) = 0.
      GO TO 106
  105 Y(M) = X/Z
  106 DO 107 IP=1,MM1
         I = M-IP
         Y(I) = Y(I)-D(I)*Y(I+1)
  107 CONTINUE
      RETURN
      END
      SUBROUTINE TRIDP (KSTART,KSTOP,ISING,M,MM1,A,B,C,Y,TWOCOS,D,W)
      REAL       A(1)       ,B(1)       ,C(1)       ,Y(1)       ,
     1                TWOCOS(1)  ,D(1)       ,W(1)
C
C     SUBROUTINE TO SOLVE PERIODIC TRIDIAGONAL SYSTEM
C
      DO 103 K=KSTART,KSTOP
         X = -TWOCOS(K)
         D(1) = C(1)/(B(1)+X)
         W(1) = A(1)/(B(1)+X)
         Y(1) = Y(1)/(B(1)+X)
         BM = B(M)
         Z = C(M)
         DO 101 I=2,MM1
            DEN = B(I)+X-A(I)*D(I-1)
            D(I) = C(I)/DEN
            W(I) = -A(I)*W(I-1)/DEN
            Y(I) = (Y(I)-A(I)*Y(I-1))/DEN
            Y(M) = Y(M)-Z*Y(I-1)
            BM = BM-Z*W(I-1)
            Z = -Z*D(I-1)
  101    CONTINUE
         D(MM1) = D(MM1)+W(MM1)
         Z = A(M)+Z
         DEN = BM+X-Z*D(MM1)
         Y(M) = Y(M)-Z*Y(M-1)
         Y(M) = Y(M)/DEN
         Y(MM1) = Y(MM1)-D(MM1)*Y(M)
         DO 102 IP=2,MM1
            I = M-IP
            Y(I) = Y(I)-D(I)*Y(I+1)-W(I)*Y(M)
  102    CONTINUE
  103 CONTINUE
      IF (ISING .EQ. 1) RETURN
      D(1) = C(1)/(B(1)-2.)
      W(1) = A(1)/(B(1)-2.)
      Y(1) = Y(1)/(B(1)-2.)
      BM = B(M)
      Z = C(M)
      DO 104 I=2,MM1
         DEN = B(I)-2.-A(I)*D(I-1)
         D(I) = C(I)/DEN
         W(I) = -A(I)*W(I-1)/DEN
         Y(I) = (Y(I)-A(I)*Y(I-1))/DEN
         Y(M) = Y(M)-Z*Y(I-1)
         BM = BM-Z*W(I-1)
         Z = -Z*D(I-1)
  104 CONTINUE
      D(MM1) = D(MM1)+W(MM1)
      Z = A(M)+Z
      DEN = BM-2.-Z*D(MM1)
      Y(M) = Y(M)-Z*Y(M-1)
      IF (DEN .NE. 0.) GO TO 105
      Y(M) = 0.
      GO TO 106
  105 Y(M) = Y(M)/DEN
  106 Y(MM1) = Y(MM1)-D(MM1)*Y(M)
      DO 107 IP=2,MM1
         I = M-IP
         Y(I) = Y(I)-D(I)*Y(I+1)-W(I)*Y(M)
  107 CONTINUE
      RETURN
      END
      FUNCTION APXEPS (DUM)
C          RETURN MACHINE ACC.
      APXEPS = 1.00  E -4
      DO 20 I = 1,100
      S = APXEPS * 0.01
      S = S + 1.
      S = S - 1.
C     PRINT 10,I,S,APXEPS
      IF (S.LE.DUM) RETURN
   20 APXEPS = APXEPS *.1
      RETURN
   10 FORMAT (10X,'I = ',I4,' S = ',F22.18,' EPS = ',F22.18)
      END
