

use GEOS_Mod


integer i,l
integer i1, i2, rate

real*4, dimension(140:340) :: q1, q2, q3, q4, q5, q6, t, p
real*4, dimension(140:340) :: dq1, dq2, dq3, dq4, dq5, dq6
real*4, dimension(140:340,100) :: dq1x, dq2x, tx, px, q3x, q4x

call GEOS_QsatSet(Formulation=3)

do i=140,340
   p(i)=100000.
   t(i)=i
   px(i,:)=10000.
   tx(i,:)=i
   q3(i)=GEOS_QsatLQU(t(i))
   q4(i)=GEOS_QsatICE(t(i))
   q5(i)=GEOS_QsatLQU(t(i),P(i))
   q6(i)=GEOS_QsatICE(t(i),P(i))
enddo

call geos_qsatset(usetable=.false.)

do i=140,340
   q1(i)=geos_QSATLQU(t(i))
   q2(i)=geos_QSATICE(t(i))
   print *, t(i),q1(i),q3(i),q2(i),q4(i),q5(i),q6(i)
enddo

call random_number(t)

t = 253. + 50.*t


call SYSTEM_CLOCK(COUNT_RATE=RATE)

call SYSTEM_CLOCK(i1)

DO L=1,10000
do i=140,340
   q1(i)=geos_QSATICE(t(i))
   q2(i)=geos_QSATLQU(t(i))
ENDDO
ENDDO


call SYSTEM_CLOCK(i2)

call geos_qsatset(usetable=.true.)

print '(A,f12.8)', 'form  ',  (i2-i1)/Float(rate)

call SYSTEM_CLOCK(i1)

DO L=1,10000
do i=140,340
   q3(I)=GEOS_QsatICE(t(I))
   q4(I)=GEOS_QsatLQU(t(I))
ENDDO
ENDDO

call SYSTEM_CLOCK(i2)

print '(A,f12.8)', 'table S ',  (i2-i1)/Float(rate)

call SYSTEM_CLOCK(i1)

DO L=1,10000
do i=140,340
   q3(I)=GEOS_QsatICE(t(I),p(i))
   q4(I)=GEOS_QsatLQU(t(I),p(i))
ENDDO
ENDDO

call SYSTEM_CLOCK(i2)

print '(A,f12.8)', 'table SQ ',  (i2-i1)/Float(rate)

call SYSTEM_CLOCK(i1)

DO L=1,10000
   q3=GEOS_QsatICE(t)
   q4=GEOS_QsatLQU(t)
ENDDO

call SYSTEM_CLOCK(i2)

print '(A,f12.8)', 'table V',  (i2-i1)/Float(rate)

call SYSTEM_CLOCK(i1)

DO L=1,10000
   q4=GEOS_qsatICE(t,p)
   q3=GEOS_qsatLQU(t,p)
ENDDO

call SYSTEM_CLOCK(i2)

print '(A,f12.8)', 'table VQ',  (i2-i1)/Float(rate)

call SYSTEM_CLOCK(i1)

DO L=1,10000
   q4=GEOS_qsatICE(t,p,dq1)
   q3=GEOS_qsatLQU(t,p,dq2)
ENDDO

call SYSTEM_CLOCK(i2)

print '(A,f12.8)', 'table VQD',  (i2-i1)/Float(rate)

call SYSTEM_CLOCK(i1)

DO L=1,100
   q4x=GEOS_qsatICE(tx,px,dq1x)
   q3x=GEOS_qsatLQU(tx,px,dq2x)
ENDDO

call SYSTEM_CLOCK(i2)

print '(A,f12.8)', 'table VQD',  (i2-i1)/Float(rate)

do i=140,340
t(i)=i
enddo

call GEOS_QsatSet(Formulation=1)
q1=GEOS_Qsatlqu(T)
q4=GEOS_Qsatice(T)
call GEOS_QsatSet(Formulation=2)
q2=GEOS_Qsatlqu(T)
q5=GEOS_Qsatice(T)
call GEOS_QsatSet(Formulation=3)
q3=GEOS_Qsatlqu(T)
q6=GEOS_Qsatice(T)


do i=140,340
   print *, t(i),q1(i),q2(i),q3(i),q4(i),q5(i),q6(i)
enddo


call GEOS_QsatSet(Formulation=1)
q1=GEOS_Qsatlqu(T,p)
q4=GEOS_Qsatice(T,p)
call GEOS_QsatSet(Formulation=2)
q2=GEOS_Qsatlqu(T,p)
q5=GEOS_Qsatice(T,p)
call GEOS_QsatSet(Formulation=3)
q3=GEOS_Qsatlqu(T,p)
q6=GEOS_Qsatice(T,p)


do i=140,340
   print *, t(i),q1(i),q2(i),q3(i),q4(i),q5(i),q6(i)
enddo

stop

end

