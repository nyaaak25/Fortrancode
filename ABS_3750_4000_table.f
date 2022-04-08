C***********************************************************************
	  PROGRAM ABS_3750_4000_table
C***********************************************************************
C Compile : ifort ABS_3750_4000_table.f /home/pat1/WORK0/aoki/SARTre/TIPS_2011.o -o ABS.out 
C
C NAME : ABS
C	
C Version 1.0
C
C MODIFICATION HISTORY:
C SA, Oct., 2012. Ver1.0
C
C SA: Shohei Aoki, aoki@pat.gp.tohoku.ac.jp
C***********************************************************************
C***  Difine parameters
      implicit none

C	  Atmospheric parameters
C		-30�w���� 

      INTEGER NLAYER,CD,I,J
	  PARAMETER(NLAYER=51)

C	  ���x[km]
      real*8  HIGH(NLAYER) 
C	  ���x[k]
      real*8  TEMP(NLAYER) 
C	  ����[mb]
      real*8  PRES(NLAYER) 

C	  PFS parameters
	  CHARACTER*100 OPFIL1,OPFIL2,OPFIL3,OPFIL4
	  INTEGER NCH
	  PARAMETER(NCH=250000)
	  real*8 Ls, Lon, Lat, LT
	  real*8 WN0
	  real*8 WN(NCH)
	  PARAMETER(WN0=3750.0d+00)

C	  parameters
	  real*8 SP,H,R,g,T1,T2,a,b,H1,H2
	  PARAMETER(R=192.0d+00)
	  PARAMETER(g=3.72d+00)
	  PARAMETER(SP=610.0d+00)

C     Output
	  real*8 Kw(NCH,NLAYER)

C	  loop
	  Integer IT1,IT2
	  
C	  Wn Grid
	  WN(1)=WN0
	  DO 1 I= 2, NCH
		WN(I) = WN(1) + dble(I-1)*0.001d+00
1	  CONTINUE

C**** Output
 	  OPEN (3,FILE='OMEGA_Water/look-up-table/LUtable.v',
     &			FORM='binary',STATUS='NEW')
 	  WRITE(3) 1,1
 	  WRITE(3) 1,NCH
 	  WRITE(3) WN
 	  CLOSE(3)

C**** Loop Start
		  DO 101 IT1=1,5
            IF (IT1.EQ.1) T1=135.0d+00
		  	IF (IT1.EQ.2) T1=160.0d+00
		  	IF (IT1.EQ.3) T1=213.0d+00
		  	IF (IT1.EQ.4) T1=260.0d+00
            IF (IT1.EQ.5) T1=285.0d+00

			DO 102 IT2=1,3
				IF (IT2.EQ.1) T2=80.0d+00
			  	IF (IT2.EQ.2) T2=146.0d+00
		  		IF (IT2.EQ.3) T2=200.0d+00
C*******
      IF (IT1.EQ.1.and.IT2.EQ.1) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA1_TB1.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA1_TB1.atmos'
	  ENDIF
      IF (IT1.EQ.1.and.IT2.EQ.2) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA1_TB2.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA1_TB2.atmos'
	  ENDIF
      IF (IT1.EQ.1.and.IT2.EQ.3) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA1_TB3.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA1_TB3.atmos'
	  ENDIF
C*******
      IF (IT1.EQ.2.and.IT2.EQ.1) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA2_TB1.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA2_TB1.atmos'
	  ENDIF
      IF (IT1.EQ.2.and.IT2.EQ.2) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA2_TB2.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA2_TB2.atmos'
	  ENDIF
      IF (IT1.EQ.2.and.IT2.EQ.3) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA2_TB3.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA2_TB3.atmos'
	  ENDIF
C*******
      IF (IT1.EQ.3.and.IT2.EQ.1) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA3_TB1.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA3_TB1.atmos'
	  ENDIF
      IF (IT1.EQ.3.and.IT2.EQ.2) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA3_TB2.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA3_TB2.atmos'
	  ENDIF
      IF (IT1.EQ.3.and.IT2.EQ.3) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA3_TB3.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA3_TB3.atmos'
	  ENDIF
C*******
      IF (IT1.EQ.4.and.IT2.EQ.1) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA4_TB1.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA4_TB1.atmos'
	  ENDIF
      IF (IT1.EQ.4.and.IT2.EQ.2) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA4_TB2.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA4_TB2.atmos'
	  ENDIF
      IF (IT1.EQ.4.and.IT2.EQ.3) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA4_TB3.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA4_TB3.atmos'
	  ENDIF
C*******
      IF (IT1.EQ.5.and.IT2.EQ.1) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA5_TB1.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA5_TB1.atmos'
	  ENDIF
      IF (IT1.EQ.5.and.IT2.EQ.2) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA5_TB2.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA5_TB2.atmos'
	  ENDIF
      IF (IT1.EQ.5.and.IT2.EQ.3) then
		OPFIL1 ='OMEGA_Water/look-up-table/TA5_TB3.k'
      	OPFIL2 ='OMEGA_Water/look-up-table/TA5_TB3.atmos'
	  ENDIF
C*******


C***  Hight
	  DO 3 i = 2, NLAYER
		HIGH(i) = HIGH(1) + dble(i-1)*1000.0d+00
3	  continue

C***  Scale Hight
	  H=R*T1/g

C***  Pressure Profile
	  DO 4 I = 1, NLAYER
		PRES(I)=SP*dexp(-HIGH(I)/H)
4	  CONTINUE

C***  Temp Profile
	  H1=H*0.1d+00
	  H2=H*4.0d+00
	  a=(T1-T2)/(H1-H2)
	  b=T1-a*H1

	  DO 5 I = 1, NLAYER
		TEMP(I)=a*HIGH(I)+b
		IF(HIGH(I).ge.H*4.0) TEMP(I)=T2
5	  CONTINUE

C***  CKD
	  CALL CKD(NLAYER,HIGH,TEMP,PRES,NCH,WN,Kw)
C***  OUTPUT
	  CALL OUTPUT(OPFIL1,OPFIL2,
     &				NLAYER,NCH,HIGH,TEMP,PRES,WN,Kw)

C***
102	  CONTINUE
101	  CONTINUE
100	  CONTINUE
C***
	  Write(6,*) 'Done!' 
C***	  
	  End
C***********************************************************************
	  SUBROUTINE CKD(NLAYER,HIGH,TEMP,PRES,NCH,WN,Kw)
C***********************************************************************
C***	  
      implicit none
	  INTEGER I,J,L

C	  Atmospheric parameters
      INTEGER NLAYER
      real*8  HIGH(NLAYER) 
      real*8  TEMP(NLAYER) 
      real*8  PRES(NLAYER) 
      real*8  T, P, R

C	  PFS parameters
	  INTEGER NCH
	  real*8 WN(NCH)

C	  ABSC
	  real*8 Kw(NCH,NLAYER)

C***	  
		write(6,*) 'Calculating...'
	    CALL CALK(1,NCH,NLAYER,TEMP,PRES,WN,KwC)

C***	  
	  return
	  END
C***********************************************************************
	  SUBROUTINE CALK(MOL,NCH,NLAYER,TEMP,PRES,WN,Kw)
C     ���x�A���͂��ƂɑS�g����K���v�Z
C***********************************************************************
	  implicit none 
	  Integer NCH
	  INTEGER NLAYER
	  INTEGER N_gridP,N_gridT
      real*8  TEMP(NLAYER) 
      real*8  PRES(NLAYER) 
	  real*8  Kw(NCH,NLAYER)
	  real*8  WN(NCH)
C***  local
	  INTEGER I,J,L,LAYER
	  INTEGER*8 WNI(NCH)
	  real*8 T,P,R
	  real*8 dU
	  real*8 Kb
	  CHARACTER*50 OPFIL
	  Parameter(Kb=1.380658d-23)
C***  local
      INTEGER MOL,ISONO,IWORK
	  real*8  FREQ0,STREN0,TRPROB,ALPHA0,ALPHAS,ENERGY,TDEP
	  real*8  ABF,ALPHA,STREN,F(NCH),ALPHAL,ALPHAD,RTPI,STREN2
      real*8  C,TEMP0,PRESS0,NA,MOLW(8),X(NCH),Y,VOIGT,Ps
	  PARAMETER (PRESS0=1.01325d+05,TEMP0=296.0d+00,C=2.99792458d+08)
	  PARAMETER (NA=6.0221367d+23,RTPI=1.7724538509d+00)
C	  ABF=1.3 by Ignative et al., 2005
	  PARAMETER (ABF=1.0d0)
C***
	  IF(MOL.EQ.1) then
	   MOLW(1)=0.018d+00
	   MOLW(2)=0.020d+00
	   MOLW(3)=0.019d+00
	   MOLW(4)=0.019d+00
	   MOLW(5)=0.021d+00
	   MOLW(6)=0.020d+00
	   MOLW(7)=0.000d+00
	   MOLW(8)=0.000d+00
	  endif

	  IF(MOL.EQ.2) then
       MOLW(1)=0.044d+00
	   MOLW(2)=0.045d+00
	   MOLW(3)=0.046d+00
	   MOLW(4)=0.045d+00
	   MOLW(5)=0.047d+00
	   MOLW(6)=0.046d+00
	   MOLW(7)=0.048d+00
	   MOLW(8)=0.047d+00
	  endif

	  IF(MOL.EQ.5) then
       MOLW(1)=0.028d+00
	   MOLW(2)=0.029d+00
	   MOLW(3)=0.030d+00
	   MOLW(4)=0.029d+00
	   MOLW(5)=0.031d+00
	   MOLW(6)=0.030d+00
	   MOLW(7)=0.000d+00
	   MOLW(8)=0.000d+00
	  endif

	  IF(MOL.EQ.6) then
	   MOLW(1)=0.016d+00
	   MOLW(2)=0.017d+00
	   MOLW(3)=0.017d+00
	   MOLW(4)=0.000d+00
	   MOLW(5)=0.000d+00
	   MOLW(6)=0.000d+00
	   MOLW(7)=0.000d+00
	   MOLW(8)=0.000d+00
	  endif

C***  ������
	  DO 1 I = 1, NCH
		DO 2 LAYER = 1, NLAYER
			Kw(I,LAYER) = 0.0d+00
2 	  CONTINUE 
1 	  CONTINUE 
	  IWORK = 0
      close(2)

C***  HITRAN�f�[�^�ǂݍ���
	  CALL FINDH(WN(1),MOL)
200	  CONTINUE
      CALL HREAD(MOL,WN(NCH),IWORK,ISONO,FREQ0
     &			 ,STREN0,TRPROB,ALPHA0,ALPHAS,ENERGY,TDEP)
C***
	  IF (IWORK.eq.1) GO TO 999
C***
	  write(6,*)'FREQ0:' ,FREQ0,'	MOLNUM:',MOL,'	ISONUM:',ISONO
C***
	  DO 100 LAYER = 1, NLAYER
		T = TEMP(LAYER)
		P = PRES(LAYER)
		R = VMR(LAYER)

C***�@AIR-BROADENED �C��
	  ALPHA=ALPHA0*ABF

C***�@�C�ӂ̉��x�A���͂ɉ����������x�֕ϊ�
	  STREN2=STREN(STREN0,T,ENERGY,MOL,FREQ0,ISONO)

C***�@���`�֐����v�Z
C     ����
	  Ps = P * R

C	  ���[�����c���`�� (air-broadning + self-broading)
C	  ALPHAL=(ALPHA*(P-Ps)+ALPHAS*Ps)/PRESS0*(TEMP0/T)**TDEP
C	  ���[�����c���`�� (air-broadning�ߎ�)
	  ALPHAL=(ALPHA*P)/PRESS0*(TEMP0/T)**TDEP

C     �h�b�v���[���`�̃�
	  ALPHAD=FREQ0/C*dSQRT(2.0d+00*NA*Kb*T/MOLW(ISONO))	 

C     X AND Y FOR VOJGT FUNCTION
	  DO 4 I = 1, NCH
	    X(I)=dABS(WN(I)-FREQ0)/ALPHAD
4     CONTINUE
	  Y = ALPHAL/ALPHAD

	  DO 5 I = 1, NCH
		F(I) = VOIGT(X(I),Y)/ALPHAD/RTPI
5 	  CONTINUE 

C*** �g����Ԃ�K�v�Z
	  DO 6 I = 1, NCH
		Kw(I,LAYER) = Kw(I,LAYER) + F(I)*STREN2
6 	  CONTINUE 

C*** 
100	  CONTINUE
C*** 
998	  CONTINUE
C*** 
	  IF(IWORK.NE.1) GO TO 200
999	  CONTINUE
C*** 
	  DO 7 LAYER = 1, NLAYER
	  DO 8 I = 1, NCH
		dU=PRES(LAYER)/TEMP(LAYER)/Kb*1.0d-1
	    Kw(I,LAYER)=Kw(I,LAYER)*dU
8 	  CONTINUE 
7 	  CONTINUE 

C***
	  return
	  end
C***********************************************************************
	  SUBROUTINE OUTPUT(OPFIL1,OPFIL2,NLAYER,NCH,HIGH,TEMP,PRES,WN,Kw)
C***********************************************************************
      implicit none
	  CHARACTER*100 OPFIL1,OPFIL2
      INTEGER I,J,NLAYER,NCH
      real*8  WN(NCH) 
	  real*8  Kw(NCH,NLAYER)
      real*8  HIGH(NLAYER) 
      real*8  TEMP(NLAYER) 
      real*8  PRES(NLAYER) 
C**** 
	  DO 10 I = 1, NLAYER
		HIGH(I) = HIGH(I)*1d-3
		PRES(I) = PRES(I)*1d-2
10	  CONTINUE
C**** 
	  OPEN (3,FILE=OPFIL1,form='binary',STATUS='NEW')
	  DO 1 J = NLAYER, 1, -1
C		WRITE(*,*) 1, real(HIGH(j)), real(TEMP(j)), real(PRES(j))
		WRITE(3) 1, real(HIGH(j)), real(TEMP(j)), 
     &				real(PRES(j)), real(Kw(1:NCH,J))
1	  CONTINUE
	  CLOSE(3)
C****
	  OPEN (3,FILE=OPFIL2,STATUS='NEW')
	  WRITE(3,*) NLAYER
	  DO 3 J = 1, NLAYER
		WRITE(3,*) real(HIGH(j)), real(TEMP(j)), real(PRES(j))
3	  CONTINUE
	  CLOSE(3)
CC****
 		return
C****
		end
C***********************************************************************
      SUBROUTINE FINDH(FREQL0,MOL)
C***********************************************************************
C***	  
      implicit none
      CHARACTER RDFILE*50
	  INTEGER MOL
      real*8 FREQL0,FREQ0

C	  �w�肵���g���̂͂��߂��n�C�g�����f�[�^����T��
      IF(MOL.EQ.1) RDFILE='/home/pat1/WORK0/aoki/hitran/01_hit08.par'      
      OPEN (2,FILE=RDFILE,STATUS='OLD',ERR=906)
      READ (2,1000,ERR=907) FREQ0
      IF (FREQ0.GE.FREQL0) GO TO 100
1	  CONTINUE
      READ (2,1000,ERR=907) FREQ0
      IF (FREQ0.GE.FREQL0) RETURN
      GO TO 1
C****
906   STOP 'HITRAN OPEN ERROR'
907   STOP 'HITRAN READ ERROR'
1000  FORMAT (3X,F12.6)
C***	  
100	  CONTINUE
	  RETURN
	  END
C***********************************************************************
      SUBROUTINE HREAD(MOL,FREQU0,IWORK,ISONO,FREQ0,STREN0,TRPROB,
     &					ALPHA0,ALPHAS,ENERGY,TDEP)
C***********************************************************************
C***	  
C MOLNO,ISONO,FREQ0,STREN0,TRPROB,ALPHA0,ALPHAS,ENERGY,TDEP
C ���q�ԍ�,���ʑ̔ԍ�,�g��,�J�ڋ��x,�A�C���V���^�C���W��,AIR-BROADENED HALF-WIDTH,SELF-BROADENED HALF-WIDTH,LOWER-STATE ENERGY,TEMPERATURE-DEPENDANCE EXPONENT FOR ��
      implicit none
      INTEGER MOLNO,ISONO,MOL,IWORK
	  real*8  FREQ0,STREN0,FREQU0,TRPROB,ALPHA0,ALPHAS,ENERGY,TDEP
  
	  READ (2,1000,ERR=907) MOLNO,ISONO,FREQ0,STREN0,TRPROB,
     &                      ALPHA0,ALPHAS,ENERGY,TDEP
      IF (FREQ0.LE.FREQU0) THEN
	    IF (MOLNO.NE.MOL) GO TO 907
		RETURN
      END IF
 1000 FORMAT (I2,I1,F12.6,2E10.3,2F5.4,F10.4,F4.2,F8.6,2A15,2A15,2I1,
     &        2I1,2I1,2I2,2I2,2I2,A1,2F7.1)
C***      
      IWORK=1
      RETURN
C***      
907   STOP 'HITRAN READ ERROR'
      END
C***********************************************************************
      real*8 FUNCTION STREN(STREN0,TEMP1,ENERGY,MOL,FREQ0,ISONO)
C***********************************************************************
C***	  
      implicit none
      INTEGER MOL,ISONO
      real*8 STREN0,FREQ0,ENERGY
	  real*8 dTEMP0,dTEMP1
	  real*8 TEMP0,TEMP1
      PARAMETER (TEMP0=296.0d+00)
	  real*8 Qi,Q0,Q1,Q

		call BD_TIPS_2011(MOL,TEMP0,ISONO,Qi,Q0)
		call BD_TIPS_2011(MOL,TEMP1,ISONO,Qi,Q1)
		Q = Q0/Q1

		  STREN=STREN0*Q
     &      *dEXP(1.439d+00*ENERGY*(1d+00/TEMP0-1/TEMP1))
     &      *(1-dEXP(-1.439d+00*FREQ0/TEMP1))
     &      /(1-dEXP(-1.439d+00*FREQ0/TEMP0))
	  RETURN
      END
C***********************************************************************
      real*8 FUNCTION VOIGT (X,Y)
C	  REF :  DRAYSON S. R., 1975
C***********************************************************************
C****
C****  ROUTINE COMPUTES THE VOIGT FUNCTION: Y/PI*INTEGRAL FROM  ***
C****   - TO + INFINITY OF EXP(-T*T)/(Y*Y+(X-T)*(X-T)) DT       ***
C****
      real*8 X,Y
	  real*8 B(22),RI(15),XN(15),YN(15)
      real*8 D0(25),D1(25),D2(25),D3(25),D4(25)
      real*8 HN(25),H,XX(3),HH(3),NBY2(19),C(21)
      LOGICAL TRU
      DATA B(1)/0.0/,B(2)/0.7093602E-7/
      DATA XN/10.0,9.0,2*8.0,7.0,6.0,5.0,4.0,7*3.0/
      DATA YN/3*0.6,0.5,2*0.4,4*0.3,1.0,0.9,0.8,2*0.7/
      DATA H/0.201/
      DATA XX/0.5246476,1.65068,0.7071068/
      DATA HH/0.2562121,0.2588268E-1,0.2820948/
      DATA NBY2/9.5,9.0,8.5,8.0,7.5,7.0,6.5,6.0,5.5,5.0,4.5,4.0,3.5,3.0,
     &  2.5,2.0,1.5,1.0,0.5/
      DATA C / 0.7093602E-7,-0.2518434E-6, 0.8566874E-6,-0.2787638E-5,
     &         0.8660740E-5,-0.2565551E-4, 0.7228775E-4,-0.1933631E-3,
     &         0.4899520E-3,-0.1173267E-2, 0.2648762E-2,-0.5623190E-2,
     &         0.1119601E-1,-0.2084976E-1, 0.3621573E-1,-0.5851412E-1,
     &         0.8770816E-1,-0.121664, 0.15584, -0.184, 0.2           /
      DATA TRU/.FALSE./
      IF (TRU) GO TO 104
C****
C****  REGION I: COMPUTE DAWSON'S FUNCTION AT MESH POINTS. ***
C****
      TRU=.TRUE.
      DO 101 I=1,15
        RI(I)=-I/2.0
  101 CONTINUE
      DO 103 I=1,25
        HN(I)=H*(I-0.5)
        CO=4*HN(I)*HN(I)/25-2
        DO 102 J=2,21
          B(J+1)=CO*B(J)-B(J-1)+C(J)
  102   CONTINUE
        D0(I)=HN(I)*(B(22)-B(21))/5
        D1(I)=1-2*HN(I)*D0(I)
        D2(I)=(HN(I)*D1(I)+D0(I))/RI(2)
        D3(I)=(HN(I)*D2(I)+D1(I))/RI(3)
        D4(I)=(HN(I)*D3(I)+D2(I))/RI(4)
  103 CONTINUE
  104 CONTINUE
      IF (X.GE.5.0) GO TO 112
      IF (Y.LE.1.0) GO TO 110
      IF (X.GT.1.85*(3.6-Y)) GO TO 112
C****
C****  REGION II: CONTINUED FRACTION. COMPUTE NUMBER OF TERMS NEEDED. ***
C****
      IF (Y.LT.1.45) GO TO 107
      I=Y+Y
      GO TO 108
  107 CONTINUE
      I=11*Y
  108 CONTINUE
      J=X+X+1.85
      MAX=XN(J)*YN(I)+0.46
      MIN=MIN0(16,21-2*MAX)
C****
C****  EVALUATE CONTINUED FRACTION. ***
C****
      UU=Y
      VV=X
      DO 109 J=MIN,19
        U=NBY2(J)/(UU*UU+VV*VV)
        UU=Y+U*UU
        VV=X-U*VV
  109 CONTINUE
      VOIGT=UU/(UU*UU+VV*VV)/1.772454
      RETURN
C****
  110 CONTINUE
      Y2=Y*Y
      IF (X+Y.GE.5.0) GO TO 113
C****
C****  REGION I: COMPUTE DAWSON'S FUNCTION AT X FROM TAYLOR SERIES. ***
C****
      N=X/H
      DX=X-HN(N+1)
      U=(((D4(N+1)*DX+D3(N+1))*DX+D2(N+1))*DX+D1(N+1))*DX+D0(N+1)
      V=1-2*X*U
C****
C****  TAYLOR SERIES EXPANSION ABOUT Y=0.0. ***
C****
      VV=EXP(Y2-X*X)*COS(2*X*Y)/1.128379-Y*V
      UU=-Y
      MAX=5+(12.5-X)*0.8*Y
      DO 111 I=2,MAX,2
        U=(X*V+U)/RI(I)
        V=(X*U+V)/RI(I+1)
        UU=-UU*Y2
        VV=VV+V*UU
  111 CONTINUE
      VOIGT=1.128379*VV
      RETURN
C****
  112 CONTINUE
      Y2=Y*Y
      IF (Y.LT.11-0.6875*X) GO TO 113
C****
C****  REGION IIIB: 2-POINT GAUSS-HERMITE QUADRATURE. ***
C****
      U=X-XX(3)
      V=X+XX(3)
      VOIGT=Y*(HH(3)/(Y2+U*U)+HH(3)/(Y2+V*V))
      RETURN
C****
C****  REGION IIIA: 4-POINT GAUSS-HERMITE QUADRATURE. ***
C****
  113 CONTINUE
      U=X-XX(1)
      V=X+XX(1)
      UU=X-XX(2)
      VV=X+XX(2)
      VOIGT=Y*(HH(1)/(Y2+U*U)+HH(1)/(Y2+V*V)
     &      +HH(2)/(Y2+UU*UU)+HH(2)/(Y2+VV*VV))
      RETURN
      END
