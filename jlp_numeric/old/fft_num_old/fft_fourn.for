C++*****************************************************************
C Set of routines for FFT computation (without NAG)
C Contains:
C FFT_2D_FOURN, FFT_2D_FOURN_DBLE, FFT_1D, FOURN2, FOURN1, RECENTRE
C
C Warning: this version divides by NX*NY the inverse FFT
C
C JLP
C Version 21-03-96
C--******************************************************************
C********************************************************************
C FFT 2D_FOURN routine
C Input:
C TR, TI:  real part and imaginary part  (input/output) of the image
C
C KOD=1, or 2 direct
C KOD=-1, or -2 inverse
C KOD=-2, or 2: power spectrum, and phase
C********************************************************************
	SUBROUTINE FFT_2D_FOURN(TR,TI,NX,NY,IDIM,KOD)
	REAL*4 TR(IDIM,*),TI(IDIM,*)
	REAL PI
	INTEGER NX,NY,KOD,ISIGN
        PI=3.14159265358979323846	
 
C Check the size (should be a power of 2 : NX=2**M and NY=NX)
C M = ln(NX)/ln(2)
	M=0
	NF=1
1	M=M+1
	NF=NF+NF
	IF(NF.LT.NX) GOTO 1
C        PRINT *,'M,NX,NF',M,NX,NF
 
	IF((NF.NE.NX).OR.(NX.NE.NY))THEN
	   WRITE(6,11) NX,NY
11	   FORMAT(' FFT_2D/Fatal error: Wrong size for the input image',/,
     1	' (should be square and power of two)',/,' NX, NY :',I5,1X,I5)
           STOP
	ENDIF
 
C Flag for direct or inverse FFT:
	IF(KOD.LT.0) THEN
          ISIGN = -1
        ELSE
          ISIGN = 1
	ENDIF

C Actual FFT now:
         CALL FOURN2(TR,TI,NX,NY,IDIM,ISIGN)

C Check the value of KOD:
	IF(IABS(KOD).EQ.2)THEN
C Power and phase spectrum:
	 DO 60 J=1,NY
	  DO 60 I=1,NX
	    AR=TR(I,J)
	    AI=TI(I,J)
	    TR(I,J)=SQRT(AR*AR+AI*AI)
            IF(AR.NE.0.) THEN
	       TI(I,J)=ATAN(AI/AR)
	       IF(ACOS(AI/AR).LT.0.)TI(I,J)=TI(I,J)+PI
            ELSE
	       IF(AI.GE.0.)THEN
                 TI(I,J)=PI/2.
               ELSE
                 TI(I,J)=3.*PI/2.
               ENDIF
            ENDIF
            IF(TI(I,J).LT.0.)TI(I,J)=TI(I,J)+2.*PI
60	  CONTINUE
	ENDIF

        RETURN
        END
C********************************************************************
C FFT 2D routine for double precision arrays
C (NB:it is faster in FFT_NAG) 
C
C Input:
C TR, TI:  real part and imaginary part  (input/output) of the image
C KOD: flag for direct(=1) or inverse(=-1) FFT:
C
C********************************************************************
	SUBROUTINE FFT_2D_FOURN_DBLE(TR,TI,NX,NY,IDIM,KOD)
	INTEGER*4 NX,NY,KOD,M,NF
	REAL*8 TR(IDIM,*),TI(IDIM,*)
 
C Check the size (should be a power of 2 : NX=2**M and NY=NX)
C M = ln(NX)/ln(2)
	M=0
	NF=1
1	M=M+1
	NF=NF+NF
	IF(NF.LT.NX) GOTO 1
C        PRINT *,'M,NX,NF',M,NX,NF
 
	IF((NF.NE.NX).OR.(NX.NE.NY))THEN
	   WRITE(6,11) NX,NY
11	   FORMAT(' FFT_2D_DOUBLE/Fatal error: Wrong size',
     1            ' for the input image',/,
     1	' (should be square and power of two)',/,' NX, NY :',I5,1X,I5)
           STOP
	ENDIF
 
C Actual FFT now:
         CALL FOURN2_R8(TR,TI,NX,NY,NX,KOD)

        RETURN
        END
 
C******************************************************************
C FOURN2 routine
C Input:
C TR, TI:  real part and imaginary part  (input/output) of the image
C
C ISIGN=1 direct
C ISIGN=-1 inverse 
C********************************************************************
        SUBROUTINE FOURN2(TR,TI,NX,NY,IDIM,ISIGN)
	REAL*4 TR(IDIM,*),TI(IDIM,*)
        REAL DATA(1024*1024*2)
        INTEGER NN(2),ISIGN

C Check size:
        IF(NX.GT.1024.OR.NY.GT.1024)THEN
         WRITE(6,*) ' FOURN2/Fatal error: maximum size',
     1              ' allowed is (1024 x 1024)'
         STOP
        ENDIF

C Rearranging the data for FOURN1
C (real and complex value ordered in a standard fortran array...)
        K=1
        DO J=1,NY
          DO I=1,NX
          DATA(K)=TR(I,J)
          DATA(K+1)=TI(I,J)
          K = K+2
          END DO
        END DO

C Actual FFT:
        NN(1)=NX
        NN(2)=NY
        CALL FOURN1(DATA,NN,2,ISIGN)

C Rearranging the data
        K=1
        DO J=1,NY
          DO I=1,NX
          TR(I,J)=DATA(K)
          TI(I,J)=DATA(K+1)
          K = K+2
          END DO
        END DO

        RETURN
        END
C******************************************************************
C FOURN2_R8 routine
C Input:
C TR, TI:  real part and imaginary part  (input/output) of the image
C
C ISIGN=1 direct
C ISIGN=-1 inverse 
C********************************************************************
        SUBROUTINE FOURN2_R8(TR,TI,NX,NY,IDIM,ISIGN)
        INTEGER*4 I,J,NX,NY,IDIM,NN(2),ISIGN
	REAL*8 TR(IDIM,*),TI(IDIM,*)
        REAL*4 DATA(1024*1024*2)

C Check size:
        IF(NX.GT.512.OR.NY.GT.512)THEN
         WRITE(6,*) ' FOURN2_R8/Fatal error: maximum size',
     1              ' allowed is 1024x1024'
         STOP
        ENDIF

C Rearranging the data for FOURN1
C (real and complex value ordered in a standard fortran array...)
        K=1
        DO J=1,NY
          DO I=1,NX
          DATA(K)=TR(I,J)
          DATA(K+1)=TI(I,J)
          K = K+2
          END DO
        END DO

C Actual FFT:
        NN(1)=NX
        NN(2)=NY
        CALL FOURN1(DATA,NN,2,ISIGN)

C Rearranging the data
        K=1
        DO J=1,NY
          DO I=1,NX
          TR(I,J)=DATA(K)
          TI(I,J)=DATA(K+1)
          K = K+2
          END DO
        END DO

        RETURN
        END
C********************************************************************
C FFT 1D routine
C Input:
C TR, TI:  real part and imaginary part  (input/output) of the image
C
C********************************************************************
	SUBROUTINE FFT_1D_FOURN(RE,IM,NX,NY,IDIM,KOD)
        INTEGER NX,NY,IDIM,NF,M,I,J,KOD
        REAL RE(IDIM,*),IM(IDIM,*)
	REAL TR(512),TI(512),XNORM

C Check the size (should be a power of 2 : NY=2**M)
C M = ln(NY)/ln(2)
        M=0
        NF=1
1       M=M+1
        NF=NF+NF
        IF(NF.LT.NY) GOTO 1
        PRINT *,'FFT1D/M,NY,NF',M,NY,NF
        IF(NF.NE.NY.OR.NY.GT.512)THEN
          WRITE(6,8)
8         FORMAT('FFT_1D_FOURN/ Fatal error: NY not a power of two',
     1           ' or larger than 512')
        ENDIF
        XNORM=SQRT(REAL(NY))
C Main loop on all the columns:
        DO I=1,NX
C Transfer to temporary array:
          DO J=1,NY
            TR(J)=RE(I,J)
            TI(J)=IM(I,J)
          END DO
          CALL FFT_1D_MONODIM(TR,TI,NY,M)
C Transfer from temporary array and normalization:
          DO J=1,NY
            RE(I,J)=TR(J)/XNORM
            IM(I,J)=TI(J)/XNORM
          END DO
        END DO
        RETURN
        END
C********************************************************************
	SUBROUTINE FFT_1D_MONODIM(TR,TI,NX,M)
	REAL TR(512),TI(512)
	INTEGER NX,NS2,NM1,I,J,K
        INTEGER M,L1,LE
        REAL AR,AI,PI,ANG,CST
        REAL BR,BI,UA,UI,UR,WR,WI
	PI=3.14159265358979323846
 
C Sorting the input array:
	NS2=NX/2
	NM1=NX-1
	J=1
	DO 10 I=1,NM1
C Swap I and J:
	  IF(I.LT.J) THEN
	    AR=TR(J)
	    AI=TI(J)
	    TR(J)=TR(I)
	    TI(J)=TI(I)
	    TR(I)=AR
	    TI(I)=AI
	  ENDIF
	  K=NS2
6	IF(K.GE.J) GOTO7
	  J=J-K
	  K=K/2
	  GOTO 6
7	J=J+K
10	CONTINUE
C
C Computing the FFT:
	LE=1
	ANG=PI
	CST=0.5
	DO 40 K=1,M
	  L1=LE
	  LE=LE*2
	  UR=1.0
	  UI=0.0
	  WR=COS(ANG)
	  WI=SIN(ANG)
	  ANG=CST*ANG
C
	  DO 30 J=1,L1
	    DO 20 I=J,NX,LE
	     BR=TR(I)
	     BI=TI(I)
	     IP=I+L1
	     AR=TR(IP)*UR-TI(IP)*UI
	     AI=TR(IP)*UI-TI(IP)*UR
	     TR(IP)=BR-AR
	     TI(IP)=BI-AI
	     TR(I)=BR+AR
	     TI(I)=BI+AI
20	 CONTINUE
	UA=UR*WR-UI*WI
	UI=UR*WI+UI*WR
	UR=UA
30	CONTINUE
40	CONTINUE
 
C        PRINT *,'M',M
C        PRINT *,'L1,NM1,NX,CST,NS2',L1,NM1,NX,CST,NS2
	RETURN
	END
C**********************************************************************
C++*******************************************************************
C FOURN1
C From "Numerical Recipees" p 451
C
C Replaces DATA by its NDIM-dimensional disccrete Fourier transform, if
C SIGN is input as 1.
C NN is an integer array of length NDIM, containing the lengths of each
C dimension (number of complex values), which MUST all be powers of 2.
C DATA is a real array of length twice the product of these lengths, in
C which the data are stored as in a multidimensional complex Fortran array.
C
C If ISIGN is input as -1, DATA is replaced by its inverse transform
C
C Warning: This routine divides by NX*NY the inverse transform.
C
C JLP
C Version 03-08-2008
C--*********************************************************************
	subroutine FOURN1(data,nn,ndim,isign)
 
C Double precision for trigonometric recurrences:
	real*8 wr,wi,wpr,wpi,wtemp,theta
 
	integer*4 ndim,isign,ntot,nprev,n,i1,i2,i3,i2rev,i3rev
	integer*4 ibit,ifp1,ifp2,ip1,ip2,k1,k2,idim,nrem
	integer*4 nn(*)
	real*4 data(*),tempr,tempi
 
C Compute total number of complex values:
	ntot=1
	do 11 idim=1,ndim
	  ntot=ntot*nn(idim)
11	continue
 
	nprev=1
 
C Main loop over the dimensions:
	do 18 idim=1,ndim
	  n=nn(idim)
	  nrem=ntot/(n*nprev)
	  ip1=2*nprev
	  ip2=ip1*n
	  ip3=ip2*nrem
	  i2rev=1
 
C This is the bit reversal routine
C (to set up the book keeping for the next step):
	  do 14 i2=1,ip2,ip1
	      if(i2.lt.i2rev)then
	         do 13 i1=i2,i2+ip1-2,2
	            do 12 i3=i1,ip3,ip2
	               i3rev=i2rev+i3-i2
C Swap real array  (i3 <-> i3rev) and imaginary array (i3+1 <-> i3rev+1)
	               tempr=data(i3)
	               tempi=data(i3+1)
	               data(i3)=data(i3rev)
	               data(i3+1)=data(i3rev+1)
	               data(i3rev)=tempr
	               data(i3rev+1)=tempi
12	            continue
13	         continue
              endif
              ibit=ip2/2
1	      if((ibit.ge.ip1).and.(i2rev.gt.ibit))then
	          i2rev=i2rev-ibit
	          ibit=ibit/2
	      go to 1
	      endif
	      i2rev=i2rev+ibit
14	   continue
 
C Here begins the Danielson-Lanczos section of the routine:
C (i.e. iterative processing of the data)
	ifp1=ip1
2	if(ifp1.lt.ip2)then
	  ifp2=2*ifp1
C Initialize for the trig. recurrence:
C JLP2008: there was a minus sign here, which is wrong according
C to Num. recipees in C p 524
C (This caused a central symmetry of the transform until August 2008...)
	  theta=6.28318530717959*float(isign)/(ifp2/ip1)
	  wpr=-2.d0*dsin(0.5d0*theta)**2
	  wpi=dsin(theta)
	  wr=1.d0
	  wi=0.d0
	  do 17 i3=1,ifp1,ip1
	    do 16 i1=i3,i3+ip1-2,2
	      do 15 i2=i1,ip3,ifp2
	        k1=i2
	        k2=k1+ifp1
C Danielson-Lanczos formula:
	        tempr=sngl(wr)*data(k2)-sngl(wi)*data(k2+1)
	        tempi=sngl(wr)*data(k2+1)+sngl(wi)*data(k2)
	        data(k2)=data(k1)-tempr
	        data(k2+1)=data(k1+1)-tempi
	        data(k1)=data(k1)+tempr
	        data(k1+1)=data(k1+1)+tempi
15	      continue
16	    continue
 
C Trigonometric recurrence:
	    wtemp=wr
	    wr=wr*wpr-wi*wpi+wr
	    wi=wi*wpr+wtemp*wpi+wi
17	    continue
	  ifp1=ifp2
	  go to 2
	  endif
	  nprev=n*nprev
18	continue

C Divides inverse transform by the total size of the array:
	 if(isign.le.0)then
	 stot=float(ntot)
	 do 46 k=1,2*ntot
	   data(k)=data(k)/stot
46	 continue
         endif

	return
	end
C*********************************************************************
	SUBROUTINE RECENTRE(INPUT,OUTPUT,NX,NY,IDIM)
	REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
	INTEGER*4 NX,NY,NNX,NNY
 
	NNX=2*(NX/2)
	NNY=2*(NY/2)
 
	IF((NX.NE.NNX).OR.(NY.NE.NNY))THEN
	  PRINT *,' RECENTRE/Fatal error: NX or NY not even numbers'
          PRINT *,' NX, NY =',NX,NY
	  STOP
	ENDIF
 
C Uses a buffer to allow for the same array in input and output:
	 DO J=1,NY/2
	   DO I=1,NX/2
	    X1=INPUT(I,J)
	    X2=INPUT(I+NX/2,J+NY/2)
	    X3=INPUT(I+NX/2,J)
	    X4=INPUT(I,J+NY/2)
C
	    OUTPUT(I+NX/2,J+NY/2)=X1
	    OUTPUT(I,J)=X2
	    OUTPUT(I,J+NY/2)=X3
	    OUTPUT(I+NX/2,J)=X4
	   END DO
	 END DO
       	 RETURN
	 END
