C++***************************************************************
C Set of FFT routines 
C Using NAG library 
C Contains: FFT_2D, FFT_2D_FAST, FFT_1D_FAST, RECENTRE, RECENTRE_1D
C
C Compatible with fft_jlp 
C (except that fft_jlp doesn''t include FFT_1D and RECENTRE_1D yet)
C
C JLP
C Version of 06-09-95
C--***************************************************************
	SUBROUTINE FOURN1
	PRINT *,' FATAL ERROR: Dummy FOURN1 routine'
	PRINT *,' Sorry bad package selected'
	STOP
	END
C********************************************************************
C FFT 2D routine
C Input:
C TR, TI:  real part and imaginary part  (input/output) of the image
C
C KOD=1, or 2 direct
C KOD=-1, or -2 inverse
C KOD=-2, or 2: power spectrum, and phase
C********************************************************************
	SUBROUTINE FFT_2D(TR,TI,NX,NY,IDIM1,KOD)
	PARAMETER (IDIM=600)
	REAL TR(IDIM1,*),TI(IDIM1,*)
	DOUBLE PRECISION DR(IDIM*IDIM),DI(IDIM*IDIM),WORK(2000)
	REAL F_NORM
	INTEGER NX,NY,KOD
	INTEGER LWORK,ND(2),NDIM
	PI=3.14159265358979323846
 
C Conversion of the input (when KOD=-2)
	IF(KOD.EQ.-2) THEN
	  DO J=1,NY
	   DO I=1,NX
	    XM=TR(I,J)
	    XP=TI(I,J)
	    TR(I,J)=XM*COS(XP)
	    TI(I,J)=XM*SIN(XP)
	   ENDDO
	  ENDDO
	ENDIF	
 
C Copy the arrays 
	II=0
	DO J=1,NY
	  DO I=1,NX
	    II=II+1
	    DR(II)=TR(I,J)
	    DI(II)=TI(I,J)
	  END DO
	END DO
 
C Compute the FFT:
	NDIM=2
	ND(1)=NX
	ND(2)=NY
	LWORK=2000
	NN=NX*NY
	IFAIL=0
C WARNING: NAG FFT divides the result by SQRT(NX*NY)
	IF(KOD.GT.0)THEN
	   CALL C06FJF(NDIM,ND,NN,DR,DI,WORK,LWORK,IFAIL)
	ELSE
	   CALL C06GCF(DI,NN,IFAIL)
	   CALL C06FJF(NDIM,ND,NN,DR,DI,WORK,LWORK,IFAIL)
	   CALL C06GCF(DI,NN,IFAIL)
	ENDIF
 
C Copy back the arrays :
C and divides/multiplies by sqrt(ntot) since NAG divides by sqrt(ntot):
	IF(KOD.GT.0) THEN
	  F_NORM=SQRT(FLOAT(NX*NY))
	ELSE
	  F_NORM=1./SQRT(FLOAT(NX*NY))
	ENDIF

	II=0
	DO J=1,NY
	  DO I=1,NX
	    II=II+1
	    TR(I,J)=DR(II)*F_NORM
	    TI(I,J)=DI(II)*F_NORM
	  END DO
	END DO
 
	IF(KOD.EQ.2)THEN
C Power and phase spectrum in output when KOD=2:
	  DO J=1,NY
	    DO I=1,NX
	      AR=TR(I,J)
	      AI=TI(I,J)
	      TR(I,J)=SQRT(AR*AR+AI*AI)
	      IF(AR.EQ.0) THEN
	        TI(I,J)=PI/2.
	        IF(AI.LT.0.)TI(I,J)=3.*PI/2.
	      ELSE
	        TI(I,J)=ATAN(AI/AR)
	        IF(AR.LT.0.)TI(I,J)=TI(I,J)+PI
	      ENDIF
              IF(TI(I,J).LT.0.)TI(I,J)=TI(I,J)+2.*PI
	    ENDDO
	  ENDDO
	ENDIF
	
	RETURN
	END
C********************************************************************
C FFT 1D routine along the lines
C Input:
C TR, TI:  real part and imaginary part  (input/output) of the image
C
C KOD=1, or 2 direct
C KOD=-1, or -2 inverse
C KOD=-2, or 2: power spectrum, and phase
C********************************************************************
	SUBROUTINE FFT_1D_X(TR,TI,NX,NY,IDIM1,KOD)
	PARAMETER (IDIM=600)
	REAL TR(IDIM1,*),TI(IDIM1,*)
	DOUBLE PRECISION DR(IDIM*IDIM),DI(IDIM*IDIM),WORK(2000)
	REAL F_NORM
	INTEGER NX,NY,KOD
	PI=3.14159265358979323846
 
C Main loop on J=1,NY
	DO J=1,NY

C Conversion of the input (when KOD=-2)
	II=0
	IF(KOD.EQ.-2) THEN
	   DO I=1,NX
	    XM=TR(I,J)
	    XP=TI(I,J)
	    II=II+1
	    DR(II)=XM*COS(XP)
	    DI(II)=XM*SIN(XP)
	   ENDDO
        ELSE
	  DO I=1,NX
	    II=II+1
	    DR(II)=TR(I,J)
	    DI(II)=TI(I,J)
	  END DO
	ENDIF	
 
C Compute the FFT:
	  IFAIL=0
C WARNING: NAG FFT divides the result by SQRT(NX*NY)
	  IF(KOD.GT.0)THEN
	    CALL C06FCF(DR,DI,NX,WORK,IFAIL)
	  ELSE
	    CALL C06GCF(DI,NX,IFAIL)
	    CALL C06FCF(DR,DI,NX,WORK,IFAIL)
	    CALL C06GCF(DI,NX,IFAIL)
	  ENDIF
 
C Copy back the arrays :
C and divides/multiplies by sqrt(ntot) since NAG divides by sqrt(ntot):
	IF(KOD.GT.0) THEN
	  F_NORM=SQRT(FLOAT(NX))
	ELSE
	  F_NORM=1./SQRT(FLOAT(NX))
	ENDIF

	II=0
	  DO I=1,NX
	    II=II+1
	    TR(I,J)=DR(II)*F_NORM
	    TI(I,J)=DI(II)*F_NORM
	  END DO
 
	  IF(KOD.EQ.2)THEN
C Power and phase spectrum in output when KOD=2:
	    DO I=1,NX
	      AR=TR(I,J)
	      AI=TI(I,J)
	      TR(I,J)=SQRT(AR*AR+AI*AI)
	      IF(AR.EQ.0) THEN
	        TI(I,J)=PI/2.
	        IF(AI.LT.0.)TI(I,J)=3.*PI/2.
	      ELSE
	        TI(I,J)=ATAN(AI/AR)
	        IF(AR.LT.0.)TI(I,J)=TI(I,J)+PI
	      ENDIF
              IF(TI(I,J).LT.0.)TI(I,J)=TI(I,J)+2.*PI
	    ENDDO
	  ENDIF

C End of loop on J=1,NY: 
	ENDDO
	
	RETURN
	END
C*********************************************************************
	SUBROUTINE RECENTRE(INPUT,OUTPUT,NX,NY,IDIM)
	REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
        REAL*4 X1,X2,X3,X4
	INTEGER*4 I,J,NX,NY,NNX,NNY,NX2,NY2
 
	NX2=NX/2
	NY2=NY/2
	NNX=2*NX2
	NNY=2*NY2
 
	IF((NX.NE.NNX).OR.(NY.NE.NNY))THEN
	  PRINT *,' Recentre/Fatal error: NX or NY not even numbers'
	  STOP
	ENDIF
 
C Uses a buffer to allow for the same array in input and output:
       
	 DO J=1,NY2
	   DO I=1,NX2
C Starting from:
C  X4 X2
C  X1 X3
	    X1=INPUT(I,J)
	    X2=INPUT(I+NX2,J+NY2)
	    X3=INPUT(I+NX2,J)
	    X4=INPUT(I,J+NY2)
C Then
C  X3 X1
C  X2 X4
	    OUTPUT(I,J)=X2
	    OUTPUT(I+NX2,J+NY2)=X1
	    OUTPUT(I+NX2,J)=X4
	    OUTPUT(I,J+NY2)=X3
C            IF(I.EQ.1.AND.J.EQ.1)THEN
C                PRINT *,'X1,X2,X3,X4',X1,X2,X3,X4
C                PRINT *,'I1,I2,I3,I4',I,I+NX2,J,J+NY2
C                PRINT *,'OUT: X1,X2,X3,X4',OUTPUT(I,J),OUTPUT(I+NX/2,J+NY2),
C     1          OUTPUT(I+NX2,J),OUTPUT(I,J+NY2)
C            ENDIF
	   END DO
	 END DO

       	 RETURN
	 END
C*********************************************************************
C Used for spectra: 
C*********************************************************************
	SUBROUTINE RECENTRE_1D(INPUT,OUTPUT,NX,NY,IDIM)
	REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
        REAL*4 X1,X2,X3,X4
	INTEGER*4 I,J,NX,NY,NNX,NNY,NX2,NY2
 
	NX2=NX/2
	NNX=2*NX2
 
	IF(NX.NE.NNX)THEN
	  PRINT *,' Recentre/Fatal error: NX is not even'
	  STOP
	ENDIF
 
C Uses a buffer to allow for the same array in input and output:
       
	 DO J=1,NY
	   DO I=1,NX2
C Starting from:
C  X1 X2
	    X1=INPUT(I,J)
	    X2=INPUT(I+NX2,J)
C Then
C  X2 X1
	    OUTPUT(I+NX2,J)=X1
	    OUTPUT(I,J)=X2
	   END DO
	 END DO

       	 RETURN
	 END
C********************************************************************
C FFT_2D_FAST routine
C Input:
C DR, DI:  real part and imaginary part  (input/output) of the image
C          IDIM = NX (always), double precision arrays.
C
C KOD=1, or 2 direct
C KOD=-1, or -2 inverse
C KOD=-2, or 2: power spectrum, and phase
C********************************************************************
	SUBROUTINE FFT_2D_FAST(DR,DI,NX,NY)
	DOUBLE PRECISION DR(*),DI(*),WORK(2000)
	REAL F_NORM
	INTEGER NX,NY
	INTEGER LWORK,ND(2),NDIM

C Compute the FFT:
	NDIM=2
	ND(1)=NX
	ND(2)=NY
	LWORK=2000
	NN=NX*NY
	IFAIL=0
C WARNING: NAG FFT divides the result by SQRT(NX*NY)
	 CALL C06FJF(NDIM,ND,NN,DR,DI,WORK,LWORK,IFAIL)
 
C The user should multiply the output arrays by sqrt(ntot) 
C since NAG divides by sqrt(ntot):
C	  F_NORM=SQRT(FLOAT(NX*NY))
	
	RETURN
	END
C********************************************************************
C FFT_1D_FAST_NEW routine (works with new NAG MARK15...)
C To compute FFT only along the columns.
C Input data: real values. 
C
C Input:
C DR(NX*NY):   input image IDIM = NX (always)
C DI(NX*NY):    work area
C
C Output:
C DR(NX*NY): real part
C DI(NX*NY): imaginary part
C
C TRIG(2*NY)
C********************************************************************
	SUBROUTINE FFT_1D_FAST_NEW(DR,DI,NX,NY,TRIG,FIRST_CALL)
	DOUBLE PRECISION DR(*),DI(*),TRIG(*)
	INTEGER NX,NY,FIRST_CALL

C Compute the FFT:
	IFAIL=0
C WARNING: NAG FFT divides the result by SQRT(NX*NY)
C 'I' for initialization and storage in TRIG(2*NX)
        IF(FIRST_CALL.NE.0)THEN
         CALL C06FPF(NX,NY,DR,'I',TRIG,DI,IFAIL)
        ELSE
C 'S' for subsequent calls: 
         CALL C06FPF(NX,NY,DR,'S',TRIG,DI,IFAIL)
        ENDIF
C Note that output from C06FPF is in hermitian format
C Conversion to full complex form by calling C06GSF:
        CALL C06GSF(NX,NY,DR,DI,IFAIL)
 
C The user should multiply the output arrays by sqrt(ntot) 
C since NAG divides by sqrt(ntot):
C	  F_NORM=SQRT(FLOAT(NX*NY))
	
	RETURN
	END
C********************************************************************
C FFT_1D_FAST routine
C To compute 1-D FFT
C
C Input/Output:
C DR(N): real part
C DI(N): imaginary part
C
C WORK(N)
C********************************************************************
	SUBROUTINE FFT_1D_FAST(DR,DI,NN,WORK)
	DOUBLE PRECISION DR(*),DI(*),WORK(*)
	INTEGER NN

C Compute the FFT:
	IFAIL=0
C WARNING: NAG FFT divides the result by SQRT(NN)
        CALL C06FCF(DR,DI,NN,WORK,IFAIL)

C The user should multiply the output arrays by sqrt(ntot) 
C since NAG divides by sqrt(ntot):
C	  F_NORM=SQRT(FLOAT(NN))
	
	RETURN
	END
