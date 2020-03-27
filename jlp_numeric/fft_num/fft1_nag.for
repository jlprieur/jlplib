C++***************************************************************
C Subset of FFT routines modified for fft1.for 
C Using NAG library 
C (Extracted from fft_nag.for to be compiled with fft1.for)
C
C JLP
C Version of 06-09-95
C--***************************************************************
C********************************************************************
C FFT 1D routine along the columns
C Input:
C TR, TI:  real part and imaginary part  (input/output) of the image
C
C KOD=1, or 2 direct
C KOD=-1, or -2 inverse
C KOD=-2, or 2: power spectrum, and phase
C********************************************************************
	SUBROUTINE FFT_1D_Y_NAG(TR,TI,NX,NY,IDIM1,KOD)
	PARAMETER (IDIM=600)
	REAL TR(IDIM1,*),TI(IDIM1,*)
	DOUBLE PRECISION DR(IDIM*IDIM),DI(IDIM*IDIM),WORK(2000)
	REAL F_NORM
	INTEGER NX,NY,KOD
	PI=3.14159265358979323846
 
C Main loop on I=1,NX
	DO I=1,NX

C Conversion of the input (when KOD=-2)
	II=0
	IF(KOD.EQ.-2) THEN
	   DO J=1,NY
	    XM=TR(I,J)
	    XP=TI(I,J)
	    II=II+1
	    DR(II)=XM*COS(XP)
	    DI(II)=XM*SIN(XP)
	   ENDDO
        ELSE
	  DO J=1,NY
	    II=II+1
	    DR(II)=TR(I,J)
	    DI(II)=TI(I,J)
	  END DO
	ENDIF	
 
C Compute the FFT:
	  IFAIL=0
C WARNING: NAG FFT divides the result by SQRT(NX*NY)
	  IF(KOD.GT.0)THEN
	    CALL C06FCF(DR,DI,NY,WORK,IFAIL)
	  ELSE
	    CALL C06GCF(DI,NY,IFAIL)
	    CALL C06FCF(DR,DI,NY,WORK,IFAIL)
	    CALL C06GCF(DI,NY,IFAIL)
	  ENDIF
 
C Copy back the arrays 
C Division by sqrt(ntot) 
C	F_NORM=1./SQRT(FLOAT(NY))
C JLP2000: NAG already divides by ny:
	F_NORM=1.

	II=0
	  DO J=1,NY
	    II=II+1
	    TR(I,J)=DR(II)*F_NORM
	    TI(I,J)=DI(II)*F_NORM
	  END DO
 
	  IF(KOD.EQ.2)THEN
C Power and phase spectrum in output when KOD=2:
	    DO J=1,NY
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

C End of loop on I=1,NX: 
	ENDDO
	
	RETURN
	END
