C++***************************************************************
C Program to compute FFT_1D
C Calling FFT1 (either in "fft_nag.for" or "fft_jlp.for") 
C WARNING: NAG FFT divides the result by SQRT(NX)
C
C JLP
C Version of 21-01-00
C Unknown problem at the end...
C--***************************************************************
	PROGRAM FFT1
        INTEGER*4 PNTR_RE, PNTR_IM, PNTR_TR, PNTR_TI, MADRID(1)
     	INTEGER*4 KOD,POWER_PHASE, ISIZE
	CHARACTER NAME*40,COMMENTS*80,ANS*1
        COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
 
C Inquire the format (input/output) :
	CALL JLP_INQUIFMT
 
C Option:
        WRITE(6,*) ' Version Jan. 2000: unknown problem at the end!'
19	WRITE(6,15)
15	FORMAT(/,'FFT1, program to compute FFT along the columns',/,
     1  ' Options: ',/,
     1	' 1=direct FFT (in/out: real,imaginary)',/,
     1	' 2=direct FFT (in: real,ima /out: power,phase)',/,
     1	' -1=inverse FFT (in/out: real,imaginary)',/,
     1	' -2=inverse FFT (in: power,phase /out: real,ima)',/,
     1	' Enter your choice:')
	READ(5,*) KOD
	IF((IABS(KOD).NE.1).AND.(IABS(KOD).NE.2))THEN
	   WRITE(6,17)
17	   FORMAT(' Error: enter again the option')
	   GOTO 19
	ENDIF
 
C Input :
	WRITE(6,*)' Input image (real part):'
	READ(5,10) NAME
	CALL JLP_VM_READIMAG(PNTR_RE,NX,NY,NAME,COMMENTS)
        ISIZE=NX*NY*8
        CALL JLP_GETVM(PNTR_TR,ISIZE)
        CALL TO_DOUBLE(MADRID(PNTR_RE),MADRID(PNTR_TR),NX,NY,NX)
        IF(KOD.EQ.-1)THEN
	  CALL RECENT_FFT_1D_Y(MADRID(PNTR_TR),MADRID(PNTR_TR),NX,NY,NX)
        ENDIF
 
C Imaginary part:
	  IF(KOD.NE.-2) THEN
	    WRITE(6,22)
22	  FORMAT(' Input image (imaginary part):',/,
     1	' Null imaginary part? (N) ?')
	  ELSE
	    WRITE(6,23)
23	  FORMAT(' Input image (phase):',/,
     1	' Null phase? (N) ?')
	  ENDIF
C
	  READ(5,10) ANS
10	  FORMAT(A)
 
        ISIZE=NX*NY*8
        CALL JLP_GETVM(PNTR_TI,ISIZE)
	IF(ANS.NE.'Y'.AND.ANS.NE.'y')THEN
	  READ(5,10) NAME
	  CALL JLP_VM_READIMAG(PNTR_IM,NX1,NY1,NAME,COMMENTS)
          CALL TO_DOUBLE(MADRID(PNTR_IM),MADRID(PNTR_TI),NX,NY,NX)
          IF(KOD.EQ.-1)THEN
	    CALL RECENT_FFT_1D_Y(MADRID(PNTR_TI),MADRID(PNTR_TI),NX1,NY1,NX1)
          ENDIF
C Check the size:
	  IF((NX1.NE.NX).OR.(NY1.NE.NY))THEN
	    WRITE(6,21)
21	   FORMAT(' FATAL ERROR: Wrong size for the input images',/,
     1	' (should be the same for the real/imaginary parts)')
	    GOTO 999
	  ENDIF
	ELSE
          CALL ZERO2(MADRID(PNTR_TI),NX,NY)
	ENDIF
 
        IF(KOD.EQ.2)THEN
          KOD=1
          POWER_PHASE=1
        ELSE
          POWER_PHASE=0
        ENDIF
C FFT:
	WRITE(6,*) ' Start FFT along the columns'
	CALL FFT_1D_Y(MADRID(PNTR_TR),MADRID(PNTR_TI),NX,NY,NX,KOD)
	WRITE(6,*) ' FFT OK'
 
C Conversion to power/phase:
        IF(POWER_PHASE.EQ.1)THEN
         CALL CONVERT_TO_POWER(MADRID(PNTR_TR),MADRID(PNTR_TI),NX,NY,NX)
        ENDIF
C Output :
	WRITE(6,*)' Real part / Power spect.:'
	READ(5,10) NAME
	CALL RECENT_FFT_1D_Y(MADRID(PNTR_TR),MADRID(PNTR_TR),NX,NY,NX)
        CALL TO_REAL(MADRID(PNTR_TR),MADRID(PNTR_RE),NX,NY,NX)
        CALL JLP_FREEVM(PNTR_TR)
	CALL JLP_WRITEIMAG(MADRID(PNTR_RE),NX,NY,NX,NAME,COMMENTS)
 
	WRITE(6,*)' Imaginary part / Phase:'
	READ(5,10) NAME
	CALL RECENT_FFT_1D_Y(MADRID(PNTR_TI),MADRID(PNTR_TI),NX,NY,NX)
        CALL TO_REAL(MADRID(PNTR_TI),MADRID(PNTR_IM),NX,NY,NX)
        CALL JLP_FREEVM(PNTR_TI)
	CALL JLP_WRITEIMAG(MADRID(PNTR_IM),NX,NY,NX,NAME,COMMENTS)
 
999	CALL JLP_END
	STOP
	END
C****************************************************************
        SUBROUTINE ZERO2(ARRAY,NX,NY)
        DOUBLE PRECISION ARRAY(NX,NY)
	  DO J=1,NY
	   DO I=1,NX
	    ARRAY(I,J)=0.D0
	   ENDDO
	  ENDDO
        RETURN
        END
C****************************************************************
        SUBROUTINE ZERO1(ARRAY,NX)
        DOUBLE PRECISION ARRAY(NX)
	   DO I=1,NX
	    ARRAY(I)=0.D0
	   ENDDO
        RETURN
        END
C****************************************************************
C Copy column IX from (2D) ARRAY2 to (1D) ARRAY1
C****************************************************************
        SUBROUTINE COPY21(ARRAY2,ARRAY1,NX,NY,IX)
        DOUBLE PRECISION ARRAY2(NX,NY),ARRAY1(NY)
	  DO J=1,NY
	    ARRAY1(J)=ARRAY2(IX,J)
	  ENDDO
        RETURN
        END
C****************************************************************
C Copy (1D) ARRAY1 to column IX of (2D) ARRAY2
C****************************************************************
        SUBROUTINE COPY12(ARRAY2,ARRAY1,NX,NY,IX)
        DOUBLE PRECISION ARRAY2(NX,NY),ARRAY1(NY)
	  DO J=1,NY
	    ARRAY2(IX,J)=ARRAY1(IX)
	  ENDDO
        RETURN
        END
C****************************************************************
C Copy (REAL) ARRAY1 to (DOUBLE) ARRAY2
C****************************************************************
        SUBROUTINE TO_DOUBLE(ARRAY1,ARRAY2,NX,NY,IDIM)
        INTEGER*4 IDIM,NX,NY,I,J
        REAL ARRAY1(IDIM,*)
        DOUBLE PRECISION ARRAY2(IDIM,*)
	  DO J=1,NY
	  DO I=1,NX
	    ARRAY2(I,J)=ARRAY1(I,J)
	  ENDDO
	  ENDDO
        RETURN
        END
C****************************************************************
C Copy (DOUBLE) ARRAY1 to (REAL) ARRAY2
C****************************************************************
        SUBROUTINE TO_REAL(ARRAY1,ARRAY2,NX,NY,IDIM)
        INTEGER*4 IDIM,NX,NY,I,J
        DOUBLE PRECISION ARRAY1(IDIM,*)
        REAL ARRAY2(IDIM,*)
          WRITE(6,*) 'NX=',NX,'NY=',NY,'IDIM=',IDIM
	  DO J=1,NY
           WRITE(6,*) 'J=',J
	  DO I=1,NX
	    ARRAY2(I,J)=ARRAY1(I,J)
	  ENDDO
	  ENDDO
        RETURN
        END
C****************************************************************
C Power and phase spectrum:
        SUBROUTINE CONVERT_TO_POWER(TR,TI,NX,NY,IDIM)
        INTEGER I,J,NX,NY,IDIM
        DOUBLE PRECISION TR(IDIM,*),TI(IDIM,*)
        DOUBLE PRECISION AR,AI,PI
        PI=3.14159
C
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
60        CONTINUE
        RETURN
        END
C****************************************************************
C	include 'hrsa:fft_nag.for'
C	include 'hrsa:fft_jlp.for'
