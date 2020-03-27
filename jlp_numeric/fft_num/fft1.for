C++***************************************************************
C Program to compute FFT_1D
C Calling FFT1 (either in "fft_nag.for" or "fft_jlp.for") 
C Normalisation of the FFT by SQRT(NY)
C
C 28/01/2000: not working with rectangular arrays with FFTW
C             but working with fft1_nag!
C
C JLP
C Version of 21-01-00
C--***************************************************************
	PROGRAM FFT1
        INTEGER*4 PNTR_RE, PNTR_IM, MADRID(1)
     	INTEGER*4 KOD, IOPT, ISIZE, NX, NY, NX1, NY1
	CHARACTER NAME*40, COMMENTS*80, ANS*1
        COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
 
C Inquire the format (input/output) :
	CALL JLP_INQUIFMT
 
C Option:
19	WRITE(6,15)
15	FORMAT(/,'FFT1, program to compute FFT along the columns',/,
     1  ' Options: ',/,
     1	' 1=direct FFT (in/out: real,imaginary)',/,
     1	' 2=direct FFT (in: real,ima /out: modulus,phase)',/,
     1	' 3=direct FFT (in: real,ima /out: square modulus,phase)',/,
     1	' -1=inverse FFT (in/out: real,imaginary)',/,
     1	' -2=inverse FFT (in: modulus,phase /out: real,ima)',/,
     1	' Enter your choice:')
	READ(5,*) IOPT 
	IF((IABS(IOPT).NE.1).AND.(IABS(IOPT).NE.2).AND.IOPT.NE.3)THEN
	   WRITE(6,17)
17	   FORMAT(' Error: enter again the option')
	   GOTO 19
	ENDIF
        IF(IOPT.LT.0)THEN
          KOD=-1
        ELSE
          KOD=1
        ENDIF
 
C Input :
	WRITE(6,*)' Input image (real part):'
	READ(5,10) NAME
	CALL JLP_VM_READIMAG(PNTR_RE,NX,NY,NAME,COMMENTS)
        IF(KOD.EQ.-1)THEN
	  CALL RECENT_FFT_1D_Y_FLOAT(MADRID(PNTR_RE),MADRID(PNTR_RE),
     1                               NX,NY,NX)
        ENDIF
 
C Imaginary part:
	  IF(IOPT.NE.-2) THEN
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
 
	IF(ANS.NE.'Y'.AND.ANS.NE.'y')THEN
	  READ(5,10) NAME
	  CALL JLP_VM_READIMAG(PNTR_IM,NX1,NY1,NAME,COMMENTS)
          IF(IOPT.EQ.-1)THEN
	    CALL RECENT_FFT_1D_Y_FLOAT(MADRID(PNTR_IM),MADRID(PNTR_IM),
     1                                 NX1,NY1,NX1)
          ENDIF
C Check the size:
	  IF((NX1.NE.NX).OR.(NY1.NE.NY))THEN
	    WRITE(6,21)
21	   FORMAT(' FATAL ERROR: Wrong size for the input images',/,
     1	' (should be the same for the real/imaginary parts)')
	    GOTO 999
	  ENDIF
	ELSE
          CALL JLP_GETVM(PNTR_IM,NX*NY*4)
          CALL ZERO2(MADRID(PNTR_IM),NX,NY,NX)
	ENDIF
 
C FFT:
        WRITE(6,*) ' Start FFT (FFTW version) along the columns'
        CALL FFT_1D_Y_FLOAT(MADRID(PNTR_RE),MADRID(PNTR_IM),NX,NY,NX,KOD)
C	WRITE(6,*) ' Start FFT (NAG version) along the columns'
C	CALL FFT_1D_Y_NAG(MADRID(PNTR_RE),MADRID(PNTR_IM),NX,NY,NX,KOD)
	WRITE(6,*) ' FFT OK'
 
C Conversion to power/phase:
        IF(IOPT.EQ.2)THEN
         CALL TO_MOD_PHA(MADRID(PNTR_RE),MADRID(PNTR_IM),NX,NY,NX)
        ELSEIF(IOPT.EQ.3)THEN
         CALL TO_SQMOD_PHA(MADRID(PNTR_RE),MADRID(PNTR_IM),NX,NY,NX)
        ENDIF
C Output :
	WRITE(6,*)' Real part / Power spect.:'
	READ(5,10) NAME
C Always recenter the output: if it is in direct space a null phase
C would break up the object in two upon the bottom and upper edges.
	CALL RECENT_FFT_1D_Y_FLOAT(MADRID(PNTR_RE),MADRID(PNTR_RE),
     1                             NX,NY,NX)
	CALL JLP_WRITEIMAG(MADRID(PNTR_RE),NX,NY,NX,NAME,COMMENTS)
 
	WRITE(6,*)' Imaginary part / Phase:'
	READ(5,10) NAME
	CALL RECENT_FFT_1D_Y_FLOAT(MADRID(PNTR_IM),MADRID(PNTR_IM),
     1                             NX,NY,NX)
	CALL JLP_WRITEIMAG(MADRID(PNTR_IM),NX,NY,NX,NAME,COMMENTS)
 
999	CALL JLP_END
	STOP
	END
C****************************************************************
C Only for debugging: 
	SUBROUTINE RECENT_FFT_1D_Y_FLOA(R1,R2,NX,NY,IDIM)
        INTEGER NX,NY,IDIM
        REAL R1(IDIM,*),R2(IDIM,*)
        RETURN
        END
C****************************************************************
        SUBROUTINE ZERO2(ARRAY,NX,NY,IDIM)
        INTEGER NX,NY,I,J,IDIM
        REAL ARRAY(IDIM,*)
	  DO J=1,NY
	   DO I=1,NX
	    ARRAY(I,J)=0.
	   ENDDO
	  ENDDO
        RETURN
        END
C****************************************************************
        SUBROUTINE ZERO1(ARRAY,NX)
        INTEGER NX,I
        REAL ARRAY(NX)
	   DO I=1,NX
	    ARRAY(I)=0.
	   ENDDO
        RETURN
        END
C****************************************************************
C Copy column IX from (2D) ARRAY2 to (1D) ARRAY1
C****************************************************************
        SUBROUTINE COPY21(ARRAY2,ARRAY1,NX,NY,IX)
        REAL ARRAY2(NX,NY),ARRAY1(NY)
	  DO J=1,NY
	    ARRAY1(J)=ARRAY2(IX,J)
	  ENDDO
        RETURN
        END
C****************************************************************
C Copy (1D) ARRAY1 to column IX of (2D) ARRAY2
C****************************************************************
        SUBROUTINE COPY12(ARRAY2,ARRAY1,NX,NY,IX)
        REAL ARRAY2(NX,NY),ARRAY1(NY)
	  DO J=1,NY
	    ARRAY2(IX,J)=ARRAY1(IX)
	  ENDDO
        RETURN
        END
C****************************************************************
C Modulus and phase:
        SUBROUTINE TO_MOD_PHA(TR,TI,NX,NY,IDIM)
        INTEGER I,J,NX,NY,IDIM
        REAL TR(IDIM,*),TI(IDIM,*)
        REAL AR,AI,PI
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
C Square modulus and phase:
        SUBROUTINE TO_SQMOD_PHA(TR,TI,NX,NY,IDIM)
        INTEGER I,J,NX,NY,IDIM
        REAL TR(IDIM,*),TI(IDIM,*)
C
        CALL TO_MOD_PHA(TR,TI,NX,NY,IDIM)
C
         DO 60 J=1,NY
          DO 60 I=1,NX
            TR(I,J)=TR(I,J)*TR(I,J)
60       CONTINUE

        RETURN
        END
C****************************************************************
C To use NAG
C        include 'fft:fft1_nag.for'
C To use a routine derived from Numerical Recipees:
C       include 'fft:fft_jlp.for'
C To use "FFTW", leave this without any inclusion (in "$JLPLIB/jlp/jlputil.a")
