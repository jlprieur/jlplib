C++***************************************************************
C Program to compute FFT_1D
C Calling FFT1 (either in "fft_nag.for" or "fft_jlp.for") 
C WARNING: NAG FFT divides the result by SQRT(NX)
C
C JLP
C Version of 26-09-95
C--***************************************************************
	PROGRAM FFT1
        INTEGER*4 PNTR_RE, PNTR_IM, MADRID(1)
     	INTEGER KOD
	CHARACTER NAME*40,COMMENTS*80,ANS*1
        COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
 
C Inquire the format (input/output) :
	CALL JLP_INQUIFMT
 
C Option:
19	WRITE(6,15)
15	FORMAT(' Options: ',/,
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
        IF(KOD.EQ.-1)THEN
	  CALL RECENT_FFT_1D_Y_FLOAT(MADRID(PNTR_RE),MADRID(PNTR_RE),NX,NY,NX)
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
 
	IF(ANS.NE.'Y'.AND.ANS.NE.'y')THEN
	  READ(5,10) NAME
	  CALL JLP_VM_READIMAG(PNTR_IM,NX1,NY1,NAME,COMMENTS)
          IF(KOD.EQ.-1)THEN
	  CALL RECENT_FFT_1D_Y_FLOAT(MADRID(PNTR_IM),MADRID(PNTR_IM),
     1                               NX1,NY1,NX1)
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
          CALL ZERO(MADRID(PNTR_IM),NX,NY)
	ENDIF
 
C FFT:
	WRITE(6,*) ' Start FFT'
	CALL FFT_1D(MADRID(PNTR_RE),MADRID(PNTR_IM),NX,NY,NX,KOD)
	WRITE(6,*) ' FFT OK'
 
C Output :
	WRITE(6,*)' Real part / Power spect.:'
	READ(5,10) NAME
	CALL RECENT_FFT_1D_Y_FLOAT(MADRID(PNTR_RE),MADRID(PNTR_RE),NX,NY,NX)
	CALL JLP_WRITEIMAG(MADRID(PNTR_RE),NX,NY,NX,NAME,COMMENTS)
 
	WRITE(6,*)' Imaginary part / Phase:'
	READ(5,10) NAME
	CALL RECENT_FFT_1D_Y_FLOAT(MADRID(PNTR_IM),MADRID(PNTR_IM),NX,NY,NX)
	CALL JLP_WRITEIMAG(MADRID(PNTR_IM),NX,NY,NX,NAME,COMMENTS)
 
999	CALL JLP_END
	STOP
	END
C****************************************************************
        SUBROUTINE ZERO(ARRAY,NX,NY)
        REAL ARRAY(NX,NY)
	  DO J=1,NY
	   DO I=1,NX
	    ARRAY(I,J)=0.
	   ENDDO
	  ENDDO
        RETURN
        END
C****************************************************************
C	include 'fft:fft_nag.for'
C	include 'fft:fft_jlp.for'
