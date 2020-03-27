C++***************************************************************
C Program to compute FFT2
C Calling FFT_2D (in "fft_nag.for", "fft_fourn.for" or fftw_set.c) 
C WARNING: NAG FFT divides the result by SQRT(NX*NY)
C
C JLP
C Version of 08-10-2008
C--***************************************************************
        PROGRAM FFT2
        IMPLICIT NONE
C For 32-bit computers, PNTR should be declared as integer*4:
C        INTEGER*4 PNTR_RE,PNTR_IM
C For 64-bit computers, PNTR should be declared as integer*8:
        INTEGER*8 PNTR_RE,PNTR_IM
        INTEGER*4 KOD,IOPT,NX,NY,NX1,NY1,MADRID(1)
        CHARACTER NAME*60,COMMENTS*80,ANS*1
        COMMON /VMR/MADRID
 
        CALL JLP_BEGIN
 
C Inquire the format (input/output) :
        CALL JLP_INQUIFMT
 
C Option:
        PRINT *,'FFTW CONVENTION WITH +2 i PI for direct FFT...'
        WRITE(6,15)
15        FORMAT('FFT2, Version 08/10/2008',/,' Options: ',/,
     1        ' 1=direct FFT (in/out: real,imaginary)',/,
     1        ' 2=direct FFT (in: real,ima /out: modsq,phase)',/,
     1        ' 3=direct FFT (in: real,ima /out: mod,phase)',/,
     1        ' -1=inverse FFT (in/out: real,imaginary)',/,
     1        ' -2=inverse FFT (in: modsq,phase /out: real,ima)',/,
     1        ' -3=inverse FFT (in: mod,phase /out: real,ima)',/,
     1        ' Enter your choice:')
        READ(5,*) IOPT 
        IF((IOPT.EQ.0).OR.(IOPT.LT.-3)
     1     .OR.(IOPT.GT.3))THEN
           WRITE(6,17)
17           FORMAT(' Exit: bad option')
        ENDIF
 
C Input :
        IF(IOPT.EQ.-3)THEN
          WRITE(6,*)' Input image (modulus):'
        ELSE IF(IOPT.EQ.-2)THEN
          WRITE(6,*)' Input image (power spectrum):'
        ELSE
          WRITE(6,*)' Input image (real part):'
        ENDIF
        READ(5,10) NAME
        CALL JLP_VM_READIMAG(PNTR_RE,NX,NY,NAME,COMMENTS)
 
C Imaginary part:
          IF((IOPT.EQ.1).OR.(IOPT.EQ.-1))THEN
            WRITE(6,22)
22          FORMAT(' Input image (imaginary part):',/,
     1        ' Null imaginary part? (N) ?')
          ELSE
            WRITE(6,23)
23          FORMAT(' Input image (phase):',/,
     1        ' Null phase? (N) ?')
          ENDIF
C
          READ(5,10) ANS
10          FORMAT(A)
 
        IF(ANS.NE.'Y'.AND.ANS.NE.'y')THEN
          WRITE(6,*)' Enter filename: '
          READ(5,10) NAME
          CALL JLP_VM_READIMAG(PNTR_IM,NX1,NY1,NAME,COMMENTS)
C Check the size:
          IF((NX1.NE.NX).OR.(NY1.NE.NY))THEN
            WRITE(6,21)
21           FORMAT(' FATAL ERROR: Wrong size for the input images',/,
     1        ' (should be the same for the real/imaginary parts)')
            GOTO 999
          ENDIF
        ELSE
          CALL JLP_GETVM(PNTR_IM,NX*NY*4)
          CALL ZERO2(MADRID(PNTR_IM),NX,NY,NX)
        ENDIF
 
        IF(IOPT.EQ.-2)THEN
          CALL MODSQ_PHASE_TO_RE_IM(MADRID(PNTR_RE),MADRID(PNTR_IM),
     1                              NX,NY,NX)
        ELSE IF(IOPT.EQ.-3)THEN
          CALL MOD_PHASE_TO_RE_IM(MADRID(PNTR_RE),MADRID(PNTR_IM),
     1                            NX,NY,NX)
        ENDIF

C JLP 2008: I shift the images to 0,0, to avoid discontinuities
C (since I assume that the initial image is centered on the frame
C whereas the Fourier transform works with zero frequency at 0,0)
        CALL RECENT_FFT(MADRID(PNTR_RE),MADRID(PNTR_RE),NX,NY,NX)
        CALL RECENT_FFT(MADRID(PNTR_IM),MADRID(PNTR_IM),NX,NY,NX)

C FFT direct FFT corresponds to KOD=1:
C WARNING:
C JLP 2008: for FFTW I use the convention of +2 i PI for direct transform
C and - 2 i PI for inverse transform (as in vcrb.exe for PISCO data)
        IF(IOPT.GT.0)THEN
          KOD=1
        ELSE
          KOD=-1
        ENDIF
C****************************************************************
C Can use NAG library (with FFT_2D in 'fft_nag.for')
C or a routine derived from Numerical Recipees in 'fft_fourn.for'
C       CALL FFT_2D_FOURN(MADRID(PNTR_RE),MADRID(PNTR_IM),NX,NY,NX,KOD)
C Now in "FFTW" in "fftw_set.c"
        CALL FFTW_2D_FLT(MADRID(PNTR_RE),MADRID(PNTR_IM),NX,NY,NX,KOD)
 
C Check the value of IOPT:
        IF(IOPT.EQ.2)THEN
          CALL RE_IM_TO_MODSQ_PHASE(MADRID(PNTR_RE),MADRID(PNTR_IM),
     1                              NX,NY,NX)
        ELSE IF(IOPT.EQ.3)THEN
          CALL RE_IM_TO_MOD_PHASE(MADRID(PNTR_RE),MADRID(PNTR_IM),
     1                            NX,NY,NX)
        ENDIF

C Output :
        WRITE(6,*)' Real part / modsq or modulus:'
        READ(5,10) NAME
C Always recenter the output: if it is in direct space a null phase
C would break up the object in two upon the bottom and upper edges.
        CALL RECENT_FFT(MADRID(PNTR_RE),MADRID(PNTR_RE),NX,NY,NX)
        CALL JLP_WRITEIMAG(MADRID(PNTR_RE),NX,NY,NX,NAME,COMMENTS)
 
        WRITE(6,*)' Imaginary part / Phase:'
        READ(5,10) NAME
        CALL RECENT_FFT(MADRID(PNTR_IM),MADRID(PNTR_IM),NX,NY,NX)
        CALL JLP_WRITEIMAG(MADRID(PNTR_IM),NX,NY,NX,NAME,COMMENTS)
 
999        CALL JLP_END
        STOP
        END
C****************************************************************
C Compute power spectrum and phase from real and imaginary parts
C****************************************************************
        SUBROUTINE RE_IM_TO_MODSQ_PHASE(TR,TI,NX,NY,IDIM)
        INTEGER NX,NY,I,J,IDIM
        REAL TR(IDIM,*),TI(IDIM,*)
         DO 60 J=1,NY
          DO 60 I=1,NX
            AR=TR(I,J)
            AI=TI(I,J)
            TR(I,J)=AR*AR+AI*AI
            CALL PHASE_FROM_RE_IM(AR,AI,TI(I,J))
60        CONTINUE
        RETURN
        END
C****************************************************************
C Compute real and imaginary parts from power spectrum and phase 
C****************************************************************
        SUBROUTINE MODSQ_PHASE_TO_RE_IM(TR,TI,NX,NY,IDIM)
        INTEGER NX,NY,I,J,IDIM
        REAL TR(IDIM,*),TI(IDIM,*)
        REAL AR,AI
         DO 60 J=1,NY
          DO 60 I=1,NX
            AR=TR(I,J)
            IF(AR.GT.0.)THEN
              AR=SQRT(AR)
            ELSE
              AR=0.
            ENDIF
            AI=TI(I,J)
            TR(I,J)=AR*COS(AI)
            TI(I,J)=AR*SIN(AI)
60        CONTINUE
        RETURN
        END
C****************************************************************
C Compute modulus and phase from real and imaginary parts
C****************************************************************
        SUBROUTINE RE_IM_TO_MOD_PHASE(TR,TI,NX,NY,IDIM)
        IMPLICIT NONE
        INTEGER NX,NY,I,J,IDIM
        REAL TR(IDIM,*),TI(IDIM,*),AR,AI
         DO 60 J=1,NY
          DO 60 I=1,NX
            AR=TR(I,J)
            AI=TI(I,J)
            TR(I,J)=SQRT(AR*AR+AI*AI)
            CALL PHASE_FROM_RE_IM(AR, AI, TI(I,J))
60        CONTINUE
        RETURN
        END
C****************************************************************
C Compute real and imaginary parts from modulus and phase 
C****************************************************************
        SUBROUTINE MOD_PHASE_TO_RE_IM(TR,TI,NX,NY,IDIM)
        INTEGER NX,NY,I,J,IDIM
        REAL TR(IDIM,*),TI(IDIM,*)
        REAL AR,AI
         DO 60 J=1,NY
          DO 60 I=1,NX
            AR=TR(I,J)
            AI=TI(I,J)
            TR(I,J)=AR*COS(AI)
            TI(I,J)=AR*SIN(AI)
60        CONTINUE
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
C**********************************************************************
C Compute the phase (in the range [0, 2PI]) from real and imaginary part
C*********************************************************************
        SUBROUTINE PHASE_FROM_RE_IM(AR, AI, PHASE)
        IMPLICIT NONE
        REAL AI, AR, PI, EPSILON, PHASE
        PI=3.14159
C JLP2008: round off errors are around 1.e-9:
        EPSILON = 1.E-9
          IF(ABS(AR).LE.EPSILON) AR = 0.
          IF(ABS(AI).LE.EPSILON) AI = 0.
          IF(AR.NE.0.) THEN
             PHASE=ATAN(AI/AR)
             IF(AR.LT.0.)PHASE=PHASE+PI
          ELSE
             IF(AI.EQ.0.)THEN
               PHASE=0.
             ELSE IF(AI.GT.0.)THEN
               PHASE=PI/2.
             ELSE 
               PHASE=3.*PI/2.
             ENDIF
          ENDIF
          IF(PHASE.LT.0.)PHASE=PHASE+2.*PI
         RETURN
         END
