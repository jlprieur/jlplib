C++***************************************************************
C Program to test FFT and Fourier transforms
C
C JLP
C Version of 12-07-91
C--***************************************************************
	PROGRAM FFT1
	PARAMETER (IDIM=200)
	REAL IMAGR(IDIM,IDIM),IMAGI(IDIM,IDIM),IMAGE(IDIM,IDIM)
     	INTEGER KOD
	CHARACTER NAME*40,COMMENTS*80
 
        CALL JLP_BEGIN
C Inquire the format (input/output) :
	CALL JLP_INQUIFMT
 
C Test:
	NX=32
	NY=NX
	DO J=1,NY
	  DO I=1,NX
	    IMAGR(I,J)=0.
          END DO
        END DO

	IMAGR(6,16)=22.0
	IMAGR(6,17)=24.0
	IMAGR(20,20)=26.0
	IMAGR(10,10)=28.0
 
C Input :
	WRITE(6,*)' Input image (real): (max size is 200x200)'
	NAME=' '
	CALL JLP_READIMAG(IMAGR,NX,NY,IDIM,NAME,COMMENTS)
 
C Option:
        II = 0
        IF(II.NE.0) THEN
19	WRITE(6,15)
15	FORMAT(' Options: ',/,
     1	' 1=direct FFT (output=real,imaginary)',/,
     1	' -1=inverse FFT (output=real,imaginary)',/,
     1	' 2=direct FFT (output=power,phase)',/,
     1	' -2=inverse FFT (output=power,phase)',/,
     1	' Enter your choice:')
	READ(5,*) KOD
	ENDIF

	KOD=1
	IF((IABS(KOD).NE.1).AND.(IABS(KOD).NE.2))THEN
	   WRITE(6,17)
17	   FORMAT(' Error: enter again the option')
	   GOTO 19
	ENDIF
 
C Imaginary part:
	IF(KOD.LT.0)THEN
	  WRITE(6,*)' Input image (imaginary part):'
	  NAME=' '
	  CALL JLP_READIMAG(IMAGI,NX1,NY1,IDIM,NAME,COMMENTS)
	  IF((NX.NE.NX1).OR.(NY.NE.NY1))THEN
	    WRITE(2,21)
	    WRITE(6,21)
21	    FORMAT(' FATAL ERROR: Wrong size for the input image')
            STOP
	  ENDIF
	ELSE
	  DO J=1,NY
	   DO I=1,NX
	    IMAGI(I,J)=0.
	   ENDDO
	  ENDDO
	ENDIF
 
	WRITE(6,*) IMAGR(6,16),IMAGR(6,17),IMAGR(20,20),IMAGR(20,21)
	WRITE(6,*) IMAGI(6,16),IMAGI(6,17),IMAGI(20,20),IMAGI(20,21)
 
C FFT:
	PRINT *,' Normal fourier : 0, else FFT : 1'
	READ(5,*)II
	IF(II.EQ.1)THEN
	  WRITE(6,*) ' Start FFT'
	  CALL FFT_2D(IMAGR,IMAGI,NX,NY,IDIM,KOD)
        ELSE

	DO J=1,NY
	  DO I=1,NX
	    IMAGE(I,J)=IMAGR(I,J)
          END DO
        END DO
	WRITE(6,*) ' Start Fourier Transform'
	ISIGN=1
	CALL JLP_FOURIER(IMAGE,IMAGR,IMAGI,NX,NY,IDIM,ISIGN)
	WRITE(6,*) ' FFT OK'
C        CALL RECENTRE(IMAGR,IMAGR,NX,NY,IDIM)
C        CALL RECENTRE(IMAGI,IMAGI,NX,NY,IDIM)
        ENDIF
 
	WRITE(6,*) IMAGR(1,1),IMAGR(16,16)
	WRITE(6,*) IMAGI(1,1),IMAGI(16,16)
 
	WRITE(6,*) IMAGR(6,16),IMAGR(6,17),IMAGR(20,20),IMAGR(20,21)
	WRITE(6,*) IMAGI(6,16),IMAGI(6,17),IMAGI(20,20),IMAGI(20,21)
 
C Inverse FFT:
	WRITE(6,*) ' Start FFT-1'
	KOD=-1
	CALL FFT_2D(IMAGR,IMAGI,NX,NY,IDIM,KOD)
	WRITE(6,*) ' FFT-1 OK'
 
	WRITE(6,*) IMAGR(1,1),IMAGR(16,16)
	WRITE(6,*) IMAGI(1,1),IMAGI(16,16)
 
	WRITE(6,*) IMAGR(6,16),IMAGR(6,17),IMAGR(20,20),IMAGR(20,21)
	WRITE(6,*) IMAGI(6,16),IMAGI(6,17),IMAGI(20,20),IMAGI(20,21)
 
C        IF (II.EQ.0) STOP
C Output :
	WRITE(6,*)' Real part / Power spect.:'
	NAME=' '
	CALL JLP_WRITEIMAG(IMAGR,NX,NY,IDIM,NAME,COMMENTS)
	WRITE(6,*)' Imaginary part / Phase:'
	NAME=' '
	CALL JLP_WRITEIMAG(IMAGI,NX,NY,IDIM,NAME,COMMENTS)
 
999	CLOSE(2)
        CALL JLP_END
        STOP
	END
C**********************************************************************
C Interface routine with fourier or fourier1 from d_utilities.for
C**********************************************************************
	SUBROUTINE FFT_2D1(IMAGR,IMAGI,NX,NY,IDIM,KOD)
	REAL IMAGR(IDIM,*),IMAGI(IDIM,*)
        INTEGER P_R,P_I,NX,NY,KOD,NSIZE,IOP
	INTEGER MADRID(1)
	COMMON /MADRID/MADRID

	NSIZE=NX*NY*8
	CALL JLP_GETVM(P_R,NSIZE)
	CALL JLP_GETVM(P_I,NSIZE)

C Transfer to double precision:
        CALL TTO8(MADRID(P_R),IMAGR,NX,NY,IDIM)
        CALL TTO8(MADRID(P_I),IMAGI,NX,NY,IDIM)

C Calling actual FFT now:
	IF(KOD.GT.0)THEN
   	  IOP=1
	ELSE
	  IOP=-1
	ENDIF
        CALL FOURIER1(MADRID(P_R),MADRID(P_I),NX,NY,IOP)

C Transfer back to simple precision:
        CALL FRO8(IMAGR,MADRID(P_R),NX,NY,IDIM)
        CALL FRO8(IMAGI,MADRID(P_I),NX,NY,IDIM)
	CALL JLP_FREEVM(P_R,NSIZE)
	CALL JLP_FREEVM(P_I,NSIZE)

	RETURN
	END
C**********************************************************************
         SUBROUTINE TTO8(BR8,BR4,NX,NY,IDIM)
         REAL BR4(IDIM,*)
	 DOUBLE PRECISION BR8(NX,NY)
	 DO J=1,NY
	   DO I=1,NX
	     BR8(I,J)=DBLE(BR4(I,J))
	   END DO
	 END DO
         RETURN
	 END
C**********************************************************************
         SUBROUTINE FRO8(BR4,BR8,NX,NY,IDIM)
         REAL BR4(IDIM,*)
	 DOUBLE PRECISION BR8(NX,NY)
	 DO J=1,NY
	   DO I=1,NX
	     BR4(I,J)=SNGL(BR8(I,J))
	   END DO
	 END DO
         RETURN
	 END
C********************************************************
	include 'hrsa:fft_jlp.for'
	include 'hrsa:jlp_fourier.for'
C********************************************************
C	include 'hrsa:fft_nag.for'
C********************************************************
	include 'hrsa:d_utilities.for'
