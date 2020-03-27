C++********************************************************************
C Set of routines:
C JLP_FOURIER
C
C JLP
C Version 11-07-91
C--*******************************************************************
C JLP_FOURIER
C Computes the Fourier Transform of a real image (full of zeroes...) 
C
C Zero frequency is in RE(1,1), and IM(1,1)
C
C Input:
C IMAGE: Input image
C
C Output:
C RE,IM: Real and imaginary part 
C
C RE(KX,KY) = Sum on IX,IY of IMAGE(IX,IY)*COS(2*PI*(IX*(KX-1)/NX+IY*(KY-1)/NY))
C IM(KX,KY) = Sum on IX,IY of IMAGE(IX,IY)*SIN(2*PI*(IX*(KX-1)/NX+IY*(KY-1)/NY))
C
C Tests: Big improvement in accuracy 
C          when X0,X1,X0,Y1,PI,ANGLE,MYCOS, and MYSIN are real*8...
C          and when DCOS and DSIN are used.
C*******************************************************************
	SUBROUTINE JLP_FOURIER(IMAGE,RE,IM,NX,NY,IDIM,ISIGN)
	PARAMETER (IDIM1=512, SMALLER_VALUE=1.E-12)
	REAL IMAGE(IDIM,*),RE(IDIM,*),IM(IDIM,*)
	REAL*8 X0,Y0,X1,Y1,SIGN
	REAL*8 PI,ANGLE
	INTEGER NX,NY,ISIGN
	REAL*8 MYCOS(IDIM1),MYSIN(IDIM1)
	
	PI=3.14159265358979323846
	X0=2.*PI/FLOAT(NX)
	Y0=2.*PI/FLOAT(NY)
	SIGN=FLOAT(ISIGN)

C Resetting arrays to zero:
	DO KY=1,NY
	  DO KX=1,NX
	    RE(KX,KY)=0.
	    IM(KX,KY)=0.
	  END DO
	END DO

        IF(NX.EQ.NY)THEN
C When NX=NY, we have:
C RE(KX,KY) = Sum on IX,IY of IMAGE(IX,IY)*COS((2*PI/NX)*(IX*(KX-1)+IY*(KY-1)))
C IM(KX,KY) = Sum on IX,IY of IMAGE(IX,IY)*SIN((2*PI/NX)*(IX*(KX-1)+IY*(KY-1)))
C
C Cosinus and sinus:
C MYCOS(I) = COS(2*PI*(I-1)/NX)
         DO I=1,NX
	   ANGLE=X0*FLOAT(I-1)
	   MYCOS(I)=DCOS(ANGLE)
	   MYSIN(I)=DSIN(ANGLE)
         END DO

C Fourier Transform with pre-computed cosinus and sinus :
	DO IY=1,NY
	  DO IX=1,NX
	    IF(ABS(IMAGE(IX,IY)).GT.SMALLER_VALUE)THEN
	      DO KY=1,NY
	        DO KX=1,NX
                  I=(IX-1)*(KX-1)+(IY-1)*(KY-1)
                  I=MOD(I,NX)+1
                  RE(KX,KY)=RE(KX,KY)+IMAGE(IX,IY)*MYCOS(I)
                  IM(KX,KY)=IM(KX,KY)+IMAGE(IX,IY)*MYSIN(I)
	        END DO
	      END DO
	    ENDIF
	  END DO
	END DO

        ELSE

C Fourier Transform without pre-computed cosinus and sinus:
	DO IY=1,NY
	  Y1=Y0*FLOAT(IY-1)
	  DO IX=1,NX
	    X1=X0*FLOAT(IX-1)
	    IF(ABS(IMAGE(IX,IY)).GT.SMALLER_VALUE)THEN
	      DO KY=1,NY
	        DO KX=1,NX
	          ANGLE=X1*FLOAT(KX-1)+Y1*FLOAT(KY-1)
                  RE(KX,KY)=RE(KX,KY)+IMAGE(IX,IY)*DCOS(ANGLE)
                  IM(KX,KY)=IM(KX,KY)+IMAGE(IX,IY)*DSIN(ANGLE)
	        END DO
	      END DO
	    ENDIF
	  END DO
	END DO

        ENDIF

	RETURN
	END
