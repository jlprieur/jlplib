******************************************************************
* Necessary set of routines for C06GCF and G06FJF
* 
* C06FTEXT
* X01FTEXT
* P01FTEXT
*
* JLP Version 07-10-91
******************************************************************
*UPTODATE C06ECQTEXT
      SUBROUTINE C06ECQ(X, Y, PTS, M, P)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     RADIX PRIME COMPLEX FOURIER TRANSFORM KERNEL
C     .. SCALAR ARGUMENTS ..
      INTEGER M, P, PTS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X(PTS), Y(PTS)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION ANGLE, IS, IU, RS, RU, T, TWOPI, XT, YT
      INTEGER J, JJ, K0, K, KS1, KS2, MOVER2, MP, PM, PP, U, V
      LOGICAL FOLD, ZERO
C     .. LOCAL ARRAYS ..
      DOUBLE PRECISION A(18), AA(9,9), B(18), BB(9,9), C(18), IA(9),
     * IB(9),RA(9), RB(9), S(18)
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DCOS, DSIN, X01AAF
C     *** IMPLEMENTATION DEPENDENT DECLARATION ***
C     DOUBLE PRECISION DBLE
C     ..
      TWOPI = 2.0D0*X01AAF(0.0D0)
      MOVER2 = M/2 + 1
      MP = M*P
      PP = P/2
      PM = P - 1
      DO 20 U=1,PP
         JJ = P - U
         ANGLE = TWOPI*DBLE(U)/DBLE(P)
         A(U) = DCOS(ANGLE)
         B(U) = DSIN(ANGLE)
         A(JJ) = A(U)
         B(JJ) = -B(U)
   20 CONTINUE
      DO 60 U=1,PP
         DO 40 V=1,PP
            JJ = U*V - ((U*V)/P)*P
            AA(V,U) = A(JJ)
            BB(V,U) = B(JJ)
   40    CONTINUE
   60 CONTINUE
C
      DO 300 J=1,MOVER2
         FOLD = J.GT.1 .AND. 2*J.LT.M + 2
         K0 = J
         ANGLE = TWOPI*DBLE(J-1)/DBLE(MP)
         ZERO = ANGLE.EQ.0.0D0
         C(1) = DCOS(ANGLE)
         S(1) = DSIN(ANGLE)
         DO 80 U=2,PM
            C(U) = C(U-1)*C(1) - S(U-1)*S(1)
            S(U) = S(U-1)*C(1) + C(U-1)*S(1)
   80    CONTINUE
         GO TO 140
  100    CONTINUE
         FOLD = .FALSE.
         K0 = M + 2 - J
         DO 120 U=1,PM
            T = C(U)*A(U) + S(U)*B(U)
            S(U) = -S(U)*A(U) + C(U)*B(U)
            C(U) = T
  120    CONTINUE
  140    CONTINUE
C
         DO 280 K=K0,PTS,MP
            XT = X(K)
            YT = Y(K)
            KS1 = M + K
            KS2 = (P-1)*M + K
            RS = X(KS1) + X(KS2)
            IS = Y(KS1) + Y(KS2)
            RU = X(KS1) - X(KS2)
            IU = Y(KS1) - Y(KS2)
            DO 160 U=1,PP
               RA(U) = XT + RS*AA(U,1)
               IA(U) = YT + IS*AA(U,1)
               RB(U) = RU*BB(U,1)
               IB(U) = IU*BB(U,1)
  160       CONTINUE
            XT = XT + RS
            YT = YT + IS
            DO 200 U=2,PP
               JJ = P - U
               KS1 = U*M + K
               KS2 = JJ*M + K
               RS = X(KS1) + X(KS2)
               IS = Y(KS1) + Y(KS2)
               RU = X(KS1) - X(KS2)
               IU = Y(KS1) - Y(KS2)
               XT = XT + RS
               YT = YT + IS
               DO 180 V=1,PP
                  RA(V) = RA(V) + RS*AA(V,U)
                  IA(V) = IA(V) + IS*AA(V,U)
                  RB(V) = RB(V) + RU*BB(V,U)
                  IB(V) = IB(V) + IU*BB(V,U)
  180          CONTINUE
  200       CONTINUE
            X(K) = XT
            Y(K) = YT
            DO 260 U=1,PP
               JJ = P - U
               IF (ZERO) GO TO 220
               XT = RA(U) + IB(U)
               YT = IA(U) - RB(U)
               KS1 = U*M + K
               X(KS1) = XT*C(U) + YT*S(U)
               Y(KS1) = YT*C(U) - XT*S(U)
               XT = RA(U) - IB(U)
               YT = IA(U) + RB(U)
               KS1 = JJ*M + K
               X(KS1) = XT*C(JJ) + YT*S(JJ)
               Y(KS1) = YT*C(JJ) - XT*S(JJ)
               GO TO 240
  220          CONTINUE
               KS1 = U*M + K
               X(KS1) = RA(U) + IB(U)
               Y(KS1) = IA(U) - RB(U)
               KS1 = JJ*M + K
               X(KS1) = RA(U) - IB(U)
               Y(KS1) = IA(U) + RB(U)
  240          CONTINUE
  260       CONTINUE
  280    CONTINUE
         IF (FOLD) GO TO 100
  300 CONTINUE
C
      RETURN
      END
** END OF C06ECQTEXT
*UPTODATE C06ECRTEXT
      SUBROUTINE C06ECR(X0, Y0, PTS, X1, Y1, M1, X2, Y2, M2, X3,Y3, M3,
     * X4, Y4, M4, X5, Y5, M5, X6, Y6, M6, X7, Y7, M7, M)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     RADIX EIGHT COMPLEX FOURIER TRANSFORM KERNEL
C     .. SCALAR ARGUMENTS ..
      INTEGER M1, M2, M3, M4, M5, M6, M7, M, PTS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X0(PTS), X1(M1), X2(M2), X3(M3), X4(M4), X5(M5),
     * X6(M6),X7(M7), Y0(PTS), Y1(M1), Y2(M2), Y3(M3), Y4(M4), Y5(M5),
     *Y6(M6), Y7(M7)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION ANGLE, C1, C2, C3, C4, C5, C6, C7, E, I1, I2, I3,
     * I4,I5, I6, I7, IS0, IS1, IS2, IS3, ISS0, ISS1, ISU0, ISU1, IU0,
     *IU1, IU2, IU3, IUS0, IUS1, IUU0, IUU1, R1, R2, R3, R4, R5,R6, R7,
     * RS0, RS1, RS2, RS3, RSS0, RSS1, RSU0, RSU1, RU0,RU1, RU2, RU3,
     * RUS0, RUS1, RUU0, RUU1, S1, S2, S3, S4, S5,S6, S7, T, TWOPI
      INTEGER J, K0, K, M8, MOVER2
      LOGICAL FOLD, ZERO
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DCOS, DSIN, X01AAF
C     *** IMPLEMENTATION DEPENDENT DECLARATION ***
C     DOUBLE PRECISION DBLE
C     ..
      M8 = M*8
      MOVER2 = M/2 + 1
      TWOPI = 2.0D0*X01AAF(0.0D0)
      E = DCOS(TWOPI/8.0D0)
C
      DO 120 J=1,MOVER2
         FOLD = J.GT.1 .AND. 2*J.LT.M + 2
         K0 = J
         ANGLE = TWOPI*DBLE(J-1)/DBLE(M8)
         ZERO = ANGLE.EQ.0.0D0
         C1 = DCOS(ANGLE)
         S1 = DSIN(ANGLE)
         C2 = C1*C1 - S1*S1
         S2 = S1*C1 + C1*S1
         C3 = C2*C1 - S2*S1
         S3 = S2*C1 + C2*S1
         C4 = C2*C2 - S2*S2
         S4 = S2*C2 + C2*S2
         C5 = C4*C1 - S4*S1
         S5 = S4*C1 + C4*S1
         C6 = C4*C2 - S4*S2
         S6 = S4*C2 + C4*S2
         C7 = C4*C3 - S4*S3
         S7 = S4*C3 + C4*S3
         GO TO 40
   20    CONTINUE
         FOLD = .FALSE.
         K0 = M + 2 - J
         T = (C1+S1)*E
         S1 = (C1-S1)*E
         C1 = T
         T = S2
         S2 = C2
         C2 = T
         T = (-C3+S3)*E
         S3 = (C3+S3)*E
         C3 = T
         C4 = -C4
         T = -(C5+S5)*E
         S5 = (-C5+S5)*E
         C5 = T
         T = -S6
         S6 = -C6
         C6 = T
         T = (C7-S7)*E
         S7 = -(C7+S7)*E
         C7 = T
   40    CONTINUE
C
         DO 100 K=K0,PTS,M8
            RS0 = X0(K) + X4(K)
            IS0 = Y0(K) + Y4(K)
            RU0 = X0(K) - X4(K)
            IU0 = Y0(K) - Y4(K)
            RS1 = X1(K) + X5(K)
            IS1 = Y1(K) + Y5(K)
            RU1 = X1(K) - X5(K)
            IU1 = Y1(K) - Y5(K)
            RS2 = X2(K) + X6(K)
            IS2 = Y2(K) + Y6(K)
            RU2 = X2(K) - X6(K)
            IU2 = Y2(K) - Y6(K)
            RS3 = X3(K) + X7(K)
            IS3 = Y3(K) + Y7(K)
            RU3 = X3(K) - X7(K)
            IU3 = Y3(K) - Y7(K)
            RSS0 = RS0 + RS2
            ISS0 = IS0 + IS2
            RSU0 = RS0 - RS2
            ISU0 = IS0 - IS2
            RSS1 = RS1 + RS3
            ISS1 = IS1 + IS3
            RSU1 = RS1 - RS3
            ISU1 = IS1 - IS3
            RUS0 = RU0 - IU2
            IUS0 = IU0 + RU2
            RUU0 = RU0 + IU2
            IUU0 = IU0 - RU2
            RUS1 = RU1 - IU3
            IUS1 = IU1 + RU3
            RUU1 = RU1 + IU3
            IUU1 = IU1 - RU3
            T = (RUS1+IUS1)*E
            IUS1 = (IUS1-RUS1)*E
            RUS1 = T
            T = (RUU1+IUU1)*E
            IUU1 = (IUU1-RUU1)*E
            RUU1 = T
            X0(K) = RSS0 + RSS1
            Y0(K) = ISS0 + ISS1
            IF (ZERO) GO TO 60
            R1 = RUU0 + RUU1
            I1 = IUU0 + IUU1
            R2 = RSU0 + ISU1
            I2 = ISU0 - RSU1
            R3 = RUS0 + IUS1
            I3 = IUS0 - RUS1
            R4 = RSS0 - RSS1
            I4 = ISS0 - ISS1
            R5 = RUU0 - RUU1
            I5 = IUU0 - IUU1
            R6 = RSU0 - ISU1
            I6 = ISU0 + RSU1
            R7 = RUS0 - IUS1
            I7 = IUS0 + RUS1
            X4(K) = R1*C1 + I1*S1
            Y4(K) = I1*C1 - R1*S1
            X2(K) = R2*C2 + I2*S2
            Y2(K) = I2*C2 - R2*S2
            X6(K) = R3*C3 + I3*S3
            Y6(K) = I3*C3 - R3*S3
            X1(K) = R4*C4 + I4*S4
            Y1(K) = I4*C4 - R4*S4
            X5(K) = R5*C5 + I5*S5
            Y5(K) = I5*C5 - R5*S5
            X3(K) = R6*C6 + I6*S6
            Y3(K) = I6*C6 - R6*S6
            X7(K) = R7*C7 + I7*S7
            Y7(K) = I7*C7 - R7*S7
            GO TO 80
   60       CONTINUE
            X4(K) = RUU0 + RUU1
            Y4(K) = IUU0 + IUU1
            X2(K) = RSU0 + ISU1
            Y2(K) = ISU0 - RSU1
            X6(K) = RUS0 + IUS1
            Y6(K) = IUS0 - RUS1
            X1(K) = RSS0 - RSS1
            Y1(K) = ISS0 - ISS1
            X5(K) = RUU0 - RUU1
            Y5(K) = IUU0 - IUU1
            X3(K) = RSU0 - ISU1
            Y3(K) = ISU0 + RSU1
            X7(K) = RUS0 - IUS1
            Y7(K) = IUS0 + RUS1
   80       CONTINUE
  100    CONTINUE
         IF (FOLD) GO TO 20
  120 CONTINUE
C
      RETURN
      END
** END OF C06ECRTEXT
*UPTODATE C06ECSTEXT
      SUBROUTINE C06ECS(X0, Y0, PTS, X1, Y1, M1, X2, Y2, M2, X3,Y3, M3,
     * X4, Y4, M4, M)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     RADIX FIVE COMPLEX FOURIER TRANSFORM KERNEL
C     .. SCALAR ARGUMENTS ..
      INTEGER M1, M2, M3, M4, M, PTS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X0(PTS), X1(M1), X2(M2), X3(M3), X4(M4), Y0(PTS),
     *Y1(M1), Y2(M2), Y3(M3), Y4(M4)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION A1, A2, ANGLE, AS, AU, B1, B2, C1, C2, C3, C4,
     * I0, I1,I2, I3, I4, IA1, IA2, IAS, IAU, IB1, IB2, IS1, IS2, ISS,
     *IU1, IU2, R0, R1, R2, R3, R4, RA1, RA2, RAS, RAU, RB1, RB2,RS1,
     * RS2, RSS, RU1, RU2, S1, S2, S3, S4, T, TWOPI
      INTEGER J, K0, K, M5, MOVER2
      LOGICAL FOLD, ZERO
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DCOS, DSIN, DSQRT, X01AAF
C     *** IMPLEMENTATION DEPENDENT DECLARATION ***
C     DOUBLE PRECISION DBLE
C     ..
      M5 = M*5
      MOVER2 = M/2 + 1
      TWOPI = 2.0D0*X01AAF(0.0D0)
      A1 = DCOS(TWOPI/5.0D0)
      B1 = DSIN(TWOPI/5.0D0)
      A2 = DCOS(2.0D0*TWOPI/5.0D0)
      B2 = DSIN(2.0D0*TWOPI/5.0D0)
      AS = -1.0D0/4.0D0
      AU = DSQRT(5.0D0)/4.0D0
C
      DO 120 J=1,MOVER2
         FOLD = J.GT.1 .AND. 2*J.LT.M + 2
         K0 = J
         ANGLE = TWOPI*DBLE(J-1)/DBLE(M5)
         ZERO = ANGLE.EQ.0.0D0
         C1 = DCOS(ANGLE)
         S1 = DSIN(ANGLE)
         C2 = C1*C1 - S1*S1
         S2 = S1*C1 + C1*S1
         C3 = C2*C1 - S2*S1
         S3 = S2*C1 + C2*S1
         C4 = C2*C2 - S2*S2
         S4 = S2*C2 + C2*S2
         GO TO 40
   20    CONTINUE
         FOLD = .FALSE.
         K0 = M + 2 - J
         T = C1*A1 + S1*B1
         S1 = C1*B1 - S1*A1
         C1 = T
         T = C2*A2 + S2*B2
         S2 = C2*B2 - S2*A2
         C2 = T
         T = C3*A2 - S3*B2
         S3 = -C3*B2 - S3*A2
         C3 = T
         T = C4*A1 - S4*B1
         S4 = -C4*B1 - S4*A1
         C4 = T
   40    CONTINUE
C
         DO 100 K=K0,PTS,M5
            R0 = X0(K)
            I0 = Y0(K)
            RS1 = X1(K) + X4(K)
            IS1 = Y1(K) + Y4(K)
            RU1 = X1(K) - X4(K)
            IU1 = Y1(K) - Y4(K)
            RS2 = X2(K) + X3(K)
            IS2 = Y2(K) + Y3(K)
            RU2 = X2(K) - X3(K)
            IU2 = Y2(K) - Y3(K)
            RSS = RS1 + RS2
            ISS = IS1 + IS2
            RAS = R0 + RSS*AS
            IAS = I0 + ISS*AS
            RAU = (RS1-RS2)*AU
            IAU = (IS1-IS2)*AU
            RA1 = RAS + RAU
            IA1 = IAS + IAU
            RA2 = RAS - RAU
            IA2 = IAS - IAU
            RB1 = RU1*B1 + RU2*B2
            IB1 = IU1*B1 + IU2*B2
            RB2 = RU1*B2 - RU2*B1
            IB2 = IU1*B2 - IU2*B1
            X0(K) = R0 + RSS
            Y0(K) = I0 + ISS
            IF (ZERO) GO TO 60
            R1 = RA1 + IB1
            I1 = IA1 - RB1
            R2 = RA2 + IB2
            I2 = IA2 - RB2
            R3 = RA2 - IB2
            I3 = IA2 + RB2
            R4 = RA1 - IB1
            I4 = IA1 + RB1
            X1(K) = R1*C1 + I1*S1
            Y1(K) = I1*C1 - R1*S1
            X2(K) = R2*C2 + I2*S2
            Y2(K) = I2*C2 - R2*S2
            X3(K) = R3*C3 + I3*S3
            Y3(K) = I3*C3 - R3*S3
            X4(K) = R4*C4 + I4*S4
            Y4(K) = I4*C4 - R4*S4
            GO TO 80
   60       CONTINUE
            X1(K) = RA1 + IB1
            Y1(K) = IA1 - RB1
            X2(K) = RA2 + IB2
            Y2(K) = IA2 - RB2
            X3(K) = RA2 - IB2
            Y3(K) = IA2 + RB2
            X4(K) = RA1 - IB1
            Y4(K) = IA1 + RB1
   80       CONTINUE
  100    CONTINUE
         IF (FOLD) GO TO 20
  120 CONTINUE
C
      RETURN
      END
** END OF C06ECSTEXT
*UPTODATE C06ECTTEXT
      SUBROUTINE C06ECT(X0, Y0, PTS, X1, Y1, M1, X2, Y2, M2, X3,Y3, M3,
     * M)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     RADIX FOUR COMPLEX FOURIER TRANSFORM KERNEL
C     .. SCALAR ARGUMENTS ..
      INTEGER M1, M2, M3, M, PTS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X0(PTS), X1(M1), X2(M2), X3(M3), Y0(PTS), Y1(M1),
     *Y2(M2), Y3(M3)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION ANGLE, C1, C2, C3, I1, I2, I3, IS0, IS1, IU0,
     * IU1, R1,R2, R3, RS0, RS1, RU0, RU1, S1, S2, S3, T, TWOPI
      INTEGER J, K0, K, M4, MOVER2
      LOGICAL FOLD, ZERO
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DCOS, DSIN, X01AAF
C     *** IMPLEMENTATION DEPENDENT DECLARATION ***
C     DOUBLE PRECISION DBLE
C     ..
      M4 = M*4
      MOVER2 = M/2 + 1
      TWOPI = 2.0D0*X01AAF(0.0D0)
C
      DO 120 J=1,MOVER2
         FOLD = J.GT.1 .AND. 2*J.LT.M + 2
         K0 = J
         ANGLE = TWOPI*DBLE(J-1)/DBLE(M4)
         ZERO = ANGLE.EQ.0.0D0
         C1 = DCOS(ANGLE)
         S1 = DSIN(ANGLE)
         C2 = C1*C1 - S1*S1
         S2 = S1*C1 + C1*S1
         C3 = C2*C1 - S2*S1
         S3 = S2*C1 + C2*S1
         GO TO 40
   20    CONTINUE
         FOLD = .FALSE.
         K0 = M + 2 - J
         T = C1
         C1 = S1
         S1 = T
         C2 = -C2
         T = C3
         C3 = -S3
         S3 = -T
   40    CONTINUE
C
         DO 100 K=K0,PTS,M4
            RS0 = X0(K) + X2(K)
            IS0 = Y0(K) + Y2(K)
            RU0 = X0(K) - X2(K)
            IU0 = Y0(K) - Y2(K)
            RS1 = X1(K) + X3(K)
            IS1 = Y1(K) + Y3(K)
            RU1 = X1(K) - X3(K)
            IU1 = Y1(K) - Y3(K)
            X0(K) = RS0 + RS1
            Y0(K) = IS0 + IS1
            IF (ZERO) GO TO 60
            R1 = RU0 + IU1
            I1 = IU0 - RU1
            R2 = RS0 - RS1
            I2 = IS0 - IS1
            R3 = RU0 - IU1
            I3 = IU0 + RU1
            X2(K) = R1*C1 + I1*S1
            Y2(K) = I1*C1 - R1*S1
            X1(K) = R2*C2 + I2*S2
            Y1(K) = I2*C2 - R2*S2
            X3(K) = R3*C3 + I3*S3
            Y3(K) = I3*C3 - R3*S3
            GO TO 80
   60       CONTINUE
            X2(K) = RU0 + IU1
            Y2(K) = IU0 - RU1
            X1(K) = RS0 - RS1
            Y1(K) = IS0 - IS1
            X3(K) = RU0 - IU1
            Y3(K) = IU0 + RU1
   80       CONTINUE
  100    CONTINUE
         IF (FOLD) GO TO 20
  120 CONTINUE
C
      RETURN
      END
** END OF C06ECTTEXT
*UPTODATE C06ECUTEXT
      SUBROUTINE C06ECU(X0, Y0, PTS, X1, Y1, M1, X2, Y2, M2, M)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     RADIX THREE COMPLEX FOURIER TRANSFORM KERNEL
C     .. SCALAR ARGUMENTS ..
      INTEGER M1, M2, M, PTS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X0(PTS), X1(M1), X2(M2), Y0(PTS), Y1(M1), Y2(M2)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION A, ANGLE, B, C1, C2, I0, I1, I2, IA, IB, IS, R0,
     * R1, R2,RA, RB, RS, S1, S2, T, TWOPI
      INTEGER J, K0, K, M3, MOVER2
      LOGICAL FOLD, ZERO
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DCOS, DSIN, DSQRT, X01AAF
C     *** IMPLEMENTATION DEPENDENT DECLARATION ***
C     DOUBLE PRECISION DBLE
C     ..
      M3 = M*3
      MOVER2 = M/2 + 1
      TWOPI = 2.0D0*X01AAF(0.0D0)
C     A = COS(TWOPI/3.0)
      A = -0.5D0
C     B = SIN(TWOPI/3.0)
      B = DSQRT(0.75D0)
C
      DO 120 J=1,MOVER2
         FOLD = J.GT.1 .AND. 2*J.LT.M + 2
         K0 = J
         ANGLE = TWOPI*DBLE(J-1)/DBLE(M3)
         ZERO = ANGLE.EQ.0.0D0
         C1 = DCOS(ANGLE)
         S1 = DSIN(ANGLE)
         C2 = C1*C1 - S1*S1
         S2 = S1*C1 + C1*S1
         GO TO 40
   20    CONTINUE
         FOLD = .FALSE.
         K0 = M + 2 - J
         T = C1*A + S1*B
         S1 = C1*B - S1*A
         C1 = T
         T = C2*A - S2*B
         S2 = -C2*B - S2*A
         C2 = T
   40    CONTINUE
C
         DO 100 K=K0,PTS,M3
            R0 = X0(K)
            I0 = Y0(K)
            RS = X1(K) + X2(K)
            IS = Y1(K) + Y2(K)
            X0(K) = R0 + RS
            Y0(K) = I0 + IS
            RA = R0 + RS*A
            IA = I0 + IS*A
            RB = (X1(K)-X2(K))*B
            IB = (Y1(K)-Y2(K))*B
            IF (ZERO) GO TO 60
            R1 = RA + IB
            I1 = IA - RB
            R2 = RA - IB
            I2 = IA + RB
            X1(K) = R1*C1 + I1*S1
            Y1(K) = I1*C1 - R1*S1
            X2(K) = R2*C2 + I2*S2
            Y2(K) = I2*C2 - R2*S2
            GO TO 80
   60       CONTINUE
            X1(K) = RA + IB
            Y1(K) = IA - RB
            X2(K) = RA - IB
            Y2(K) = IA + RB
   80       CONTINUE
  100    CONTINUE
         IF (FOLD) GO TO 20
  120 CONTINUE
C
      RETURN
      END
** END OF C06ECUTEXT
*UPTODATE C06ECVTEXT
      SUBROUTINE C06ECV(X0, Y0, PTS, X1, Y1, M1, M)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     RADIX TWO COMPLEX FOURIER TRANSFORM KERNEL
C     .. SCALAR ARGUMENTS ..
      INTEGER M1, M, PTS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X0(PTS), X1(M1), Y0(PTS), Y1(M1)
C     ..
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION ANGLE, C, IS, IU, RS, RU, S, TWOPI
      INTEGER J, K0, K, M2, MOVER2
      LOGICAL FOLD, ZERO
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DCOS, DSIN, X01AAF
C     *** IMPLEMENTATION DEPENDENT DECLARATION ***
C     DOUBLE PRECISION DBLE
C     ..
      M2 = M*2
      MOVER2 = M/2 + 1
      TWOPI = 2.0D0*X01AAF(0.0D0)
C
      DO 120 J=1,MOVER2
         FOLD = J.GT.1 .AND. 2*J.LT.M + 2
         K0 = J
         ANGLE = TWOPI*DBLE(J-1)/DBLE(M2)
         ZERO = ANGLE.EQ.0.0D0
         C = DCOS(ANGLE)
         S = DSIN(ANGLE)
         GO TO 40
   20    CONTINUE
         FOLD = .FALSE.
         K0 = M + 2 - J
         C = -C
   40    CONTINUE
C
         DO 100 K=K0,PTS,M2
            RS = X0(K) + X1(K)
            IS = Y0(K) + Y1(K)
            RU = X0(K) - X1(K)
            IU = Y0(K) - Y1(K)
            X0(K) = RS
            Y0(K) = IS
            IF (ZERO) GO TO 60
            X1(K) = RU*C + IU*S
            Y1(K) = IU*C - RU*S
            GO TO 80
   60       CONTINUE
            X1(K) = RU
            Y1(K) = IU
   80       CONTINUE
  100    CONTINUE
         IF (FOLD) GO TO 20
  120 CONTINUE
C
      RETURN
      END
** END OF C06ECVTEXT
*UPTODATE C06ECWTEXT
      SUBROUTINE C06ECW(X, Y, PTS, FACTOR)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     COMPLEX FOURIER TRANSFORM KERNEL DRIVER
C     .. SCALAR ARGUMENTS ..
      INTEGER PTS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION X(PTS), Y(PTS)
      INTEGER FACTOR(21)
C     ..
C     .. LOCAL SCALARS ..
      INTEGER F, M1, M2, M3, M4, M5, M6, M7, M, P
C     .. SUBROUTINE REFERENCES ..
C     C06ECQ, C06ECR, C06ECS, C06ECT, C06ECU, C06ECV
C     ..
      F = 0
      M = PTS
   20 CONTINUE
      F = F + 1
      P = FACTOR(F)
      IF (P.EQ.0) RETURN
      IF (P.EQ.1) GO TO 20
      M = M/P
      M1 = PTS - M
      M2 = M1 - M
      M3 = M2 - M
      M4 = M3 - M
      M5 = M4 - M
      M6 = M5 - M
      M7 = M6 - M
      IF (P.EQ.2) GO TO 40
      IF (P.EQ.3) GO TO 60
      IF (P.EQ.4) GO TO 80
      IF (P.EQ.5) GO TO 100
      IF (P.EQ.8) GO TO 120
      GO TO 140
C
   40 CONTINUE
      CALL C06ECV(X(1), Y(1), PTS, X(M+1), Y(M+1), M1, M)
      GO TO 20
C
   60 CONTINUE
      CALL C06ECU(X(1), Y(1), PTS, X(M+1), Y(M+1), M1, X(2*M+1),Y(2*M+1)
     *, M2, M)
      GO TO 20
C
   80 CONTINUE
      CALL C06ECT(X(1), Y(1), PTS, X(M+1), Y(M+1), M1, X(2*M+1),Y(2*M+1)
     *, M2, X(3*M+1), Y(3*M+1), M3, M)
      GO TO 20
C
  100 CONTINUE
      CALL C06ECS(X(1), Y(1), PTS, X(M+1), Y(M+1), M1, X(2*M+1),Y(2*M+1)
     *, M2, X(3*M+1), Y(3*M+1), M3, X(4*M+1), Y(4*M+1),M4, M)
      GO TO 20
C
  120 CONTINUE
      CALL C06ECR(X(1), Y(1), PTS, X(M+1), Y(M+1), M1, X(2*M+1),Y(2*M+1)
     *, M2, X(3*M+1), Y(3*M+1), M3, X(4*M+1), Y(4*M+1),M4, X(5*M+1),
     * Y(5*M+1), M5, X(6*M+1), Y(6*M+1), M6,X(7*M+1), Y(7*M+1), M7, M)
      GO TO 20
C
  140 CONTINUE
      CALL C06ECQ(X, Y, PTS, M, P)
      GO TO 20
C
      END
** END OF C06ECWTEXT
*UPTODATE C06FAZTEXT
      SUBROUTINE C06FAZ(PTS, PMAX, TWOGRP, TFACT, RFACT, IERROR)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     COPY REORDERING FACTORING PROGRAMME
C     .. SCALAR ARGUMENTS ..
      INTEGER IERROR, PMAX, PTS, TWOGRP
C     .. ARRAY ARGUMENTS ..
      INTEGER RFACT(21), TFACT(21)
C     ..
C     .. LOCAL SCALARS ..
      INTEGER F, J, JJ, N, NEST, P, PTWO, Q
C     .. LOCAL ARRAYS ..
      INTEGER PP(10), QQ(20)
C     ..
      DATA NEST /20/
      N = PTS
      F = 2
      P = 0
      Q = 0
   20 CONTINUE
      IF (N.LE.1) GO TO 100
      DO 40 J=F,PMAX
         IF (N.EQ.(N/J)*J) GO TO 60
   40 CONTINUE
      GO TO 280
   60 CONTINUE
      IF (2*P+Q.GE.NEST) GO TO 300
      F = J
      N = N/F
      IF (N.EQ.(N/F)*F) GO TO 80
      Q = Q + 1
      QQ(Q) = F
      GO TO 20
   80 CONTINUE
      N = N/F
      P = P + 1
      PP(P) = F
      GO TO 20
C
  100 CONTINUE
      IF (P.LT.1) GO TO 140
      DO 120 J=1,P
         JJ = P + 1 - J
         TFACT(J) = PP(JJ)
         JJ = P + Q + J
         TFACT(JJ) = PP(J)
  120 CONTINUE
  140 CONTINUE
      IF (Q.LT.1) GO TO 180
      DO 160 J=1,Q
         JJ = P + J
         TFACT(JJ) = QQ(J)
  160 CONTINUE
  180 CONTINUE
      JJ = 2*P + Q
      TFACT(JJ+1) = 0
      RFACT(JJ+1) = 0
      DO 200 J=1,JJ
         RFACT(J) = TFACT(J)
  200 CONTINUE
      IF (JJ.EQ.1) RFACT(1) = 0
      PTWO = 1
      J = 0
  220 CONTINUE
      J = J + 1
      IF (TFACT(J).EQ.0) GO TO 260
      IF (TFACT(J).NE.2) GO TO 220
      PTWO = PTWO*2
      TFACT(J) = 1
      IF (PTWO.GE.TWOGRP) GO TO 240
      IF (TFACT(J+1).EQ.2) GO TO 220
  240 CONTINUE
      TFACT(J) = PTWO
      PTWO = 1
      GO TO 220
  260 CONTINUE
      IERROR = 0
      GO TO 320
  280 IERROR = 1
      GO TO 320
C
  300 IERROR = 2
  320 RETURN
      END
** END OF C06FAZTEXT
*UPTODATE C06FCFTEXT
      SUBROUTINE C06FCF(X, Y, PTS, WORK, IFAIL)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     COMPLEX FOURIER TRANSFORM
C     .. SCALAR ARGUMENTS ..
      INTEGER IFAIL, PTS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION WORK(PTS), X(PTS), Y(PTS)
C     ..
C     .. LOCAL SCALARS ..
C$P 1
      CHARACTER SRNAME*6
      DOUBLE PRECISION SQPTS
      INTEGER IERROR, IPTS, PMAX, TWOGRP
C     .. LOCAL ARRAYS ..
      INTEGER RFACT(21), TFACT(21)
C     .. FUNCTION REFERENCES ..
      DOUBLE PRECISION DSQRT
C     *** IMPLEMENTATION DEPENDENT DECLARATION ***
C     DOUBLE PRECISION DBLE
      INTEGER P01AAF
C     .. SUBROUTINE REFERENCES ..
C     C06ECW, C06FAY, C06FAZ
C     ..
C JLP91:
      DATA SRNAME /'C06FCF'/
      DATA PMAX /19/
      DATA TWOGRP /8/
      IF (PTS.LE.1) GO TO 40
      CALL C06FAZ(PTS, PMAX, TWOGRP, TFACT, RFACT, IERROR)
      IF (IERROR.NE.0) GO TO 60
      CALL C06ECW(X, Y, PTS, TFACT)
      CALL C06FAY(X, PTS, RFACT, WORK)
      CALL C06FAY(Y, PTS, RFACT, WORK)
      SQPTS = DSQRT(DBLE(PTS))
      DO 20 IPTS=1,PTS
         X(IPTS) = X(IPTS)/SQPTS
         Y(IPTS) = Y(IPTS)/SQPTS
   20 CONTINUE
      IFAIL = 0
      GO TO 80
C
   40 IERROR = 3
   60 IFAIL = P01AAF(IFAIL,IERROR,SRNAME)
   80 RETURN
      END
** END OF C06FCFTEXT
*UPTODATE C06FFFTEXT
      SUBROUTINE C06FFF(NDIM, L, ND, N, X, Y, WORK, LWORK, IFAIL)
C     MARK 11 RELEASE. NAG COPYRIGHT 1984.
C
C     DISCRETE FOURIER TRANSFORM OF ONE VARIABLE IN A
C     MULTI-VARIABLE SEQUENCE OF COMPLEX DATA VALUES
C
C     .. SCALAR ARGUMENTS ..
      INTEGER IFAIL, L, LWORK, N, NDIM
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION WORK(LWORK), X(N), Y(N)
      INTEGER ND(NDIM)
C     ..
C     .. LOCAL SCALARS ..
C$P 1
      CHARACTER SRNAME*6
      INTEGER I, IFAIL1, NI, NK, NL
C     .. FUNCTION REFERENCES ..
      INTEGER P01AAF
C     .. SUBROUTINE REFERENCES ..
C     C06FFZ
C     ..
      DATA SRNAME /'C06FFF'/
      IF (NDIM.LT.1) GO TO 60
      IF (L.LT.1 .OR. L.GT.NDIM) GO TO 100
      NK = 1
      NI = 1
      DO 20 I=1,NDIM
         IF (ND(I).LT.1) GO TO 120
         IF (I.GT.L) NK = NK*ND(I)
         IF (I.LT.L) NI = NI*ND(I)
   20 CONTINUE
      NL = ND(L)
      IF (NI*NL*NK.NE.N) GO TO 80
      IF (NL.EQ.1) GO TO 40
      IF (LWORK.LT.3*NL) GO TO 160
      CALL C06FFZ(X, Y, NI, NL, NK, WORK, WORK(NL+1), WORK(2*NL+1),
     *IFAIL1)
      IF (IFAIL1.NE.0) GO TO 140
   40 IFAIL = 0
      RETURN
   60 IFAIL1 = 1
      GO TO 180
   80 IFAIL1 = 2
      GO TO 180
  100 IFAIL1 = 3
      GO TO 180
  120 IFAIL1 = 3 + 10*I
      GO TO 180
  140 IFAIL1 = IFAIL1 + 10*L
      GO TO 180
  160 IFAIL1 = 4 + 10*L
  180 IFAIL = P01AAF(IFAIL,IFAIL1,SRNAME)
      RETURN
      END
** END OF C06FFFTEXT
*UPTODATE C06FFZTEXT
      SUBROUTINE C06FFZ(X, Y, NI, NJ, NK, W1, W2, W3, IFAIL)
C     MARK 11 RELEASE. NAG COPYRIGHT 1984.
C
C     DISCRETE FOURIER TRANSFORM OF THE 2ND VARIABLE IN A
C     3-DIMENSIONAL SEQUENCE OF COMPLEX DATA VALUES
C
C     .. SCALAR ARGUMENTS ..
      INTEGER IFAIL, NI, NJ, NK
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION W1(NJ), W2(NJ), W3(NJ), X(NI,NJ,NK), Y(NI,NJ,NK)
C     ..
C     .. LOCAL SCALARS ..
      INTEGER I, J, K
C     .. SUBROUTINE REFERENCES ..
C     C06FCF
C     ..
      DO 80 K=1,NK
         DO 60 I=1,NI
            DO 20 J=1,NJ
               W1(J) = X(I,J,K)
               W2(J) = Y(I,J,K)
   20       CONTINUE
            IFAIL = 1
            CALL C06FCF(W1, W2, NJ, W3, IFAIL)
            DO 40 J=1,NJ
               X(I,J,K) = W1(J)
               Y(I,J,K) = W2(J)
   40       CONTINUE
   60    CONTINUE
   80 CONTINUE
      RETURN
      END
** END OF C06FFZTEXT
*UPTODATE C06FJFTEXT
      SUBROUTINE C06FJF(NDIM, ND, N, X, Y, WORK, LWORK, IFAIL)
C     MARK 11 RELEASE. NAG COPYRIGHT 1984.
C
C     MULTI-DIMENSIONAL DISCRETE FOURIER TRANSFORM OF A
C     MULTI-DIMENSIONAL SEQUENCE OF COMPLEX DATA VALUES
C
C     .. SCALAR ARGUMENTS ..
      INTEGER IFAIL, LWORK, N, NDIM
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION WORK(LWORK), X(N), Y(N)
      INTEGER ND(NDIM)
C     ..
C     .. LOCAL SCALARS ..
C$P 1
      CHARACTER SRNAME*6
      INTEGER IFAIL1, L
C     .. FUNCTION REFERENCES ..
      INTEGER P01AAF
C     .. SUBROUTINE REFERENCES ..
C     C06FFF
C     ..
      DATA SRNAME /'C06FJF'/
      IF (NDIM.LT.1) GO TO 40
      DO 20 L=1,NDIM
         IFAIL1 = 1
         CALL C06FFF(NDIM, L, ND, N, X, Y, WORK, LWORK, IFAIL1)
         IF (IFAIL1.NE.0) GO TO 60
   20 CONTINUE
      IFAIL = 0
      RETURN
   40 IFAIL1 = 1
   60 IFAIL = P01AAF(IFAIL,IFAIL1,SRNAME)
      RETURN
      END
** END OF C06FJFTEXT
*JLP91***********************************
*UPTODATE C06GCFTEXT
      SUBROUTINE C06GCF(Y, PTS, IFAIL)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     COMPLEX CONJUGATE
C     .. SCALAR ARGUMENTS ..
      INTEGER IFAIL, PTS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION Y(PTS)
C     ..
C     .. LOCAL SCALARS ..
C$P 1
      CHARACTER SRNAME*6
      INTEGER IERROR, J
C     .. FUNCTION REFERENCES ..
      INTEGER P01AAF
C     ..
      DATA SRNAME /'C06FCF'/
      IF (PTS.LE.0) GO TO 40
      IERROR = 0
      DO 20 J=1,PTS
         Y(J) = -Y(J)
   20 CONTINUE
      GO TO 60
C
   40 IERROR = 1
   60 IFAIL = P01AAF(IFAIL,IERROR,SRNAME)
      RETURN
      END
** END OF C06GCFTEXT
*UPTODATE C06FAYTEXT
      SUBROUTINE C06FAY(X, PTS, FACTOR, WORK)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     MARK 11 REVISED. IER-444 (FEB 1984).
C     SINGLE COPY REORDERING PROGRAMME
C     EQUIVALENCE (I1,I(1)), (K1,K(1)), (L1,L(1))
C     EQUIVALENCE (I2,I(2)), (K2,K(2)), (L2,L(2))
C     EQUIVALENCE (I3,I(3)), (K3,K(3)), (L3,L(3))
C     EQUIVALENCE (I4,I(4)), (K4,K(4)), (L4,L(4))
C     EQUIVALENCE (I5,I(5)), (K5,K(5)), (L5,L(5))
C     EQUIVALENCE (I6,I(6)), (K6,K(6)), (L6,L(6))
C     EQUIVALENCE (I7,I(7)), (K7,K(7)), (L7,L(7))
C     EQUIVALENCE (I8,I(8)), (K8,K(8)), (L8,L(8))
C     EQUIVALENCE (I9,I(9)), (K9,K(9)), (L9,L(9))
C     EQUIVALENCE (I10,I(10)), (K10,K(10)), (L10,L(10))
C     EQUIVALENCE (K11,K(11))
C     .. SCALAR ARGUMENTS ..
      INTEGER PTS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION WORK(PTS), X(PTS)
      INTEGER FACTOR(21)
C     ..
C     .. LOCAL SCALARS ..
      INTEGER I10, I1, I2, I3, I4, I5, I6, I7, I8, I9, J, JJ, K10,K11,
     * K1, K2, K3, K4, K5, K6, K7, K8, K9, KK, L10, L1, L2,L3, L4, L5,
     * L6, L7, L8, L9, LEVEL, LOOP, NEST
C     .. LOCAL ARRAYS ..
      INTEGER I(20), K(20), L(20)
C     ..
      DATA NEST /20/
      DATA LOOP /10/
      IF (FACTOR(1).EQ.0) GO TO 380
      DO 20 J=1,NEST
         L(J) = 1
         I(J) = 1
   20 CONTINUE
      KK = PTS
      DO 40 J=1,NEST
         IF (FACTOR(J).EQ.0) GO TO 60
         L(J) = KK
         I(J) = KK/FACTOR(J)
         KK = KK/FACTOR(J)
   40 CONTINUE
   60 CONTINUE
C
      L1 = L(1)
      L2 = L(2)
      L3 = L(3)
      L4 = L(4)
      L5 = L(5)
      L6 = L(6)
      L7 = L(7)
      L8 = L(8)
      L9 = L(9)
      L10 = L(10)
      I1 = I(1)
      I2 = I(2)
      I3 = I(3)
      I4 = I(4)
      I5 = I(5)
      I6 = I(6)
      I7 = I(7)
      I8 = I(8)
      I9 = I(9)
      I10 = I(10)
C
      KK = 0
      LEVEL = NEST
      K(LEVEL) = 1
      GO TO 100
   80 CONTINUE
      IF (LEVEL.GE.NEST) GO TO 340
      LEVEL = LEVEL + 1
      K(LEVEL) = K(LEVEL) + I(LEVEL)
      IF (K(LEVEL).GT.L(LEVEL)) GO TO 80
  100 CONTINUE
      LEVEL = LEVEL - 1
      DO 120 J=LOOP,LEVEL
         JJ = LEVEL + LOOP - J
         K(JJ) = K(JJ+1)
  120 CONTINUE
      K11 = K(11)
      DO 320 K10=K11,L10,I10
         DO 300 K9=K10,L9,I9
            DO 280 K8=K9,L8,I8
               DO 260 K7=K8,L7,I7
                  DO 240 K6=K7,L6,I6
                     DO 220 K5=K6,L5,I5
                        DO 200 K4=K5,L4,I4
                           DO 180 K3=K4,L3,I3
                              DO 160 K2=K3,L2,I2
                                 DO 140 K1=K2,L1,I1
                                    KK = KK + 1
                                    WORK(KK) = X(K1)
  140                            CONTINUE
  160                         CONTINUE
  180                      CONTINUE
  200                   CONTINUE
  220                CONTINUE
  240             CONTINUE
  260          CONTINUE
  280       CONTINUE
  300    CONTINUE
  320 CONTINUE
      LEVEL = LOOP
      GO TO 80
  340 CONTINUE
      DO 360 J=1,PTS
         X(J) = WORK(J)
  360 CONTINUE
C
  380 CONTINUE
      RETURN
      END
** END OF C06FAYTEXT
*****************************************************************
*****************************************************************
* X01FTEXT
*UPTODATE X01AAFTEXT
      DOUBLE PRECISION FUNCTION X01AAF(X)
C     NAG COPYRIGHT 1975
C     MARK 4.5 RELEASE
      DOUBLE PRECISION X
C     * PI *
C     RETURNS THE VALUE OF THE CONSTANT PI
C     THE X PARAMETER IS NOT USED
C     FOR DEC VAX 11/780
C     X01AAF = 3.14159265358979324D0
      X01AAF = 3.14159265358979324D0
      RETURN
      END
** END OF X01AAFTEXT
*UPTODATE X01ABFTEXT
      DOUBLE PRECISION FUNCTION X01ABF(X)
C     NAG COPYRIGHT 1975
C     MARK 4.5 RELEASE
      DOUBLE PRECISION X
C     * GAMMA *
C     RETURNS THE VALUE OF EULERS CONSTANT GAMMA WHERE
C     GAMMA = LIM(1+1/2+...+1/N - LOG(N)) AS N TENDS TO INFINITY
C     THE X PARAMETER IS NOT USED
C     FOR DEC VAX 11/780
C     X01ABF = 0.577215664901532861D0
      X01ABF = 0.577215664901532861D0
      RETURN
      END
** END OF X01ABFTEXT
*****************************************************************
*****************************************************************
* P01FTEXT
*UPTODATE P01AAFTEXT
      INTEGER FUNCTION P01AAF(IFAIL, ERROR, SRNAME)
C     MARK 1 RELEASE.  NAG COPYRIGHT 1971
C     MARK 3 REVISED
C     MARK 4A REVISED, IER-45
C     MARK 4.5 REVISED
C     MARK 7 REVISED (DEC 1978)
C     MARK 11 REVISED (FEB 1984)
C     RETURNS THE VALUE OF ERROR OR TERMINATES THE PROGRAM.
      INTEGER ERROR, IFAIL, NOUT
C$P 1
      CHARACTER SRNAME*6
C     TEST IF NO ERROR DETECTED
      IF (ERROR.EQ.0) GO TO 20
C     DETERMINE OUTPUT UNIT FOR MESSAGE
      NOUT=6
C      CALL X04AAF (0,NOUT)
C     TEST FOR SOFT FAILURE
      IF (MOD(IFAIL,10).EQ.1) GO TO 10
C     HARD FAILURE
      WRITE (NOUT,99999) SRNAME, ERROR
C     ******************** IMPLEMENTATION NOTE ********************
C     THE FOLLOWING STOP STATEMENT MAY BE REPLACED BY A CALL TO AN
C     IMPLEMENTATION-DEPENDENT ROUTINE TO DISPLAY A MESSAGE AND/OR
C     ABORT THE PROGRAM.
C     *************************************************************
      STOP
C     SOFT FAIL
C     TEST IF ERROR MESSAGES SUPPRESSED
   10 IF (MOD(IFAIL/10,10).EQ.0) GO TO 20
      WRITE (NOUT,99999) SRNAME, ERROR
   20 P01AAF = ERROR
      RETURN
99999 FORMAT (' ERROR DETECTED BY NAG LIBRARY ROUTINE ', A8,
     * ' - IFAIL = ', I5//)
      END
** END OF P01AAFTEXT
