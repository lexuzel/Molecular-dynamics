

        program build
        REAL :: LNG, STEP
        INTEGER :: ITER
        LNG = 38.2813831
        STEP = LNG / 10.0
        ITER = 1
        OPEN(01, FILE='CONFIG')
        WRITE(01, 102) 0, 1, 1000, 0.005
        WRITE(01, 101) LNG, 0.0, 0.0
        WRITE(01, 101) 0.0, LNG, 0.0
        WRITE(01, 101) 0.0, 0.0, LNG
        DO X=1,10
        DO Y=1,10
        DO Z=1,10
        IF((-1.0)**ITER == -1.0) THEN
        WRITE(01, 103)'Ar', ITER
        ELSE 
        WRITE(01, 103)'Kr', ITER
        END IF
        WRITE(01, 104)STEP*X-LNG/2, STEP*Y-LNG/2, STEP*Z-LNG/2
C        WRITE(01, 104) 0.0, 0.0, 0.0
C        WRITE(01, 104) 0.0, 0.0, 0.0
        ITER = ITER + 1
        END DO
        END DO
        END DO

101     FORMAT(F20.8, F20.8, F20.8)
102     FORMAT(I10, I10, I10, F20.8)
103     FORMAT(A2, 6X, I10)
104     FORMAT(F20.8, F20.8, F20.8)
        end program build
