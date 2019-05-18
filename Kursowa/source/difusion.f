       program build
       implicit none
       REAL, DIMENSION (:), ALLOCATABLE :: T
       REAL, DIMENSION (:), ALLOCATABLE :: V_Ar
       REAL, DIMENSION (:), ALLOCATABLE :: V_Kr
       REAL :: S_Ar
       REAL :: S_Kr
       REAL :: DX
       REAL :: Bk, Ar_mas, Kr_mas, CONST_Ar, CONST_Kr
       INTEGER :: N, I, Temp
       S_Ar = 0.0
       S_Kr = 0.0
       N = 0
       Temp = 116
       Bk = 1.38E-23
       Ar_mas = 6.6358E-26
       Kr_mas = 13.9199E-26
       CONST_Ar = Bk*Temp/Ar_mas * 1.0E-12
       CONST_Kr = Bk*Temp/Kr_mas * 1.0E-12

       OPEN(01,FILE='../VACF_Ar')
       DO
       READ(01, *, END=10)
       N = N + 1
       END DO
  10   CLOSE(01)

       ALLOCATE (T(N))
       ALLOCATE (V_Ar(N))
       ALLOCATE (V_Kr(N))

       OPEN(01,FILE='../VACF_Ar')
       DO I=1,N
       READ(01, *) T(I), V_Ar(I)
       ENDDO


       OPEN(02,FILE='../VACF_Kr')
       DO I=1,N
       READ(02, *) T(I), V_Kr(I)
       ENDDO

       OPEN(03,FILE='../DIF_Ar')
       OPEN(04,FILE='../DIF_Kr')

       WRITE(03, *) 0.0, 0.0
       WRITE(04, *) 0.0, 0.0

       DO I=2,N
       DX = T(I) - T(I-1)
       S_Ar = S_Ar + (V_Ar(I-1)+V_Ar(I))/2 * DX * CONST_Ar
       S_Kr = S_Kr + (V_Kr(I-1)+V_Kr(I))/2 * DX * CONST_Kr
       WRITE(03, *) T(I), S_Ar
       WRITE(04, *) T(I), S_Kr
       ENDDO

       DEALLOCATE (T)
       DEALLOCATE (V_Ar)
       DEALLOCATE (V_Kr)

       CLOSE(01)
       CLOSE(02)
       CLOSE(03)
       CLOSE(04)
       end program build
