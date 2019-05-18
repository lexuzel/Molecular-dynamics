       REAL TIME, DX_Ar, DX_Kr
       REAL Bk, Temp, Ar_mas, Kr_mas, CONST_Ar, CONST_Kr

       Bk = 1.38E-23
       Temp = 116
       Ar_mas = 6.6358E-26
       Kr_mas = 13.9199E-26

       CONST_Ar = Bk*Temp/Ar_mas * 1.0E-12
       CONST_Kr = Bk*Temp/Kr_mas * 1.0E-12

       OPEN(1,FILE='../DX2_Ar')
       OPEN(2,FILE='../DX2_Kr')

       OPEN(3,FILE='../DXD_Ar')
       OPEN(4,FILE='../DXD_Kr')

       READ(1,*)
       READ(2,*)

       WRITE(3,*) 0.0, 0.0
       WRITE(4,*) 0.0, 0.0
       
       DO I=2,300
       READ(1,*) TIME, DX_Ar
       READ(2,*) TIME, DX_Kr

       WRITE(3,*) TIME, DX_Ar/6/TIME * CONST_Ar
       WRITE(4,*) TIME, DX_Kr/6/TIME * CONST_Kr
       END DO

       CLOSE(1)
       CLOSE(2)
       CLOSE(3)
       CLOSE(4)
       END

