      program build
      INTEGER TIME,TIME_MAX
      PARAMETER (NP = 500, TIME_MAX = 300)
      DIMENSION X_Ar(TIME_MAX, NP), X_Kr(TIME_MAX, NP)
      DIMENSION Y_Ar(TIME_MAX, NP), Y_Kr(TIME_MAX, NP)
      DIMENSION Z_Ar(TIME_MAX, NP), Z_Kr(TIME_MAX, NP)
      DIMENSION DX_Ar(TIME_MAX), DX_Kr(TIME_MAX)
      REAL dx_a, dy_a, dz_a, dx_k, dy_k, dz_k

      OPEN(01,FILE='../HISTORY')
      UL=38.2813835100
      DT=0.002
      T=116.00

      OPEN(02,FILE='../DX2_Ar')
      OPEN(03,FILE='../DX2_Kr')

      READ(01,*)
      READ(01,*)

      DO TIME = 1,TIME_MAX
         READ(01,*)
         READ(01,*)
         READ(01,*)

         DO NUM = 1,NP
            READ(01,*)
            READ(01,*)
                READ(01,*) X_Ar(TIME,NUM), 
     .                     Y_Ar(TIME,NUM),
     .                     Z_Ar(TIME,NUM)
           

            READ(01,*)
            READ(01,*)
                READ(01,*) X_Kr(TIME,NUM), 
     .                     Y_Kr(TIME,NUM), 
     .                     Z_Kr(TIME,NUM)
         ENDDO
         READ(01,*)
      ENDDO
      
      DO TIME = 1,TIME_MAX
         DX_Ar(TIME) = 0.0
         DX_Kr(TIME) = 0.0
      ENDDO

      dx_a = 0.0
      dy_a = 0.0
      dz_a = 0.0
      dx_k = 0.0
      dy_k = 0.0
      dz_k = 0.0
      DO TIME = 1,TIME_MAX
         DO NUM = 1,NP
         IF (TIME.GT.1)
            IF ((X_Ar(TIME,NUM) - X_Ar(TIME-1,NUM)).GT.UL/2.OR.
     .          (X_Ar(TIME-1,NUM) - X_Ar(TIME,NUM)).GT.UL/2)
     .         dx_a = dx_a + (UL - X_Ar(TIME,NUM) - X_Ar(TIME-1,NUM))**2
            ELSE
     .         dx_a = dx_a + ((X_Ar(TIME,NUM) - X_Ar(TIME-1,NUM))**2 
            ENDIF
            IF ((Y_Ar(TIME,NUM) - Y_Ar(TIME-1,NUM)).GT.UL/2.OR.
     .          (Y_Ar(TIME-1,NUM) - Y_Ar(TIME,NUM)).GT.UL/2)
     .         dy_a = dy_a + (UL - Y_Ar(TIME,NUM) - Y_Ar(TIME-1,NUM))**2
            ELSE
     .         dy_a = dy_a + ((Y_Ar(TIME,NUM) - Y_Ar(TIME-1,NUM))**2 
            ENDIF
            IF ((Z_Ar(TIME,NUM) - Z_Ar(TIME-1,NUM)).GT.UL/2.OR.
     .          (Z_Ar(TIME-1,NUM) - Z_Ar(TIME,NUM)).GT.UL/2)
     .         dz_a = dz_a + (UL - Z_Ar(TIME,NUM) - Z_Ar(TIME-1,NUM))**2
            ELSE
     .         dz_a = dz_a + ((Z_Ar(TIME,NUM) - Z_Ar(TIME-1,NUM))**2 
            ENDIF

            IF ((X_Kr(TIME,NUM) - X_Kr(TIME-1,NUM)).GT.UL/2.OR.
     .          (X_Kr(TIME-1,NUM) - X_Kr(TIME,NUM)).GT.UL/2)
     .         dx_k = dx_k + (UL - X_Kr(TIME,NUM) - X_Kr(TIME-1,NUM))**2
            ELSE
     .         dx_k = dx_k + ((X_Kr(TIME,NUM) - X_Kr(TIME-1,NUM))**2 
            ENDIF
            IF ((Y_Kr(TIME,NUM) - Y_Kr(TIME-1,NUM)).GT.UL/2.OR.
     .          (Y_Kr(TIME-1,NUM) - Y_Kr(TIME,NUM)).GT.UL/2)
     .         dy_k = dy_k + (UL - Y_Kr(TIME,NUM) - Y_Kr(TIME-1,NUM))**2
            ELSE
     .         dy_k = dy_k + ((Y_Kr(TIME,NUM) - Y_Kr(TIME-1,NUM))**2 
            ENDIF
            IF ((Z_Kr(TIME,NUM) - Z_Kr(TIME-1,NUM)).GT.UL/2.OR.
     .          (Z_Kr(TIME-1,NUM) - Z_Kr(TIME,NUM)).GT.UL/2)
     .         dz_k = dz_k + (UL - Z_Kr(TIME,NUM) - Z_Kr(TIME-1,NUM))**2
            ELSE
     .         dz_k = dz_k + ((Z_Kr(TIME,NUM) - Z_Kr(TIME-1,NUM))**2 
            ENDIF
        ENDIF
            DX_Ar(TIME)=DX_Ar(TIME) + dx_a + dy_a + dz_a
            DX_Kr(TIME)=DX_Kr(TIME) + dx_k + dy_k + dz_k
         ENDDO
      ENDDO

   99 CONTINUE
          DO TIME = 1, TIME_MAX-1
            DO NUM = 1, NP
                X_Ar(TIME,NUM)=X_Ar(TIME+1,NUM)
                Y_Ar(TIME,NUM)=Y_Ar(TIME+1,NUM)
                Z_Ar(TIME,NUM)=Z_Ar(TIME+1,NUM)
 
                X_Kr(TIME,NUM)=X_Kr(TIME+1,NUM)
                Y_Kr(TIME,NUM)=Y_Kr(TIME+1,NUM)
                Z_Kr(TIME,NUM)=Z_Kr(TIME+1,NUM)
            ENDDO
          ENDDO

          READ(01,*,END = 999)
          READ(01,*,END = 999)
          READ(01,*,END = 999)
          DO NUM = 1,NP
            READ(01,*,END = 999)
            READ(01,*,END = 999)
                READ(01,*,END = 999) X_Ar(TIME_MAX,NUM), 
     .                               Y_Ar(TIME_MAX,NUM),
     .                               Z_Ar(TIME_MAX,NUM)
            READ(01,*,END = 999)
            READ(01,*,END = 999)
                READ(01,*,END = 999) X_Kr(TIME_MAX,NUM), 
     .                               Y_Kr(TIME_MAX,NUM), 
     .                               Z_Kr(TIME_MAX,NUM)
         ENDDO
         READ(01,*,END = 999)
      dx_a = 0.0
      dy_a = 0.0
      dz_a = 0.0
      dx_k = 0.0
      dy_k = 0.0
      dz_k = 0.0
      DO TIME = 1,TIME_MAX
         DO NUM = 1,NP
          IF (TIME.GT.1)
            IF ((X_Ar(TIME,NUM) - X_Ar(TIME-1,NUM)).GT.UL/2.OR.
     .          (X_Ar(TIME-1,NUM) - X_Ar(TIME,NUM)).GT.UL/2)
     .         dx_a = dx_a + (UL - X_Ar(TIME,NUM) - X_Ar(TIME-1,NUM))**2
            ELSE
     .         dx_a = dx_a + ((X_Ar(TIME,NUM) - X_Ar(TIME-1,NUM))**2 
            ENDIF
            IF ((Y_Ar(TIME,NUM) - Y_Ar(TIME-1,NUM)).GT.UL/2.OR.
     .          (Y_Ar(TIME-1,NUM) - Y_Ar(TIME,NUM)).GT.UL/2)
     .         dy_a = dy_a + (UL - Y_Ar(TIME,NUM) - Y_Ar(TIME-1,NUM))**2
            ELSE
     .         dy_a = dy_a + ((Y_Ar(TIME,NUM) - Y_Ar(TIME-1,NUM))**2 
            ENDIF
            IF ((Z_Ar(TIME,NUM) - Z_Ar(TIME-1,NUM)).GT.UL/2.OR.
     .          (Z_Ar(TIME-1,NUM) - Z_Ar(TIME,NUM)).GT.UL/2)
     .         dz_a = dz_a + (UL - Z_Ar(TIME,NUM) - Z_Ar(TIME-1,NUM))**2
            ELSE
     .         dz_a = dz_a + ((Z_Ar(TIME,NUM) - Z_Ar(TIME-1,NUM))**2 
            ENDIF

            IF ((X_Kr(TIME,NUM) - X_Kr(TIME-1,NUM)).GT.UL/2.OR.
     .          (X_Kr(TIME-1,NUM) - X_Kr(TIME,NUM)).GT.UL/2)
     .         dx_k = dx_k + (UL - X_Kr(TIME,NUM) - X_Kr(TIME-1,NUM))**2
            ELSE
     .         dx_k = dx_k + ((X_Kr(TIME,NUM) - X_Kr(TIME-1,NUM))**2 
            ENDIF
            IF ((Y_Kr(TIME,NUM) - Y_Kr(TIME-1,NUM)).GT.UL/2.OR.
     .          (Y_Kr(TIME-1,NUM) - Y_Kr(TIME,NUM)).GT.UL/2)
     .         dy_k = dy_k + (UL - Y_Kr(TIME,NUM) - Y_Kr(TIME-1,NUM))**2
            ELSE
     .         dy_k = dy_k + ((Y_Kr(TIME,NUM) - Y_Kr(TIME-1,NUM))**2 
            ENDIF
            IF ((Z_Kr(TIME,NUM) - Z_Kr(TIME-1,NUM)).GT.UL/2.OR.
     .          (Z_Kr(TIME-1,NUM) - Z_Kr(TIME,NUM)).GT.UL/2)
     .         dz_k = dz_k + (UL - Z_Kr(TIME,NUM) - Z_Kr(TIME-1,NUM))**2
            ELSE
     .         dz_k = dz_k + ((Z_Kr(TIME,NUM) - Z_Kr(TIME-1,NUM))**2 
            ENDIF
        ENDIF
            DX_Ar(TIME)=DX_Ar(TIME) + dx_a + dy_a + dz_a
            DX_Kr(TIME)=DX_Kr(TIME) + dx_k + dy_k + dz_k
        ENDDO
      ENDDO
      GOTO 99

  999 CONTINUE
         DO TIME = 1,TIME_MAX
            WRITE(2,100)(TIME-1)*5*DT, DX_Ar(TIME)/1000
            WRITE(3,100)(TIME-1)*5*DT, DX_Kr(TIME)/1000
         ENDDO
  100 FORMAT(f8.4,f20.5)
      CLOSE(01)
      CLOSE(02)
      CLOSE(03)

      PRINT *,'DIFF_Ar = ', DX_Ar(TIME_MAX) * 1.0E-11 / 
     .                      (30 * DT * (TIME_MAX-1))
      PRINT *,'DIFF_Kr = ', DX_Kr(TIME_MAX) * 1.0E-11 / 
     .                      (30 * DT * (TIME_MAX-1))

      end program build
