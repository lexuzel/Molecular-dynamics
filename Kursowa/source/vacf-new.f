      program build
      INTEGER TIME,TIME_MAX
      PARAMETER (NP = 500, TIME_MAX = 300)
      DIMENSION VX_Ar(TIME_MAX,NP), VX_Kr(TIME_MAX,NP)
      DIMENSION VY_Ar(TIME_MAX,NP), VY_Kr(TIME_MAX,NP)
      DIMENSION VZ_Ar(TIME_MAX,NP), VZ_Kr(TIME_MAX,NP)
      DIMENSION VACF_Ar(TIME_MAX), VACF_Kr(TIME_MAX)
      REAL DT 
      DT=0.002  


      OPEN(01,FILE='../HISTORY')
      OPEN(02,FILE='../VACF_Ar')
      OPEN(03,FILE='../VACF_Kr')

      READ(01,*)
      READ(01,*)

      DO TIME = 1,TIME_MAX
         READ(01,*)
         READ(01,*)
         READ(01,*)
         READ(01,*)

         DO N = 1,NP
            READ(01,*)
            READ(01,*)
                READ(01,*) VX_Ar(TIME,N), 
     .                     VY_Ar(TIME,N),
     .                     VZ_Ar(TIME,N)
            READ(01,*)
            READ(01,*)
                READ(01,*) VX_Kr(TIME,N), 
     .                     VY_Kr(TIME,N), 
     .                     VZ_Kr(TIME,N)
         ENDDO
         PRINT *,'TIME=',TIME
      ENDDO
      
      
      DO TIME = 1,TIME_MAX
         VACF_Ar(TIME) = 0.0
         VACF_Kr(TIME) = 0.0
      ENDDO

      DO TIME = 1,TIME_MAX
         DO N = 1,NP
            VACF_Ar(TIME)=VACF_Ar(TIME) + VX_Ar(1,N)*VX_Ar(TIME,N)
     .                                  + VY_Ar(1,N)*VY_Ar(TIME,N)
     .                                  + VZ_Ar(1,N)*VZ_Ar(TIME,N)
            VACF_Kr(TIME)=VACF_Kr(TIME) + VX_Kr(1,N)*VX_Kr(TIME,N)
     .                                  + VY_Kr(1,N)*VY_Kr(TIME,N)
     .                                  + VZ_Kr(1,N)*VZ_Kr(TIME,N)
         ENDDO
      ENDDO

   99 CONTINUE
 
          READ(01,*,END = 999)
          READ(01,*,END = 999)
          READ(01,*,END = 999)
          READ(01,*,END = 999)
          
          DO TIME = 1, TIME_MAX-1
            DO N = 1, NP
                VX_Ar(TIME,N)=VX_Ar(TIME+1,N)
                VY_Ar(TIME,N)=VY_Ar(TIME+1,N)
                VZ_Ar(TIME,N)=VZ_Ar(TIME+1,N)
 
                VX_Kr(TIME,N)=VX_Kr(TIME+1,N)
                VY_Kr(TIME,N)=VY_Kr(TIME+1,N)
                VZ_Kr(TIME,N)=VZ_Kr(TIME+1,N)
            ENDDO
          ENDDO

         DO N = 1,NP
            READ(01,*,END = 999)
            READ(01,*,END = 999)
                READ(01,*,END = 999) VX_Ar(TIME_MAX,N), 
     .                             VY_Ar(TIME_MAX,N),
     .                             VZ_Ar(TIME_MAX,N)
            READ(01,*,END = 999)
            READ(01,*,END = 999)
                READ(01,*,END = 999) VX_Kr(TIME_MAX,N), 
     .                             VY_Kr(TIME_MAX,N), 
     .                             VZ_Kr(TIME_MAX,N)
         ENDDO
      DO TIME = 1,TIME_MAX
         DO N = 1,NP
            VACF_Ar(TIME)=VACF_Ar(TIME) + VX_Ar(1,N)*VX_Ar(TIME,N)
     .                                  + VY_Ar(1,N)*VY_Ar(TIME,N)
     .                                  + VZ_Ar(1,N)*VZ_Ar(TIME,N)
            VACF_Kr(TIME)=VACF_Kr(TIME) + VX_Kr(1,N)*VX_Kr(TIME,N)
     .                                  + VY_Kr(1,N)*VY_Kr(TIME,N)
     .                                  + VZ_Kr(1,N)*VZ_Kr(TIME,N)
         ENDDO
      ENDDO
      GOTO 99

  999 CONTINUE
         DO TIME = 1,TIME_MAX
            WRITE(2,100)(TIME-1)*5*DT, VACF_Ar(TIME)/VACF_Ar(1)
            WRITE(3,100)(TIME-1)*5*DT, VACF_Kr(TIME)/VACF_Kr(1)
         ENDDO
  100 FORMAT(f8.4,f20.5)
      CLOSE(01)
      CLOSE(02)
      CLOSE(03)
      end program build
