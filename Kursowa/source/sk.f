       PARAMETER(N=500)
       DIMENSION R(1:N), G_AA(1:N), G_BB(1:N), G_AB(1:N)
       OPEN(01,FILE='../ArAr')
       OPEN(02,FILE='../KrKr')
       OPEN(03,FILE='../ArKr')
       PI = acos(-1.0)
       UL = 38.2813835100
       V=UL**3
       NP = 1000
       CONST = 4.0*PI*NP/V

       DO I=1,N
       READ(01,*) R(I), G_AA(I)
       READ(02,*) R(I), G_BB(I)
       READ(03,*) R(I), G_AB(I)
       ENDDO

       VK_MIN = 2 * PI/R(N)

       OPEN(04,FILE='../SK_AA')
       OPEN(05,FILE='../SK_BB')
       OPEN(06,FILE='../SK_AB')
       OPEN(07,FILE='../SK_TOT')
       DO J = 1,20
       VK = J*VK_MIN

       S_AA = 0.0
       S_BB = 0.0
       S_AB = 0.0

       F_PREV_AA = (G_AA(1)-1.0)*sin(VK*R(1))/VK*R(1)
       F_PREV_BB = (G_BB(1)-1.0)*sin(VK*R(1))/VK*R(1)
       F_PREV_AB = (G_AB(1)-1.0)*sin(VK*R(1))/VK*R(1)

       DO I=2,N
       DX = R(I) - R(I-1)
       F_AA = (G_AA(I)-1.0)*sin(VK*R(I))/VK*R(I)
       F_BB = (G_BB(I)-1.0)*sin(VK*R(I))/VK*R(I)
       F_AB = (G_AB(I)-1.0)*sin(VK*R(I))/VK*R(I)

       S_AA = S_AA + (F_PREV_AA + F_AA)/2 * DX * CONST
       S_BB = S_BB + (F_PREV_BB + F_BB)/2 * DX * CONST
       S_AB = S_AB + (F_PREV_AB + F_AB)/2 * DX * CONST

       F_PREV_AA = F_AA
       F_PREV_BB = F_BB
       F_PREV_AB = F_AB
       ENDDO
       WRITE(04, *) VK, S_AA+1.0
       WRITE(05, *) VK, S_BB+1.0
       WRITE(06, *) VK, S_AB+1.0
       
       SK_TOT = 0.5*(S_AA + 1) + 2*0.25*(S_AB + 1) + 0.5*S_BB
       WRITE(07, *) VK, SK_TOT
       ENDDO
       STOP
       END
