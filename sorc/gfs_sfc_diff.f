      SUBROUTINE GFSDIF(SFCPRS,SFCSPD,SFCTMP,SFCQ, 
     &           SKINT,Z0,Z1I,IVEGSRC,VEGTYPI,SHDFAC, 
     &           SHDMAX,SNWDPH,CM2,CH2)

      use module_sfc_diff
! I/O ARGUMENTS
!     SFCPRS = PRESSURE AT LOWEST MODEL LAYER (SIGMA 0.9973)
      REAL SFCPRS,SFCSPD,SFCTMP,SFCQ,SKINT,Z0,CM2,CH2

! GFS_SFC_DIFF CALL VARIABLES
      INTEGER IM,sfc_z0_type,ivegsrc,vegtypi
      integer vegtyp(1)
      REAL U1,V1,Z1I
      REAL PS(1),T1(1),Q1(1),Z1(1),
     &     TSKIN,Z0RL,CM,CH,RB,
     &     RCL,PRSL1(1),PRSLKI(1),SLIMSK,
     &     STRESS,FM,FH,
     &     WIND(1),DDVEL,FM10,FH2,
     &     tsurf,z0pert(1),ztpert(1),u10m(1),v10m(1),
     &     shdfac,shdmax,sigmaf(1),snwdph
! 1 - land, 2 - ice, 3 - water
      real tskin3(3), tsurf3(3), snwdph3(3),
     &     z0rl3(3), ustar3(3),cm3(3), ch3(3),
     &     rb3(3), stress3(3), fm3(3), fh3(3),
     &     fm103(3), fh23(3)
           
      LOGICAL flag_iter(1),wet(1),dry(1),icy(1),redrag

      IM = 1
      PS(1) = SFCPRS / 1000. / 0.9973 !SFCPRS PASSED IN AS PASCAL, BUT GFS_SFC_DIFF MAKES DOUBLE ADJUSTMENT 
      U1 = SFCSPD
      V1 = 0.
      T1(1) = SFCTMP
      Q1(1) = SFCQ
      Z1(1) = Z1I
      vegtyp(1)=vegtypi
!     TSKIN = SKINT
!     Z0RL = Z0 * 100.             !Z0 IN [M], Z0RL IN [CM]
!     CM = 0.0001
!     CH = 0.0001
!     RB = 0.
      RCL = 1.
      PRSL1(1)  = SFCPRS / 1000.      !SFCPRS PASSED IN AS PASCAL, BUT GFS_SFC_DIFF MAKES DOUBLE ADJUSTMENT
      PRSLKI(1) = 1. / 0.9973 
      SLIMSK = 1. 
!     STRESS = 0.
!     FM = 0.
!     FH = 0.
!     USTAR = 0.
      WIND(1)  = 0.001
      DDVEL = 0.
!     FM10  = 0.
!     FH2   = 0.
!     TSURF = SKINT
      FLAG_ITER = .TRUE.
      wet(1) = .false.
      dry(1) = .true.
      icy(1) = .false.
      redrag = .false.
      sfc_z0_type = 0
      z0pert(1) = 0.
      ztpert(1) = 0.
      u10m(1) = u1
      v10m(1) = v1
      sigmaf(1) = max(shdfac,0.01)

      do n=1,3
       tskin3(n)=SKINT
       tsurf3(n)=SKINT
       snwdph3(n)=snwdph
       z0rl3(n)=Z0 * 100.
       ustar3(n)= 0.
       cm3(n)= 0.0001
       ch3(n)= 0.0001
       rb3(n)= 0.
       stress3(n)= 0.
       fm3(n)= 0.
       fh3(n)= 0.
       fm103(n)= 0.
       fh23(n)= 0.
      enddo
      
      call sfc_diff(im,ps,t1,q1,z1, wind,
     &              prsl1,prslki,
     &              sigmaf,vegtyp,shdmax,ivegsrc,
     &              z0pert,ztpert,
     &              flag_iter,redrag,
     &              u10m,v10m,sfc_z0_type,
     &              wet,dry,icy,
     &              tskin3, tsurf3, snwdph3, z0rl3, ustar3,
     &              cm3, ch3, rb3, stress3, fm3, fh3, fm103, fh23)

      CM2 = CM3(1) * SFCSPD
      CH2 = CH3(1) * SFCSPD

      RETURN
      END   
