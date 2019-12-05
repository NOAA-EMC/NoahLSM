      PROGRAM  MAIN

      IMPLICIT NONE

C ----------------------------------------------------------------------
C NOAH LAND-SURFACE MODEL, UNCOUPLED 1-D COLUMN: VERSION 2.7.1 July 2004
C
C THIS MAIN PROGRAM AND ITS FAMILY OF SUBROUTINES COMPRISE VERSION 2.7.1
C OF THE PUBLIC RELEASE OF THE UNCOUPLED 1-D COLUMN VERSION OF THE 
C "NOAH" LAND-SURFACE MODEL (LSM). THE NOAH LSM IS A DESCENDANT OF AN 
C EARLIER GENERATION OF THE OREGON STATE UNIVERSITY (OSU) LSM, BUT IT 
C INCLUDES SUBSTANTIAL PHYSICS EXTENSIONS AND RECODING ACCOMPLISHED 
C ALONG THE WAY BY NCEP, HL (NWS), AFGWC, AND AFGL/AFPL/AFRL.  HENCE 
C THE ACRONYM "NOAH" DENOTES N-NCEP, O-OSU, A-AIR FORCE, H-HYDRO LAB.
C ----------------------------------------------------------------------
C FOR DOCUMENTATION OF THIS CODE AND INSTRUCTIONS ON ITS EXECUTION AND
C INPUT/OUTPUT FILES, SEE "NOAH LSM USER'S GUIDE" IN FILE
C README_2.5.2.tx
C IN THE SAME PUBLIC SERVER DIRECTORY AS THIS SOURCE CODE. 
C ----------------------------------------------------------------------
C PROGRAM HISTORY LOG
C VERSION 1.0  --  01 MAR 1999
C VERSION 1.1  --  08 MAR 1999
C VERSION 2.0  --  27 JUL 1999
C VERSION 2.1  --  23 OCT 2000
C VERSION 2.2  --  07 FEB 2001
C VERSION 2.3  --  07 MAY 2001 = operational Eta implementation
C VERSION 2.4  --  27 JUN 2001 = ops Eta with NO physics changes
C VERSION 2.5  --  18 OCT 2001
C   physics changes:
C     in SUBROUTINE REDPRM change CSOIL_DATA from /1.26E+6/ to /2.00E+6/
C     in SUBROUTINE REDPRM change ZBOT_DATA from /-3.0/ to /-8.0/
C     replace de-bugged SUBROUTINE TDFCND
C VERSION 2.5.1--  05 MAR 2002
C VERSION 2.5.2--  31 MAY 2002
C     fix in SUBROUTINE REDPRM related to FRZFACT
C     fix in FUNCTION DEVAP related to FX calculation
C VERSION 2.6 --  
C VERSION 2.6a--  
C VERSION 2.6.1--  
C VERSION 2.7--  
C VERSION 2.7.1--
C VERSION 2.7GFS -- 25 NOV 2019
C ----------------------------------------------------------------------

      CHARACTER*72 CNTRFL, FILENAME, FILENAME2

      LOGICAL FPFLAG

      INTEGER NSOLD
      PARAMETER(NSOLD = 20)
            
      INTEGER ICE
      INTEGER IDAY
      INTEGER IIDAY
      INTEGER IMONTH
      INTEGER IREC1
      INTEGER IREC3
      INTEGER IREC5
      INTEGER IRECD
      INTEGER NOUTDAY
      INTEGER NOUT1
      INTEGER NOUT3
      INTEGER NOUT5
CCCC......INTEGER NOUTRES
      INTEGER NREAD1
      INTEGER NREAD2
      INTEGER NRECL
CCCC......INTEGER NROOT
      INTEGER NROOT
      INTEGER NRUN
      INTEGER NRUN2
      INTEGER NSOIL
      INTEGER SLOPETYP
      INTEGER SOILTYP
      INTEGER IVEGTYP 
      INTEGER ITIME
      INTEGER jday
      INTEGER ITIME_ctl
      INTEGER jday_ctl
      INTEGER jday0
      INTEGER IJ
      INTEGER IJ1
      INTEGER INDI
      INTEGER IBINOUT
      INTEGER NSPINUP
      INTEGER NCYCLES

      LOGICAL L2nd_data
      
CCC......REAL R
CCC......REAL CP
      
CCC......PARAMETER (R = 287.04, CP = 1004.5)

      REAL BETA
      REAL DRIP
      REAL EC
      REAL EDIR
      REAL ET(NSOLD)
      REAL ETNS
      REAL ETT
      REAL ESNOW
      REAL F
CCC......REAL FXEXP
      REAL FLX1
      REAL FLX2
      REAL FLX3
c      REAL RUNOF
      REAL DEW
c      REAL RIB
      REAL RUNOFF3
      REAL SMSCDIF_O
      REAL SMSCDIF
C LW      REAL SIGMA
      REAL LATITUDE
      REAL LONGITUDE
      REAL RUNOFF2
      REAL Q1
      REAL AET
      REAL ALB
      REAL ALBEDO
      REAL CH
      REAL CM
      REAL CMC
      REAL CMC_bef
      REAL DQSDT
CCCC   REAL CZIL
CCCC   REAL REFKDT
      REAL DQSDT2
      REAL DT
C LW      REAL EMISS
      REAL ESAT
      REAL ETA
      REAL ETA_OBS
      REAL ETP
      REAL FUP
      REAL GHF
      REAL H
      REAL H_OBS
      REAL LE
      REAL LWDN
      REAL LW_in
      REAL PRCP
      REAL PTU

      REAL SMCWLT
      REAL SMCDRY
      REAL SMCREF
      REAL SMCMAX
      REAL Par_in
      REAL Par_out
      REAL Q2
      REAL Q2SAT
      REAL RES
      REAL RESTOT
      REAL RH
      REAL RNET
      REAL RUNOFF1
      REAL Rg
      REAL SFCSPD
      REAL SFCPRS
      REAL SFCTMP
      REAL SHDFAC
      REAL SHDMAX
      REAL SHDMIN

      REAL ALBEDOM (13)
      REAL SHDFACM (13)
CCCC...REAL MLAI    (13)
CCCC...REAL XLAI
      REAL SKN_IRT
      REAL SMC(NSOLD)
      REAL STC0(NSOLD,15)
      REAL SMC0(NSOLD,15)
      REAL SH2O0(NSOLD,15)
      REAL CMC0(15)
      REAL SNOWH0(15)
      REAL SNEQV0(15)
      REAL T1_ini(15)
      REAL SNOMLT
      REAL SNOALB
      REAL SOILW
      REAL SOILM
      REAL SOILM_bef
      REAL SOLDN
      REAL STC(NSOLD)
      REAL S
      REAL S_OBS
      REAL T1
      REAL T1_OBS
      REAL T14
      REAL T1V
      REAL T2V
C LW      REAL TAK
      REAL TBOT
      REAL TH2
      REAL TH2V
      REAL T_16
      REAL T_02
      REAL T_32
      REAL T_04
      REAL T_64
      REAL T_08
      REAL Z
CCCC   REAL Z0
      REAL eddyuw
      REAL sm_20
      REAL sm_05
      REAL sm_60
      REAL u_bar
      REAL uprim2
      REAL vprim2
      REAL w_dir
      REAL wet
      REAL wprim2
C
      REAL PCPDAY
      REAL PCPSUM
      REAL ETADAY
      REAL ETSUM
      REAL RUNOFFSUM
      REAL RUNOFFDAY
      REAL SALP
      REAL RSNOW
      REAL SMCMM
      REAL SMCDIF
      REAL SMCNW
      REAL SMC_OBS(NSOLD)
      REAL ETADAY_O
      REAL ETSUM_O
      REAL SMCDIF_O
      REAL SMCMM_O
      REAL SMCNW_O
C 
      REAL SH2O(NSOLD)
      REAL SLDPTH(NSOLD)
      REAL SNEQV
      REAL SNEQV_bef
      REAL LVH2O

      REAL SNCOVR
      REAL SNOWH
      REAL SNUP
      REAL SNUPX(30)
      REAL RSMIN
      REAL XLAI
      REAL RC
      REAL PC
      REAL RCS
      REAL RCT
      REAL RCQ
      REAL RCSOIL

      REAL FFROZP
      REAL TFREEZ
      REAL SOLNET

C ----------------------------------------------------------------------
c declare decimal julian day (0-365), reflected solar
      DOUBLE PRECISION XJDAY
      REAL SOLUP
C ----------------------------------------------------------------------
c declare new variables
      REAL SNDENS
      REAL LSUBS
C ----------------------------------------------------------------------
c added for v2.7gfs
      integer couple
      real sfcems
      integer ivegsrc
      real bexpp
      real xlaip
      logical lheatstrg
      real z0


      PARAMETER (LVH2O = 2.501000E+6)
      DATA SHDMAX /0.8/
      DATA SHDMIN /0.0/
      DATA SNUPX /0.020, 0.020, 0.020, 0.020, 0.020, 0.020,
     &            0.020, 0.020, 0.020, 0.040, 0.040, 0.040,
     &            0.040, 0.040, 0.040, 0.010, 0.013, 0.020,
     &            0.013, 0.020, 0.020, 0.020, 0.020, 0.013,
     &            0.013, 0.013, 0.013, 0.000, 0.000, 0.000/
      DATA SALP /4.0/
      DATA FFROZP /0.0/
      DATA TFREEZ /273.15/
      FPFLAG = .FALSE.

C #############################################################

C MIC$ TASKCOMMON RITE

C ----------------------------------------------------------------------
c open special output files
        OPEN (UNIT=14,FILE='2.7GFS_SEB.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
        OPEN (UNIT=15,FILE='2.7GFS_ET.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
        OPEN (UNIT=16,FILE='2.7GFS_snow.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
        OPEN (UNIT=17,FILE='2.7GFS_tempsC.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
        OPEN (UNIT=18,FILE='2.7GFS_soilmoist.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
        OPEN (UNIT=19,FILE='2.7GFS_forcing.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
C ----------------------------------------------------------------------
C initialize decimal julian day (0-365), 0=1Jan00:00
c        XJDAY = 0.0
        XJDAY = -1800./(24.*3600.)
C ----------------------------------------------------------------------

C -------------------------------------------------------------
C     THERE ARE 10 STEPS IN THIS MAIN PROGRAM DRIVER
C
C DRIVER STEP 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 1. READ CONTROL FILE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C >>>>>>>>>>>>>>>>>>>>>>>>>        <<<<<<<<<<<<<<<<<<<<<<<<<<<
        CNTRFL = 'controlfile_ver_2.7GFS'

C ----------------------------------------------------------------------
      CALL READCNTL(CNTRFL,NCYCLES,L2nd_data,NRUN,NRUN2,
     & DT,NSOIL,NSOLD,Z,SLDPTH,
     & SOILTYP,IVEGTYP,SLOPETYP,ALBEDOM,SHDFACM,SHDMAX,SHDMIN,
     & SNOALB,ICE,TBOT,T1,
     & STC,SMC,SH2O,CMC,SNOWH,SNEQV,FILENAME,FILENAME2,
     & LATITUDE,
     & LONGITUDE,
     & jday_ctl,IBINOUT,
     & ITIME_ctl)
C Parameters check
      IF (NSOLD .LT. 2) THEN
      PRINT*,' '
      PRINT*,'NSOLD MUST BE AT LEAST 2, NSOLD = ', NSOLD
      PRINT*,' ! ! ! ! ! ! ! ! ! ! ! ! ! ! !'
        stop 999
      END IF

      IF (NSOIL .LT. 2) THEN
      PRINT*,' '
      PRINT*,'NSOIL MUST BE AT LEAST 2, NSOIL = ', NSOIL
      PRINT*,' ! ! ! ! ! ! ! ! ! ! ! ! ! ! !'
        stop 999
      END IF

      IF (NSOLD .LT. NSOIL) THEN
      PRINT*,' '
      PRINT*,'NSOLD MUST BE GREATER OR EQUAL THAN NSOIL,'
      PRINT*,' NSOLD = ',NSOLD,'           NSOIL = ',NSOIL
      PRINT*,' ! ! ! ! ! ! ! ! ! ! ! ! ! ! !'
        stop 999
      END IF

C END of Parameters check

c debug print READCNTL output
      PRINT*,'  '
      PRINT*,'-----------------------------'
      PRINT*,'READCNTL output'
      PRINT*,'-----------------------------'
      PRINT*,'  ALBEDOM=',ALBEDOM
      PRINT*,'      CMC=',CMC
      PRINT*,'   CNTRFL=',CNTRFL
CCC    PRINT*,'CZIL=',CZIL
      PRINT*,'       DT=',DT
      PRINT*,' FILENAME=',FILENAME
      PRINT*,' FILENAME2=',FILENAME2
      PRINT*,' L2nd_data=',L2nd_data
      PRINT*,'      ICE=',ICE
      PRINT*,'  IBINOUT=',IBINOUT
      PRINT*,' SLOPETYP=',SLOPETYP
      PRINT*,'  SOILTYP=',SOILTYP
      PRINT*,'ITIME_ctl=',ITIME_ctl
      PRINT*,'  IVEGTYP=',IVEGTYP
      PRINT*,' LATITUDE=',LATITUDE
      PRINT*,'LONGITUDE=',LONGITUDE
CCC    PRINT*,'    MLAI=',MLAI
CCC    PRINT*,'   NROOT=',NROOT
      PRINT*,'  NCYCLES=',NCYCLES
      PRINT*,'     NRUN=',NRUN
      PRINT*,'    NRUN2=',NRUN2
      PRINT*,'    NSOIL=',NSOIL
CCC    PRINT*,'  REFKDT=',REFKDT
      WRITE(*,*)'  SH2O=',(SH2O(IJ) ,IJ=1,NSOIL)
      PRINT*,'  SHDFACM=',SHDFACM
      WRITE(*,*)'SLDPTH=',(SLDPTH(IJ) ,IJ=1,NSOIL)
      WRITE(*,*)'   SMC=',(SMC(IJ) ,IJ=1,NSOIL)
      PRINT*,'    SNEQV=',SNEQV
      PRINT*,'   SNOALB=',SNOALB
      PRINT*,'    SNOWH=',SNOWH
      WRITE(*,*)'      STC=',(STC(IJ) ,IJ=1,NSOIL)
      PRINT*,'       T1=',T1
      PRINT*,'     TBOT=',TBOT
      PRINT*,'        Z=',Z
CCC    PRINT*,'      Z0=',Z0
      PRINT*,' jday_ctl=',jday_ctl
      PRINT*,'-----------------------------'
      PRINT*,' end of READCNTL output '
      PRINT*,'-----------------------------'
      PRINT*,'  '

C DRIVER STEP 2  OPEN INPUT/OUTPUT FILES  <<<<<<<<<<<<<<<<<<<
C 2. OPEN INPUT/OUTPUT FILES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

C >> OPEN FILES CONTAINING ATM. STATION DATA <<<<<
C      NREAD1=25
      NREAD1=29  
      NREAD2=29  
      OPEN(NREAD1,FILE=FILENAME,STATUS='OLD')
C IF YOU RUN A 2nd data file with atmospheric forcing at the end
C (usually after SPIN-UP CYCLE)
      IF (L2nd_data) THEN

      NREAD2=27
        
      OPEN(NREAD2,FILE=FILENAME2,STATUS='OLD')
      
      PRINT*,' '      
      PRINT*,' WILL USE ',FILENAME2
      PRINT*,' AS FORCING FILE IN THE LAST RUN'      
      DO IJ=1,10
       PRINT*,' '
      END DO

      ENDIF
C
C ------------------------------------------------------------
C
c (IBINOUT's must be positive 1 for binary output for GrADS)
c (IBINOUT's must be negative 1 for ascii output intead)
C IBINOUT is set to -1 or +1 in controlfile.
      NOUT1 = 43*IBINOUT
      NOUT3 = 45*IBINOUT
      NOUT5 = 47*IBINOUT
      NOUTDAY= 49*IBINOUT
C DATA WORD LENGTH (RECORD LENGTH, DEPENDS ON THE BINARY SIZE (MEANING
C             DEPENDENT) OF THE REAL VARIABLES TO BE WRITEN FOR GrADS)
C For Single Precision, 
C        SGI100   Linux (RedHat5.2)
C f77... NRECL=1   NRECL=4
C f90... NRECL=4
      NRECL=4
C HYDROLOGY
      IF (NOUT1 .GT. 0) THEN
        IREC1 = 1
        OPEN(UNIT=NOUT1,FILE= 
     & 'HYDRO.GRS',
     & STATUS='REPLACE'
     & ,ACCESS='DIRECT'
     & ,FORM='UNFORMATTED'
     & ,RECL=NRECL
     &)
      ELSEIF (NOUT1 .LT. 0) THEN
        OPEN(UNIT=-NOUT1,FILE= 
     & 'HYDRO.TXT',
     & STATUS='REPLACE')
      END IF
      
C THERMODYNAMICS
      IF (NOUT3 .GT. 0) THEN
        IREC3 = 1
        OPEN(UNIT=NOUT3,FILE=
     & 'THERMO.GRS',
     & STATUS='REPLACE'
     &,ACCESS='DIRECT'
     &,FORM='UNFORMATTED'
     &,RECL=NRECL
     &)
      ELSEIF (NOUT3 .LT. 0) THEN
        OPEN(UNIT=-NOUT3,FILE=
     & 'THERMO.TXT',
     & STATUS='REPLACE')
      END IF
C DEBUG !!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      OPEN(UNIT=113,FILE='FRH2O_Newton.TXT')
C DEBUG !!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C OBSERVATIONAL DATA
      IF (NOUT5 .GT. 0) THEN
        IREC5 = 1
        OPEN(UNIT=NOUT5,
     &  FILE='OBS_DATA.GRS',
     &  STATUS='REPLACE'
     &, ACCESS='DIRECT'
     &, FORM='UNFORMATTED'
     &,RECL=NRECL
     &)
      ELSEIF (NOUT5 .LT. 0) THEN
        OPEN(UNIT=-NOUT5,
     &  FILE='OBS_DATA.TXT',
     &  STATUS='REPLACE')
      END IF

C DAILY ACCUMULATION VALUES
      IF (NOUTDAY .GT. 0) THEN
        IRECD = 1
        OPEN(UNIT=NOUTDAY,
     &  FILE='DAILY.GRS',
     &  STATUS='REPLACE'
     &, ACCESS='DIRECT'
     &, FORM='UNFORMATTED'
     &,RECL=NRECL
     &)
      ELSEIF (NOUTDAY .LT. 0) THEN
        OPEN(UNIT=-NOUTDAY,
     &  FILE='DAILY.TXT',
     &  STATUS='REPLACE')
      END IF
C 
C >>>>>>>> OPENING OUTPUT FILES FINISHED <<<<<<<<<<<<<<<<<<<<<<<<<<<
C ------------------------------------------------------------
C Initialize Day number, summed sfc energy bal residual, 
C  Phota Thermal Unit (PTU)
C
      jday0  = -1
      IIDAY  = 0
      RESTOT = 0.0
      PTU    = 0.0
C
C READ FIRST RECORD OF FORCING TO CHECK THE START DATE <<<<<<<<<<<<
C 
C >>>>>>>>> READPILPS-->READBND - Read Tilden Meyers BND data <<<<<
C First initialize a few variables (only those saved in XOLD array):

       SFCTMP = 0.
       RH = 0.
       SFCPRS = 0.
       Rg = 0.
       LW_in = 0.
       PRCP = 0.
       SKN_IRT = 0.
       u_bar = 0.
C      CALL READBND(jday,  itime, SFCTMP,   RH, SFCPRS,  Rg,
C    * LW_in, PRCP,SKN_IRT,
C    * w_dir, u_bar,
C    * DT, IMONTH,IDAY,NREAD1)
         CALL READBND(jday,  ITIME, SFCTMP,   RH, SFCPRS,  Rg,
     * Par_in, Par_out, RNET, LW_in, GHF,  PRCP,   wet,  SKN_IRT,
     * T_02,  T_04,  T_08,  T_16, T_32, T_64, sm_05,
     * sm_20, sm_60,  w_dir, u_bar,     eddyuw, 
     * uprim2,      vprim2,      wprim2,         H,     LE,
     * DT,IMONTH,IDAY,NREAD1)

C THIS IS FOR THE DAILY WATER BALANCE ------------------
C Initialize daily accum vars after reading control file
      PCPDAY = 0.0
      PCPSUM = 0.0
      ETADAY = 0.0
      ETSUM = 0.0
      ETADAY_O = 0.0
      ETSUM_O = 0.0
      RUNOFFSUM = 0.0
      RUNOFFDAY = 0.0
      SMCMM = 0.0

C ----- DO NOT INITIALIZE THIS INSIDE TIME LOOP ! -------------
C initialize variables for water balance (daily sm differences)
C initial SMCMM from control file !!!!!!!!!!!!!   
C but only 3 layers !!!!!!!!!!!!!   
          do ij = 1,3
        SMCMM = SLDPTH(ij)*1000.*SMC(ij) + SMCMM
          end do
C initial SOILM for 1st timestep water balance          
        SOILM = 0.0
          do ij = 1,NSOIL
        SOILM = SOILM + SLDPTH(ij)*SMC(ij)
          end do          
C initial OBS SMCMM same as from control file 
C but only 3 layers !!!!!!!!!!!!! 
      SMCMM_O = SMCMM
      SMSCDIF_O = 0.0
      SMCNW_O = 0.0
      SMSCDIF = 0.0
      SMCNW = 0.0
C THIS PART ABOVE IS FOR THE DAILY WATER BALANCE -------
C ----- DO NOT INITIALIZE THE ABOVE INSIDE TIME LOOP ! --------
C debug print READBND output
      PRINT*,'  '
      PRINT*,'------------------------------------'
      PRINT*,'print READBND output'
      PRINT*,'------------------------------------'
      PRINT*,'    jday=',jday
      PRINT*,'   ITIME=',ITIME
      PRINT*,'  SFCTMP=',SFCTMP
      PRINT*,'      RH=',RH
      PRINT*,'  SFCPRS=',SFCPRS
      PRINT*,'      Rg=',Rg

      PRINT*,'  Par_in=',Par_in
      PRINT*,' Par_out=',Par_out
      PRINT*,'    RNET=',RNET
      PRINT*,'   LW_in=',LW_in
      PRINT*,'     GHF=',GHF
      PRINT*,'    PRCP=',PRCP
      PRINT*,'     wet=',wet
      PRINT*,' SKN_IRT=',SKN_IRT
      PRINT*,'   T_02=',T_02
      PRINT*,'   T_04=',T_04
      PRINT*,'   T_08=',T_08
      PRINT*,'   T_16=',T_16
      PRINT*,'   T_32=',T_32
      PRINT*,'   T_64=',T_64
      PRINT*,'   sm_05=',sm_05
      PRINT*,'  sm_20=',sm_20
      PRINT*,'  sm_60=',sm_60
      PRINT*,'    w_dir=',w_dir
      PRINT*,'    u_bar=',u_bar
      PRINT*,'   eddyuw=',eddyuw
      PRINT*,'   uprim2=',uprim2
      PRINT*,'   vprim2=',vprim2
      PRINT*,'   wprim2=',wprim2
      PRINT*,'        H=',H
      PRINT*,'       LE=',LE
      PRINT*,'   IMONTH=',IMONTH
      PRINT*,'     IDAY=',IDAY
      PRINT*,'   NREAD1=',NREAD1
      PRINT*,'------------------------------------'
      PRINT*,' end of printed READBND output'
      PRINT*,'------------------------------------'
      PRINT*,'  '
     
      rewind NREAD1
     
C CHECK DATE and time for simulation on first step
      IF ((ITIME_ctl .NE. ITIME) .or. (jday_ctl .NE. jday)) THEN
      PRINT*,'date/time specified in control file does not match
     & initial read from forcing data file, the program stops'
      PRINT*,' '
      PRINT*,'Julian day in control file= ',jday_ctl
      PRINT*,'Julian day in forcing data file= ',jday
      PRINT*,' '
      PRINT*,'Initial time in control file= ',ITIME_ctl
      PRINT*,'Initial time in forcing data file= ',ITIME
       stop 999
      ENDIF

C ---------------------------------------------------------------------|
C INITIALIZE CH, CM (before time loop)
      CH=1.E-4
      CM=1.E-4
C 1998 May 22 0030 (Julian= 142) typical values initialization
C      CH= 0.0150022404
C      CM= 0.0205970779
C ---------------------------------------------------------------------|
C     START SPIN-UP LOOP                                               |
C      
      DO NSPINUP = 1,NCYCLES

C Save the initial Soil moisture 
C (here is the place to save initial conditions, STC,SMC,SH2O)
           DO IJ=1,NSOIL
             STC0(IJ,NSPINUP) = STC(IJ)
             SMC0(IJ,NSPINUP) = SMC(IJ)
             SH2O0(IJ,NSPINUP) = SH2O(IJ)
           END DO  
           CMC0(NSPINUP)   = CMC
           SNOWH0(NSPINUP) = SNOWH
           SNEQV0(NSPINUP) = SNEQV
           T1_ini(NSPINUP) = T1
C DRIVER STEP 3  (time step loop) <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C ---------------------------------------------------------------------|
C ---------------------------------------------------------------------|
C                                                                      |
C  START TIME LOOP                                                     |
C                                                                      |
      DO INDI = 1,NRUN

C ----------------------------------------------------------------------
c increment decimal julian day (0-365)
      XJDAY = XJDAY + DT/(24.*3600.)
C ----------------------------------------------------------------------

C DRIVER STEP 4 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 4. READ FORCING DATA <<<<<<<<<<<<<<<<<<<<<<<<<
C 
C  READ THE REQUIRED ATMOSPHERIC FORCING DATA, AS WELL AS NONREQUIRED
C  (BUT EXTREMELY USEFUL) COMPANION SIMULTANEOUS VALIDATING FLUX-SITE 
C  DATA FROM THE EAST-CENTRAL ILLINOIS OBSERVING FLUX SITE (JUST SOUTH 
C  OF CHAMPAIGN IL, NEAR BONDVILLE IL) OPERATED AND MAINTAINED (WITH 
C  SOME GEWEX/GCIP SUPPORT) BY TILDEN MEYERS OF NOAA/ARL 
C
C  THE SEVEN REQUIRED SURFACE ATMOSPHERIC FORCING VARIABLES REQUIRED BY 
C  THIS LSM AND MOST MODERN-ERA LSMs ARE:

C  1)  AIR TEMPERATURE, 2) AIR HUMIDITY, 3) WIND SPEED, 4) SFC PRESSURE,
C  5)  DOWNWARD SOLAR RAD, 6) DOWNWARD LONGWAVE RAD, AND MOST IMPORTANTLY,
C  7)  PRECIPITATION
C 
C  (NOTE:  FOR THE UNITS REQUIRED OF THE FORCING DATA BY THIS LSM, 
C          SEE COMMENT BLOCK AT BEGINNING OF ROUTINE "SFLX". ROUTINE 
C          "READBND" HERE DOES THE REQUIRED UNITS CONVERSION FOR CALL TO SFLX)
C
C >>>>>>>>> READPILPS-->READBND - Read Tilden Meyers BND data <<<<<

C      CALL READBND(jday,  itime, SFCTMP,   RH, SFCPRS,  Rg,
C    * LW_in, PRCP,SKN_IRT,
C    * w_dir, u_bar,
C    * DT, IMONTH,IDAY,NREAD1)

         CALL READBND(jday,  ITIME, SFCTMP,   RH, SFCPRS,  Rg,
     * Par_in, Par_out, RNET, LW_in, GHF,  PRCP,   wet,  SKN_IRT,
     * T_02,  T_04,  T_08,  T_16, T_32, T_64, sm_05,
     * sm_20, sm_60,  w_dir, u_bar,     eddyuw,
     * uprim2,      vprim2,      wprim2,         H,     LE,
     * DT,IMONTH,IDAY,NREAD1)         

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     
C     SAVE OBSERVATIONS FOR LATER VALIDATION OUTPUT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C Skin Temperature in Kelvin, (T1 IN SFLX)
C      IF (SKN_IRT .GT. -273.) THEN
C      T1_OBS = SKN_IRT + 273.15
C      ELSE
C      T1_OBS = -6999.
C      END IF
C
C >>>>>>>> SIGN CONVENTIONS FOR READ FLUXES <<<<<<<<<<<<<<<<<<
C 
C S_OBS: SOIL HEAT FLUX READ FROM DATA (S IN SFLX)
C       (W M-2: NEGATIVE, IF DOWNWARD FROM SURFACE)
C      S_OBS =  GHF
C
C H_OBS: SENSIBLE HEAT FLUX READ FROM DATA (H IN SFLX)
C    (W M-2: NEGATIVE, IF UPWARD FROM SURFACE)
C      H_OBS =  H
C
C ETA_OBS: LATENT HEAT FLUX READ FROM DATA (ETA IN SFLX)
C    (W M-2: NEGATIVE, IF UPWARD FROM SURFACE)
C      ETA_OBS = LE
C
C SMC_OBS: Soil moisture content (volumetric) FROM DATA
C  (just for water balance purposes) - don't have 4th layer
C       SMC_OBS(1) = sm_05
C       SMC_OBS(2) = sm_20
C       SMC_OBS(3) = sm_60
C       SMC_OBS(4) = 0.0
C
C Keep the day number of the simulation 
        IF (jday0 .NE. jday) THEN
           IIDAY = IIDAY + 1
           jday0 = jday
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   IN A FUTURE VERSION, CONSIDER IMPLEMENTING A PHYSICAL
C   TIME-DEPENDENT PHENOLOGY MODEL, THAT MIGHT CALCULATE A GROWING SEASON
C   GROWTH-STAGE INDEX, SUCH AS PTU = PHOTO THERMAL UNIT, DEPENDENT ON
C   ACCUMULATIVE GROWING SEASON AIR TEMPERATURE AND RADIATION.
C     FOR NOW, INCREMENT PTU IN A DUMMY WAY AS A STUB PLACE HOLDER
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      PTU = PTU + 0.10
C
C DRIVER STEP 5 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 5. INTERPOLATE DAILY LAND SURFACE CHARACTERISTICS FROM MONTHLY 
C    VALUES:
C
C........................... monthly, daily ............           
           CALL MONTH_D(JDAY,ALBEDOM,ALB)
           CALL MONTH_D(JDAY,SHDFACM,SHDFAC)
C...........CALL MONTH_D(JDAY,MLAI   ,XLAI)
           
        END IF
C
C >>>>>>>>>>>>>>>>>>>>>>>>>>  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

C  ###########################################################
C
C                         RADIATION

C The following step (OBTENTION OF LWDN) has been 
C commented out, given that
C the current forcing data file available provides the 
C downward long wave radiative forcing as measured by NOAA's
C SURFRAD site located within 5 Km of the flux/meteorological
C measurements system site.
C 
C OBTENTION OF LWDN <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 
C COMPUTATION OF LWDN (INCOMING LW RADIATION) FROM TAIR AND Q:
C   
C...............LWDN = EMISS*SIGMA*(TAK)^4.   
C
C WHERE:   TAK = AIR TEMP IN KELVIN
C        EMISS = 0.7  OR  (IDSO AND JACKSON, 1969): 
C 
C        EMISS = (1 - 0.261 EXP(-7.77*10^(-4)X(273-TAK)^2)
C
C      NEED STEFAN-BOLTZMANN CONSTANT, SIGMA 
C         SIGMA = 5.672 * 10^-8  W M^-2 T^-4
C
C           SIGMA = 5.672E-8
C           TAK = SFCTMP
C           EMISS = 1 - 0.261*EXP((-7.77E-4)*(273-TAK)^2.)
C
C           LWDN = EMISS*SIGMA*TAK^4.

C  ###########################################################

C DRIVER STEP 6 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<          
C 6. ASSIGN DOWNWARD SOLAR AND LONGWAVE RADIATION <<<<<<<<<<<<<

C Using LW_in READ FROM DATA instead of LWDN calculated

      LWDN = LW_in      

      SOLDN = Rg

C DRIVER STEP 7 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 7. CALCULATE A SATURATION MIX RATIO (Q2SAT) <<<<<<<<<<<<<<<<
C          
                    
C NEED Q2 (FROM REL.HUMID.) USE SUBROUTINE QDATAP

          CALL QDATAP(SFCTMP,SFCPRS,RH,Q2,Q2SAT, ESAT)

          IF (Q2 .LT. 0.1E-5) Q2 = 0.1E-5

          IF (Q2 .GE.  Q2SAT) Q2 = Q2SAT*0.99
                  
C CALCULATE SLOPE OF SAT SPECIFIC HUMIDITY CURVE FOR PENMAN: DQSDT2
          DQSDT2 = DQSDT (SFCTMP, SFCPRS)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  CALC VIRTUAL TEMPS AND POTENTIAL TEMPS AT GRND (SUB 1) AND AT
C  THE 1ST MDL LVL ABV THE GRND (SUB 2). EXPON IS CP DIVD BY R.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      TH2 = SFCTMP + ( 0.0098 * Z )
      T2V = SFCTMP * (1.0 + 0.61 * Q2 )
             
      T1V  =    T1 * (1.0 + 0.61 * Q2 )
      TH2V =   TH2 * (1.0 + 0.61 * Q2 )          

C
C DRIVER STEP 8 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 8. CALCULATE CH (EXCHANGE COEFFICIENT) <<<<<<<<<<<<<<<<<<<<<

C SFCSPD = sqrt(u*u+v*v), if individual (u,v) components provided,
C but Tilden Meyers data provides wind speed as u_bar.

          SFCSPD = u_bar
          
CC!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C 
C IMPORTANT NOTE: TO CALCULATE THE SFC EXCHANGE COEF (CH) FOR HEAT AND
C                 MOISTURE, SUBROUTINE SFCDIF BELOW CAN
C                
C    A)  BE CALLED HERE FROM THE DRIVER, THUS CH IS INPUT TO SFLX
C           (AS IS TYPICAL IN A COUPLED ATMOSPHERE/LAND MODEL) OR
C
C    B)  BE CALLED INTERNALLY IN ROUTINE SFLX (THUS CH IS OUTPUT FROM SFLX),
C        THEREIN BETWEEN THE CALLS TO ROUTINES "REDPRM" AND "PENMAN"
C
C    OPTION B IS THE DEFAULT HERE.  THAT IS, IN THE UNCOUPLED, OFF-LINE LSM    
C    REPRESENTED HEREIN BY THIS DRIVER, WE CALL SFCDIF LATER IN ROUTINE SFLX.
C
C  THE ROUTINE SFCDIF REPRESENTS THE SO-CALLED "SURFACE LAYER" OR THE
C  "CONSTANT FLUX LAYER" (THE LOWEST 20-100 M OF THE ATMOSPHERE).
C  HENCE ROUTINE SFCDIF EMBODIES THE "ATMOSPHERIC AERODYNAMIC RESISTANCE".
C
C	ONE CLASS OF USER THAT OPTION B IS INTENDED FOR IS THAT USER WHO MIGHT
C    WANT TO CONSTRUCT HIS/HER OWN DRIVER CODE FROM SCRATCH AND ONLY WANTS TO
C    WORRY ABOUT THE INPUT OF FORCING AND INITIAL STATE VARIABLES AND NOT
C    HAVE TO CONSIDER THE CHOICE OF THE SURFACE LAYER PHYSICS.
C
C    TO ENABLE THE FLEXIBILITY OF EITHER OPTION A OR B, WE PASS 
C    THE ARGUMENTS "CH", "CM", AND "SFCSPD" (WIND SPEED:JUST CALCULATED ABOVE)
C    TO ROUTINE SFLX TO SUPPORT OPTION B -- THAT IS, FOR INPUT TO THE CALL TO
C    ROUTINE SFCDIF THEREIN.  IN OPTION A, THE ARGUMENTS "SFCSPD" AND "CM"
C    ARE NEITHER NEEDED IN ROUTINE SFLX, NOR ALTERED BY ROUTINE SFLX. 
C
C    CH IS THE SFC EXCHANGE COEFFICIENT FOR HEAT/MOISTURE
C    CM IS THE SFC EXCHANGE COEFFICIENT FOR MOMENTUM
C
C    IF ONE CHOOSES OPTION A, THEN ONE MUST 
C      1 - ACTIVATE (UNCOMMENT) THE CALL TO SFCDIF BELOW,
C      2 - ACTIVATE (UNCOMMENT) THE ASSIGNMENT OF "Z0" AND "CZIL" NEXT BELOW
C      3 - DE-ACTIVATE (COMMENT OUT) THE CALL TO SFCDIF IN ROUTINE SFLX.
C
C    Z0 = (use value set in routine REDPRM for veg class given in control file
C    CZIL = (use value set in routine REDPRM)
C
C    THE ROUGHNESS LENGTH PARAMETERS "Z0" AND "CZIL" MUST BE SET HERE IN THE
C    DRIVER TO SUPPORT THE "OPTION-A", I.E. THE CALL TO SFCDIF BELOW.  IN SO
C    DOING, THE "Z0" AND "CZIL" ASSIGNED HERE MUST CORRESPOND TO THEIR VALUES
C    NORMALLY ASSIGNED IN ROUTINE REDPRM, CALLED FROM SFLX JUST BEFORE CALL
C    SFCDIF.  THUS THE VALUE OF "Z0" ASSIGNED HERE MUST CORRESPOND TO THAT
C    ASSIGNED IN ROUTINE REDPRM FOR THE CHOSEN VEG CLASS THAT WAS ALREADY
C    INPUT FROM THE READ OF THE CONTROL FILE EARLIER IN THIS DRIVER.
C
C    BECAUSE OF THE IMPLICIT ITERATIVE NATURE OF THE "PAULSON" SURFACE-LAYER
C    SCHEME USED IN ROUTINE SFCDIF, CH AND CM ARE CO-DEPENDENT.  SIMILARLY,
C    THE IMPLICIT NATURE OF THE SFCDIF SCHEME ALSO REQUIRES THAT FOR EITHER
C    OPTION A OR B, CH AND CM MUST BE INITIALIZED EARLIER IN THE DRIVER BEFORE
C    THE START OF THE TIME-STEP LOOP, AS WELL AS BE CARRIED FORWARD FROM
C    TIME STEP TO TIME STEP AS "STATE VARIABLES", BECAUSE THE VALUES OF 
C    CH AND CM FROM A PREVIOUS TIME STEP REPRESENT THE FIRST-GUESS VALUES FOR
C    THE CALL TO SFCDIF IN THE PRESENT TIME STEP. 
C
C    SOME USERS MAY CHOOSE TO EXECUTE AN ENTIRELY DIFFERENT SCHEME IN PLACE OF 
C    ROUTINE SFCDIF HERE, E.G. AN EXPLICIT SCHEME SUCH AS LOUIS (1979) THAT 
C    EMPLOYS NO ITERATION AND HAS NO REQUIREMENT TO CARRY CH AND CM FORWARD
C    AS STATE VARIABLES FROM TIME STEP TO TIME STEP.  IN THAT CASE, IN
C    OPTION A, THE ROUTINE SHOULD BE CALLED HERE IN THE DRIVER AFTER ALL 
C    NECESSARY INPUT ARGUMENTS FOR IT ARE DEFINED AT THIS POINT, OR CALLED IN 
C    ROUTINE SFLX, AT THE POINT SFCDIF IS CALLED.
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCC...CALL SFCDIF ( Z, Z0, T1V, TH2V, SFCSPD,CZIL, CM, CH )
C
C Water balance: save total column soil moisture and water equiv snow
      SOILM_bef = SOILM
      SNEQV_bef = SNEQV
        CMC_bef = CMC

C DRIVER STEP 8a <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C determine precip type

      FFROZP = 0.0
      IF (PRCP .GT. 0.0) THEN
        IF (.NOT. FPFLAG) THEN
          IF (SFCTMP .LE. TFREEZ) THEN
            FFROZP = 1.0
          ENDIF
        ENDIF
      ENDIF

C DRIVER STEP 8b <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C determine net incoming solar
      IF (SNEQV .EQ. 0.0) THEN
        ALBEDO = ALB
      ELSE
        SNUP=SNUPX(IVEGTYP)
C       CALL SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR)
          IF (SNEQV .LT. SNUP) THEN
            RSNOW = SNEQV/SNUP
            SNCOVR = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
          ELSE
            SNCOVR = 1.0
          ENDIF

        ALBEDO = ALB + SNCOVR*(SNOALB-ALB)
        IF (ALBEDO .GT. SNOALB) ALBEDO=SNOALB
      ENDIF

      SOLNET = SOLDN*(1.0-ALBEDO)

C DRIVER STEP 9 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 9. CALL LAND-SURFACE PHYSICS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C
       couple = 0  
       sfcems=1.     
       ivegsrc=1
       bexpp=0
       xlaip=0
       lheatstrg=.false.
       z0=0


      call sflx (
C  ---  inputs:
     &      nsoil, couple, ice, ffrozp, dt, z, sldpth,  
     &       SOLDN, solnet, lwdn, sfcems, sfcprs, sfctmp,
     &       sfcspd, prcp, q2, q2sat, dqsdt2, th2, ivegsrc,
     &       IVEGTYP, SOILTYP, SLOPETYP, SHDMIN, alb, SNOALB,
     &       bexpp, xlaip,
     &       lheatstrg,shdmax,
C  ---  input/outputs:
     &       tbot, cmc, T1, STC, SMC, SH2O, SNEQV, CH, CM,
     &       z0,
C  ---  outputs:
     &       nroot, shdfac, snowh, albedo, eta, H, ec,   
     &       edir, et, ett, esnow, drip, dew, beta, etp, S, 
     &       flx1, flx2, flx3, runoff1, runoff2, runoff3,
     &       snomlt, sncovr, rc, pc, rsmin, xlai, rcs, rct, rcq,
     &       rcsoil, soilw, soilm, smcwlt, smcdry, smcref, smcmax)


C 
      AET = ETA

C CALCULATE UPWARD LONGWAVE RAD USING UPDATED SKIN TEMPERATURE

      T14 = T1 * T1 * T1 * T1
      FUP = 5.67E-8 * T14

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  CALCULATE RESIDUAL OF ALL SURFACE ENERGY BALANCE EQN TERMS.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      S = -S

      F = SOLDN*(1.0-ALBEDO) + LWDN 
C
      RES    = F - H - S - AET - FUP - FLX1 - FLX2 - FLX3
      RESTOT = RESTOT + RES
                            
C ----------------------------------------------------------------------
      IF (SNEQV .GT. 0) THEN
        SNDENS=SNEQV/SNOWH
      ELSE
        SNDENS=0.0
      ENDIF
      SOLUP=SOLDN*ALBEDO
      ETNS=EC+EDIR+ETT
      write(14,1001) xjday,SOLDN,SOLUP,LWDN,FUP,H,AET,S,FLX1,FLX2,FLX3,
     &               RES
      write(15,1002) xjday,ETP,ETA,ESNOW,ETNS,EC,EDIR,ETT,ET(1),ET(2),
     &               ET(3),ET(4)
      write(16,1003) xjday,SNEQV,SNOWH,SNDENS,SNCOVR,ALBEDO
      write(17,1004) xjday,SFCTMP-273.15,T1-273.15,STC(1)-273.15,
     &               STC(2)-273.15,STC(3)-273.15,
     &               STC(4)-273.15,TBOT-273.15
      write(18,1005) xjday,SMC(1),SMC(1)-SH2O(1),SMC(2),SMC(2)-SH2O(2),
     &               SMC(3),SMC(3)-SH2O(3),SMC(4),SMC(4)-SH2O(4),CMC
C xjday  = decimal julian day, 0.00 = 0:00 1 Jan
C SOLDN  = incoming solar (W/m2)
C LWDN   = downward longwave (W/m2)
C SFCPRS = surface pressure (mb)
C PRCP   = precipitation (mm/30min)
C SFCTMP = air temperature (C)
C Q2     = specific humidity (g/kg)
C SFCSPD = wind speed (m/s)
      write(19,1006) xjday,SOLDN,LWDN,SFCPRS/100.,PRCP*1800.,
     &               SFCTMP-273.15,Q2*1000.,SFCSPD
 1001 format(F10.6,10F10.3,F12.6)
 1002 format(F10.6,10F10.3)
c 1003 format(F10.6,5F8.4)
 1003 format(F10.6,5F10.6)
 1004 format(F10.6,7F10.4)
 1005 format(F10.6,8F10.6)
 1006 format(F10.6,2F10.2,5F10.3)
C ----------------------------------------------------------------------

         IF ((INDI .LT. 50) .OR. (MOD(INDI,50) .EQ. 0)) THEN
C Debug      PRINT*,' CALCULATE RESIDUAL OF ALL SURFACE ENERGY'
      PRINT*,'  --------------------------------------'
C...PRINT*,'S=',S,' RES=',RES,'  RESTOT=',RESTOT,'  TIMESTEP=',INDI
      PRINT*,' RES=',RES,'  TIMESTEP=',INDI
      PRINT*,' RES/(S+ETA) (in %)=',100*RES/(S+ETA),'%'
         ENDIF

C WILL OUTPUT RESULTS ONLY IF LAST CYCLE (WITH DIFFERENT DATA FILE 
C OPTION ON) OR IF ONLY ONE DATAFILE IS USED.

      IF ((NREAD1 .EQ. NREAD2) .OR. (.NOT. L2nd_data)) THEN
      
C ACCUMULATE DAILY VALUES IN mm FOR WATER BALANCE
      PCPSUM = PCPSUM + PRCP*DT
      ETSUM = ETSUM + ETA*DT/LVH2O
C      ETSUM_O = ETSUM_O + ETA_OBS*DT/LVH2O
      RUNOFFSUM = RUNOFFSUM + RUNOFF1*DT*1000. + RUNOFF2*DT*1000.

        IF (ITIME .EQ. 0) THEN
C Remember to initialize SMCMM, SMCDIF... after reading controlfile.
C Suffix "_O" stands for OBSERVED quantities.
        SMCNW = 0.0
        SMCNW_O =0.0
          do ij = 1,3
        SMCNW = SLDPTH(ij)*1000.*SMC(ij) + SMCNW
C        SMCNW_O = SLDPTH(ij)*1000.*SMC_OBS(ij) + SMCNW_O
          end do
        SMCDIF = SMCNW - SMCMM
        SMCMM = SMCNW
        SMCDIF_O = SMCNW_O - SMCMM_O
        SMCMM_O = SMCNW_O
        RUNOFFDAY = RUNOFFSUM
        PCPDAY = PCPSUM
        ETADAY = -ETSUM
C        ETADAY_O = -ETSUM_O
        PCPSUM = 0.0
        ETSUM = 0.0
        ETSUM_O = 0.0
        RUNOFFSUM = 0.0
C
C DRIVER STEP 10 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 10. Write to model output files

C  EXTENSION .GRS FILES - GrADS-readable format output (unformatted)
C  EXTENSION .TXT FILES - ASCII formatted.

C
C  WRITE DAILY ACCUMULATION VARIABLES, DAILY.GRS, GrADS FORMAT
C                                   or DAILY.TXT, ASCII formatted
C
C      CALL PRTDAILY(NOUTDAY,jday,
C     & IIDAY,ITIME,ETADAY,ETADAY_O,
C     & PCPDAY,SMCDIF,SMCDIF_O,RUNOFFDAY,
C     & IRECD)

        ENDIF

C
C WRITE VARIABLES IN WATER AMOUNT UNITS (HYDRO.GRS or .TXT)
C
      CALL PRTHYDF(INDI,NOUT1,NSOIL,jday,
     & IIDAY,ITIME,ETA,
     & ETP,PRCP,SH2O,SMC,ALBEDO,ALB,SNOALB,
     & SNEQV, SNEQV_bef, SNOMLT, SNOWH, SOILM, SOILM_bef, SOILW,
     & RUNOFF1, RUNOFF2, RUNOFF3,
     & DT,EDIR,EC,ETT,CMC,CMC_bef,IREC1,DEW)
C
C  WRITE VARIABLES IN THERMODYNAMIC ENERGY UNITS (THERMO.GRS or .TXT)
C
      CALL PRTHMF(INDI,NOUT3,NSOIL,jday,
     &          IIDAY,ITIME,CH,CM,Z,F,S,CMC,
     &          SMCMAX,SFCTMP,T1,Q1,SFCPRS,
     &          SFCSPD,ETA,ETP,STC,
     &          DT,H,AET,RES,IREC3,
     &          FLX1,FLX2,FLX3)
C  
C WRITE ATMOSPHERIC INPUT FORCING DATA AND INDEPENDENT VALIDATION DATA 
C IN FILE (BND_DATA.GRS or .TXT)
C
C      CALL PRTBND(INDI, NOUT5,   jday,
C    & IIDAY,  ITIME,   SFCTMP,  Q2,     SFCPRS, Rg,
C    * LW_in,  PRCP,
C    *  w_dir, u_bar,
C    * IMONTH, IDAY, IREC5)
      CALL PRTBND(INDI, NOUT5,   jday,
     & IIDAY,  ITIME,   SFCTMP,  Q2,     SFCPRS, Rg,
     * LW_in,  Par_in,  Par_out, RNET,   S_OBS,  PRCP,  wet, T1_OBS,
     * T_02,  T_04,   T_08,   T_16, T_32, T_64,
     * sm_05, sm_20, sm_60, w_dir,  u_bar,  eddyuw,
     * uprim2, vprim2,  wprim2,  H_OBS,  ETA_OBS,
     * IMONTH, IDAY,    IREC5)

      ENDIF
C
C =============================================================
      END DO
C                                                                      |
C   END OF TIME LOOP                                                   |
C                                                                      |
C ---------------------------------------------------------------------|
C ---------------------------------------------------------------------|
C Debug      PRINT*,' CALCULATE RESIDUAL OF ALL SURFACE ENERGY'
      PRINT*,'  --------------------------------------'
C...PRINT*,'S=',S,' RES=',RES,'  RESTOT=',RESTOT,'  TIMESTEP=',INDI
      PRINT*,'SUM OF ALL TIMESTEPS ENERGY RESIDUAL=',RESTOT,
     &'  TIMESTEPS=',INDI*NSPINUP
      PRINT*,'  SPIN-UP CYCLE ',NSPINUP,' of ',NCYCLES
      PRINT*,' AVG RES PER TIMESTEP =',RESTOT/(INDI*NSPINUP)
      PRINT*,'  --------------------------------------'
      PRINT*,'  '

C Reset forcing data for next cycle, if any.
      rewind NREAD1

C IF "FINAL RUN WITH DIFFERENT FORCING DATA" SELECTED, SWITCH NREAD1
C TO POINT TOWARDS THIS FILE AND RUN A NUMBER OF SIMULATION TIMESTEPS
      IF (L2nd_data .AND. (NSPINUP .EQ. NCYCLES-1)) then
      NREAD1=NREAD2
      NRUN=NRUN2
      ENDIF 

      END DO                     
C   END OF SPIN-UP CYCLE LOOP    
C ---------------------------------------------------------------------|
C ---------------------------------------------------------------------|
       IF (NCYCLES .GT. 1) THEN
      PRINT*,'  '
      PRINT*,'  Initial Soil Moisture Change Due to each Spin-Up cycle:'
      PRINT*,'  '
       ENDIF
C-----------------------------------------------------------------------
           DO NSPINUP=1,NCYCLES
C-----------------------------------------------------------------------
      IF (NSPINUP .LT. NCYCLES) THEN
      PRINT*,'  '
      PRINT*,'       LAYER      Run           Next        Diff SMC-SMC0'
       DO IJ=1,NSOIL
       PRINT*,IJ,SMC0(IJ,NSPINUP),SMC0(IJ,NSPINUP+1),(
     &         SMC0(IJ,NSPINUP+1)-SMC0(IJ,NSPINUP))
       END DO
      ENDIF
C-----------------------------------------------------------------------
      PRINT*,'  '
      PRINT*,'  --------------------------------------'
      PRINT*,'  This cycle initial conditions:'
      PRINT*,'  '
      WRITE(*,*) T1_ini(NSPINUP),
     &'  T1........Initial skin temperature (K)'
      WRITE(*,*)(STC0(IJ,NSPINUP), IJ=1,NSOIL),'STC'
      WRITE(*,*)(SMC0(IJ,NSPINUP), IJ=1,NSOIL),'SMC'
      WRITE(*,*)(SH2O0(IJ,NSPINUP), IJ=1,NSOIL),'SH2O'
      WRITE(*,*) CMC0(NSPINUP),
     &'  CMC.......Initial canopy water content (m)'
      WRITE(*,*) SNOWH0(NSPINUP),
     &'  SNOWH.....Initial actual snow depth (m)'
      WRITE(*,*) SNEQV0(NSPINUP),
     &'  SNEQV.....Initial water equiv snow depth (m)'
      PRINT*,'  --------------------------------------'
           END DO
CCC   PRINT*,'     ^      ^      ^      ^      ^      ^ '
      PRINT*,'   --- Above are the last initial conditions used ---'
      PRINT*,' --------------------------------------------------------'
      PRINT*,'  '
      PRINT*,'  '
      PRINT*,' --------------------------------------------------------'
      PRINT*,'  Difference in initial SMC to be obtained if running '
      PRINT*,'  another cycle:'
      PRINT*,'       LAYER    This run        Next        Diff SMC-SMC0'
              DO IJ=1,NSOIL
               PRINT*,IJ,SMC0(IJ,NCYCLES),SMC(IJ),(
     &                 SMC(IJ)-SMC0(IJ,NCYCLES))
              END DO
      PRINT*,'  '
      PRINT*,'  --------------------------------------'
      PRINT*,'  State Variables at the end'
      PRINT*,'  --------------------------------------'
C      PRINT*,'Final date: '
      PRINT*,'Julian=',JDAY,'   MONTH=',IMONTH,'   DAY=',IDAY,
     &'   TIME(hhmm)=',ITIME
      PRINT*,'  --------------------------------------'
      WRITE(*,*) T1,'  T1........Skin temperature (K)'
      WRITE(*,*)(STC(IJ1), IJ1=1,NSOIL),' STC'
      WRITE(*,*)(SMC(IJ1), IJ1=1,NSOIL),' SMC'
      WRITE(*,*)(SH2O(IJ1), IJ1=1,NSOIL),' SH2O'
      WRITE(*,*) CMC,'  CMC.......Canopy water content (m)'
      WRITE(*,*) SNOWH,'  SNOWH.....Actual snow depth (m)'
      WRITE(*,*) SNEQV,'  SNEQV.....Water equiv snow depth (m)'
      WRITE(*,*) 'CH=',CH,'   CM=',CM
      PRINT*,'  --------------------------------------'
C      PRINT*,'  '
      PRINT*,'  Do not confuse this with the initial conditions'
      PRINT*,'  used for the last run (further above).'
      PRINT*,'  This data is useful for initializing continuation runs'
      PRINT*,'  (if you stop, say, 1998May220030, you can use this'
      PRINT*,'  to initialize from this date/time)'
      PRINT*,'  '
C Close open files 
      IF (NOUT1 .GT. 0) THEN
        CLOSE(NOUT1)
      ELSEIF (NOUT1 .LT. 0) THEN
        CLOSE(-NOUT1)
      END IF
      IF (NOUT3 .GT. 0) THEN
        CLOSE(NOUT3)
      ELSEIF (NOUT3 .LT. 0) THEN
        CLOSE(-NOUT3)
      END IF
      IF (NOUT5 .GT. 0) THEN
        CLOSE(NOUT5)
      ELSEIF (NOUT5 .LT. 0) THEN
        CLOSE(-NOUT5)
      END IF
      IF (NOUTDAY .GT. 0) THEN
        CLOSE(NOUTDAY)
      ELSEIF (NOUTDAY .LT. 0) THEN
        CLOSE(-NOUTDAY)
      END IF
      IF (L2nd_data) THEN
        CLOSE(NREAD2 )
      ENDIF
      CLOSE(NREAD1 )

      STOP 0
C END OF DRIVER PROGRAM ----------------------------------------------
      END
      FUNCTION DQS (T) 


      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  PURPOSE:  TO CALCULATE VALUES OF VAPOR PRESSURE (E)
CC            AND P * DQS/DT (P TIMES CHG IN SAT MXG RATIO WITH RESPECT
CC            TO THE CHG IN TEMP) IN SUBSTITUTION TO THE LOOK-UP TABLES.
CC
CC            SUBSTITUTES LOOK-UP TABLES ASSOCIATED WITH THE DATA
CC            BLOCK  /CHMXR/ .
CC
CC            FORMULAS AND CONSTANTS FROM ROGERS AND YAU, 1989.
CC
CC                         ADDED BY PABLO J. GRUNMANN, 6/30/97.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      REAL DESDT
      REAL DQS
CK    REAL ESD
      REAL LW
      REAL T
      REAL ES
C
CK    REAL CP
CK    REAL CV
CK    REAL CVV
      REAL CPV
      REAL RV
      REAL CW
      REAL EPS
      REAL ESO
      REAL TO
      REAL LVH2O

      PARAMETER (LVH2O = 2.501000E+6)
C
CK    PARAMETER  (CP = 1005.)
CK    PARAMETER  (CV = 718.)
CK    PARAMETER (CVV = 1410.)
      PARAMETER (CPV = 1870.)
      PARAMETER  (RV = 461.5)
      PARAMETER ( CW = 4187.)
      PARAMETER (EPS =  0.622)
      PARAMETER (ESO =  611.2)
      PARAMETER ( TO =  273.15)
C 
C     ABOUT THE PARAMETERS:
C      
C     EPS ---------- WATER - DRY AIR MOLECULAR MASS RATIO, EPSILON
C      
C   VALUES FOR SPECIFIC HEAT CAPACITY AND INDIVIDUAL GAS CONSTANTS 
C   IN [JOULES/(KG*KELVIN)] UNITS.
C
C     DRY AIR: 
C             CP, CV
C     WATER VAPOR:
C                 CVV = 1410. 
C                 CPV = 1870.
C                 RV  =  461.5
C     LIQUID WATER:
C                  CW = 4187.
C
C     ESO = ES(T=273.15 K) = SAT. VAPOR PRESSURE (IN PASCAL) AT T=TO
C      TO = 273.15
C      
C     SAT. MIXING  RATIO: QS ~= EPS*ES/P
C     CLAUSIUS-CLAPEYRON: DES/DT = L*ES/(RV*T^2)
C     @QS/@T =  (EPS/P)*DES/DT
C    
          LW = LVH2O - ( CW - CPV ) * ( T - TO )
          ES = ESO*EXP (LW*(1/TO - 1/T)/RV)  
          DESDT = LW*ES/(RV*T*T)
C
C      FOR INSERTION IN DQSDT FUNCTION: 
C      DQSDT = DQS/P , WHERE DQS = EPS*DESDT  
C
          DQS = EPS*DESDT
C     
          RETURN
          END
      FUNCTION DQSDT ( SFCTMP, SFCPRS )


      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    PURPOSE:  TO RETRIEVE THE APPROPRIATE VALUE OF DQSDT (THE CHANGE
CC    =======   OF THE SATURATION MIXING RATIO WITH RESPECT TO THE 
CC              CHANGE IN TEMPERATURE) FROM: 
CC
CC               FORMULAS INTRODUCED IN NEW FUNCTION DQS 
CC                                  (MODIFIED BY PABLO GRUNMANN, 7/9/97).
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      REAL SFCTMP
      REAL SFCPRS
      REAL DQS
      REAL DQSDT

      IF ((SFCTMP .GE. 173.0) .AND. (SFCTMP  .LE.  373.0)) THEN

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       IF THE INPUT SFC AIR TEMP IS BTWN 173 K AND 373 K, USE
C       FUNCTION DQS TO DETERMINE THE SLOPE OF SAT.MIX RATIO FUNCTION
C                                 -ADAPTED TO USE NEW DQS, 7/9/97.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        DQSDT = DQS (SFCTMP) / SFCPRS

      ELSE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       OTHERWISE, SET DQSDT EQUAL TO ZERO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        DQSDT = 0.0

      END IF

      RETURN
      END
      FUNCTION E (T) 


      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  PURPOSE:  TO CALCULATE VALUES OF SAT. VAPOR PRESSURE (E)
CC            SUBSTITUTES LOOK-UP TABLES ASSOCIATED WITH THE DATA
CC            BLOCK  /VAPPRS/ .
CC            FORMULAS AND CONSTANTS FROM ROGERS AND YAU, 1989.
CC
CC                         ADDED BY PABLO J. GRUNMANN, 7/9/97.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      REAL LW
      REAL T
      REAL E
C
CK    REAL EPS
CK    REAL CP
CK    REAL CV
CK    REAL CVV
      REAL CPV
      REAL RV
      REAL CW
      REAL ESO
      REAL TO
      REAL LVH2O

      PARAMETER (LVH2O = 2.501000E+6)
C
CK    PARAMETER (EPS = 0.622)
CK    PARAMETER  (CP = 1005.)
CK    PARAMETER  (CV = 718.)
CK    PARAMETER (CVV = 1410.)
      PARAMETER (CPV = 1870.)
      PARAMETER  (RV = 461.5)
      PARAMETER  (CW = 4187.)
      PARAMETER (ESO = 611.2)
      PARAMETER  (TO = 273.15)      
C 
C     ABOUT THE PARAMETERS:
C      
C     EPS ---------- WATER - DRY AIR MOLECULAR MASS RATIO, EPSILON
C      
C   VALUES FOR SPECIFIC HEAT CAPACITY AND INDIVIDUAL GAS CONSTANTS 
C   IN [JOULES/(KG*KELVIN)] UNITS.
C
C     DRY AIR: 
C             CP, CV
C     WATER VAPOR:
C                 CVV = 1410. 
C                 CPV = 1870.
C                 RV  =  461.5
C     LIQUID WATER:
C                  CW = 4187.
C
C     ESO = ES(TO) = SAT. VAPOR PRESSURE (IN PASCAL) AT T=TO
C      TO = 273.15
C_______________________________________________________________________
C
C     CLAUSIUS-CLAPEYRON: DES/DT = L*ES/(RV*T^2)
C     
C    
          LW = LVH2O - ( CW - CPV ) * ( T - TO )
          E = ESO*EXP (LW*(1/TO - 1/T)/RV)  
C
     
          RETURN
          END 
      SUBROUTINE JULDATE(JULD,IMONTH,IDAY,JULM)
      

      IMPLICIT NONE

CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    NAME:  JULIAN DAYS TO MM/DD DATE
CC
CC
CC    PURPOSE:  TO CONVERT FROM JULIAN DAY NUMBER OF THE YEAR TO
CC              CALENDAR DATE  (MONTH, DAY).
CC                                      PABLO J. GRUNMANN, 05/98
CC
CC    VARIABLES:
CC    =========
CC
CC    LABEL            ............DESCRIPTION...............
CC
CC    IDMONTH(IM)   NUMBER OF DAYS IN MONTH NUMBER "IM", FOR IM=1,12
CC    JULD            JULIAN DAY
CC    ILDM,ILDMP    JULIAN DAY OF THE LAST DAY OF A PARTICULAR 
CC                  MONTH AND ITS CONSECUTIVE (ILDMP).
CC    IMONTH,IDAY   MONTH, DAY - CALENDAR DATE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INTEGER IM
      INTEGER IDMONTH(12)
      INTEGER JULM(13)
      INTEGER JULD
      INTEGER IDAY
      INTEGER ILDM
      INTEGER ILDMP
      INTEGER IMONTH     
C.................................................................        
C  DEFINE LAST DAY OF FEBRUARY AND MODIFY DATA IDMONTH 
C IF NECESSARY:
C                 FEB 28
C                 FEB 29 (LEAP YEAR)
C..................JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG.SEP,OCT,NOV,DEC
      DATA IDMONTH/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
C      DATA JULM/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 
C     & 334, 365/
C.................................................................        
      ILDMP = 0
      IMONTH = 0
      
C      Print*,JULM
      JULM(1)= 0
      
      DO IM = 1,12
        JULM(IM+1) = JULM(IM) + IDMONTH(IM)
        
        ILDM = JULM(IM) 
        ILDMP = JULM(IM+1)
        IF ((JULD .LE. ILDMP) .AND. (JULD .GT. ILDM)) THEN
          IMONTH = IM
          IDAY = JULD - JULM(IM)
        ENDIF
      END DO 
        
      RETURN
      END
      SUBROUTINE MONTH_D(JDAY,XMON,XDAY)
      

      IMPLICIT NONE
      
C    INTERPOLATE TO DAY OF YEAR.
C    THE MONTHLY FIELDS ASSUMED VALID AT 15TH OF MONTH.
C    
C    READ FROM A FILE THAT HAS 13 RECORDS, ONE PER MONTH WITH
C    JANUARY OCCURRING BOTH AS RECORD 1 AND AGAIN AS RECORD 13,
C    THE LATTER TO SIMPLIFY TIME INTERPOLATION FOR DAYS
C    BETWEEN DEC 16 AND JAN 15. WE TREAT JAN 1 TO JAN 15
C    AS JULIAN DAYS 366 TO 380 BELOW, I.E WRAP AROUND YEAR.
C
C   BASED ON SUBROUTINE CNSTS.f WHICH
C   INCLUDED REVISION BY F. CHEN 7/96 TO REFLECT A NEW NESDIS 
C   VEGETATION FRACTION PRODUCT (FIVE-YEAR CLIMATOLOGY WITH 
C   0.144 DEGREE RESOLUTION FROM 89.928S, 180W TO 89.928N, 180E)
C   
C   This is PABLO J. GRUNMANN's version for use with the uncoupled, 
C   column mode Land-Surface Model
C 

C      DATA JULM/0,31,59,90,120,151,181,212,243,273,304,334,365/
C.......getting JULM from subroutine JULDATE to enable easy 
C.......one number adjustment in case of leap year.

C
C ####  DO TIME INTERPOLATION ####
C            
       INTEGER IDAY
       INTEGER IMON1
       INTEGER IMON2
       INTEGER IMONTH
       INTEGER JDAY
       INTEGER JULD
       
       INTEGER JULM(13)

      REAL DAY1
      REAL DAY2
      REAL RDAY
      REAL WGHT1
      REAL WGHT2
      REAL XDAY
      
      REAL XMON(13)

       JULD = JDAY
       CALL JULDATE(JULD,IMONTH,IDAY,JULM)
       
       
       IF(JULD.LE.15) JULD=JULD+365
       
       IMON2=IMONTH
       IF (IDAY .GT. 15) IMON2 = IMON2 + 1
       IF (IMON2 .EQ.  1) IMON2 = 13
       
       IMON1 = IMON2 - 1
       
C #### ASSUME DATA VALID AT 15TH OF MONTH 
       DAY2 = REAL(JULM(IMON2)+15)
       DAY1 = REAL(JULM(IMON1)+15)
       RDAY = REAL(JULD)
       WGHT1 = (DAY2-RDAY)/(DAY2-DAY1)
       WGHT2 = (RDAY-DAY1)/(DAY2-DAY1)
C       
         XDAY=WGHT1*XMON(IMON1)+WGHT2*XMON(IMON2)
         
            
      RETURN
      END
      SUBROUTINE PRTBND(INDI,    NOUT,   jday,
     & IIDAY,  IITIME,  SFCTMP,  Q2,     SFCPRS, Rg,
     * LW_in,  Par_in,  Par_out, rnet,
     & S_OBS,  PRCP,    wet,     T1_OBS,
     * T_02,  T_04,   T_08,   T_16, T_32, T_64,
     * sm_05, sm_20, sm_60, w_dir,  u_bar,  eddyuw,
     * uprim2, vprim2,  wprim2,  H_OBS,  ETA_OBS,
     * IMONTH, IDAY,    IREC)

C       SUBROUTINE PRTBND(INDI, NOUT,   jday,
C    & IIDAY,  IITIME,   SFCTMP,  Q2,     SFCPRS, Rg,
C    * LW_in,  PRCP,
C    *  w_dir,  u_bar,
C    * IMONTH, IDAY, IREC)      

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C From MAIN:                                                     
C      CALL PRTBND(INDI,NOUT5,jday,                              
C     & IIDAY, ITIME, SFCTMP, Q2, SFCPRS, Rg,                    
C     * LW_in, Par_in, Par_out, RNET,                            
C     & S_OBS, PRCP, wet, T1_OBS,                                
C     * T_02, T_04, T_08, T_16, T_32, T_64,             
C     * sm_05, sm_20, sm_60, w_dir, u_bar, eddyuw,          
C     * uprim2, vprim2, wprim2, H_OBS, ETA_OBS,                  
C     * IMONTH, IDAY, IREC5)                                     
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    NAME:  PRINT BONDVILLE, IL (BND_) DATA:
CC
CC    PURPOSE:  TO WRITE OUT OBSERVATIONAL DATA THAT CAN BE
CC    =======        USED FOR VALIDATION.
CC    
CC    VARIABLES:
CC    =========
C  OBSERVED    SFLX STATE VARIABLES 
C
C   (T1_OBS)                T1: SKIN (GRND SFC) TEMPERATURE (K)
C   (sm_XXcm)    SMC(3 layers): SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
C                               FOR LAYER CENTERED AT XX cm DEPTH.
C   (STC_OBS)    STC(3 layers): SOIL TEMP (K)
C
C  OBSERVED   SFLX OUTPUT 
C  
C (ETA_OBS)  ETA: ACTUAL LATENT HEAT FLUX (W M-2: NEGATIVE, IF UPWARD FROM SURFACE)
C  (H_OBS)     H: SENSIBLE HEAT FLUX (W M-2: NEGATIVE, IF UPWARD FROM SURFACE)
C  (S_OBS)     S: SOIL HEAT FLUX (W M-2: NEGATIVE, IF DOWNWARD FROM SURFACE)
C             Q1: EFFECTIVE MIXING RATIO AT GRND SFC ( KG KG-1) 
C
CC   ORIGINAL BND_ DATA VARIABLE NAMES
C
C     jday             Julian Day
C     time             LST, half hour ending (using IITIME)
C     Ta             air temperature (C), at 3 m
C     RH             relative humidity (%) at 3 m
C                    (converted to specific humidity Q2)
C     Pres             surface pressure in mb
C     Rg             incoming short wave radiation (W/m2)
C     Par_in             incoming visible radiation (0.4-0.7 um) in uE/m2/s
C     Par_out             outgoing or reflected visible light
C     Rnet             net radiation (W/m2)

C     GHF             (as S_OBS)  soil or ground heat flux (W/m2)
C                                   
C     RAIN            total rain for half hour (inches)
C     PRCP            precipitation rate conversion (Kg m-2 s-1)
C     wet             wetness sensor (volts), high values indicate wetness.

C     IRT             (as T1_OBS) surface or skin temp (CONVERTED TO K)

C     T_02             soil temp at 2 cm (C)
C     T_04             soil tmep at 4 cm
C     T_08             soil temp at 8 cm
C     T_16             soil temp at 16 cm
C     T_32             soil temp at 32 cm
C     T_64             soil temp at 64 cm
c
C     STC5           linear interpolations, using the soil
C     STC20          temperatures above, to match the model
C     STC60          depths (5,20 and 60 cm).
C
C     sm_05              soil volumetric water content at 5 cm zone
C     sm_20        soil volumetric water content at 20 cm zone
C     sm_60             soil volumetric water content at 60 cm zone
C                    (as SMC_OBS(3 layers))
C
C     w_dir             wind direction
C     u_bar             average wind vector speed (m/s), at 6m
C     eddyuw             (u'w') kinematic shear stress (m2/s2)
C     uprim2             (u'2) streamwise velocity variance (m2/s2)
C     vprim2             (v'2) crosswind velocity variance (m2/s2)
C     wprim2             (w'2) vertical velocity variance  (m2/s2)

C     H              (as H_OBS) sensible heat flux (W/m2)
C                     
C     LE             (as ETA_OBS) latent heat flux (W/m2)


CC The eddy covariance sensors are located at 6 m AGL
CC The bulk density of the soil is 1.4 gm/cm3
CC The site is currently in corn stubble (like it would look
CC  after combining)
CC
CC The units uE/m2/s refer to micro Einsteins per square meter per
CC  second. A uE is 6.02 x 10 (17)  photons.
CC______________________________________________________________________    


      IMPLICIT NONE

      REAL T0
      PARAMETER (T0=273.15)

      INTEGER INDI, NOUT, NASCII, IREC
      INTEGER IIDAY, IITIME
      INTEGER IMONTH, IDAY, jday
      
      REAL Q2
      REAL SFCPRS
      REAL SDATE
      REAL DDTIME
      REAL Rg
      REAL LW_in
      REAL Par_in
      REAL Par_out
      REAL Rnet
      REAL S_OBS
      REAL PRCP
      REAL wet
      REAL T1_OBS

      REAL T_02
      REAL T_04
      REAL T_08
      REAL T_16
      REAL T_32
      REAL T_64

      REAL STC5
      REAL STC20
      REAL STC60
      
      REAL sm_05
      REAL sm_20
      REAL sm_60
      REAL w_dir
      REAL u_bar
      REAL eddyuw
      REAL uprim2
      REAL vprim2
      REAL wprim2        
      REAL H_OBS
      REAL ETA_OBS
      REAL RMONTH
      REAL RIDAY
      REAL SFCTMP

C ____________________________________________________________
C
C  For now, print soil temperature observations as they are,
C  T_02,  T_04,  T_08,  T_16, T_32 and T_64.
C
C  In the future, may use a cubic spline or
C  a simple linear scheme to interpolate
C  to STC_OBS( 5cm ), STC_OBS( 20cm ) and STC_OBS( 60cm ).
C ____________________________________________________________
C 
C  DATA CONVERSIONS TO MATCH MODEL'S VARIABLES

C Soil Temperature in K

       T_02 = T_02 + T0
       T_04 = T_04 + T0
       T_08 = T_08 + T0
      T_16 = T_16 + T0
      T_32 = T_32 + T0
      T_64 = T_64 + T0
      
C SOIL TEMPERATURE AT SOIL MOISTURE MEASUREMENT
C   LEVELS (LINEAR INTERP)
      STC5 = T_04 + (T_08 - T_04)/4.
      STC20 = T_16 + 4*(T_32 - T_16)/16.
      STC60 = T_64 + 4*(T_32 - T_64)/32.

C ____________________________________________________________
C
C  ##### WRITE UNFORMATTED FOR GRADS OUTPUT ########
C

      IF (NOUT .GT. 0) THEN
c convert to REAL for binary output
      SDATE  = FLOAT(IIDAY)
      DDTIME = FLOAT(IITIME)
      
      RMONTH = FLOAT(IMONTH)
      RIDAY  = FLOAT(IDAY  )
      
      WRITE(NOUT,REC=IREC) SDATE
      IREC = IREC + 1    
      WRITE(NOUT,REC=IREC) DDTIME
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SFCTMP
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) Q2
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SFCPRS
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) Rg
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) LW_in
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) Par_in
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) Par_out
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) rnet
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) S_OBS
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) PRCP
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) wet
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) T1_OBS
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) T_02
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) T_04
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) T_08
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) T_16
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) T_32
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) T_64
      IREC = IREC + 1
C
      WRITE(NOUT,REC=IREC) STC5
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) STC20
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) STC60
      IREC = IREC + 1
C
      WRITE(NOUT,REC=IREC) sm_05
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) sm_20
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) sm_60
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) w_dir
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) u_bar
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) eddyuw
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) uprim2
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) vprim2
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) wprim2
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) H_OBS
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETA_OBS
      IREC = IREC + 1

      WRITE(NOUT,REC=IREC) RMONTH
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RIDAY
      IREC = IREC + 1

      ELSEIF (NOUT .LT. 0) THEN
            NASCII = -NOUT

      WRITE(NASCII,200) jday, IITIME, SFCTMP, Q2,
C    & SFCPRS, Rg, LW_in,PRCP,w_dir, u_bar,
C    & IMONTH, IDAY
     & SFCPRS, Rg, LW_in, Par_in, Par_out,
     & rnet, S_OBS, PRCP, wet, T1_OBS,
     & T_02, T_04, T_08, T_16, T_32, T_64,
     & STC5, STC20, STC60, sm_05, sm_20, sm_60,
     & w_dir, u_bar, eddyuw, uprim2, vprim2, wprim2,
     & H_OBS, ETA_OBS, IMONTH, IDAY
      
      END IF

 200  FORMAT(2(I6,1X),8(f15.4,1X),2(I6,1X))
 
      RETURN
      END
      SUBROUTINE PRTDAILY(NOUT,jday,
     &                 IIDAY,ITIME,ETADAY,ETADAY_O,
     &                 PCPDAY,SMCDIF,SMCDIF_O,RUNOFFDAY,
     &                 IREC)


      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    NAME:  PRINT DAILY ACCUMULATED HYDROLOGICAL VARIABLES
CC
CC    VARIABLES:
CC    =========
CC
CC       LABEL     I/O     TYPE    ............DESCRIPTION...............
CC
CC       AET       LOC      RS      ACTUAL EVAPOTRANSPIRATIVE ENERGY
CC                                 (J M-2 S-1)
CC       AET2      LOC      RS      ACTUAL EVAPOTRANSPIRATION (MM)
CC       CH        IOC      RS      DRAG COEF FOR HEAT/MOISTURE
CC       CM        IOC      RS      DRAG COEFFICIENT FOR MOMENTUM
CC       CMC       IOC      RS      CANOPY MOISTURE CONTENT (M)
CC       DEW        IB      RS      DEWFALL AMOUNT  (M S-1)
CC       DRIP       IB      RS      EXCESS CANOPY MOISTURE (M)
CC       EC(EC1)    IB      RS      CANOPY EVAPORATION (M S-1)
CC       EDIR(EDIR1)IB      RS      DIRECT SOIL EVAPORATION (M S-1)
CC       ETA       IOC      RA      FINAL ACTUAL EVAPOTRANSP (KG M-2 S-1)
CC       ETAS      IOC      RA      FINAL ACTUAL EVAPOTRANSP (MM)
CC       ETP       IOC      RA      FINAL POTNTL EVAPOTRANSP (KG M-2 S-1)
CC       ETPS      IOC      RA      FINAL POTNTL EVAPOTRANSP (MM)
CC       ETT(ETT1)  IB      RS      ACCUM PLANT TRANSPIRATION (M S-1)
CC       F         IOC      RS      NET FLUX (TOT DOWNWARD RADIATION)
CC       FLX1       IB      RS      1ST FLUX VALUE (W M-2)
CC       FLX2       IB      RS      2ND FLUX VALUE (W M-2)
CC       FLX3       IB      RS      3RD FLUX VALUE (W M-2)
CC       FOG        IC      LS      INTERMEDIATE HRLY FOG FLAG
CC       FUP        IB      RS      UPWARD GRND LW RADIATION (W M-2)
CC       H         LOC      RS      SENSIBLE HEAT FLUX (W M-2)
CC       HEAT      LOC      RS      SENSIBLE HEAT FLUX SUB-PRODUCT
CC       HEMI       IC      IS      CURRENT HEMISPHERE (1=N, 2=S)
CC       I          IC      IS      1/8 MESH I COORDINATE
CC       ICLAMT     IC      IA      INTERMEDIATE HRLY CLD AMOUNT
CC       ICLTYP     IC      IA      INTERMEDIATE HRLY CLD TYPE
CC       J          IC      IS      1/8 MESH J COORDINATE
CC       K         LOC      IS      LOOP INDEX
CC       NSOIL      IC      IS      SOIL LAYER NUMBER
CC       PET       LOC      RS      POTENTIAL EVAPOTRANSPIRATION (MM)
CC       PRCP      IOC      RA      HALF HOURLY PRECIP AMT (KG M-2 S-1)
CC       Q2        IOC      RS      MIXING RATIO AT 1ST MDL LVL ABV SKIN
CC       Q2SAT     IOC      RS      SAT MXNG RATIO AT 1ST MDL LVL ABV SKIN
CC       RES       LOC      RS      ENERGY BALANCE EQN RESIDUAL (W M-2)
CC       RIB        IB      RS      BULK RICHARDSON NUMBER
CC       RLDOWN     IC      RS      DOWNWARD LONGWAVE RADIATION (W M-2)
CC       RR        LOC      RS      SENSIBLE HEAT SUB-PRODUCT
CC       RSOLIN     IC      RS      SOLAR RADIATION (W M-2)
CC       RUNOFF1    IB      RS      GRND SFC RUNOFF (M )
CC       RUNOFF2    IB      RS      UNDERGROUND  RUNOFF (M )
CC       RUNOFF3    IB      RS      RUNOFF WITHIN SOIL LAYERS (M )
CC       SMC       IOC      RA      SOIL MOISTURE CONTENT (VOLUMETRIC)
CC       ZSOIL     IOC      RA      SOIL LAYER DEPTH  ( M )
C      IMPLICIT         DOUBLE PRECISION (A-H,O-Z)

      INTEGER JDAY
      INTEGER IIDAY
      INTEGER ITIME
      INTEGER IREC
      INTEGER NOUT
      INTEGER NASCII

      REAL CMCS
      REAL EC1S
      REAL EDIR1S
      REAL ETT1S
      REAL RTIME
      REAL SDATE

      REAL ETADAY, ETADAY_O, PCPDAY
      REAL SMCDIF, SMCDIF_O, RUNOFFDAY
       

      IF (NOUT .GT. 0) THEN
      SDATE=FLOAT(IIDAY)
      RTIME=FLOAT(ITIME)
      WRITE(NOUT,REC=IREC) SDATE
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RTIME
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) PCPDAY
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETADAY
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETADAY_O
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RUNOFFDAY
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) EDIR1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETT1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) EC1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) CMCS
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SMCDIF
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SMCDIF_O
      IREC = IREC + 1
      
      ELSEIF (NOUT .LT. 0) THEN
            NASCII = -NOUT

      WRITE(NASCII,200)
     & jday,
     & ITIME,
     & PCPDAY,
     & ETADAY,
     & ETADAY_O,
     & RUNOFFDAY,
     & EDIR1S,
     & ETT1S,
     & EC1S,
     & CMCS,
     & SMCDIF,
     & SMCDIF_O      
      
      
      END IF

 200  FORMAT(I6,1X,I6,10(1x,F15.4))

      RETURN
      END
      SUBROUTINE PRTHMF(INDI,NOUT,NSOIL,jday,
     &          IIDAY,IITIME,CH,CM,Z,F,S,CMC,
     &          SMCMAX,SFCTMP,T1,Q1,SFCPRS,
     &          SFCSPD,ETA,ETP,STC,
     &          DT,H,AET,RES,IREC,
     &          FLX1,FLX2,FLX3)


      IMPLICIT NONE

CCC                                    CCCC
C From MAIN:                              C 
C      CALL PRTHMF(INDI,NOUT3,NSOIL,jday, C
C     &  IIDAY,ITIME,CH,CM,Z,F,S,CMC,     C
C     &  SMCMAX,SFCTMP,T1,Q1,SFCPRS,      C
C     &  SFCSPD,ETA,ETP,STC,         C
C     &  DT,RNET,H,AET,RES,IREC3)         C
CCCC                                   CCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    NAME:  PRINT THERMODYNAMIC VARIABLES
CC
CC    CPC:   N/A
CC    ===
CC
CC    PURPOSE:  TO CALC AND WRITE OUT PARAMETERS TO AID IN SCIENTIFIC
CC    =======   TUNING AND DEBUGGING OF THE OSU SOIL HYDROLOGY MODEL.
CC              THIS ROUTINE IS ONLY INVOKED FOR POINTS SELECTED BY
CC              SPECIFICATION IN THE CONTROL FILE.
CC
CC    METHOD:   1. CALC NEEDED PARTIAL PRODUCTS AND SUMS.
CC    ======    2. WRITE OUT RESULTS TO PRINT$ FILE.
CC
CC    REFERENCES:  FUNCTIONAL DESCRIPTION, SUBSYSTEM SPECIFICATION
CC
CC    VARIABLES:
CC    =========
CC
CC       LABEL     I/O     TYPE    ............DESCRIPTION...............
CC
CC       AET      LOC      RS      ACTUAL EVAPOTRANSPIRATIVE ENERGY
CC                                 (J M-2 S-1)
CC       AET2     LOC      RS      ACTUAL EVAPOTRANSPIRATION (MM)
CC       CH       IOC      RS      DRAG COEF FOR HEAT/MOISTURE
CC       CM       IOC      RS      DRAG COEFFICIENT FOR MOMENTUM
CC       CMC      IOC      RS      CANOPY MOISTURE CONTENT (M)
CC       DEW       IB      RS      DEWFALL AMOUNT  (M S-1)
CC       DRIP      IB      RS      EXCESS CANOPY MOISTURE (M)
CC       EC        IB      RS      CANOPY EVAPORATION (M S-1)
CC       EDIR      IB      RS      DIRECT SOIL EVAPORATION (M S-1)
CC       ETA      IOC      RA      FINAL ACTUAL EVAPOTRANSP (KG M-2 S-1)
CC       ETP      IOC      RA      FINAL POTNTL EVAPOTRANSP (KG M-2 S-1)
CC       ETPS     IOC      RA      FINAL POTNTL EVAPOTRANSP (J M-2 S-1)
CC       ETT       IB      RS      ACCUM PLANT TRANSPIRATION (M S-1)
CC       F        IOC      RS      NET FLUX (TOT DOWNWARD RADIATION)
CC       FLX1      IB      RS      1ST FLUX VALUE (W M-2)
CC       FLX2      IB      RS      2ND FLUX VALUE (W M-2)
CC       FLX3      IB      RS      3RD FLUX VALUE (W M-2)
CC       FUP       IB      RS      UPWARD GRND LW RADIATION (W M-2)
CC       H        LOC      RS      SENSIBLE HEAT FLUX (W M-2)
CC       HEAT     LOC      RS      SENSIBLE HEAT FLUX SUB-PRODUCT
CC       I         IC      IS      1/8 MESH I COORDINATE
CC       ICLAMT    IC      IA      INTERMEDIATE HRLY CLD AMOUNT
CC       ICLTYP    IC      IA      INTERMEDIATE HRLY CLD TYPE
CC       J         IC      IS      1/8 MESH J COORDINATE
CC       K        LOC      IS      LOOP INDEX
CC       NSOIL     IC      IS      SOIL LAYER NUMBER
CC       PET      LOC      RS      POTENTIAL EVAPOTRANSPIRATION (MM)
CC       Q2       IOC      RS      MIXING RATIO AT 1ST MDL LVL ABV SKIN
CC       Q2SAT    IOC      RS      SAT MXNG RATIO AT 1ST MDL LVL ABV SKIN
CC       RES      LOC      RS      ENERGY BALANCE EQN RESIDUAL (W M-2)
CC       RIB       IB      RS      BULK RICHARDSON NUMBER
CC       RLDOWN    IC      RS      DOWNWARD LONGWAVE RADIATION (W M-2)
CC       RR       LOC      RS      SENSIBLE HEAT SUB-PRODUCT
CC       RSOLIN    IC      RS      SOLAR RADIATION (W M-2)
CC       RUNOFF    IB      RS      GRND SFC RUNOFF (M S-1)
CC       S         IC      RS      GRND SFC FLUX (W M-2)
CC       SFCPRS   IOC      RS      SFC PRESSURE (PASCALS)
CC       SFCSPD    IC      RS      SFC WIND SPEED (M S-1)
CC       SFCTMP   IOC      RS      SFC TEMPERATURE (K)
CC       SMC      IOC      RA      SOIL MOISTURE CONTENT (VOLUMETRIC)
CC       SMCMAX   IOC      RS      MAXIMUM SOIL MOISTURE CONTENT LIMIT
CC       SNODEP   IOC      RA      SNOW DEPTH ( M )
CC       STC      IOC      RA      SOIL TEMPERATURE ( 5 CM AND 95 CM )
CC       T1       IOC      RS      SKIN (GRND SFC) TEMPERATURE (K)
CC       T14      LOC      RS      GRND SFC TEMP TO THE 4TH POWER (K+4)
CC       TIME     IOC      IS      1 HRLY TIME (LOOP INDEX)
CC       Z        IOC      RS      HT ABOVE GRND LVL (M)
CC       ZSOIL    IOC      RA      SOIL LAYER DEPTH  ( M )

      INTEGER IIDAY
      INTEGER jday
      INTEGER IITIME
      INTEGER INDI
      INTEGER IREC
      INTEGER NOUT
      INTEGER NASCII
      INTEGER NSOIL
      INTEGER LAYER
      INTEGER NUMVARS
      
      REAL RNETcalc
      REAL AET
      REAL AET2
      REAL BETA
      REAL CH
      REAL CM
      REAL CMC
      REAL DRIP
      REAL DT
      REAL EC
      REAL EDIR
      REAL ETA
      REAL ETP
      REAL ETPS
      REAL ETT
      REAL F
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL FUP
      REAL H
      REAL Q1
      REAL RES
      REAL S
      REAL SDATE
      REAL RTIME
      REAL SFCPRS
      REAL SFCSPD
      REAL SFCTMP
      REAL SMCMAX
      REAL STC(NSOIL)
      REAL T1
      REAL T14
      REAL Z
C
C MIC$ TASKCOMMON RITE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  CONVERT ACTUAL EVAPOTRANSPIRATION VALUE FROM [KG M-2 S-1] TO [W M-2]
C  (BY MULTIPLYING BY WATER LATENT HEAT (~2.501E+6) FOR USE IN ENERGY 
C  BALANCE. ALSO CHANGE ACTUAL AND POTENTIAL EVAPORTRANSPIRATION 
C  VALUES FROM  [KG M-2 S-1]  TO  [MM] (BY MULTIPLYING BY DT).
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      T14 = T1 * T1 * T1 * T1
      AET = ETA

      ETPS = ETP
      AET2 = DT * ETA
      FUP = 5.67E-8 * T14

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  RESIDUAL OF ALL SURFACE ENERGY BALANCE EQN TERMS:
C     RES = F - H - S - AET - FUP - FLX1 - FLX2 - FLX3
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   NET RADIATION
C
      RNETcalc = F - FUP
C
      IF (NOUT .GT. 0) THEN
      SDATE=FLOAT(IIDAY)
      RTIME=FLOAT(IITIME)
      WRITE(NOUT,REC=IREC) SDATE
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RTIME
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) F
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RNETcalc
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) CH
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) CM
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) H
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) AET
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RES
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) FUP
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) FLX1
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) FLX2
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) FLX3
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) T1
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) Q1
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETPS
      IREC = IREC + 1
      DO LAYER=1,NSOIL
        WRITE(NOUT,REC=IREC) STC(LAYER)
        IREC = IREC + 1
      END DO
      
      ELSEIF (NOUT .LT. 0) THEN
            NASCII = -NOUT

      NUMVARS = 15+NSOIL

      WRITE(NASCII,200)
     &  jday,
     &  IITIME,
     &  F,
     &  RNETcalc,
     &  CH,CM,
     &  H,
     &  S,
     &  AET,
     &  RES,
     &  FUP,FLX1, FLX2, FLX3,
     &  T1,
     &  Q1,
     &  ETPS,
     &  (STC(LAYER), LAYER=1,NSOIL)

      END IF
      
 200  FORMAT(I6,1X,I6,19(1x,F15.4))
 
      RETURN
      END
      SUBROUTINE PRTHYDF(INDI,NOUT,NSOIL,jday,
     & IIDAY,DDTIME,ETA,
     & ETP,PRCP,SH2O,SMC,ALBEDO,ALB,SNOALB,
     & SNEQV, SNEQV_bef, SNOMLT, SNOWH, SOILM, SOILM_bef, SOILW,
     & RUNOFF1,
     & RUNOFF2,
     & RUNOFF3,
     & DT,EDIR,EC,ETT,CMC,CMC_bef,IREC,DEW)

      IMPLICIT NONE
       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC   From MAIN:                                             CC
C                                                            CC
C   CALL PRTHYDF(INDI,NOUT1,NSOIL,jday,                      CC
C  & IIDAY,ITIME,ETA,                                        CC
C  & ETP,PRCP,SH2O,SMC,ALBEDO,ALB,SNOALB,                    CC
C  & SNEQV, SNEQV_bef, SNOMLT, SNOWH, SOILM, SOILM_bef, SOILW,CC
C  & RUNOFF1, RUNOFF2, RUNOFF3,                              CC
C  & DT,EDIR,EC,ETT,CMC,IREC1)                               CC
CCC                                                          CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    NAME:  PRINT HYDROLOGICAL VARIABLES
C
C    VARIABLES:
C    =========
C
C   LABEL     I/O     TYPE    ............DESCRIPTION..............
C
C   ALB                        SNOW-FREE ALBEDO (FRACTION)
C   AET       LOC      RS      ACTUAL EVAPOTRANSPIRATIVE ENERGY
C                               (J M-2 S-1)
C   AET2      LOC      RS      ACTUAL EVAPOTRANSPIRATION (MM)
C   CH        IOC      RS      DRAG COEF FOR HEAT/MOISTURE
C   CM        IOC      RS      DRAG COEFFICIENT FOR MOMENTUM
C   CMC       IOC      RS      CANOPY MOISTURE CONTENT (M)
C   DEW        IB      RS      DEWFALL AMOUNT  (M S-1)
C   DRIP       IB      RS      EXCESS CANOPY MOISTURE (M)
C   EC(EC1)    IB      RS      CANOPY EVAPORATION (M S-1)
C   EDIR(EDIR1)IB      RS      DIRECT SOIL EVAPORATION (M S-1)
C   ETA       IOC      RA      FINAL ACTUAL EVAPOTRANSP (KG M-2 S-1)
C   ETAS      IOC      RA      FINAL ACTUAL EVAPOTRANSP (MM)
C   ETP       IOC      RA      FINAL POTNTL EVAPOTRANSP (KG M-2 S-1)
C   ETPS      IOC      RA      FINAL POTNTL EVAPOTRANSP (MM)
C   ETT(ETT1)  IB      RS      ACCUM PLANT TRANSPIRATION (M S-1)
C   F         IOC      RS      NET FLUX (TOT DOWNWARD RADIATION)
C   FLX1       IB      RS      1ST FLUX VALUE (W M-2)
C   FLX2       IB      RS      2ND FLUX VALUE (W M-2)
C   FLX3       IB      RS      3RD FLUX VALUE (W M-2)
C   FOG        IC      LS      INTERMEDIATE HRLY FOG FLAG
C   FUP        IB      RS      UPWARD GRND LW RADIATION (W M-2)
C   H         LOC      RS      SENSIBLE HEAT FLUX (W M-2)
C   HEAT      LOC      RS      SENSIBLE HEAT FLUX SUB-PRODUCT
C   HEMI       IC      IS      CURRENT HEMISPHERE (1=N, 2=S)
C   I          IC      IS      1/8 MESH I COORDINATE
C   ICLAMT     IC      IA      INTERMEDIATE HRLY CLD AMOUNT
C   ICLTYP     IC      IA      INTERMEDIATE HRLY CLD TYPE
C   J          IC      IS      1/8 MESH J COORDINATE
C   K         LOC      IS      LOOP INDEX
C   NSOIL      IC      IS      SOIL LAYER NUMBER
C   PET       LOC      RS      POTENTIAL EVAPOTRANSPIRATION (MM)
C   PRCPDT    IOC      RA      TIME-STEP ACCUM PRECIP (KG M-2)
C   PRCP      IOC      RA      MEAN PRECIP RATE DURING TIME-STEP(KG M-2 s-1)
C   Q2        IOC      RS      MIXING RATIO AT 1ST MDL LVL ABV SKIN
C   Q2SAT     IOC      RS      SAT MXNG RATIO AT 1ST MDL LVL ABV SKIN
C   RES       LOC      RS      ENERGY BALANCE EQN RESIDUAL (W M-2)
C   RIB        IB      RS      BULK RICHARDSON NUMBER
C   RLDOWN     IC      RS      DOWNWARD LONGWAVE RADIATION (W M-2)
C   RR        LOC      RS      SENSIBLE HEAT SUB-PRODUCT
C   RSOLIN     IC      RS      SOLAR RADIATION (W M-2)
C   RUNOFF1    IB      RS      GRND SFC RUNOFF (M )
C   RUNOFF2    IB      RS      UNDERGROUND  RUNOFF (M )
C   RUNOFF3    IB      RS      RUNOFF WITHIN SOIL LAYERS (M )
C   SMC       IOC      RA      SOIL MOISTURE CONTENT (VOLUMETRIC)
C  SH2O(NSOIL): UNFROZEN SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
C  SNEQV:  WATER EQUIVALENT SNOW DEPTH (M) (formerly called SNODEP)
C  SNOMLT:  SNOW MELT (M) (WATER EQUIVALENT)
C  SNOWH:  SNOW DEPTH (M)
C  SNOALB: MAX ALBEDO OVER DEEP SNOW (FRACTION)
C  SOILM:  TOTAL SOIL COLUMN WATER CONTENT (M)
C  SOILW:  AVAILABLE SOIL MOISTURE (UNITLESS FRACTION) 
C  ZSOIL     IOC      RA      SOIL LAYER DEPTH  ( M )
C  WRES:   WATER BALANCE RESIDUAL (mm)

      INTEGER jday
      INTEGER IIDAY
      INTEGER DDTIME
      INTEGER INDI
      INTEGER IREC
      INTEGER NASCII
      INTEGER NOUT
      INTEGER NSOIL
      INTEGER LAYER
      INTEGER NUMVARS
C
      REAL ALB
      REAL ALBEDO
      REAL CMC
      REAL CMC_bef
      REAL CMCS
      REAL DEWDT
      REAL DT
      REAL EC1S
      REAL EDIR1S
      REAL ETA
      REAL ETP
      REAL ETAMM
      REAL ETPMM
      REAL ETT1S
      REAL LVH2O
      REAL PRCP
      REAL PRCPDT
      REAL RTIME
      REAL RUNOFF1
      REAL RUNOFF11
      REAL RUNOFF2
      REAL RUNOFF22
      REAL RUNOFF3
      REAL RUNOFF33
      REAL SDATE
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SNEQV
      REAL SNEQV_bef
      REAL SNEQVMM
      REAL SNOMLT
      REAL SNOALB
      REAL SNOWH
      REAL SOILM
      REAL SOILM_bef
      REAL SOILW
      REAL SNMELT
      REAL SOILMM
      REAL TRUNOFF
      REAL WRES
c COMMON /RITE vars
      REAL BETA
      REAL DRIP
      REAL EC
      REAL EDIR
      REAL ETT
      REAL DEW

      Parameter (LVH2O = 2.501000E+6)
C
C MIC$ TASKCOMMON RITE

C      PRINT*,' '
C      PRINT*,'COMMON/RITE in prtHydf'
C      PRINT*,' '
C      PRINT*,'BETA=',BETA
C      PRINT*,'DRIP=',DRIP
C      PRINT*,'EC=',EC
C      PRINT*,'EDIR=',EDIR
C      PRINT*,'ETT=',ETT
C      PRINT*,'FLX1=',FLX1
C      PRINT*,'FLX2=',FLX2
C      PRINT*,'FLX3=',FLX3
C      PRINT*,'RUNOFF=',RUNOFF
C      PRINT*,'DEW=',DEW
C      PRINT*,'RIB=',RIB
C      PRINT*,'RUNOXX3=',RUNOXX3
C      PRINT*,' '
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Units conversion from ENERGY (w m-2) to mm of WATER changing state
C LVH2O: Latent heat for evaporation for water.
      ETAMM = ETA*DT/LVH2O
      ETPMM = ETP*DT/LVH2O
C Converting to mm of water accumulated during time-step DT
      EC1S=EC*1000.*DT
      EDIR1S=EDIR*1000.*DT
      ETT1S = ETT*1000.*DT
      CMCS=CMC*1000.
      RUNOFF11 = RUNOFF1*DT*1000.
      RUNOFF22 = RUNOFF2*DT*1000.

      RUNOFF33 = RUNOFF3*1000.
      TRUNOFF = RUNOFF11 + RUNOFF22 + RUNOFF33
C
C Water Balance Residual: 
C Storage change in time DT (SMC,SNOW,CMC)=(Precip+Dew-(Runoff+Evap))*DT
C Suffix "_bef" stands for "Time-step before" (previous time-step value)
C
      DEWDT = DEW*DT
      PRCPDT  = PRCP*DT/1000.
      
C ETA contains effects of direct and canopy evap, transpiration and dew      

      WRES= (SOILM-SOILM_bef + SNEQV-SNEQV_bef + CMC-CMC_bef 
     &     - PRCPDT +(RUNOFF1+RUNOFF2)*DT)*1000. + ETAMM
      
      SNEQVMM = SNEQV*1000.
      SNMELT  = SNOMLT*1000.
      SOILMM  = SOILM*1000.
      
      IF (NOUT .GT. 0) THEN
      SDATE=FLOAT(IIDAY)
      RTIME=FLOAT(DDTIME)
      WRITE(NOUT,REC=IREC) SDATE
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RTIME
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) PRCP
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETPMM
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETAMM
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RUNOFF11
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RUNOFF22
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RUNOFF33
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) TRUNOFF
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) DEWDT*1000.
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) EDIR1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETT1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) EC1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) CMCS
      IREC = IREC + 1
      DO LAYER=1, NSOIL
        WRITE(NOUT,REC=IREC) SH2O(LAYER)
        IREC = IREC + 1
      END DO
      DO LAYER=1, NSOIL
        WRITE(NOUT,REC=IREC) SMC(LAYER)
        IREC = IREC + 1
      END DO
      WRITE(NOUT,REC=IREC) ALBEDO
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ALB
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SNOALB
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SNOWH
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SOILW
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SNEQVMM
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SNMELT
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SOILMM
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) WRES
      IREC = IREC + 1
      
      ELSEIF (NOUT .LT. 0) THEN
            NASCII = -NOUT

      NUMVARS = 23+NSOIL+NSOIL
      
      WRITE(NASCII,200) jday, DDTIME, PRCP, ETP,
     & ETA,
     & RUNOFF11, RUNOFF22, RUNOFF33, TRUNOFF,
     & DEWDT*1000., EDIR1S, ETT1S, EC1S, CMCS,
     & (SH2O(LAYER), LAYER=1,NSOIL),
     & (SMC(LAYER), LAYER=1,NSOIL),
     & ALBEDO,ALB,SNOALB,
     & SNOWH, 
     & SOILW,
     & SNEQVMM,
     & SNMELT,
     & SOILMM,WRES

     
      END IF

 200  FORMAT(I6,1X,I6,29(1x,F15.4))
C 200  FORMAT(I6,1X,I6,60(1x,F15.4))
 
      RETURN
      END
      SUBROUTINE QDATAP (T,P,RH,QD,QS,ES) 

      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  PURPOSE:  OBTAIN SPECIFIC HUMIDITY (q) FROM RELATIVE HUMIDITY 
CC            AND GIVEN PRESSURE AND TEMPERATURE.
CC            
CC
CC            FORMULAS AND CONSTANTS FROM ROGERS AND YAU, 1989: 'A 
CC            SHORT COURSE IN CLOUD PHYSICS', PERGAMON PRESS, 3rd ED.
CC
CC                                   Pablo J. Grunmann, 3/6/98.
CC                Updated to eliminate subroutine SVP, 6/24/98.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C----------------------------------------
C In:
C        T    Temperature (K)
C        P    Pressure (Pa)
C        RH   Relative humidity (%)
C-----------------------------------------
C Out:
C        QD   Specific humidity (Kg/Kg)
C        QS   Saturation Specific humidity (Kg/Kg)
C        ES   Saturation vapor pressure for water (Pa)
C----------------------------------------
      REAL T
      REAL P
      REAL RH
      REAL RHF
      REAL QD
      REAL QS
      REAL ES
      REAL EP
      REAL EPS
      REAL E

      PARAMETER (eps=0.622 )
C 
C     ABOUT THE PARAMETER:
C      
C     eps ---------- (Water)/(dry air) molecular mass ratio, epsilon
C _____________________________________________________________________
C
C    function E(T) = Sat. vapor pressure (in Pascal) at 
C                    temperature T (uses Clausius-Clapeyron).
          Es = E(T)
C  CONVERT REL. HUMIDITY (%) TO THE FRACTIONAL VALUE
          RHF = RH/100.

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC      CALCULATE SATURATION MIXING RATIO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      QS = 0.622 * ES /P   was substituted by a more precise
C formula:                              -PABLO J. GRUNMANN, 05/28/98.
        QS = 0.622 * ES /(P - (1.-0.622)*ES)

C
C  CONVERSION FROM REL. HUMIDITY:
C     (Rogers, pg. 17)
C
        EP = (P*Es*RHF)/(P - Es*(1. - RHF))
        QD = eps*EP/(P - (1. - eps)*EP)
C     
          RETURN
          END
      
C     SUBROUTINE READBND(jday,  itime, SFCTMP,   RH, SFCPRS,  Rg,
C    * LW_in, PRCP,SKN_IRT,
C    * w_dir, u_bar,
C    * DT, IMONTH,IDAY,NREAD)
      SUBROUTINE READBND(jday,  itime, SFCTMP,   RH, SFCPRS,  Rg,
     * Par_in, Par_out,  rnet, LW_in, GHF, PRCP, wet, SKN_IRT,
     * T_02,  T_04,  T_08,  T_16, T_32, T_64, sm_05,
     * sm_20, sm_60,  w_dir, u_bar,     eddyuw,
     * uprim2,   vprim2,      wprim2,         H,     LE,
     * DT, IMONTH,IDAY,NREAD)
     
      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    NAME:  READBND
CC
CC
CC    PURPOSE:  
CC           THIS IS A MODIFICATION OF V.KOREN'S  
CC           SUBROUTINE READPILPS(ICALB,NREAD,NREAD2,IYEAR,IMONTH,IDAY,
CC                                IHOUR,P,T,Q,U,V,LWDN,RAIN,SOLDN,SOUT,
CC                                TSKNEST)
CC           TO READ FORCING DATA FROM BONDVILLE, IL ATDD SITE.
CC
CC                                      PABLO J. GRUNMANN, 05/98
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                  ARGUMENT LIST IN THE CALL READBND
C                 PREPARED BY PABLO GRUNMANN IN 05/98
C
C       SFCPRS: PRESSURE AT 1ST MDL LVL ABV SKIN (PASCALS)
C       PRCP:   PRECIP RATE (KG M-2 s-1)
C       SFCTMP: AIR TEMPERATURE  AT 1ST MDL LVL ABV SKIN (K)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      INTEGER jday,itime,IMONTH,IDAY,NREAD,I
      INTEGER  JULM(13)
      
      REAL DT
      REAL SFCTMP
      REAL T
      REAL RH
      REAL SFCPRS
      REAL P
      REAL Rg
      REAL Par_in
      REAL Par_out
      REAL rnet
      REAL LW_in
      REAL GHF
      REAL PRCP
      REAL RAIN
      REAL wet
      REAL SKN_IRT
      REAL T_02
      REAL T_04
      REAL T_08
      REAL T_16
      REAL T_32
      REAL T_64
      REAL sm_05
      REAL sm_20
      REAL sm_60
      REAL w_dir
      REAL u_bar
      REAL eddyuw
      REAL uprim2
      REAL vprim2
      REAL wprim2
      REAL w_speed
      REAL CO2
      REAL H
      REAL LE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c Save previous step required forcing data
c to make up in case of missing.
      REAL XOLD(32)
       do i=1,32
        XOLD(i)=0.0
       end do
       XOLD(2)=   SFCTMP - 273.15
       XOLD(3)=   RH
       XOLD(5)=   SFCPRS/1.E2
       XOLD(6)=   Rg
       XOLD(7)=   LW_in
       XOLD(12)=  PRCP*DT/25.4
       XOLD(14)=  SKN_IRT
       XOLD(25)=  u_bar
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      READ BND_ DATA
C  ATTENTION!
C  REMEMBER TO REMOVE TITLE FROM BND_ FILE (1ST LINE), THEN READ DATA

        
C        READ (NREAD,*) jday,  itime, w_speed,
C    * T,   RH,  P, Rg, LW_in, RAIN
        READ(NREAD,*) jday,  itime, w_speed,  w_dir,
     * T,   RH,  P,  Rg, Par_in,   Par_out,
     * rnet,   GHF, RAIN,   wet,  SKN_IRT,
     * T_02,  T_04,  T_08,  T_16, T_32, T_64,
     * u_bar,  eddyuw, uprim2,   vprim2,   wprim2,
     * H,  LE, CO2, LW_in,
     * sm_05, sm_20, sm_60          
    
C-----------------------------------------------------------------------
C TILDEN MEYERS' BND_ DATA:
C
C    jday       Julian Day
C    itime       LST, half hour ending
C    Ta         air temperature (C), at 3 m
C    RH         relative humidity (%) at 3 m
C    Pres       surface pressure in mb
C    Rg         incoming short wave radiation (W/m2)
C    Par_in     incoming visible radiation (0.4-0.7 um) in uE/m2/s
C    Par_out    outgoing or reflected visible light
C    Rnet       net radiation (W/m2)
C    GHF        soil or ground heat flux (W/m2)
C    RAIN       total rain for half hour (inches)
C    wet       wetness sensor (voltage - higher values indicatE wetness)
C    SKN_IRT    surface or skin temp (C)
C     T_2     soil temp at 2 cm (C)
C     T_4     soil tmep at 4 cm
C     T_8     soil temp at 8 cm
C    T_16    soil temp at 16 cm
C    T_32    soil temp at 32 cm
C    T_64    soil temp at 64 cm
C    sm_5    soil volumetric water content at 5 centimeter_depht
C   sm_20   soil volumetric water content at 20 centimeter_depht
C   sm_60   soil volumetric water content at 60 centimeter_depht
C    w_dir         wind direction
C    u_bar         average wind vector speed (m/s), at 6m
C    u'w'          kinematic shear stress (m2/s2)
C    u'2           streamwise velocity variance (m2/s2)
C    v'2           crosswind velocity variance (m2/s2)
C    w'2           vertical velocity variance  (m2/s2)
C    H             sensible heat flux (W/m2)
C    LE            latent energy flux (W/m2)
C    
C The eddy covariance sensors are located at 6 m AGL
C The bulk density of the soil is 1.4 gm/cm3
C The site is currently in corn stubble (like it would look 
C  after combining)
C
C The units uE/m2/s refer to micro Einsteins per square meter per
C  second. A uE is 6.02 x 10 (17)  photons.
C
C
C-----------------------------------------------------------------------

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  THIS PART STILL REQUIRES ATTENTION:                       C
C                                                            C
C   IN LACK OF FORCING DATA, CURRENT PROCEDURE HAS BEEN      C
C (AND IT IS) TO USE PREVIOUS TIME-STEP RECORDED VALUES.     C
C                                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       IF (T       .LT. -173.) THEN
         T       = XOLD(2)
       ENDIF
       IF (RH      .LT.    0. ) THEN
         RH      = XOLD(3)
       ENDIF
       IF (P       .LT.  50. ) THEN
         P       = XOLD(5)
       ENDIF
       IF (Rg      .LT. -100. ) THEN
         Rg      = XOLD(6)
       ENDIF
       IF (LW_in      .LT. 0.) THEN
         LW_in      = XOLD(7)
       ENDIF
       IF (RAIN    .LT.    0. ) THEN
         RAIN    = XOLD(12)
       ENDIF
       u_bar=w_speed
C       IF (u_bar   .LT.    0. ) THEN
C MISSING u_bar, USE W_SPEED WITH LINEAR REGRESSION CONVERSION
C         u_bar   = 0.85*w_speed
C BUT, IF EVEN W_SPEED IS NOT AVAILABLE, USE PREVIOUS STEP VALUE
         IF (u_bar   .LT.    0. ) THEN
          u_bar   = XOLD(25)
C         ENDIF
       ENDIF


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CONVERT VARIABLES
C
C OBTAIN DAY AND MONTH: JULIAN DATE SUBROUTINE
      CALL JULDATE(JDAY,IMONTH,IDAY,JULM)
C
C  CONVERT RAIN FROM INCHES IN 30MIN TO KG M^-2 S^-1 
C (SAME AS PRECIP.RATE IN MM/SEC)
C
        PRCP = RAIN*25.4/DT
C
C  AIR TEMPERATURE IN KELVIN
C
        SFCTMP = T + 273.15
C
C  SFC PRESSURE IN PASCAL
C
        SFCPRS =  P*1.E2
C
C ---- DONE CONVERTING VARIABLES ----------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        RETURN
        END 
C ----------------------------------------------------------------------
      SUBROUTINE READCNTL(CNTRFL,NCYCLES,L2nd_data,NRUN,NRUN2,
     & DT,NSOIL,NSOLD,Z,
     & SLDPTH,SOILTP,VEGTYP,SLOPETYP,ALBEDOM,SHDFACM,SHDMAX,SHDMIN,
     & SNOALB,ICE,TBOT,T1,
     & STC,SMC,SH2O,CMC,SNODEP,SNEQV,FILENAME,FILENAME2,
     & LATITUDE,
     & LONGITUDE,
     & JDAY,IBINOUT,
     & TIME)
     
      IMPLICIT NONE

C READS A CONTROL FILE WHICH INCLUDES INPUT PARAMETERS FOR OSU MODEL  -

      INTEGER NCYCLES
      INTEGER NRUN
      INTEGER NRUN2
      INTEGER NSOIL
      INTEGER NSOLD
CCCC      INTEGER NROOT
      INTEGER SOILTP
      INTEGER VEGTYP
      INTEGER SLOPETYP
      INTEGER ICE
      INTEGER IBINOUT
      INTEGER SYDAYS
      INTEGER SYSECS
      INTEGER INTEGERDT

      LOGICAL L2nd_data

      REAL SLDPTH(NSOLD)
      REAL STC(NSOLD)
      REAL SMC(NSOLD)
      REAL SH2O(NSOLD)
      REAL DT
      REAL Z
      REAL ALBEDOM(13)
      REAL SHDFACM(13)
      REAL SNOALB
      REAL TBOT
CCCC      REAL Z0
      REAL T1
      REAL CMC
      REAL SNODEP
      REAL SNEQV
CCCC      REAL CZIL
CCCC      REAL REFKDT
      
      REAL LATITUDE
      REAL LONGITUDE
      
      INTEGER  JDAY
      INTEGER  TIME
            
      INTEGER NREAD, I, K

      REAL SHDMAX
      REAL SHDMIN
      INTEGER IMON

      CHARACTER*72 CNTRFL, LINEFEED
      CHARACTER*72 FILENAME, FILENAME2

        NREAD = 21
        OPEN (UNIT=NREAD, FILE=CNTRFL, STATUS='OLD',
     &        FORM='FORMATTED')

C JUMP-READ 2 LINES (CHARACTER) TO SKIP COMMENTS (LINE FEED)

      DO I=1,2
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO

C Model Configuration:
        READ (NREAD,  * ) LATITUDE
        READ (NREAD,  * ) LONGITUDE
        READ (NREAD, 100) IBINOUT
        READ (NREAD, 100) JDAY
        READ (NREAD, 100) TIME
C
        READ (NREAD, 100) NCYCLES
        READ (NREAD, 100) SYDAYS
        READ (NREAD,  * ) L2nd_data
        READ (NREAD, 150) NRUN2
        READ (NREAD,  * ) DT
        READ (NREAD, 100) NSOIL
        READ (NREAD,  * ) Z
        READ (NREAD,  * ) (SLDPTH(K), K=1,NSOIL)

CCCC     IF (MOD(YSEC,DT) .NE. 0) THEN
CCCC   PRINT*,' '
CCCC   PRINT*,'####    ####      -ERROR-     ####    ####'
CCCC   PRINT*,' '
CCCC   PRINT*,'SECONDS IN SPIN-UP YEAR IS NOT AN '
CCCC   PRINT*,'INTEGRAL NUMBER OF TIME STEPS. '
CCCC   PRINT*,'Check YSEC and DT so that YSEC/DT is an integer:'
CCCC   PRINT*,' '
CCCC   Write(*,200),YSEC/DT
CCCC   PRINT*,'INT(YSEC/DT)-YSEC/DT= ',INT(YSEC/DT)-YSEC/DT
CCCC   PRINT*,'        MOD(YSEC,DT)= ',MOD(YSEC,DT)
CCCC   PRINT*,' '
CCCC   PRINT*,'####        ####          ####        ####'
CCCC   PRINT*,' '
CCCC   stop 999
CCCC     ENDIF
      IF (NCYCLES .GT. 1) then
c f90 -g -DEBUG:fullwarn=on: A real division was encountered in an 
c                            expression being converted to integer.
C        NRUN=SYDAYS*24*3600/DT
        SYSECS=SYDAYS*24*3600
	INTEGERDT=DT
        NRUN=SYSECS/INTEGERDT
      ELSE
        NRUN=NRUN2
      ENDIF
C NEW: NROOT, SLDPTH(K), K=1,NSOIL
C REMOVED:    ZSOIL(K), K=1,NSOIL 

C READ 3 LINES (CHARACTER) TO SKIP COMMENTS (LINE FEED)

      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
        WRITE(*,'(A)') LINEFEED
      END DO

C ATMOSPHERIC DATA FILES TO BE USED FOR FORCING:

        READ(NREAD,'(A)') FILENAME
      PRINT*,'  FILENAME = ', FILENAME
        READ(NREAD,'(A)') FILENAME2
      PRINT*,'  FILENAME2 = ', FILENAME2
C
C READ 3 LINES (CHARACTER) TO SKIP COMMENTS (LINE FEED)
      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO
C  ------------------------
C Land surface characteristics:
C  ------------------------
        READ (NREAD, 100) SOILTP
        READ (NREAD, 100) VEGTYP
        READ (NREAD, 100) SLOPETYP
C ...........SKIP COMMENTS (LINE FEED)
      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO
C ...........
        READ (NREAD, *) (ALBEDOM(K), K=1,12)
        ALBEDOM(13) = ALBEDOM(1)
C ...........SKIP COMMENTS (LINE FEED)
      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO
C ...........
        READ (NREAD, *) (SHDFACM(K), K=1,12)
        DO IMON=1,12
c          WRITE(*,*) IMON,SHDFACM(IMON),SHDMIN
          SHDMAX = MAX(SHDMAX,SHDFACM(IMON))
          SHDMIN = MIN(SHDMIN,SHDFACM(IMON))
        ENDDO
c        WRITE(*,*) SHDMIN
        SHDFACM(13) = SHDFACM(1)

C ...........SKIP COMMENTS (LINE FEED)
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
C ...........
        READ (NREAD, *) SNOALB
        READ (NREAD, 100) ICE

C NEW: ALBEDO, SHDFAC, ICE, 
C REMOVED: LAND, CFACTR
                
C
C READ 3 LINES (CHARACTER) TO SKIP COMMENTS (LINE FEED)
      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO

C ------------------ 
C PHYSICAL PARAMETERS:
C ------------------ 
        READ (NREAD, *) TBOT
C
C READ 3 LINES (CHARACTER) TO SKIP COMMENTS (LINE FEED)
      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO

C ------------------------ 
C INITIAL STATE VARIABLES:
C ------------------------ 
        READ (NREAD, *) T1
        READ (NREAD, *) (STC(K), K=1,NSOIL)
        READ (NREAD, *) (SMC(K), K=1,NSOIL)
        READ (NREAD, *) (SH2O(K), K=1,NSOIL)
        READ (NREAD, *) CMC
        READ (NREAD, *) SNODEP
        READ (NREAD, *) SNEQV
C READ LAST 2 LINES (CHARACTER) TO SHOW THAT CONTROLFILE WAS
C COMPLETELY READ
      DO I=1,2
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   V. KOREN   5/21/97
C      TWO PARAMETERS TO RUN FROZEN GROUND
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                            C
C        READ (NREAD, *)   CVFRZ             C
C        READ (NREAD, *)   FRZK              C
C                                            C
C         READ (NREAD, *) SLOPE              C
C                                            C
C        CVFRZ = 3.0                         C
C        FRZCL = 0.50                        C
C        SLOPE = 0.0005                      C
C                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 100    FORMAT(I7)
 150    FORMAT(I9)
C 250    FORMAT(I12)
C 200    FORMAT(14X,'SYDAYS*24*3600/DT= ',F18.12)
C 400    FORMAT(10(2X, F14.8))
        CLOSE(NREAD)
C
        RETURN
        END     
