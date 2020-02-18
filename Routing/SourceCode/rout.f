      PROGRAM rout
c     Routing algorithm developed by D. Lohmann.
c     Code first developed by the University of Washington. See WA Hydrology Homepage for more details.
c     Modified by the Resilient Water Systems Group/Singapore University of Technology and Design to account for reservoir operations.
c     Reservoir presentation and operations are incorporated by the following steps
c     1. We first calculate the time lag from each cell to a reservoir / the basin outlet
c     2. We then calculate the flow to each reservoirs and the outlet
c     3. Track the reservoir network
c     4. Reservoir operations are modelled based on rule curves (1)/ operating rules (2)/ predefined time-series data (3)
      IMPLICIT NONE

C************************************************************************************************************************************************************************************
C     Declare variables
c     RCS ID STRING
      CHARACTER*50 RCSID
c     DATA RCSID/"$Id: read_routines.f,v1.0 2019/04/17$"/
      INTEGER   IARGC
      INTEGER   isaleap
      EXTERNAL  isaleap
C     Change dimensions here:
C     1. nrow and ncol should be larger than the grid
C     2. nyr should equal run length yrs+1
      INTEGER NROW, NCOL, DAYS, NYR
      PARAMETER (NROW = 360, NCOL = 360)
      PARAMETER (NYR = 35)
C     -------------- No changes after here -------------------------------------------------------
C     -------------- Note: 200 is the maximum number of reservoirs. Change if more reservoirs added
C     Unit-hydrograph parameters
      INTEGER   KE, LE, TMAX, UH_DAY, PMAX
      REAL      DT
      PARAMETER (DAYS=NYR*366)
      PARAMETER (KE   = 12)
      PARAMETER (LE   = 48)
      PARAMETER (DT   = 3600.0)
      PARAMETER (UH_DAY = 96)
      PARAMETER (TMAX = UH_DAY*24)
      PARAMETER (PMAX = 15000)
      REAL      UH_BOX(PMAX,KE),UHM(NCOL,NROW,LE)
      REAL      UH_S(PMAX,KE+UH_DAY-1,200)
      REAL      UH_DAILY(PMAX,UH_DAY)
      REAL      FR(TMAX,2)
      CHARACTER*80 UH_STRING
C     Routing
      REAL      AREA,FACTOR_SUM
      REAL      XC,YC,SIZE
      REAL      FDUM
      REAL      VELO(NCOL,NROW), DIFF(NCOL,NROW)
      REAL      XMASK(NCOL,NROW), FRACTION(NCOL,NROW)
      REAL      BASE(DAYS), RUNO(DAYS), FLOW(DAYS)
      INTEGER   DIREC(NCOL,NROW,2)
      INTEGER   IDAY(DAYS), IMONTH(DAYS), IYEAR(DAYS)
      INTEGER   MO(12*NYR),YR(12*NYR)
      INTEGER   NO_OF_BOX(200)
      INTEGER   CATCHIJ(PMAX,2,200)
      INTEGER   H(NCOL,NROW)
      INTEGER   PI,PJ,NR
      INTEGER   IROW,ICOL
      INTEGER   LP,M,Y,J
      INTEGER   DPREC,FINAL
      INTEGER   NDAY
      INTEGER   NMONTHS
      LOGICAL   TORF
C     Reservoir parameters
      INTEGER   RESER(NCOL,NROW)
      REAL      VOL(200,DAYS),FLOWIN(200,DAYS),FLOWOUT(200,DAYS)
      REAL      HHO(200,DAYS),LUONGDIEN(200,DAYS),HTK(200,DAYS)
      INTEGER   NORESERVOIRS,N
      INTEGER   RES_DIRECT(200,3)
      REAL      RES_EVAPORATION(200,DAYS)
      REAL      TRVLTIME(200)
C     Filename
      CHARACTER*21 NAME
      CHARACTER*20 NAMERS
      CHARACTER*5  NAME5
      CHARACTER*72 FILE_INPUT,FILENAME,RFILENAME
      CHARACTER*72 INPATH,OUTPATH,RPATH
C     Variables for monthly means
      INTEGER   DAYS_IN_MONTH(12)
      DATA      DAYS_IN_MONTH /31,28,31,30,31,30,31,31,30,31,30,31/
      INTEGER   START_YEAR,STOP_YEAR,FIRST_YEAR,LAST_YEAR
      INTEGER   START_MO,STOP_MO,FIRST_MO,LAST_MO
      REAL      MONTHLY(12*NYR)
      REAL      MONTHLY_mm(12*NYR)
      REAL      YEARLY(12)
      REAL      YEARLY_mm(12)
      NORESERVOIRS = 0

C************************************************************************************************************************************************************************************
C     OPEN NECESSARY FILES
C************************************************************************************************************************************************************************************
c     Process commandline args
      IF(IARGC().NE.1)THEN
           PRINT*, 'USAGE:  rout <infile>'
           STOP
      ENDIF
      CALL GETARG(1,FILE_INPUT)
      OPEN(1,FILE=FILE_INPUT,STATUS='OLD',ERR=9001)
      READ(1,'(//A)') FILENAME
      CALL READ_DIREC(DIREC,NCOL,NROW,H,XC,YC,SIZE,
     $     FILENAME,IROW,ICOL)
c     Process velocity file
      READ(1,*)
      READ(1,*) TORF
      IF(TORF)THEN
           READ(1,'(A)') FILENAME
           CALL READ_VELO(VELO,NCOL,NROW,FILENAME,IROW,ICOL)
      ELSE
           READ(1,*) FDUM
           CALL INIT_ARRAY(VELO,NCOL,NROW,FDUM)
      ENDIF
c     Process diffusion file
      READ(1,*)
      READ(1,*)TORF
      IF(TORF)THEN
          READ(1,'(A)') FILENAME
          CALL READ_DIFF(DIFF,NCOL,NROW,FILENAME,IROW,ICOL)
      ELSE
          READ(1,*) FDUM
          CALL INIT_ARRAY(DIFF,NCOL,NROW,FDUM)
      ENDIF
c     Process xmask file
      READ(1,*)
      READ(1,*)TORF
      IF(TORF)THEN
          READ(1,'(A)') FILENAME
          CALL READ_XMASK(XMASK,NCOL,NROW,FILENAME,IROW,ICOL)
      ELSE
          READ(1,*) FDUM
          CALL INIT_ARRAY(XMASK,NCOL,NROW,FDUM)
      ENDIF
c     Read fraction file
      READ(1,*)
      READ(1,*)TORF
      IF(TORF)THEN
          READ(1,'(A)') FILENAME
          CALL READ_FRACTION(FRACTION,NCOL,NROW,FILENAME,IROW,ICOL)
      ELSE
          READ(1,*) FDUM
          CALL INIT_ARRAY(FRACTION,NCOL,NROW,FDUM)
      ENDIF
c     Read station file
      READ(1,'(/A)')FILENAME
      OPEN(10,FILE=FILENAME)
c     Read input path and precision of VIC filenames
      READ(1,'(/A)')INPATH
      READ(1,*)DPREC
c     Read output pathname
      READ(1,'(/A)')OUTPATH
c     Read input path of reservoir information
      READ(1,'(/A)') RFILENAME
      READ(1,'(/A)') RPATH
c     Read input file name of reservoir locations
      CALL READ_RESE(RESER,ICOL,IROW,NCOL,NROW,RFILENAME)
c     Number of days to process
      READ(1,*)
c     Start and end year/month from VIC simulation
      READ(1,*) START_YEAR, START_MO, STOP_YEAR, STOP_MO
c     Calculate number of days & months in simulation
      M=START_MO
      Y=START_YEAR
      NMONTHS = 0
      NDAY=0
      DO J=START_MO,12*(STOP_YEAR-START_YEAR)+STOP_MO
        IF(M.EQ.2) THEN
           LP=isaleap(Y)
        ELSE
           LP=0
        ENDIF
        NDAY = NDAY+DAYS_IN_MONTH(M)+LP
        NMONTHS = NMONTHS + 1
        MO(NMONTHS) = M
        YR(NMONTHS) = Y
        M = M + 1
        IF (M .GT. 12) THEN
            M = 1
            Y  = Y + 1
        ENDIF
      END DO
      IF(NDAY.GT.DAYS) THEN
         PRINT*, 'IN ROUT.F RESET DAYS TO ', NDAY
         STOP
      ENDIF
      PRINT*,'NDAY = ',NDAY, ' NMONTHS = ',NMONTHS
c     Read start and end year/month for writing output
      READ(1,*) FIRST_YEAR, FIRST_MO, LAST_YEAR, LAST_MO
c     Read uh file
      READ(1,'(/A)')FILENAME

C************************************************************************************************************************************************************************************
C     START MODELLING
C************************************************************************************************************************************************************************************

c     Calculate impulse response function for grid cells
      CALL MAKE_UHM(UHM,VELO,DIFF,XMASK,NCOL,NROW,LE,DT,IROW,ICOL)
C     Loop over required stations
 100  CONTINUE
      READ(10,*,END=110) NR,NAME,PI,PJ,AREA
      READ(10,'(A80)',END=110) UH_STRING
      IF (NR .EQ. 1) THEN
        WRITE(*,'(I2,2X,A,I4,I4,G12.6)')
     &       NR, NAME, PI, PJ
        PRINT*, 'Routing station: ', NAME
        PI=ICOL+1-PI 						!note, the arrays are flipped left to right
        NAME5 = NAME
c     Look for cells, contributing to the outlet
        print*, 'Searching catchment...'
        CALL SEARCH_WHOLECATCHMENT
     &      (PI,PJ,DIREC,NCOL,NROW,
     &       NO_OF_BOX,CATCHIJ,PMAX,IROW,ICOL,NORESERVOIRS,RES_DIRECT,
     &       RESER)
        print*, 'Reading grid_UH...'
c     Read a pre-defined UH grid
        CALL READ_GRID_UH
     &      (UH_BOX,KE,PMAX,NO_OF_BOX, CATCHIJ,FILENAME,NORESERVOIRS)
c     Make UH grid for reservoir catchments
        print*, 'Making grid UH for reservoirs...'
        DO N = 1,NO_OF_BOX(NORESERVOIRS)
        IF ((RESER(CATCHIJ(N,1,NORESERVOIRS),CATCHIJ(N,2,NORESERVOIRS))
     &        .GT.0) .AND. (RESER(CATCHIJ(N,1,NORESERVOIRS),CATCHIJ(N,2,NORESERVOIRS))
     &        .NE.9999)) THEN					! the second condition can be removed
           WRITE(NAMERS,*) RESER(CATCHIJ(N,1,NORESERVOIRS),CATCHIJ(N,2,NORESERVOIRS))
           NAMERS = "r"//trim(adjustl(NAMERS))//"UH"
           CALL SEARCH_CATCHMENTRS(CATCHIJ(N,1,NORESERVOIRS),
     &           CATCHIJ(N,2,NORESERVOIRS),DIREC,NCOL,NROW,NO_OF_BOX,CATCHIJ,PMAX,
     &           IROW,ICOL,NORESERVOIRS,RES_DIRECT,RESER,0,SIZE,VELO,PI,PJ)
           CALL MAKE_GRID_UH
     &          (DIREC, NO_OF_BOX, UH_DAY, TMAX, PI, PJ, LE, UH_DAILY, KE,
     &          CATCHIJ,UHM, FR, PMAX, NCOL, NROW, UH_BOX, UH_S,
     &          UH_STRING,NAMERS,NORESERVOIRS,RESER,
     &          RESER(CATCHIJ(N,1,NORESERVOIRS),CATCHIJ(N,2,NORESERVOIRS)),
     &          RES_DIRECT)
        END IF
      END DO
c     Make UH grid for the rest of the basin to the basin outlet
      print*, 'making grid UH...'
      CALL SEARCH_CATCHMENTRS(PI,PJ,
     &          DIREC,NCOL,NROW,NO_OF_BOX,CATCHIJ,PMAX,
     &          IROW,ICOL,NORESERVOIRS,RES_DIRECT,RESER,1,SIZE,VELO,PI,PJ)
      CALL MAKE_GRID_UH
     &         (DIREC, NO_OF_BOX, UH_DAY, TMAX, PI, PJ, LE, UH_DAILY, KE,
     &         CATCHIJ,UHM, FR, PMAX, NCOL, NROW, UH_BOX, UH_S,
     &         UH_STRING,NAME5,NORESERVOIRS,RESER,NORESERVOIRS,RES_DIRECT)
c     Flow generation
      print*, 'making convolution...'
         CALL MAKE_CONVOLUTIONRS
     &         (RPATH,RESER,NCOL, NROW, NO_OF_BOX, PMAX, DAYS,
     &         CATCHIJ, BASE, RUNO, FLOW, KE, UH_DAY, UH_S, FRACTION,
     &         FACTOR_SUM,XC,YC,SIZE,DPREC,INPATH,ICOL,NDAY,
     &         IDAY,IMONTH,IYEAR, START_YEAR, START_MO, MO, YR, NYR, VOL,
     &         FLOWIN, FLOWOUT, HHO, LUONGDIEN,HTK,DIREC,IROW,
     &         PI,PJ,NORESERVOIRS,RES_DIRECT,RES_EVAPORATION,TRVLTIME)
c     Writing data into files
      print*, 'writing data...'
         CALL WRITE_DATA
     &        (FLOW, NDAY, NAME5, FACTOR_SUM, OUTPATH,IDAY,IMONTH,IYEAR,
     &        VOL,FLOWIN, FLOWOUT, HHO, LUONGDIEN,HTK,RESER,NCOL,NROW,
     &        ICOL,IROW, RPATH, NORESERVOIRS,RES_DIRECT,RES_EVAPORATION,NO_OF_BOX)
         CALL WRITE_MONTH
     &        (DAYS_IN_MONTH,START_YEAR, STOP_YEAR, FIRST_YEAR,
     &        LAST_YEAR, START_MO, STOP_MO, FIRST_MO,LAST_MO,
     &        NAME5, DAYS, FLOW, FACTOR_SUM, MONTHLY, MONTHLY_mm,
     &        YEARLY,YEARLY_mm,OUTPATH,NDAY,IMONTH,IYEAR,MO,YR,NMONTHS,NYR)
      END IF
      GOTO 100
 110  CONTINUE
      STOP
 9001 WRITE(*,*) 'CANNOT OPEN: ', FILE_INPUT
      END
C************************************************************************************************************************************************************************************
c     FUNCTION  ISALEAP
      integer function isaleap( iyr )
c     return 1 if a leap yr else 0
      if( (mod(iyr,4) .eq. 0 .and. mod(iyr,100) .ne.0)
     $                       .or. mod(iyr,400) .eq. 0) then
         isaleap = 1
      else
         isaleap = 0
      endif
      end
C     END OF FILE
C************************************************************************************************************************************************************************************