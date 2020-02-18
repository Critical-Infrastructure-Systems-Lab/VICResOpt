c     Contain main subroutines of the routing component and reservoir operation
c     Build based on the old make_convolution file 
c     Note: currently the maximum number of reservoirs is 200. Modify the variable declaration parts to increase (if needed)

C************************************************************************************************************************************************************************************
C     Read diffusion file
C************************************************************************************************************************************************************************************
      SUBROUTINE READ_DIFF(DIFF,NCOL,NROW,FILENAME,
     $ IROW, ICOL)
c     Declare variables
      INTEGER NCOL,NROW,IROW,ICOL,I,J
      REAL DIFF(NCOL,NROW)
      CHARACTER*72 FILENAME
c     Subroutine main body
      OPEN(10, FILE = FILENAME,FORM = 'FORMATTED',
     $     STATUS='OLD',ERR=9001)
      DO I = 1,6                  									! Read file, skip header
         READ(10,*)
      END DO
      DO J = IROW,1,-1
         READ(10,*) (DIFF(I,J), I=ICOL,1,-1)
      END DO
      CLOSE(10)
      RETURN
 9001 WRITE(*,*)'CANNOT OPEN INPUT FILE IN READ_DIFF',
     $     FILENAME
      END

C************************************************************************************************************************************************************************************
C     Read fraction file
C************************************************************************************************************************************************************************************
      SUBROUTINE READ_FRACTION(FRACTION,NCOL,NROW,FILENAME,
     $        IROW,ICOL)
c     Declare variables
      INTEGER NCOL,NROW,ICOL,IROW,I,J
      REAL FRACTION(NCOL,NROW)
      CHARACTER*72 FILENAME
c     Subroutine main body
      OPEN(22, FILE = FILENAME,
     &     FORM = 'FORMATTED',STATUS='OLD',ERR=9001)
      DO I = 1,6                 									! Read file, skip header 
         READ(22,*)
      END DO
      DO J = IROW,1,-1
         READ(22,*) (FRACTION(I,J), I=ICOL,1,-1)
      END DO
      CLOSE(22)
      RETURN
 9001 WRITE(*,*) 'CANNOT OPEN INPUT FILE IN READ_FRACTION',
     $     FILENAME
      STOP
      END

C************************************************************************************************************************************************************************************
C     Read GRID_UH
C************************************************************************************************************************************************************************************
      SUBROUTINE READ_GRID_UH
     &    (UH_BOX,KE,PMAX,NOB,CATCHIJ,FILENAME,NORESERVOIRS)
      IMPLICIT NONE
c     Declare variables
      INTEGER KE, PMAX
      INTEGER NOB(200)
      INTEGER CATCHIJ(PMAX,2,200)
      INTEGER N,K,NORESERVOIRS
      REAL    UH_BOX(PMAX,KE)
      REAL    JUNK
      CHARACTER*72 FILENAME
c     Subroutine main body
      DO N = 1,NOB(NORESERVOIRS)
         OPEN(14,FILE = FILENAME,FORM = 'FORMATTED',
     $     STATUS='OLD', ERR=9001)
         DO K = 1,KE
            READ(14,*) JUNK, UH_BOX(N,K)
         END DO
         CLOSE(14)
      END DO
      RETURN
 9001 WRITE(*,*) 'CANNOT OPEN INPUT FILE IN GRID_UH',
     $ FILENAME
      STOP
      END

C************************************************************************************************************************************************************************************
C     Read VELOCITY FILE
C************************************************************************************************************************************************************************************
      SUBROUTINE READ_VELO(VELO,NCOL,NROW,FILENAME,
     $ IROW,ICOL)
c     Declare variables
      INTEGER NCOL,NROW,IROW,ICOL,I,J
      REAL VELO(NCOL,NROW)
      CHARACTER*72 FILENAME
c     Subroutine main body
      OPEN(10, FILE = FILENAME,FORM = 'FORMATTED',
     $     STATUS='OLD', ERR=9001)
      DO I = 1,6                 									! Read file, skip header 
         READ(10,*)
      END DO
      DO J = IROW,1,-1
         READ(10,*) (VELO(I,J), I=ICOL,1,-1)
      END DO
      CLOSE(10)
      RETURN
 9001 WRITE(*,*) 'CANNOT OPEN INPUT FILE IN  READ_VELO',
     $ FILENAME
      STOP
      END

C************************************************************************************************************************************************************************************
C     Read XMASK FILE
C************************************************************************************************************************************************************************************
      SUBROUTINE READ_XMASK(XMASK,NCOL,NROW,FILENAME,
     $        IROW,ICOL)
c     Declare variables
      INTEGER NCOL,NROW,ICOL,IROW,I,J
      REAL XMASK(NCOL,NROW)
      CHARACTER*72, FILENAME
c     Subroutine main body
      OPEN(10, FILE = FILENAME,FORM = 'FORMATTED',
     $     STATUS='OLD',ERR=9001)
      DO I = 1,6                 									! Read file, skip header 
         READ(10,*)
      END DO
      DO J = IROW,1,-1
         READ(10,*, END=20) (XMASK(I,J), I=ICOL,1,-1)
      END DO
      CLOSE(10)
      RETURN
 20   WRITE(*,*) 'REACHED END OF XMASK:  LAST ROW', j
 9001 WRITE(*,*) 'CANNOT OPEN INPUT FILE IN READ_XMASK'
      STOP
      END

C************************************************************************************************************************************************************************************
C     Read flow direction file
C************************************************************************************************************************************************************************************
      SUBROUTINE READ_DIREC(DIREC,NCOL,NROW,H,XC,YC,SIZE
     $     ,FILENAME,IROW,ICOL)
      IMPLICIT NONE
c     Declare variables
      INTEGER NCOL,NROW,I,J,IROW,ICOL,IMISS
      INTEGER DIREC(NCOL,NROW,2)
      INTEGER H(NCOL,NROW)
      REAL XC, YC, SIZE
      CHARACTER*72 FILENAME
      CHARACTER*14 CDUM
c     Subroutine main body
      OPEN(10, FILE = FILENAME, FORM = 'FORMATTED',
     $     STATUS='OLD',ERR=9001)
      READ(10,*) CDUM, ICOL											! number of columms (flow direction matrix)
      READ(10,*) CDUM, IROW											! number of rows
      READ(10,*) CDUM, XC
      READ(10,*) CDUM, YC
      READ(10,*) CDUM, SIZE											! cell size
      READ(10,*) CDUM, IMISS
      IF(IROW.GT.NROW .OR. ICOL.GT.NCOL)THEN
         WRITE(*,*) 'Incorrect dimensions:'
         WRITE(*,*) 'Reset nrow and ncol in main to;',
     $        irow, icol
         STOP
      ENDIF
      DO J = IROW,1,-1
         READ(10,*) (H(I,J), I=ICOL,1,-1)
      END DO
      CLOSE(10)
C     Convert flow direction from 1 - 8 into grid-based cells
      DO I = 1, ICOL												! convert the flow direction matrix
         DO J = 1,IROW
            IF (H(I,J) .EQ. 0) THEN
               DIREC(I,J,1) = 0
               DIREC(I,J,2) = 0
            ELSE IF (H(I,J) .EQ. 1) THEN
               DIREC(I,J,1) = I
               DIREC(I,J,2) = J+1
            ELSE IF (H(I,J) .EQ. 2) THEN
               DIREC(I,J,1) = I-1
               DIREC(I,J,2) = J+1
            ELSE IF (H(I,J) .EQ. 3) THEN
               DIREC(I,J,1) = I-1
               DIREC(I,J,2) = J
            ELSE IF (H(I,J) .EQ. 4) THEN
               DIREC(I,J,1) = I-1
               DIREC(I,J,2) = J-1
            ELSE IF (H(I,J) .EQ. 5) THEN
               DIREC(I,J,1) = I
               DIREC(I,J,2) = J-1
            ELSE IF (H(I,J) .EQ. 6) THEN
               DIREC(I,J,1) = I+1
               DIREC(I,J,2) = J-1
            ELSE IF (H(I,J) .EQ. 7) THEN
               DIREC(I,J,1) = I+1
               DIREC(I,J,2) = J
            ELSE IF (H(I,J) .EQ. 8) THEN
               DIREC(I,J,1) = I+1
               DIREC(I,J,2) = J+1
            END IF
         END DO
      END DO
      RETURN
 9001 WRITE(*,*) 'CANNOT OPEN INPUT FILE IN READ_DIREC',
     $  FILENAME
      STOP
      END

C************************************************************************************************************************************************************************************
C     Estimate flows at reservoirs/outets based on the unit hydrograph and linearlized Saint Vernant methods
C************************************************************************************************************************************************************************************
      SUBROUTINE MAKE_CONVOLUTION
     & (RPATH,RESER,NCOL, NROW, NO_OF_BOX, PMAX, DAYS, CATCHIJ,
     &  BASE, RUNO, FLOW, KE, UH_DAY, UH_S, FRACTION, FACTOR_SUM,
     &  XC, YC, SIZE, DPREC, INPATH,ICOL,NDAY,IDAY,IMONTH,IYEAR, START_YEAR, START_MO,
     &  MO, YR, NYR, VOL, FLOWIN, FLOWOUT, HHO, RESFLOWS, NO,RES_EVAPORATION, NO_STAS)
      IMPLICIT NONE
c     Declare variables
      INTEGER     N, NO, I, J, DAYS, NDAY, II, JJ, K, SODONG
      INTEGER     NCOL,NROW,ICOL,PMAX,KE,UH_DAY
      INTEGER     NO_OF_BOX(200)
      INTEGER     CATCHIJ(PMAX,2,200)
      INTEGER     NYR, START_YEAR, START_MO
      INTEGER     RESER(NCOL,NROW)
      INTEGER     IDAY(DAYS), IMONTH(DAYS), IYEAR(DAYS)
      INTEGER     MO(12*NYR),YR(12*NYR)
      INTEGER     MISS_NUM
      INTEGER     CURRENTYEAR
      INTEGER     MONTH_OF_YEAR
      INTEGER     DPREC, CLEN
      INTEGER     OPEYEAR, NO_STAS
      REAL        RES_EVAPORATION(200,DAYS)
      REAL        UH_S(PMAX,KE+UH_DAY-1,200)
      REAL        BASE(DAYS), RUNO(DAYS), FLOW(DAYS), AIRTEMP(DAYS), WINDS(DAYS), RHD(DAYS)
      REAL        FRACTION(NCOL,NROW)
      REAL        VRESER(200), QRESER(200)
      REAL        HRESERMAX(200), HRESERMIN(200)
      REAL        V0, FLOWINN
      REAL        PI, RERD, FACTOR, FACTOR_SUM
      REAL        DESIGNWL, CURRENTWL
      REAL        JLOC, ILOC, EVA_FACTOR
      REAL        XC, YC, SIZE
      REAL        AREA, AREA_SUM
      REAL        VOL(200,DAYS), FLOWIN(200,DAYS), FLOWOUT(200,DAYS)
      REAL        HHO(200,DAYS)
      REAL        K_CONST
      REAL        DUM1,DUM2,DUM3,DUM4,DUM5,DUM6,DUM7,DUM8,DUM9,DUM10,DUM11,DUM12,DUM13
      REAL        RESFLOWS(200,DAYS)
      REAL        nN, Ra, deltagamma
      REAL        potentialevapo(DAYS)
      REAL        Ramatrix(12, 13)
      REAL        weightingmatrix(45), eamatrix(43), deltaTa4matrix(23)
      REAL        temp, ea, deltaTa4, ed, wind
      REAL        vic_eva_vege(DAYS), vic_trans_vege(DAYS), vic_eva_soil(DAYS)
      LOGICAL      TORF
      CHARACTER*20 RESNO
      CHARACTER*72 RPATH
      CHARACTER*100 TEMPRPATH
      CHARACTER*20 LOC
      CHARACTER*72 INPATH
      PARAMETER   (RERD  = 6371229.0)  											!radius of earth in meters
c     Subroutine main body
C     Ra matrix (mm day-1) for the Penman formula
      Ramatrix = reshape((/1.4, 3.6, 7.0, 11.1, 14.6, 16.4, 15.6, 12.6, 8.5, 4.7, 2.0, 0.9,
     &                     3.7, 6.0, 9.2, 12.7, 15.5, 16.6, 16.1, 13.7, 10.4, 7.1, 4.4, 3.1,
     &                     6.2, 8.4, 11.1, 13.8, 15.9, 16.7, 16.3, 14.7, 12.1, 9.3, 6.8, 5.6,
     &                     8.1, 10.5, 12.8, 14.7, 16.1, 16.5, 16.2, 15.2, 13.5, 11.2, 9.1, 7.9,
     &                     10.8, 12.4, 14.0, 15.2, 15.7, 15.8, 15.8, 15.4, 14.4, 12.9, 11.3, 10.4,
     &                     12.8, 13.9, 14.8, 15.2, 15.0, 14.8, 14.9, 15.0, 14.8, 14.2, 13.1, 12.5,
     &                     14.6, 15.0, 15.2, 14.7, 13.9, 13.4, 13.6, 14.3, 14.9, 15.0, 14.6, 14.3,
     &                     15.9, 15.7, 15.1, 13.9, 12.5, 11.7, 12.0, 13.1, 14.4, 15.4, 15.7, 15.8,
     &                     16.8, 16.0, 14.5, 12.5, 10.7, 9.7, 10.1, 11.6, 13.6, 15.3, 16.4, 16.9,
     &                     17.2, 15.8, 13.5, 10.9, 8.6, 7.5, 7.9, 9.7, 12.3, 14.8, 16.7, 17.5,
     &                     17.3, 15.1, 12.2, 8.9, 6.4, 5.2, 5.6, 7.6, 10.7, 13.8, 16.5, 17.8,
     &                     16.9, 14.1, 10.4, 6.7, 4.1, 2.9, 3.4, 5.4, 8.7, 12.5, 16.0, 17.6,
     &                     16.5, 12.6, 8.3, 4.3, 1.8, 0.9, 1.3, 3.1, 6.5, 10.8, 15.1, 17.5 /),
     &                     shape(Ramatrix))	 
c     Weighting factor delta/gamma matrix for the Penman formula
      weightingmatrix = (/0.69, 0.71, 0.73, 0.76, 0.78, 0.80, 0.83, 0.85, 0.88, 0.91, 0.94, 0.97,
     &                  1.00, 1.03, 1.06, 1.09, 1.13, 1.16, 1.19, 1.23, 1.26, 1.30, 1.34, 1.38, 
     &                  1.42, 1.47, 1.51, 1.56, 1.60, 1.65, 1.69, 1.74, 1.79, 1.84, 1.89, 1.94,
     &                  1.99, 2.04, 2.11, 2.17, 2.23, 2.28, 2.35, 2.41, 2.48/)
C     Ea matrix
      eamatrix = (/4.58, 4.75, 4.93, 5.11, 5.30, 5.49, 5.69, 5.89, 6.10, 6.32, 6.54, 6.77, 7.02, 7.26,
     &            7.52, 7.79, 8.05, 8.33, 8.62, 8.91, 9.21, 9.52, 9.84, 10.18, 10.52, 10.87, 11.23, 
     &            11.61, 11.99, 12.38, 12.79, 13.13, 13.63, 14.08, 14.53, 15.00, 15.49, 15.97, 16.47,
     &            17.53, 18.68, 19.80, 21.10/)
C     DeltaTa4matrix for the Penman formula
      deltaTa4matrix = (/11.0, 11.2, 11.4, 11.5, 11.7, 11.9, 12.0, 12.2, 12.3, 12.5, 12.7, 12.9, 13.1,
     &            13.3, 13.5, 13.7, 13.9, 14.0, 14.2, 14.4, 14.6, 14.8, 15.0/)
C     MISS_NUM is the number of grid cell output files not found
      MISS_NUM=0
C     *** 0 <= K_CONST = 1.0
C *** K_CONST smaller 1.0 makes it a simple linear storage
      K_CONST = 1.0
      PI = ATAN(1.0) * 4.0
      AREA_SUM   = 0.0
      FACTOR_SUM = 0.0
      OPEYEAR = 0
      CURRENTYEAR = START_YEAR
      DO I = 1,NDAY
         FLOW(I) = 0.0
         RES_EVAPORATION(NO,I) = 0.0
      END DO
C     Look for starting year
      IF (NO .NE. NO_STAS) THEN
         WRITE(RESNO,*) NO
         TEMPRPATH = trim(RPATH)//"res"//trim(ADJUSTL(RESNO))//".txt"			! only calculate open surface water evaporation after the commision year
         OPEN(26, FILE = TEMPRPATH,FORM = 'FORMATTED', STATUS='OLD',ERR=9002)
         READ(26,*)
         READ(26,*) DUM1,DUM2,DUM3,DUM4,DUM5,DUM6, OPEYEAR
         CLOSE(26)
      END IF
C     Calculate the area of each cell
      DO N = 1,NO_OF_BOX(NO) 
         DO I = 1,NDAY
            RUNO(I) = 0.0
            BASE(I) = 0.0
         END DO
         II = CATCHIJ(N,1,NO)
         JJ = CATCHIJ(N,2,NO)
c     the grid has been flipped left to right
c     find the revised cooordinates
         ILOC=XC + (ICOL-II)*SIZE + SIZE/2.0
         JLOC=YC + JJ*SIZE - SIZE/2.0
C        CONVERSIONFACTOR for mm/day to ft**3/sec
         AREA =  RERD**2*ABS(SIZE)*PI/180*										! give area of box in square meters
     &        ABS(SIN((JLOC-SIZE/2.0)*PI/180)-
     $        SIN((JLOC+SIZE/2.0)*PI/180))
         AREA_SUM = AREA_SUM + AREA
c         WRITE(*,*) N, ILOC, JLOC
         FACTOR = FRACTION(II,JJ)*35.315*AREA/(86400.0*1000.0)  				!convert to sq.mi. by cell fract (original) - later, we convert back to SI unit
         FACTOR_SUM = FACTOR_SUM + FACTOR
         call create_vic_names(jloc,iloc,loc,clen,dprec)
         INQUIRE(FILE=INPATH(1:(INDEX(INPATH,' ')-1))//LOC(1:CLEN),
     $                     EXIST=TORF)
         IF(torf)THEN
           OPEN(20,FILE=INPATH(1:(INDEX(INPATH,' ')-1))//
     $        LOC(1:CLEN),
     $        STATUS='OLD',ERR=9001)
           DO I = 1,NDAY 
             READ(20,*,END=9001,ERR=9001) IYEAR(I), IMONTH(I), IDAY(I),
     &        DUM1, DUM2, RUNO(I), BASE(I), DUM3, DUM4, DUM5, DUM6, DUM7, vic_eva_vege(I),
     &        vic_trans_vege(I), vic_eva_soil(I), DUM8, DUM9, DUM10, DUM11, DUM12, RHD(I), DUM13,
     &        AIRTEMP(I),WINDS(I) 												! (IMPORTANT) modify here accordingly depending on your selected outputs from rainfall-runoff
c     check to be sure dates in VIC file start at same time specified
c     in input file
             IF(I.eq.1) THEN
               IF(IYEAR(I).ne.YR(1) . or. IMONTH(I).ne.MO(1)) THEN
                  print*, 'VIC output file does not match specified '
                  print*, 'period in input file.'
                  stop
               ENDIF
             ENDIF
           END DO
         else
           print*, INPATH(1:(INDEX(INPATH,' ')-1))//LOC(1:CLEN),
     &             ' NOT FOUND, INSERTING ZEROS'
           miss_num = miss_num+1
          DO i=1,nday
             IYEAR(I)=9999
             IMONTH(I)=99
             IDAY(I)=99
             runo(i)=0
             base(i)=0
             potentialevapo(i) = 0
         END DO
        ENDIF
        IF (RESER(II,JJ) .EQ. 9999) THEN
        ! Calculate evaporation via water surface according to Penman formua (see Penman, 1948)
        ! Note: the vic rainfall-runoff uses equations in Handbook of Hydrology (Penman-Monteith evapotranspiration)
        ! E0 = (delta/gamma*H+Ea)/(delta/gamma+1)
        ! H = (1-r)Rin-R0
        ! (1-r)Rin = 0.95Ra(0.18*0.55n/N)
        ! n/N is represented by nN which is the ration between actual sunshine hours and possible sunshine hours
            nN = 0.6 															! modify if needed
            DO I = 1,NDAY
                ! Ra is the solar radian (depending on location, see J.S.G. McCulloch, 1965. E. African Forest. J. (3) 286-295 for appropiate values
                MONTH_OF_YEAR = INT(MOD(NDAY,365)/30) + START_MO	! appropiate (ignore 29,31, not so important in this case)
                CURRENTYEAR = START_YEAR + INT(MOD(NDAY,365))
                IF (CURRENTYEAR>=OPEYEAR) THEN
                    IF (MONTH_OF_YEAR>12) THEN
                        MONTH_OF_YEAR = 12
                    END IF
                IF (INT(JLOC/10)>60) THEN
                    Ra = Ramatrix(MONTH_OF_YEAR,1)
                ELSE IF ((INT(JLOC/10)<=60) .AND. (INT(ILOC/10)>=0)) THEN
                    Ra = Ramatrix(MONTH_OF_YEAR,(7-INT(JLOC/10)))
                ELSE IF (INT(JLOC/10)<-60) THEN
                    Ra = Ramatrix(MONTH_OF_YEAR,13)
                ELSE 
                    Ra = Ramatrix(MONTH_OF_YEAR,(7-INT(JLOC/10)))
                END IF
                ! delta/gamma is depedent on temperature
                temp = AIRTEMP(I)        										! (degree celcius)
                wind = WINDS(I) * 24 * 3600 / 1609       						! (miles/day)
                IF (temp<0) THEN
                    deltagamma = 0.69											! could be modifed (using intepolation)
                ELSE IF (temp>22) THEN
                    deltagamma = 2.48											! could be modifed (using intepolation)
                ELSE
                    deltagamma = weightingmatrix(1+int(temp*2))
                END IF
                ! calculate the saturation vapor pressure ea
                IF (temp<0) THEN
                    ea = 4.4													! could be modifed (using intepolation)
                ELSE if (temp>22) THEN
                    ea = 21.1													! could be modifed (using intepolation)
                ELSE
                    ea = eamatrix(1+int(temp*2))
                END IF
                ! calculate deltaTa4
                IF (temp<-1) THEN												! minimum value in the table is -1o Celcius
                    deltaTa4 = 11.0												! could be modifed (using intepolation)
                ELSE IF (temp>21) THEN 											! maximum value in the table is 21o Celcius
                    deltaTa4 = 15.0												! could be modifed (using intepolation)
                ELSE
                    deltaTa4 = deltaTa4matrix(int(temp)+2)
                END IF
                ! actual vapor pressure
                ed = ea * RHD(I) / 100
                ! potential evaporation 
                potentialevapo(I) = (deltagamma*(0.95*(0.18+0.55*nN)*Ra
     &             - deltaTa4*(0.56-0.09*sqrt(ed))*(0.10+0.90*nN))
     &             + 0.35*(0.5+wind/100)*(ea-ed))/(deltagamma+1)
                ! 0.95 = 1.0 - 0.05 (albedo for water)
                ! convert potential to actual evaporation by multiplying a factor of K (modify if needed)
                EVA_FACTOR = 0.9
                RES_EVAPORATION(NO,I) = RES_EVAPORATION(NO,I)+potentialevapo(I)*EVA_FACTOR	! this is the total water loss due to evaporation in a reservoir (for checking only)
              END IF
            END DO
        END IF  
        ! Calculate baseflow and runoff
        DO I = 1,NDAY
            RUNO(I) = RUNO(I) * FACTOR * 0.028 			 ! 0.028 convert from cfs into cms
            BASE(I) = BASE(I) * FACTOR * 0.028
        END DO
        DO I = 1,NDAY
            DO J = 1,KE+UH_DAY-1
                IF ((I-J+1) .GE. 1) THEN
                    FLOW(I) = FLOW(I)+UH_S(N,J,NO)*(BASE(I-J+1)+RUNO(I-J+1))
                END IF
            END DO   
            ! allow negative values - evaporation is higher than inflow
            IF (potentialevapo(I)*EVA_FACTOR>(vic_eva_vege(I)+vic_eva_soil(I)+vic_trans_vege(I))) THEN
                ! also consider the water losses due to evapotranspiration calculated by VIC rainfall-runoff
                FLOW(I) = FLOW(I)-potentialevapo(I)*FACTOR*0.028/1000*EVA_FACTOR 
     &          + (vic_eva_soil(I)+vic_eva_vege(I)+vic_trans_vege(I))*FACTOR*0.028/1000
            END IF
            IF (potentialevapo(I)<0.0001) THEN 			! if too small, set to 0, also avoid negative values
                potentialevapo(I) = 0
            END IF
            RESFLOWS(NO,I) = FLOW(I)
        END DO
        CLOSE(20)
      END DO
      if(MISS_NUM>0) then
        print*, MISS_NUM, ' files not found, zero runoff/baseflow used'
      end if
      RETURN
 9001 WRITE(*,*) 'Error reading time-series data, ',
     $     'insufficient data or missing input file',
     $     INPATH(1:INDEX(INPATH,' ')-1)//LOC(1:CLEN)
 9002 WRITE(*,*) 'Error in reading reservoir data'
      END

C************************************************************************************************************************************************************************************
C     Determine the distance/time from the downstream cell of a reservoir to its cascade reservoir
C************************************************************************************************************************************************************************************
      SUBROUTINE CALCULATE_DISTANCE(SI,SJ,ICOL,IROW,NCOL,NROW,DIREC,TRVLTIME,
     & RESORDER,RESER,FINAL,NORESERVOIRS,SIZE_CELL,VELO,FI,FJ)
      IMPLICIT NONE
c     Declare variables  
      INTEGER SI,SJ,II,JJ,COUNTCELL,FINAL,NCOL,NROW,NORESERVOIRS,FI,FJ
      INTEGER DIREC(NCOL,NROW,2)
      INTEGER RESER(NCOL,NROW)
      INTEGER I,J,ICOL,IROW,III,JJJ,RESORDER
      REAL    VELO(NCOL,NROW)
      REAL    TRVLTIME(200)
      REAL    SIZE_CELL
c     Subroutine main body
      II = SI
      JJ = SJ
      COUNTCELL = 0
 400  CONTINUE
      IF ((II .GT. ICOL) .OR. (II .LT.1) .OR.
     &      (JJ .GT. IROW) .OR. (JJ .LT.1)) THEN
               GOTO 410
      END IF
      IF (((RESER(II,JJ)>0) .AND. (RESER(II,JJ) .NE. 9999))
     &     .OR. (II .EQ. FI) .AND. (JJ .EQ. FJ) .AND. (FINAL .EQ. 1)) THEN
           GOTO 410
      ELSE
          IF ((DIREC(II,JJ,1).NE.0) .AND.    										!check if the current
     &      (DIREC(II,JJ,2) .NE.0)) THEN   											!ii,jj cell routes down
                III = DIREC(II,JJ,1)         										!to the subbasin outlet
                JJJ = DIREC(II,JJ,2)         										!point, following the
                II  = III                    										!direction of direc(,)
                JJ  = JJJ                    										!from each cell
                COUNTCELL = COUNTCELL + 1											!this is for checking purposes, could be deleted
                IF (FINAL .EQ. 0) THEN
                    TRVLTIME(RESORDER) = TRVLTIME(RESORDER) + SIZE_CELL*1000*116/VELO(SI,SJ)	! +1 for the current cell (116km = 1 degree = 116000m; ignore the distortion of cells in the upper and lower lattitude)
                ELSE 
                    TRVLTIME(NORESERVOIRS) = TRVLTIME(NORESERVOIRS) + SIZE_CELL*1000*116/VELO(SI,SJ)			! +1 for the current cell (116km = 1 degree = 116000m; ignore the distortion of cells in the upper and lower lattitude)
                END IF
                GOTO 400
         END IF                           											!if you get there,
      END IF                                										!no_of_box increments
 410  CONTINUE       
      TRVLTIME(RESORDER) = TRVLTIME(RESORDER)/24/3600								! convert to traveling time in number of days
      RETURN
      END

C************************************************************************************************************************************************************************************
C     Determine which cells contribute to flows at each reservoirs
C************************************************************************************************************************************************************************************

      SUBROUTINE SEARCH_CATCHMENTRS
     & (PI,PJ,DIREC,NCOL,NROW,NO_OF_BOX,CATCHIJ,PMAX,
     $  IROW,ICOL,NORESERVOIRS,RES_DIRECT,RESER,FINAL,SIZE_CELL,VELO,FI,FJ)
      IMPLICIT NONE
c     Declare variables
      INTEGER PI,PJ,I,J,NCOL,NROW,PMAX,ICOL,IROW
      INTEGER II, JJ, III, JJJ, K,FI,FJ
      INTEGER DIREC(NCOL,NROW,2)
      INTEGER NO_OF_BOX(200)
      INTEGER CATCHIJ(PMAX,2,200)
      INTEGER RESER(NCOL,NROW)
      INTEGER RES_DIRECT(200,3)
      INTEGER NORESERVOIRS,RESORDER
      INTEGER FINAL
      REAL    SIZE_CELL
      REAL    TRVLTIME(200)
      REAL    VELO(NCOL,NROW)
c     Subroutine main body
      RESORDER = NORESERVOIRS
      DO I = 1, NORESERVOIRS
        IF (RESER(PI,PJ).EQ. RES_DIRECT(I,1)) THEN
            RESORDER = I
        END IF
      END DO
      NO_OF_BOX(RESORDER)=0
      If (FINAL .EQ. 1) THEN
        RESORDER = NORESERVOIRS
      END IF
      ! This part adds cell to bags (each bag represents one reservoir)
      DO I = 1, ICOL
         DO J = 1, IROW
            II = I
            JJ = J
 300        CONTINUE 
            IF ((II .GT. ICOL) .OR. (II .LT.1) .OR.
     &      (JJ .GT. IROW) .OR. (JJ .LT.1)) THEN
               GOTO 320
            END IF
            IF ((II .EQ.  PI) .AND. (JJ .EQ. PJ)) THEN
               NO_OF_BOX(RESORDER) = NO_OF_BOX(RESORDER) + 1
               CATCHIJ(NO_OF_BOX(RESORDER),1,RESORDER) = I
               CATCHIJ(NO_OF_BOX(RESORDER),2,RESORDER) = J
               WRITE(*,*) 'Cell (',I,',',J,')---> Res ID ',RES_DIRECT(RESORDER,1)				! show which cells go to which reservoir
               IF (FINAL .EQ. 0) THEN
                    GOTO 310 
               ELSE
                    GOTO 320
               END IF
            ELSE IF ((FINAL .EQ. 1) .AND. (RESER(II,JJ)>0)) THEN
                GOTO 320
            ELSE IF ((RESER(II,JJ)>0) .AND.(RESER(II,JJ) .NE. 9999)) THEN
                    GOTO 310
            ELSE
               IF ((DIREC(II,JJ,1).NE.0) .AND.    								!check if the current
     &             (DIREC(II,JJ,2) .NE.0)) THEN   								!ii,jj cell routes down
                     III = DIREC(II,JJ,1)         								!to the subbasin outlet
                     JJJ = DIREC(II,JJ,2)         								!point, following the
                     II  = III                    								!direction of direc(,)
                     JJ  = JJJ                    								!from each cell
                     GOTO 300
               END IF   														!if you get there,
          END IF                                								!no_of_box increments
 310      CONTINUE                              								!and you try another
          IF ((I .GE. 2) .AND. (I .LT. (ICOL-1)).AND. (J .GE. 2)				! this part to track which cell a reservoir releases to
     &    .AND. (J .LT. (IROW-1))) THEN
                IF ((RESER(I-1,J) .GT. 0) .AND. (DIREC(I-1,J,1) .EQ. I)
     &            .AND. (DIREC(I-1,J,2) .EQ. J) .AND. (RESER(I-1,J) .NE. 9999)) THEN
                    DO K = 1, NORESERVOIRS
                        IF (RESER(I-1,J) .EQ. RES_DIRECT(K,1)) THEN
                            RES_DIRECT(K,3) =  NO_OF_BOX(RESORDER)
                            CALL CALCULATE_DISTANCE(I,J,ICOL,IROW,NCOL,NROW,DIREC,TRVLTIME,
     &                       RESORDER,RESER,FINAL,NORESERVOIRS,SIZE_CELL,VELO,FI,FJ)
                        END IF
                    END DO
                END IF
                IF ((RESER(I+1,J) .GT.0) .AND. (DIREC(I+1,J,1) .EQ. I)
     &            .AND. (DIREC(I+1,J,2) .EQ. J) .AND. (RESER(I+1,J) .NE.9999)) THEN
                    DO K = 1, NORESERVOIRS
                        IF (RESER(I+1,J) .EQ. RES_DIRECT(K,1)) THEN
                            RES_DIRECT(K,3) =  NO_OF_BOX(RESORDER)
                            CALL CALCULATE_DISTANCE(I,J,ICOL,IROW,NCOL,NROW,DIREC,TRVLTIME,
     &                      RESORDER,RESER,FINAL,NORESERVOIRS,SIZE_CELL,VELO,FI,FJ)
                        END IF
                    END DO
                END IF
                IF ((RESER(I,J-1) .GT.0) .AND. (DIREC(I,J-1,1) .EQ. I)
     &            .AND. (DIREC(I,J-1,2) .EQ. J) .AND. (RESER(I,J-1) .NE.9999)) THEN
                    DO K = 1, NORESERVOIRS
                        IF (RESER(I,J-1) .EQ. RES_DIRECT(K,1)) THEN
                            RES_DIRECT(K,3) =  NO_OF_BOX(RESORDER)
                            CALL CALCULATE_DISTANCE(I,J,ICOL,IROW,NCOL,NROW,DIREC,TRVLTIME,
     &                      RESORDER,RESER,FINAL,NORESERVOIRS,SIZE_CELL,VELO,FI,FJ)
                        END IF
                    END DO
                END IF
                IF ((RESER(I,J+1) .GT.0) .AND. (DIREC(I,J+1,1) .EQ. I)
     &            .AND. (DIREC(I,J+1,2) .EQ. J) .AND. (RESER(I,J+1) .NE.9999)) THEN
                    DO K = 1, NORESERVOIRS
                        IF (RESER(I,J+1) .EQ. RES_DIRECT(K,1)) THEN
                            RES_DIRECT(K,3) =  NO_OF_BOX(RESORDER)
                            CALL CALCULATE_DISTANCE(I,J,ICOL,IROW,NCOL,NROW,DIREC,TRVLTIME,
     &                      RESORDER,RESER,FINAL,NORESERVOIRS,SIZE_CELL,VELO,FI,FJ)
                        END IF
                    END DO
                END IF
                IF ((RESER(I-1,J-1) .GT.0) .AND. (DIREC(I-1,J-1,1) .EQ. I)
     &          .AND. (DIREC(I-1,J-1,2) .EQ. J) .AND. (RESER(I-1,J-1) .NE.9999)) THEN
                    DO K = 1, NORESERVOIRS
                        IF (RESER(I-1,J-1) .EQ. RES_DIRECT(K,1)) THEN
                            RES_DIRECT(K,3) =  NO_OF_BOX(RESORDER)
                            CALL CALCULATE_DISTANCE(I,J,ICOL,IROW,NCOL,NROW,DIREC,TRVLTIME,
     &                      RESORDER,RESER,FINAL,NORESERVOIRS,SIZE_CELL,VELO,FI,FJ)
                        END IF
                    END DO
                END IF
                IF ((RESER(I-1,J+1) .GT.0) .AND. (DIREC(I-1,J+1,1) .EQ. I)
     &          .AND. (DIREC(I-1,J+1,2) .EQ. J) .AND. (RESER(I-1,J+1) .NE.9999)) THEN
                    DO K = 1, NORESERVOIRS
                        IF (RESER(I-1,J+1) .EQ. RES_DIRECT(K,1)) THEN
                            RES_DIRECT(K,3) =  NO_OF_BOX(RESORDER)	
                            CALL CALCULATE_DISTANCE(I,J,ICOL,IROW,NCOL,NROW,DIREC,TRVLTIME,
     &                      RESORDER,RESER,FINAL,NORESERVOIRS,SIZE_CELL,VELO,FI,FJ)
                        END IF
                    END DO
                END IF
                IF ((RESER(I+1,J-1) .GT.0) .AND. (DIREC(I+1,J-1,1) .EQ. I)
     &          .AND. (DIREC(I+1,J-1,2) .EQ. J) .AND. (RESER(I+1,J-1) .NE.9999)) THEN
                    DO K = 1, NORESERVOIRS
                        IF (RESER(I+1,J-1) .EQ. RES_DIRECT(K,1)) THEN
                            RES_DIRECT(K,3) =  NO_OF_BOX(RESORDER)
                            CALL CALCULATE_DISTANCE(I,J,ICOL,IROW,NCOL,NROW,DIREC,TRVLTIME,
     &                      RESORDER,RESER,FINAL,NORESERVOIRS,SIZE_CELL,VELO,FI,FJ)
                        END IF
                    END DO
                END IF
                IF ((RESER(I+1,J+1) .GT.0) .AND. (DIREC(I+1,J+1,1) .EQ. I)
     &           .AND. (DIREC(I+1,J+1,2) .EQ. J) .AND. (RESER(I+1,J+1) .NE.9999)) THEN
                    DO K = 1, NORESERVOIRS
                        IF (RESER(I+1,J+1) .EQ. RES_DIRECT(K,1)) THEN
                            RES_DIRECT(K,3) =  NO_OF_BOX(RESORDER)
                            CALL CALCULATE_DISTANCE(I,J,ICOL,IROW,NCOL,NROW,DIREC,TRVLTIME,
     &                      RESORDER,RESER,FINAL,NORESERVOIRS,SIZE_CELL,VELO,FI,FJ)
                        END IF
                    END DO
                END IF
            END IF
320         CONTINUE
        END DO
      END DO
      II = PI
      JJ = PJ
500   CONTINUE				! this part is to locate the sequence of reservoirs
      IF ((II .GT. ICOL) .OR. (II .LT.1) .OR.
     &      (JJ .GT. IROW) .OR. (JJ .LT.1)) THEN
                RES_DIRECT(RESORDER,2) = 0
                GOTO 510
      END IF
      IF ((RESER(II,JJ)>0) .AND. (RESER(II,JJ) .NE. 9999)
     &     .AND. ((II .NE. PI) .OR. (JJ .NE. PJ))) THEN
                RES_DIRECT(RESORDER,2) = RESER(II,JJ)
                GOTO 510
      ELSE
          IF ((DIREC(II,JJ,1).NE.0) .AND.    										!check if the current
     &      (DIREC(II,JJ,2) .NE.0)) THEN   											!ii,jj cell routes down
                III = DIREC(II,JJ,1)         										!to the subbasin outlet
                JJJ = DIREC(II,JJ,2)         										!point, following the
                II  = III                    										!direction of direc(,)
                JJ  = JJJ                    										!from each cell
                GOTO 500
         END IF                           											!if you get there,
      END IF                                										!no_of_box increments
510   CONTINUE 
      WRITE(*,*) 'Res ',RES_DIRECT(RESORDER,1),'---> Res ',RES_DIRECT(RESORDER,2)
      WRITE(*,*) 'Number of grid cells upstream of present reservoir',
     $     no_of_box(RESORDER)
      RETURN
      END

C************************************************************************************************************************************************************************************
C     Consider rule curves and opearting rules
C************************************************************************************************************************************************************************************

      SUBROUTINE MAKE_CONVOLUTIONRS
     & (RPATH,RESER,NCOL, NROW, NO_OF_BOX, PMAX, DAYS, CATCHIJ,
     &  BASE, RUNO, FLOW, KE, UH_DAY, UH_S, FRACTION, FACTOR_SUM,
     &  XC, YC, SIZE, DPREC, INPATH,ICOL,NDAY,IDAY,IMONTH,IYEAR, START_YEAR,START_MO,
     &  MO,YR,NYR,VOL,FLOWIN,FLOWOUT,HHO,ENERGYPRO,HTK,DIREC,IROW
     &  , XNE, YNE, NO_STAS,RES_DIRECT,RES_EVAPORATION,TRVLTIME)
      IMPLICIT NONE
c     Declare variables
      INTEGER     N, DAYS, NDAY, START_YEAR,START_MO
      INTEGER     NCOL,NROW,ICOL,PMAX,KE,UH_DAY
      INTEGER     CATCHIJ(PMAX,2,200)
      INTEGER     NYR
      INTEGER     RESER(NCOL,NROW)
      INTEGER     IDAY(DAYS), IMONTH(DAYS), IYEAR(DAYS)
      INTEGER     MO(12*NYR),YR(12*NYR)
      INTEGER     DIREC(NCOL,NROW,2)
      INTEGER     RES_DIRECT(200,3)
      INTEGER     YEAR(200,DAYS),MONTH(200,DAYS),DAY(200,DAYS)
      INTEGER     OPEYEAR(200)
      INTEGER     IROW, CURRENTYEAR
      INTEGER     NO_OF_BOX(200)
      INTEGER     XNE, YNE
      INTEGER     I, J, K, L, NO_STAS
      INTEGER     NO_OF_ROW(200)
      INTEGER     RULE(200)
      INTEGER     STARTDAY(200)
      INTEGER     DPREC, RESORDER
      REAL        UH_S(PMAX,KE+UH_DAY-1,200)
      REAL        TRVLTIME(200)
      REAL        BASE(DAYS), RUNO(DAYS), FLOW(DAYS)
      REAL        FRACTION(NCOL,NROW)
      REAL        PI, RERD, FACTOR, FACTOR_SUM
      REAL        CURRENTWL, DESIGNWL
      REAL        RES_EVAPORATION(200,DAYS)
      REAL        XC, YC, SIZE
      REAL        VOL(200,DAYS), FLOWIN(200,DAYS), FLOWOUT(200,DAYS)
      REAL        HHO(200,DAYS), ENERGYPRO(200,DAYS), HTK(200,DAYS)
      REAL        HMAX(200), HMIN(200)
      REAL        RESFLOWS(200,DAYS) 
      REAL        HRESERMAX(200), HRESERMIN(200), VINITIAL(200), H0(200)
      REAL        QRESER(200),VRESER(200),REALHEAD(200),VRESERTHAT(200),VDEAD(200)
      REAL        QRESERTHAT(200), HYDRAUHEAD(200)
      REAL        OP1(200,2) ! rule curve
      REAL        OP2(200,DAYS) ! pre-defined time series data
      REAL        X1(200), X2(200), X3(200), X4(200)	! operating rule
      REAL        SEEPAGE(200), INFILTRATION(200), Demand(200)
      REAL        CRTDATE, TEMPO
      REAL        RESLV(200,12), OP5X1(200,12), OP5X2(200,12), OP5X3(200,12), OP5X4(200,12), DEMAND5(200,12)
      REAL        KFACTOR
      CHARACTER*72 RPATH
      CHARACTER*100 TEMPRPATH,PATHRES3
      CHARACTER*72 INPATH
      CHARACTER*20 CHUOI
      CHARACTER*20 NAMERS
      CHARACTER*100 FILENAME
c     Subroutine main body
c     Look for reservoirs sequences
      CURRENTYEAR = START_YEAR
      DO N = 1,NO_STAS
        print*, 'Working on reservoir no...',RES_DIRECT(N,1)
        CALL MAKE_CONVOLUTION
     &        (RPATH,RESER,NCOL, NROW, NO_OF_BOX, PMAX, DAYS,
     &        CATCHIJ, BASE, RUNO, FLOW, KE, UH_DAY, UH_S, FRACTION,
     &        FACTOR_SUM,XC,YC,SIZE,DPREC,INPATH,ICOL,NDAY,
     &        IDAY,IMONTH,IYEAR, START_YEAR, START_MO, MO, YR, NYR, VOL,
     &        FLOWIN, FLOWOUT, HHO, RESFLOWS,RES_DIRECT(N,1),RES_EVAPORATION, NO_STAS)
      END DO
c    Initiate reservoir parameters
      DO I = 1, NDAY
        DO J = 1, NO_STAS-1
            FLOWIN(J,I) = 0
            FLOWOUT(J,I) = 0
            VOL(J,I) = 0
            ENERGYPRO(J,I) = 0
            HTK(J,I) = 0
        END DO
      END DO
      DO  J  = 1, NO_STAS-1
       WRITE(CHUOI,*) RES_DIRECT(J,1)
       TEMPRPATH = trim(RPATH)//"res"//trim(ADJUSTL(CHUOI))//".txt"
       OPEN(25, FILE = TEMPRPATH,FORM = 'FORMATTED',
     & STATUS='OLD',ERR=9002)
       READ(25,*)  
       READ(25,*) HRESERMAX(J),HRESERMIN(J),VRESERTHAT(J),VDEAD(J),
     & HYDRAUHEAD(J), QRESERTHAT(J), OPEYEAR(J), VINITIAL(J)
       KFACTOR = SQRT(VDEAD(J)/VRESERTHAT(J))							! calculate the bottom level of reservoir
       H0(J) = HRESERMIN(J) - abs(HRESERMAX(J)-HRESERMIN(J))*KFACTOR/(1-KFACTOR)
       READ(25,*)
       READ(25,*) SEEPAGE(J), INFILTRATION(J)
       READ(25,*)
       READ(25,*) RULE(J)
       IF (RULE(J) .EQ. 1) THEN ! simplified rule curve
            READ(25,*) HMAX(J), HMIN(J), OP1(J,1),OP1(J,2)
            CLOSE(25)
       ELSE IF (RULE(J) .EQ. 2) THEN ! rule curve
            READ(25,*) RESLV(J,1), RESLV(J,2), RESLV(J,3), RESLV(J,4), RESLV(J,5), RESLV(J,6),
     & RESLV(J,7), RESLV(J,8), RESLV(J,9), RESLV(J,10), RESLV(J,11), RESLV(J,12)
       ELSE IF (RULE(J) .EQ. 3) THEN ! operating rule
            READ(25,*) Demand(J), X1(J), X2(J), X3(J), X4(J)
            CLOSE(25)
       ELSE IF (RULE(J) .EQ. 4) THEN! predefined time-series
            READ(25,'(/A)')FILENAME
            CLOSE(25)
            PATHRES3 = trim(ADJUSTL(FILENAME))
            OPEN(26, FILE = PATHRES3,FORM = 'FORMATTED',STATUS='OLD',ERR=9002)
            READ(26,*) NO_OF_ROW(J) 						! Read the number of rows of reservoir J th
            DO L = 1, NO_OF_ROW(J)
                READ(26,*) YEAR(J,L), MONTH(J,L), DAY(J,L), OP2(J,L)
                IF ((YEAR(J,L) .EQ. START_YEAR) .AND. (MONTH(J,L) .EQ. START_MO) .AND. (DAY(J,L) .EQ. 1)) THEN
                    STARTDAY(J) = L							! If the time-series is shorter than the simulation period (release = 0)
                END IF
            END DO
            CLOSE(26)
       ELSE IF (RULE(J) .EQ. 5) THEN
            DO L = 1, 12
                READ(25,*) DEMAND5(J,L), OP5X1(J,L), OP5X2(J,L), OP5X3(J,L), OP5X4(J,L)
            END DO
            CLOSE(25)
       END IF
       IF (CURRENTYEAR>=OPEYEAR(J)) THEN
            VOL(J,1) = VINITIAL(J)
       ELSE
            VOL(J,1) = 0
       END IF
      END DO
      DO I = 1, NDAY
       DO J = 1, NO_STAS-1
         CURRENTYEAR = START_YEAR + (START_MO-1)*30 + INT(I/365)		! approximate, does not consider leap years
         IF (CURRENTYEAR>=OPEYEAR(J)) THEN
            VRESER(J) = VRESERTHAT(J)
            REALHEAD(J) = HYDRAUHEAD(J)
            QRESER(J) = QRESERTHAT(J)
         ELSE
            VRESER(J) = 0.000001		! set the storage of the reservoir cell to a small number if the reservoir does not go into operations yet
            REALHEAD(J) = 0
            QRESER(J) = 999999
         END IF
         FLOWIN(J,I) = RESFLOWS(J,I)
c        Calculate the designed water level
c        Note RULE = 1: simplified rule curve = 2: rule curve = 3: operating rules: =4 pre-defined time-series data
         CRTDATE = 1.0* mod(I,365)+(START_MO-1)*30						! approcimate
         IF ((RULE(J) .EQ. 1) .or. (RULE(J) .EQ. 2)) THEN   ! (Options 1 and 2: rule curves)
            IF (RULE(J) .EQ. 1) THEN
                ! If Day 1 > Day 2 ---- switch the values
                IF (OP1(J,1)>OP1(J,2)) THEN
                    TEMPO = OP1(J,1)
                    OP1(J,1) = OP1(J,2)
                    OP1(J,2) = TEMPO
                END IF
                ! Caculate target water level
                IF ((CRTDATE .GT. OP1(J,1)) .and. (CRTDATE .LT. OP1(J,2))) THEN
                    IF (HMAX(J) .GE. HMIN(J)) THEN
                        DESIGNWL=(CRTDATE-OP1(J,1))/(OP1(J,2)-OP1(J,1))
     &                  *(HMAX(J)-HMIN(J))
                    ELSE
                        DESIGNWL=(HMIN(J)-HMAX(J))-(CRTDATE-OP1(J,1))/(OP1(J,2)-OP1(J,1))
     &                  *(HMIN(J)-HMAX(J))
                    END IF
                ELSE IF (CRTDATE .GE. OP1(J,2)) THEN
                    IF (HMAX(J) .GE. HMIN(J)) THEN 
                        DESIGNWL=(HMAX(J)-HMIN(J))
     &                  -(CRTDATE-OP1(J,2))/(365-OP1(J,2)+OP1(J,1))*
     &                  (HMAX(J)-HMIN(J))
                    ELSE
                        DESIGNWL=(CRTDATE-OP1(J,2))/(365-OP1(J,2)+OP1(J,1))*
     &                  (HMIN(J)-HMAX(J))
                    END IF
                ELSE
                    IF (HMAX(J) .GE. HMIN(J)) THEN 
                        DESIGNWL=(HMAX(J)-HMIN(J))
     &                  -(CRTDATE+365-OP1(J,2))/(365-OP1(J,2)+OP1(J,1))*
     &                  (HMAX(J)-HMIN(J))
                    ELSE
                        DESIGNWL=(CRTDATE+365-OP1(J,2))/(365-OP1(J,2)+OP1(J,1))*
     &                  (HMIN(J)-HMAX(J))
                    END IF
                END IF
                DESIGNWL = DESIGNWL + (HMIN(J) - HRESERMIN(J))
            ELSE
                IF (MOD(I,365) .LE. 31) THEN !jan
                    DESIGNWL = RESLV(J,1)
                ELSE IF (MOD(I,365) .LE. 59) THEN ! feb
                    DESIGNWL = RESLV(J,2)
                ELSE IF (MOD(I,365) .LE. 90) THEN ! mar
                    DESIGNWL = RESLV(J,3)
                ELSE IF (MOD(I,365) .LE. 120) THEN ! apr
                    DESIGNWL = RESLV(J,4)
                ELSE IF (MOD(I,365) .LE. 151) THEN ! may
                    DESIGNWL = RESLV(J,5)
                ELSE IF (MOD(I,365) .LE. 181) THEN ! jun
                    DESIGNWL = RESLV(J,6)
                ELSE IF (MOD(I,365) .LE. 212) THEN ! jul
                    DESIGNWL = RESLV(J,7)
                ELSE IF (MOD(I,365) .LE. 243) THEN ! aug
                    DESIGNWL = RESLV(J,8)
                ELSE IF (MOD(I,365) .LE. 273) THEN ! sep
                    DESIGNWL = RESLV(J,9)
                ELSE IF (MOD(I,365) .LE. 304) THEN ! oct
                    DESIGNWL = RESLV(J,10)
                ELSE IF (MOD(I,365) .LE. 334) THEN ! nov
                    DESIGNWL = RESLV(J,11)
                ELSE ! dec
                    DESIGNWL = RESLV(J,12)
                END IF
                DESIGNWL = DESIGNWL - H0(J)
            END IF
            HTK(J,I) = DESIGNWL + H0(J)													! water head
            CURRENTWL = VOL(J,I) * (HRESERMAX(J)-H0(J))/VRESER(J)
            IF (CURRENTWL>=DESIGNWL) THEN												! Zone 3
                IF ((VOL(J,I)+FLOWIN(J,I)*24*3.6 -QRESER(J)*24*3.6)						! Case 2
     &          >(DESIGNWL* VRESER(J)) /(HRESERMAX(J)-H0(J))) THEN
                    VOL(J,I+1) = VOL(J,I) + FLOWIN(J,I)*24*3.6-QRESER(J)*24*3.6
                    FLOWOUT(J,I) = QRESER(J)
                ELSE																	! Case 1
                    VOL(J,I+1)=(DESIGNWL*VRESER(J))/(HRESERMAX(J)-H0(J))
                    FLOWOUT(J,I)=(VOL(J,I)-VOL(J,I+1))/24/3.6 + FLOWIN(J,I)
                END IF
                GOTO 123
            ELSE																		! Zone 2
                IF ((VOL(J,I)+FLOWIN(J,I)*24*3.6)> ((DESIGNWL* VRESER(J))				! Case 2
     &           /(HRESERMAX(J)-H0(J)))) THEN
                    VOL(J,I+1)=(DESIGNWL * VRESER(J))/(HRESERMAX(J)
     &              -H0(J))
                    FLOWOUT(J,I)=FLOWIN(J,I)-(VOL(J,I+1)-VOL(J,I))/24/3.6
                    IF (FLOWOUT(J,I)>QRESER(J)) THEN
                        VOL(J,I+1)=VOL(J,I+1)+(FLOWOUT(J,I)-QRESER(J))*24*3.6
                        FLOWOUT(J,I) = QRESER(J)
                    END IF
                    GOTO 123
                ELSE																	! Case 1 (this case covers Zone 1 + Zone 2 case 1)
                    VOL(J,I+1) = VOL(J,I) + FLOWIN(J,I)*24*3.6
                    GOTO 123
                END IF
            END IF
        ELSE IF (RULE(J) .EQ. 3) THEN ! opearting rule (Option 3)
        ! Note x1 and x4 in radian (0 to pi/2), not degree
            IF (VOL(J,I) < VDEAD(J)) THEN ! below dead water level
                FLOWOUT(J,I) = 0																					! case 1
            ELSE IF (VOL(J,I) .LE. X2(J)) THEN ! hedging
                IF ((VOL(J,I)-VDEAD(J)+FLOWIN(J,I)*24*3.6) .LE. (Demand(J)+(VOL(J,I)-X2(J))*tan(X1(J))*24*3.6)) THEN		! discharge more than the water amount in reservoir (case 2)
                    FLOWOUT(J,I) = (VOL(J,I)-VDEAD(J))/24/3.6+FLOWIN(J,I)											! discharge all of water in the reservoir
                ELSE	! (case 3)
                    FLOWOUT(J,I) = Demand(J)+(VOL(J,I)-X2(J))*tan(X1(J))
                END IF
            ELSE IF (VOL(J,I).GE. X3(J)) THEN 	! spilling
                IF ((Demand(J) + (VOL(J,I)-X3(J))*tan(X4(J)))*24*3.6>(VOL(J,I))-VDEAD(J)) THEN		! discharge more than the water in reservoir (just to make sure)
                    FLOWOUT(J,I) = (VOL(J,I)-VDEAD(J))/24/3.6										! discharge all of water in the reservoir
                ELSE IF ((Demand(J) + (VOL(J,I)-X3(J))*tan(X4(J)))>QRESER(J)) THEN
                    FLOWOUT(J,I) = QRESER(J)
                ELSE	 ! case 5
                    FLOWOUT(J,I) = Demand(J) + (VOL(J,I)-X3(J))*tan(X4(J))
                END IF
            ELSE ! releasing
                IF (Demand(J)*24*3.6>(VOL(J,I)-VDEAD(J))) THEN
                    FLOWOUT(J,I) = (VOL(J,I)-VDEAD(J))/24/3.6
                ELSE
                    FLOWOUT(J,I) = Demand(J)			! case 4
                END IF
            END IF
            IF (FLOWOUT(J,I)>QRESER(J)) THEN			! double check just in case users chose unrealistic x1 and x4
                FLOWOUT(J,I) = QRESER(J)
            END IF
            VOL(J,I+1) = VOL(J,I) + (FLOWIN(J,I)-FLOWOUT(J,I))*24*3.6
            GOTO 123
         ELSE IF (RULE(J) .EQ. 4) THEN! pre-defined time series (Option 4)
            IF ((OP2(J,I+STARTDAY(J)) .GT. QRESER(J)) .AND. (VOL(J,I) .LT. VRESER(J))) THEN
                OP2(J,I+STARTDAY(J)) = QRESER(J)
            END IF
            IF (OP2(J,I+STARTDAY(J))*24*3.6 .GT. ((VOL(J,I)-VDEAD(J)))+FLOWIN(J,I)*24*3.6) THEN
                FLOWOUT(J,I) = (VOL(J,I)-VDEAD(J))/24/3.6 + FLOWIN(J,I)
            ELSE
                FLOWOUT(J,I) = OP2(J,I+STARTDAY(J))
            END IF
            VOL(J,I+1) = VOL(J,I) + (FLOWIN(J,I)-FLOWOUT(J,I))*24*3.6
            GOTO 123
         ELSE IF (RULE(J) .EQ. 5) THEN! pre-defined time series (Option 5) - note: this option is similar to OP3 but for a periodic demand
            ! Note x1 and x4 in radian (0 to pi/2), not degree, this part can be shorthen
            IF (MOD(I,365) .LE. 31) THEN !jan
                X1(J) = OP5X1(J,1)
                X2(J) = OP5X2(J,1)
                X3(J) = OP5X3(J,1)
                X4(J) = OP5X4(J,1)
                Demand(J) = DEMAND5(J,1)
            ELSE IF (MOD(I,365) .LE. 59) THEN ! feb
                X1(J) = OP5X1(J,2)
                X2(J) = OP5X2(J,2)
                X3(J) = OP5X3(J,2)
                X4(J) = OP5X4(J,2)
                Demand(J) = DEMAND5(J,2)
            ELSE IF (MOD(I,365) .LE. 90) THEN ! mar
                X1(J) = OP5X1(J,3)
                X2(J) = OP5X2(J,3)
                X3(J) = OP5X3(J,3)
                X4(J) = OP5X4(J,3)
                Demand(J) = DEMAND5(J,3)
            ELSE IF (MOD(I,365) .LE. 120) THEN ! apr
                X1(J) = OP5X1(J,4)
                X2(J) = OP5X2(J,4)
                X3(J) = OP5X3(J,4)
                X4(J) = OP5X4(J,4)
                Demand(J) = DEMAND5(J,4)
            ELSE IF (MOD(I,365) .LE. 151) THEN ! may
                X1(J) = OP5X1(J,5)
                X2(J) = OP5X2(J,5)
                X3(J) = OP5X3(J,5)
                X4(J) = OP5X4(J,5)
                Demand(J) = DEMAND5(J,5)
            ELSE IF (MOD(I,365) .LE. 181) THEN ! jun
                X1(J) = OP5X1(J,6)
                X2(J) = OP5X2(J,6)
                X3(J) = OP5X3(J,6)
                X4(J) = OP5X4(J,6)
                Demand(J) = DEMAND5(J,6)
            ELSE IF (MOD(I,365) .LE. 212) THEN ! jul
                X1(J) = OP5X1(J,7)
                X2(J) = OP5X2(J,7)
                X3(J) = OP5X3(J,7)
                X4(J) = OP5X4(J,7)
                Demand(J) = DEMAND5(J,7)
            ELSE IF (MOD(I,365) .LE. 243) THEN ! aug
                X1(J) = OP5X1(J,8)
                X2(J) = OP5X2(J,8)
                X3(J) = OP5X3(J,8)
                X4(J) = OP5X4(J,8)
                Demand(J) = DEMAND5(J,8)
            ELSE IF (MOD(I,365) .LE. 273) THEN ! sep
                X1(J) = OP5X1(J,9)
                X2(J) = OP5X2(J,9)
                X3(J) = OP5X3(J,9)
                X4(J) = OP5X4(J,9)
                Demand(J) = DEMAND5(J,9)
            ELSE IF (MOD(I,365) .LE. 304) THEN ! oct
                X1(J) = OP5X1(J,10)
                X2(J) = OP5X2(J,10)
                X3(J) = OP5X3(J,10)
                X4(J) = OP5X4(J,10)
                Demand(J) = DEMAND5(J,10)
                Demand(J) = DEMAND5(J,10)
            ELSE IF (MOD(I,365) .LE. 334) THEN ! nov
                X1(J) = OP5X1(J,11)
                X2(J) = OP5X2(J,11)
                X3(J) = OP5X3(J,11)
                X4(J) = OP5X4(J,11)
                Demand(J) = DEMAND5(J,11)
            ELSE ! dec
                X1(J) = OP5X1(J,12)
                X2(J) = OP5X2(J,12)
                X3(J) = OP5X3(J,12)
                X4(J) = OP5X4(J,12)
                Demand(J) = DEMAND5(J,12)
            END IF
            IF (VOL(J,I) < VDEAD(J)) THEN ! below dead water level
                FLOWOUT(J,I) = 0																					! case 1
            ELSE IF (VOL(J,I) .LE. X2(J)) THEN ! hedging
                IF ((VOL(J,I)-VDEAD(J)+FLOWIN(J,I)*24*3.6) .LE. (Demand(J)+(VOL(J,I)-X2(J))*tan(X1(J))*24*3.6)) THEN		! discharge more than the water amount in reservoir (case 2)                    
                    FLOWOUT(J,I) = (VOL(J,I)-VDEAD(J))/24/3.6+FLOWIN(J,I)											! discharge all of water in the reservoir
                ELSE	! (case 3)
                    FLOWOUT(J,I) = Demand(J)+(VOL(J,I)-X2(J))*tan(X1(J))
                END IF
            ELSE IF (VOL(J,I).GE. X3(J)) THEN 	! spilling
                IF ((Demand(J) + (VOL(J,I)-X3(J))*tan(X4(J)))*24*3.6>(VOL(J,I))-VDEAD(J)) THEN		! discharge more than the water in reservoir (just to make sure)
                    FLOWOUT(J,I) = (VOL(J,I)-VDEAD(J))/24/3.6										! discharge all of water in the reservoir
                ELSE IF ((Demand(J) + (VOL(J,I)-X3(J))*tan(X4(J)))>QRESER(J)) THEN
                    FLOWOUT(J,I) = QRESER(J)
                ELSE	 ! case 5
                    FLOWOUT(J,I) = Demand(J) + (VOL(J,I)-X3(J))*tan(X4(J))
                END IF
            ELSE ! releasing
                IF (Demand(J)*24*3.6>(VOL(J,I)-VDEAD(J))) THEN
                    FLOWOUT(J,I) = (VOL(J,I)-VDEAD(J))/24/3.6
                ELSE
                    FLOWOUT(J,I) = Demand(J)			! case 4
                END IF
            END IF
            IF (FLOWOUT(J,I)>QRESER(J)) THEN			! double check just in case users chose unrealistic x1 and x4
                FLOWOUT(J,I) = QRESER(J)
            END IF
            VOL(J,I+1) = VOL(J,I) + (FLOWIN(J,I)-FLOWOUT(J,I))*24*3.6
            GOTO 123
 123     END IF
         ! Check if there are any negative values
         IF (ENERGYPRO(J,I)<0) THEN
            ENERGYPRO(J,I)=0
         END IF
         IF (FLOWOUT(J,I)<0) THEN
            FLOWOUT(J,I)=0
         END IF
         IF (VOL(I,J)<0)THEN ! Not allow dropping below the minimum water level (mostly due to evaporation)
            VOL(I,J)=0
         END IF
         ! Calculate energy production
         IF (VOL(J,I+1)<=VRESER(J)) THEN
               ENERGYPRO(J,I) = 0.9 * 9.81 * FLOWOUT(J,I)
         ELSE
               ENERGYPRO(J,I) = 0.9 * 9.81 * QRESER(J)
         END IF
         !IF (J .EQ. 3) THEN
         !    WRITE(*,*) RES_DIRECT(J,1),' - ',ENERGYPRO(J,I),'  ',HHO(J,I),'  ',HHO(J,I+1),'  ',FLOWOUT(J,I),'  ',HRESERMAX(J),'  ',REALHEAD(J)
         !END IF
         ! Update water losses due to seepage and infiltration
         ! Infiltration is permanent losses + water seepage is added to outflow
         ! Note seepage occurs until there is no water left (considering dead volume also)
         IF (VOL(J,I+1) - (SEEPAGE(J)+INFILTRATION(J))*24*3.6 .GT. 0) THEN
            VOL(J,I+1) = VOL(J,I+1) - (SEEPAGE(J)+INFILTRATION(J))*24*3.6
            !Note that seepage does not contribute to energy production, so we add here
            FLOWOUT(J,I) = FLOWOUT(J,I) + SEEPAGE(J)
         ELSE
            VOL(J,I+1) = 0
         END IF
c        Check the neccesity to spill water
         IF (VOL(J,I+1)>VRESER(J)) THEN
             FLOWOUT(J,I) = FLOWOUT(J,I)+(VOL(J,I+1)-VRESER(J))/24/3.6
             VOL(J,I+1) = VRESER(J)
         END IF
         HHO(J,I)=VOL(J,I)/VRESER(J)* (HRESERMAX(J)-H0(J))
     & + H0(J)
         HHO(J,I+1)=VOL(J,I+1)/VRESER(J)* (HRESERMAX(J)-H0(J))
     & + H0(J)
         ! Note: hydraulic head calculated from the maximum water level
         ENERGYPRO(J,I) = ENERGYPRO(J,I) *
     &   (( HHO(J,I)+HHO(J,I+1))/2-(HRESERMAX(J)-REALHEAD(J)))/1000			! this part is for hydropower production estimation, ignore if work with irrigation reservoirs
        ! Propagate water to the downstream reservoir, considering the time lag
         RESORDER = NO_STAS
         DO K = 1, NO_STAS
            IF ((RES_DIRECT(K,1) .EQ. RES_DIRECT(J,2))) THEN
                RESORDER = K
            END IF
         END DO
         IF (FLOWOUT(J,I) .LT. 0) THEN
            FLOWOUT(J,I) = 0
         END IF
         IF (RES_DIRECT(J,2) .EQ. 0) THEN
            RESFLOWS(NO_STAS,I+1+INT(TRVLTIME(J))) = RESFLOWS(NO_STAS,I+1+INT(TRVLTIME(J))) + FLOWOUT(J,I)
         ELSE
            RESFLOWS(RESORDER,I+1+INT(TRVLTIME(J))) = RESFLOWS(RESORDER,I+1+INT(TRVLTIME(J))) + FLOWOUT(J,I)
         END IF
      !   WRITE(*,*) '-------------------------------------------------'
      !   WRITE(*,*) 'Hdesign ',HTK(J,I)
      !   WRITE(*,*) 'CRTDATE',CRTDATE,'Reservoir No.',J,' FLOWIN ',FLOWIN(J,I),
     &!  'FLOWOUT',FLOWOUT(J,I),' VOL ',VOL(J,I),' ELEC', ENERGYPRO(J,I)
        END DO
        IF (RESFLOWS(NO_STAS,I) .LT. 0) THEN
             RESFLOWS(NO_STAS,I) = 0
        END IF
        FLOW(I) = RESFLOWS(NO_STAS,I)
      END DO
      RETURN
 9001 WRITE(*,*) 'Error reading UH data'
 9002 WRITE(*,*) 'Error in reading reservoir data'
      END
C     END OF FILE
C************************************************************************************************************************************************************************************