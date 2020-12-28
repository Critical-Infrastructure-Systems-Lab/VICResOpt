c     SUBROUTINES RELATED TO WRITING RESULTS TO FILES

C************************************************************************************************************************************************************************************
C     WRITE DAILY DATA AT THE BASIN OUTLET AND RESERVOIRS
C************************************************************************************************************************************************************************************
      SUBROUTINE WRITE_DATA
     & (FLOW, DAYS, NAME5, FACTOR_SUM, OUTPATH,IDAY,IMONTH,IYEAR,
     &  VOL, FLOWIN, FLOWOUT, HHO, HYDROPOWER,HTK, RESER, NCOL,NROW,
     &  ICOL, IROW, RPATH, NO_STAS,RES_DIRECT,RES_EVAPORATION,NO_OF_BOX)
      IMPLICIT NONE
c     Declare variables
c     RCS ID STRING
      INTEGER       IROW, ICOL
      INTEGER       NO_OF_BOX(200)
      INTEGER       DAYS
      INTEGER       NROW, NCOL
      INTEGER       IDAY(DAYS), IMONTH(DAYS), IYEAR(DAYS)
      INTEGER       I, J, K, CLEN
      INTEGER       RES_DIRECT(200,3)
      INTEGER       RESER(NCOL,NROW)
      INTEGER       H(NCOL,NROW)
      INTEGER       RULE, IRRIGATION
      INTEGER       NO_STAS, NAM
      REAL          VOL(200,DAYS),FLOWIN(200,DAYS),FLOWOUT(200,DAYS)
      REAL          HHO(200,DAYS),HYDROPOWER(200,DAYS),HTK(200,DAYS)
      REAL          SEEPAGE, INFILTRATION
      REAL          RES_EVAPORATION(200,DAYS)
      REAL          HRESERMAX, HRESERMIN
      REAL          QRESER,VRESER,HPHATDIEN,QSPILL,QTUR,VDEAD,VINI
      REAL          X1,X2,X3
      REAL          FLOW(DAYS)
      REAL          FACTOR_SUM
      CHARACTER*200 TEMPRPATH, ABC
      CHARACTER*72  RPATH, IPATH
      CHARACTER*20  CHUOI
      CHARACTER*5   NAME5
      CHARACTER*72  OUTPATH, TENHO
c     Subroutine main body
      CLEN = INDEX(OUTPATH,' ')-1
      OPEN(30, FILE = OUTPATH(1:CLEN)//NAME5//'.day')
      OPEN(31, FILE = OUTPATH(1:CLEN)//NAME5//'.day_mm')
      DO I = 1,DAYS
            WRITE(30,*) IYEAR(I),IMONTH(I),IDAY(I),FLOW(I)
            WRITE(31,*) IYEAR(I),IMONTH(I),IDAY(I),FLOW(I) / FACTOR_SUM
      END DO
      CLOSE(30)
      CLOSE(31)
      DO I=1, NO_STAS-1
            WRITE(CHUOI,*) RES_DIRECT(I,1)
            TEMPRPATH = trim(RPATH)//"res"//trim(ADJUSTL(CHUOI))//".txt"
            OPEN(25, FILE = TEMPRPATH,FORM = 'FORMATTED',
     &      STATUS='OLD')
            READ(25,*)
            READ(25,*) HRESERMAX, HRESERMIN, VRESER, VDEAD, HPHATDIEN,
     &      QRESER, NAM, VINI, TENHO
            READ(25,*)
            READ(25,*) SEEPAGE, INFILTRATION
            READ(25,*)
            READ(25,*) IRRIGATION
            READ(25,*) IPATH
            READ(25,*)
            READ(25,*) RULE
            CLOSE(25)
            WRITE(CHUOI,*) RES_DIRECT(I,1)
            print*, 'Exporting data for Reservoir ',TENHO
            OPEN(40, FILE = OUTPATH(1:CLEN)//'reservoir_'
     &       //trim(ADJUSTL(CHUOI))//'.day')
            IF ((RULE .EQ. 1) .OR. (RULE .EQ. 2) .OR. (RULE .EQ. 3) .OR. (RULE .EQ. 5)) THEN
                WRITE(40,*) 'Volume_1000cm WaterLevel_m Qinflow_cms '
     &          //'Qspilways_cms Qturbine_cms Energy_MW'
                DO K = 1, DAYS
                    IF (FLOWOUT(I,K)>QRESER) THEN
                        QSPILL = FLOWOUT(I,K) - QRESER
                        QTUR = QRESER
                    ELSE
                        QSPILL = 0
                        QTUR = FLOWOUT(I,K)
                    END IF
                    IF (VOL(I,K)<0) THEN
                        VOL(I,K) = 0
                    END IF
                    if (HHO(I,K)<0) then
                        HHO(I,K) = 0
                    end if
                    if (FLOWIN(I,K)<0) then
                        FLOWIN(I,K) = 0
                    end if
                    if (HYDROPOWER(I,K)<0) then
                        HYDROPOWER(I,K) = 0
                    end if
                    WRITE(40,*) VOL(I,K), HHO(I,K), FLOWIN(I,K), QSPILL, QTUR, HYDROPOWER(I,K)
                END DO
            ELSE
                WRITE(40,*) 'Volume_1000cm WaterLevel_m Qinflow_cms '
     &           //'Qoutflow_cms Energy_MW'
                DO K = 1, DAYS
                    IF (VOL(I,K)<0) THEN
                        VOL(I,K) = 0
                    END IF
                    if (HHO(I,K)<0) then
                        HHO(I,K) = 0
                    end if
                    if (FLOWIN(I,K)<0) then
                        FLOWIN(I,K) = 0
                    end if
                    if (HYDROPOWER(I,K)<0) then
                        HYDROPOWER(I,K) = 0
                    end if
                    WRITE(40,*) VOL(I,K), HHO(I,K), FLOWIN(I,K),
     &               FLOWOUT(I,K), HYDROPOWER(I,K)
                END DO
            END IF
            CLOSE(40)
      END DO
      RETURN
      END

C************************************************************************************************************************************************************************************
C     WRITE MONTHLY/YEAR DATA AT THE BASIN OUTLET
C************************************************************************************************************************************************************************************
      SUBROUTINE WRITE_MONTH
     & (DAYS_IN_MONTH,START_YEAR, STOP_YEAR, FIRST_YEAR, LAST_YEAR,
     &  START_MO, STOP_MO, FIRST_MO, LAST_MO,
     &  NAME5, DAYS, FLOW, FACTOR_SUM, MONTHLY, MONTHLY_mm,
     &  YEARLY, YEARLY_mm,OUTPATH,NDAY,IMONTH,IYEAR,MO,YR,NMONTHS,NYR)
      IMPLICIT NONE
      INTEGER DAYS_IN_MONTH(12)
      INTEGER NYR
      INTEGER START_YEAR, STOP_YEAR, FIRST_YEAR, LAST_YEAR
      INTEGER START_MO, STOP_MO, FIRST_MO, LAST_MO   !AWW-092700
      INTEGER DAYS,NDAY,NMONTHS
      INTEGER IMONTH(DAYS),IYEAR(DAYS)
      INTEGER SKIPTO, STOPAT
      INTEGER OLDMO
      INTEGER I, MONTH, YEAR, DAY_IN_MONTH
      INTEGER M, MCOUNT(12)     !AWW-092700
      INTEGER MO(12*NYR),YR(12*NYR)
c     INTEGER MNTH_INDX
      REAL    FLOW(DAYS)
      REAL    FACTOR_SUM
      REAL    MONTHLY(12*(STOP_YEAR-START_YEAR+1))
      REAL    MONTHLY_mm(12*(STOP_YEAR-START_YEAR+1))
      REAL    YEARLY(12)
      REAL    YEARLY_mm(12)
      CHARACTER*5  NAME5
      CHARACTER*72 OUTPATH, TMPPTH
c     concatenate output string
      I=INDEX(OUTPATH,' ')-1
C      OUTPATH(I:I+4)=NAME5
C      I=I+4
      OPEN(40, FILE = OUTPATH(1:I)//NAME5//'.month')
      OPEN(41, FILE = OUTPATH(1:I)//NAME5//'.month_mm')
      OPEN(42, FILE = OUTPATH(1:I)//NAME5//'.year')
      OPEN(43, FILE = OUTPATH(1:I)//NAME5//'.year_mm')
      OPEN(77, FILE = OUTPATH(1:I)//NAME5//'.end_of_month')
c     iniitalize monthly averages
      DO I = 1, 12*(STOP_YEAR-START_YEAR+1)
         MONTHLY(I) = 0.0
         MONTHLY_mm(I) = 0.0
      END DO
c     Average flows for each month in simulation
      M=1
      OLDMO=MO(1)
      DO I = 1, NDAY
         IF(IMONTH(I).ne.OLDMO) THEN
            M=M+1
            OLDMO=IMONTH(I)
         ENDIF
         MONTHLY(M)=MONTHLY(M)+FLOW(I)/DAYS_IN_MONTH(IMONTH(I))
         MONTHLY_mm(M) = MONTHLY_mm(M) + FLOW(I)/FACTOR_SUM
      END DO
C     writing monthly averages
      DO I = 1,12
         YEARLY(I) = 0.0
         YEARLY_mm(I) = 0.0
         MCOUNT(I) = 0
      END DO
c     Find months in time series to start and stop writing data
c     Note array starts at 1 regardless of actual month number
      SKIPTO = (FIRST_YEAR-START_YEAR)*12+(FIRST_MO-START_MO)+1
      STOPAT = NMONTHS-((STOP_YEAR-LAST_YEAR)*12+(STOP_MO-LAST_MO))
      DO I=SKIPTO,STOPAT
         WRITE(40,*) YR(I),MO(I), MONTHLY(I)
         WRITE(41,*) YR(I),MO(I), MONTHLY_mm(I)
         YEARLY(MOD(I-1,12)+1) = YEARLY(MOD(I-1,12)+1) + MONTHLY(I)
         YEARLY_mm(MOD(I-1,12)+1) =
     &      YEARLY_mm(MOD(I-1,12)+1) + MONTHLY_mm(I)
         MCOUNT(MO(I)) = MCOUNT(MO(I))+1
      END DO
      DO I = 1, 12
         IF(MCOUNT(I) .GT. 0) THEN
           WRITE(42,*) I, YEARLY(I)/MCOUNT(I)
           WRITE(43,*) I, YEARLY_mm(I)/MCOUNT(I)
         ELSE
           WRITE(42,*) I, '  0'
           WRITE(43,*) I, '  0'
         END IF
      END DO
      CLOSE(40)
      CLOSE(41)
      CLOSE(42)
      CLOSE(43)
      CLOSE(77)
      RETURN
      END
C     END OF FILE
C************************************************************************************************************************************************************************************