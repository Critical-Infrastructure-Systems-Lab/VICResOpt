c     18/07/2023 correct an error related to NORESERVOIRS when running routing with multiple outlet (Line 98) - Thank Bruno Invernizzi (Politechnico di Milano)
c     SUBROUTINES FOR INITIALIZATION (roughly)

C************************************************************************************************************************************************************************************
C     ARRAY INITIALIZATION
C************************************************************************************************************************************************************************************

      SUBROUTINE INIT_ARRAY( A, NROW, NCOL, VALUE )
C     Initialiase float array A to VALUE
      IMPLICIT NONE
c     RCS ID STRING
      CHARACTER*50 RCSID
c     DATA RCSID/"$Id: init_routines.f,v 1.1 2005/04/07 05:07:28 vicadmin Exp $"/
      INTEGER NCOL, NROW
      INTEGER I, J
      REAL A(NCOL,NROW)
      REAL VALUE
      DO J=1, NROW
         DO I=1, NCOL
            A(I,J)=VALUE
         END DO
      END DO
      RETURN
      END

C************************************************************************************************************************************************************************************
C     CREATE VIC NAMES
C************************************************************************************************************************************************************************************

      SUBROUTINE CREATE_VIC_NAMES( JLOC, ILOC, EXTEN, CLEN, DPREC )
c     create string containing vic file names to be
c     appended to path given in input file
c     filenames allowed a maximum of 5 decimal places
      IMPLICIT NONE
      CHARACTER*10 JICHAR(2)
      CHARACTER*20 EXTEN
      REAL JLOC, ILOC
      INTEGER NSPACE, CLEN, CLEN_OLD, DPREC, I
      WRITE(JICHAR(1),'(F10.5)')JLOC
      WRITE(JICHAR(2),'(F10.5)')ILOC
      CLEN_OLD=1
      DO I=1,2
         NSPACE=1
 5       IF(JICHAR(I)(NSPACE:NSPACE).EQ.' ')THEN
            NSPACE=NSPACE+1
            GOTO 5
         ENDIF
         CLEN=CLEN_OLD+11-NSPACE-5+DPREC
         EXTEN(CLEN_OLD:CLEN)=JICHAR(I)(NSPACE:5+DPREC)
         IF(I.EQ.1)THEN
            EXTEN(CLEN:CLEN)='_'
         ENDIF
         CLEN_OLD=CLEN+1
      END DO
      CLEN=CLEN-1
      RETURN
      END

C************************************************************************************************************************************************************************************
C     READ RESERVOIR LOCATION - THIS SUBROUTINE READS THE RESERVOIR MATRIX
C************************************************************************************************************************************************************************************

      SUBROUTINE READ_RESE(RESER,ICOL,IROW,NCOL,NROW,RFILENAME)
        IMPLICIT NONE
        INTEGER IROW, ICOL
        INTEGER I,J
        CHARACTER*72 RFILENAME
        INTEGER NROW, NCOL
        INTEGER RESER(NCOL,NROW)
        OPEN(11, FILE = RFILENAME,FORM = 'FORMATTED',
     $     STATUS='OLD',ERR=9001)
        DO J = IROW,1,-1
         READ(11,*) (RESER(I,J), I=ICOL,1,-1)
        END DO
        CLOSE(11)
      RETURN
9001  WRITE(*,*) 'CANNOT OPEN RESERVOIR LOCATION FILE'
      STOP
      END

C************************************************************************************************************************************************************************************
C     LIMIT CALCULATION BOUNDARY - THIS SUBROUTING FILTERS CELLS WHICH DO NOT CONTRIBUTE TO THE FLOW AT THE BASIN OUTLET
C************************************************************************************************************************************************************************************

      SUBROUTINE SEARCH_WHOLECATCHMENT
     & (PI,PJ,DIREC,NCOL,NROW,NO_OF_BOX,CATCHIJ,PMAX,
     $  IROW,ICOL,NORESERVOIRS,RES_DIRECT,RESER)
      IMPLICIT NONE
      INTEGER PI,PJ,I,J,NCOL,NROW,PMAX,ICOL,IROW
      INTEGER II,JJ,III,JJJ,K
      INTEGER DIREC(NCOL,NROW,2)
      INTEGER NO_OF_BOX(200)
      INTEGER CATCHIJ(PMAX,2,200)
      INTEGER NORESERVOIRS
      INTEGER RES_DIRECT(200,3)
      INTEGER RESER(NCOL,NROW)
      INTEGER CELL_OF_RES
      NORESERVOIRS = 0
      NORESERVOIRS = NORESERVOIRS + 1
      RES_DIRECT(NORESERVOIRS,1) = NORESERVOIRS
      NO_OF_BOX(NORESERVOIRS) = 0
      CELL_OF_RES = 0
      DO I = 1, ICOL
        DO J = 1, IROW
            II = I
            JJ = J
 300        CONTINUE
            IF ((II .GT. ICOL) .OR. (II .LT.1) .OR.
     &          (JJ .GT. IROW) .OR. (JJ .LT.1)) THEN
                GOTO 310
            END IF
            IF ((II .EQ. PI) .AND. (JJ .EQ. PJ)) THEN
               NO_OF_BOX(NORESERVOIRS) = NO_OF_BOX(NORESERVOIRS) + 1
               CATCHIJ(NO_OF_BOX,1,NORESERVOIRS) = I
               CATCHIJ(NO_OF_BOX,2,NORESERVOIRS) = J
               GOTO 310
            ELSE
               IF ((DIREC(II,JJ,1).NE.0) .AND.
     &             (DIREC(II,JJ,2) .NE.0)) THEN
                     III = DIREC(II,JJ,1)
                     JJJ = DIREC(II,JJ,2)
                     II  = III
                     JJ  = JJJ
                     GOTO 300
               END IF
            END IF
 310        CONTINUE
            IF ((RESER(I,J)>0) .AND. (II .EQ. PI) .AND. (JJ .EQ. PJ) 
     &        .AND. (RESER(I,J) .NE. 9999)) THEN
                RES_DIRECT(NORESERVOIRS,1) = RESER(I,J)
                NORESERVOIRS = NORESERVOIRS + 1
                CELL_OF_RES = 0 
                RES_DIRECT(NORESERVOIRS,1) = NORESERVOIRS
                NO_OF_BOX(NORESERVOIRS) = NO_OF_BOX(NORESERVOIRS-1)
                NO_OF_BOX(NORESERVOIRS-1) = 0
                DO K = 1, NO_OF_BOX(NORESERVOIRS)
                    CATCHIJ(K,1,NORESERVOIRS) = CATCHIJ(K,1,NORESERVOIRS-1)
                    CATCHIJ(K,2,NORESERVOIRS) = CATCHIJ(K,2,NORESERVOIRS-1)
                    CATCHIJ(K,1,NORESERVOIRS-1) = 0
                    CATCHIJ(K,2,NORESERVOIRS-1) = 0
                END DO
            END IF
         END DO
      END DO
      WRITE(*,*) 'Number of grid cells',
     $     no_of_box(NORESERVOIRS)
      RETURN
      END
C     END OF FILE
C************************************************************************************************************************************************************************************
