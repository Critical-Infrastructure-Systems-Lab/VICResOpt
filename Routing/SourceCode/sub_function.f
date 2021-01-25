C************************************************************************************************************************************************************************************
C       Convert from day to month
C************************************************************************************************************************************************************************************
        INTEGER FUNCTION CAL_MONTH(CRTDATE)
        INTEGER CRTDATE
        IF (CRTDATE .LE. 31) THEN !jan
            CAL_MONTH = 1
        ELSE IF (CRTDATE .LE. 59) THEN ! feb
            CAL_MONTH = 2
        ELSE IF (CRTDATE .LE. 90) THEN ! mar
            CAL_MONTH = 3
        ELSE IF (CRTDATE .LE. 120) THEN ! apr
            CAL_MONTH = 4
        ELSE IF (CRTDATE .LE. 151) THEN ! may
            CAL_MONTH = 5
        ELSE IF (CRTDATE .LE. 181) THEN ! jun
            CAL_MONTH = 6
        ELSE IF (CRTDATE .LE. 212) THEN ! jul
            CAL_MONTH = 7
        ELSE IF (CRTDATE .LE. 243) THEN ! aug
            CAL_MONTH = 8
        ELSE IF (CRTDATE .LE. 273) THEN ! sep
            CAL_MONTH = 9
        ELSE IF (CRTDATE .LE. 304) THEN ! oct
            CAL_MONTH = 10
        ELSE IF (CRTDATE .LE. 334) THEN ! nov
            CAL_MONTH = 11
        ELSE ! dec
            CAL_MONTH = 12
        END IF
        RETURN
        END