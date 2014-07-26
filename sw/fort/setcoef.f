C     path:      $Source: /storm/rc1/cvsroot/rc/rrtm_sw/src/setcoef.f,v 
C     author:    $Author: jdelamer $
C     revision:  $Revision: 2.6 $
C     created:   $Date: 2004/04/15 18:50:57 $
      SUBROUTINE SETCOEF
C
C  ---------------------------------------------------------------------
C |                                                                     
C |  Copyright 2002, 2003, Atmospheric & Environmental Research, Inc. (A
C |  This software may be used, copied, or redistributed as long as it i
C |  not sold and this copyright notice is reproduced on each copy made.
C |  This model is provided as is without any express or implied warrant
C |                       (http://www.rtweb.aer.com/)                   
C |                                                                     
C  ---------------------------------------------------------------------

C     Purpose:  For a given atmosphere, calculate the indices and
C     fractions related to the pressure and temperature interpolations.

      PARAMETER (MXMOL=38)
      PARAMETER (MXLAY = 203)
      PARAMETER (MG =16)

C  Input      
c      COMMON /CONTROL/  NUMANGS, IOUT, ISTART, IEND, ICLD
      COMMON /CONTROL/  IAER, NSTR, IOUT, ISTART, IEND, ICLD,
     &                  idelm, isccos
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                  PZ(0:MXLAY),TZ(0:MXLAY),TBOUND
      COMMON /SPECIES/  COLDRY(MXLAY),WKL(MXMOL,MXLAY),WBROAD(MXLAY),
     &                  COLMOL(MXLAY),NMOL

C  Output
      COMMON /PROFDATA/ LAYTROP,LAYSWTCH,LAYLOW,COLH2O(MXLAY),
     &                  COLCO2(MXLAY),COLO3(MXLAY),COLN2O(MXLAY),
     &                  COLCH4(MXLAY),COLO2(MXLAY),CO2MULT(MXLAY)
      COMMON /INTFAC/   FAC00(MXLAY),FAC01(MXLAY),
     &                  FAC10(MXLAY),FAC11(MXLAY)
      COMMON /INTIND/   JP(MXLAY),JT(MXLAY),JT1(MXLAY)
      COMMON /SELF/     SELFFAC, SELFFRAC, INDSELF
      COMMON /FOREIGN/  FORFAC, FORFRAC, INDFOR

C  --------

      COMMON /CVRSET/   HNAMSET,HVRSET

      CHARACTER*18 HNAMSET,HVRSET

      DIMENSION SELFFAC(MXLAY),SELFFRAC(MXLAY),INDSELF(MXLAY)
      DIMENSION FORFAC(MXLAY), FORFRAC(MXLAY), INDFOR(MXLAY)
      DIMENSION PREF(59),PREFLOG(59),TREF(59)

C     These pressures are chosen such that the ln of the first pressure
C     has only a few non-zero digits (i.e. ln(PREF(1)) = 6.96000) and
C     each subsequent ln(pressure) differs from the previous one by 0.2.
      DATA PREF /
     &    1.05363E+03,8.62642E+02,7.06272E+02,5.78246E+02,4.73428E+02,
     &    3.87610E+02,3.17348E+02,2.59823E+02,2.12725E+02,1.74164E+02,
     &    1.42594E+02,1.16746E+02,9.55835E+01,7.82571E+01,6.40715E+01,
     &    5.24573E+01,4.29484E+01,3.51632E+01,2.87892E+01,2.35706E+01,
     &    1.92980E+01,1.57998E+01,1.29358E+01,1.05910E+01,8.67114E+00,
     &    7.09933E+00,5.81244E+00,4.75882E+00,3.89619E+00,3.18993E+00,
     &    2.61170E+00,2.13828E+00,1.75067E+00,1.43333E+00,1.17351E+00,
     &    9.60789E-01,7.86628E-01,6.44036E-01,5.27292E-01,4.31710E-01,
     &    3.53455E-01,2.89384E-01,2.36928E-01,1.93980E-01,1.58817E-01,
     &    1.30029E-01,1.06458E-01,8.71608E-02,7.13612E-02,5.84256E-02,
     &    4.78349E-02,3.91639E-02,3.20647E-02,2.62523E-02,2.14936E-02,
     &    1.75975E-02,1.44076E-02,1.17959E-02,9.65769E-03/
      DATA PREFLOG /
     &     6.9600E+00, 6.7600E+00, 6.5600E+00, 6.3600E+00, 6.1600E+00,
     &     5.9600E+00, 5.7600E+00, 5.5600E+00, 5.3600E+00, 5.1600E+00,
     &     4.9600E+00, 4.7600E+00, 4.5600E+00, 4.3600E+00, 4.1600E+00,
     &     3.9600E+00, 3.7600E+00, 3.5600E+00, 3.3600E+00, 3.1600E+00,
     &     2.9600E+00, 2.7600E+00, 2.5600E+00, 2.3600E+00, 2.1600E+00,
     &     1.9600E+00, 1.7600E+00, 1.5600E+00, 1.3600E+00, 1.1600E+00,
     &     9.6000E-01, 7.6000E-01, 5.6000E-01, 3.6000E-01, 1.6000E-01,
     &    -4.0000E-02,-2.4000E-01,-4.4000E-01,-6.4000E-01,-8.4000E-01,
     &    -1.0400E+00,-1.2400E+00,-1.4400E+00,-1.6400E+00,-1.8400E+00,
     &    -2.0400E+00,-2.2400E+00,-2.4400E+00,-2.6400E+00,-2.8400E+00,
     &    -3.0400E+00,-3.2400E+00,-3.4400E+00,-3.6400E+00,-3.8400E+00,
     &    -4.0400E+00,-4.2400E+00,-4.4400E+00,-4.6400E+00/
C     These are the temperatures associated with the respective 
C     pressures for the MLS standard atmosphere. 
      DATA TREF /
     &     2.9420E+02, 2.8799E+02, 2.7894E+02, 2.6925E+02, 2.5983E+02,
     &     2.5017E+02, 2.4077E+02, 2.3179E+02, 2.2306E+02, 2.1578E+02,
     &     2.1570E+02, 2.1570E+02, 2.1570E+02, 2.1706E+02, 2.1858E+02,
     &     2.2018E+02, 2.2174E+02, 2.2328E+02, 2.2479E+02, 2.2655E+02,
     &     2.2834E+02, 2.3113E+02, 2.3401E+02, 2.3703E+02, 2.4022E+02,
     &     2.4371E+02, 2.4726E+02, 2.5085E+02, 2.5457E+02, 2.5832E+02,
     &     2.6216E+02, 2.6606E+02, 2.6999E+02, 2.7340E+02, 2.7536E+02,
     &     2.7568E+02, 2.7372E+02, 2.7163E+02, 2.6955E+02, 2.6593E+02,
     &     2.6211E+02, 2.5828E+02, 2.5360E+02, 2.4854E+02, 2.4348E+02,
     &     2.3809E+02, 2.3206E+02, 2.2603E+02, 2.2000E+02, 2.1435E+02,
     &     2.0887E+02, 2.0340E+02, 1.9792E+02, 1.9290E+02, 1.8809E+02,
     &     1.8329E+02, 1.7849E+02, 1.7394E+02, 1.7212E+02/


C ****************** START OF EXECUTABLE CODE **************************
      HVRSET = '$Revision: 2.6 $'

      STPFAC = 296./1013.

      INDBOUND = TBOUND - 159.
      TBNDFRAC = TBOUND - INT(TBOUND)
      INDLEV0 = TZ(0) - 159.
      T0FRAC = TZ(0) - INT(TZ(0))

      LAYTROP = 0
      LAYSWTCH = 0
      LAYLOW = 0
      DO 7000 LAY = 1, NLAYERS
C        Find the two reference pressures on either side of the
C        layer pressure.  Store them in JP and JP1.  Store in FP the
C        fraction of the difference (in ln(pressure)) between these
C        two values that the layer pressure lies.
         PLOG = ALOG(PAVEL(LAY))
         JP(LAY) = INT(36. - 5*(PLOG+0.04))
         IF (JP(LAY) .LT. 1) THEN
            JP(LAY) = 1
         ELSEIF (JP(LAY) .GT. 58) THEN
            JP(LAY) = 58
         ENDIF
         JP1 = JP(LAY) + 1
         FP = 5. * (PREFLOG(JP(LAY)) - PLOG)

C        Determine, for each reference pressure (JP and JP1), which
C        reference temperature (these are different for each  
C        reference pressure) is nearest the layer temperature but does
C        not exceed it.  Store these indices in JT and JT1, resp.
C        Store in FT (resp. FT1) the fraction of the way between JT
C        (JT1) and the next highest reference temperature that the 
C        layer temperature falls.
         JT(LAY) = INT(3. + (TAVEL(LAY)-TREF(JP(LAY)))/15.)
         IF (JT(LAY) .LT. 1) THEN
            JT(LAY) = 1
         ELSEIF (JT(LAY) .GT. 4) THEN
            JT(LAY) = 4
         ENDIF
         FT = ((TAVEL(LAY)-TREF(JP(LAY)))/15.) - FLOAT(JT(LAY)-3)
         JT1(LAY) = INT(3. + (TAVEL(LAY)-TREF(JP1))/15.)
         IF (JT1(LAY) .LT. 1) THEN
            JT1(LAY) = 1
         ELSEIF (JT1(LAY) .GT. 4) THEN
            JT1(LAY) = 4
         ENDIF
         FT1 = ((TAVEL(LAY)-TREF(JP1))/15.) - FLOAT(JT1(LAY)-3)

         WATER = WKL(1,LAY)/COLDRY(LAY)
         SCALEFAC = PAVEL(LAY) * STPFAC / TAVEL(LAY)

C        If the pressure is less than ~100mb, perform a different
C        set of species interpolations.
         IF (PLOG .LE. 4.56) GO TO 5300
         LAYTROP =  LAYTROP + 1
         IF (PLOG .GE. 6.62) LAYLOW = LAYLOW + 1

C        Set up factors needed to separately include the water vapor
C        foreign-continuum in the calculation of absorption coefficient.
         FORFAC(LAY) = SCALEFAC / (1.+WATER)
         FACTOR = (332.0-TAVEL(LAY))/36.0
         INDFOR(LAY) = MIN(2, MAX(1, INT(FACTOR)))
         FORFRAC(LAY) = FACTOR - FLOAT(INDFOR(LAY))
C
C        Set up factors needed to separately include the water vapor
C        self-continuum in the calculation of absorption coefficient.
         SELFFAC(LAY) = WATER * FORFAC(LAY)
         FACTOR = (TAVEL(LAY)-188.0)/7.2
         INDSELF(LAY) = MIN(9, MAX(1, INT(FACTOR)-7))
         SELFFRAC(LAY) = FACTOR - FLOAT(INDSELF(LAY) + 7)

C        Calculate needed column amounts.
         COLH2O(LAY) = 1.E-20 * WKL(1,LAY)
         COLCO2(LAY) = 1.E-20 * WKL(2,LAY)
         COLO3(LAY) = 1.E-20 * WKL(3,LAY)
c         COLO3(LAY) = 0.
C         COLO3(LAY) = colo3(lay)/1.16
         COLN2O(LAY) = 1.E-20 * WKL(4,LAY)
         COLCH4(LAY) = 1.E-20 * WKL(6,LAY)
         COLO2(LAY) = 1.E-20 * WKL(7,LAY)
         COLMOL(LAY) = 1.E-20 * COLDRY(LAY) + COLH2O(LAY)
c         colco2(lay) = 0.
c         colo3(lay) = 0.
c         coln2o(lay) = 0.
c         colch4(lay) = 0.
c         colo2(lay) = 0.
c         colmol(lay) = 0.
         IF (COLCO2(LAY) .EQ. 0.) COLCO2(LAY) = 1.E-32 * COLDRY(LAY)
         IF (COLN2O(LAY) .EQ. 0.) COLN2O(LAY) = 1.E-32 * COLDRY(LAY)
         IF (COLCH4(LAY) .EQ. 0.) COLCH4(LAY) = 1.E-32 * COLDRY(LAY)
         IF (COLO2(LAY) .EQ. 0.) COLO2(LAY) = 1.E-32 * COLDRY(LAY)
C        Using E = 1334.2 cm-1.
         CO2REG = 3.55E-24 * COLDRY(LAY)
         CO2MULT(LAY)= (COLCO2(LAY) - CO2REG) *
     &        272.63*EXP(-1919.4/TAVEL(LAY))/(8.7604E-4*TAVEL(LAY))
         GO TO 5400

C        Above LAYTROP.
 5300    CONTINUE

C        Set up factors needed to separately include the water vapor
C        foreign-continuum in the calculation of absorption coefficient.
         FORFAC(LAY) = SCALEFAC / (1.+WATER)
         FACTOR = (TAVEL(LAY)-188.0)/36.0
         INDFOR(LAY) = 3
         FORFRAC(LAY) = FACTOR - 1.0
C
C        Calculate needed column amounts.
         COLH2O(LAY) = 1.E-20 * WKL(1,LAY)
         COLCO2(LAY) = 1.E-20 * WKL(2,LAY)
         COLO3(LAY) = 1.E-20 * WKL(3,LAY)
         COLN2O(LAY) = 1.E-20 * WKL(4,LAY)
         COLCH4(LAY) = 1.E-20 * WKL(6,LAY)
         COLO2(LAY) = 1.E-20 * WKL(7,LAY)
         COLMOL(LAY) = 1.E-20 * COLDRY(LAY) + COLH2O(LAY)
         IF (COLCO2(LAY) .EQ. 0.) COLCO2(LAY) = 1.E-32 * COLDRY(LAY)
         IF (COLN2O(LAY) .EQ. 0.) COLN2O(LAY) = 1.E-32 * COLDRY(LAY)
         IF (COLCH4(LAY) .EQ. 0.) COLCH4(LAY) = 1.E-32 * COLDRY(LAY)
         IF (COLO2(LAY) .EQ. 0.) COLO2(LAY) = 1.E-32 * COLDRY(LAY)
         CO2REG = 3.55E-24 * COLDRY(LAY)
         CO2MULT(LAY)= (COLCO2(LAY) - CO2REG) *
     &        272.63*EXP(-1919.4/TAVEL(LAY))/(8.7604E-4*TAVEL(LAY))

 5400    CONTINUE

C        We have now isolated the layer ln pressure and temperature,
C        between two reference pressures and two reference temperatures 
C        (for each reference pressure).  We multiply the pressure 
C        fraction FP with the appropriate temperature fractions to get 
C        the factors that will be needed for the interpolation that yiel
C        the optical depths (performed in routines TAUGBn for band n).

         COMPFP = 1. - FP
         FAC10(LAY) = COMPFP * FT
         FAC00(LAY) = COMPFP * (1. - FT)
         FAC11(LAY) = FP * FT1
         FAC01(LAY) = FP * (1. - FT1)

 7000 CONTINUE
      RETURN
      END

