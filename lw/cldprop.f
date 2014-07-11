C     path:      $Source: /storm/rc1/cvsroot/rc/rrtm_lw/src/cldprop.f,v $
C     author:    $Author: jdelamer $
C     revision:  $Revision: 3.3 $
C     created:   $Date: 2010/07/07 21:10:51 $
C
C  --------------------------------------------------------------------------
C |                                                                          |
C |  Copyright 2002, 2003, Atmospheric & Environmental Research, Inc. (AER). |
C |  This software may be used, copied, or redistributed as long as it is    |
C |  not sold and this copyright notice is reproduced on each copy made.     |
C |  This model is provided as is without any express or implied warranties. |
C |                       (http://www.rtweb.aer.com/)                        |
C |                                                                          |
C  --------------------------------------------------------------------------

      Subroutine CLDPROP

C     Purpose:  Compute the cloud optical depth(s) for each cloudy
C               layer.

      PARAMETER (MXLAY=603)
      PARAMETER (NBANDS = 16)

      COMMON /CONTROL/  NUMANGS, ISCAT, NSTR, 
     &                  IOUT, ISTART, IEND, ICLD
      COMMON /IFIL/     IRD,IPR,IPU,IDUM(15)
      COMMON /BANDS/     WAVENUM1(NBANDS),WAVENUM2(NBANDS),
     &                   DELWAVE(NBANDS)
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                  PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /CLOUDIN/   ICD,ICLDATM,INFLAG,
     &     CLDDAT1(MXLAY),CLDDAT2(MXLAY),
     &     ICEFLAG,LIQFLAG,CLDDAT3(MXLAY),CLDDAT4(MXLAY)
      COMMON /CLOUDDAT/ NCBANDS,CLDFRAC(MXLAY),
     &     TAUCLOUD(MXLAY,NBANDS), 
     &     SSACLOUD(MXLAY,NBANDS),
     &     XMOM(0:16,MXLAY,NBANDS),
     &     TAUTOT(NBANDS)

      COMMON / CLDOPTPROPS /ABSCLD1, ABSLIQ0,ABSICE0(2), 
     &     ABSICE1(2,5), ABSICE2(43,16), ABSICE3(46,16),
     &     ABSLIQ1(58,16),
     &     EXTICE2(43,16),SSAICE2(43,16),ASYICE2(43,16),
     &     EXTICE3(46,16),SSAICE3(46,16),ASYICE3(46,16)

      COMMON /CVRCLD/    HNAMCLD,HVRCLD

      CHARACTER*18       HNAMCLD,HVRCLD

      DIMENSION ABSCOICE(NBANDS), ABSCOLIQ(NBANDS)
      DIMENSION EXTCOLIQ(NBANDS),SSACOLIQ(NBANDS),GLIQ(NBANDS)
      DIMENSION EXTCOICE(NBANDS),SSACOICE(NBANDS),GICE(NBANDS)
      DIMENSION FORWLIQ(NBANDS),FORWICE(NBANDS)

      DIMENSION IPAT(16,0:2)

      DATA EPS /1.E-6/

      DATA IPAT /1,1,1,1,1,1,1,1,1, 1, 1, 1, 1, 1, 1, 1,
     &           1,2,3,3,3,4,4,4,5, 5, 5, 5, 5, 5, 5, 5,
     &           1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/

C     Explanation of the method for each value of INFLAG.  Values of
C     0 or 1 for INFLAG do not distingish being liquid and ice clouds.
C     INFLAG = 2 does distinguish between liquid and ice clouds, and
C     requires further user input to specify the method to be used to 
C     compute the aborption due to each
C     INFLAG = 0:  For each cloudy layer, the cloud fraction and (gray)
C                  cloud optical depth (and if ISCAT=2) single-scattering albedo,
C                  and phase-function moments are input.	 
C     INFLAG = 1:  For each cloudy layer, the cloud fraction and cloud
C                  water path (g/m2) are input.  The (gray) cloud optical 
C                  depth is computed as in CCM2.
C     INFLAG = 2:  For each cloudy layer, the cloud fraction, cloud 
C                  water path (g/m2), and cloud ice fraction are input.
C       ICEFLAG = 0:  The ice effective radius (microns) is input and the
C                     optical depths due to ice clouds are computed as in CCM3.
C       ICEFLAG = 1:  The ice effective radius (microns) is input and the
C                     optical depths due to ice clouds are computed as in 
C                     Ebert and Curry, JGR, 97, 3831-3836 (1992).  The 
C                     spectral regions in this work have been matched with
C                     the spectral bands in RRTM to as great an extent 
C                     as possible:  
C                     E&C 1      IB = 5      RRTM bands 9-16
C                     E&C 2      IB = 4      RRTM bands 6-8
C                     E&C 3      IB = 3      RRTM bands 3-5
C                     E&C 4      IB = 2      RRTM band 2
C                     E&C 5      IB = 1      RRTM band 1
C       ICEFLAG = 2:  The ice effective radius (microns) is input and the
C                     optical properties due to ice clouds are computed from
C                     the optical properties stored in the RT code,
C                     STREAMER v3.0 (Reference: Key. J., Streamer 
C                     User's Guide, Cooperative Institute for
C                     Meteorological Satellite Studies, 2001, 96 pp.).
C                     Valid range of values for re are between 5.0 and
C                     131.0 micron.
C       ICEFLAG = 3: The ice generalized effective size (dge) is input
C                    and the optical properties, are calculated as in
C                    Q. Fu, J. Climate, (1998). Q. Fu provided high resolution
C                    tables which were appropriately averaged for the
C                    bands in RRTM_LW.  Linear interpolation is used to
C                    get the coefficients from the stored tables.
C                    Valid range of values for dge are between 5.0 and
C                    140.0 micron.
C       LIQFLAG = 0:  The optical depths due to water clouds are computed as
C                     in CCM3.
C       LIQFLAG = 1:  The water droplet effective radius (microns) is input 
C                     and the optical depths due to water clouds are computed 
C                     as in Hu and Stamnes, J., Clim., 6, 728-742, (1993).
C                     The values for absorption coefficients appropriate for
C                     the spectral bands in RRTM have been obtained for a 
C                     range of effective radii by an averaging procedure 
C                     based on the work of J. Pinto (private communication).
C                     Linear interpolation is used to get the absorption 
C                     coefficients for the input effective radius.
C     INFLAG = 10:  For each cloudy layer, the cloud fraction and 
C                   cloud optical depth (and if ISCAT=2) single-scattering albedo,
C                   and phase-function moments are input for each band.	 


      HVRCLD = '$Revision: 3.3 $'

      ICLDATM = 0
      NCBANDS = 1

      IF (ISCAT .EQ. 0 .OR. ISCAT .EQ. 1) THEN
         
         IF (INFLAG .EQ. 1) THEN 
            WRITE(ICD,7998) '  LAY','  BAND  ',
     &           '  WVN1  ','  WVN2  ',
     &           '   ABS OD    ','SUM(ABS OD)  '
            WRITE(ICD,7999) ' BY BLOCK    '
         ELSEIF (INFLAG .EQ. 2) THEN
            WRITE(ICD,8900) '  LAY','  BAND  ',
     &           '  WVN1  ','  WVN2  ',
     &           '   ICE OD    ','   LIQ OD    ',
     &           '  TOT ABS OD ','  TOT ABS OD '

            WRITE(ICD,8901) '  BY BLOCK  '
         ENDIF

         NPRELAY = 0
         DO 3000 LAY = 1, NLAYERS
            IF (CLDFRAC(LAY) .GE. EPS) THEN
               IF (CLDFRAC(LAY) .NE. 1.0 .AND. ISCAT .GE. 1) 
     &              STOP 'CLDFRAC MUST BE 1 WHEN USING DISORT'
               ICLDATM = 1

C           Ice clouds and water clouds combined.
               IF(INFLAG .EQ. 1) THEN
                  CWP = CLDDAT1(LAY)
		  NCBANDS = 16
                  DO 1000 IB = 1, NCBANDS                  
                     TAUCLOUD(LAY,IB) = ABSCLD1 * CWP
                     TAUTOT(IB) = TAUTOT(IB) + 
     &                    TAUCLOUD(LAY,IB)                     
                     WRITE(ICD,9002) LAY,IB,WAVENUM1(IB),WAVENUM2(IB),
     &                    TAUCLOUD(LAY,IB),
     &                    TAUTOT(IB)
 1000             CONTINUE
                  
C           Separate treatement of ice clouds and water clouds.
               ELSEIF(INFLAG .EQ. 2) THEN
                  CWP = CLDDAT1(LAY)
                  FICE = CLDDAT2(LAY)
                  RADICE = CLDDAT3(LAY)
                  
C              Calculation of absorption coefficients due to ice clouds.
                  IF (FICE .EQ. 0.0) THEN
                     ABSCOICE(1) = 0.0
                     ICEPAT = 0
                  ELSEIF (ICEFLAG .EQ. 0) THEN
                     IF (RADICE .LT. 10.0) STOP 'ICE RADIUS TOO SMALL'
                     ABSCOICE(1) = FICE * (ABSICE0(1) + 
     &                    ABSICE0(2)/RADICE)
                     ICEPAT = 0
                  ELSEIF (ICEFLAG .EQ. 1) THEN
                     IF (RADICE .LT. 13.0 .OR. RADICE .GT. 130.) STOP
     &                    'ICE RADIUS OUT OF BOUNDS'
                     NCBANDS = 5
                     DO 2000 IB = 1, NCBANDS
                        ABSCOICE(IB) = FICE * (ABSICE1(1,IB) + 
     &                       ABSICE1(2,IB)/RADICE)
 2000                CONTINUE
                     ICEPAT = 1
                  ELSEIF (ICEFLAG .EQ. 2) THEN
                     IF (RADICE .LT. 5.0 .OR. RADICE .GT. 131.) STOP
     &                    'ICE RADIUS OUT OF BOUNDS'
                     NCBANDS = 16
                     FACTOR = (RADICE - 2.)/3.
                     INDEX = INT(FACTOR)
                     IF (INDEX .EQ. 43) INDEX = 42
                     FINT = FACTOR - FLOAT(INDEX)
                     DO 2200 IB = 1, NCBANDS
                        ABSCOICE(IB) = FICE * 
     &                       (ABSICE2(INDEX,IB) + FINT *
     &                       (ABSICE2(INDEX+1,IB) - 
     &                       (ABSICE2(INDEX,IB))))
 2200                CONTINUE
                     ICEPAT = 2
                  ELSEIF (ICEFLAG .EQ. 3) THEN
                     IF (RADICE .LT. 5.0 .OR. RADICE .GT. 140.) STOP
     &                    'ICE GENERALIZED EFFECTIVE SIZE OUT OF BOUNDS'
                     NCBANDS = 16
                     FACTOR = (RADICE-2.)/3.
                     INDEX = INT(FACTOR)
                     IF (INDEX .EQ. 46) INDEX = 45
                     FINT = FACTOR - FLOAT(INDEX)
                     DO 2300 IB = 1, NCBANDS
                        ABSCOICE(IB) = FICE * 
     &                       (ABSICE3(INDEX,IB) + FINT *
     &                       (ABSICE3(INDEX+1,IB) - 
     &                       (ABSICE3(INDEX,IB))))
 2300                CONTINUE
                     ICEPAT = 2
                  ENDIF
                  
C     Calculation of absorption coefficients due to water clouds.
                  FLIQ = 1. - FICE
                  IF (FLIQ .EQ. 0.0) THEN
                     ABSCOLIQ(1) = 0.0
                     LIQPAT = 0
                     IF (ICEPAT .EQ. 1) ICEPAT = 2
                  ELSEIF (LIQFLAG .EQ. 0) THEN
                     ABSCOLIQ(1) = FLIQ * ABSLIQ0
                     LIQPAT = 0
                     IF (ICEPAT .EQ. 1) ICEPAT = 2
                  ELSEIF (LIQFLAG .EQ. 1) THEN
                     RADLIQ = CLDDAT4(LAY)
                     IF (RADLIQ .LT. 2.5 .OR. RADLIQ .GT. 60.) STOP
     &                    'LIQUID EFFECTIVE RADIUS OUT OF BOUNDS'
                     INDEX = RADLIQ - 1.5
                     IF (INDEX .EQ. 58) INDEX = 57
                     IF (INDEX .EQ. 0) INDEX = 1
                     FINT = RADLIQ - 1.5 - INDEX
                     NCBANDS = 16
                     DO 2400 IB = 1, NCBANDS
                        ABSCOLIQ(IB) = FLIQ * 
     &                       (ABSLIQ1(INDEX,IB) + FINT *
     &                       (ABSLIQ1(INDEX+1,IB) - 
     &                       (ABSLIQ1(INDEX,IB))))
 2400                CONTINUE
                     LIQPAT = 2
                  ENDIF
                  
C KEEP RUNNING TOTAL OF OPTICAL DEPTH OF EACH CLOUD.  WHEN
C A CLEAR LAYER SEPARATES CLOUDY LAYERS, RESET COUNTER

                  IF (LAY .NE. NPRELAY+1) TAUTOT(:) = 0.0

                  DO 2800 IB = 1, NCBANDS
                     TAULIQ = CWP * ABSCOLIQ(IPAT(IB,LIQPAT))
                     TAUICE = CWP * ABSCOICE(IPAT(IB,ICEPAT))
                     TAUCLOUD(LAY,IB) = TAULIQ +
     &                    TAUICE
                     TAUTOT(IB) = TAUTOT(IB) + 
     &                    TAUCLOUD(LAY,IB)
                     IF (NCBANDS .EQ. 1) THEN
                         DO 2600 IBPR =1, 16                             
                             WRITE(ICD,9000) LAY,IBPR,
     &                           WAVENUM1(IBPR),WAVENUM2(IBPR),
     &                           TAUICE,TAULIQ,
     &                           TAUCLOUD(LAY,IB),
     &                           TAUTOT(IB)
 2600                    CONTINUE
                     ELSEIF (NCBANDS .EQ. 5) THEN
                         DO 2700 IBPR = 1, 16
                             IF (IPAT(IBPR,1) .EQ. IB) THEN
                                 WRITE(ICD,9000) LAY,IBPR,
     &                               WAVENUM1(IBPR),WAVENUM2(IBPR),
     &                               TAUICE,TAULIQ,
     &                               TAUCLOUD(LAY,IB),
     &                               TAUTOT(IB)
                             ENDIF
 2700                    CONTINUE
                     ELSE
                         WRITE(ICD,9000) LAY,IB,
     &                       WAVENUM1(IB),WAVENUM2(IB),
     &                       TAUICE,TAULIQ,
     &                       TAUCLOUD(LAY,IB),
     &                       TAUTOT(IB)
                     ENDIF
 2800             CONTINUE
               ENDIF
               NPRELAY = LAY
            ENDIF
 3000    CONTINUE

      ELSEIF (ISCAT .EQ. 2) THEN
         WRITE(ICD,8902) '  LAY','  BAND  ',
     &        '  WVN1  ','  WVN2  ',
     &        '   ICE OD    ','   LIQ OD   ',
     &        '  TOT EXT OD ',' TOT EXT OD ',
     &        '  ICE SSA    ',' LIQ  SSA   ',
     &        '     ICE     ','    LIQ     '
         WRITE(ICD,8903) '   BY BLOCK  ','  ASYM  FAC  ',
     &        '  ASYM  FAC  '

         NPRELAY = 0
         DO 6000 LAY = 1, NLAYERS

            IF (CLDFRAC(LAY) .GE. EPS) THEN
               IF (CLDFRAC(LAY) .NE. 1.0 ) 
     &              STOP 'CLDFRAC MUST BE 1 WHEN ISCAT=1,2'
               ICLDATM = 1
               
               IF (INFLAG .EQ. 1) THEN
                  PRINT*,'INFLAG OPTION 1 NOT ALLOWED WITH ISCAT=2'
                  STOP
               ENDIF
               
C     Separate treatment of ice clouds and water clouds.
               IF (INFLAG .EQ. 2) THEN
                  CWP = CLDDAT1(LAY)
                  FICE = CLDDAT2(LAY)
                  RADICE = CLDDAT3(LAY)
                  
                  IF (FICE .NE. 1.0) THEN
                     PRINT*,'ICE FRACTION MUST BE SET TO 1.0'
                     STOP
                  ENDIF
                  
                  IF (ICEFLAG .NE. 2 .AND. ICEFLAG .NE. 3) THEN
                     PRINT*,'ICEFLAG MUST BE SET TO 2 OR 3 FOR
     &                    SCATTERING CALCULATIONS',iceflag
                     STOP
                  ENDIF

                  IF (ICEFLAG .EQ. 2) THEN
                     IF (RADICE .LT. 5.0 .OR. RADICE .GT. 131.)
     &                    STOP 'ICE RADIUS OUT OF BOUNDS'
                     NCBANDS = 16
                     FACTOR = (RADICE - 2.)/3.
                     INDEX = INT(FACTOR)
                     IF (INDEX .EQ. 43) INDEX = 42
                     FINT = FACTOR - FLOAT(INDEX)
                     DO 5200 IB = 1, NCBANDS
                        EXTCOICE(IB) = FICE * 
     &                       (EXTICE2(INDEX,IB) + FINT *
     &                       (EXTICE2(INDEX+1,IB) - 
     &                       EXTICE2(INDEX,IB)))
                        SSACOICE(IB) = SSAICE2(INDEX,IB) + FINT *
     &                       (SSAICE2(INDEX+1,IB) - 
     &                       SSAICE2(INDEX,IB))
                        GICE(IB) = ASYICE2(INDEX,IB) + FINT *
     &                       (ASYICE2(INDEX+1,IB) - 
     &                       ASYICE2(INDEX,IB))
 5200                CONTINUE
                     ICEPAT = 2
                  ENDIF

                  IF (ICEFLAG .EQ. 3) THEN
                     IF (RADICE .LT. 5.0 .OR. RADICE .GT. 140.)
     &                    STOP 'ICE RADIUS OUT OF BOUNDS'
                     NCBANDS = 16
                     FACTOR = (RADICE-2)/3.
                     INDEX = INT(FACTOR)
                     IF (INDEX .EQ. 46) INDEX = 45
                     FINT = FACTOR - FLOAT(INDEX)
                     DO 5300 IB = 1, NCBANDS
                        EXTCOICE(IB) = FICE * 
     &                       (EXTICE3(INDEX,IB) + FINT *
     &                       (EXTICE3(INDEX+1,IB) - 
     &                       EXTICE3(INDEX,IB)))
                        SSACOICE(IB) = SSAICE3(INDEX,IB) + FINT *
     &                       (SSAICE3(INDEX+1,IB) - 
     &                       SSAICE3(INDEX,IB))
                        GICE(IB) = ASYICE3(INDEX,IB) + FINT *
     &                       (ASYICE3(INDEX+1,IB) - 
     &                       ASYICE3(INDEX,IB))
 5300                CONTINUE
                     ICEPAT = 2
                  ENDIF
                  
C     Calculation of water clouds
                FLIQ = 1. - FICE
                IF (FLIQ .NE. 0.0) STOP
     &               'LIQUID PARTICLES NOT PERMITTED, NO SCATTERING 
     &               PROPS AVAILABLE'

C KEEP RUNNING TOTAL OF OPTICAL DEPTH OF EACH CLOUD.  WHEN
C A CLEAR LAYER SEPARATES CLOUDY LAYERS, RESET COUNTER

                  IF (LAY .NE. NPRELAY+1) TAUTOT(:) = 0.0

c     Calculation of optical properties of ice-only cloud.

                DO 5800 IB = 1, NCBANDS
                   TAUCLOUD(LAY,IB) = CWP * EXTCOICE(IB)
                   SSACLOUD(LAY,IB) = SSACOICE(IB)
                   XMOM(0,LAY,IB) = 1.0
                   DO 5750 ISTR = 1, NSTR
                      XMOM(ISTR,LAY,IB) = GICE(IB)**ISTR
 5750              CONTINUE
                   TAUTOT(IB) = TAUTOT(IB) + 
     &                  TAUCLOUD(LAY,IB)
                   WRITE(ICD,9001) LAY,IB,WAVENUM1(IB),WAVENUM2(IB),
     &                  TAUCLOUD(LAY,IB),0.0,TAUCLOUD(LAY,IB),
     &                  TAUTOT(IB),SSACLOUD(LAY,IB),
     &                  0.0,GICE(IB),0.0
 5800           CONTINUE
             ENDIF   
             NPRELAY = LAY
          ENDIF
 6000  CONTINUE
      ENDIF

 7998 FORMAT(A5,A6,A8,A8,2(A13))
 7999 FORMAT(2X,3X,1X,4X,1X,7X,1X,7X,1X,
     &     1(12X,1X),A13)
 8900 FORMAT(A5,A6,A8,A8,4(A13))
 8901 FORMAT(2X,3X,1X,4X,1X,7X,1X,7X,1X,
     &     12X,1X,12X,1X,12X,1X,A13)
 8902 FORMAT(A5,A6,A8,A8,8(A13))
 8903 FORMAT((2X,3X),(1X,4X,1X),2(7X,1X),
     &     3(12X,1X),A13,2(12X,1X),A13,A13)

 9000 FORMAT((2X,I3),(1X,I4,1X),2(F7.1,1X),4(E12.5,1X))
 9001 FORMAT(2X,I3,1X,I4,1X,2(F7.1,1X),4(E12.5,1X),
     &     4(E12.5,1X))
 9002 FORMAT((2X,I3),(1X,I4,1X),2(F7.1,1X),2(E12.5,1X))

      RETURN
      END

      BLOCK DATA CLDPARAMS

      COMMON / CLDOPTPROPS / ABSCLD1, ABSLIQ0,ABSICE0(2), 
     &     ABSICE1(2,5), ABSICE2(43,16), 
     &     ABSICE3(46,16), ABSLIQ1(58,16), 
     &     EXTICE2(43,16), SSAICE2(43,16),ASYICE2(43,16),
     &     EXTICE3(46,16), SSAICE3(46,16),ASYICE3(46,16)


C     ABSCLDn is the liquid water absorption coefficient (m2/g). 
C     For INFLAG = 1.
      DATA ABSCLD1 /0.0602410/
C  
C     Everything below is for INFLAG = 2.

C     ABSICEn(J,IB) are the parameters needed to compute the liquid water 
C     absorption coefficient in spectral region IB for ICEFLAG=n.  The 
C     units of ABSICEn(1,IB) are m2/g and ABSICEn(2,IB) has units 
C     (microns (m2/g)).
C     For ICEFLAG = 0.
      DATA ABSICE0 /0.005,  1.0/
C     For ICEFLAG = 1.
      DATA ABSICE1 /0.0036, 1.136,
     &              0.0068, 0.600,
     &              0.0003, 1.338,
     &              0.0016, 1.166,
     &              0.0020, 1.118/

C     For ICEFLAG = 2.  In each band, the absorption
C     coefficients are listed for a range of effective radii from 5.0
C     to 131.0 microns in increments of 3.0 microns.
C     Spherical Ice Particle Parameterization
C     ABSORPTION UNITS (ABS COEF/IWC): [(m^-1)/(g m^-3)]
      DATA (ABSICE2(I,1),I=1,43) /
C    BAND 1
     &7.798999e-02,6.340479e-02,5.417973e-02,4.766245e-02,4.272663e-02,
     &3.880939e-02,3.559544e-02,3.289241e-02,3.057511e-02,2.855800e-02,
     &2.678022e-02,2.519712e-02,2.377505e-02,2.248806e-02,2.131578e-02,
     &2.024194e-02,1.925337e-02,1.833926e-02,1.749067e-02,1.670007e-02,
     &1.596113e-02,1.526845e-02,1.461739e-02,1.400394e-02,1.342462e-02,
     &1.287639e-02,1.235656e-02,1.186279e-02,1.139297e-02,1.094524e-02,
     &1.051794e-02,1.010956e-02,9.718755e-03,9.344316e-03,8.985139e-03,
     &8.640223e-03,8.308656e-03,7.989606e-03,7.682312e-03,7.386076e-03,
     &7.100255e-03,6.824258e-03,6.557540e-03/
      DATA (ABSICE2(I,2),I=1,43) /
C    BAND 2
     &2.784879e-02,2.709863e-02,2.619165e-02,2.529230e-02,2.443225e-02,
     &2.361575e-02,2.284021e-02,2.210150e-02,2.139548e-02,2.071840e-02,
     &2.006702e-02,1.943856e-02,1.883064e-02,1.824120e-02,1.766849e-02,
     &1.711099e-02,1.656737e-02,1.603647e-02,1.551727e-02,1.500886e-02,
     &1.451045e-02,1.402132e-02,1.354084e-02,1.306842e-02,1.260355e-02,
     &1.214575e-02,1.169460e-02,1.124971e-02,1.081072e-02,1.037731e-02,
     &9.949167e-03,9.526021e-03,9.107615e-03,8.693714e-03,8.284096e-03,
     &7.878558e-03,7.476910e-03,7.078974e-03,6.684586e-03,6.293589e-03,
     &5.905839e-03,5.521200e-03,5.139543e-03/
      DATA (ABSICE2(I,3),I=1,43) /
C    BAND 3
     &1.065397e-01,8.005726e-02,6.546428e-02,5.589131e-02,4.898681e-02,
     &4.369932e-02,3.947901e-02,3.600676e-02,3.308299e-02,3.057561e-02,
     &2.839325e-02,2.647040e-02,2.475872e-02,2.322164e-02,2.183091e-02,
     &2.056430e-02,1.940407e-02,1.833586e-02,1.734787e-02,1.643034e-02,
     &1.557512e-02,1.477530e-02,1.402501e-02,1.331924e-02,1.265364e-02,
     &1.202445e-02,1.142838e-02,1.086257e-02,1.032445e-02,9.811791e-03,
     &9.322587e-03,8.855053e-03,8.407591e-03,7.978763e-03,7.567273e-03,
     &7.171949e-03,6.791728e-03,6.425642e-03,6.072809e-03,5.732424e-03,
     &5.403748e-03,5.086103e-03,4.778865e-03/
      DATA (ABSICE2(I,4),I=1,43) /
C    BAND 4
     &1.804566e-01,1.168987e-01,8.680442e-02,6.910060e-02,5.738174e-02,
     &4.902332e-02,4.274585e-02,3.784923e-02,3.391734e-02,3.068690e-02,
     &2.798301e-02,2.568480e-02,2.370600e-02,2.198337e-02,2.046940e-02,
     &1.912777e-02,1.793016e-02,1.685420e-02,1.588193e-02,1.499882e-02,
     &1.419293e-02,1.345440e-02,1.277496e-02,1.214769e-02,1.156669e-02,
     &1.102694e-02,1.052412e-02,1.005451e-02,9.614854e-03,9.202335e-03,
     &8.814470e-03,8.449077e-03,8.104223e-03,7.778195e-03,7.469466e-03,
     &7.176671e-03,6.898588e-03,6.634117e-03,6.382264e-03,6.142134e-03,
     &5.912913e-03,5.693862e-03,5.484308e-03/
      DATA (ABSICE2(I,5),I=1,43) /
C    BAND 5
     &2.131806e-01,1.311372e-01,9.407171e-02,7.299442e-02,5.941273e-02,
     &4.994043e-02,4.296242e-02,3.761113e-02,3.337910e-02,2.994978e-02,
     &2.711556e-02,2.473461e-02,2.270681e-02,2.095943e-02,1.943839e-02,
     &1.810267e-02,1.692057e-02,1.586719e-02,1.492275e-02,1.407132e-02,
     &1.329989e-02,1.259780e-02,1.195618e-02,1.136761e-02,1.082583e-02,
     &1.032552e-02,9.862158e-03,9.431827e-03,9.031157e-03,8.657217e-03,
     &8.307449e-03,7.979609e-03,7.671724e-03,7.382048e-03,7.109032e-03,
     &6.851298e-03,6.607615e-03,6.376881e-03,6.158105e-03,5.950394e-03,
     &5.752942e-03,5.565019e-03,5.385963e-03/
      DATA (ABSICE2(I,6),I=1,43) /
C    BAND 6
     &1.546177e-01,1.039251e-01,7.910347e-02,6.412429e-02,5.399997e-02,
     &4.664937e-02,4.104237e-02,3.660781e-02,3.300218e-02,3.000586e-02,
     &2.747148e-02,2.529633e-02,2.340647e-02,2.174723e-02,2.027731e-02,
     &1.896487e-02,1.778492e-02,1.671761e-02,1.574692e-02,1.485978e-02,
     &1.404543e-02,1.329489e-02,1.260066e-02,1.195636e-02,1.135657e-02,
     &1.079664e-02,1.027257e-02,9.780871e-03,9.318505e-03,8.882815e-03,
     &8.471458e-03,8.082364e-03,7.713696e-03,7.363817e-03,7.031264e-03,
     &6.714725e-03,6.413021e-03,6.125086e-03,5.849958e-03,5.586764e-03,
     &5.334707e-03,5.093066e-03,4.861179e-03/
      DATA (ABSICE2(I,7),I=1,43) /
C    BAND 7
     &7.583404e-02,6.181558e-02,5.312027e-02,4.696039e-02,4.225986e-02,
     &3.849735e-02,3.538340e-02,3.274182e-02,3.045798e-02,2.845343e-02,
     &2.667231e-02,2.507353e-02,2.362606e-02,2.230595e-02,2.109435e-02,
     &1.997617e-02,1.893916e-02,1.797328e-02,1.707016e-02,1.622279e-02,
     &1.542523e-02,1.467241e-02,1.395997e-02,1.328414e-02,1.264164e-02,
     &1.202958e-02,1.144544e-02,1.088697e-02,1.035218e-02,9.839297e-03,
     &9.346733e-03,8.873057e-03,8.416980e-03,7.977335e-03,7.553066e-03,
     &7.143210e-03,6.746888e-03,6.363297e-03,5.991700e-03,5.631422e-03,
     &5.281840e-03,4.942378e-03,4.612505e-03/
      DATA (ABSICE2(I,8),I=1,43) /
C    BAND 8
     &9.022185e-02,6.922700e-02,5.710674e-02,4.898377e-02,4.305946e-02,
     &3.849553e-02,3.484183e-02,3.183220e-02,2.929794e-02,2.712627e-02,
     &2.523856e-02,2.357810e-02,2.210286e-02,2.078089e-02,1.958747e-02,
     &1.850310e-02,1.751218e-02,1.660205e-02,1.576232e-02,1.498440e-02,
     &1.426107e-02,1.358624e-02,1.295474e-02,1.236212e-02,1.180456e-02,
     &1.127874e-02,1.078175e-02,1.031106e-02,9.864433e-03,9.439878e-03,
     &9.035637e-03,8.650140e-03,8.281981e-03,7.929895e-03,7.592746e-03,
     &7.269505e-03,6.959238e-03,6.661100e-03,6.374317e-03,6.098185e-03,
     &5.832059e-03,5.575347e-03,5.327504e-03/
      DATA (ABSICE2(I,9),I=1,43) /
C    BAND 9
     &1.294087e-01,8.788217e-02,6.728288e-02,5.479720e-02,4.635049e-02,
     &4.022253e-02,3.555576e-02,3.187259e-02,2.888498e-02,2.640843e-02,
     &2.431904e-02,2.253038e-02,2.098024e-02,1.962267e-02,1.842293e-02,
     &1.735426e-02,1.639571e-02,1.553060e-02,1.474552e-02,1.402953e-02,
     &1.337363e-02,1.277033e-02,1.221336e-02,1.169741e-02,1.121797e-02,
     &1.077117e-02,1.035369e-02,9.962643e-03,9.595509e-03,9.250088e-03,
     &8.924447e-03,8.616876e-03,8.325862e-03,8.050057e-03,7.788258e-03,
     &7.539388e-03,7.302478e-03,7.076656e-03,6.861134e-03,6.655197e-03,
     &6.458197e-03,6.269543e-03,6.088697e-03/
      DATA (ABSICE2(I,10),I=1,43) /
C    BAND 10
     &1.593628e-01,1.014552e-01,7.458955e-02,5.903571e-02,4.887582e-02,
     &4.171159e-02,3.638480e-02,3.226692e-02,2.898717e-02,2.631256e-02,
     &2.408925e-02,2.221156e-02,2.060448e-02,1.921325e-02,1.799699e-02,
     &1.692456e-02,1.597177e-02,1.511961e-02,1.435289e-02,1.365933e-02,
     &1.302890e-02,1.245334e-02,1.192576e-02,1.144037e-02,1.099230e-02,
     &1.057739e-02,1.019208e-02,9.833302e-03,9.498395e-03,9.185047e-03,
     &8.891237e-03,8.615185e-03,8.355325e-03,8.110267e-03,7.878778e-03,
     &7.659759e-03,7.452224e-03,7.255291e-03,7.068166e-03,6.890130e-03,
     &6.720536e-03,6.558794e-03,6.404371e-03/
      DATA (ABSICE2(I,11),I=1,43) /
C    BAND 11
     &1.656227e-01,1.032129e-01,7.487359e-02,5.871431e-02,4.828355e-02,
     &4.099989e-02,3.562924e-02,3.150755e-02,2.824593e-02,2.560156e-02,
     &2.341503e-02,2.157740e-02,2.001169e-02,1.866199e-02,1.748669e-02,
     &1.645421e-02,1.554015e-02,1.472535e-02,1.399457e-02,1.333553e-02,
     &1.273821e-02,1.219440e-02,1.169725e-02,1.124104e-02,1.082096e-02,
     &1.043290e-02,1.007336e-02,9.739338e-03,9.428223e-03,9.137756e-03,
     &8.865964e-03,8.611115e-03,8.371686e-03,8.146330e-03,7.933852e-03,
     &7.733187e-03,7.543386e-03,7.363597e-03,7.193056e-03,7.031072e-03,
     &6.877024e-03,6.730348e-03,6.590531e-03/
      DATA (ABSICE2(I,12),I=1,43) /
C    BAND 12
     &9.194591e-02,6.446867e-02,4.962034e-02,4.042061e-02,3.418456e-02,
     &2.968856e-02,2.629900e-02,2.365572e-02,2.153915e-02,1.980791e-02,
     &1.836689e-02,1.714979e-02,1.610900e-02,1.520946e-02,1.442476e-02,
     &1.373468e-02,1.312345e-02,1.257858e-02,1.209010e-02,1.164990e-02,
     &1.125136e-02,1.088901e-02,1.055827e-02,1.025531e-02,9.976896e-03,
     &9.720255e-03,9.483022e-03,9.263160e-03,9.058902e-03,8.868710e-03,
     &8.691240e-03,8.525312e-03,8.369886e-03,8.224042e-03,8.086961e-03,
     &7.957917e-03,7.836258e-03,7.721400e-03,7.612821e-03,7.510045e-03,
     &7.412648e-03,7.320242e-03,7.232476e-03/
      DATA (ABSICE2(I,13),I=1,43) /
C    BAND 13
     &1.437021e-01,8.872535e-02,6.392420e-02,4.991833e-02,4.096790e-02,
     &3.477881e-02,3.025782e-02,2.681909e-02,2.412102e-02,2.195132e-02,
     &2.017124e-02,1.868641e-02,1.743044e-02,1.635529e-02,1.542540e-02,
     &1.461388e-02,1.390003e-02,1.326766e-02,1.270395e-02,1.219860e-02,
     &1.174326e-02,1.133107e-02,1.095637e-02,1.061442e-02,1.030126e-02,
     &1.001352e-02,9.748340e-03,9.503256e-03,9.276155e-03,9.065205e-03,
     &8.868808e-03,8.685571e-03,8.514268e-03,8.353820e-03,8.203272e-03,
     &8.061776e-03,7.928578e-03,7.803001e-03,7.684443e-03,7.572358e-03,
     &7.466258e-03,7.365701e-03,7.270286e-03/
      DATA (ABSICE2(I,14),I=1,43) /
C    BAND 14
     &1.288870e-01,8.160295e-02,5.964745e-02,4.703790e-02,3.888637e-02,
     &3.320115e-02,2.902017e-02,2.582259e-02,2.330224e-02,2.126754e-02,
     &1.959258e-02,1.819130e-02,1.700289e-02,1.598320e-02,1.509942e-02,
     &1.432666e-02,1.364572e-02,1.304156e-02,1.250220e-02,1.201803e-02,
     &1.158123e-02,1.118537e-02,1.082513e-02,1.049605e-02,1.019440e-02,
     &9.916989e-03,9.661116e-03,9.424457e-03,9.205005e-03,9.001022e-03,
     &8.810992e-03,8.633588e-03,8.467646e-03,8.312137e-03,8.166151e-03,
     &8.028878e-03,7.899597e-03,7.777663e-03,7.662498e-03,7.553581e-03,
     &7.450444e-03,7.352662e-03,7.259851e-03/
      DATA (ABSICE2(I,15),I=1,43) /
C    BAND 15
     &8.254229e-02,5.808787e-02,4.492166e-02,3.675028e-02,3.119623e-02,
     &2.718045e-02,2.414450e-02,2.177073e-02,1.986526e-02,1.830306e-02,
     &1.699991e-02,1.589698e-02,1.495199e-02,1.413374e-02,1.341870e-02,
     &1.278883e-02,1.223002e-02,1.173114e-02,1.128322e-02,1.087900e-02,
     &1.051254e-02,1.017890e-02,9.873991e-03,9.594347e-03,9.337044e-03,
     &9.099589e-03,8.879842e-03,8.675960e-03,8.486341e-03,8.309594e-03,
     &8.144500e-03,7.989986e-03,7.845109e-03,7.709031e-03,7.581007e-03,
     &7.460376e-03,7.346544e-03,7.238978e-03,7.137201e-03,7.040780e-03,
     &6.949325e-03,6.862483e-03,6.779931e-03/
      DATA (ABSICE2(I,16),I=1,43) /
C    BAND 16
     &1.382062e-01,8.643227e-02,6.282935e-02,4.934783e-02,4.063891e-02,
     &3.455591e-02,3.007059e-02,2.662897e-02,2.390631e-02,2.169972e-02,
     &1.987596e-02,1.834393e-02,1.703924e-02,1.591513e-02,1.493679e-02,
     &1.407780e-02,1.331775e-02,1.264061e-02,1.203364e-02,1.148655e-02,
     &1.099099e-02,1.054006e-02,1.012807e-02,9.750215e-03,9.402477e-03,
     &9.081428e-03,8.784143e-03,8.508107e-03,8.251146e-03,8.011373e-03,
     &7.787140e-03,7.577002e-03,7.379687e-03,7.194071e-03,7.019158e-03,
     &6.854061e-03,6.697986e-03,6.550224e-03,6.410138e-03,6.277153e-03,
     &6.150751e-03,6.030462e-03,5.915860e-03/

C ICEFLAG = 3; Fu parameterization. Particle size 5 - 140 micron in 
C increments of 3 microns.
C units = m2/g
C     Hexagonal Ice Particle Parameterization
C     ABSORPTION UNITS (ABS COEF/IWC): [(m^-1)/(g m^-3)]
      DATA (ABSICE3(I,1),I=1,46) /
C    BAND 1
     &3.110649e-03,4.666352e-02,6.606447e-02,6.531678e-02,6.012598e-02,
     &5.437494e-02,4.906411e-02,4.441146e-02,4.040585e-02,3.697334e-02,
     &3.403027e-02,3.149979e-02,2.931596e-02,2.742365e-02,2.577721e-02,
     &2.433888e-02,2.307732e-02,2.196644e-02,2.098437e-02,2.011264e-02,
     &1.933561e-02,1.863992e-02,1.801407e-02,1.744812e-02,1.693346e-02,
     &1.646252e-02,1.602866e-02,1.562600e-02,1.524933e-02,1.489399e-02,
     &1.455580e-02,1.423098e-02,1.391612e-02,1.360812e-02,1.330413e-02,
     &1.300156e-02,1.269801e-02,1.239127e-02,1.207928e-02,1.176014e-02,
     &1.143204e-02,1.109334e-02,1.074243e-02,1.037786e-02,9.998198e-03,
     &9.602126e-03/
      DATA (ABSICE3(I,2),I=1,46) /
C    BAND 2
     &3.984966e-04,1.681097e-02,2.627680e-02,2.767465e-02,2.700722e-02,
     &2.579180e-02,2.448677e-02,2.323890e-02,2.209096e-02,2.104882e-02,
     &2.010547e-02,1.925003e-02,1.847128e-02,1.775883e-02,1.710358e-02,
     &1.649769e-02,1.593449e-02,1.540829e-02,1.491429e-02,1.444837e-02,
     &1.400704e-02,1.358729e-02,1.318654e-02,1.280258e-02,1.243346e-02,
     &1.207750e-02,1.173325e-02,1.139941e-02,1.107487e-02,1.075861e-02,
     &1.044975e-02,1.014753e-02,9.851229e-03,9.560240e-03,9.274003e-03,
     &8.992020e-03,8.713845e-03,8.439074e-03,8.167346e-03,7.898331e-03,
     &7.631734e-03,7.367286e-03,7.104742e-03,6.843882e-03,6.584504e-03,
     &6.326424e-03/
      DATA (ABSICE3(I,3),I=1,46) /
C    BAND 3
     &6.933163e-02,8.540475e-02,7.701816e-02,6.771158e-02,5.986953e-02,
     &5.348120e-02,4.824962e-02,4.390563e-02,4.024411e-02,3.711404e-02,
     &3.440426e-02,3.203200e-02,2.993478e-02,2.806474e-02,2.638464e-02,
     &2.486516e-02,2.348288e-02,2.221890e-02,2.105780e-02,1.998687e-02,
     &1.899552e-02,1.807490e-02,1.721750e-02,1.641693e-02,1.566773e-02,
     &1.496515e-02,1.430509e-02,1.368398e-02,1.309865e-02,1.254634e-02,
     &1.202456e-02,1.153114e-02,1.106409e-02,1.062166e-02,1.020224e-02,
     &9.804381e-03,9.426771e-03,9.068205e-03,8.727578e-03,8.403876e-03,
     &8.096160e-03,7.803564e-03,7.525281e-03,7.260560e-03,7.008697e-03,
     &6.769036e-03/
      DATA (ABSICE3(I,4),I=1,46) /
C    BAND 4
     &1.765735e-01,1.382700e-01,1.095129e-01,8.987475e-02,7.591185e-02,
     &6.554169e-02,5.755500e-02,5.122083e-02,4.607610e-02,4.181475e-02,
     &3.822697e-02,3.516432e-02,3.251897e-02,3.021073e-02,2.817876e-02,
     &2.637607e-02,2.476582e-02,2.331871e-02,2.201113e-02,2.082388e-02,
     &1.974115e-02,1.874983e-02,1.783894e-02,1.699922e-02,1.622280e-02,
     &1.550296e-02,1.483390e-02,1.421064e-02,1.362880e-02,1.308460e-02,
     &1.257468e-02,1.209611e-02,1.164628e-02,1.122287e-02,1.082381e-02,
     &1.044725e-02,1.009154e-02,9.755166e-03,9.436783e-03,9.135163e-03,
     &8.849193e-03,8.577856e-03,8.320225e-03,8.075451e-03,7.842755e-03,
     &7.621418e-03/
      DATA (ABSICE3(I,5),I=1,46) /
C    BAND 5
     &2.339673e-01,1.692124e-01,1.291656e-01,1.033837e-01,8.562949e-02,
     &7.273526e-02,6.298262e-02,5.537015e-02,4.927787e-02,4.430246e-02,
     &4.017061e-02,3.669072e-02,3.372455e-02,3.116995e-02,2.894977e-02,
     &2.700471e-02,2.528842e-02,2.376420e-02,2.240256e-02,2.117959e-02,
     &2.007567e-02,1.907456e-02,1.816271e-02,1.732874e-02,1.656300e-02,
     &1.585725e-02,1.520445e-02,1.459852e-02,1.403419e-02,1.350689e-02,
     &1.301260e-02,1.254781e-02,1.210941e-02,1.169468e-02,1.130118e-02,
     &1.092675e-02,1.056945e-02,1.022757e-02,9.899560e-03,9.584021e-03,
     &9.279705e-03,8.985479e-03,8.700322e-03,8.423306e-03,8.153590e-03,
     &7.890412e-03/
      DATA (ABSICE3(I,6),I=1,46) /
C    BAND 6
     &1.145369e-01,1.174566e-01,9.917866e-02,8.332990e-02,7.104263e-02,
     &6.153370e-02,5.405472e-02,4.806281e-02,4.317918e-02,3.913795e-02,
     &3.574916e-02,3.287437e-02,3.041067e-02,2.828017e-02,2.642292e-02,
     &2.479206e-02,2.335051e-02,2.206851e-02,2.092195e-02,1.989108e-02,
     &1.895958e-02,1.811385e-02,1.734245e-02,1.663573e-02,1.598545e-02,
     &1.538456e-02,1.482700e-02,1.430750e-02,1.382150e-02,1.336499e-02,
     &1.293447e-02,1.252685e-02,1.213939e-02,1.176968e-02,1.141555e-02,
     &1.107508e-02,1.074655e-02,1.042839e-02,1.011923e-02,9.817799e-03,
     &9.522962e-03,9.233688e-03,8.949041e-03,8.668171e-03,8.390301e-03,
     &8.114723e-03/
      DATA (ABSICE3(I,7),I=1,46) /
C    BAND 7
     &1.222345e-02,5.344230e-02,5.523465e-02,5.128759e-02,4.676925e-02,
     &4.266150e-02,3.910561e-02,3.605479e-02,3.342843e-02,3.115052e-02,
     &2.915776e-02,2.739935e-02,2.583499e-02,2.443266e-02,2.316681e-02,
     &2.201687e-02,2.096619e-02,2.000112e-02,1.911044e-02,1.828481e-02,
     &1.751641e-02,1.679866e-02,1.612598e-02,1.549360e-02,1.489742e-02,
     &1.433392e-02,1.380002e-02,1.329305e-02,1.281068e-02,1.235084e-02,
     &1.191172e-02,1.149171e-02,1.108936e-02,1.070341e-02,1.033271e-02,
     &9.976220e-03,9.633021e-03,9.302273e-03,8.983216e-03,8.675161e-03,
     &8.377478e-03,8.089595e-03,7.810986e-03,7.541170e-03,7.279706e-03,
     &7.026186e-03/
      DATA (ABSICE3(I,8),I=1,46) /
C    BAND 8
     &6.711058e-02,6.918198e-02,6.127484e-02,5.411944e-02,4.836902e-02,
     &4.375293e-02,3.998077e-02,3.683587e-02,3.416508e-02,3.186003e-02,
     &2.984290e-02,2.805671e-02,2.645895e-02,2.501733e-02,2.370689e-02,
     &2.250808e-02,2.140532e-02,2.038609e-02,1.944018e-02,1.855918e-02,
     &1.773609e-02,1.696504e-02,1.624106e-02,1.555990e-02,1.491793e-02,
     &1.431197e-02,1.373928e-02,1.319743e-02,1.268430e-02,1.219799e-02,
     &1.173682e-02,1.129925e-02,1.088393e-02,1.048961e-02,1.011516e-02,
     &9.759543e-03,9.421813e-03,9.101089e-03,8.796559e-03,8.507464e-03,
     &8.233098e-03,7.972798e-03,7.725942e-03,7.491940e-03,7.270238e-03,
     &7.060305e-03/
      DATA (ABSICE3(I,9),I=1,46) /
C    BAND 9
     &1.236780e-01,9.222386e-02,7.383997e-02,6.204072e-02,5.381029e-02,
     &4.770678e-02,4.296928e-02,3.916131e-02,3.601540e-02,3.335878e-02,
     &3.107493e-02,2.908247e-02,2.732282e-02,2.575276e-02,2.433968e-02,
     &2.305852e-02,2.188966e-02,2.081757e-02,1.982974e-02,1.891599e-02,
     &1.806794e-02,1.727865e-02,1.654227e-02,1.585387e-02,1.520924e-02,
     &1.460476e-02,1.403730e-02,1.350416e-02,1.300293e-02,1.253153e-02,
     &1.208808e-02,1.167094e-02,1.127862e-02,1.090979e-02,1.056323e-02,
     &1.023786e-02,9.932665e-03,9.646744e-03,9.379250e-03,9.129409e-03,
     &8.896500e-03,8.679856e-03,8.478852e-03,8.292904e-03,8.121463e-03,
     &7.964013e-03/
      DATA (ABSICE3(I,10),I=1,46) /
C    BAND 10
     &1.655966e-01,1.134205e-01,8.714344e-02,7.129241e-02,6.063739e-02,
     &5.294203e-02,4.709309e-02,4.247476e-02,3.871892e-02,3.559206e-02,
     &3.293893e-02,3.065226e-02,2.865558e-02,2.689288e-02,2.532221e-02,
     &2.391150e-02,2.263582e-02,2.147549e-02,2.041476e-02,1.944089e-02,
     &1.854342e-02,1.771371e-02,1.694456e-02,1.622989e-02,1.556456e-02,
     &1.494415e-02,1.436491e-02,1.382354e-02,1.331719e-02,1.284339e-02,
     &1.239992e-02,1.198486e-02,1.159647e-02,1.123323e-02,1.089375e-02,
     &1.057679e-02,1.028124e-02,1.000607e-02,9.750376e-03,9.513303e-03,
     &9.294082e-03,9.092003e-03,8.906412e-03,8.736702e-03,8.582314e-03,
     &8.442725e-03/
      DATA (ABSICE3(I,11),I=1,46) /
C    BAND 11
     &1.775615e-01,1.180046e-01,8.929607e-02,7.233500e-02,6.108333e-02,
     &5.303642e-02,4.696927e-02,4.221206e-02,3.836768e-02,3.518576e-02,
     &3.250063e-02,3.019825e-02,2.819758e-02,2.643943e-02,2.487953e-02,
     &2.348414e-02,2.222705e-02,2.108762e-02,2.004936e-02,1.909892e-02,
     &1.822539e-02,1.741975e-02,1.667449e-02,1.598330e-02,1.534084e-02,
     &1.474253e-02,1.418446e-02,1.366325e-02,1.317597e-02,1.272004e-02,
     &1.229321e-02,1.189350e-02,1.151915e-02,1.116859e-02,1.084042e-02,
     &1.053338e-02,1.024636e-02,9.978326e-03,9.728357e-03,9.495613e-03,
     &9.279327e-03,9.078798e-03,8.893383e-03,8.722488e-03,8.565568e-03,
     &8.422115e-03/
      DATA (ABSICE3(I,12),I=1,46) /
C    BAND 12
     &9.465447e-02,6.432047e-02,5.060973e-02,4.267283e-02,3.741843e-02,
     &3.363096e-02,3.073531e-02,2.842405e-02,2.651789e-02,2.490518e-02,
     &2.351273e-02,2.229056e-02,2.120335e-02,2.022541e-02,1.933763e-02,
     &1.852546e-02,1.777763e-02,1.708528e-02,1.644134e-02,1.584009e-02,
     &1.527684e-02,1.474774e-02,1.424955e-02,1.377957e-02,1.333549e-02,
     &1.291534e-02,1.251743e-02,1.214029e-02,1.178265e-02,1.144337e-02,
     &1.112148e-02,1.081609e-02,1.052642e-02,1.025178e-02,9.991540e-03,
     &9.745130e-03,9.512038e-03,9.291797e-03,9.083980e-03,8.888195e-03,
     &8.704081e-03,8.531306e-03,8.369560e-03,8.218558e-03,8.078032e-03,
     &7.947730e-03/
      DATA (ABSICE3(I,13),I=1,46) /
C    BAND 13
     &1.560311e-01,9.961097e-02,7.502949e-02,6.115022e-02,5.214952e-02,
     &4.578149e-02,4.099731e-02,3.724174e-02,3.419343e-02,3.165356e-02,
     &2.949251e-02,2.762222e-02,2.598073e-02,2.452322e-02,2.321642e-02,
     &2.203516e-02,2.096002e-02,1.997579e-02,1.907036e-02,1.823401e-02,
     &1.745879e-02,1.673819e-02,1.606678e-02,1.544003e-02,1.485411e-02,
     &1.430574e-02,1.379215e-02,1.331092e-02,1.285996e-02,1.243746e-02,
     &1.204183e-02,1.167164e-02,1.132567e-02,1.100281e-02,1.070207e-02,
     &1.042258e-02,1.016352e-02,9.924197e-03,9.703953e-03,9.502199e-03,
     &9.318400e-03,9.152066e-03,9.002749e-03,8.870038e-03,8.753555e-03,
     &8.652951e-03/
      DATA (ABSICE3(I,14),I=1,46) /
C    BAND 14
     &1.559547e-01,9.896700e-02,7.441231e-02,6.061469e-02,5.168730e-02,
     &4.537821e-02,4.064106e-02,3.692367e-02,3.390714e-02,3.139438e-02,
     &2.925702e-02,2.740783e-02,2.578547e-02,2.434552e-02,2.305506e-02,
     &2.188910e-02,2.082842e-02,1.985789e-02,1.896553e-02,1.814165e-02,
     &1.737839e-02,1.666927e-02,1.600891e-02,1.539279e-02,1.481712e-02,
     &1.427865e-02,1.377463e-02,1.330266e-02,1.286068e-02,1.244689e-02,
     &1.205973e-02,1.169780e-02,1.135989e-02,1.104492e-02,1.075192e-02,
     &1.048004e-02,1.022850e-02,9.996611e-03,9.783753e-03,9.589361e-03,
     &9.412924e-03,9.253977e-03,9.112098e-03,8.986903e-03,8.878039e-03,
     &8.785184e-03/
      DATA (ABSICE3(I,15),I=1,46) /
C    BAND 15
     &1.102926e-01,7.176622e-02,5.530316e-02,4.606056e-02,4.006116e-02,
     &3.579628e-02,3.256909e-02,3.001360e-02,2.791920e-02,2.615617e-02,
     &2.464023e-02,2.331426e-02,2.213817e-02,2.108301e-02,2.012733e-02,
     &1.925493e-02,1.845331e-02,1.771269e-02,1.702531e-02,1.638493e-02,
     &1.578648e-02,1.522579e-02,1.469940e-02,1.420442e-02,1.373841e-02,
     &1.329931e-02,1.288535e-02,1.249502e-02,1.212700e-02,1.178015e-02,
     &1.145348e-02,1.114612e-02,1.085730e-02,1.058633e-02,1.033263e-02,
     &1.009564e-02,9.874895e-03,9.669960e-03,9.480449e-03,9.306014e-03,
     &9.146339e-03,9.001138e-03,8.870154e-03,8.753148e-03,8.649907e-03,
     &8.560232e-03/
      DATA (ABSICE3(I,16),I=1,46) /
C    BAND 16
     &1.688344e-01,1.077072e-01,7.994467e-02,6.403862e-02,5.369850e-02,
     &4.641582e-02,4.099331e-02,3.678724e-02,3.342069e-02,3.065831e-02,
     &2.834557e-02,2.637680e-02,2.467733e-02,2.319286e-02,2.188299e-02,
     &2.071701e-02,1.967121e-02,1.872692e-02,1.786931e-02,1.708641e-02,
     &1.636846e-02,1.570743e-02,1.509665e-02,1.453052e-02,1.400433e-02,
     &1.351407e-02,1.305631e-02,1.262810e-02,1.222688e-02,1.185044e-02,
     &1.149683e-02,1.116436e-02,1.085153e-02,1.055701e-02,1.027961e-02,
     &1.001831e-02,9.772141e-03,9.540280e-03,9.321966e-03,9.116517e-03,
     &8.923315e-03,8.741803e-03,8.571472e-03,8.411860e-03,8.262543e-03,
     &8.123136e-03/


C     For LIQFLAG = 0.
      DATA ABSLIQ0 /0.0903614/

C     For LIQFLAG = 1.  In each band, the absorption
C     coefficients are listed for a range of effective radii from 2.5
C     to 59.5 microns in increments of 1.0 micron.
      DATA (ABSLIQ1(I, 1),I=1,58) /
c     BAND  1
     & 1.64047E-03, 6.90533E-02, 7.72017E-02, 7.78054E-02, 7.69523E-02,
     & 7.58058E-02, 7.46400E-02, 7.35123E-02, 7.24162E-02, 7.13225E-02,
     & 6.99145E-02, 6.66409E-02, 6.36582E-02, 6.09425E-02, 5.84593E-02,
     & 5.61743E-02, 5.40571E-02, 5.20812E-02, 5.02245E-02, 4.84680E-02,
     & 4.67959E-02, 4.51944E-02, 4.36516E-02, 4.21570E-02, 4.07015E-02,
     & 3.92766E-02, 3.78747E-02, 3.64886E-02, 3.53632E-02, 3.41992E-02,
     & 3.31016E-02, 3.20643E-02, 3.10817E-02, 3.01490E-02, 2.92620E-02,
     & 2.84171E-02, 2.76108E-02, 2.68404E-02, 2.61031E-02, 2.53966E-02,
     & 2.47189E-02, 2.40678E-02, 2.34418E-02, 2.28392E-02, 2.22586E-02,
     & 2.16986E-02, 2.11580E-02, 2.06356E-02, 2.01305E-02, 1.96417E-02,
     & 1.91682E-02, 1.87094E-02, 1.82643E-02, 1.78324E-02, 1.74129E-02,
     & 1.70052E-02, 1.66088E-02, 1.62231E-02/
      DATA (ABSLIQ1(I, 2),I=1,58) /
c     BAND  2
     & 2.19486E-01, 1.80687E-01, 1.59150E-01, 1.44731E-01, 1.33703E-01,
     & 1.24355E-01, 1.15756E-01, 1.07318E-01, 9.86119E-02, 8.92739E-02,
     & 8.34911E-02, 7.70773E-02, 7.15240E-02, 6.66615E-02, 6.23641E-02,
     & 5.85359E-02, 5.51020E-02, 5.20032E-02, 4.91916E-02, 4.66283E-02,
     & 4.42813E-02, 4.21236E-02, 4.01330E-02, 3.82905E-02, 3.65797E-02,
     & 3.49869E-02, 3.35002E-02, 3.21090E-02, 3.08957E-02, 2.97601E-02,
     & 2.86966E-02, 2.76984E-02, 2.67599E-02, 2.58758E-02, 2.50416E-02,
     & 2.42532E-02, 2.35070E-02, 2.27997E-02, 2.21284E-02, 2.14904E-02,
     & 2.08834E-02, 2.03051E-02, 1.97536E-02, 1.92271E-02, 1.87239E-02,
     & 1.82425E-02, 1.77816E-02, 1.73399E-02, 1.69162E-02, 1.65094E-02,
     & 1.61187E-02, 1.57430E-02, 1.53815E-02, 1.50334E-02, 1.46981E-02,
     & 1.43748E-02, 1.40628E-02, 1.37617E-02/
      DATA (ABSLIQ1(I, 3),I=1,58) /
c     BAND  3
     & 2.95174E-01, 2.34765E-01, 1.98038E-01, 1.72114E-01, 1.52083E-01,
     & 1.35654E-01, 1.21613E-01, 1.09252E-01, 9.81263E-02, 8.79448E-02,
     & 8.12566E-02, 7.44563E-02, 6.86374E-02, 6.36042E-02, 5.92094E-02,
     & 5.53402E-02, 5.19087E-02, 4.88455E-02, 4.60951E-02, 4.36124E-02,
     & 4.13607E-02, 3.93096E-02, 3.74338E-02, 3.57119E-02, 3.41261E-02,
     & 3.26610E-02, 3.13036E-02, 3.00425E-02, 2.88497E-02, 2.78077E-02,
     & 2.68317E-02, 2.59158E-02, 2.50545E-02, 2.42430E-02, 2.34772E-02,
     & 2.27533E-02, 2.20679E-02, 2.14181E-02, 2.08011E-02, 2.02145E-02,
     & 1.96561E-02, 1.91239E-02, 1.86161E-02, 1.81311E-02, 1.76673E-02,
     & 1.72234E-02, 1.67981E-02, 1.63903E-02, 1.59989E-02, 1.56230E-02,
     & 1.52615E-02, 1.49138E-02, 1.45791E-02, 1.42565E-02, 1.39455E-02,
     & 1.36455E-02, 1.33559E-02, 1.30761E-02/
      DATA (ABSLIQ1(I, 4),I=1,58) /
c     BAND  4
     & 3.00925E-01, 2.36949E-01, 1.96947E-01, 1.68692E-01, 1.47190E-01,
     & 1.29986E-01, 1.15719E-01, 1.03568E-01, 9.30028E-02, 8.36658E-02,
     & 7.71075E-02, 7.07002E-02, 6.52284E-02, 6.05024E-02, 5.63801E-02,
     & 5.27534E-02, 4.95384E-02, 4.66690E-02, 4.40925E-02, 4.17664E-02,
     & 3.96559E-02, 3.77326E-02, 3.59727E-02, 3.43561E-02, 3.28662E-02,
     & 3.14885E-02, 3.02110E-02, 2.90231E-02, 2.78948E-02, 2.69109E-02,
     & 2.59884E-02, 2.51217E-02, 2.43058E-02, 2.35364E-02, 2.28096E-02,
     & 2.21218E-02, 2.14700E-02, 2.08515E-02, 2.02636E-02, 1.97041E-02,
     & 1.91711E-02, 1.86625E-02, 1.81769E-02, 1.77126E-02, 1.72683E-02,
     & 1.68426E-02, 1.64344E-02, 1.60427E-02, 1.56664E-02, 1.53046E-02,
     & 1.49565E-02, 1.46214E-02, 1.42985E-02, 1.39871E-02, 1.36866E-02,
     & 1.33965E-02, 1.31162E-02, 1.28453E-02/
      DATA (ABSLIQ1(I, 5),I=1,58) /
c     BAND  5
     & 2.64691E-01, 2.12018E-01, 1.78009E-01, 1.53539E-01, 1.34721E-01,
     & 1.19580E-01, 1.06996E-01, 9.62772E-02, 8.69710E-02, 7.87670E-02,
     & 7.29272E-02, 6.70920E-02, 6.20977E-02, 5.77732E-02, 5.39910E-02,
     & 5.06538E-02, 4.76866E-02, 4.50301E-02, 4.26374E-02, 4.04704E-02,
     & 3.84981E-02, 3.66948E-02, 3.50394E-02, 3.35141E-02, 3.21038E-02,
     & 3.07957E-02, 2.95788E-02, 2.84438E-02, 2.73790E-02, 2.64390E-02,
     & 2.55565E-02, 2.47263E-02, 2.39437E-02, 2.32047E-02, 2.25056E-02,
     & 2.18433E-02, 2.12149E-02, 2.06177E-02, 2.00495E-02, 1.95081E-02,
     & 1.89917E-02, 1.84984E-02, 1.80269E-02, 1.75755E-02, 1.71431E-02,
     & 1.67283E-02, 1.63303E-02, 1.59478E-02, 1.55801E-02, 1.52262E-02,
     & 1.48853E-02, 1.45568E-02, 1.42400E-02, 1.39342E-02, 1.36388E-02,
     & 1.33533E-02, 1.30773E-02, 1.28102E-02/
      DATA (ABSLIQ1(I, 6),I=1,58) /
c     BAND  6
     & 8.81182E-02, 1.06745E-01, 9.79753E-02, 8.99625E-02, 8.35200E-02,
     & 7.81899E-02, 7.35939E-02, 6.94696E-02, 6.56266E-02, 6.19148E-02,
     & 5.83355E-02, 5.49306E-02, 5.19642E-02, 4.93325E-02, 4.69659E-02,
     & 4.48148E-02, 4.28431E-02, 4.10231E-02, 3.93332E-02, 3.77563E-02,
     & 3.62785E-02, 3.48882E-02, 3.35758E-02, 3.23333E-02, 3.11536E-02,
     & 3.00310E-02, 2.89601E-02, 2.79365E-02, 2.70502E-02, 2.62618E-02,
     & 2.55025E-02, 2.47728E-02, 2.40726E-02, 2.34013E-02, 2.27583E-02,
     & 2.21422E-02, 2.15522E-02, 2.09869E-02, 2.04453E-02, 1.99260E-02,
     & 1.94280E-02, 1.89501E-02, 1.84913E-02, 1.80506E-02, 1.76270E-02,
     & 1.72196E-02, 1.68276E-02, 1.64500E-02, 1.60863E-02, 1.57357E-02,
     & 1.53975E-02, 1.50710E-02, 1.47558E-02, 1.44511E-02, 1.41566E-02,
     & 1.38717E-02, 1.35960E-02, 1.33290E-02/
      DATA (ABSLIQ1(I, 7),I=1,58) /
c     BAND  7
     & 4.32174E-02, 7.36078E-02, 6.98340E-02, 6.65231E-02, 6.41948E-02,
     & 6.23551E-02, 6.06638E-02, 5.88680E-02, 5.67124E-02, 5.38629E-02,
     & 4.99579E-02, 4.86289E-02, 4.70120E-02, 4.52854E-02, 4.35466E-02,
     & 4.18480E-02, 4.02169E-02, 3.86658E-02, 3.71992E-02, 3.58168E-02,
     & 3.45155E-02, 3.32912E-02, 3.21390E-02, 3.10538E-02, 3.00307E-02,
     & 2.90651E-02, 2.81524E-02, 2.72885E-02, 2.62821E-02, 2.55744E-02,
     & 2.48799E-02, 2.42029E-02, 2.35460E-02, 2.29108E-02, 2.22981E-02,
     & 2.17079E-02, 2.11402E-02, 2.05945E-02, 2.00701E-02, 1.95663E-02,
     & 1.90824E-02, 1.86174E-02, 1.81706E-02, 1.77411E-02, 1.73281E-02,
     & 1.69307E-02, 1.65483E-02, 1.61801E-02, 1.58254E-02, 1.54835E-02,
     & 1.51538E-02, 1.48358E-02, 1.45288E-02, 1.42322E-02, 1.39457E-02,
     & 1.36687E-02, 1.34008E-02, 1.31416E-02/
      DATA (ABSLIQ1(I, 8),I=1,58) /
c     BAND  8
     & 1.41881E-01, 7.15419E-02, 6.30335E-02, 6.11132E-02, 6.01931E-02,
     & 5.92420E-02, 5.78968E-02, 5.58876E-02, 5.28923E-02, 4.84462E-02,
     & 4.60839E-02, 4.56013E-02, 4.45410E-02, 4.31866E-02, 4.17026E-02,
     & 4.01850E-02, 3.86892E-02, 3.72461E-02, 3.58722E-02, 3.45749E-02,
     & 3.33564E-02, 3.22155E-02, 3.11494E-02, 3.01541E-02, 2.92253E-02,
     & 2.83584E-02, 2.75488E-02, 2.67925E-02, 2.57692E-02, 2.50704E-02,
     & 2.43918E-02, 2.37350E-02, 2.31005E-02, 2.24888E-02, 2.18996E-02,
     & 2.13325E-02, 2.07870E-02, 2.02623E-02, 1.97577E-02, 1.92724E-02,
     & 1.88056E-02, 1.83564E-02, 1.79241E-02, 1.75079E-02, 1.71070E-02,
     & 1.67207E-02, 1.63482E-02, 1.59890E-02, 1.56424E-02, 1.53077E-02,
     & 1.49845E-02, 1.46722E-02, 1.43702E-02, 1.40782E-02, 1.37955E-02,
     & 1.35219E-02, 1.32569E-02, 1.30000E-02/
      DATA (ABSLIQ1(I, 9),I=1,58) /
c     BAND  9
     & 6.72726E-02, 6.61013E-02, 6.47866E-02, 6.33780E-02, 6.18985E-02,
     & 6.03335E-02, 5.86136E-02, 5.65876E-02, 5.39839E-02, 5.03536E-02,
     & 4.71608E-02, 4.63630E-02, 4.50313E-02, 4.34526E-02, 4.17876E-02,
     & 4.01261E-02, 3.85171E-02, 3.69860E-02, 3.55442E-02, 3.41954E-02,
     & 3.29384E-02, 3.17693E-02, 3.06832E-02, 2.96745E-02, 2.87374E-02,
     & 2.78662E-02, 2.70557E-02, 2.63008E-02, 2.52450E-02, 2.45424E-02,
     & 2.38656E-02, 2.32144E-02, 2.25885E-02, 2.19873E-02, 2.14099E-02,
     & 2.08554E-02, 2.03230E-02, 1.98116E-02, 1.93203E-02, 1.88482E-02,
     & 1.83944E-02, 1.79578E-02, 1.75378E-02, 1.71335E-02, 1.67440E-02,
     & 1.63687E-02, 1.60069E-02, 1.56579E-02, 1.53210E-02, 1.49958E-02,
     & 1.46815E-02, 1.43778E-02, 1.40841E-02, 1.37999E-02, 1.35249E-02,
     & 1.32585E-02, 1.30004E-02, 1.27502E-02/
      DATA (ABSLIQ1(I,10),I=1,58) /
c     BAND 10
     & 7.97040E-02, 7.63844E-02, 7.36499E-02, 7.13525E-02, 6.93043E-02,
     & 6.72807E-02, 6.50227E-02, 6.22395E-02, 5.86093E-02, 5.37815E-02,
     & 5.14682E-02, 4.97214E-02, 4.77392E-02, 4.56961E-02, 4.36858E-02,
     & 4.17569E-02, 3.99328E-02, 3.82224E-02, 3.66265E-02, 3.51416E-02,
     & 3.37617E-02, 3.24798E-02, 3.12887E-02, 3.01812E-02, 2.91505E-02,
     & 2.81900E-02, 2.72939E-02, 2.64568E-02, 2.54165E-02, 2.46832E-02,
     & 2.39783E-02, 2.33017E-02, 2.26531E-02, 2.20314E-02, 2.14359E-02,
     & 2.08653E-02, 2.03187E-02, 1.97947E-02, 1.92924E-02, 1.88106E-02,
     & 1.83483E-02, 1.79043E-02, 1.74778E-02, 1.70678E-02, 1.66735E-02,
     & 1.62941E-02, 1.59286E-02, 1.55766E-02, 1.52371E-02, 1.49097E-02,
     & 1.45937E-02, 1.42885E-02, 1.39936E-02, 1.37085E-02, 1.34327E-02,
     & 1.31659E-02, 1.29075E-02, 1.26571E-02/
      DATA (ABSLIQ1(I,11),I=1,58) /
c     BAND 11
     & 1.49438E-01, 1.33535E-01, 1.21542E-01, 1.11743E-01, 1.03263E-01,
     & 9.55774E-02, 8.83382E-02, 8.12943E-02, 7.42533E-02, 6.70609E-02,
     & 6.38761E-02, 5.97788E-02, 5.59841E-02, 5.25318E-02, 4.94132E-02,
     & 4.66014E-02, 4.40644E-02, 4.17706E-02, 3.96910E-02, 3.77998E-02,
     & 3.60742E-02, 3.44947E-02, 3.30442E-02, 3.17079E-02, 3.04730E-02,
     & 2.93283E-02, 2.82642E-02, 2.72720E-02, 2.61789E-02, 2.53277E-02,
     & 2.45237E-02, 2.37635E-02, 2.30438E-02, 2.23615E-02, 2.17140E-02,
     & 2.10987E-02, 2.05133E-02, 1.99557E-02, 1.94241E-02, 1.89166E-02,
     & 1.84317E-02, 1.79679E-02, 1.75238E-02, 1.70983E-02, 1.66901E-02,
     & 1.62983E-02, 1.59219E-02, 1.55599E-02, 1.52115E-02, 1.48761E-02,
     & 1.45528E-02, 1.42411E-02, 1.39402E-02, 1.36497E-02, 1.33690E-02,
     & 1.30976E-02, 1.28351E-02, 1.25810E-02/
      DATA (ABSLIQ1(I,12),I=1,58) /
c     BAND 12
     & 3.71985E-02, 3.88586E-02, 3.99070E-02, 4.04351E-02, 4.04610E-02,
     & 3.99834E-02, 3.89953E-02, 3.74886E-02, 3.54551E-02, 3.28870E-02,
     & 3.32576E-02, 3.22444E-02, 3.12384E-02, 3.02584E-02, 2.93146E-02,
     & 2.84120E-02, 2.75525E-02, 2.67361E-02, 2.59618E-02, 2.52280E-02,
     & 2.45327E-02, 2.38736E-02, 2.32487E-02, 2.26558E-02, 2.20929E-02,
     & 2.15579E-02, 2.10491E-02, 2.05648E-02, 1.99749E-02, 1.95704E-02,
     & 1.91731E-02, 1.87839E-02, 1.84032E-02, 1.80315E-02, 1.76689E-02,
     & 1.73155E-02, 1.69712E-02, 1.66362E-02, 1.63101E-02, 1.59928E-02,
     & 1.56842E-02, 1.53840E-02, 1.50920E-02, 1.48080E-02, 1.45318E-02,
     & 1.42631E-02, 1.40016E-02, 1.37472E-02, 1.34996E-02, 1.32586E-02,
     & 1.30239E-02, 1.27954E-02, 1.25728E-02, 1.23559E-02, 1.21445E-02,
     & 1.19385E-02, 1.17376E-02, 1.15417E-02/
      DATA (ABSLIQ1(I,13),I=1,58) /
c     BAND 13
     & 3.11868E-02, 4.48357E-02, 4.90224E-02, 4.96406E-02, 4.86806E-02,
     & 4.69610E-02, 4.48630E-02, 4.25795E-02, 4.02138E-02, 3.78236E-02,
     & 3.74266E-02, 3.60384E-02, 3.47074E-02, 3.34434E-02, 3.22499E-02,
     & 3.11264E-02, 3.00704E-02, 2.90784E-02, 2.81463E-02, 2.72702E-02,
     & 2.64460E-02, 2.56698E-02, 2.49381E-02, 2.42475E-02, 2.35948E-02,
     & 2.29774E-02, 2.23925E-02, 2.18379E-02, 2.11793E-02, 2.07076E-02,
     & 2.02470E-02, 1.97981E-02, 1.93613E-02, 1.89367E-02, 1.85243E-02,
     & 1.81240E-02, 1.77356E-02, 1.73588E-02, 1.69935E-02, 1.66392E-02,
     & 1.62956E-02, 1.59624E-02, 1.56393E-02, 1.53259E-02, 1.50219E-02,
     & 1.47268E-02, 1.44404E-02, 1.41624E-02, 1.38925E-02, 1.36302E-02,
     & 1.33755E-02, 1.31278E-02, 1.28871E-02, 1.26530E-02, 1.24253E-02,
     & 1.22038E-02, 1.19881E-02, 1.17782E-02/
      DATA (ABSLIQ1(I,14),I=1,58) /
c     BAND 14
     & 1.58988E-02, 3.50652E-02, 4.00851E-02, 4.07270E-02, 3.98101E-02,
     & 3.83306E-02, 3.66829E-02, 3.50327E-02, 3.34497E-02, 3.19609E-02,
     & 3.13712E-02, 3.03348E-02, 2.93415E-02, 2.83973E-02, 2.75037E-02,
     & 2.66604E-02, 2.58654E-02, 2.51161E-02, 2.44100E-02, 2.37440E-02,
     & 2.31154E-02, 2.25215E-02, 2.19599E-02, 2.14282E-02, 2.09242E-02,
     & 2.04459E-02, 1.99915E-02, 1.95594E-02, 1.90254E-02, 1.86598E-02,
     & 1.82996E-02, 1.79455E-02, 1.75983E-02, 1.72584E-02, 1.69260E-02,
     & 1.66013E-02, 1.62843E-02, 1.59752E-02, 1.56737E-02, 1.53799E-02,
     & 1.50936E-02, 1.48146E-02, 1.45429E-02, 1.42782E-02, 1.40203E-02,
     & 1.37691E-02, 1.35243E-02, 1.32858E-02, 1.30534E-02, 1.28270E-02,
     & 1.26062E-02, 1.23909E-02, 1.21810E-02, 1.19763E-02, 1.17766E-02,
     & 1.15817E-02, 1.13915E-02, 1.12058E-02/
      DATA (ABSLIQ1(I,15),I=1,58) /
c     BAND 15
     & 5.02079E-03, 2.17615E-02, 2.55449E-02, 2.59484E-02, 2.53650E-02,
     & 2.45281E-02, 2.36843E-02, 2.29159E-02, 2.22451E-02, 2.16716E-02,
     & 2.11451E-02, 2.05817E-02, 2.00454E-02, 1.95372E-02, 1.90567E-02,
     & 1.86028E-02, 1.81742E-02, 1.77693E-02, 1.73866E-02, 1.70244E-02,
     & 1.66815E-02, 1.63563E-02, 1.60477E-02, 1.57544E-02, 1.54755E-02,
     & 1.52097E-02, 1.49564E-02, 1.47146E-02, 1.43684E-02, 1.41728E-02,
     & 1.39762E-02, 1.37797E-02, 1.35838E-02, 1.33891E-02, 1.31961E-02,
     & 1.30051E-02, 1.28164E-02, 1.26302E-02, 1.24466E-02, 1.22659E-02,
     & 1.20881E-02, 1.19131E-02, 1.17412E-02, 1.15723E-02, 1.14063E-02,
     & 1.12434E-02, 1.10834E-02, 1.09264E-02, 1.07722E-02, 1.06210E-02,
     & 1.04725E-02, 1.03269E-02, 1.01839E-02, 1.00436E-02, 9.90593E-03,
     & 9.77080E-03, 9.63818E-03, 9.50800E-03/
      DATA (ABSLIQ1(I,16),I=1,58) /
c     BAND 16
     & 5.64971E-02, 9.04736E-02, 8.11726E-02, 7.05450E-02, 6.20052E-02,
     & 5.54286E-02, 5.03503E-02, 4.63791E-02, 4.32290E-02, 4.06959E-02,
     & 3.74690E-02, 3.52964E-02, 3.33799E-02, 3.16774E-02, 3.01550E-02,
     & 2.87856E-02, 2.75474E-02, 2.64223E-02, 2.53953E-02, 2.44542E-02,
     & 2.35885E-02, 2.27894E-02, 2.20494E-02, 2.13622E-02, 2.07222E-02,
     & 2.01246E-02, 1.95654E-02, 1.90408E-02, 1.84398E-02, 1.80021E-02,
     & 1.75816E-02, 1.71775E-02, 1.67889E-02, 1.64152E-02, 1.60554E-02,
     & 1.57089E-02, 1.53751E-02, 1.50531E-02, 1.47426E-02, 1.44428E-02,
     & 1.41532E-02, 1.38734E-02, 1.36028E-02, 1.33410E-02, 1.30875E-02,
     & 1.28420E-02, 1.26041E-02, 1.23735E-02, 1.21497E-02, 1.19325E-02,
     & 1.17216E-02, 1.15168E-02, 1.13177E-02, 1.11241E-02, 1.09358E-02,
     & 1.07525E-02, 1.05741E-02, 1.04003E-02/
C     Spherical Ice Particle Parameterization
C     EXTINCTION UNITS (EXT COEF/IWC): [(m^-1)/(g m^-3)]
      DATA (EXTICE2(I,1),I=1,43) /
C    BAND 1
     &1.024095e-01,8.941945e-02,8.047835e-02,7.378238e-02,6.846938e-02,
     &6.408248e-02,6.035569e-02,5.712182e-02,5.426928e-02,5.172011e-02,
     &4.941777e-02,4.731999e-02,4.539440e-02,4.361567e-02,4.196359e-02,
     &4.042182e-02,3.897697e-02,3.761791e-02,3.633530e-02,3.512124e-02,
     &3.396896e-02,3.287266e-02,3.182731e-02,3.082849e-02,2.987237e-02,
     &2.895553e-02,2.807497e-02,2.722800e-02,2.641222e-02,2.562548e-02,
     &2.486583e-02,2.413152e-02,2.342097e-02,2.273271e-02,2.206544e-02,
     &2.141794e-02,2.078911e-02,2.017793e-02,1.958346e-02,1.900483e-02,
     &1.844125e-02,1.789198e-02,1.735631e-02/
      DATA (EXTICE2(I,2),I=1,43) /
C    BAND 2
     &1.565614e-01,1.317435e-01,1.158132e-01,1.041811e-01,9.506821e-02,
     &8.760376e-02,8.129839e-02,7.585057e-02,7.106177e-02,6.679452e-02,
     &6.294987e-02,5.945427e-02,5.625161e-02,5.329815e-02,5.055916e-02,
     &4.800661e-02,4.561755e-02,4.337298e-02,4.125699e-02,3.925613e-02,
     &3.735892e-02,3.555550e-02,3.383735e-02,3.219702e-02,3.062800e-02,
     &2.912455e-02,2.768159e-02,2.629459e-02,2.495953e-02,2.367277e-02,
     &2.243106e-02,2.123145e-02,2.007127e-02,1.894810e-02,1.785972e-02,
     &1.680411e-02,1.577943e-02,1.478396e-02,1.381614e-02,1.287453e-02,
     &1.195779e-02,1.106467e-02,1.019404e-02/
      DATA (EXTICE2(I,3),I=1,43) /
C    BAND 3
     &2.968902e-01,2.116436e-01,1.670425e-01,1.388669e-01,1.191456e-01,
     &1.044167e-01,9.291190e-02,8.362511e-02,7.593776e-02,6.944668e-02,
     &6.387677e-02,5.903330e-02,5.477418e-02,5.099304e-02,4.760855e-02,
     &4.455733e-02,4.178919e-02,3.926383e-02,3.694847e-02,3.481615e-02,
     &3.284447e-02,3.101465e-02,2.931082e-02,2.771947e-02,2.622900e-02,
     &2.482941e-02,2.351202e-02,2.226926e-02,2.109450e-02,1.998187e-02,
     &1.892621e-02,1.792292e-02,1.696789e-02,1.605746e-02,1.518833e-02,
     &1.435755e-02,1.356242e-02,1.280053e-02,1.206967e-02,1.136783e-02,
     &1.069318e-02,1.004405e-02,9.418899e-03/
      DATA (EXTICE2(I,4),I=1,43) /
C    BAND 4
     &3.846587e-01,2.479981e-01,1.835502e-01,1.457513e-01,1.207923e-01,
     &1.030276e-01,8.971024e-02,7.933926e-02,7.102391e-02,6.420128e-02,
     &5.849786e-02,5.365580e-02,4.949125e-02,4.586950e-02,4.268954e-02,
     &3.987410e-02,3.736306e-02,3.510892e-02,3.307361e-02,3.122631e-02,
     &2.954173e-02,2.799898e-02,2.658062e-02,2.527196e-02,2.406056e-02,
     &2.293580e-02,2.188858e-02,2.091103e-02,1.999630e-02,1.913844e-02,
     &1.833223e-02,1.757307e-02,1.685689e-02,1.618009e-02,1.553944e-02,
     &1.493210e-02,1.435548e-02,1.380728e-02,1.328541e-02,1.278799e-02,
     &1.231331e-02,1.185983e-02,1.142613e-02/
      DATA (EXTICE2(I,5),I=1,43) /
C    BAND 5
     &3.836455e-01,2.424484e-01,1.772772e-01,1.396330e-01,1.150705e-01,
     &9.775863e-02,8.488845e-02,7.493832e-02,6.701161e-02,6.054541e-02,
     &5.516830e-02,5.062520e-02,4.673511e-02,4.336599e-02,4.041923e-02,
     &3.781967e-02,3.550906e-02,3.344150e-02,3.158034e-02,2.989599e-02,
     &2.836426e-02,2.696518e-02,2.568214e-02,2.450121e-02,2.341058e-02,
     &2.240023e-02,2.146155e-02,2.058714e-02,1.977057e-02,1.900626e-02,
     &1.828931e-02,1.761544e-02,1.698084e-02,1.638217e-02,1.581643e-02,
     &1.528097e-02,1.477340e-02,1.429159e-02,1.383361e-02,1.339773e-02,
     &1.298237e-02,1.258611e-02,1.220766e-02/
      DATA (EXTICE2(I,6),I=1,43) /
C    BAND 6
     &2.463346e-01,1.731218e-01,1.359502e-01,1.129018e-01,9.697616e-02,
     &8.519425e-02,7.605765e-02,6.872406e-02,6.268077e-02,5.759639e-02,
     &5.324640e-02,4.947285e-02,4.616112e-02,4.322579e-02,4.060185e-02,
     &3.823881e-02,3.609686e-02,3.414409e-02,3.235464e-02,3.070730e-02,
     &2.918447e-02,2.777146e-02,2.645583e-02,2.522706e-02,2.407611e-02,
     &2.299519e-02,2.197757e-02,2.101737e-02,2.010945e-02,1.924928e-02,
     &1.843286e-02,1.765663e-02,1.691744e-02,1.621245e-02,1.553914e-02,
     &1.489521e-02,1.427861e-02,1.368747e-02,1.312010e-02,1.257496e-02,
     &1.205064e-02,1.154585e-02,1.105943e-02/
      DATA (EXTICE2(I,7),I=1,43) /
C    BAND 7
     &2.332844e-01,1.753972e-01,1.432347e-01,1.220420e-01,1.067171e-01,
     &9.496334e-02,8.557395e-02,7.784593e-02,7.133827e-02,6.575843e-02,
     &6.090362e-02,5.662826e-02,5.282472e-02,4.941149e-02,4.632555e-02,
     &4.351730e-02,4.094709e-02,3.858278e-02,3.639802e-02,3.437096e-02,
     &3.248331e-02,3.071964e-02,2.906680e-02,2.751353e-02,2.605011e-02,
     &2.466811e-02,2.336015e-02,2.211980e-02,2.094133e-02,1.981973e-02,
     &1.875050e-02,1.772963e-02,1.675356e-02,1.581905e-02,1.492320e-02,
     &1.406338e-02,1.323721e-02,1.244252e-02,1.167733e-02,1.093985e-02,
     &1.022841e-02,9.541504e-03,8.877727e-03/
      DATA (EXTICE2(I,8),I=1,43) /
C    BAND 8
     &3.163919e-01,2.164187e-01,1.662972e-01,1.356062e-01,1.146515e-01,
     &9.932297e-02,8.756212e-02,7.821700e-02,7.058960e-02,6.423083e-02,
     &5.883780e-02,5.419837e-02,5.015924e-02,4.660673e-02,4.345460e-02,
     &4.063621e-02,3.809917e-02,3.580171e-02,3.371008e-02,3.179670e-02,
     &3.003880e-02,2.841738e-02,2.691650e-02,2.552264e-02,2.422427e-02,
     &2.301149e-02,2.187574e-02,2.080962e-02,1.980662e-02,1.886106e-02,
     &1.796794e-02,1.712282e-02,1.632176e-02,1.556126e-02,1.483817e-02,
     &1.414969e-02,1.349327e-02,1.286663e-02,1.226769e-02,1.169458e-02,
     &1.114559e-02,1.061916e-02,1.011386e-02/
      DATA (EXTICE2(I,9),I=1,43) /
C    BAND 9
     &3.936120e-01,2.470191e-01,1.797882e-01,1.411182e-01,1.159652e-01,
     &9.828077e-02,8.516020e-02,7.503375e-02,6.697841e-02,6.041569e-02,
     &5.496451e-02,5.036352e-02,4.642748e-02,4.302142e-02,4.004462e-02,
     &3.742042e-02,3.508943e-02,3.300489e-02,3.112952e-02,2.943321e-02,
     &2.789136e-02,2.648370e-02,2.519337e-02,2.400622e-02,2.291028e-02,
     &2.189540e-02,2.095285e-02,2.007513e-02,1.925574e-02,1.848903e-02,
     &1.777005e-02,1.709446e-02,1.645842e-02,1.585854e-02,1.529181e-02,
     &1.475555e-02,1.424733e-02,1.376502e-02,1.330667e-02,1.287053e-02,
     &1.245501e-02,1.205867e-02,1.168022e-02/
      DATA (EXTICE2(I,10),I=1,43) /
C    BAND 10
     &4.254883e-01,2.572374e-01,1.828685e-01,1.411963e-01,1.146376e-01,
     &9.627516e-02,8.284435e-02,7.260635e-02,6.455138e-02,5.805351e-02,
     &5.270441e-02,4.822656e-02,4.442483e-02,4.115808e-02,3.832175e-02,
     &3.583677e-02,3.364222e-02,3.169044e-02,2.994361e-02,2.837136e-02,
     &2.694900e-02,2.565626e-02,2.447637e-02,2.339529e-02,2.240124e-02,
     &2.148420e-02,2.063566e-02,1.984827e-02,1.911574e-02,1.843257e-02,
     &1.779398e-02,1.719579e-02,1.663432e-02,1.610633e-02,1.560893e-02,
     &1.513957e-02,1.469597e-02,1.427608e-02,1.387807e-02,1.350029e-02,
     &1.314125e-02,1.279961e-02,1.247414e-02/
      DATA (EXTICE2(I,11),I=1,43) /
C    BAND 11
     &4.395814e-01,2.605045e-01,1.829231e-01,1.400554e-01,1.130281e-01,
     &9.450563e-02,8.105812e-02,7.087317e-02,6.290529e-02,5.651022e-02,
     &5.126987e-02,4.690139e-02,4.320677e-02,4.004337e-02,3.730588e-02,
     &3.491492e-02,3.280956e-02,3.094223e-02,2.927534e-02,2.777872e-02,
     &2.642795e-02,2.520302e-02,2.408740e-02,2.306730e-02,2.213115e-02,
     &2.126916e-02,2.047299e-02,1.973550e-02,1.905053e-02,1.841276e-02,
     &1.781754e-02,1.726083e-02,1.673906e-02,1.624911e-02,1.578818e-02,
     &1.535382e-02,1.494384e-02,1.455627e-02,1.418935e-02,1.384150e-02,
     &1.351130e-02,1.319746e-02,1.289882e-02/
      DATA (EXTICE2(I,12),I=1,43) /
C    BAND 12
     &5.011192e-01,2.791706e-01,1.886085e-01,1.406223e-01,1.113326e-01,
     &9.178395e-02,7.790561e-02,6.759635e-02,5.966820e-02,5.340185e-02,
     &4.833776e-02,4.416940e-02,4.068494e-02,3.773356e-02,3.520513e-02,
     &3.301747e-02,3.110810e-02,2.942870e-02,2.794136e-02,2.661592e-02,
     &2.542815e-02,2.435834e-02,2.339031e-02,2.251066e-02,2.170821e-02,
     &2.097355e-02,2.029873e-02,1.967696e-02,1.910244e-02,1.857014e-02,
     &1.807575e-02,1.761548e-02,1.718604e-02,1.678454e-02,1.640844e-02,
     &1.605547e-02,1.572364e-02,1.541118e-02,1.511649e-02,1.483815e-02,
     &1.457488e-02,1.432555e-02,1.408910e-02/
      DATA (EXTICE2(I,13),I=1,43) /
C    BAND 13
     &4.923918e-01,2.742336e-01,1.852807e-01,1.381734e-01,1.094335e-01,
     &9.025950e-02,7.665201e-02,6.654720e-02,5.877857e-02,5.263995e-02,
     &4.768033e-02,4.359892e-02,4.018790e-02,3.729932e-02,3.482517e-02,
     &3.268489e-02,3.081720e-02,2.917473e-02,2.772033e-02,2.642446e-02,
     &2.526337e-02,2.421773e-02,2.327170e-02,2.241216e-02,2.162816e-02,
     &2.091049e-02,2.025135e-02,1.964410e-02,1.908305e-02,1.856331e-02,
     &1.808062e-02,1.763130e-02,1.721212e-02,1.682026e-02,1.645321e-02,
     &1.610878e-02,1.578501e-02,1.548016e-02,1.519267e-02,1.492116e-02,
     &1.466438e-02,1.442121e-02,1.419062e-02/
      DATA (EXTICE2(I,14),I=1,43) /
C    BAND 14
     &4.869234e-01,2.718080e-01,1.839165e-01,1.373055e-01,1.088377e-01,
     &8.982848e-02,7.632799e-02,6.629626e-02,5.857948e-02,5.247880e-02,
     &4.754761e-02,4.348792e-02,4.009378e-02,3.721849e-02,3.475494e-02,
     &3.262317e-02,3.076238e-02,2.912556e-02,2.767578e-02,2.638373e-02,
     &2.522579e-02,2.418276e-02,2.323891e-02,2.238118e-02,2.159868e-02,
     &2.088225e-02,2.022413e-02,1.961773e-02,1.905737e-02,1.853819e-02,
     &1.805595e-02,1.760698e-02,1.718807e-02,1.679640e-02,1.642949e-02,
     &1.608514e-02,1.576140e-02,1.545655e-02,1.516903e-02,1.489746e-02,
     &1.464059e-02,1.439730e-02,1.416658e-02/
      DATA (EXTICE2(I,15),I=1,43) /
C    BAND 15
     &4.712597e-01,2.658087e-01,1.810589e-01,1.358108e-01,1.080308e-01,
     &8.940151e-02,7.612280e-02,6.622477e-02,5.858962e-02,5.253839e-02,
     &4.763608e-02,4.359183e-02,4.020412e-02,3.732921e-02,3.486192e-02,
     &3.272361e-02,3.085441e-02,2.920791e-02,2.774769e-02,2.644471e-02,
     &2.527560e-02,2.422135e-02,2.326630e-02,2.239751e-02,2.160413e-02,
     &2.087706e-02,2.020855e-02,1.959204e-02,1.902185e-02,1.849312e-02,
     &1.800163e-02,1.754369e-02,1.711609e-02,1.671601e-02,1.634095e-02,
     &1.598871e-02,1.565735e-02,1.534510e-02,1.505042e-02,1.477192e-02,
     &1.450833e-02,1.425854e-02,1.402151e-02/
      DATA (EXTICE2(I,16),I=1,43) /
C    BAND 16
     &4.101824e-01,2.435514e-01,1.713697e-01,1.314865e-01,1.063406e-01,
     &8.910701e-02,7.659480e-02,6.711784e-02,5.970353e-02,5.375249e-02,
     &4.887577e-02,4.481025e-02,4.137171e-02,3.842744e-02,3.587948e-02,
     &3.365396e-02,3.169419e-02,2.995593e-02,2.840419e-02,2.701091e-02,
     &2.575336e-02,2.461293e-02,2.357423e-02,2.262443e-02,2.175276e-02,
     &2.095012e-02,2.020875e-02,1.952199e-02,1.888412e-02,1.829018e-02,
     &1.773586e-02,1.721738e-02,1.673144e-02,1.627510e-02,1.584579e-02,
     &1.544122e-02,1.505934e-02,1.469833e-02,1.435654e-02,1.403251e-02,
     &1.372492e-02,1.343255e-02,1.315433e-02/
C     SINGLE-SCATTERING ALBEDO: Unitless
      DATA (SSAICE2(I,1),I=1,43) /
C    BAND 1
     &2.384497e-01,2.909285e-01,3.267788e-01,3.540130e-01,3.759747e-01,
     &3.943837e-01,4.102388e-01,4.241709e-01,4.366037e-01,4.478358e-01,
     &4.580852e-01,4.675163e-01,4.762559e-01,4.844041e-01,4.920411e-01,
     &4.992324e-01,5.060323e-01,5.124859e-01,5.186316e-01,5.245023e-01,
     &5.301260e-01,5.355275e-01,5.407281e-01,5.457469e-01,5.506008e-01,
     &5.553048e-01,5.598726e-01,5.643165e-01,5.686477e-01,5.728765e-01,
     &5.770124e-01,5.810643e-01,5.850404e-01,5.889484e-01,5.927958e-01,
     &5.965895e-01,6.003362e-01,6.040423e-01,6.077142e-01,6.113580e-01,
     &6.149798e-01,6.185855e-01,6.221813e-01/
      DATA (SSAICE2(I,2),I=1,43) /
C    BAND 2
     &8.221222e-01,7.943076e-01,7.738458e-01,7.572275e-01,7.430030e-01,
     &7.304254e-01,7.190571e-01,7.086179e-01,6.989172e-01,6.898189e-01,
     &6.812222e-01,6.730501e-01,6.652427e-01,6.577517e-01,6.505382e-01,
     &6.435701e-01,6.368202e-01,6.302659e-01,6.238876e-01,6.176684e-01,
     &6.115934e-01,6.056497e-01,5.998256e-01,5.941108e-01,5.884958e-01,
     &5.829720e-01,5.775313e-01,5.721662e-01,5.668698e-01,5.616352e-01,
     &5.564557e-01,5.513249e-01,5.462362e-01,5.411828e-01,5.361576e-01,
     &5.311530e-01,5.261609e-01,5.211719e-01,5.161756e-01,5.111597e-01,
     &5.061093e-01,5.010065e-01,4.958288e-01/
      DATA (SSAICE2(I,3),I=1,43) /
C    BAND 3
     &6.411479e-01,6.217354e-01,6.080982e-01,5.975189e-01,5.888491e-01,
     &5.814910e-01,5.750920e-01,5.694265e-01,5.643407e-01,5.597253e-01,
     &5.554996e-01,5.516022e-01,5.479855e-01,5.446115e-01,5.414499e-01,
     &5.384755e-01,5.356676e-01,5.330090e-01,5.304849e-01,5.280827e-01,
     &5.257918e-01,5.236027e-01,5.215074e-01,5.194988e-01,5.175707e-01,
     &5.157175e-01,5.139344e-01,5.122171e-01,5.105619e-01,5.089654e-01,
     &5.074245e-01,5.059368e-01,5.044998e-01,5.031117e-01,5.017706e-01,
     &5.004753e-01,4.992246e-01,4.980175e-01,4.968537e-01,4.957327e-01,
     &4.946547e-01,4.936202e-01,4.926301e-01/
      DATA (SSAICE2(I,4),I=1,43) /
C    BAND 4
     &5.308658e-01,5.286308e-01,5.270808e-01,5.259007e-01,5.249552e-01,
     &5.241732e-01,5.235120e-01,5.229445e-01,5.224518e-01,5.220204e-01,
     &5.216405e-01,5.213044e-01,5.210062e-01,5.207411e-01,5.205054e-01,
     &5.202958e-01,5.201099e-01,5.199454e-01,5.198004e-01,5.196735e-01,
     &5.195632e-01,5.194684e-01,5.193881e-01,5.193214e-01,5.192675e-01,
     &5.192259e-01,5.191958e-01,5.191767e-01,5.191683e-01,5.191702e-01,
     &5.191819e-01,5.192032e-01,5.192338e-01,5.192736e-01,5.193222e-01,
     &5.193797e-01,5.194458e-01,5.195204e-01,5.196036e-01,5.196951e-01,
     &5.197952e-01,5.199036e-01,5.200205e-01/
      DATA (SSAICE2(I,5),I=1,43) /
C    BAND 5
     &4.443291e-01,4.591129e-01,4.693524e-01,4.772410e-01,4.836841e-01,
     &4.891456e-01,4.938956e-01,4.981055e-01,5.018908e-01,5.053336e-01,
     &5.084939e-01,5.114170e-01,5.141381e-01,5.166852e-01,5.190805e-01,
     &5.213424e-01,5.234860e-01,5.255239e-01,5.274670e-01,5.293243e-01,
     &5.311038e-01,5.328121e-01,5.344554e-01,5.360388e-01,5.375669e-01,
     &5.390438e-01,5.404732e-01,5.418583e-01,5.432020e-01,5.445071e-01,
     &5.457758e-01,5.470105e-01,5.482129e-01,5.493851e-01,5.505286e-01,
     &5.516449e-01,5.527355e-01,5.538017e-01,5.548446e-01,5.558653e-01,
     &5.568650e-01,5.578445e-01,5.588047e-01/
      DATA (SSAICE2(I,6),I=1,43) /
C    BAND 6
     &3.723265e-01,3.996998e-01,4.181439e-01,4.320349e-01,4.431625e-01,
     &4.524352e-01,4.603781e-01,4.673218e-01,4.734879e-01,4.790323e-01,
     &4.840687e-01,4.886825e-01,4.929397e-01,4.968922e-01,5.005815e-01,
     &5.040415e-01,5.073000e-01,5.103805e-01,5.133026e-01,5.160831e-01,
     &5.187363e-01,5.212749e-01,5.237097e-01,5.260504e-01,5.283055e-01,
     &5.304825e-01,5.325884e-01,5.346293e-01,5.366107e-01,5.385378e-01,
     &5.404153e-01,5.422477e-01,5.440388e-01,5.457926e-01,5.475127e-01,
     &5.492024e-01,5.508652e-01,5.525042e-01,5.541224e-01,5.557230e-01,
     &5.573090e-01,5.588835e-01,5.604495e-01/
      DATA (SSAICE2(I,7),I=1,43) /
C    BAND 7
     &6.749288e-01,6.475680e-01,6.291381e-01,6.152112e-01,6.040010e-01,
     &5.946084e-01,5.865167e-01,5.794023e-01,5.730486e-01,5.673037e-01,
     &5.620572e-01,5.572259e-01,5.527461e-01,5.485674e-01,5.446498e-01,
     &5.409604e-01,5.374723e-01,5.341632e-01,5.310141e-01,5.280089e-01,
     &5.251338e-01,5.223770e-01,5.197280e-01,5.171778e-01,5.147184e-01,
     &5.123427e-01,5.100445e-01,5.078180e-01,5.056582e-01,5.035605e-01,
     &5.015208e-01,4.995353e-01,4.976005e-01,4.957133e-01,4.938707e-01,
     &4.920701e-01,4.903088e-01,4.885844e-01,4.868948e-01,4.852377e-01,
     &4.836110e-01,4.820127e-01,4.804408e-01/
      DATA (SSAICE2(I,8),I=1,43) /
C    BAND 8
     &7.148414e-01,6.801247e-01,6.565983e-01,6.387794e-01,6.244318e-01,
     &6.124207e-01,6.020902e-01,5.930271e-01,5.849538e-01,5.776752e-01,
     &5.710486e-01,5.649666e-01,5.593463e-01,5.541225e-01,5.492428e-01,
     &5.446646e-01,5.403528e-01,5.362779e-01,5.324151e-01,5.287436e-01,
     &5.252450e-01,5.219039e-01,5.187066e-01,5.156411e-01,5.126970e-01,
     &5.098650e-01,5.071367e-01,5.045048e-01,5.019627e-01,4.995044e-01,
     &4.971244e-01,4.948179e-01,4.925804e-01,4.904078e-01,4.882964e-01,
     &4.862427e-01,4.842437e-01,4.822964e-01,4.803980e-01,4.785462e-01,
     &4.767386e-01,4.749730e-01,4.732474e-01/
      DATA (SSAICE2(I,9),I=1,43) /
C    BAND 9
     &6.712279e-01,6.442293e-01,6.257659e-01,6.116928e-01,6.003067e-01,
     &5.907386e-01,5.824839e-01,5.752233e-01,5.687419e-01,5.628879e-01,
     &5.575502e-01,5.526449e-01,5.481072e-01,5.438859e-01,5.399399e-01,
     &5.362355e-01,5.327451e-01,5.294455e-01,5.263172e-01,5.233434e-01,
     &5.205098e-01,5.178041e-01,5.152154e-01,5.127343e-01,5.103524e-01,
     &5.080623e-01,5.058575e-01,5.037321e-01,5.016807e-01,4.996987e-01,
     &4.977816e-01,4.959258e-01,4.941275e-01,4.923836e-01,4.906910e-01,
     &4.890472e-01,4.874496e-01,4.858958e-01,4.843839e-01,4.829118e-01,
     &4.814778e-01,4.800801e-01,4.787172e-01/
      DATA (SSAICE2(I,10),I=1,43) /
C    BAND 10
     &6.254590e-01,6.055970e-01,5.921137e-01,5.818892e-01,5.736492e-01,
     &5.667460e-01,5.608054e-01,5.555910e-01,5.509442e-01,5.467533e-01,
     &5.429368e-01,5.394330e-01,5.361945e-01,5.331840e-01,5.303713e-01,
     &5.277321e-01,5.252462e-01,5.228967e-01,5.206694e-01,5.185522e-01,
     &5.165348e-01,5.146082e-01,5.127644e-01,5.109968e-01,5.092993e-01,
     &5.076665e-01,5.060936e-01,5.045765e-01,5.031113e-01,5.016946e-01,
     &5.003232e-01,4.989945e-01,4.977057e-01,4.964546e-01,4.952390e-01,
     &4.940570e-01,4.929068e-01,4.917867e-01,4.906952e-01,4.896308e-01,
     &4.885923e-01,4.875785e-01,4.865881e-01/
      DATA (SSAICE2(I,11),I=1,43) /
C    BAND 11
     &6.232263e-01,6.037961e-01,5.906828e-01,5.807779e-01,5.728182e-01,
     &5.661645e-01,5.604483e-01,5.554375e-01,5.509768e-01,5.469570e-01,
     &5.432984e-01,5.399411e-01,5.368390e-01,5.339556e-01,5.312619e-01,
     &5.287341e-01,5.263529e-01,5.241018e-01,5.219671e-01,5.199373e-01,
     &5.180022e-01,5.161534e-01,5.143831e-01,5.126849e-01,5.110531e-01,
     &5.094823e-01,5.079682e-01,5.065066e-01,5.050939e-01,5.037269e-01,
     &5.024025e-01,5.011181e-01,4.998713e-01,4.986598e-01,4.974815e-01,
     &4.963348e-01,4.952177e-01,4.941288e-01,4.930666e-01,4.920297e-01,
     &4.910170e-01,4.900272e-01,4.890593e-01/
      DATA (SSAICE2(I,12),I=1,43) /
C    BAND 12
     &8.165189e-01,7.690707e-01,7.369135e-01,7.125590e-01,6.929511e-01,
     &6.765386e-01,6.624248e-01,6.500445e-01,6.390180e-01,6.290783e-01,
     &6.200302e-01,6.117269e-01,6.040549e-01,5.969249e-01,5.902653e-01,
     &5.840177e-01,5.781341e-01,5.725743e-01,5.673045e-01,5.622957e-01,
     &5.575233e-01,5.529660e-01,5.486050e-01,5.444242e-01,5.404091e-01,
     &5.365471e-01,5.328269e-01,5.292383e-01,5.257724e-01,5.224210e-01,
     &5.191768e-01,5.160329e-01,5.129835e-01,5.100229e-01,5.071461e-01,
     &5.043485e-01,5.016258e-01,4.989740e-01,4.963895e-01,4.938691e-01,
     &4.914094e-01,4.890078e-01,4.866614e-01/
      DATA (SSAICE2(I,13),I=1,43) /
C    BAND 13
     &7.081550e-01,6.764607e-01,6.549873e-01,6.387269e-01,6.256366e-01,
     &6.146798e-01,6.052573e-01,5.969915e-01,5.896290e-01,5.829913e-01,
     &5.769484e-01,5.714020e-01,5.662765e-01,5.615123e-01,5.570617e-01,
     &5.528856e-01,5.489522e-01,5.452345e-01,5.417100e-01,5.383595e-01,
     &5.351665e-01,5.321168e-01,5.291979e-01,5.263991e-01,5.237107e-01,
     &5.211244e-01,5.186325e-01,5.162284e-01,5.139061e-01,5.116601e-01,
     &5.094855e-01,5.073778e-01,5.053332e-01,5.033477e-01,5.014182e-01,
     &4.995414e-01,4.977147e-01,4.959352e-01,4.942007e-01,4.925089e-01,
     &4.908577e-01,4.892452e-01,4.876697e-01/
      DATA (SSAICE2(I,14),I=1,43) /
C    BAND 14
     &7.353033e-01,6.997772e-01,6.756819e-01,6.574216e-01,6.427122e-01,
     &6.303940e-01,6.197965e-01,6.104970e-01,6.022115e-01,5.947404e-01,
     &5.879375e-01,5.816930e-01,5.759219e-01,5.705575e-01,5.655461e-01,
     &5.608441e-01,5.564153e-01,5.522298e-01,5.482621e-01,5.444907e-01,
     &5.408970e-01,5.374650e-01,5.341808e-01,5.310321e-01,5.280082e-01,
     &5.250995e-01,5.222976e-01,5.195949e-01,5.169845e-01,5.144605e-01,
     &5.120172e-01,5.096496e-01,5.073532e-01,5.051238e-01,5.029576e-01,
     &5.008511e-01,4.988011e-01,4.968047e-01,4.948591e-01,4.929617e-01,
     &4.911103e-01,4.893027e-01,4.875368e-01/
      DATA (SSAICE2(I,15),I=1,43) /
C    BAND 15
     &8.248475e-01,7.814674e-01,7.518947e-01,7.294008e-01,7.112284e-01,
     &6.959732e-01,6.828218e-01,6.712600e-01,6.609423e-01,6.516250e-01,
     &6.431297e-01,6.353220e-01,6.280981e-01,6.213760e-01,6.150901e-01,
     &6.091866e-01,6.036215e-01,5.983576e-01,5.933637e-01,5.886134e-01,
     &5.840837e-01,5.797549e-01,5.756098e-01,5.716333e-01,5.678121e-01,
     &5.641345e-01,5.605899e-01,5.571691e-01,5.538635e-01,5.506656e-01,
     &5.475687e-01,5.445663e-01,5.416530e-01,5.388234e-01,5.360730e-01,
     &5.333974e-01,5.307925e-01,5.282548e-01,5.257807e-01,5.233673e-01,
     &5.210114e-01,5.187106e-01,5.164621e-01/
      DATA (SSAICE2(I,16),I=1,43) /
C    BAND 16
     &6.630615e-01,6.451169e-01,6.333696e-01,6.246927e-01,6.178420e-01,
     &6.121976e-01,6.074069e-01,6.032505e-01,5.995830e-01,5.963030e-01,
     &5.933372e-01,5.906311e-01,5.881427e-01,5.858395e-01,5.836955e-01,
     &5.816896e-01,5.798046e-01,5.780264e-01,5.763429e-01,5.747441e-01,
     &5.732213e-01,5.717672e-01,5.703754e-01,5.690403e-01,5.677571e-01,
     &5.665215e-01,5.653297e-01,5.641782e-01,5.630643e-01,5.619850e-01,
     &5.609381e-01,5.599214e-01,5.589328e-01,5.579707e-01,5.570333e-01,
     &5.561193e-01,5.552272e-01,5.543558e-01,5.535041e-01,5.526708e-01,
     &5.518551e-01,5.510561e-01,5.502729e-01/
C     ASYMMETRY FACTOR: Unitless
      DATA (ASYICE2(I,1),I=1,43) /
C    BAND 1
     &2.255639e-01,4.645627e-01,5.756219e-01,6.411863e-01,6.850450e-01,
     &7.167515e-01,7.409211e-01,7.600705e-01,7.756950e-01,7.887423e-01,
     &7.998436e-01,8.094368e-01,8.178357e-01,8.252715e-01,8.319187e-01,
     &8.379115e-01,8.433551e-01,8.483330e-01,8.529127e-01,8.571493e-01,
     &8.610881e-01,8.647670e-01,8.682179e-01,8.714677e-01,8.745396e-01,
     &8.774535e-01,8.802267e-01,8.828741e-01,8.854091e-01,8.878434e-01,
     &8.901874e-01,8.924504e-01,8.946407e-01,8.967659e-01,8.988330e-01,
     &9.008482e-01,9.028173e-01,9.047457e-01,9.066384e-01,9.085002e-01,
     &9.103353e-01,9.121481e-01,9.139426e-01/
      DATA (ASYICE2(I,2),I=1,43) /
C    BAND 2
     &5.393286e-01,6.558766e-01,7.164199e-01,7.545486e-01,7.811948e-01,
     &8.010778e-01,8.165998e-01,8.291243e-01,8.394884e-01,8.482370e-01,
     &8.557418e-01,8.622656e-01,8.680002e-01,8.730891e-01,8.776420e-01,
     &8.817442e-01,8.854634e-01,8.888538e-01,8.919594e-01,8.948166e-01,
     &8.974552e-01,8.999005e-01,9.021735e-01,9.042922e-01,9.062719e-01,
     &9.081256e-01,9.098647e-01,9.114988e-01,9.130362e-01,9.144842e-01,
     &9.158490e-01,9.171357e-01,9.183488e-01,9.194918e-01,9.205677e-01,
     &9.215785e-01,9.225256e-01,9.234093e-01,9.242292e-01,9.249837e-01,
     &9.256698e-01,9.262828e-01,9.268157e-01/
      DATA (ASYICE2(I,3),I=1,43) /
C    BAND 3
     &6.402550e-01,7.366100e-01,7.861283e-01,8.170823e-01,8.385946e-01,
     &8.545773e-01,8.670110e-01,8.770146e-01,8.852724e-01,8.922285e-01,
     &8.981848e-01,9.033542e-01,9.078918e-01,9.119133e-01,9.155069e-01,
     &9.187415e-01,9.216713e-01,9.243398e-01,9.267824e-01,9.290280e-01,
     &9.311009e-01,9.330210e-01,9.348055e-01,9.364687e-01,9.380231e-01,
     &9.394792e-01,9.408463e-01,9.421324e-01,9.433444e-01,9.444886e-01,
     &9.455702e-01,9.465940e-01,9.475642e-01,9.484844e-01,9.493581e-01,
     &9.501880e-01,9.509766e-01,9.517263e-01,9.524388e-01,9.531158e-01,
     &9.537587e-01,9.543686e-01,9.549462e-01/
      DATA (ASYICE2(I,4),I=1,43) /
C    BAND 4
     &6.868425e-01,7.885874e-01,8.342997e-01,8.602518e-01,8.769749e-01,
     &8.886475e-01,8.972569e-01,9.038686e-01,9.091054e-01,9.133553e-01,
     &9.168731e-01,9.198324e-01,9.223563e-01,9.245338e-01,9.264314e-01,
     &9.280994e-01,9.295769e-01,9.308944e-01,9.320762e-01,9.331421e-01,
     &9.341079e-01,9.349870e-01,9.357901e-01,9.365266e-01,9.372040e-01,
     &9.378290e-01,9.384073e-01,9.389435e-01,9.394420e-01,9.399062e-01,
     &9.403395e-01,9.407445e-01,9.411237e-01,9.414794e-01,9.418132e-01,
     &9.421271e-01,9.424225e-01,9.427007e-01,9.429630e-01,9.432104e-01,
     &9.434440e-01,9.436646e-01,9.438730e-01/
      DATA (ASYICE2(I,5),I=1,43) /
C    BAND 5
     &7.273309e-01,8.266992e-01,8.655715e-01,8.855658e-01,8.974914e-01,
     &9.053017e-01,9.107583e-01,9.147555e-01,9.177919e-01,9.201655e-01,
     &9.220647e-01,9.236137e-01,9.248978e-01,9.259770e-01,9.268948e-01,
     &9.276835e-01,9.283676e-01,9.289656e-01,9.294923e-01,9.299591e-01,
     &9.303752e-01,9.307482e-01,9.310841e-01,9.313880e-01,9.316640e-01,
     &9.319156e-01,9.321458e-01,9.323571e-01,9.325516e-01,9.327311e-01,
     &9.328973e-01,9.330515e-01,9.331948e-01,9.333284e-01,9.334532e-01,
     &9.335698e-01,9.336792e-01,9.337819e-01,9.338784e-01,9.339694e-01,
     &9.340551e-01,9.341361e-01,9.342127e-01/
      DATA (ASYICE2(I,6),I=1,43) /
C    BAND 6
     &7.887503e-01,8.770704e-01,9.104538e-01,9.272855e-01,9.371982e-01,
     &9.436352e-01,9.481052e-01,9.513645e-01,9.538305e-01,9.557506e-01,
     &9.572803e-01,9.585216e-01,9.595441e-01,9.603968e-01,9.611150e-01,
     &9.617249e-01,9.622461e-01,9.626938e-01,9.630796e-01,9.634129e-01,
     &9.637009e-01,9.639497e-01,9.641639e-01,9.643477e-01,9.645041e-01,
     &9.646359e-01,9.647453e-01,9.648341e-01,9.649039e-01,9.649559e-01,
     &9.649912e-01,9.650105e-01,9.650147e-01,9.650041e-01,9.649792e-01,
     &9.649403e-01,9.648876e-01,9.648210e-01,9.647406e-01,9.646461e-01,
     &9.645374e-01,9.644141e-01,9.642758e-01/
      DATA (ASYICE2(I,7),I=1,43) /
C    BAND 7
     &8.378017e-01,8.852928e-01,9.090067e-01,9.234678e-01,9.333026e-01,
     &9.404700e-01,9.459500e-01,9.502900e-01,9.538212e-01,9.567564e-01,
     &9.592387e-01,9.613686e-01,9.632181e-01,9.648409e-01,9.662775e-01,
     &9.675592e-01,9.687106e-01,9.697512e-01,9.706969e-01,9.715604e-01,
     &9.723525e-01,9.730820e-01,9.737564e-01,9.743818e-01,9.749637e-01,
     &9.755068e-01,9.760148e-01,9.764914e-01,9.769395e-01,9.773618e-01,
     &9.777606e-01,9.781380e-01,9.784958e-01,9.788357e-01,9.791591e-01,
     &9.794674e-01,9.797619e-01,9.800437e-01,9.803137e-01,9.805730e-01,
     &9.808224e-01,9.810629e-01,9.812952e-01/
      DATA (ASYICE2(I,8),I=1,43) /
C    BAND 8
     &8.410085e-01,8.742948e-01,8.935566e-01,9.065956e-01,9.162142e-01,
     &9.237079e-01,9.297715e-01,9.348164e-01,9.391043e-01,9.428109e-01,
     &9.460592e-01,9.489383e-01,9.515147e-01,9.538391e-01,9.559510e-01,
     &9.578816e-01,9.596561e-01,9.612949e-01,9.628148e-01,9.642301e-01,
     &9.655524e-01,9.667918e-01,9.679568e-01,9.690549e-01,9.700923e-01,
     &9.710747e-01,9.720070e-01,9.728933e-01,9.737376e-01,9.745431e-01,
     &9.753129e-01,9.760497e-01,9.767558e-01,9.774336e-01,9.780849e-01,
     &9.787115e-01,9.793151e-01,9.798972e-01,9.804592e-01,9.810022e-01,
     &9.815276e-01,9.820363e-01,9.825294e-01/
      DATA (ASYICE2(I,9),I=1,43) /
C    BAND 9
     &8.447005e-01,8.780132e-01,8.968896e-01,9.095104e-01,9.187442e-01,
     &9.258958e-01,9.316569e-01,9.364336e-01,9.404822e-01,9.439738e-01,
     &9.470275e-01,9.497295e-01,9.521436e-01,9.543184e-01,9.562917e-01,
     &9.580932e-01,9.597468e-01,9.612720e-01,9.626849e-01,9.639986e-01,
     &9.652243e-01,9.663716e-01,9.674484e-01,9.684618e-01,9.694176e-01,
     &9.703212e-01,9.711770e-01,9.719892e-01,9.727611e-01,9.734961e-01,
     &9.741968e-01,9.748658e-01,9.755053e-01,9.761174e-01,9.767038e-01,
     &9.772662e-01,9.778062e-01,9.783250e-01,9.788239e-01,9.793041e-01,
     &9.797666e-01,9.802124e-01,9.806423e-01/
      DATA (ASYICE2(I,10),I=1,43) /
C    BAND 10
     &8.564947e-01,8.915851e-01,9.102676e-01,9.221993e-01,9.306152e-01,
     &9.369369e-01,9.418971e-01,9.459156e-01,9.492519e-01,9.520761e-01,
     &9.545046e-01,9.566202e-01,9.584835e-01,9.601400e-01,9.616244e-01,
     &9.629641e-01,9.641805e-01,9.652912e-01,9.663102e-01,9.672493e-01,
     &9.681181e-01,9.689247e-01,9.696762e-01,9.703783e-01,9.710361e-01,
     &9.716540e-01,9.722357e-01,9.727846e-01,9.733035e-01,9.737950e-01,
     &9.742615e-01,9.747048e-01,9.751268e-01,9.755291e-01,9.759132e-01,
     &9.762803e-01,9.766317e-01,9.769684e-01,9.772913e-01,9.776014e-01,
     &9.778994e-01,9.781862e-01,9.784623e-01/
      DATA (ASYICE2(I,11),I=1,43) /
C    BAND 11
     &8.697900e-01,9.013810e-01,9.181518e-01,9.288524e-01,9.363985e-01,
     &9.420679e-01,9.465182e-01,9.501255e-01,9.531224e-01,9.556611e-01,
     &9.578459e-01,9.597507e-01,9.614299e-01,9.629240e-01,9.642643e-01,
     &9.654751e-01,9.665758e-01,9.675818e-01,9.685059e-01,9.693585e-01,
     &9.701484e-01,9.708827e-01,9.715676e-01,9.722085e-01,9.728097e-01,
     &9.733753e-01,9.739087e-01,9.744127e-01,9.748899e-01,9.753428e-01,
     &9.757732e-01,9.761830e-01,9.765738e-01,9.769470e-01,9.773040e-01,
     &9.776459e-01,9.779737e-01,9.782883e-01,9.785908e-01,9.788818e-01,
     &9.791621e-01,9.794323e-01,9.796931e-01/
      DATA (ASYICE2(I,12),I=1,43) /
C    BAND 12
     &8.283310e-01,8.543591e-01,8.712236e-01,8.835970e-01,8.933168e-01,
     &9.012909e-01,9.080327e-01,9.138601e-01,9.189835e-01,9.235486e-01,
     &9.276609e-01,9.313988e-01,9.348222e-01,9.379780e-01,9.409034e-01,
     &9.436284e-01,9.461776e-01,9.485715e-01,9.508272e-01,9.529591e-01,
     &9.549796e-01,9.568992e-01,9.587273e-01,9.604716e-01,9.621394e-01,
     &9.637367e-01,9.652691e-01,9.667413e-01,9.681578e-01,9.695225e-01,
     &9.708388e-01,9.721100e-01,9.733388e-01,9.745280e-01,9.756799e-01,
     &9.767967e-01,9.778803e-01,9.789326e-01,9.799553e-01,9.809499e-01,
     &9.819179e-01,9.828606e-01,9.837793e-01/
      DATA (ASYICE2(I,13),I=1,43) /
C    BAND 13
     &8.306222e-01,8.696458e-01,8.911483e-01,9.052375e-01,9.153823e-01,
     &9.231361e-01,9.293121e-01,9.343824e-01,9.386425e-01,9.422880e-01,
     &9.454540e-01,9.482376e-01,9.507103e-01,9.529263e-01,9.549272e-01,
     &9.567459e-01,9.584087e-01,9.599368e-01,9.613475e-01,9.626553e-01,
     &9.638721e-01,9.650082e-01,9.660721e-01,9.670713e-01,9.680121e-01,
     &9.689001e-01,9.697400e-01,9.705361e-01,9.712922e-01,9.720114e-01,
     &9.726968e-01,9.733509e-01,9.739760e-01,9.745744e-01,9.751477e-01,
     &9.756979e-01,9.762263e-01,9.767345e-01,9.772236e-01,9.776949e-01,
     &9.781495e-01,9.785882e-01,9.790121e-01/
      DATA (ASYICE2(I,14),I=1,43) /
C    BAND 14
     &8.240566e-01,8.644102e-01,8.868070e-01,9.015382e-01,9.121690e-01,
     &9.203056e-01,9.267921e-01,9.321201e-01,9.365980e-01,9.404302e-01,
     &9.437583e-01,9.466840e-01,9.492823e-01,9.516101e-01,9.537112e-01,
     &9.556203e-01,9.573649e-01,9.589674e-01,9.604460e-01,9.618159e-01,
     &9.630899e-01,9.642786e-01,9.653911e-01,9.664352e-01,9.674178e-01,
     &9.683445e-01,9.692205e-01,9.700502e-01,9.708376e-01,9.715862e-01,
     &9.722990e-01,9.729789e-01,9.736282e-01,9.742491e-01,9.748438e-01,
     &9.754140e-01,9.759613e-01,9.764872e-01,9.769931e-01,9.774802e-01,
     &9.779496e-01,9.784025e-01,9.788396e-01/
      DATA (ASYICE2(I,15),I=1,43) /
C    BAND 15
     &7.868651e-01,8.322305e-01,8.577581e-01,8.747072e-01,8.870264e-01,
     &8.965099e-01,9.041069e-01,9.103734e-01,9.156595e-01,9.201985e-01,
     &9.241524e-01,9.276378e-01,9.307412e-01,9.335281e-01,9.360494e-01,
     &9.383451e-01,9.404472e-01,9.423817e-01,9.441700e-01,9.458298e-01,
     &9.473759e-01,9.488208e-01,9.501753e-01,9.514485e-01,9.526482e-01,
     &9.537815e-01,9.548541e-01,9.558715e-01,9.568383e-01,9.577585e-01,
     &9.586359e-01,9.594736e-01,9.602747e-01,9.610417e-01,9.617771e-01,
     &9.624829e-01,9.631611e-01,9.638136e-01,9.644418e-01,9.650473e-01,
     &9.656314e-01,9.661955e-01,9.667405e-01/
      DATA (ASYICE2(I,16),I=1,43) /
C    BAND 16
     &7.946655e-01,8.547685e-01,8.806016e-01,8.949880e-01,9.041676e-01,
     &9.105399e-01,9.152249e-01,9.188160e-01,9.216573e-01,9.239620e-01,
     &9.258695e-01,9.274745e-01,9.288441e-01,9.300267e-01,9.310584e-01,
     &9.319665e-01,9.327721e-01,9.334918e-01,9.341387e-01,9.347236e-01,
     &9.352551e-01,9.357402e-01,9.361850e-01,9.365942e-01,9.369722e-01,
     &9.373225e-01,9.376481e-01,9.379516e-01,9.382352e-01,9.385010e-01,
     &9.387505e-01,9.389854e-01,9.392070e-01,9.394163e-01,9.396145e-01,
     &9.398024e-01,9.399809e-01,9.401508e-01,9.403126e-01,9.404670e-01,
     &9.406144e-01,9.407555e-01,9.408906e-01/

C     Hexagonal Ice Particle Parameterization
C     EXTINCTION UNITS (EXT COEF/IWC): [(m^-1)/(g m^-3)]
      DATA (EXTICE3(I,1),I=1,46) /
C    BAND 1
     &3.408303e-03,5.644582e-02,8.802256e-02,9.139862e-02,8.751853e-02,
     &8.196726e-02,7.636347e-02,7.118487e-02,6.654102e-02,6.241830e-02,
     &5.876490e-02,5.552147e-02,5.263196e-02,5.004698e-02,4.772428e-02,
     &4.562809e-02,4.372831e-02,4.199961e-02,4.042058e-02,3.897310e-02,
     &3.764178e-02,3.641344e-02,3.527679e-02,3.422209e-02,3.324092e-02,
     &3.232593e-02,3.147072e-02,3.066968e-02,2.991787e-02,2.921093e-02,
     &2.854498e-02,2.791659e-02,2.732268e-02,2.676052e-02,2.622764e-02,
     &2.572182e-02,2.524106e-02,2.478356e-02,2.434767e-02,2.393190e-02,
     &2.353490e-02,2.315542e-02,2.279235e-02,2.244463e-02,2.211133e-02,
     &2.179156e-02/
      DATA (EXTICE3(I,2),I=1,46) /
C    BAND 2
     &1.672430e-03,9.734149e-02,1.408746e-01,1.412504e-01,1.319985e-01,
     &1.210834e-01,1.106662e-01,1.012940e-01,9.302056e-02,8.575111e-02,
     &7.935612e-02,7.370958e-02,6.870035e-02,6.423409e-02,6.023195e-02,
     &5.662833e-02,5.336865e-02,5.040738e-02,4.770635e-02,4.523343e-02,
     &4.296144e-02,4.086725e-02,3.893106e-02,3.713588e-02,3.546701e-02,
     &3.391173e-02,3.245891e-02,3.109885e-02,2.982301e-02,2.862385e-02,
     &2.749470e-02,2.642966e-02,2.542343e-02,2.447131e-02,2.356906e-02,
     &2.271289e-02,2.189937e-02,2.112540e-02,2.038818e-02,1.968516e-02,
     &1.901402e-02,1.837265e-02,1.775911e-02,1.717164e-02,1.660862e-02,
     &1.606856e-02/
      DATA (EXTICE3(I,3),I=1,46) /
C    BAND 3
     &1.972490e-01,2.471923e-01,2.190426e-01,1.886876e-01,1.635161e-01,
     &1.433107e-01,1.270080e-01,1.136759e-01,1.026128e-01,9.330520e-02,
     &8.537673e-02,7.854795e-02,7.260841e-02,6.739721e-02,6.278951e-02,
     &5.868713e-02,5.501189e-02,5.170080e-02,4.870261e-02,4.597520e-02,
     &4.348362e-02,4.119867e-02,3.909578e-02,3.715408e-02,3.535578e-02,
     &3.368560e-02,3.213036e-02,3.067862e-02,2.932039e-02,2.804694e-02,
     &2.685059e-02,2.572455e-02,2.466282e-02,2.366004e-02,2.271144e-02,
     &2.181276e-02,2.096016e-02,2.015018e-02,1.937972e-02,1.864596e-02,
     &1.794633e-02,1.727851e-02,1.664038e-02,1.603000e-02,1.544561e-02,
     &1.488558e-02/
      DATA (EXTICE3(I,4),I=1,46) /
C    BAND 4
     &3.811222e-01,2.987800e-01,2.363621e-01,1.936824e-01,1.633485e-01,
     &1.408443e-01,1.235383e-01,1.098370e-01,9.873002e-02,8.954892e-02,
     &8.183532e-02,7.526487e-02,6.960186e-02,6.467097e-02,6.033921e-02,
     &5.650387e-02,5.308439e-02,5.001673e-02,4.724933e-02,4.474022e-02,
     &4.245490e-02,4.036473e-02,3.844576e-02,3.667782e-02,3.504375e-02,
     &3.352892e-02,3.212075e-02,3.080835e-02,2.958230e-02,2.843434e-02,
     &2.735724e-02,2.634465e-02,2.539094e-02,2.449112e-02,2.364075e-02,
     &2.283587e-02,2.207291e-02,2.134870e-02,2.066036e-02,2.000527e-02,
     &1.938110e-02,1.878570e-02,1.821714e-02,1.767362e-02,1.715355e-02,
     &1.665542e-02/
      DATA (EXTICE3(I,5),I=1,46) /
C    BAND 5
     &3.995665e-01,2.934192e-01,2.273221e-01,1.845439e-01,1.549229e-01,
     &1.332808e-01,1.168041e-01,1.038520e-01,9.340782e-02,8.481006e-02,
     &7.761020e-02,7.149375e-02,6.623375e-02,6.166239e-02,5.765292e-02,
     &5.410788e-02,5.095109e-02,4.812212e-02,4.557248e-02,4.326279e-02,
     &4.116070e-02,3.923946e-02,3.747670e-02,3.585359e-02,3.435419e-02,
     &3.296488e-02,3.167396e-02,3.047135e-02,2.934829e-02,2.829713e-02,
     &2.731120e-02,2.638460e-02,2.551213e-02,2.468919e-02,2.391168e-02,
     &2.317594e-02,2.247869e-02,2.181699e-02,2.118818e-02,2.058988e-02,
     &2.001992e-02,1.947633e-02,1.895732e-02,1.846126e-02,1.798667e-02,
     &1.753218e-02/
      DATA (EXTICE3(I,6),I=1,46) /
C    BAND 6
     &1.813874e-01,1.978092e-01,1.715767e-01,1.471311e-01,1.276487e-01,
     &1.122980e-01,1.000450e-01,9.009510e-02,8.187953e-02,7.499315e-02,
     &6.914382e-02,6.411723e-02,5.975329e-02,5.593036e-02,5.255454e-02,
     &4.955227e-02,4.686517e-02,4.444635e-02,4.225773e-02,4.026807e-02,
     &3.845151e-02,3.678649e-02,3.525486e-02,3.384124e-02,3.253254e-02,
     &3.131752e-02,3.018649e-02,2.913104e-02,2.814387e-02,2.721856e-02,
     &2.634948e-02,2.553167e-02,2.476072e-02,2.403273e-02,2.334421e-02,
     &2.269203e-02,2.207339e-02,2.148578e-02,2.092691e-02,2.039474e-02,
     &1.988739e-02,1.940317e-02,1.894054e-02,1.849807e-02,1.807450e-02,
     &1.766862e-02/
      DATA (EXTICE3(I,7),I=1,46) /
C    BAND 7
     &4.608531e-02,1.749754e-01,1.750374e-01,1.581885e-01,1.407428e-01,
     &1.254718e-01,1.125654e-01,1.017008e-01,9.250502e-02,8.465627e-02,
     &7.789684e-02,7.202462e-02,6.688150e-02,6.234315e-02,5.831110e-02,
     &5.470656e-02,5.146596e-02,4.853753e-02,4.587872e-02,4.345428e-02,
     &4.123479e-02,3.919551e-02,3.731550e-02,3.557690e-02,3.396443e-02,
     &3.246491e-02,3.106693e-02,2.976055e-02,2.853708e-02,2.738892e-02,
     &2.630933e-02,2.529237e-02,2.433275e-02,2.342578e-02,2.256724e-02,
     &2.175337e-02,2.098078e-02,2.024641e-02,1.954749e-02,1.888153e-02,
     &1.824626e-02,1.763959e-02,1.705966e-02,1.650472e-02,1.597320e-02,
     &1.546365e-02/
      DATA (EXTICE3(I,8),I=1,46) /
C    BAND 8
     &2.757113e-01,2.671436e-01,2.242236e-01,1.887468e-01,1.615472e-01,
     &1.405234e-01,1.239339e-01,1.105653e-01,9.958709e-02,9.042264e-02,
     &8.266308e-02,7.601193e-02,7.024963e-02,6.521047e-02,6.076722e-02,
     &5.682062e-02,5.329219e-02,5.011908e-02,4.725037e-02,4.464441e-02,
     &4.226678e-02,4.008881e-02,3.808641e-02,3.623922e-02,3.452991e-02,
     &3.294361e-02,3.146754e-02,3.009060e-02,2.880315e-02,2.759675e-02,
     &2.646398e-02,2.539832e-02,2.439397e-02,2.344581e-02,2.254924e-02,
     &2.170017e-02,2.089493e-02,2.013021e-02,1.940303e-02,1.871069e-02,
     &1.805076e-02,1.742100e-02,1.681940e-02,1.624410e-02,1.569343e-02,
     &1.516584e-02/
      DATA (EXTICE3(I,9),I=1,46) /
C    BAND 9
     &4.502510e-01,3.193893e-01,2.440352e-01,1.965220e-01,1.640522e-01,
     &1.405164e-01,1.226933e-01,1.087361e-01,9.751395e-02,8.829643e-02,
     &8.059148e-02,7.405560e-02,6.844183e-02,6.356811e-02,5.929729e-02,
     &5.552411e-02,5.216644e-02,4.915928e-02,4.645050e-02,4.399781e-02,
     &4.176655e-02,3.972805e-02,3.785837e-02,3.613737e-02,3.454802e-02,
     &3.307576e-02,3.170811e-02,3.043432e-02,2.924505e-02,2.813215e-02,
     &2.708850e-02,2.610783e-02,2.518461e-02,2.431393e-02,2.349144e-02,
     &2.271324e-02,2.197585e-02,2.127615e-02,2.061131e-02,1.997879e-02,
     &1.937629e-02,1.880173e-02,1.825320e-02,1.772898e-02,1.722749e-02,
     &1.674727e-02/
      DATA (EXTICE3(I,10),I=1,46) /
C    BAND 10
     &5.006417e-01,3.306610e-01,2.458712e-01,1.953080e-01,1.617721e-01,
     &1.379144e-01,1.200780e-01,1.062405e-01,9.519348e-02,8.617061e-02,
     &7.866248e-02,7.231733e-02,6.688443e-02,6.218032e-02,5.806758e-02,
     &5.444133e-02,5.122008e-02,4.833958e-02,4.574849e-02,4.340528e-02,
     &4.127601e-02,3.933266e-02,3.755191e-02,3.591416e-02,3.440286e-02,
     &3.300390e-02,3.170522e-02,3.049640e-02,2.936844e-02,2.831347e-02,
     &2.732464e-02,2.639591e-02,2.552198e-02,2.469812e-02,2.392016e-02,
     &2.318436e-02,2.248739e-02,2.182625e-02,2.119825e-02,2.060095e-02,
     &2.003217e-02,1.948990e-02,1.897234e-02,1.847783e-02,1.800487e-02,
     &1.755207e-02/
      DATA (EXTICE3(I,11),I=1,46) /
C    BAND 11
     &5.218467e-01,3.358426e-01,2.470669e-01,1.951513e-01,1.610989e-01,
     &1.370472e-01,1.191559e-01,1.053276e-01,9.431944e-02,8.534878e-02,
     &7.789788e-02,7.161072e-02,6.623443e-02,6.158443e-02,5.752286e-02,
     &5.394468e-02,5.076843e-02,4.792999e-02,4.537818e-02,4.307168e-02,
     &4.097672e-02,3.906547e-02,3.731479e-02,3.570525e-02,3.422043e-02,
     &3.284639e-02,3.157116e-02,3.038446e-02,2.927736e-02,2.824213e-02,
     &2.727197e-02,2.636094e-02,2.550379e-02,2.469588e-02,2.393307e-02,
     &2.321170e-02,2.252846e-02,2.188042e-02,2.126493e-02,2.067958e-02,
     &2.012221e-02,1.959087e-02,1.908376e-02,1.859927e-02,1.813592e-02,
     &1.769235e-02/
      DATA (EXTICE3(I,12),I=1,46) /
C    BAND 12
     &5.974497e-01,3.649665e-01,2.622591e-01,2.044015e-01,1.672868e-01,
     &1.414571e-01,1.224453e-01,1.078669e-01,9.633309e-02,8.698039e-02,
     &7.924354e-02,7.273708e-02,6.718913e-02,6.240238e-02,5.823024e-02,
     &5.456145e-02,5.131011e-02,4.840878e-02,4.580383e-02,4.345204e-02,
     &4.131820e-02,3.937336e-02,3.759346e-02,3.595836e-02,3.445109e-02,
     &3.305720e-02,3.176437e-02,3.056198e-02,2.944085e-02,2.839302e-02,
     &2.741152e-02,2.649024e-02,2.562379e-02,2.480743e-02,2.403692e-02,
     &2.330851e-02,2.261883e-02,2.196487e-02,2.134392e-02,2.075355e-02,
     &2.019154e-02,1.965589e-02,1.914480e-02,1.865660e-02,1.818979e-02,
     &1.774300e-02/
      DATA (EXTICE3(I,13),I=1,46) /
C    BAND 13
     &6.043932e-01,3.625783e-01,2.584703e-01,2.006099e-01,1.638026e-01,
     &1.383330e-01,1.196641e-01,1.053939e-01,9.413223e-02,8.501864e-02,
     &7.749223e-02,7.117168e-02,6.578876e-02,6.114930e-02,5.710925e-02,
     &5.355953e-02,5.041600e-02,4.761274e-02,4.509735e-02,4.282767e-02,
     &4.076939e-02,3.889430e-02,3.717899e-02,3.560388e-02,3.415247e-02,
     &3.281075e-02,3.156674e-02,3.041015e-02,2.933208e-02,2.832481e-02,
     &2.738159e-02,2.649650e-02,2.566434e-02,2.488050e-02,2.414089e-02,
     &2.344188e-02,2.278022e-02,2.215300e-02,2.155760e-02,2.099166e-02,
     &2.045305e-02,1.993985e-02,1.945029e-02,1.898280e-02,1.853590e-02,
     &1.810828e-02/
      DATA (EXTICE3(I,14),I=1,46) /
C    BAND 14
     &6.072115e-01,3.603507e-01,2.556553e-01,1.979378e-01,1.614082e-01,
     &1.362193e-01,1.178036e-01,1.037544e-01,9.268423e-02,8.373670e-02,
     &7.635495e-02,7.016114e-02,6.488994e-02,6.034953e-02,5.639781e-02,
     &5.292726e-02,4.985507e-02,4.711636e-02,4.465963e-02,4.244346e-02,
     &4.043417e-02,3.860408e-02,3.693023e-02,3.539343e-02,3.397752e-02,
     &3.266875e-02,3.145541e-02,3.032744e-02,2.927611e-02,2.829387e-02,
     &2.737414e-02,2.651111e-02,2.569970e-02,2.493542e-02,2.421427e-02,
     &2.353269e-02,2.288752e-02,2.227591e-02,2.169531e-02,2.114341e-02,
     &2.061814e-02,2.011762e-02,1.964012e-02,1.918411e-02,1.874815e-02,
     &1.833095e-02/
      DATA (EXTICE3(I,15),I=1,46) /
C    BAND 15
     &6.177203e-01,3.627414e-01,2.560843e-01,1.977307e-01,1.609776e-01,
     &1.357202e-01,1.173003e-01,1.032749e-01,9.224026e-02,8.333247e-02,
     &7.599102e-02,6.983637e-02,6.460236e-02,6.009687e-02,5.617773e-02,
     &5.273750e-02,4.969348e-02,4.698095e-02,4.454857e-02,4.235508e-02,
     &4.036693e-02,3.855657e-02,3.690119e-02,3.538169e-02,3.398201e-02,
     &3.268851e-02,3.148955e-02,3.037512e-02,2.933659e-02,2.836647e-02,
     &2.745820e-02,2.660605e-02,2.580498e-02,2.505053e-02,2.433873e-02,
     &2.366608e-02,2.302942e-02,2.242594e-02,2.185312e-02,2.130867e-02,
     &2.079054e-02,2.029686e-02,1.982594e-02,1.937624e-02,1.894635e-02,
     &1.853500e-02/
      DATA (EXTICE3(I,16),I=1,46) /
C    BAND 16
     &5.194013e-01,3.215089e-01,2.327917e-01,1.824424e-01,1.499977e-01,
     &1.273492e-01,1.106421e-01,9.780982e-02,8.764435e-02,7.939266e-02,
     &7.256081e-02,6.681137e-02,6.190600e-02,5.767154e-02,5.397915e-02,
     &5.073102e-02,4.785151e-02,4.528125e-02,4.297296e-02,4.088853e-02,
     &3.899690e-02,3.727251e-02,3.569411e-02,3.424393e-02,3.290694e-02,
     &3.167040e-02,3.052340e-02,2.945654e-02,2.846172e-02,2.753188e-02,
     &2.666085e-02,2.584322e-02,2.507423e-02,2.434967e-02,2.366579e-02,
     &2.301926e-02,2.240711e-02,2.182666e-02,2.127551e-02,2.075150e-02,
     &2.025267e-02,1.977725e-02,1.932364e-02,1.889035e-02,1.847607e-02,
     &1.807956e-02/
C     SINGLE-SCATTERING ALBEDO: Unitless
      DATA (SSAICE3(I,1),I=1,46) /
C    BAND 1
     &8.733202e-02,1.733042e-01,2.494597e-01,2.853637e-01,3.129915e-01,
     &3.366261e-01,3.574925e-01,3.761110e-01,3.927678e-01,4.076523e-01,
     &4.209083e-01,4.326557e-01,4.430008e-01,4.520419e-01,4.598721e-01,
     &4.665813e-01,4.722567e-01,4.769845e-01,4.808494e-01,4.839354e-01,
     &4.863257e-01,4.881034e-01,4.893508e-01,4.901503e-01,4.905838e-01,
     &4.907333e-01,4.906804e-01,4.905066e-01,4.902935e-01,4.901225e-01,
     &4.900749e-01,4.902321e-01,4.906751e-01,4.914853e-01,4.927439e-01,
     &4.945318e-01,4.969304e-01,5.000205e-01,5.038834e-01,5.086000e-01,
     &5.142513e-01,5.209185e-01,5.286824e-01,5.376241e-01,5.478246e-01,
     &5.593649e-01/
      DATA (SSAICE3(I,2),I=1,46) /
C    BAND 2
     &7.617260e-01,8.272990e-01,8.134738e-01,8.040737e-01,7.953976e-01,
     &7.869914e-01,7.787330e-01,7.705796e-01,7.625153e-01,7.545359e-01,
     &7.466425e-01,7.388394e-01,7.311327e-01,7.235295e-01,7.160381e-01,
     &7.086672e-01,7.014261e-01,6.943246e-01,6.873731e-01,6.805821e-01,
     &6.739626e-01,6.675263e-01,6.612848e-01,6.552505e-01,6.494361e-01,
     &6.438547e-01,6.385199e-01,6.334458e-01,6.286469e-01,6.241384e-01,
     &6.199357e-01,6.160553e-01,6.125138e-01,6.093286e-01,6.065180e-01,
     &6.041006e-01,6.020961e-01,6.005248e-01,5.994078e-01,5.987672e-01,
     &5.986259e-01,5.990079e-01,5.999382e-01,6.014428e-01,6.035491e-01,
     &6.062855e-01/
      DATA (SSAICE3(I,3),I=1,46) /
C    BAND 3
     &6.485070e-01,6.545008e-01,6.483874e-01,6.411445e-01,6.338616e-01,
     &6.268166e-01,6.201055e-01,6.137647e-01,6.078060e-01,6.022297e-01,
     &5.970301e-01,5.921982e-01,5.877229e-01,5.835920e-01,5.797922e-01,
     &5.763099e-01,5.731308e-01,5.702407e-01,5.676249e-01,5.652685e-01,
     &5.631568e-01,5.612747e-01,5.596072e-01,5.581391e-01,5.568553e-01,
     &5.557405e-01,5.547796e-01,5.539571e-01,5.532579e-01,5.526665e-01,
     &5.521676e-01,5.517458e-01,5.513856e-01,5.510717e-01,5.507886e-01,
     &5.505208e-01,5.502528e-01,5.499691e-01,5.496542e-01,5.492923e-01,
     &5.488681e-01,5.483658e-01,5.477697e-01,5.470643e-01,5.462337e-01,
     &5.452623e-01/
      DATA (SSAICE3(I,4),I=1,46) /
C    BAND 4
     &5.367012e-01,5.372179e-01,5.366731e-01,5.359685e-01,5.352768e-01,
     &5.346515e-01,5.341121e-01,5.336653e-01,5.333122e-01,5.330513e-01,
     &5.328793e-01,5.327924e-01,5.327859e-01,5.328549e-01,5.329943e-01,
     &5.331989e-01,5.334632e-01,5.337819e-01,5.341494e-01,5.345602e-01,
     &5.350088e-01,5.354897e-01,5.359972e-01,5.365259e-01,5.370701e-01,
     &5.376244e-01,5.381831e-01,5.387408e-01,5.392920e-01,5.398311e-01,
     &5.403526e-01,5.408512e-01,5.413213e-01,5.417575e-01,5.421545e-01,
     &5.425067e-01,5.428090e-01,5.430558e-01,5.432420e-01,5.433623e-01,
     &5.434113e-01,5.433838e-01,5.432748e-01,5.430789e-01,5.427911e-01,
     &5.424063e-01/
      DATA (SSAICE3(I,5),I=1,46) /
C    BAND 5
     &4.144473e-01,4.233085e-01,4.317951e-01,4.397881e-01,4.472768e-01,
     &4.542704e-01,4.607843e-01,4.668359e-01,4.724439e-01,4.776272e-01,
     &4.824055e-01,4.867983e-01,4.908252e-01,4.945063e-01,4.978612e-01,
     &5.009100e-01,5.036726e-01,5.061689e-01,5.084192e-01,5.104433e-01,
     &5.122613e-01,5.138934e-01,5.153598e-01,5.166804e-01,5.178756e-01,
     &5.189655e-01,5.199702e-01,5.209102e-01,5.218055e-01,5.226765e-01,
     &5.235435e-01,5.244268e-01,5.253468e-01,5.263238e-01,5.273783e-01,
     &5.285306e-01,5.298012e-01,5.312106e-01,5.327792e-01,5.345277e-01,
     &5.364765e-01,5.386462e-01,5.410574e-01,5.437308e-01,5.466871e-01,
     &5.499469e-01/
      DATA (SSAICE3(I,6),I=1,46) /
C    BAND 6
     &3.685509e-01,4.062125e-01,4.219575e-01,4.336348e-01,4.434522e-01,
     &4.520497e-01,4.596957e-01,4.665324e-01,4.726498e-01,4.781130e-01,
     &4.829739e-01,4.872771e-01,4.910627e-01,4.943681e-01,4.972286e-01,
     &4.996785e-01,5.017514e-01,5.034799e-01,5.048966e-01,5.060334e-01,
     &5.069224e-01,5.075951e-01,5.080833e-01,5.084185e-01,5.086320e-01,
     &5.087554e-01,5.088200e-01,5.088572e-01,5.088985e-01,5.089752e-01,
     &5.091187e-01,5.093605e-01,5.097319e-01,5.102646e-01,5.109899e-01,
     &5.119394e-01,5.131447e-01,5.146373e-01,5.164489e-01,5.186112e-01,
     &5.211558e-01,5.241145e-01,5.275191e-01,5.314014e-01,5.357933e-01,
     &5.407268e-01/
      DATA (SSAICE3(I,7),I=1,46) /
C    BAND 7
     &7.347648e-01,6.945725e-01,6.844409e-01,6.757818e-01,6.676970e-01,
     &6.599913e-01,6.525964e-01,6.454817e-01,6.386312e-01,6.320353e-01,
     &6.256876e-01,6.195836e-01,6.137200e-01,6.080940e-01,6.027033e-01,
     &5.975460e-01,5.926203e-01,5.879246e-01,5.834574e-01,5.792174e-01,
     &5.752032e-01,5.714137e-01,5.678476e-01,5.645040e-01,5.613817e-01,
     &5.584797e-01,5.557971e-01,5.533330e-01,5.510865e-01,5.490569e-01,
     &5.472434e-01,5.456452e-01,5.442618e-01,5.430926e-01,5.421369e-01,
     &5.413943e-01,5.408645e-01,5.405470e-01,5.404415e-01,5.405479e-01,
     &5.408659e-01,5.413956e-01,5.421369e-01,5.430900e-01,5.442550e-01,
     &5.456321e-01/
      DATA (SSAICE3(I,8),I=1,46) /
C    BAND 8
     &7.565911e-01,7.410307e-01,7.267244e-01,7.132696e-01,7.005889e-01,
     &6.886431e-01,6.774024e-01,6.668406e-01,6.569327e-01,6.476543e-01,
     &6.389815e-01,6.308907e-01,6.233581e-01,6.163602e-01,6.098736e-01,
     &6.038748e-01,5.983403e-01,5.932468e-01,5.885708e-01,5.842888e-01,
     &5.803776e-01,5.768135e-01,5.735734e-01,5.706336e-01,5.679708e-01,
     &5.655616e-01,5.633825e-01,5.614101e-01,5.596210e-01,5.579915e-01,
     &5.564984e-01,5.551181e-01,5.538271e-01,5.526020e-01,5.514191e-01,
     &5.502550e-01,5.490862e-01,5.478890e-01,5.466400e-01,5.453154e-01,
     &5.438918e-01,5.423455e-01,5.406528e-01,5.387902e-01,5.367338e-01,
     &5.344600e-01/
      DATA (SSAICE3(I,9),I=1,46) /
C    BAND 9
     &7.253131e-01,7.112494e-01,6.974209e-01,6.843066e-01,6.719929e-01,
     &6.604896e-01,6.497830e-01,6.398502e-01,6.306641e-01,6.221956e-01,
     &6.144141e-01,6.072887e-01,6.007877e-01,5.948792e-01,5.895312e-01,
     &5.847116e-01,5.803881e-01,5.765282e-01,5.730996e-01,5.700698e-01,
     &5.674064e-01,5.650768e-01,5.630484e-01,5.612888e-01,5.597653e-01,
     &5.584452e-01,5.572961e-01,5.562853e-01,5.553800e-01,5.545478e-01,
     &5.537558e-01,5.529714e-01,5.521620e-01,5.512948e-01,5.503371e-01,
     &5.492561e-01,5.480192e-01,5.465935e-01,5.449463e-01,5.430449e-01,
     &5.408564e-01,5.383480e-01,5.354869e-01,5.322403e-01,5.285753e-01,
     &5.244591e-01/
      DATA (SSAICE3(I,10),I=1,46) /
C    BAND 10
     &6.692312e-01,6.569887e-01,6.455728e-01,6.349744e-01,6.251679e-01,
     &6.161241e-01,6.078124e-01,6.002017e-01,5.932608e-01,5.869582e-01,
     &5.812625e-01,5.761422e-01,5.715658e-01,5.675017e-01,5.639183e-01,
     &5.607841e-01,5.580676e-01,5.557370e-01,5.537609e-01,5.521078e-01,
     &5.507459e-01,5.496437e-01,5.487696e-01,5.480921e-01,5.475796e-01,
     &5.472004e-01,5.469230e-01,5.467159e-01,5.465474e-01,5.463859e-01,
     &5.461999e-01,5.459577e-01,5.456279e-01,5.451788e-01,5.445788e-01,
     &5.437964e-01,5.427999e-01,5.415578e-01,5.400386e-01,5.382105e-01,
     &5.360422e-01,5.335019e-01,5.305581e-01,5.271792e-01,5.233336e-01,
     &5.189898e-01/
      DATA (SSAICE3(I,11),I=1,46) /
C    BAND 11
     &6.597440e-01,6.486312e-01,6.385754e-01,6.293389e-01,6.208334e-01,
     &6.130062e-01,6.058168e-01,5.992306e-01,5.932156e-01,5.877415e-01,
     &5.827791e-01,5.782999e-01,5.742760e-01,5.706800e-01,5.674845e-01,
     &5.646626e-01,5.621876e-01,5.600328e-01,5.581718e-01,5.565782e-01,
     &5.552258e-01,5.540883e-01,5.531398e-01,5.523542e-01,5.517054e-01,
     &5.511675e-01,5.507147e-01,5.503210e-01,5.499606e-01,5.496077e-01,
     &5.492364e-01,5.488211e-01,5.483359e-01,5.477550e-01,5.470529e-01,
     &5.462036e-01,5.451816e-01,5.439610e-01,5.425163e-01,5.408217e-01,
     &5.388515e-01,5.365800e-01,5.339817e-01,5.310307e-01,5.277016e-01,
     &5.239685e-01/
      DATA (SSAICE3(I,12),I=1,46) /
C    BAND 12
     &8.415691e-01,8.237634e-01,8.070239e-01,7.912304e-01,7.763216e-01,
     &7.622533e-01,7.489874e-01,7.364895e-01,7.247271e-01,7.136691e-01,
     &7.032852e-01,6.935461e-01,6.844229e-01,6.758871e-01,6.679108e-01,
     &6.604662e-01,6.535258e-01,6.470623e-01,6.410487e-01,6.354581e-01,
     &6.302637e-01,6.254387e-01,6.209567e-01,6.167911e-01,6.129154e-01,
     &6.093033e-01,6.059286e-01,6.027648e-01,5.997858e-01,5.969653e-01,
     &5.942772e-01,5.916953e-01,5.891935e-01,5.867455e-01,5.843253e-01,
     &5.819067e-01,5.794637e-01,5.769700e-01,5.743997e-01,5.717265e-01,
     &5.689243e-01,5.659670e-01,5.628284e-01,5.594825e-01,5.559030e-01,
     &5.520638e-01/
      DATA (SSAICE3(I,13),I=1,46) /
C    BAND 13
     &7.418384e-01,7.252704e-01,7.097171e-01,6.951785e-01,6.816320e-01,
     &6.690487e-01,6.573967e-01,6.466425e-01,6.367511e-01,6.276869e-01,
     &6.194133e-01,6.118931e-01,6.050886e-01,5.989616e-01,5.934736e-01,
     &5.885856e-01,5.842585e-01,5.804528e-01,5.771289e-01,5.742470e-01,
     &5.717672e-01,5.696493e-01,5.678531e-01,5.663385e-01,5.650650e-01,
     &5.639922e-01,5.630797e-01,5.622870e-01,5.615735e-01,5.608986e-01,
     &5.602218e-01,5.595025e-01,5.586999e-01,5.577736e-01,5.566826e-01,
     &5.553865e-01,5.538444e-01,5.520156e-01,5.498592e-01,5.473345e-01,
     &5.444005e-01,5.410162e-01,5.371407e-01,5.327328e-01,5.277513e-01,
     &5.221549e-01/
      DATA (SSAICE3(I,14),I=1,46) /
C    BAND 14
     &7.431625e-01,7.253592e-01,7.089350e-01,6.937690e-01,6.797727e-01,
     &6.668739e-01,6.550099e-01,6.441244e-01,6.341649e-01,6.250822e-01,
     &6.168287e-01,6.093588e-01,6.026277e-01,5.965913e-01,5.912065e-01,
     &5.864305e-01,5.822207e-01,5.785351e-01,5.753318e-01,5.725690e-01,
     &5.702053e-01,5.681992e-01,5.665094e-01,5.650948e-01,5.639141e-01,
     &5.629263e-01,5.620905e-01,5.613656e-01,5.607109e-01,5.600853e-01,
     &5.594482e-01,5.587586e-01,5.579758e-01,5.570591e-01,5.559676e-01,
     &5.546606e-01,5.530972e-01,5.512367e-01,5.490383e-01,5.464611e-01,
     &5.434641e-01,5.400063e-01,5.360468e-01,5.315444e-01,5.264578e-01,
     &5.207459e-01/
      DATA (SSAICE3(I,15),I=1,46) /
C    BAND 15
     &8.214523e-01,8.021560e-01,7.840431e-01,7.670541e-01,7.511382e-01,
     &7.362494e-01,7.223443e-01,7.093816e-01,6.973208e-01,6.861227e-01,
     &6.757482e-01,6.661588e-01,6.573164e-01,6.491829e-01,6.417204e-01,
     &6.348911e-01,6.286574e-01,6.229816e-01,6.178260e-01,6.131531e-01,
     &6.089254e-01,6.051053e-01,6.016552e-01,5.985377e-01,5.957152e-01,
     &5.931503e-01,5.908054e-01,5.886430e-01,5.866256e-01,5.847156e-01,
     &5.828757e-01,5.810683e-01,5.792558e-01,5.774008e-01,5.754657e-01,
     &5.734130e-01,5.712052e-01,5.688048e-01,5.661741e-01,5.632757e-01,
     &5.600721e-01,5.565255e-01,5.525985e-01,5.482534e-01,5.434526e-01,
     &5.381586e-01/
      DATA (SSAICE3(I,16),I=1,46) /
C    BAND 16
     &6.749442e-01,6.649947e-01,6.565828e-01,6.489928e-01,6.420046e-01,
     &6.355231e-01,6.294964e-01,6.238901e-01,6.186783e-01,6.138395e-01,
     &6.093543e-01,6.052049e-01,6.013742e-01,5.978457e-01,5.946030e-01,
     &5.916302e-01,5.889115e-01,5.864310e-01,5.841731e-01,5.821221e-01,
     &5.802624e-01,5.785785e-01,5.770549e-01,5.756759e-01,5.744262e-01,
     &5.732901e-01,5.722524e-01,5.712974e-01,5.704097e-01,5.695739e-01,
     &5.687747e-01,5.679964e-01,5.672238e-01,5.664415e-01,5.656340e-01,
     &5.647860e-01,5.638821e-01,5.629070e-01,5.618452e-01,5.606815e-01,
     &5.594006e-01,5.579870e-01,5.564255e-01,5.547008e-01,5.527976e-01,
     &5.507005e-01/
C     ASYMMETRY FACTOR: Unitless
      DATA (ASYICE3(I,1),I=1,46) /
C    BAND 1
     &6.596879e-01,6.405129e-01,6.397929e-01,6.577098e-01,6.788112e-01,
     &6.996961e-01,7.195365e-01,7.380555e-01,7.551580e-01,7.708248e-01,
     &7.850743e-01,7.979456e-01,8.094902e-01,8.197678e-01,8.288438e-01,
     &8.367874e-01,8.436711e-01,8.495695e-01,8.545592e-01,8.587185e-01,
     &8.621270e-01,8.648654e-01,8.670153e-01,8.686596e-01,8.698817e-01,
     &8.707659e-01,8.713971e-01,8.718612e-01,8.722444e-01,8.726337e-01,
     &8.731167e-01,8.737814e-01,8.747166e-01,8.760115e-01,8.777560e-01,
     &8.800403e-01,8.829553e-01,8.865923e-01,8.910430e-01,8.963998e-01,
     &9.027552e-01,9.102023e-01,9.188345e-01,9.287454e-01,9.400292e-01,
     &9.523461e-01/
      DATA (ASYICE3(I,2),I=1,46) /
C    BAND 2
     &7.389769e-01,7.411960e-01,7.490019e-01,7.564984e-01,7.637359e-01,
     &7.707210e-01,7.774570e-01,7.839465e-01,7.901930e-01,7.962001e-01,
     &8.019724e-01,8.075150e-01,8.128335e-01,8.179342e-01,8.228237e-01,
     &8.275093e-01,8.319986e-01,8.362997e-01,8.404208e-01,8.443707e-01,
     &8.481583e-01,8.517929e-01,8.552840e-01,8.586412e-01,8.618744e-01,
     &8.649936e-01,8.680092e-01,8.709315e-01,8.737713e-01,8.765394e-01,
     &8.792470e-01,8.819057e-01,8.845275e-01,8.871247e-01,8.897104e-01,
     &8.922982e-01,8.949025e-01,8.975389e-01,9.002236e-01,9.029745e-01,
     &9.058105e-01,9.087524e-01,9.118223e-01,9.150447e-01,9.184456e-01,
     &9.220537e-01/
      DATA (ASYICE3(I,3),I=1,46) /
C    BAND 3
     &7.533538e-01,7.631828e-01,7.742293e-01,7.848919e-01,7.950315e-01,
     &8.046253e-01,8.136762e-01,8.221958e-01,8.301991e-01,8.377028e-01,
     &8.447247e-01,8.512829e-01,8.573959e-01,8.630826e-01,8.683620e-01,
     &8.732532e-01,8.777756e-01,8.819487e-01,8.857920e-01,8.893253e-01,
     &8.925684e-01,8.955411e-01,8.982635e-01,9.007556e-01,9.030377e-01,
     &9.051300e-01,9.070528e-01,9.088266e-01,9.104718e-01,9.120090e-01,
     &9.134588e-01,9.148418e-01,9.161788e-01,9.174904e-01,9.187976e-01,
     &9.201211e-01,9.214818e-01,9.229007e-01,9.243986e-01,9.259967e-01,
     &9.277160e-01,9.295774e-01,9.316023e-01,9.338116e-01,9.362266e-01,
     &9.388685e-01/
      DATA (ASYICE3(I,4),I=1,46) /
C    BAND 4
     &7.840105e-01,7.959983e-01,8.074838e-01,8.182465e-01,8.282675e-01,
     &8.375608e-01,8.461497e-01,8.540609e-01,8.613228e-01,8.679643e-01,
     &8.740148e-01,8.795041e-01,8.844620e-01,8.889182e-01,8.929028e-01,
     &8.964456e-01,8.995766e-01,9.023257e-01,9.047230e-01,9.067984e-01,
     &9.085817e-01,9.101031e-01,9.113922e-01,9.124791e-01,9.133937e-01,
     &9.141659e-01,9.148254e-01,9.154023e-01,9.159263e-01,9.164274e-01,
     &9.169353e-01,9.174799e-01,9.180910e-01,9.187986e-01,9.196324e-01,
     &9.206223e-01,9.217981e-01,9.231897e-01,9.248270e-01,9.267399e-01,
     &9.289582e-01,9.315119e-01,9.344308e-01,9.377451e-01,9.414845e-01,
     &9.456792e-01/
      DATA (ASYICE3(I,5),I=1,46) /
C    BAND 5
     &8.304283e-01,8.399040e-01,8.485749e-01,8.565538e-01,8.638881e-01,
     &8.706126e-01,8.767578e-01,8.823528e-01,8.874253e-01,8.920026e-01,
     &8.961113e-01,8.997780e-01,9.030287e-01,9.058894e-01,9.083861e-01,
     &9.105442e-01,9.123895e-01,9.139473e-01,9.152430e-01,9.163020e-01,
     &9.171497e-01,9.178111e-01,9.183116e-01,9.186762e-01,9.189303e-01,
     &9.190989e-01,9.192071e-01,9.192802e-01,9.193432e-01,9.194211e-01,
     &9.195392e-01,9.197225e-01,9.199961e-01,9.203850e-01,9.209143e-01,
     &9.216090e-01,9.224942e-01,9.235947e-01,9.249355e-01,9.265416e-01,
     &9.284376e-01,9.306484e-01,9.331987e-01,9.361131e-01,9.394162e-01,
     &9.431324e-01/
      DATA (ASYICE3(I,6),I=1,46) /
C    BAND 6
     &8.792712e-01,8.938493e-01,9.015383e-01,9.078755e-01,9.134852e-01,
     &9.185471e-01,9.231387e-01,9.273046e-01,9.310755e-01,9.344765e-01,
     &9.375293e-01,9.402543e-01,9.426709e-01,9.447982e-01,9.466549e-01,
     &9.482596e-01,9.496311e-01,9.507879e-01,9.517486e-01,9.525319e-01,
     &9.531565e-01,9.536411e-01,9.540046e-01,9.542656e-01,9.544431e-01,
     &9.545559e-01,9.546229e-01,9.546631e-01,9.546954e-01,9.547387e-01,
     &9.548121e-01,9.549345e-01,9.551251e-01,9.554028e-01,9.557866e-01,
     &9.562958e-01,9.569494e-01,9.577665e-01,9.587663e-01,9.599680e-01,
     &9.613908e-01,9.630539e-01,9.649767e-01,9.671785e-01,9.696786e-01,
     &9.724966e-01/
      DATA (ASYICE3(I,7),I=1,46) /
C    BAND 7
     &8.992856e-01,9.093587e-01,9.140642e-01,9.183077e-01,9.222541e-01,
     &9.259455e-01,9.294018e-01,9.326363e-01,9.356600e-01,9.384824e-01,
     &9.411129e-01,9.435602e-01,9.458332e-01,9.479406e-01,9.498907e-01,
     &9.516924e-01,9.533540e-01,9.548842e-01,9.562914e-01,9.575842e-01,
     &9.587710e-01,9.598605e-01,9.608610e-01,9.617811e-01,9.626294e-01,
     &9.634142e-01,9.641441e-01,9.648277e-01,9.654732e-01,9.660894e-01,
     &9.666845e-01,9.672673e-01,9.678460e-01,9.684293e-01,9.690257e-01,
     &9.696436e-01,9.702917e-01,9.709786e-01,9.717127e-01,9.725029e-01,
     &9.733577e-01,9.742858e-01,9.752962e-01,9.763975e-01,9.775987e-01,
     &9.789088e-01/
      DATA (ASYICE3(I,8),I=1,46) /
C    BAND 8
     &8.768357e-01,8.832796e-01,8.887092e-01,8.937467e-01,8.984873e-01,
     &9.029645e-01,9.071961e-01,9.111946e-01,9.149701e-01,9.185316e-01,
     &9.218877e-01,9.250464e-01,9.280157e-01,9.308033e-01,9.334170e-01,
     &9.358643e-01,9.381529e-01,9.402904e-01,9.422842e-01,9.441418e-01,
     &9.458708e-01,9.474786e-01,9.489727e-01,9.503604e-01,9.516494e-01,
     &9.528469e-01,9.539604e-01,9.549973e-01,9.559651e-01,9.568711e-01,
     &9.577229e-01,9.585276e-01,9.592929e-01,9.600261e-01,9.607346e-01,
     &9.614258e-01,9.621071e-01,9.627859e-01,9.634698e-01,9.641660e-01,
     &9.648820e-01,9.656253e-01,9.664032e-01,9.672233e-01,9.680930e-01,
     &9.690198e-01/
      DATA (ASYICE3(I,9),I=1,46) /
C    BAND 9
     &8.609711e-01,8.676429e-01,8.740194e-01,8.800768e-01,8.858172e-01,
     &8.912476e-01,8.963767e-01,9.012137e-01,9.057682e-01,9.100498e-01,
     &9.140683e-01,9.178335e-01,9.213552e-01,9.246434e-01,9.277079e-01,
     &9.305587e-01,9.332058e-01,9.356590e-01,9.379284e-01,9.400240e-01,
     &9.419558e-01,9.437340e-01,9.453685e-01,9.468694e-01,9.482469e-01,
     &9.495111e-01,9.506721e-01,9.517401e-01,9.527252e-01,9.536376e-01,
     &9.544876e-01,9.552853e-01,9.560409e-01,9.567647e-01,9.574670e-01,
     &9.581578e-01,9.588476e-01,9.595466e-01,9.602650e-01,9.610130e-01,
     &9.618010e-01,9.626392e-01,9.635379e-01,9.645074e-01,9.655578e-01,
     &9.666994e-01/
      DATA (ASYICE3(I,10),I=1,46) /
C    BAND 10
     &8.805232e-01,8.872100e-01,8.934945e-01,8.993817e-01,9.048833e-01,
     &9.100128e-01,9.147839e-01,9.192106e-01,9.233070e-01,9.270873e-01,
     &9.305656e-01,9.337561e-01,9.366732e-01,9.393308e-01,9.417434e-01,
     &9.439252e-01,9.458903e-01,9.476530e-01,9.492276e-01,9.506283e-01,
     &9.518694e-01,9.529650e-01,9.539295e-01,9.547771e-01,9.555221e-01,
     &9.561786e-01,9.567610e-01,9.572836e-01,9.577605e-01,9.582059e-01,
     &9.586343e-01,9.590598e-01,9.594966e-01,9.599591e-01,9.604615e-01,
     &9.610179e-01,9.616428e-01,9.623503e-01,9.631546e-01,9.640701e-01,
     &9.651110e-01,9.662916e-01,9.676260e-01,9.691286e-01,9.708136e-01,
     &9.726952e-01/
      DATA (ASYICE3(I,11),I=1,46) /
C    BAND 11
     &8.860103e-01,8.920722e-01,8.976788e-01,9.029125e-01,9.078046e-01,
     &9.123744e-01,9.166373e-01,9.206068e-01,9.242956e-01,9.277160e-01,
     &9.308801e-01,9.337998e-01,9.364867e-01,9.389526e-01,9.412092e-01,
     &9.432681e-01,9.451410e-01,9.468395e-01,9.483754e-01,9.497603e-01,
     &9.510059e-01,9.521239e-01,9.531262e-01,9.540244e-01,9.548305e-01,
     &9.555563e-01,9.562136e-01,9.568142e-01,9.573703e-01,9.578935e-01,
     &9.583959e-01,9.588895e-01,9.593861e-01,9.598976e-01,9.604362e-01,
     &9.610135e-01,9.616417e-01,9.623325e-01,9.630979e-01,9.639496e-01,
     &9.648996e-01,9.659594e-01,9.671409e-01,9.684557e-01,9.699153e-01,
     &9.715312e-01/
      DATA (ASYICE3(I,12),I=1,46) /
C    BAND 12
     &8.097123e-01,8.172929e-01,8.245083e-01,8.314251e-01,8.380674e-01,
     &8.444481e-01,8.505763e-01,8.564596e-01,8.621044e-01,8.675169e-01,
     &8.727030e-01,8.776684e-01,8.824186e-01,8.869590e-01,8.912951e-01,
     &8.954321e-01,8.993753e-01,9.031299e-01,9.067009e-01,9.100936e-01,
     &9.133130e-01,9.163640e-01,9.192519e-01,9.219815e-01,9.245578e-01,
     &9.269860e-01,9.292709e-01,9.314175e-01,9.334309e-01,9.353161e-01,
     &9.370781e-01,9.387218e-01,9.402523e-01,9.416746e-01,9.429938e-01,
     &9.442149e-01,9.453428e-01,9.463827e-01,9.473394e-01,9.482180e-01,
     &9.490235e-01,9.497606e-01,9.504344e-01,9.510495e-01,9.516106e-01,
     &9.521225e-01/
      DATA (ASYICE3(I,13),I=1,46) /
C    BAND 13
     &8.377231e-01,8.469574e-01,8.556844e-01,8.638984e-01,8.716085e-01,
     &8.788282e-01,8.855727e-01,8.918582e-01,8.977012e-01,9.031189e-01,
     &9.081287e-01,9.127481e-01,9.169950e-01,9.208873e-01,9.244432e-01,
     &9.276807e-01,9.306183e-01,9.332744e-01,9.356674e-01,9.378159e-01,
     &9.397385e-01,9.414539e-01,9.429808e-01,9.443380e-01,9.455443e-01,
     &9.466185e-01,9.475794e-01,9.484460e-01,9.492372e-01,9.499718e-01,
     &9.506689e-01,9.513473e-01,9.520260e-01,9.527240e-01,9.534602e-01,
     &9.542535e-01,9.551230e-01,9.560875e-01,9.571659e-01,9.583773e-01,
     &9.597404e-01,9.612742e-01,9.629975e-01,9.649292e-01,9.670880e-01,
     &9.694928e-01/
      DATA (ASYICE3(I,14),I=1,46) /
C    BAND 14
     &8.344994e-01,8.443682e-01,8.536201e-01,8.622890e-01,8.703985e-01,
     &8.779698e-01,8.850230e-01,8.915780e-01,8.976545e-01,9.032725e-01,
     &9.084518e-01,9.132124e-01,9.175742e-01,9.215575e-01,9.251823e-01,
     &9.284689e-01,9.314374e-01,9.341082e-01,9.365017e-01,9.386382e-01,
     &9.405382e-01,9.422220e-01,9.437101e-01,9.450231e-01,9.461814e-01,
     &9.472056e-01,9.481162e-01,9.489338e-01,9.496790e-01,9.503724e-01,
     &9.510345e-01,9.516860e-01,9.523475e-01,9.530396e-01,9.537830e-01,
     &9.545982e-01,9.555059e-01,9.565267e-01,9.576813e-01,9.589902e-01,
     &9.604741e-01,9.621535e-01,9.640490e-01,9.661813e-01,9.685709e-01,
     &9.712382e-01/
      DATA (ASYICE3(I,15),I=1,46) /
C    BAND 15
     &7.853957e-01,7.968767e-01,8.077401e-01,8.180018e-01,8.276797e-01,
     &8.367923e-01,8.453586e-01,8.533977e-01,8.609288e-01,8.679711e-01,
     &8.745440e-01,8.806667e-01,8.863587e-01,8.916394e-01,8.965282e-01,
     &9.010445e-01,9.052079e-01,9.090379e-01,9.125540e-01,9.157758e-01,
     &9.187228e-01,9.214147e-01,9.238711e-01,9.261117e-01,9.281562e-01,
     &9.300242e-01,9.317354e-01,9.333098e-01,9.347669e-01,9.361267e-01,
     &9.374088e-01,9.386331e-01,9.398195e-01,9.409877e-01,9.421576e-01,
     &9.433489e-01,9.445817e-01,9.458755e-01,9.472504e-01,9.487260e-01,
     &9.503221e-01,9.520585e-01,9.539550e-01,9.560313e-01,9.583069e-01,
     &9.608016e-01/
      DATA (ASYICE3(I,16),I=1,46) /
C    BAND 16
     &8.340752e-01,8.435170e-01,8.517487e-01,8.592064e-01,8.660387e-01,
     &8.723204e-01,8.780997e-01,8.834137e-01,8.882934e-01,8.927662e-01,
     &8.968577e-01,9.005914e-01,9.039899e-01,9.070745e-01,9.098659e-01,
     &9.123836e-01,9.146466e-01,9.166734e-01,9.184817e-01,9.200886e-01,
     &9.215109e-01,9.227648e-01,9.238661e-01,9.248304e-01,9.256727e-01,
     &9.264078e-01,9.270505e-01,9.276150e-01,9.281156e-01,9.285662e-01,
     &9.289806e-01,9.293726e-01,9.297557e-01,9.301435e-01,9.305491e-01,
     &9.309859e-01,9.314671e-01,9.320055e-01,9.326140e-01,9.333053e-01,
     &9.340919e-01,9.349861e-01,9.360000e-01,9.371451e-01,9.384329e-01,
     &9.398744e-01/

      END
