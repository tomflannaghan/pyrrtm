C     path:      $Source: /storm/rc1/cvsroot/rc/rrtm_lw/src/rtrdis.f,v $
C     author:    $Author: jdelamer $
C     revision:  $Revision: 5.4 $
C     created:   $Date: 2010/07/07 21:10:53 $

      SUBROUTINE RTRDIS

C *** This program calculates the upward fluxes, downward fluxes,
C     and heating rates for an arbitrary atmosphere.  The input to
C     this program is the atmospheric profile and all Planck function
C     information.  First-order "numerical" quadrature is used for the 
C     angle integration, i.e. only one exponential is computed per layer
C     per g-value per band.

      IMPLICIT DOUBLE PRECISION (V)
      PARAMETER (MXLAY=603)
      PARAMETER (MG = 16)
      PARAMETER (NBANDS = 16)
      PARAMETER (MXANG = 4)
      PARAMETER ( MCMU = 32, MUMU = 32,
     &           MPHI = 3)
      PARAMETER (MXSTR = 16)
      

      COMMON /CONSTANTS/ FLUXFAC,HEATFAC
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,
     &                RADCN1,RADCN2,GRAV,CPDAIR,AIRMWT,SECDY 
      COMMON /FEATURES/  NG(NBANDS),NSPA(NBANDS),NSPB(NBANDS)
      COMMON /BANDS/     WAVENUM1(NBANDS),WAVENUM2(NBANDS),
     &                   DELWAVE(NBANDS)
      COMMON /CONTROL/   NUMANGS, ISCAT, NSTR, 
     &                   IOUT, ISTART, IEND, ICLD
      COMMON /SURFACE/   TBOUND,IREFLECT,SEMISS(NBANDS)
      COMMON /PROFILE/   NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                   PZ(0:MXLAY),TZ(0:MXLAY)
      COMMON /PLNKDAT/   PLANKLAY(MXLAY,NBANDS),
     &                   PLANKLEV(0:MXLAY,NBANDS),PLANKBND(NBANDS)
      COMMON /PLANKG/    FRACS(MXLAY,MG)
      COMMON /TAUGCOM/   TAUG(MXLAY,MG)

      COMMON /CLOUDDAT/  NCBANDS,CLDFRAC(MXLAY),
     &                   TAUCLOUD(MXLAY,NBANDS),
     &                   SSACLOUD(MXLAY,NBANDS),
     &                   XMOM(0:16,MXLAY,NBANDS)

      COMMON /OUTPUT/    TOTUFLUX(0:MXLAY), TOTDFLUX(0:MXLAY),
     &                   FNET(0:MXLAY), HTR(0:MXLAY)

      COMMON /CVRRDS/    HNAMRDS,HVRRDS

      CHARACTER*18       HNAMRDS,HVRRDS

                                       
      Character HEADER*127
      LOGICAL   LAMBER, ONLYFL, PLANK, USRANG, USRTAU
      INTEGER   IBCND, MAXCLY, MAXCMU, MAXPHI, MAXULV, MAXUMU, 
     &          NPHI, NSTR, NTAU, NUMU
      REAL      ACCUR, ALBEDO, BTEMP, FBEAM, FISOT, PHI0, TEMIS, TTEMP,
     &          UMU0 
      LOGICAL   PRNT( 7 )
      REAL      ALBMED( MUMU ), DFDT( MXLAY ), TAUREV(MXLAY),
     &          FLUP( MXLAY ), HL( 0:MCMU ), PHI( MPHI ),
     &          PMOM( 0:MCMU, MXLAY ), RFLDIR( MXLAY ),
     &          RFLDN( MXLAY ), SSALB( MXLAY ), TEMPER( 0:MXLAY ),
     &          TRNMED( MUMU ), U0U( MUMU, MXLAY ), UAVG( MXLAY ),
     &          UMU( MUMU ), UTAU( MXLAY ),
     &          UU( MUMU, MXLAY, MPHI ),fldir(mxlay),fldn(mxlay), 
     &          TZREV(0:MXLAY),FRACSREV(MXLAY)
      DIMENSION PHASERAY(0:MXSTR)


      DATA PRNT /.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &     .FALSE.,.FALSE./

c     Ensure all cloud properties are equal to 0.0
      IF (ICLD .EQ. 0) THEN
         TAUCLOUD(:,:) = 0.0
         SSACLOUD(:,:) = 0.0
         XMOM(:,:,:) = 0.0
      ENDIF
         
      HVRRDS = '$Revision: 5.4 $'
      HEADER = ''
      USRTAU = .FALSE.
      USRANG = .FALSE.
      NPHI = 0
      IBCND = 0
      PHI0 = 0.
      UMU0 = 0.0
      FBEAM = 0.0
      FISOT = 0.0
      TTEMP = TZ(NLAYERS)
      TEMIS = 0.0
      BTEMP = TBOUND

      IF (IREFLECT .EQ. 0) THEN
         LAMBER = .TRUE.
      ELSE
         LAMBER = .FALSE.
      ENDIF

      PLANK = .TRUE.
      ONLYFL = .TRUE.
      ACCUR = 0.0001
      MAXCLY = MXLAY
      MAXULV = MXLAY
      MAXUMU = MUMU
      MAXCMU = MCMU
      MAXPHI = MPHI
      DO 200 LAY = 0, NLAYERS
         TZREV(NLAYERS-LAY) = TZ(LAY)
         IF (LAY .NE. 0) THEN
            SSALB(LAY) = 0.
            DO 180 IQ = 0, NSTR
               PMOM(IQ,LAY) = 0.
 180        CONTINUE
         ENDIF
         TOTDFLUX(LAY) = 0.0
         TOTUFLUX(LAY) = 0.0
 200  CONTINUE

C *** Loop over frequency bands.

      DO 6000 IBAND = ISTART, IEND
        
         IF (IBAND .EQ. 1) THEN
            CALL TAUGB1
         ELSEIF (IBAND .EQ. 2) THEN
            CALL TAUGB2
         ELSEIF (IBAND .EQ. 3) THEN
            CALL TAUGB3
         ELSEIF (IBAND .EQ. 4) THEN
            CALL TAUGB4
         ELSEIF (IBAND .EQ. 5) THEN
            CALL TAUGB5
         ELSEIF (IBAND .EQ. 6) THEN
            CALL TAUGB6
         ELSEIF (IBAND .EQ. 7) THEN
            CALL TAUGB7
         ELSEIF (IBAND .EQ. 8) THEN
            CALL TAUGB8
         ELSEIF (IBAND .EQ. 9) THEN
            CALL TAUGB9
         ELSEIF (IBAND .EQ. 10) THEN
            CALL TAUGB10
         ELSEIF (IBAND .EQ. 11) THEN
            CALL TAUGB11
         ELSEIF (IBAND .EQ. 12) THEN
            CALL TAUGB12
         ELSEIF (IBAND .EQ. 13) THEN
            CALL TAUGB13
         ELSEIF (IBAND .EQ. 14) THEN
            CALL TAUGB14
         ELSEIF (IBAND .EQ. 15) THEN
            CALL TAUGB15
         ELSE
            CALL TAUGB16
         ENDIF

c  set albedo for this band
         ALBEDO = 1. - SEMISS(IBAND)
         WAVENUMLO = WAVENUM1(IBAND)
         WAVENUMHI = WAVENUM2(IBAND)

         if (iband .eq. 16 .and. istart .ne. 16) 
     &        wavenumhi = 5000.
C ***    Loop over g-channels.
         IG = 1
 1000    CONTINUE
C ***    Downward radiative transfer.


         DO 3900 LAY = NLAYERS, 1, -1
            FRACSREV(nlayers-lay+1) = FRACS(LAY,IG)
            
            TAUREV(NLAYERS-LAY+1) = TAUG(LAY,IG) + 
     &           TAUCLOUD(LAY,IBAND)
            
            SCATCLD = SSACLOUD(LAY,IBAND) * TAUCLOUD(LAY,IBAND)
            
            IF (TAUREV(NLAYERS-LAY+1) .NE. 0.0) 
     &           SSALB(NLAYERS-LAY+1) = 
     &           (SCATCLD)/TAUREV(NLAYERS-LAY+1)
            
            IF (SSALB(NLAYERS-LAY+1) .GT. 1.0) THEN
               PRINT*,'WARNING SSALB > 1.0, LAYER ',LAY,
     &              SSALB(NLAYERS-LAY+1)
            ENDIF

           PMOM(0,NLAYERS-LAY+1) = 1.
           PMOM(1,NLAYERS-LAY+1) = XMOM(1,LAY,IBAND)
           PMOM(2,NLAYERS-LAY+1) = XMOM(2,LAY,IBAND)
           DO 3850 K = 3, NSTR
              PMOM(K,NLAYERS-LAY+1) = XMOM(K,LAY,IBAND)
 3850      CONTINUE
 3900   CONTINUE

         CALL DISORT_LW( NLAYERS, FRACSREV, 
     &        TAUREV, SSALB, NSTR, PMOM, 
     &        TZREV, wavenumlo,wavenumhi,
     &        USRTAU, NTAU, UTAU, NSTR, 
     &        USRANG, NUMU, UMU, NPHI, PHI, IBCND, FBEAM, 
     &        UMU0, PHI0, FISOT, LAMBER, ALBEDO, BTEMP, TTEMP,
     &        TEMIS, PLANK, ONLYFL, ACCUR, PRNT, 
     &        HEADER, MAXCLY, MAXULV, MAXUMU, MAXPHI, MAXCMU,
     &        RFLDIR, RFLDN, FLUP, 
     &        DFDT, UAVG, UU, ALBMED, TRNMED )

         DO 3950 LEV = NLAYERS, 0, -1
               TOTUFLUX(LEV) = TOTUFLUX(LEV) + FLUP(NLAYERS-LEV+1)
               TOTDFLUX(LEV) = TOTDFLUX(LEV)+RFLDIR(NLAYERS-LEV+1)
     &             + RFLDN(NLAYERS-LEV+1) 
 3950    CONTINUE

         IF (RFLDIR(1) .GT. 1.e-5 .OR. RFLDN(1) .GT. 1.e-5) 
     &        WRITE(*,9000) IBAND, IG, RFLDN(1)

         IG = IG + 1
         IF (IG .LE. NG(IBAND)) GO TO 1000
            
 6000 CONTINUE

      FNET(NLAYERS) = TOTUFLUX(NLAYERS) - TOTDFLUX(NLAYERS)
      HTR(NLAYERS) = 0.
      DO 3951 LEV = NLAYERS-1, 0, -1
         FNET(LEV) = TOTUFLUX(LEV) - TOTDFLUX(LEV)
         HTR(LEV) = HEATFAC * (FNET(LEV) -FNET(LEV+1)) /
     &        (PZ(LEV) - PZ(LEV+1))
 3951 CONTINUE

 9000 FORMAT('DOWNWARD FLUX AT TOA GTR THAN 0. IN BAND ',i2,
     & 'AT IG =',i2,'. POSSIBLE',/,
     &'INSTABILITY IN DISORT, TRY INCREASING NUMBER OF STREAMS.',
     &     e15.7)

      RETURN
      END   
