c     Library interface for rrtm_sw. Run in the following order.
c
c     INITRRTM(ISTRM, IDELM, ICOS, JULDAT, SZA, ISOLVAR, SOLVAR)
c     INITSURFACE(IEMIS, IREFLECT, SEMISS)
c     INITPROFILE(NLAYERS, TAVEL, PAVEL, TZ, PZ, NMOL, WKL, WBRODL)
c     EXECRUN()
c



      SUBROUTINE EXECRUN
c     Executes the run.
      INCLUDE 	'param.f'
      COMMON /CONTROL/ IAER,NSTR,IOUT,ISTART,IEND,ICLD,idelm,isccos
      COMMON /OUTPUT/    TOTUFLUX(0:MXLAY,0:nbands), 
     &                   TOTDFLUX(0:MXLAY,0:nbands),
     &                   DIFDOWNFLUX(0:MXLAY,0:nbands), 
     &                   DIRDOWNFLUX(0:MXLAY,0:nbands),
     &                   FNET(0:MXLAY,0:nbands), 
     &                   HTR(0:MXLAY,0:nbands) 

C     ***    Calculate information needed by the radiative transfer routine
C     that is specific to this atmosphere, especially some of the 
C     coefficients and indices needed to compute the optical depths
C     by interpolating data from stored reference atmospheres. 
      ICLDATM = 0
      IF (ICLD .EQ. 1) CALL CLDPROP(ICLDATM)
      
      CALL SETCOEF
      
C     ***    Call the radiative transfer routine.
      CALL RTRDIS
      
      RETURN
      END

C

      SUBROUTINE INITRRTM(ISTRMINP, IDELMINP, ICOSINP, JULDAT, 
     $     SZA, ISOLVAR, SOLVAR)
                                                                         
      INCLUDE 	'param.f'
      PARAMETER (MXMOL = 38)
      PARAMETER (MAXINPX=35)
      PARAMETER (MAXXSEC=4)
C      PARAMETER (MAXPROD = MXLAY*MAXXSEC)

      DIMENSION ALTZ(0:MXLAY),IXTRANS(14)

      REAL SOLVAR(IB1:IB2)
      INTEGER ISTRMINP, IDELMINP, ICOSINP, ISOLVAR
      REAL SZA, JULDAT

      COMMON /CONSTANTS/ FLUXFAC,HEATFAC
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,
     *                RADCN1,RADCN2 
      COMMON /FEATURES/  NG(IB1:IB2),NSPA(IB1:IB2),NSPB(IB1:IB2)
      COMMON /PRECISE/   ONEMINUS
      COMMON /CONTROL/   IAER, NSTR, IOUT, ISTART, IEND, ICLD,
     &                   idelm, isccos
      COMMON /SWPROP/    ZENITH, ALBEDO, ADJFLUX(NBANDS)
      COMMON /SURFACE/  IREFLECT,SEMISS(NBANDS)
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                  PZ(0:MXLAY),TZ(0:MXLAY),TBOUND
      COMMON /SPECIES/  COLDRY(MXLAY),WKL(MXMOL,MXLAY),WBRODL(MXLAY),
     &                  COLMOL(MXLAY),NMOL
      COMMON /IFIL/     IRD,IPR,IPU,IDUM(15)
      COMMON /XSECCTRL/ NXMOL,IXINDX(MAXINPX)
      COMMON /XSEC/     WX(MAXXSEC,MXLAY)
      COMMON /PATHX/    IXMAX,NXMOL0,IXINDX0(MAXINPX),WX0(MAXINPX,MXLAY)    
      COMMON /XRRTATM/  IXSECT

      ONEMINUS = 1. - 1.E-6
      PI = 2.*ASIN(1.)
      FLUXFAC = PI * 2.D4  

      IWR = 10

C  Initialize molecular amount and cross section arrays to zero here.
      
      DO 1200 ILAY = 1,MXLAY
         DO 1100 ISP = 1,35
 1100       WKL(ISP,ILAY) = 0.0
         DO 1150 ISP = 1,MAXXSEC
 1150       WX(ISP,ILAY) = 0.0
 1200 CONTINUE

      IXMAX = MAXINPX

c     Initialization of the control variables.
      IAER = 0
      IATM = 0
      ISCAT = 0
      ISTRM = ISTRMINP
      IOUT = 0
      ICLD = 0
      IDELM = IDELMINP
      ICOS = ICOSINP

c     Validates input.
      if (idelm.gt.1 .or. idelm.lt.0 .or. icos.gt.2 .or. icos.lt.0) then
         print *,'INVALID MEASUREMENT COMPARISON FLAG'
         stop
      endif
      isccos = icos

C     No cross-sections implemented in shortwave.
      IXSECT = 0
      IF (ISTRM .EQ. 0) THEN 
         NSTR = 4
      ELSE IF  (ISTRM .EQ. 1) THEN
         NSTR = 8
      ELSE IF  (ISTRM .EQ. 2) THEN
         NSTR = 16
      ELSE 
         PRINT *, 'INVALID VALUE FOR ISTRM'
         STOP
      ENDIF

C     Set up the sun!

      ZENITH = COS(SZA * PI / 180.)
      IF (JULDAT .EQ. 0) THEN
         ADJFLUX_JD = 1.
      ELSE
         ADJFLUX_JD = EARTH_SUN (JULDAT)
      ENDIF

      IF (ISOLVAR .EQ. 0) THEN
         DO 1400 IB = IB1,IB2
            ADJFLUX(IB) = ADJFLUX_JD
 1400    CONTINUE
      ELSEIF (ISOLVAR .EQ. 1) THEN
         DO 1450 IB=IB1,IB2
            ADJFLUX(IB) = ADJFLUX_JD * SOLVAR(IB1)
 1450    CONTINUE
      ELSEIF (ISOLVAR .EQ. 2) THEN
         DO 1475 IB=IB1,IB2
            ADJFLUX(IB) = ADJFLUX_JD * SOLVAR(IB)
 1475    CONTINUE
      ELSE
         PRINT *, 'ISOLVAR = ', ISOLVAR, ' NOT A VALID INPUT VALUE'
         STOP
      ENDIF

      IOUT = 0
      IFLAG = IOUT
      
      IF (IFLAG .GT. 0 .AND. IFLAG .LE. IB2) THEN
         ISTART = IFLAG
         IEND = IFLAG
         IPBAND = IFLAG
      ELSE
         ISTART = IB1
         IEND = IB2
         IPBAND = 0
         IFLAG = IOUT
      ENDIF


      RETURN
      END


      SUBROUTINE INITSURFACE(IEMIS, IREFLECTINP, SEMISSINP)
                                                                         
      INCLUDE 	'param.f'
      PARAMETER (MXMOL = 38)
      PARAMETER (MAXINPX=35)
      PARAMETER (MAXXSEC=4)

C     Local argument definitions
      INTEGER IEMIS
      INTEGER IREFLECTINP
      REAL SEMISSINP
      DIMENSION SEMISSINP(IB1:IB2)

Cf2py intent(in) iemis
Cf2py intent(in) ireflectinp
Cf2py intent(in) semissinp
      
      COMMON /CONSTANTS/ FLUXFAC,HEATFAC
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,
     *                RADCN1,RADCN2 
      COMMON /FEATURES/  NG(IB1:IB2),NSPA(IB1:IB2),NSPB(IB1:IB2)
      COMMON /PRECISE/   ONEMINUS
      COMMON /CONTROL/   IAER, NSTR, IOUT, ISTART, IEND, ICLD,
     &                   idelm, isccos
      COMMON /SWPROP/    ZENITH, ALBEDO, ADJFLUX(NBANDS)
      COMMON /SURFACE/  IREFLECT,SEMISS(NBANDS)
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                  PZ(0:MXLAY),TZ(0:MXLAY),TBOUND
      COMMON /SPECIES/  COLDRY(MXLAY),WKL(MXMOL,MXLAY),WBRODL(MXLAY),
     &                  COLMOL(MXLAY),NMOL
      COMMON /IFIL/     IRD,IPR,IPU,IDUM(15)
      COMMON /XSECCTRL/ NXMOL,IXINDX(MAXINPX)
      COMMON /XSEC/     WX(MAXXSEC,MXLAY)
      COMMON /PATHX/    IXMAX,NXMOL0,IXINDX0(MAXINPX),WX0(MAXINPX,MXLAY)    
      COMMON /XRRTATM/  IXSECT

      IF (IEMIS .EQ. 0) THEN
         DO 1500 IB = IB1, IB2
            SEMISS(IB) = 1.
 1500    CONTINUE
      ELSEIF (IEMIS .EQ. 1) THEN
         DO 1600 IB = IB1, IB2
            SEMISS(IB) = SEMISSINP(IB1)
 1600    CONTINUE
      ELSEIF (IEMIS .EQ. 2) THEN
         DO 1700 IB = IB1, IB2
            SEMISS(IB) = SEMISSINP(IB)
 1700    CONTINUE
      ELSE
          PRINT *, 'IEMIS = ', IEMIS, ' NOT A VALID INPUT VALUE'
          STOP
      ENDIF

      RETURN
      END


      SUBROUTINE INITPROFILE(NLAYERSINP, TAVELINP, PAVELINP, 
     $     TZINP, PZINP, NMOLINP, WKLINP, WBRODLINP)
                                                                         
      INCLUDE 	'param.f'
      PARAMETER (MXMOL = 38)
      PARAMETER (MAXINPX=35)
      PARAMETER (MAXXSEC=4)

C     Local argument definitions
      INTEGER NLAYERSINP
      INTEGER NMOLINP
      REAL TAVELINP(NLAYERSINP)
      REAL PAVELINP(NLAYERSINP)
      REAL PZINP(0:NLAYERSINP)
      REAL TZINP(0:NLAYERSINP)
      REAL WKLINP(NMOLINP,NLAYERSINP)
      REAL WBRODLINP(NLAYERSINP)

Cf2py intent(in) NLAYERS
Cf2py intent(in) TAVELINP
Cf2py intent(in) PAVELINP
Cf2py intent(in) TZINP
Cf2py intent(in) PZINP
Cf2py intent(in) NMOLINP
Cf2py intent(in) WKLINP
Cf2py intent(in) WBRODLINP
      
      COMMON /CONSTANTS/ FLUXFAC,HEATFAC
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,
     *                RADCN1,RADCN2 
      COMMON /FEATURES/  NG(IB1:IB2),NSPA(IB1:IB2),NSPB(IB1:IB2)
      COMMON /PRECISE/   ONEMINUS
      COMMON /CONTROL/   IAER, NSTR, IOUT, ISTART, IEND, ICLD,
     &                   idelm, isccos
      COMMON /SWPROP/    ZENITH, ALBEDO, ADJFLUX(NBANDS)
      COMMON /SURFACE/  IREFLECT,SEMISS(NBANDS)
      COMMON /PROFILE/  NLAYERS,PAVEL(MXLAY),TAVEL(MXLAY),
     &                  PZ(0:MXLAY),TZ(0:MXLAY),TBOUND
      COMMON /SPECIES/  COLDRY(MXLAY),WKL(MXMOL,MXLAY),WBRODL(MXLAY),
     &                  COLMOL(MXLAY),NMOL
      COMMON /IFIL/     IRD,IPR,IPU,IDUM(15)
      COMMON /XSECCTRL/ NXMOL,IXINDX(MAXINPX)
      COMMON /XSEC/     WX(MAXXSEC,MXLAY)
      COMMON /PATHX/    IXMAX,NXMOL0,IXINDX0(MAXINPX),WX0(MAXINPX,MXLAY)    
      COMMON /XRRTATM/  IXSECT

      NMOL = NMOLINP
      NLAYERS = NLAYERSINP

      IF (NMOL.EQ.0) THEN
         WRITE (*,*) "Cannot process NMOL=0 when using binary mode"
         STOP
      ENDIF

c     Load in the input arrays as if read from file
      PZ(0) = PZINP(0)
      TZ(0) = TZINP(0)
      DO 2000 L = 1, NLAYERS
         PAVEL(L) = PAVELINP(L)
         TAVEL(L) = TAVELINP(L)
         PZ(L) = PZINP(L)
         TZ(L) = TZINP(L)
         DO 2010 M = 1, NMOL
            WKL(M,L) = WKLINP(M,L)
 2010    CONTINUE
         DO 2020 M = NMOL + 1, MXMOL
            WKL(M,L) = 0
 2020    CONTINUE
         WBRODL(L) = WBRODLINP(L)
 2000 CONTINUE                                                            

C     Test for mixing ratio input.
      IMIX = 1
      DO 3500 M = 1, NMOL
         IF (WKL(M,1) .GT. 1.0) THEN
            IMIX = 0
            GO TO 3600
         ENDIF
 3500 CONTINUE
 3600 CONTINUE

c     Process the layer arrays.
      DO 5000 L = 1, NLAYERS
         SUMMOL = 0.0
         DO 4100 IMOL = 2, NMOL
            SUMMOL = SUMMOL + WKL(IMOL,L)
 4100    CONTINUE
         IF (IMIX .EQ. 1) THEN
            COLDRY(L) = WBRODL(L) / (1. - SUMMOL)
            DO 4200 IMOL = 1, NMOL
               WKL(IMOL,L) = COLDRY(L) * WKL(IMOL,L)
 4200       CONTINUE
         ELSE
            COLDRY(L) = WBRODL(L) + SUMMOL
         ENDIF
 5000 CONTINUE
      
      RETURN
      END


*****************************************************************
	real function earth_sun (idn)

C   function to calculate  correction factor of the Earth's orbit 

C     dn	 	: Julian day
C     earth_sun_ratio 	: square of the ratio of mean to actual Earth-Sun distance    

      REAL IDN
      
	PI   = 	3.141592654
	gamma = 2.*PI*(idn-1)/365. 

c   use Iqbal's equation 1.2.1 

      earth_sun= 1.000110 + .034221 * cos(gamma) + .001289 * sin(gamma)
     $   +.000719 *cos(2.*gamma) + .000077 * sin(2.*gamma)
     
	return 
	end

*****************************************************************
      BLOCK DATA

      include 'param.f'

      COMMON /CONSTANTS/ FLUXFAC,HEATFAC
      COMMON /FEATURES/  NG(IB1:IB2),NSPA(IB1:IB2),NSPB(IB1:IB2)

      DATA WAVENUM1(16) /2600./,WAVENUM2(16) /3250./,DELWAVE(16) /650./
      DATA WAVENUM1(17) /3250./,WAVENUM2(17) /4000./,DELWAVE(17) /750./
      DATA WAVENUM1(18) /4000./,WAVENUM2(18) /4650./,DELWAVE(18) /650./
      DATA WAVENUM1(19) /4650./,WAVENUM2(19) /5150./,DELWAVE(19) /500./
      DATA WAVENUM1(20) /5150./,WAVENUM2(20) /6150./,DELWAVE(20) /1000./
      DATA WAVENUM1(21) /6150./,WAVENUM2(21) /7700./,DELWAVE(21) /1550./
      DATA WAVENUM1(22) /7700./,WAVENUM2(22) /8050./,DELWAVE(22) /350./
      DATA WAVENUM1(23) /8050./,WAVENUM2(23)/12850./,DELWAVE(23) /4800./
      DATA WAVENUM1(24)/12850./,WAVENUM2(24)/16000./,DELWAVE(24) /3150./
      DATA WAVENUM1(25)/16000./,WAVENUM2(25)/22650./,DELWAVE(25) /6650./
      DATA WAVENUM1(26)/22650./,WAVENUM2(26)/29000./,DELWAVE(26) /6350./
      DATA WAVENUM1(27)/29000./,WAVENUM2(27)/38000./,DELWAVE(27) /9000./
      DATA WAVENUM1(28)/38000./,WAVENUM2(28)/50000./,DELWAVE(28)/12000./
      DATA WAVENUM1(29)/820./,  WAVENUM2(29)/2600./, DELWAVE(29)/1780./

      DATA NG  /16,16,16,16,16,16,16,16,16,16,16,16,16,16/
      DATA NSPA /9, 9, 9, 9, 1, 9, 9, 1, 9, 1, 0, 1, 9, 1/
      DATA NSPB /1, 5, 1, 1, 1, 5, 1, 0, 1, 0, 0, 1, 5, 1/

C     HEATFAC is the factor by which one must multiply delta-flux/ 
C     delta-pressure, with flux in w/m-2 and pressure in mbar, to get 
C     the heating rate in units of degrees/day.  It is equal to 
C           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
C        =  (9.8066)(3600)(1e-5)/(1.004)
      DATA HEATFAC /8.4391/


      END
c**********************************************************************
      Block Data phys_consts
c
      COMMON /CONSTS/ PI,PLANCK,BOLTZ,CLIGHT,AVOGAD,ALOSMT,GASCON,
     *                RADCN1,RADCN2 
c
      DATA PI / 3.1415927410125732 /
c
c    Constants from NIST 01/11/2002
c
      DATA PLANCK / 6.62606876E-27 /, BOLTZ  / 1.3806503E-16 /,
     *     CLIGHT / 2.99792458E+10 /, 
     *     AVOGAD / 6.02214199E+23 /, ALOSMT / 2.6867775E+19 /,
     *     GASCON / 8.314472  E+07 /
     *     RADCN1 / 1.191042722E-12 /, RADCN2 / 1.4387752    /
c
c     Pi was obtained from   PI = 2.*ASIN(1.)                             A03980
c
c     units are genrally cgs
c
c     The first and second radiation constants are taken from NIST.
c     They were previously obtained from the relations:
c                            RADCN1 = 2.*PLANCK*CLIGHT*CLIGHT*1.E-07      A03990
c                            RADCN2 = PLANCK*CLIGHT/BOLTZ                 A04000
      end
c

