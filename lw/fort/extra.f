C     path:      $Source: /storm/rc1/cvsroot/rc/rrtm_lw/src/extra.f,v $
C     author:    $Author: jdelamer $
C     revision:  $Revision: 3.3 $
C     created:   $Date: 2010/07/07 21:10:51 $
      SUBROUTINE XSREAD (XV1,XV2)                                       
C                                                                       
      IMPLICIT DOUBLE PRECISION (V)                                     
C                                                                       
C********************************************************************** 
C     THIS SUBROUTINE READS IN THE DESIRED "CROSS-SECTION"              
C     MOLECULES WHICH ARE THEN MATCHED TO THE DATA CONTAINED            
C     ON INPUT FILE FSCDXS.                                             
C********************************************************************** 
C                                                                       
C     IFIL CARRIES FILE INFORMATION                                     
C                                                                       
      PARAMETER (MXFSC=600, MXLAY=MXFSC+3,MXZMD=6000,
     *           MXPDIM=MXLAY+MXZMD,IM2=MXPDIM-2,MXMOL=39,
     *           MX_XS=38,MXTRAC=22)
C
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,NPHDRF,NFHDRL,NPHDRL,       
     *              NLNGTH,KFILE,KPANEL,LINFIL,NFILE,IAFIL,IEXFIL,      
     *              NLTEFL,LNFIL4,LNGTH4                                
C                                                                       
C     IXMAX=MAX NUMBER OF X-SECTION MOLECULES, IXMOLS=NUMBER OF THESE   
C     MOLECULES SELECTED, IXINDX=INDEX VALUES OF SELECTED MOLECULES     
C     (E.G. 1=CLONO2), XAMNT(I,L)=LAYER AMOUNTS FOR I'TH MOLECULE FOR   
C     L'TH LAYER, ANALOGOUS TO AMOUNT IN /PATHD/ FOR THE STANDARD       
C     MOLECULES.                                                        
C                                                                       
      COMMON /PATHX/ IXMAX,IXMOLS,IXINDX(MX_XS),XAMNT(MX_XS,MXLAY)      
C                                                                       
C     COMMON BLOCKS AND PARAMETERS FOR THE PROFILES AND DENSITIES       
C     FOR THE CROSS-SECTION MOLECULES.                                  
C     XSNAME=NAMES, ALIAS=ALIASES OF THE CROSS-SECTION MOLECULES        
C                                                                       
      CHARACTER*10 XSFILE,XSNAME,ALIAS,BLANK                            
      COMMON /XSECTF/ XSFILE(6,5,MX_XS),XSNAME(MX_XS),ALIAS(4,MX_XS)    
      COMMON /XSECTR/ V1FX(5,MX_XS),V2FX(5,MX_XS),DVFX(5,MX_XS),
     *                WXM(MX_XS),NTEMPF(5,MX_XS),NSPECR(MX_XS),
     *                IXFORM(5,MX_XS),
     *                XSMASS(MX_XS),XDOPLR(5,MX_XS),NUMXS,IXSBIN        

      COMMON /CVREXT/    HNAMEXT,HVREXT

      CHARACTER*18       HNAMEXT,HVREXT
C                                                                       
      DIMENSION IXFLG(MX_XS)                                            
C                                                                       
      CHARACTER*120 XSREC                                               
      CHARACTER*1 CFLG,CASTSK,CPRCNT,CN,CF                              
      EQUIVALENCE (CFLG,XSREC)                                          
C                                                                       
      DATA CASTSK / '*'/,CPRCNT / '%'/,CN / 'N'/,CF / 'F'/              
      DATA BLANK / '          '/                                        
C                                                                       
C     T296 IS TEMPERATURE FOR INITAL CALCULATIN OF DOPPLER WIDTHS       
C                                                                       
      DATA T296 / 296.0 /                                               
C
      HVREXT = '$Revision: 3.3 $'
C                                                                       
      IXMAX = MX_XS                                                     
      DO 10 I = 1, IXMAX                                                
         XSNAME(I) = BLANK                                              
   10 CONTINUE                                                          
C                                                                       
C     READ IN THE NAMES OF THE MOLECULES                                
C                                                                       
      IF (IXMOLS.GT.7) THEN                                             
         READ (IRD,'(7A10)') (XSNAME(I),I=1,7)                          
         READ (IRD,'(8A10)') (XSNAME(I),I=8,IXMOLS)                     
      ELSE                                                              
         READ (IRD,'(7A10)') (XSNAME(I),I=1,IXMOLS)                     
      ENDIF                                                             
C                                                                       
C     Left-justify all inputed names                                    
C                                                                       
      DO 15 I=1,IXMOLS                                                  
         CALL CLJUST (XSNAME(I),10)                                     
 15   CONTINUE
C                                                                       
CPRT  WRITE(IPR,'(/,''  THE FOLLOWING MOLECULES ARE REQUESTED:'',//,    
CPRT 1    (5X,I5,2X,A))') (I,XSNAME(I),I=1,IXMOLS)                      
C                                                                       
C     MATCH THE NAMES READ IN AGAINST THE NAMES STORED IN ALIAS         
C     AND DETERMINE THE INDEX VALUE.  STOP IF NO MATCH IS FOUND.        
C     NAME MUST BE ALL IN CAPS.                                         
C                                                                       
      DO 40 I = 1, IXMOLS                                               
         DO 20 J = 1, IXMAX                                             
            IF ((XSNAME(I).EQ.ALIAS(1,J)) .OR.                          
     *          (XSNAME(I).EQ.ALIAS(2,J)) .OR.                          
     *          (XSNAME(I).EQ.ALIAS(3,J)) .OR.                          
     *          (XSNAME(I).EQ.ALIAS(4,J))) THEN                         
               IXINDX(I) = J                                            
               GO TO 30                                                 
            ENDIF                                                       
   20    CONTINUE                                                       
C                                                                       
C         NO MATCH FOUND                                                
C                                                                       
         WRITE (IPR,900) XSNAME(I)                                      
      CALL RRTMERR('extra.f:104:STOPPED IN XSREAD',29)
C                                                                       
   30    CONTINUE                                                       
         IXFLG(I) = 0                                                   
   40 CONTINUE                                                          
C                                                                       
      RETURN                                                            
C                                                                       
  900 FORMAT (/,'  THE NAME: ',A10, ' IS NOT ONE OF THE ',              
     *        'CROSS SECTION MOLECULES. CHECK THE SPELLING.')           
  905 FORMAT (/)                                                        
  910 FORMAT (A120)                                                     
  915 FORMAT (A10,2F10.4,F10.8,I5,5X,I5,A1,4X,6A10)                     
  920 FORMAT (/,'******* ERROR IN XSREAD ** MOLECULE SECLECTED -',A10,  
     *        '- HAS ',I2,' SPECTRAL REGIONS ON FILE FSCDXS, BUT THE',  
     *        ' MAXIMUM ALLOWED IS 6 *******',/)                        
  925 FORMAT (/,'******* MOLECULE SELECTED -',A10,'- IS NOT FOUND ON',  
     *        ' FILE FSCDXS *******',/)                                 
C                                                                       
      END                                                               
      BLOCK DATA BXSECT                                                 
C                                                                       
      IMPLICIT DOUBLE PRECISION (V)                                     
C                                                                       
C**   XSNAME=NAMES, ALIAS=ALIASES OF THE CROSS-SECTION MOLECULES        
C**            (NOTE: ALL NAMES ARE LEFT-JUSTIFIED)                     
C                                                                       
      PARAMETER(MX_XS=38)
      CHARACTER*10 XSFILE,XSNAME,ALIAS                                  
      COMMON /XSECTI/ XSMAX(6,5,MX_XS),XSTEMP(6,5,MX_XS),
     *                NPTSFX(5,MX_XS),NFILEX(5,MX_XS),NLIMX  
      COMMON /XSECTF/ XSFILE(6,5,MX_XS),XSNAME(MX_XS),ALIAS(4,MX_XS)
      COMMON /XSECTR/ V1FX(5,MX_XS),V2FX(5,MX_XS),DVFX(5,MX_XS),
     *                WXM(MX_XS),NTEMPF(5,MX_XS),NSPECR(MX_XS),
     *                IXFORM(5,MX_XS),  
     *                XSMASS(MX_XS),XDOPLR(5,MX_XS),NUMXS,IXSBIN    
      COMMON /XSECTS/ JINPUT,NMODES,NPANEL,NDUM,V1XS,V2XS,DVXS,NPTSXS   
C                                                                       
      DATA NMODES / 1 /,NPANEL / 0 /,V1XS / 0.0 /,V2XS / 0.0 /,         
     *     DVXS / 0.0 /,NPTSXS / 0 /                                    
      DATA XSMAX / 1140*0.0 /                                           
      DATA (ALIAS(1,I),I=1,MX_XS)/                                      
     *    'CLONO2    ', 'HNO4      ', 'CHCL2F    ', 'CCL4      ',       
     *    'CCL3F     ', 'CCL2F2    ', 'C2CL2F4   ', 'C2CL3F3   ',       
     *    'N2O5      ', 'HNO3      ', 'CF4       ', 'CHCLF2    ',       
     *    'CCLF3     ', 'C2CLF5    ', 24*' ZZZZZZZZ ' /                 
      DATA (ALIAS(2,I),I=1,MX_XS)/                                      
     *    'CLNO3     ', ' ZZZZZZZZ ', 'CFC21     ', ' ZZZZZZZZ ',       
     *    'CFCL3     ', 'CF2CL2    ', 'C2F4CL2   ', 'C2F3CL3   ',       
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'CHF2CL    ',       
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 24*' ZZZZZZZZ ' /                 
      DATA (ALIAS(3,I),I=1,MX_XS)/                                      
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'CFC21     ', ' ZZZZZZZZ ',       
     *    'CFC11     ', 'CFC12     ', 'CFC114    ', 'CFC113    ',       
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'CFC14     ', 'CFC22     ',       
     *    'CFC13     ', 'CFC115    ', 24*' ZZZZZZZZ ' /                 
      DATA (ALIAS(4,I),I=1,MX_XS)/                                      
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'F21       ', ' ZZZZZZZZ ',       
     *    'F11       ', 'F12       ', 'F114      ', 'F113      ',       
     *    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'F14       ', 'F22       ',       
     *    'F13       ', 'F115      ', 24*' ZZZZZZZZ ' /                 
C                                                                       
C     XSMASS IS MASS OF EACH CROSS-SECTION                              
C                                                                       
      DATA XSMASS/                                                      
     1      97.46     ,   79.01     ,  102.92     ,  153.82     ,       
     2     137.37     ,  120.91     ,  170.92     ,  187.38     ,       
     3     108.01     ,   63.01     ,   88.00     ,   86.47     ,       
     4     104.46     ,  154.47     ,  24*0.00 /                        
C                                                                       
      DATA V1FX / 190*0.0 /,V2FX / 190*0.0 /,DVFX / 190*0.0 /,          
     *     WXM / 38*0.0 /                                               
      DATA NTEMPF / 190*0 /,NSPECR / 38*0 /,IXFORM / 190*0 /,           
     *     NUMXS / 0 /                                                  
C                                                                       
      END                                                               
      SUBROUTINE CLJUST (CNAME,NCHAR)                                   
C                                                                       
C     THIS SUBROUTINE LEFT-JUSTIFIES THE CHARACTER CNAME                
C                                                                       
      CHARACTER*(*) CNAME                                               
      CHARACTER*25 CTEMP                                                
      CHARACTER*1  CTEMP1(25),BLANK                                     
      EQUIVALENCE (CTEMP,CTEMP1(1))                                     

      COMMON /CVREXT/    HNAMEXT,HVREXT

      CHARACTER*18       HNAMEXT,HVREXT

C                                                                       
      DATA BLANK / ' '/                                                 
C                                                                       
         CTEMP = CNAME                                                  
         JJ=0                                                           
         DO 10 J = 1, NCHAR                                             
            IF (CTEMP1(J).NE.BLANK) THEN                                
               JJ = J                                                   
               IF (JJ.EQ.1) GO TO 50                                    
               GO TO 20                                                 
            ENDIF                                                       
   10    CONTINUE                                                       
         IF (JJ .EQ. 0) GO TO 50                                        
C                                                                       
   20    KCNT = 0                                                       
         DO 30 K = JJ, NCHAR                                            
            KCNT = KCNT+1                                               
            CTEMP1(KCNT) = CTEMP1(K)                                    
   30    CONTINUE                                                       
C                                                                       
         KK = NCHAR-JJ+2                                                
         DO 40 L = KK,NCHAR                                             
            CTEMP1(L) = BLANK                                           
   40    CONTINUE                                                       
         CNAME = CTEMP                                                  
   50 CONTINUE                                                          
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      SUBROUTINE EXPINT (X,X1,X2,A)                                     
C                                                                       
C********************************************************************** 
C     THIS SUBROUTINE EXPONENTIALLY INTERPOLATES X1 AND X2 TO X BY      
C     THE FACTOR A                                                      
C********************************************************************** 
C                                                                       

      COMMON /CVREXT/    HNAMEXT,HVREXT

      CHARACTER*18       HNAMEXT,HVREXT

      IF (X1.EQ.0.0.OR.X2.EQ.0.0) GO TO 10                              
      X = X1*(X2/X1)**A                                                 
C                                                                       
      RETURN                                                            
C                                                                       
   10 X = X1+(X2-X1)*A                                                  
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
