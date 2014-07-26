C     path:      %P%
C     revision:  $Revision: 7.02 $
C     created:   $Date: 2003/02/10 21:21:17 $  
C     presently: %H%  %T%
C     --------------------------------------------------------------
      SUBROUTINE LBLDAT(HDATE)                                          
C                                                                       
      EXTERNAL IDATE
C
      character*8 hdate
C                                                                       
      CHARACTER GDATE*10                                                
C                                                                       
      INTEGER*4 IARRAY(3)                                               
C                                                                       
      COMMON /CVRUTL/ HNAMUTL,HVRUTL
C
      CHARACTER*18 HNAMUTL,HVRUTL
C
C     ASSIGN NAME and CVS VERSION NUMBER TO MODULE 
C
      HNAMUTL= '     util_alpha.f:'
      HVRUTL = '$Revision: 7.02 $' 

      CALL IDATE(IARRAY)                                                

      IARRAY(3)=iarray(3)-(int(iarray(3)/100))*100
C
      WRITE (GDATE,900) IARRAY(3),IARRAY(2),IARRAY(1)                   
C                                                                       
      READ (GDATE,901) HDATE                                            
C                                                                       
      RETURN                                                            
C                                                                       
  900 FORMAT (   I2.2,2('/',I2.2))
  901 FORMAT (A8)                                                       
C                                                                       
      END                                                               
C
C     --------------------------------------------------------------
      SUBROUTINE FTIME (HTIME)                                          
C                                                                       
      CHARACTER*8 htime
C                                                                       
      CHARACTER GTIME*10                                                
C                                                                       
      INTEGER*4 IARRAY(5)                                               
C                                                                       
      CALL ITIME (IARRAY)                                               
      WRITE (GTIME,900) IARRAY(1),IARRAY(3),IARRAY(5)                   
C                                                                       
      READ (GTIME,901) HTIME                                            
C                                                                       
      RETURN                                                            
C                                                                       
  900 FORMAT (I2,2(':',I2.2))                                           
  901 FORMAT (A8)                                                       
C                                                                       
      END                                                               
C
C     --------------------------------------------------------------
      SUBROUTINE CPUTIM (TIME)                                          
C                                                                       
      COMMON /TIMIN/ A1                                                 
C                                                                       
      REAL*4 ETIME,TARRAY(2)                                            
C                                                                       
C     THIS SUBROUTINE OBTAINS CPU TIME                                  
C                                                                       
      IF (A1.LE.0.) THEN                                                
         TIME = ETIME(TARRAY)                                           
      ELSE                                                              
         TIME = ETIME(TARRAY)                                           
      ENDIF                                                             
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
C
C     --------------------------------------------------------------
      BLOCK DATA BTIM                                                   
C                                                                       
      COMMON /TIMIN/ A1                                                 
C                                                                       
      DATA A1 / 0. /                                                    
C                                                                       
      END                                                               


C     --------------------------------------------------------------
      SUBROUTINE BUFIN (IFILE,IEOF,IARRAY,IWORDS)
C
C     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING
C     AT LOCATION IARRAY
C
C     IFILE IS THE FILE DESIGNATION
C                                                                       
      DATA i_4 / 4 /    
C                          
      DIMENSION IARRAY(IWORDS)

      IEOF = 1             
C                          
      READ (IFILE,END=10) IARRAY
      ITEST = MIN(IWORDS,i_4)                 
      IF (IARRAY(ITEST).EQ.-99) IEOF = -99      
C                                               
      RETURN                                    
C                                               
   10 IEOF = 0                                  
C                                               
      RETURN                                    
C                                               
      END                                       
C
C     --------------------------------------------------------------
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c         note the name change

      SUBROUTINE BUFIN_sgl(IFILE,IEOF,IARRAY,IWORDS)
C
C     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING
C     AT LOCATION IARRAY
C
C     IFILE IS THE FILE DESIGNATION
C                                  

      implicit integer*4 (i-n)
      implicit real*4    (a-h,o-z)

      DATA i_4 / 4 /    

      DIMENSION IARRAY(IWORDS)
C                          
      IEOF = 1             
C                          
C#    BUFFER IN (IFILE,1) (IARRAY(ILO),IARRAY(IHI))
C#    IF (UNIT(IFILE).EQ.0.) GO TO 10              
C                                               
      READ (IFILE,END=10) IARRAY
      ITEST = MIN(IWORDS,i_4)                 
      IF (IARRAY(ITEST).EQ.-99) IEOF = -99      
C                                               
      RETURN                                    
C                                               
   10 IEOF = 0                                  
C                                               
      RETURN                                    
C                                               
      END                                       
c_______________________________________________________________________

      SUBROUTINE BUFOUT (IFILE,IARRAY,IWORDS)
C                                                 
C     THIS SUBROUTINE BUFFERS OUT (WRITES) IWORDS FROM IARRAY STARTING
C     AT LOCATION IARRAY                                                
C                                                                     
C     IFILE IS THE FILE DESIGNATION                                   
C                                                                     
      DIMENSION IARRAY(IWORDS)
C                                                   
      WRITE (IFILE) IARRAY
C                                                    
      RETURN                                         
C                                                    
      END                                            
C
c_______________________________________________________________________

      SUBROUTINE BUFOUT_sgl(IFILE,IARRAY,IWORDS)
C                                                 
C     THIS SUBROUTINE BUFFERS OUT (WRITES) IWORDS FROM IARRAY STARTING
C     AT LOCATION IARRAY                                                
C                                                                     
C     IFILE IS THE FILE DESIGNATION                                   
C                                                                     
c
      implicit integer*4 (i-n)
      implicit real*4    (a-h,o-z)
c
      DIMENSION IARRAY(IWORDS)
C                                                   
C#    BUFFER OUT (IFILE,1) (IARRAY(ILO),IARRAY(IHI))
C#    IF (UNIT(IFILE).EQ.0.) STOP ' ERROR IN BUFOUT '
C                                                    
      WRITE (IFILE) IARRAY
C                                                    
      RETURN                                         
C                                                    
      END                                            
