C     path:      %P%
C     revision:  $Revision: 7.03 $
C     created:   $Date: 2007/02/07 13:43:44 $  
C     presently: %H%  %T%
c_______________________________________________________________________

      SUBROUTINE LBLDAT(HDATE)                                          
C                                                                       
      character*8 hdate
      integer*4 iday,imon,iyear
C                                                                       
      CHARACTER GDATE*8

      COMMON /CVRUTL/ HNAMUTL,HVRUTL
C
      CHARACTER*18 HNAMUTL,HVRUTL
C
C     ASSIGN NAME and CVS VERSION NUMBER TO MODULE 
C
      HNAMUTL= '       util_dos.f:'
      HVRUTL = '$Revision: 7.03 $' 

C                                                                       
      CALL IDATE(iday,imon,iyear) 
c
      iyear=iyear-(100*(iyear/100))
c
      WRITE (GDATE,900) iyear,imon,iday
C                                                                       
      READ (GDATE,901) HDATE                                            
C                                                                       
C     GDATE AND FORMAT ARE FOR CYBER AND CRAY                           
C                                                                       
C       -- CYBER REQUIRES FORMAT (1X,A8)                                
C       -- CRAY  REQUIRES FORMAT (A8)                                   
C                                                                       
C     CHANGE THESE TO WORD SIZE AND FORMAT OF ROUTINE DATE              
C                                                                       
      RETURN                                                            
C                                                                       
  900 FORMAT (   I2.2,2('/',I2.2))                                      
C>901 FORMAT (1X,A8)                                                    
  901 FORMAT (A8)                                                       
C                                                                       
      END                                                               
      SUBROUTINE FTIME (HTIME)                                          
C                                                                       
C                                                                       
      CHARACTER*8 gtime,htime
C                                                                       
      INTEGER*4 IARRAY(3)                                               
C                                                                       
C>>   CALL CLOCK (GTIME)                                                
C>VAX CALL TIME (GTIME)                                                 
      CALL ITIME (IARRAY)                                               
c
      WRITE (GTIME,900) IARRAY                                          
C                                                                       
      READ (GTIME,901) HTIME                                            
C                                                                       
C     GTIME AND FORMAT ARE FOR CYBER AND CRAY                           
C                                                                       
C       -- CYBER REQUIRES FORMAT (1X,A8)                                
C       -- CRAY  REQUIRES FORMAT (A8)                                   
C                                                                       
C     CHANGE THESE TO WORD SIZE AND FORMAT OF ROUTINE GTIME             
C                                                                       
      RETURN                                                            
C                                                                       
C>900 FORMAT (1X,I2,2(':',I2.2))                                        
  900 FORMAT (   I2,2(':',I2.2))                                        
C>901 FORMAT (1X,A8)                                                    
  901 FORMAT (A8)                                                       
C                                                                       
      END                                                               
      SUBROUTINE CPUTIM (TIME)                                          
C                                                                       
      COMMON /TIMIN/ A1                                                 
C                                                                       
      REAL*4 ETIME,TARRAY(2)                                            
C                                                                       
C     THIS SUBROUTINE OBTAINS CPU TIME                                  
C                                                                       
      IF (A1.LE.0.) THEN                                                
C>>      CALL SECOND (TIME)                                             
C>VAX    A1 = SECNDS(0.0)                                               
         A1 =  ETIME(TARRAY)                                            
         TIME = a1
      ELSE                                                              
C>>      CALL SECOND (TIME)                                             
C>VAX    TIME = SECNDS(A1)                                              
         TIME = ETIME(TARRAY)                                           
      ENDIF                                                             
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               
      BLOCK DATA BTIM                                                   
C                                                                       
      COMMON /TIMIN/ A1                                                 
C                                                                       
      DATA A1 / 0. /                                                    
C                                                                       
      END                                                               

      SUBROUTINE BUFIN (IFILE,IEOF,IARRAY,IWORDS)
C
C     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING
C     AT LOCATION IARRAY
C
C     IFILE IS THE FILE DESIGNATION
C                                  
      DATA i_4 / 4 /    
c
      DIMENSION IARRAY(IWORDS)
C                                                                       
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
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c         note the name change

      SUBROUTINE BUFIN_sgl (IFILE,IEOF,IARRAY,IWORDS)
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
C#    BUFFER OUT (IFILE,1) (IARRAY(ILO),IARRAY(IHI))
C#    IF (UNIT(IFILE).EQ.0.) STOP ' ERROR IN BUFOUT '
C                                                    
      WRITE (IFILE) IARRAY
C                                                    
      RETURN                                         
C                                                    
      END                                            
c_______________________________________________________________________

      SUBROUTINE BUFOUT_sgl (IFILE,IARRAY,IWORDS)
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
