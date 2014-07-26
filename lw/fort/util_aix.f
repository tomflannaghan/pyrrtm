C     path:      %P%
C     revision:  $Revision: 7.02 $
C     created:   $Date: 2003/02/10 21:21:16 $  
C     presently: %H%  %T%
                                           
c=======================================================================
c
c NAME:
c       lbldat
c
c CALLING SEQUENCE:
c       CALL lbldat( hdate )
c
c PURPOSE:
c       Returns the date, in MM/DD/YY format, in a double-precision 
c       variable (also 8-bytes).
c
c PROCEDURE:
c       Use AIX XL Fortran Service and Utility Procedure, DATE, to retur
c       the current date in MM/DD/YY format. This value is then read int
c       the double precision variable, hdate, using an internal read
c       statement.
c
c=======================================================================

      SUBROUTINE lbldat( hdate )


c     ---------
c     Arguments
c     ---------

      REAL*8 hdate


c     ---------------
c     Local variables
c     ---------------

      CHARACTER*8 d

c     ---------
c     Functions
c     ---------

      CHARACTER*8 DATE

      COMMON /CVRUTL/ HNAMUTL,HVRUTL
C
      CHARACTER*18 HNAMUTL,HVRUTL
C

c#######################################################################
c                     ## BEGIN EXECUTABLE CODE ##
c#######################################################################

C
C     ASSIGN NAME and CVS VERSION NUMBER TO MODULE 
C
      HNAMUTL= '       util_aix.f:'
      HVRUTL = '$Revision: 7.02 $' 




c     --------------------
c     Get the current date
c     --------------------

      d = DATE()


c     ---------------------------------------
c     Read the character date into the double
c     precision variable argument.
c     ---------------------------------------

      READ( d, '( a8 )' ) hdate


c     ----
c     Done
c     ----

      RETURN
      END





c=======================================================================
c
c NAME:
c       ftime
c
c PURPOSE:
c       Returns the time, in HH:MM:SS format, in a double-precision 
c       variable (also 8-bytes).
c
c CALLING SEQUENCE:
c       CALL ftime( htime )
c
c PROCEDURE:
c       Use AIX XL Fortran Service and Utility Procedure, CLOCK_, to ret
c       the current time in HH:MM:SS format. This value is then read int
c       the double precision variable, htime, using an internal read
c       statement.
c
c=======================================================================

      SUBROUTINE ftime( htime )


c     ---------
c     Arguments
c     ---------

      REAL*8 htime


c     ---------------
c     Local variables
c     ---------------

      CHARACTER*8 c


c     ---------
c     Functions
c     ---------

      CHARACTER*8 CLOCK_



c#######################################################################
c                     ## BEGIN EXECUTABLE CODE ##
c#######################################################################

c     --------------------
c     Get the current time
c     --------------------

      c = CLOCK_()


c     ---------------------------------------
c     Read the character time into the double
c     precision variable argument.
c     ---------------------------------------

      READ( c, '( a8 )' ) htime


c     ----
c     Done
c     ----

      RETURN
      END
      SUBROUTINE CPUTIM (TIME)                                          
C                                                                       
      COMMON /TIMIN/ A1                                                 
      INTEGER*4 CPU_TIME
C                                                                       
C     REAL*4 ETIME,TARRAY(2)                                            
C                                                                       
C     THIS SUBROUTINE OBTAINS CPU TIME                                  
C                                                                       
      IF (A1.LE.0.) THEN                                                
C>>      CALL SECOND (TIME)                                             
C>VAX    A1 = SECNDS(0.0)                                               
C        TIME = ETIME(TARRAY)                                           
         CPU_TIME = MCLOCK()
         TIME =  REAL(CPU_TIME)/100
      ELSE                                                              
C>>      CALL SECOND (TIME)                                             
C>VAX    TIME = SECNDS(A1)                                              
C        TIME = ETIME(TARRAY)                                           
         CPU_TIME = MCLOCK()
         TIME =  REAL(CPU_TIME)/100
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
      COMMON /CVRUTL/ HVRUTL
C
      CHARACTER*15 HVRUTL
C
      DATA i_4 / 4 / 
C
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
C#    BUFFER OUT (IFILE,1) (IARRAY(ILO),IARRAY(IHI))
C#    IF (UNIT(IFILE).EQ.0.) STOP ' ERROR IN BUFOUT '
C                                                    
      WRITE (IFILE) IARRAY
C                                                    
      RETURN                                         
C                                                    
      END                                            
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

