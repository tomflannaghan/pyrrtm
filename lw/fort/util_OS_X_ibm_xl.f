C     path:      %P%
C     revision:  $Revision: 7.1 $
C     created:   $Date: 2006/06/01 16:25:28 $  
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
c       Returns the date, in MM/DD/YY format, in a 8 byte variable.
c
c PROCEDURE:
c       Use IBM XL Fortran Service and Utility Procedure, DATE, to retur
c       the current date in MM/DD/YY format. This value is then read int
c       the double precision variable, hdate, using an internal read
c       statement.
c
c=======================================================================

      SUBROUTINE lbldat( hdate )


c     ---------
c     Arguments
c     ---------

      character*8 hdate


c     ---------------
c     Local variables
c     ---------------

      CHARACTER*8 d
      character*2 hmonth,hday,hyear

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
      HNAMUTL= '     util_ibm_xl.f:'
      HVRUTL = '$Revision: 7.1 $' 

c     --------------------
c     Get the current date
c     --------------------

      d = DATE()

c     ---------------------------------------
c     Read the character date into the double
c     precision variable argument.
c     ---------------------------------------

      READ  ( d,    900 ) hmonth,hday,hyear

      write ( hdate,905 ) hyear,hmonth,hday

 900  format(a2,1x,a2,1x,a2)
 905  format(a2,'/',a2,'/',a2)

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
c       Returns the time, in HH:MM:SS format, in an 8 byte character var
c
c CALLING SEQUENCE:
c       CALL ftime( htime )
c
c PROCEDURE:
c       Use XLF Fortran Service and Utility Procedure, CLOCK_, to return
c       the current time in HH:MM:SS format. This value is then read int
c       the double precision variable, htime, using an internal read
c       statement.
c
c=======================================================================

      SUBROUTINE ftime( htime )

c     ---------
c     Arguments
c     ---------

      CHARACTER*8 htime


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

c=======================================================================


      SUBROUTINE CPUTIM (TIME)                                          
C                                                                       
      REAL*4 time_r4
C                                                                       

c     The CPU_TIME xlf system subroutine returns the CPU time, in second
c     taken by the current process and, possibly, all the child processe
c     in all of the threads. A call to CPU_TIME will give the processor 
c     taken by the process from the start of the program. The time measu
c     only accounts for the amount of time that the program is actually 
c     and not the time that a program is suspended or waiting.

      call cpu_time(time_r4)
      time = time_r4
C                                                                       
      RETURN                                                            
C                                                                       
      END                                                               


c=======================================================================

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

