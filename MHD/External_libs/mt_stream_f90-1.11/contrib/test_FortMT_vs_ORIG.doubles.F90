

!  Test program by Michael S. Briggs, 2010 July 20.
!  Compares and tests the output of the Multi-Stream Mersenne Twister Fortran
!  random number generator of Ken-Ichi Ishikawa,
!  http://theo.phys.sci.hiroshima-u.ac.jp/~ishikawa/PRNG/mt_stream_en.html,
!  to the original (non-multi-stream) Mersenne Twister C version
!  mt19937ar.c of Takuji Nishimura and Makoto Matsumoto (2002/1/26),
!  http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/CODES/mt19937ar.c.
!  This version compares random doubles.
!  Specifically, doubles of 53-bit resolution over [0,1).

! Copyright (c) 2010, Michael S. Briggs, [michael.s.briggs[at]nasa.gov]
! All rights reserved.
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
! 
! * Redistributions of source code must retain the above copyright
!   notice, this list of conditions and the following disclaimer. 
!   
! * Redistributions in binary form must reproduce the above copyright
!   notice, this list of conditions and the following disclaimer listed
!   in this license in the documentation and/or other materials
!   provided with the distribution.
!   
! * Neither the name of the copyright holders nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!   
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT  
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 


program test_Fortran_MT_doubles

   use MSB_kind_defs
   
#ifdef _DEBUG_
   use mt_stream_debug
#else
   use mt_stream
#endif
   
   use iso_c_binding
   
   implicit none
   
   
   interface init_genrand_interf
   
      subroutine init_genrand ( iseed ) bind (C, name="init_genrand")
      
         use iso_c_binding
         
         integer (c_long), intent (in), value :: iseed
      
      end subroutine init_genrand
      
   end interface init_genrand_interf
   
   
   interface genrand_res53_interf
      
      function genrand_res53 () bind (C, name="genrand_res53")
      
         use iso_c_binding
         
         real (c_double) :: genrand_res53
      
      end function genrand_res53
   
   end interface genrand_res53_interf
   
   
   integer, parameter :: MAX_STREAM = 500
   
#ifdef _DEBUG_
   integer, parameter :: STREAM_LEN = 2**16
#else
   integer, parameter :: STREAM_LEN = 2**256  ! this case is endless....
#endif
   
   integer, parameter :: COMPARE_LEN = 100
   
   

   
   type (mt_state) :: mts (0: MAX_STREAM)
   
   integer (RegularInt_K) :: iseed = 73519232
   integer (c_long) :: jseed
   ! on 64bit  LP64 data model, c_long == 64 bits, /= integer(SELECTED_INT_KIND(9)). this case requires data conversion.
   ! on 64bit LLP64 data model, c_long == 32 bits, == integer(SELECTED_INT_KIND(9))
   ! on 32bit LP32/ILP32 data model, c_long == 32 bits, == integer(SELECTED_INT_KIND(9))
   
   integer :: id
   integer :: j
   
   real (DoubleReal_K) :: RanNum
   
   real (DoubleReal_K) :: CompareBegin_OrigC (COMPARE_LEN, MAX_STREAM)
   real (DoubleReal_K) :: CompareEnd_OrigC (COMPARE_LEN, MAX_STREAM)
   
   real (DoubleReal_K) :: CompareBegin_MasterMT (COMPARE_LEN, MAX_STREAM)
   real (DoubleReal_K) :: CompareEnd_MasterMT (COMPARE_LEN, MAX_STREAM)
   
   logical :: ErrorFound = .FALSE.
   
   real ::  beg_cpu_time, end_cpu_time
   
   integer :: clck_counts_beg, clck_counts_end, clck_rate
   
   
   !  OrigC version:
   
!   call init_genrand ( iseed )
   jseed = iseed
   call init_genrand ( jseed )
   
   write (*,*) "Original C version:"
   
   call cpu_time (beg_cpu_time)
   
   !  OrigC is single-stream, but sequentially march through it using
   !  stream segmentation notation to obtain the comparison values:
  
   !  advance to the start of stream 1
   do j=1, STREAM_LEN / 2
      RanNum = genrand_res53 ()
   end do
   
   do id=1, MAX_STREAM
   
     do j=1, COMPARE_LEN
        CompareBegin_OrigC (j, id) = genrand_res53 ()
     end do
     
     !  genrand_doublex consumes two 32-bit integers per call, so...
     do j=1, STREAM_LEN / 2 - 2 * COMPARE_LEN        
        RanNum = genrand_res53 ()
     end do
     
     do j=1, COMPARE_LEN
        CompareEnd_OrigC (j, id) = genrand_res53 ()
     end do     
   
   end do
   
   call cpu_time (end_cpu_time)
   
   write (*, *) "CPU time OrigC is", end_cpu_time - beg_cpu_time
   
   
   !  Fortran version:
   
   ! set parameters
   call set_mt19937
   
   !  initialize MT state type
   call new ( mts (0) )
   
   !  set state and initialize state (of master stream, id=0)
   call init  (mts (0), iseed)   ! scaler seed initialize
   
   !  initialize additional streams from the master
   do id=1, MAX_STREAM
      call create_stream ( mts (0), mts (id), id )
   end do


   !  Sequentially march through the zeroth (aka starting or master)
   !  Mersenne Twister Stream using stream segmentation notation to obtain
   !  comparison values:
   
   call cpu_time (beg_cpu_time)

   !  advance to the start of stream 1
   do j=1, STREAM_LEN / 2
      RanNum = genrand_double2 ( mts (0) )
   end do

   do id=1, MAX_STREAM

     do j=1, COMPARE_LEN   
        CompareBegin_MasterMT (j, id) =  genrand_double2 ( mts (0) )
     end do
     
     !  genrand_doublex consumes two 32-bit integers per call, so...
     do j=1, STREAM_LEN / 2 - 2 * COMPARE_LEN
        RanNum = genrand_double2 ( mts (0) )
     end do
     
     do j=1, COMPARE_LEN   
        CompareEnd_MasterMT (j, id) = genrand_double2 ( mts (0) )
     end do     
   
   end do
   
   call cpu_time (end_cpu_time)
   
   write (*, *) "CPU time sequential Mersenee Twister is", end_cpu_time - beg_cpu_time   


   !  Now obtain numbers from the individual streams, except the zeroth,
   !  and test them against the expected values.
   !  (The zeroth stream is the reference "master" stream, and we have
   !  already advanced through it.)
   
   call system_clock ( clck_counts_beg, clck_rate )
   
   call cpu_time (beg_cpu_time)
   
   do id=1, MAX_STREAM
   
     do j=1, COMPARE_LEN
     
        RanNum = genrand_double2 ( mts (id) )
        
        if ( RanNum /= CompareBegin_OrigC (j, id) ) then
           write (*, *) 'Error 1', id, j, RanNum, CompareBegin_OrigC (j, id)
           ErrorFound = .TRUE.
        end if
        if ( RanNum /= CompareBegin_MasterMT (j, id) ) then
           write (*, *) 'Error 2', id, j, RanNum, CompareBegin_MasterMT (j, id)
           ErrorFound = .TRUE.
        end if
        
     end do
     
     !  genrand_double2 consumes two 32-bit integers per call, so...
     do j=1, STREAM_LEN / 2 - 2 * COMPARE_LEN
        RanNum = genrand_double2 ( mts (id) )
     end do
     
     do j=1, COMPARE_LEN
     
        RanNum = genrand_double2 ( mts (id) )
        
        if ( RanNum /= CompareEnd_OrigC (j, id) ) then
           write (*, *) 'Error 3', id, j, RanNum, CompareEnd_OrigC (j, id)
           ErrorFound = .TRUE.
        end if
        if ( RanNum /= CompareEnd_MasterMT (j, id) ) then
           write (*, *) 'Error 4', id, j, RanNum, CompareEnd_MasterMT (j, id)
           ErrorFound = .TRUE.
        end if
       
     end do     
   
   end do
   
   call cpu_time (end_cpu_time)
   
   call system_clock ( clck_counts_end, clck_rate )
   
   write (*, *) "CPU time Multi-Steam Mersenee Twister is", end_cpu_time - beg_cpu_time 
   
   write (*, *) "clock rate is", clck_rate
   
   write (*, *) "clock time is", (clck_counts_end - clck_counts_beg) / real (clck_rate, SingleReal_K)
   
   
   
   if ( .NOT. ErrorFound )  &
      write (*, '( // "ALL OK!  Consistency check C mt19937ar and Mersenne sequential versus Mersenne MultiStream!" )' )


   ! Done -- free memory

   do id=0, MAX_STREAM
      call delete ( mts (id) )
   end do
   
   
   stop

end program test_Fortran_MT_doubles
