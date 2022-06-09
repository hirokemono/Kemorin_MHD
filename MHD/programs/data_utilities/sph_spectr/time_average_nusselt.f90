!>@file   time_average_nusselt.f90
!!@brief  module time_average_nusselt
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of Nusselt number file
!!
!!@verbatim
!!      integer(c_int) function                                         &
!!    &     time_average_nusselt_c(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine time_average_nusselt_f                               &
!!     &         (file_prefix, start_time, end_time)
!!        character(len=kchara), intent(in) :: file_prefix
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!@endverbatim
!
      module time_average_nusselt
!
      implicit  none
!
      private :: c_to_fstring
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      function c_to_fstring(string)
!
      use Iso_C_binding
!
      Character(1,C_char),Intent(In) :: string(*)
      Character(:,C_char),Allocatable :: c_to_fstring
!
      Integer i,len
      len = 1
      Do While (string(len)/=C_null_char)
        len = len + 1
      End Do
      len = len - 1
      Allocate(Character(len,C_char) :: c_to_fstring)
      Do i=1,len
        c_to_fstring(i:i) = string(i)
      End Do
!
      end function c_to_fstring
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_average_nusselt_c(cname, cstart, cend) Bind(C)
!
      use Iso_C_binding
      use m_precision
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: file_prefix
!
      write(file_prefix,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call time_average_nusselt_f(file_prefix, start_time, end_time)
!
      time_average_nusselt_c = 0
      end function time_average_nusselt_c
!
! -------------------------------------------------------------------
!
      subroutine time_average_nusselt_f                                 &
     &         (file_prefix, start_time, end_time)
!
      use m_precision
      use m_constants
!
      use t_no_heat_Nusselt
!
      implicit  none
!
      character(len=kchara), intent(in) :: file_prefix
      real(kind = kreal), intent(in) :: start_time, end_time

      type(nusselt_number_data), save :: Nu_t
!
      real(kind = kreal) :: prev_Nu(2) = 0.0d0
      real(kind = kreal) :: ave_Nu(2)
      real(kind = kreal) :: sdev_Nu(2)
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: i_step, ierr, icou, i
      real(kind = kreal) :: acou, time, prev_time
      real(kind = kreal) :: true_start
!
!
       Nu_t%Nusselt_file_head = trim(file_prefix)
      call open_read_no_heat_source_Nu(id_pick, Nu_t)
      write(*,*) 'Nu_t%Nusselt_file_head: ', Nu_t%Nusselt_file_head
      write(*,*) 'start_time: ', start_time, end_time
!
!       Evaluate time average
!
      icou = 0
      time = start_time
      prev_time = start_time
      ave_Nu(1:2) = 0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' averaging finished. Count=  ', icou
      do
        call read_no_heat_source_Nu                                     &
     &     (id_pick, i_step, time, Nu_t, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          if(icou .eq. 0) then
            true_start = time
            prev_time = time
            prev_Nu(1) = Nu_t%Nu_ICB
            prev_Nu(2) = Nu_t%Nu_CMB
          else
            ave_Nu(1) = ave_Nu(1) + half*(Nu_t%Nu_ICB + prev_Nu(1))     &
     &                 * (time - prev_time)
            ave_Nu(2) = ave_Nu(2) + half*(Nu_t%Nu_CMB + prev_Nu(2))     &
     &                 * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
        prev_Nu(1) = Nu_t%Nu_ICB
        prev_Nu(2) = Nu_t%Nu_CMB
!
!        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59),&
!     &       'step= ', i_step,  ' averaging finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
      write(*,*)
      close(id_pick)
!
      acou = one / (time - true_start)
      ave_Nu(1:2) = ave_Nu(1:2) * acou
!
!       Evaluate standard deviation
!
      call open_read_no_heat_source_Nu(id_pick, Nu_t)
!
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', i_step,  ' deviation finished. Count=  ', icou
      icou = 0
      time = start_time
      prev_time = start_time
      sdev_Nu(1:2) = 0.0d0
      do
        call read_no_heat_source_Nu                                     &
     &     (id_pick, i_step, time, Nu_t, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          if(icou .eq. 0) then
            true_start = time
            prev_time = time
            prev_Nu(1) = (Nu_t%Nu_ICB - ave_Nu(1))**2
            prev_Nu(2) = (Nu_t%Nu_CMB - ave_Nu(2))**2
          else
            sdev_Nu(1) = sdev_Nu(1)                                     &
     &           + half*( (Nu_t%Nu_ICB - ave_Nu(1))**2 + prev_Nu(1))    &
     &                   * (time - prev_time)
            sdev_Nu(2) = sdev_Nu(2)                                     &
     &           + half*( (Nu_t%Nu_CMB - ave_Nu(2))**2 + prev_Nu(2))    &
     &                   * (time - prev_time)
          end if
!
          icou = icou + 1
        end if
        prev_time = time
        prev_Nu(1) = (Nu_t%Nu_ICB - ave_Nu(1))**2
        prev_Nu(2) = (Nu_t%Nu_CMB - ave_Nu(2))**2
!
!        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59),&
!     &       'step= ', i_step,  ' deviation finished. Count=   ', icou
        if(time .ge. end_time) exit
      end do
      write(*,*)
      close(id_pick)
!
      acou = one / (time - true_start)
      sdev_Nu(1:2) = sqrt(sdev_Nu(1:2) * acou)
!
!    output Results
!
      write(*,'(a,1p2e25.15e3)') 'Inner and outer radius: ',            &
     &                          Nu_t%r_ICB_Nu, Nu_t%r_CMB_Nu
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, end_time
      write(*,'(a)') 'Average and Std. Dev. of Nu at ICB:'
      write(*,'(1p2e25.15e3)')  ave_Nu(1), sdev_Nu(1)
      write(*,'(a)') 'Average and Std. Dev. of Nu at CMB:'
      write(*,'(1p2e25.15e3)')  ave_Nu(2), sdev_Nu(2)
!
      end subroutine time_average_nusselt_f
!
! -------------------------------------------------------------------
!
      end module time_average_nusselt
