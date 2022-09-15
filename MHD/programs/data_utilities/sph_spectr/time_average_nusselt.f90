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
!!    &     time_average_nusselt_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine s_time_average_nusselt                               &
!!     &         (file_name, start_time, end_time)
!!        character(len=kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!@endverbatim
!
      module time_average_nusselt
!
      use ISO_C_BINDING
      use m_precision
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_average_nusselt_f(cname, cstart, cend) Bind(C)
!
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: file_name
!
      write(file_name,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call s_time_average_nusselt(.FALSE., file_name,                   &
     &                            start_time, end_time)
!
      time_average_nusselt_f = 0
      end function time_average_nusselt_f
!
! -------------------------------------------------------------------
!
      subroutine s_time_average_nusselt                                 &
     &         (flag_log, file_name, start_time, end_time)
!
      use t_no_heat_Nusselt
      use gz_Nusselt_monitor_IO
      use count_monitor_time_series
!
      logical, intent(in) :: flag_log
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time

      type(nusselt_number_data), save :: Nu_t
      type(nusselt_number_series), save :: Nu_series
!
      real(kind = kreal) :: rms_Nu(2) = 0.0d0
      real(kind = kreal) :: ave_Nu(2) = 0.0d0
      real(kind = kreal) :: sdev_Nu(2) = 0.0d0
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: i_step, ierr, icou, i
      real(kind = kreal) :: acou, time, prev_time
      real(kind = kreal) :: true_start, true_end
!
!       Load data
      write(*,*) 'file_name: ', trim(file_name)
      call load_Nusselt_time_series                                     &
     &   (flag_log, file_name, start_time, end_time,                    &
     &    true_start, true_end, Nu_t, Nu_series)
        write(*,*) 'aho'
!
      call cal_time_ave_picked_sph_spectr                               &
     &   (Nu_series%n_step, Nu_series%d_time, itwo,                     &
     &    Nu_series%Nu_numbers, ave_Nu, rms_Nu, sdev_Nu)
!
!    output Results
!
      write(*,'(a,1p2e25.15e3)') 'Inner and outer radius: ',            &
     &                          Nu_t%r_ICB_Nu, Nu_t%r_CMB_Nu
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, true_end
      write(*,'(a)') 'Average and Std. Dev. of Nu at ICB:'
      write(*,'(1p2e25.15e3)')  ave_Nu(1), sdev_Nu(1)
      write(*,'(a)') 'Average and Std. Dev. of Nu at CMB:'
      write(*,'(1p2e25.15e3)')  ave_Nu(2), sdev_Nu(2)
!
      end subroutine s_time_average_nusselt
!
! -------------------------------------------------------------------
!
      end module time_average_nusselt
