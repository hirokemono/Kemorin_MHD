!>@file   time_average_gauss_coefs.f90
!!@brief  module time_average_gauss_coefs
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of Nusselt number file
!!
!!@verbatim
!!      integer(c_int) function                                         &
!!    &     time_average_gauss_coefs_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine s_time_average_gauss_coefs                           &
!!     &         (input_file_name, start_time, end_time)
!!        character(len=kchara), intent(in) :: input_file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!@endverbatim
!
      module time_average_gauss_coefs
!
      use ISO_C_BINDING
      use m_precision
      use m_constants
      use t_gauss_coefs_monitor_IO
!
      implicit  none
!
!>      Structure for gauss coeffciients
      type(picked_gauss_coefs_IO), save, private :: gauss_IO_a
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_average_gauss_coefs_f(cname, cstart, cend) Bind(C)
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
      call s_time_average_gauss_coefs                                   &
     &   (.FALSE., file_name, start_time, end_time)
!
      time_average_gauss_coefs_f = 0
      end function time_average_gauss_coefs_f
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine s_time_average_gauss_coefs                             &
     &         (flag_log, input_file_name, start_time, end_time)
!
      use t_sph_volume_mean_series
      use count_monitor_time_series
      use gz_gauss_coefs_monitor_IO
      use write_gauss_coefs_4_monitor
!
      logical, intent(in) :: flag_log
      character(len=kchara), intent(in) :: input_file_name
      real(kind = kreal), intent(in) :: start_time, end_time
!
      real(kind = kreal), allocatable :: ave_gauss(:)
      real(kind = kreal), allocatable :: rms_gauss(:)
      real(kind = kreal), allocatable :: sdev_gauss(:)
      real(kind = kreal), allocatable :: prev_gauss(:)
      integer(kind = kint), allocatable :: imask(:)
!
      character(len=kchara) :: tave_pick_gauss_fname
      character(len=kchara) :: trms_pick_gauss_fname
      character(len=kchara) :: sdev_pick_gauss_fname
!
      integer(kind = kint) :: icou
      real(kind = kreal) :: true_start, true_end
!
!
      write(tave_pick_gauss_fname,'(a6,a)')                             &
     &                           't_ave_',  trim(input_file_name)
      write(trms_pick_gauss_fname,'(a6,a)')                             &
     &                           't_rms_',  trim(input_file_name)
      write(sdev_pick_gauss_fname,'(a8,a)')                             &
     &                           't_sigma_', trim(input_file_name)
!
!       Load gauss coefficients data
      call load_gauss_coefs_time_series                                 &
     &   (flag_log, input_file_name, start_time, end_time,              &
     &    true_start, true_end, gauss_IO_a)
!
      allocate(ave_gauss(gauss_IO_a%num_mode))
      allocate(rms_gauss(gauss_IO_a%num_mode))
      allocate(sdev_gauss(gauss_IO_a%num_mode))
      allocate(prev_gauss(gauss_IO_a%num_mode))
      allocate(imask(gauss_IO_a%n_step))
      imask(1:gauss_IO_a%n_step) = 1
!
      call cal_time_ave_picked_sph_spectr                               &
     &   (gauss_IO_a%n_step, gauss_IO_a%d_time, imask,                  &
     &    gauss_IO_a%num_mode, gauss_IO_a%d_gauss, ave_gauss,           &
     &    rms_gauss, sdev_gauss)
!
!
      do icou = 1, gauss_IO_a%num_mode
        write(*,*) icou, ave_gauss(icou), rms_gauss(icou),              &
     &         sdev_gauss(icou), trim(gauss_IO_a%gauss_coef_name(icou))
      end do
!
!  Output time average
!$omp parallel workshare
      gauss_IO_a%gauss_coef(1:gauss_IO_a%num_mode)                      &
     &      = ave_gauss(1:gauss_IO_a%num_mode)
!$omp end parallel workshare
      call s_write_gauss_coefs_4_monitor(0, tave_pick_gauss_fname,      &
     &    gauss_IO_a%i_step(gauss_IO_a%n_step), true_end, gauss_IO_a)
!
!  Output time average
!$omp parallel workshare
      gauss_IO_a%gauss_coef(1:gauss_IO_a%num_mode)                      &
     &      = rms_gauss(1:gauss_IO_a%num_mode)
!$omp end parallel workshare
      call s_write_gauss_coefs_4_monitor(0, trms_pick_gauss_fname,      &
     &    gauss_IO_a%i_step(gauss_IO_a%n_step), true_end, gauss_IO_a)
!
!  Output time average
!$omp parallel workshare
      gauss_IO_a%gauss_coef(1:gauss_IO_a%num_mode)                      &
     &      = sdev_gauss(1:gauss_IO_a%num_mode)
!$omp end parallel workshare
      call s_write_gauss_coefs_4_monitor(0, sdev_pick_gauss_fname,      &
     &    gauss_IO_a%i_step(gauss_IO_a%n_step), true_end, gauss_IO_a)
!
      deallocate(ave_gauss, rms_gauss, sdev_gauss)
      call dealloc_gauss_coef_monitor(gauss_IO_a)
      call dealloc_gauss_coefs_series(gauss_IO_a)
!
      end subroutine s_time_average_gauss_coefs
!
! -------------------------------------------------------------------
!
      end module time_average_gauss_coefs
