!>@file   m_load_gauss_coefficients_f.f90
!!@brief  module m_load_gauss_coefficients_f
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of spectrum data
!!
!!@verbatim
!!      subroutine check_gauss_coef_series_f(cname)                     &
!!     &          bind(c, name="check_gauss_coef_series_f")
!!        character(1,C_char), intent(in) :: cname(*)
!!      integer(c_int) function                                         &
!!    &     load_gauss_coefs_series_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine get_gauss_coefs_time_f(n_step, i_step, time)         &
!!     &          bind(c, name="get_gauss_coefs_time_f")
!!        integer(C_int), Value :: n_step
!!        integer(C_int), intent(inout) :: i_step(n_step)
!!        real(c_double), intent(inout) :: time(n_step)
!!      subroutine get_each_gauss_coef_series_f(yname, n_step, d_pick)  &
!!     &          bind(c, name="get_each_gauss_coef_series_f")
!!        character(1,C_char), intent(in) :: yname(*)
!!        integer(C_int), Value :: n_step
!!        real(c_double), intent(inout) :: d_pick(n_step)
!!      subroutine fin_gauss_coefs_series_f()                           &
!!     &          bind(c, name="fin_gauss_coefs_series_f")
!!        integer(C_int), Value :: n_step
!!@endverbatim
!
      module m_load_gauss_coefficients_f
!
      use ISO_C_BINDING
!
      use m_precision
      use m_constants
!
      use t_read_sph_spectra
      use t_gauss_coefs_monitor_IO
!
      implicit  none
!
      type(picked_gauss_coefs_IO), save, private :: gauss_IO_p
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine check_gauss_coef_series_f(cname)                       &
     &          bind(c, name="check_gauss_coef_series_f")
!
      use count_monitor_time_series
      use gauss_coefs_monitor_IO
!
      character(1,C_char), intent(in) :: cname(*)
      character(len=kchara) :: file_name
!
      write(file_name,'(a)') trim(c_to_fstring(cname))
      call check_gauss_coefs_time_series(file_name, gauss_IO_p)
!
      end subroutine check_gauss_coef_series_f
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     load_gauss_coefs_series_f(cname, cstart, cend) Bind(C)
!
      use count_monitor_time_series
      use gauss_coefs_monitor_IO
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      real(kind = kreal) :: true_start, true_end
      character(len=kchara) :: file_name
!
      write(file_name,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call load_gauss_coefs_time_series                                 &
     &   (.FALSE., file_name, start_time, end_time,                     &
     &    true_start, true_end, gauss_IO_p)
!
      load_gauss_coefs_series_f = gauss_IO_p%n_step
      end function load_gauss_coefs_series_f
!
! -------------------------------------------------------------------
!
      subroutine get_gauss_coefs_time_f(n_step, i_step, time)           &
     &          bind(c, name="get_gauss_coefs_time_f")
!
      use gauss_coefs_monitor_IO
!
      integer(C_int), Value :: n_step
      integer(C_int), intent(inout) :: i_step(n_step)
      real(c_double), intent(inout) :: time(n_step)
!
      integer(kind = kint) :: i
!
!$omp parallel do
      do i = 1, n_step
        i_step(i) = gauss_IO_p%i_step(i)
        time(i) =   gauss_IO_p%d_time(i)
      end do
!$omp end parallel do
!
      end subroutine get_gauss_coefs_time_f
!
! -------------------------------------------------------------------
!
      subroutine get_each_gauss_coef_series_f(yname, n_step, d_pick)    &
     &          bind(c, name="get_each_gauss_coef_series_f")
!
      use count_monitor_time_series
      use gauss_coefs_monitor_IO
!
      character(1,C_char), intent(in) :: yname(*)
      integer(C_int), Value :: n_step
!
      real(c_double), intent(inout) :: d_pick(n_step)
!
      integer(kind = kint) :: i, id_mode
      character(len=kchara) :: draw_name, coef_name
!
      coef_name = c_to_fstring(yname)
      write(*,*) 'coef_name: ', coef_name
      id_mode = 0
      do i = 1, gauss_IO_p%num_mode
        if(trim(coef_name) .eq. gauss_IO_p%gauss_coef_name(i)) then
          id_mode = i
          exit
        end if
      end do
!
      if(id_mode .le. 0) then
        write(*,*) 'Input field cannot be found.', trim(coef_name)
        return
      end if
!
!$omp parallel do
      do i = 1, n_step
        d_pick(i) =   gauss_IO_p%d_gauss(id_mode,i)
      end do
!$omp end parallel do
!
      end subroutine get_each_gauss_coef_series_f
!
! -------------------------------------------------------------------
!
      subroutine fin_gauss_coefs_series_f()                             &
     &          bind(c, name="fin_gauss_coefs_series_f")
!
      call dealloc_gauss_coef_monitor(gauss_IO_p)
      call dealloc_gauss_coefs_series(gauss_IO_p)
!
      end subroutine fin_gauss_coefs_series_f
!
! -------------------------------------------------------------------
!
      end module m_load_gauss_coefficients_f
