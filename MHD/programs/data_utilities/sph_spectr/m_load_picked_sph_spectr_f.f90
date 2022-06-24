!>@file   m_load_picked_sph_spectr_f.f90
!!@brief  module m_load_picked_sph_spectr_f
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of spectrum data
!!
!!@verbatim
!!      subroutine check_picked_sph_spectr_f(cname)                     &
!!     &          bind(c, name="check_picked_sph_spectr_f")
!!        character(1,C_char), intent(in) :: cname(*)
!!        character(len=kchara) :: file_name
!!      integer(c_int) function                                         &
!!    &     load_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!
!!      subroutine get_picked_sph_time_f(n_step, i_step, time)          &
!!     &          bind(c, name="get_picked_sph_time_f")
!!        integer(C_int), Value :: n_step
!!        integer(C_int), intent(inout) :: i_step(n_step)
!!        real(c_double), intent(inout) :: time(n_step)
!!      subroutine get_each_picked_sph_series_f                         &
!!     &         (yname, radius_id, in_degree, in_order, n_step, d_pick)&
!!     &          bind(c, name="get_each_picked_sph_series_f")
!!        character(1,C_char), intent(in) :: yname(*)
!!        integer(C_int), Value :: radius_id, in_degree, in_order
!!        integer(C_int), Value :: n_step
!!        real(c_double), intent(inout) :: d_pick(n_step)
!!      subroutine fin_picked_sph_time_series_f()                       &
!!     &          bind(c, name="fin_picked_sph_time_series_f")
!!        integer(C_int), Value :: n_step
!!@endverbatim
!
      module m_load_picked_sph_spectr_f
!
      use ISO_C_BINDING
!
      use m_precision
      use m_constants
!
      use t_read_sph_spectra
      use t_picked_sph_spectr_data_IO
!
      implicit  none
!
      type(picked_spectrum_data_IO), save, private :: pick_IO_p
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine check_picked_sph_spectr_f(cname)                       &
     &          bind(c, name="check_picked_sph_spectr_f")
!
      use count_monitor_time_series
      use picked_sph_spectr_data_IO
!
      character(1,C_char), intent(in) :: cname(*)
      character(len=kchara) :: file_name
!
      write(file_name,'(a)') trim(c_to_fstring(cname))
      call check_picked_sph_spectr(file_name, pick_IO_p)
!
      end subroutine check_picked_sph_spectr_f
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     load_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
!
      use count_monitor_time_series
      use picked_sph_spectr_data_IO
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
      call load_picked_sph_spectr_series                                &
     &   (.FALSE., file_name, start_time, end_time,                     &
     &    true_start, true_end, pick_IO_p)
!
      load_picked_sph_spectr_f = pick_IO_p%n_step
      end function load_picked_sph_spectr_f
!
! -------------------------------------------------------------------
!
      subroutine get_picked_sph_time_f(n_step, i_step, time)            &
     &          bind(c, name="get_picked_sph_time_f")
!
      integer(C_int), Value :: n_step
      integer(C_int), intent(inout) :: i_step(n_step)
      real(c_double), intent(inout) :: time(n_step)
!
      integer(kind = kint) :: i
!
!$omp parallel do
      do i = 1, n_step
        i_step(i) = pick_IO_p%i_step(i)
        time(i) =   pick_IO_p%d_time(i)
      end do
!$omp end parallel do
!
      end subroutine get_picked_sph_time_f
!
! -------------------------------------------------------------------
!
      subroutine get_each_picked_sph_series_f                           &
     &         (yname, radius_id, in_degree, in_order, n_step, d_pick)  &
     &          bind(c, name="get_each_picked_sph_series_f")
!
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: yname(*)
      integer(C_int), Value :: radius_id, in_degree, in_order
      integer(C_int), Value :: n_step
!
      real(c_double), intent(inout) :: d_pick(n_step)
!
      integer(kind = kint) :: i, idx, id_comp, id_mode
      character(len=kchara) :: draw_name
!
      draw_name = c_to_fstring(yname)
      write(*,*) 'draw_name', draw_name
      id_comp = 0
      do i = 1, pick_IO_p%ntot_comp
        if(trim(draw_name) .eq. pick_IO_p%spectr_name(i)) then
          id_comp = i
          exit
        end if
      end do
!
      if(id_comp .le. 0) then
        write(*,*) 'Input field cannot be found.', trim(draw_name)
        return
      end if
!
      id_mode = 0
      do i = 1, pick_IO_p%num_layer * pick_IO_p%num_sph_mode
        if(    radius_id .eq. pick_IO_p%idx_sph(i,1)                    &
     &   .and. in_degree .eq. pick_IO_p%idx_sph(i,3)                    &
     &   .and. in_order .eq.  pick_IO_p%idx_sph(i,4)) then
          id_mode = i
          exit
        end if
      end do
!
      if(id_mode .le. 0) then
        write(*,*) 'Input field cannot be found.'
        return
      end if
      write(*,*) 'id_mode, id_comp', id_mode, id_comp

      idx = id_comp + (id_mode-1) * pick_IO_p%ntot_comp
!$omp parallel do
      do i = 1, n_step
        d_pick(i) =   pick_IO_p%d_pick(idx,i)
      end do
!$omp end parallel do
!
      end subroutine get_each_picked_sph_series_f
!
! -------------------------------------------------------------------
!
      subroutine fin_picked_sph_time_series_f()                         &
     &          bind(c, name="fin_picked_sph_time_series_f")
!
      call dealloc_pick_sph_series(pick_IO_p)
      call dealloc_pick_sph_monitor_IO(pick_IO_p)
!
      end subroutine fin_picked_sph_time_series_f
!
! -------------------------------------------------------------------
!
      end module m_load_picked_sph_spectr_f
