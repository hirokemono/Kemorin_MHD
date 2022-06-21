!>@file   time_ave_picked_sph_spectr.f90
!!@brief  module time_ave_picked_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of spectrum data
!!
!!@verbatim
!!        integer(c_int) function                                       &
!!      &     time_ave_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!      subroutine s_time_ave_picked_sph_spectr                         &
!!     &         (flag_log, file_name, start_time, end_time)
!!        logical, intent(in) :: flag_log
!!        character(len=kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!@endverbatim
!
      module time_ave_picked_sph_spectr
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
      type(picked_spectrum_data_IO), save, private :: pick_IO_a
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
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
      call s_time_ave_picked_sph_spectr                                 &
     &   (.FALSE., file_name, start_time, end_time)
!
      time_ave_picked_sph_spectr_f = 0
      end function time_ave_picked_sph_spectr_f
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine s_time_ave_picked_sph_spectr                           &
     &         (flag_log, file_name, start_time, end_time)
!
      use picked_sph_spectr_data_IO
      use count_monitor_time_series
!
      logical, intent(in) :: flag_log
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time
!
      real(kind = kreal), allocatable :: sdev_spec(:)
      real(kind = kreal), allocatable :: ave_spec(:)
      real(kind = kreal), allocatable :: rms_spec(:)
!
      character(len=kchara) :: tave_fname
      character(len=kchara) :: trms_fname
      character(len=kchara) :: sdev_fname
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: i_step
      real(kind = kreal) :: true_start, true_end
!
!
      write(tave_fname,'(a6,a)') 't_ave_', trim(file_name)
      write(trms_fname,'(a6,a)') 't_rms_', trim(file_name)
      write(sdev_fname,'(a8,a)') 't_sigma_', trim(file_name)
!
!      Load picked mode file
      if(flag_log) call check_picked_sph_spectr(file_name, pick_IO_a)
      call load_picked_sph_spectr_series                                &
     &   (flag_log, file_name, start_time, end_time,                    &
     &    true_start, true_end, pick_IO_a)
!
      allocate(ave_spec(pick_IO_a%ntot_data))
      allocate(rms_spec(pick_IO_a%ntot_data))
      allocate(sdev_spec(pick_IO_a%ntot_data))
!
      call cal_time_ave_picked_sph_spectr                               &
     &   (pick_IO_a%n_step, pick_IO_a%d_time, pick_IO_a%ntot_data,      &
     &    pick_IO_a%d_pick, ave_spec, rms_spec, sdev_spec)
!
!$omp parallel workshare
      pick_IO_a%d_pk(1:pick_IO_a%ntot_data)                             &
     &      = ave_spec(1:pick_IO_a%ntot_data)
!$omp end parallel workshare
      call write_tave_sph_spec_monitor                                  &
     &   (tave_fname, pick_IO_a%i_step(pick_IO_a%n_step),               &
     &    true_end, true_start, pick_IO_a)
!
!    output RMS deviation
!
!$omp parallel workshare
      pick_IO_a%d_pk(1:pick_IO_a%ntot_data)                             &
     &      = rms_spec(1:pick_IO_a%ntot_data)
!$omp end parallel workshare
!
      call write_tave_sph_spec_monitor                                  &
     &   (trms_fname, pick_IO_a%i_step(pick_IO_a%n_step),               &
     &    true_end, true_start, pick_IO_a)
!
!    output standard deviation
!
!$omp parallel workshare
      pick_IO_a%d_pk(1:pick_IO_a%ntot_data)                             &
     &      = sdev_spec(1:pick_IO_a%ntot_data)
!$omp end parallel workshare
!
      call write_tave_sph_spec_monitor                                  &
     &   (sdev_fname, pick_IO_a%i_step(pick_IO_a%n_step),               &
     &    true_end, true_start, pick_IO_a)
!
      call dealloc_pick_sph_monitor_IO(pick_IO_a)
      call dealloc_pick_sph_series(pick_IO_a)
      deallocate(ave_spec, sdev_spec, rms_spec)
!
      end subroutine s_time_ave_picked_sph_spectr
!
! -------------------------------------------------------------------
!
      end module time_ave_picked_sph_spectr
