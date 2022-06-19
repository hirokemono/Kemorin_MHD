!>@file   time_ave_picked_sph_spectr.f90
!!@brief  module time_ave_picked_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of spectrum data
!!
!!@verbatim
!!      integer(c_int) function                                         &
!!    &     load_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
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
      type(picked_spectrum_data_IO), save, private :: pick_IO
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
      subroutine get_picked_sph_time_f(n_step, i_step, time)            &
     &          bind(c, name="get_picked_sph_time_f")
!
      integer(C_int), Value :: n_step
      integer(C_int), intent(inout) :: i_step(n_step)
      real(c_double), intent(inout) :: time(n_step)
!
      integer(kind = kint) :: i
      character(len=kchara) :: draw_name
!
!$omp parallel do
      do i = 1, n_step
        i_step(i) = pick_IO%i_step(i)
        time(i) =   pick_IO%d_time(i)
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
      character(1,C_char), intent(in) :: yname(*)
      integer(C_int), Value :: radius_id, in_degree, in_order
!
      integer(C_int), Value :: n_step
      real(c_double), intent(inout) :: d_pick(n_step)
!
      integer(kind = kint) :: i, idx, id_comp, id_mode
      character(len=kchara) :: draw_name
!
      draw_name = c_to_fstring(yname)
      write(*,*) 'draw_name', draw_name
      id_comp = 0
      do i = 1, pick_IO%ntot_comp
        if(trim(draw_name) .eq. pick_IO%spectr_name(i)) then
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
      do i = 1, pick_IO%ntot_pick_spectr
        if(    radius_id .eq. pick_IO%idx_sph(i,1)                      &
     &   .and. in_degree .eq. pick_IO%idx_sph(i,3)                      &
     &   .and. in_order .eq.  pick_IO%idx_sph(i,4)) then
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

      idx = id_comp + (id_mode-1) * pick_IO%ntot_comp
!$omp parallel do
      do i = 1, n_step
        d_pick(i) =   pick_IO%d_pick(idx,i)
      end do
!$omp end parallel do
!
      end subroutine get_each_picked_sph_series_f
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     load_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
!
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
     &    true_start, true_end, pick_IO)
!
      load_picked_sph_spectr_f = pick_IO%n_step
      end function load_picked_sph_spectr_f
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
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
      use m_precision
      use m_constants
!
      use picked_sph_spectr_data_IO
      use set_parallel_file_name
!
      implicit  none
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
      integer(kind = kint) :: i_step, ierr, icou, i
      real(kind = kreal) :: time, prev_time
      real(kind = kreal) :: true_start, true_end
!
!
      write(tave_fname,'(a6,a)') 't_ave_', trim(file_name)
      write(trms_fname,'(a6,a)') 't_rms_', trim(file_name)
      write(sdev_fname,'(a8,a)') 't_sigma_', trim(file_name)
!
!      Load picked mode file
      call load_picked_sph_spectr_series                                &
     &   (flag_log, file_name, start_time, end_time,                    &
     &    true_start, true_end, pick_IO)
!
      allocate(ave_spec(pick_IO%ntot_data))
      allocate(rms_spec(pick_IO%ntot_data))
      allocate(sdev_spec(pick_IO%ntot_data))
!
!$omp parallel workshare
      ave_spec =   0.0d0
      rms_spec =   0.0d0
      sdev_spec =  0.0d0
!$omp end parallel workshare
!
      call cal_time_ave_picked_sph_spectr(pick_IO,                      &
     &    ave_spec, rms_spec, sdev_spec)
!
!$omp parallel workshare
      pick_IO%d_pk(1:pick_IO%ntot_data)                                 &
     &      = ave_spec(1:pick_IO%ntot_data)
!$omp end parallel workshare
      call write_tave_sph_spec_monitor                                  &
     &   (tave_fname, i_step, prev_time, true_start, pick_IO)
!
!    output RMS deviation
!
!$omp parallel workshare
      pick_IO%d_pk(1:pick_IO%ntot_data)                                 &
     &      = rms_spec(1:pick_IO%ntot_data)
!$omp end parallel workshare
!
      call write_tave_sph_spec_monitor                                  &
     &   (trms_fname, i_step, time, true_start, pick_IO)
!
!    output standard deviation
!
!$omp parallel workshare
      pick_IO%d_pk(1:pick_IO%ntot_data)                                 &
     &      = sdev_spec(1:pick_IO%ntot_data)
!$omp end parallel workshare
!
      call write_tave_sph_spec_monitor                                  &
     &   (sdev_fname, i_step, time, true_start, pick_IO)
!
      call dealloc_pick_sph_monitor_IO(pick_IO)
      call dealloc_pick_sph_series(pick_IO)
      deallocate(ave_spec, sdev_spec, rms_spec)
!
      end subroutine s_time_ave_picked_sph_spectr
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cal_time_ave_picked_sph_spectr(pick_IO,                &
     &          ave_spec, rms_spec, sdev_spec)
!
      type(picked_spectrum_data_IO), intent(in) :: pick_IO
      real(kind = kreal), intent(inout) :: sdev_spec(pick_IO%ntot_data)
      real(kind = kreal), intent(inout) :: ave_spec(pick_IO%ntot_data)
      real(kind = kreal), intent(inout) :: rms_spec(pick_IO%ntot_data)
!
      integer(kind = kint) :: icou
      real(kind = kreal) :: acou
!
!
      acou = one / (pick_IO%d_time(pick_IO%ntot_data)                   &
     &            - pick_IO%d_time(1))
!
      do icou = 1, pick_IO%n_step-1
!$omp parallel workshare
        ave_spec(1:pick_IO%ntot_data)                                   &
     &          = ave_spec(1:pick_IO%ntot_data)                         &
     &            + half * (pick_IO%d_pick(1:pick_IO%ntot_data,icou)    &
     &                    + pick_IO%d_pick(1:pick_IO%ntot_data,icou+1)) &
     &           * (pick_IO%d_time(icou+1) - pick_IO%d_time(icou))
!$omp end parallel workshare
      end do
!$omp parallel workshare
      ave_spec(1:pick_IO%ntot_data)                                     &
     &     =   ave_spec(1:pick_IO%ntot_data) * acou
!$omp end parallel workshare

      do icou = 1, pick_IO%n_step-1
!$omp parallel workshare
        rms_spec(1:pick_IO%ntot_data)                                   &
     &         = rms_spec(1:pick_IO%ntot_data)                          &
     &          + half * (pick_IO%d_pick(1:pick_IO%ntot_data,icou)**2   &
     &               + pick_IO%d_pick(1:pick_IO%ntot_data,icou+1)**2)   &
     &          * (pick_IO%d_time(icou+1) - pick_IO%d_time(icou))
        sdev_spec(1:pick_IO%ntot_data)                                  &
     &         = sdev_spec(1:pick_IO%ntot_data)                         &
     &          + half * ((pick_IO%d_pick(1:pick_IO%ntot_data,icou)     &
     &                   - ave_spec(1:pick_IO%ntot_data))**2            &
     &                  + (pick_IO%d_pick(1:pick_IO%ntot_data,icou+1)   &
     &                   - ave_spec(1:pick_IO%ntot_data))**2)           &
     &          * (pick_IO%d_time(icou+1) - pick_IO%d_time(icou))
!$omp end parallel workshare
      end do
!$omp parallel workshare
      rms_spec(1:pick_IO%ntot_data)                                     &
     &      = sqrt(rms_spec(1:pick_IO%ntot_data) * acou)
      sdev_spec(1:pick_IO%ntot_data)                                    &
     &      =  sqrt(sdev_spec(1:pick_IO%ntot_data) * acou)
!$omp end parallel workshare
!
      end subroutine cal_time_ave_picked_sph_spectr
!
! -------------------------------------------------------------------
!
      end module time_ave_picked_sph_spectr
