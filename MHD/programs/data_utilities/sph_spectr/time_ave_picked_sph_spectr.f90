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
      use Iso_C_binding
!
      use m_precision
      use m_constants
!
      use t_read_sph_spectra
      use t_picked_sph_spectr_data_IO
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
      use set_parallel_file_name
!
      implicit  none
!
      logical, intent(in) :: flag_log
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(picked_spectrum_data_IO), save :: pick_IO
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
!      Open picked mode file
!
      write(*,*) 'Open file: ', trim(file_name)
      open(id_pick, file = file_name)
      call read_pick_series_head(id_pick, pick_IO)
!
!
      icou = 0
      true_start = start_time
      prev_time = true_start
      true_end = true_start
      do
        call read_sph_spec_time                                      &
     &     (id_pick, pick_IO, i_step, time, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
!          call append_picked_sph_series(i_step, time, pick_IO)
          if(icou .eq. 0) true_start = time
!
          icou = icou + 1
          if(flag_log) then
            write(*,'(69a1,a5,i12,a4,1pe16.8e3,a20,i12)',advance="NO")  &
     &        (char(8),i=1,69), 'step ', i_step,                        &
     &        ' at ', time, ' is read. count is  ', icou
          end if
        end if
!
        if(time .ge. end_time) exit
      end do
      true_end = time
      rewind(id_pick)
      write(*,*)
      call dealloc_pick_sph_monitor_IO(pick_IO)
!
      call read_pick_series_head(id_pick, pick_IO)
      call alloc_pick_sph_series(icou, pick_IO)
!
!       Evaluate time average
      icou = 0
      do
        call read_sph_spec_monitor                                      &
     &     (id_pick, i_step, time, pick_IO, ierr)
        if(ierr .gt. 0) exit
!
        if(time .ge. start_time) then
          icou = icou + 1
          call copy_to_pick_sph_series(icou, i_step, time, pick_IO)
!
          if(flag_log) then
            write(*,'(69a1,a5,i12,a4,1pe16.8e3,a20,i12)',advance="NO")  &
     &          (char(8),i=1,69), 'step ', i_step,                      &
     &          ' at ', time, ' is read. count is  ', icou
          end if
        end if
!
        if(time .ge. end_time) exit
      end do
      close(id_pick)
      write(*,*)
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
!
!
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
