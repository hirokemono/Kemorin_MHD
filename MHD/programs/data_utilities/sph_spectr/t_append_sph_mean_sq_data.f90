!>@file   t_append_sph_mean_sq_data.f90
!!        program t_append_sph_mean_sq_data
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2020
!!
!!
!> @brief Append mean square data file
!!
!!@verbatim
!!      subroutine append_sph_mean_sq_file(flag_spectr, flag_vol_ave,   &
!!     &          append_file_name, target_file_name)
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        character(len=kchara), intent(in) :: append_file_name
!!        character(len=kchara), intent(in) :: target_file_name
!!@endverbatim
      module t_append_sph_mean_sq_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
      use t_pick_copy_monitor_data
      use t_buffer_4_gzip
!
      implicit none
!
      private :: pick_copy_sph_pwr_data_to_end
      private :: sel_num_sph_mean_sq_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine append_sph_mean_sq_file(flag_spectr, flag_vol_ave,     &
     &          append_file_name, target_file_name)
!
      use select_gz_stream_file_IO
      use gz_spl_sph_spectr_head_IO
      use gz_spl_sph_spectr_data_IO
!
      implicit none
!
      logical, intent(in) :: flag_spectr, flag_vol_ave
      character(len=kchara), intent(in) :: append_file_name
      character(len=kchara), intent(in) :: target_file_name
!
!
      type(read_sph_spectr_data), save :: sph_IN1
      type(read_sph_spectr_data), save :: sph_OUT1
!
      integer(kind = kint), parameter :: id_append_file = 15
      integer(kind = kint), parameter :: id_write_file = 16
!
      integer(kind = kint) :: istep_start
      real(kind = kreal) :: start_time
!
      integer(kind = kint) :: ntot_pick
!
      logical :: flag_gzip1
      type(buffer_4_gzip), save :: zbuf1
      character, pointer, save  :: FPz_f1
      type(monitor_field_pickup_table), save :: comp_tbl1
!
!
      write(*,*) 'Open data file to append.'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call select_input_sph_series_head(FPz_f1, id_append_file,         &
     &    flag_gzip1, flag_current_fmt, flag_spectr, flag_vol_ave,      &
     &    sph_IN1, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*) istep_start, start_time
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
!
!
      write(*,*) 'Open target file', ': ', trim(target_file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_write_file, target_file_name, flag_gzip1, zbuf1)
      call select_input_sph_series_head(FPz_f1, id_write_file,          &
     &    flag_gzip1, flag_current_fmt, flag_spectr, flag_vol_ave,      &
     &    sph_OUT1, zbuf1)
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_write_file, flag_gzip1, zbuf1)
!
!
      if(sph_IN1%nri_sph .ne. sph_OUT1%nri_sph) then
        write(*,*) '# of radial layer does not match',                  &
     &      sph_IN1%nri_sph, sph_OUT1%nri_sph
        stop
      end if
      if(sph_IN1%ltr_sph .ne. sph_OUT1%ltr_sph) then
        write(*,*) '# of truncation does not match',                    &
     &      sph_IN1%ltr_sph, sph_OUT1%ltr_sph
        stop
      end if
!
      call init_pick_copy_sph_pwr_list                                  &
     &   (sph_IN1%ntot_sph_spec, sph_OUT1%ntot_sph_spec,                &
     &    sph_IN1%ene_sph_spec_name(sph_IN1%num_time_labels+1),         &
     &    sph_OUT1%ene_sph_spec_name(sph_OUT1%num_time_labels+1),       &
     &    comp_tbl1)
      call dealloc_sph_espec_data(sph_IN1)
!
      if(flag_vol_ave) then
        ntot_pick = (sph_OUT1%ltr_sph+1)
      else
        ntot_pick = sph_OUT1%nri_sph * (sph_OUT1%ltr_sph+1)
      end if
      write(*,*) 'ntot_pick', ntot_pick,                                &
     &         sph_OUT1%nri_sph, sph_OUT1%ltr_sph
!
      call open_bwd_serch_to_append(target_file_name, id_write_file,    &
     &    istep_start, start_time, ntot_pick)
!
      write(*,*) 'Open file to append again'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
!
      call select_input_sph_series_head(FPz_f1, id_append_file,         &
     &    flag_gzip1, flag_current_fmt, flag_spectr, flag_vol_ave,      &
     &    sph_IN1, zbuf1)
!
      if(comp_tbl1%fast_flag) then
        write(*,*) 'Copy data as text'
        call copy_sph_monitor_to_end                                    &
     &     (FPz_f1, id_append_file, flag_gzip1,                         &
     &      id_write_file, ntot_pick, zbuf1)
      else
        write(*,*) 'Read and select data'
        call pick_copy_sph_pwr_data_to_end                              &
     &     (FPz_f1, id_append_file, id_write_file, flag_gzip1,          &
     &      flag_spectr, flag_vol_ave, comp_tbl1, sph_IN1, sph_OUT1,    &
     &      zbuf1)
        call dealloc_monitor_fld_pickup_tbl(comp_tbl1)
      end if
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
!
      close(id_write_file)
!
      call dealloc_sph_espec_data(sph_IN1)
      call dealloc_sph_espec_data(sph_OUT1)
!
      end subroutine append_sph_mean_sq_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pick_copy_sph_pwr_data_to_end                          &
     &         (FPz_f, id_append_file, id_write_file,                   &
     &          flag_gzip, flag_spectr, flag_vol_ave,                   &
     &          comp_tbl, sph_IN, sph_OUT, zbuf)
!
      use gz_spl_sph_spectr_head_IO
      use gz_spl_sph_spectr_data_IO
      use write_sph_monitor_data
 !
      character, pointer, intent(in)  :: FPz_f
      integer(kind = kint), intent(in) :: id_append_file
      integer(kind = kint), intent(in) :: id_write_file
      logical, intent(in) :: flag_gzip, flag_spectr, flag_vol_ave
      type(monitor_field_pickup_table), intent(in) :: comp_tbl
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: ierr, n_mode
!
      do
        call sel_gz_input_sph_series_data(FPz_f, id_append_file,        &
     &      flag_gzip, flag_current_fmt, flag_spectr, flag_vol_ave,     &
     &      sph_IN, zbuf, ierr)
        if(ierr .gt. 0) exit
!
        sph_OUT%i_step = sph_IN%i_step
        sph_OUT%time = sph_IN%time
        n_mode = sel_num_sph_mean_sq_data(flag_spectr, flag_vol_ave,    &
     &                                    sph_IN)
        call pick_copy_monitor_data                                     &
     &     (comp_tbl, sph_IN%ntot_sph_spec, sph_OUT%ntot_sph_spec,      &
     &      n_mode, sph_IN%spectr_IO(1,0,1), sph_OUT%spectr_IO(1,0,1))
        call select_output_sph_series_data                              &
     &     (id_write_file, flag_spectr, flag_vol_ave, sph_OUT)
      end do
!
      end subroutine pick_copy_sph_pwr_data_to_end
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function sel_num_sph_mean_sq_data            &
     &                            (flag_spectr, flag_vol_ave, sph_IN)
!
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: n_mode
!
!
      if(flag_spectr) then
        if(flag_vol_ave) then
          n_mode = sph_IN%ltr_sph + 1
        else
          n_mode = (sph_IN%ltr_sph + 1) * sph_IN%nri_sph
        end if
      else
        if(flag_vol_ave) then
          n_mode = 1
        else
          n_mode = sph_IN%nri_sph
        end if
      end if
      sel_num_sph_mean_sq_data = n_mode
!
      end function sel_num_sph_mean_sq_data
!
! -----------------------------------------------------------------------
!
      end module t_append_sph_mean_sq_data
