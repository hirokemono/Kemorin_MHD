!>@file   t_append_picked_spectr_file.f90
!!        program t_append_picked_spectr_file
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2020
!!
!!
!> @brief Append mean square data file
!!
!!@verbatim
!!      subroutine append_picked_spectr_file                            &
!!     &         (append_file_name, write_file_name)
!!        character(len=kchara), intent(in) :: append_file_name
!!        character(len=kchara), intent(in) :: write_file_name
!!@endverbatim
      module t_append_picked_spectr_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
      use t_pick_copy_monitor_data
      use t_buffer_4_gzip
      use t_pick_copy_monitor_data
!
      implicit none
!
      private :: pick_copy_pick_sph_data_to_end
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine append_picked_spectr_file                              &
     &         (append_file_name, write_file_name)
!
      use select_gz_stream_file_IO
      use t_picked_sph_spectr_data_IO
      use picked_sph_spectr_data_IO
      use gz_spl_sph_spectr_data_IO
!
      implicit none
!
      character(len=kchara), intent(in) :: append_file_name
      character(len=kchara), intent(in) :: write_file_name
!
!
      type(picked_spectrum_data_IO), save :: read_pick_IO
      type(picked_spectrum_data_IO), save :: write_pick_IO
!
      integer(kind = kint), parameter :: id_append_file = 15
      integer(kind = kint), parameter :: id_write_file = 16
!
      character, pointer :: FPz_f1
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
      type(monitor_field_pickup_table), save :: comp_tbl1
!
      integer(kind = kint) :: istep_start
      real(kind = kreal) :: start_time
!
      integer(kind = kint) :: nd, nline_snap
!
!
      write(*,*) 'Open append file', ': ', trim(append_file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call read_pick_series_head(FPz_f1, id_append_file, flag_gzip1,    &
     &                           read_pick_IO, zbuf1)
      call alloc_pick_sph_monitor_IO(read_pick_IO)
      call read_pick_series_comp_name                                   &
     &   (FPz_f1, id_append_file, flag_gzip1, read_pick_IO, zbuf1)
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      read(zbuf1%fixbuf(1),*) istep_start, start_time
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
!
      write(*,*) 'Open target file', ': ', trim(write_file_name)
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_write_file, write_file_name, flag_gzip1, zbuf1)
      call read_pick_series_head(FPz_f1, id_write_file, flag_gzip1,     &
     &                           write_pick_IO, zbuf1)
      call alloc_pick_sph_monitor_IO(write_pick_IO)
      call read_pick_series_comp_name                                   &
     &   (FPz_f1, id_write_file, flag_gzip1, write_pick_IO, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_write_file, flag_gzip1, zbuf1)
!
      if(read_pick_IO%num_layer .ne. write_pick_IO%num_layer) then
        write(*,*) '# of radial layer does not match',                  &
     &        read_pick_IO%num_layer, write_pick_IO%num_layer
        stop
      end if
      if(read_pick_IO%num_mode .ne. write_pick_IO%num_mode) then
        write(*,*) '# of spherical harmonics mode does not match',      &
     &      read_pick_IO%num_mode, write_pick_IO%num_mode
        stop
      end if
!
      call init_pick_copy_sph_pwr_list                                  &
     &   (read_pick_IO%ntot_comp, write_pick_IO%ntot_comp,              &
     &    read_pick_IO%spectr_name(1), write_pick_IO%spectr_name(1),    &
     &    comp_tbl1)
!
      nline_snap = read_pick_IO%num_mode * read_pick_IO%num_layer
      call open_bwd_serch_to_append(write_file_name, id_write_file,     &
     &    istep_start, start_time, nline_snap)
!
      write(*,*) 'Open file to append again'
      call sel_open_read_gz_stream_file                                 &
     &   (FPz_f1, id_append_file, append_file_name, flag_gzip1, zbuf1)
      call read_pick_series_head(FPz_f1, id_append_file, flag_gzip1,    &
     &                           read_pick_IO, zbuf1)
      call read_pick_series_comp_name                                   &
     &   (FPz_f1, id_append_file, flag_gzip1, read_pick_IO, zbuf1)
!
      if(comp_tbl1%fast_flag) then
        write(*,*) 'Copy data as text'
        call copy_sph_monitor_to_end                                    &
     &     (FPz_f1, id_append_file, flag_gzip1,                         &
     &      id_write_file, nline_snap, zbuf1)
      else
        write(*,*) 'Read and select data'
        call pick_copy_pick_sph_data_to_end                             &
     &     (FPz_f1, id_append_file, id_write_file, flag_gzip1,          &
     &      comp_tbl1, read_pick_IO, write_pick_IO, zbuf1)
      end if
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_append_file, flag_gzip1, zbuf1)
      close(id_write_file)
!
      call dealloc_pick_sph_monitor_IO(read_pick_IO)
      call dealloc_pick_sph_monitor_IO(write_pick_IO)
!
      end subroutine append_picked_spectr_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pick_copy_pick_sph_data_to_end                         &
     &         (FPz_f, id_append_file, id_write_file, flag_gzip,        &
     &          comp_tbl, picked_IN, picked_OUT, zbuf)
!
      use picked_sph_spectr_data_IO
      use write_snap_pick_sph_spectr
 !
      character, pointer, intent(in)  :: FPz_f
      integer(kind = kint), intent(in) :: id_append_file
      integer(kind = kint), intent(in) :: id_write_file
      logical, intent(in) :: flag_gzip
      type(monitor_field_pickup_table), intent(in) :: comp_tbl
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IN
      type(picked_spectrum_data_IO), intent(inout) :: picked_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: ierr, n_mode
      integer(kind = kint) :: i_step
      real(kind = kreal) :: time
!
!
      do
        call read_sph_spec_monitor(FPz_f, id_append_file, flag_gzip,    &
     &      i_step, time, picked_IN, zbuf, ierr)
        if(ierr .gt. 0) exit
!
        n_mode = picked_OUT%num_mode * picked_OUT%num_layer
        call pick_copy_monitor_data                                     &
     &     (comp_tbl, picked_IN%ntot_comp, picked_OUT%ntot_comp,        &
     &      n_mode, picked_IN%d_pk(1), picked_OUT%d_pk(1))
        call write_picked_sph_snap                                      &
     &     (id_write_file, i_step, time, picked_OUT)
      end do
!
      end subroutine pick_copy_pick_sph_data_to_end
!
! -----------------------------------------------------------------------
!
      end module t_append_picked_spectr_file
