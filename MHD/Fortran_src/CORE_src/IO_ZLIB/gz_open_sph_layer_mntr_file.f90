!>@file   gz_open_sph_layer_mntr_file.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!> @brief Time spectrum data output routines for utilities
!!
!!@verbatim
!!      subroutine check_sph_layer_mean_file(base_name,                 &
!!     &          sph_OUT, zbuf, flag_gzip_lc, error)
!!      subroutine check_sph_layer_spectr_file                          &
!!     &         (base_name, monitor_labels, sph_OUT,                   &
!!     &          FPz_f, zbuf, flag_gzip_lc, error)
!!      subroutine sel_open_sph_layer_mean_file(id_file, base_name,     &
!!     &          monitor_labels, sph_OUT, zbuf, flag_gzip_lc)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len = kchara), intent(in) :: base_name
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        logical, intent(inout) :: flag_gzip_lc
!!
!!      subroutine write_sph_pwr_layer_head(flag_gzip, id_file,         &
!!     &          monitor_labels, sph_OUT, zbuf)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        type(sph_spectr_head_labels), intent(in) :: monitor_labels
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!!
      module gz_open_sph_layer_mntr_file
!
      use m_precision
      use m_constants
      use t_buffer_4_gzip
      use t_read_sph_spectra
      use t_sph_spectr_head_labels
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_layer_mean_file(base_name,                   &
     &          sph_OUT, FPz_f, zbuf, flag_gzip_lc, error)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use compare_sph_monitor_header
!
      character(len = kchara), intent(in) :: base_name
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      character, pointer, intent(inout) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc, error
!
      integer(kind = kint), parameter :: id_stream = 44
      type(read_sph_spectr_data) :: sph_IN_f
      type(sph_spectr_head_labels) :: sph_lbl_IN_f
      character(len = kchara) :: file_name
      logical :: flag_miss
!
!
      error = .FALSE.
      call sel_open_check_gz_stream_file(FPz_f, id_stream, base_name,   &
     &    flag_gzip_lc, flag_miss, file_name, zbuf)
      if(flag_miss) go to 99
!
      call read_sph_layer_mean_head                                     &
     &   (FPz_f, id_stream, flag_gzip_lc, flag_current_fmt,             &
     &    sph_lbl_IN_f, sph_IN_f, zbuf)
      call sel_close_read_gz_stream_file(FPz_f, id_stream,              &
     &                                   flag_gzip_lc, zbuf)
!
      error = .not. cmp_sph_layer_monitor_heads(sph_lbl_IN_f, sph_IN_f, &
     &                                         sph_pwr_labels, sph_OUT)
      call dealloc_sph_espec_name(sph_IN_f)
      return
!
   99 continue
      write(*,*) 'No file ', trim(file_name), '. Make it.'
!
      open(id_stream, file=file_name,                                   &
     &     FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_layer_head(flag_gzip_lc, id_stream,            &
     &                              sph_pwr_labels, sph_OUT, zbuf)
      close(id_stream)
!
      end subroutine check_sph_layer_mean_file
!
!  --------------------------------------------------------------------
!
      subroutine check_sph_layer_spectr_file                            &
     &         (base_name, monitor_labels, sph_OUT,                     &
     &          FPz_f, zbuf, flag_gzip_lc, error)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use compare_sph_monitor_header
!
      character(len = kchara), intent(in) :: base_name
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      character, pointer, intent(inout) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc, error
!
      integer(kind = kint), parameter :: id_stream = 44
      type(read_sph_spectr_data) :: sph_IN_f
      type(sph_spectr_head_labels) :: sph_lbl_IN_f
      character(len = kchara) :: file_name
      logical :: flag_miss
!
!
      error = .FALSE.
      call sel_open_check_gz_stream_file(FPz_f, id_stream, base_name,   &
     &    flag_gzip_lc, flag_miss, file_name, zbuf)
      if(flag_miss) go to 99
!
      call read_sph_layer_spectr_head                                   &
     &   (FPz_f, id_stream, flag_gzip_lc, flag_current_fmt,             &
     &    sph_lbl_IN_f, sph_IN_f, zbuf)
      call sel_close_read_gz_stream_file(FPz_f, id_stream,              &
     &                                   flag_gzip_lc, zbuf)
!
      error = .not. cmp_sph_layer_monitor_heads(sph_lbl_IN_f, sph_IN_f, &
     &                                         monitor_labels, sph_OUT)
      call dealloc_sph_espec_name(sph_IN_f)
      return
!
   99 continue
      write(*,*) 'No file ', trim(file_name), '. Make it.'
!
      open(id_stream, file=file_name,                                   &
     &     FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_layer_head(flag_gzip_lc, id_stream,            &
     &                              monitor_labels, sph_OUT, zbuf)
      close(id_stream)
!
      end subroutine check_sph_layer_spectr_file
!
!  --------------------------------------------------------------------
!
      subroutine sel_open_sph_layer_mean_file(id_file, base_name,       &
     &          monitor_labels, sph_OUT, zbuf, flag_gzip_lc)
!
      use select_gz_stream_file_IO
      use set_parallel_file_name
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: base_name
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc
!
      character(len = kchara) :: fname, prefix, extension
      logical :: flag_miss
!
!
      fname = base_name
      call split_extrension(base_name, prefix, extension)
      if(cmp_no_case(prefix, 'NO_FILE')) return
!
      call check_gzip_or_ascii_file(base_name, fname,                   &
     &                              flag_gzip_lc, flag_miss)
!
      if(flag_miss) then
        write(*,*) 'Make file to write: ', trim(fname)
        open(id_file, file=fname, status='replace',                     &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
        call write_sph_pwr_layer_head(flag_gzip_lc, id_file,            &
     &                                monitor_labels, sph_OUT, zbuf)
      else
        open(id_file, file=fname, status='old', position='append',      &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      end if
!
      end subroutine sel_open_sph_layer_mean_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_pwr_layer_head(flag_gzip, id_file,           &
     &          monitor_labels, sph_OUT, zbuf)
!
      use select_gz_stream_file_IO
      use sph_power_spectr_data_text
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      call len_sph_layer_spectr_header(monitor_labels, sph_OUT,         &
     &                                 len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip, id_file,                 &
     &    sph_layer_spectr_header_text(len_tot, len_each,               &
     &                                 monitor_labels, sph_OUT), zbuf)
!
      end subroutine write_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      end module gz_open_sph_layer_mntr_file
