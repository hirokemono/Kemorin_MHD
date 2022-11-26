!>@file   gz_open_sph_monitor_file.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!> @brief Time spectrum data output routines for utilities
!!
!!@verbatim
!!      subroutine check_sph_vol_monitor_file(id_file, base_name,       &
!!     &          sph_OUT, zbuf, flag_gzip_lc, error)
!!      subroutine sel_open_sph_vol_monitor_file                        &
!!     &         (id_file, fname, sph_OUT, zbuf, flag_gzip_lc)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len = kchara), intent(in) :: fname
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        logical, intent(inout) :: flag_gzip_lc
!!
!!      subroutine check_sph_layer_mean_file(id_file, base_name,        &
!!     &          sph_OUT, zbuf, flag_gzip_lc, error)
!!      subroutine sel_open_sph_layer_mean_file                         &
!!     &         (id_file, fname, sph_OUT, zbuf, flag_gzip_lc)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len = kchara), intent(in) :: fname
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        logical, intent(inout) :: flag_gzip_lc
!!@endverbatim
!!
      module gz_open_sph_monitor_file
!
      use m_precision
      use m_constants
      use t_buffer_4_gzip
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_vol_monitor_file(id_stream, base_name,       &
     &          sph_OUT, FPz_f, zbuf, flag_gzip_lc, error)
!
      use t_read_sph_spectra
      use select_gz_stream_file_IO
      use sph_power_spectr_data_text
      use sel_gz_input_sph_mtr_head
      use compare_sph_monitor_header
      use set_parallel_file_name
      use delete_data_files
!
      integer(kind = kint), intent(in) :: id_stream
      character(len = kchara), intent(in) :: base_name
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      character, pointer, intent(inout) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc, error
!
      type(read_sph_spectr_data) :: sph_IN_f
      type(sph_spectr_head_labels) :: sph_lbl_IN_f
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
      character(len = kchara) :: fname, gzip_name
!
!
      error = .FALSE.
      fname = base_name
      gzip_name = add_gzip_extension(base_name)
      if(check_file_exist(fname)) then
        flag_gzip_lc = .FALSE.
      else if(check_file_exist(gzip_name)) then
        flag_gzip_lc = .TRUE.
        fname = gzip_name
      else
        go to 99
      end if
!
      call sel_open_read_gz_stream_file(FPz_f, id_stream,               &
     &                                  fname, flag_gzip_lc, zbuf)
      call s_select_input_sph_series_head                               &
     &   (FPz_f, id_stream, flag_gzip_lc, flag_current_fmt,             &
     &    spectr_off, volume_on, sph_lbl_IN_f, sph_IN_f, zbuf)
      call sel_close_read_gz_stream_file(FPz_f, id_stream,              &
     &                                   flag_gzip_lc, zbuf)
      error = .not. cmp_sph_volume_monitor_heads(sph_lbl_IN_f, sph_IN_f, &
     &                                         sph_pwr_labels, sph_OUT)
      call dealloc_sph_espec_data(sph_IN_f)
      call dealloc_sph_espec_name(sph_IN_f)
      return
!
   99 continue
!
      if(flag_gzip_lc) fname = add_gzip_extension(fname)
      open(id_stream, file=fname, FORM='UNFORMATTED', ACCESS='STREAM')
!
      call len_sph_vol_spectr_header(sph_pwr_labels, sph_OUT,           &
     &                               len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip_lc, id_stream,            &
     &    sph_vol_spectr_header_text(len_tot, len_each,                 &
     &                               sph_pwr_labels, sph_OUT), zbuf)
      close(id_stream)
!
      end subroutine check_sph_vol_monitor_file
!
!  --------------------------------------------------------------------
!
      subroutine sel_open_sph_vol_monitor_file                          &
     &         (id_file, base_name, sph_OUT, zbuf, flag_gzip_lc)
!
      use t_read_sph_spectra
      use select_gz_stream_file_IO
      use sph_power_spectr_data_text
      use set_parallel_file_name
      use delete_data_files
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: base_name
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
      character(len = kchara) :: fname, gzip_name
!
!
      fname = base_name
      gzip_name = add_gzip_extension(base_name)
      if(check_file_exist(fname)) then
        flag_gzip_lc = .FALSE.
      else if(check_file_exist(gzip_name)) then
        flag_gzip_lc = .TRUE.
        fname = gzip_name
      else
        go to 99
      end if
!
      open(id_file, file=fname, status='old', position='append',        &
     &     FORM='UNFORMATTED', ACCESS='STREAM')
      return
!
   99 continue
!
      if(flag_gzip_lc) fname = add_gzip_extension(fname)
      open(id_file, file=fname, FORM='UNFORMATTED', ACCESS='STREAM')
!
      call len_sph_vol_spectr_header(sph_pwr_labels, sph_OUT,           &
     &                               len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip_lc, id_file,              &
     &    sph_vol_spectr_header_text(len_tot, len_each,                 &
     &                               sph_pwr_labels, sph_OUT), zbuf)
!
      end subroutine sel_open_sph_vol_monitor_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_sph_layer_mean_file(id_stream, base_name,        &
     &          sph_OUT, FPz_f, zbuf, flag_gzip_lc, error)
!
      use t_read_sph_spectra
      use select_gz_stream_file_IO
      use sph_power_spectr_data_text
      use sel_gz_input_sph_mtr_head
      use compare_sph_monitor_header
      use set_parallel_file_name
      use delete_data_files
!
      integer(kind = kint), intent(in) :: id_stream
      character(len = kchara), intent(in) :: base_name
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      character, pointer, intent(inout) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc, error
!
      type(read_sph_spectr_data) :: sph_IN_f
      type(sph_spectr_head_labels) :: sph_lbl_IN_f
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
      character(len = kchara) :: fname, gzip_name
!
!
      error = .FALSE.
      fname = base_name
      gzip_name = add_gzip_extension(base_name)
      if(check_file_exist(fname)) then
        flag_gzip_lc = .FALSE.
      else if(check_file_exist(gzip_name)) then
        flag_gzip_lc = .TRUE.
        fname = gzip_name
      else
        go to 99
      end if
!
      call sel_open_read_gz_stream_file(FPz_f, id_stream,               &
     &                                  fname, flag_gzip_lc, zbuf)
      call s_select_input_sph_series_head                               &
     &   (FPz_f, id_stream, flag_gzip_lc, flag_current_fmt,             &
     &    spectr_off, volume_on, sph_lbl_IN_f, sph_IN_f, zbuf)
      error = .not. cmp_sph_layer_monitor_heads(sph_lbl_IN_f, sph_IN_f, &
     &                                         sph_pwr_labels, sph_OUT)
      call dealloc_sph_espec_data(sph_IN_f)
      call dealloc_sph_espec_name(sph_IN_f)
      return
!
   99 continue
!
      if(flag_gzip_lc) fname = add_gzip_extension(fname)
      open(id_stream, file=fname, FORM='UNFORMATTED', ACCESS='STREAM')
!
      call len_sph_layer_spectr_header(sph_pwr_labels, sph_OUT,         &
     &                                 len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip_lc, id_stream,            &
     &    sph_layer_spectr_header_text(len_tot, len_each,               &
     &                                 sph_pwr_labels, sph_OUT), zbuf)
      close(id_stream)
!
      end subroutine check_sph_layer_mean_file
!
!  --------------------------------------------------------------------
!
      subroutine sel_open_sph_layer_mean_file                           &
     &         (id_file, base_name, sph_OUT, zbuf, flag_gzip_lc)
!
      use t_read_sph_spectra
      use select_gz_stream_file_IO
      use sph_power_spectr_data_text
      use set_parallel_file_name
      use delete_data_files
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: base_name
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
      character(len = kchara) :: fname, gzip_name
!
!
      fname = base_name
      gzip_name = add_gzip_extension(base_name)
      if(check_file_exist(fname)) then
        flag_gzip_lc = .FALSE.
      else if(check_file_exist(gzip_name)) then
        flag_gzip_lc = .TRUE.
        fname = gzip_name
      else
        go to 99
      end if
!
      open(id_file, file=fname, status='old', position='append',        &
     &     FORM='UNFORMATTED', ACCESS='STREAM')
      return
!
   99 continue
!
      if(flag_gzip_lc) fname = add_gzip_extension(fname)
      open(id_file, file=fname, FORM='UNFORMATTED', ACCESS='STREAM')
!
      call len_sph_layer_spectr_header(sph_pwr_labels, sph_OUT,         &
     &                                 len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip_lc, id_file,              &
     &    sph_layer_spectr_header_text(len_tot, len_each,               &
     &                                 sph_pwr_labels, sph_OUT), zbuf)
!
      end subroutine sel_open_sph_layer_mean_file
!
!  --------------------------------------------------------------------
!
      end module gz_open_sph_monitor_file
