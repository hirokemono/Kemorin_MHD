!>@file   gz_layer_mean_monitor_IO.F90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!> @brief  Layerd mean square data output
!!
!!@verbatim
!!      subroutine swap_layer_mean_to_IO(nri_sph, ntot_comp,            &
!!     &                                 rms_sph, spectr_IO)
!!        integer(kind = kint), intent(in) :: nri_sph, ntot_comp
!!        real(kind = kreal), intent(in) :: rms_sph(nri_sph, ntot_comp)
!!        real(kind = kreal), intent(inout)                             &
!!     &                     :: spectr_IO(ntot_comp,nri_sph)
!!
!!      subroutine sel_gz_read_layer_mean_mtr                           &
!!     &         (FPz_f, id_stream, flag_gzip, nri_sph, ntot_comp,      &
!!     &          i_step, time, kr_sph, r_sph, spectr_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: nri_sph, ntot_comp
!!        integer(kind = kint), intent(inout) :: i_step
!!        real(kind = kreal), intent(inout) :: time
!!        integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
!!        real(kind = kreal), intent(inout) :: r_sph(nri_sph)
!!        real(kind = kreal), intent(inout)                             &
!!     &                     :: spectr_IO(ntot_comp,nri_sph)
!!        integer(kind = kint), intent(inout) :: ierr
!!        type(buffer_4_gzip), intent(inout) :: zbuf
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
!!      subroutine sel_gz_write_layer_mean_mtr                          &
!!     &         (flag_gzip, id_file, i_step, time,                     &
!!     &          nri_sph, kr_sph, r_sph, ntot_comp, spectr_IO, zbuf)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: i_step
!!        integer(kind = kint), intent(in) :: nri_sph
!!        integer(kind = kint), intent(in) :: kr_sph(nri_sph)
!!        real(kind = kreal), intent(in) :: time
!!        real(kind = kreal), intent(in) :: r_sph(nri_sph)
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(in) :: spectr_IO(ntot_comp,nri_sph)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
      module gz_layer_mean_monitor_IO
!
      use m_precision
      use m_constants
      use t_buffer_4_gzip
!
      implicit none
!
      private :: gz_write_layer_mean_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine swap_layer_mean_to_IO(nri_sph, ntot_comp,              &
     &                                 rms_sph, spectr_IO)
!
      integer(kind = kint), intent(in) :: nri_sph, ntot_comp
      real(kind = kreal), intent(in) :: rms_sph(nri_sph, ntot_comp)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ntot_comp,nri_sph)
!
      integer(kind = kint) :: kr
!
!
!$omp parallel do private(kr)
      do kr = 1, nri_sph
        spectr_IO(1:ntot_comp,kr) = rms_sph(kr,1:ntot_comp)
      end do
!$omp end parallel do
!
      end subroutine swap_layer_mean_to_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_gz_read_layer_mean_mtr                             &
     &         (FPz_f, id_stream, flag_gzip, nri_sph, ntot_comp,        &
     &          i_step, time, kr_sph, r_sph, spectr_IO, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: nri_sph, ntot_comp
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) :: time
      integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
      real(kind = kreal), intent(inout) :: r_sph(nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ntot_comp,nri_sph)
      integer(kind = kint), intent(inout) :: ierr
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: kr
!
!
      ierr = 1
      do kr = 1, nri_sph
        call sel_read_line_gz_stream(FPz_f, id_stream, flag_gzip, zbuf)
        if(zbuf%len_used .lt. 0) return
!
        read(zbuf%fixbuf(1),*,err=99) i_step, time,                     &
     &                 kr_sph(kr), r_sph(kr), spectr_IO(1:ntot_comp,kr)
      end do
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine sel_gz_read_layer_mean_mtr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
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
!  --------------------------------------------------------------------
!
      subroutine sel_gz_write_layer_mean_mtr                            &
     &         (flag_gzip, id_file, i_step, time,                       &
     &          nri_sph, kr_sph, r_sph, ntot_comp, spectr_IO, zbuf)
!
      use sph_monitor_data_text
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(in) :: nri_sph
      integer(kind = kint), intent(in) :: kr_sph(nri_sph)
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in) :: r_sph(nri_sph)
      integer(kind = kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: spectr_IO(ntot_comp,nri_sph)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: k
!
!
#ifdef ZLIB_IO
      if(flag_gzip) then
        call gz_write_layer_mean_monitor(id_file, i_step, time,         &
     &      nri_sph, kr_sph, r_sph, ntot_comp, spectr_IO, zbuf)
        return
      end if
#endif
!
      do k = 1, nri_sph
        write(id_file) layer_pwr_data_text(i_step, time,                &
     &                                     kr_sph(k), r_sph(k),         &
     &                                     ntot_comp, spectr_IO(1,k))
      end do
!
      end subroutine sel_gz_write_layer_mean_mtr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
#ifdef ZLIB_IO
      subroutine gz_write_layer_mean_monitor(id_file, i_step, time,     &
     &          nri_sph, kr_sph, r_sph, ntot_comp, spectr_IO, zbuf)
!
      use sph_monitor_data_text
      use gzip_defleate
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: nri_sph
      integer(kind = kint), intent(in) :: kr_sph(nri_sph)
      real(kind = kreal), intent(in) :: r_sph(nri_sph)
      integer(kind = kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: spectr_IO(ntot_comp,nri_sph)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: k, line_len
!
!
      line_len = len(layer_pwr_data_text(i_step, time,                  &
     &                                   kr_sph(1), r_sph(1),           &
     &                                   ntot_comp, spectr_IO(1,1)))
      zbuf%ilen_gz = int(dble(nri_sph*line_len)*1.01 + 24,              &
     &                   KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
      if(nri_sph .eq. 1) then
        call gzip_defleat_char_once(line_len,                           &
     &      layer_pwr_data_text(i_step, time, kr_sph(1), r_sph(1),      &
     &                               ntot_comp, spectr_IO(1,1)),        &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      else
        call gzip_defleat_char_begin(line_len,                          &
     &      layer_pwr_data_text(i_step, time, kr_sph(1), r_sph(1),      &
     &                               ntot_comp, spectr_IO(1,1)),        &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
        do k = 2, nri_sph-1
          call gzip_defleat_char_cont(line_len,                         &
     &        layer_pwr_data_text(i_step, time, kr_sph(k), r_sph(k),    &
     &                             ntot_comp, spectr_IO(1,k)), zbuf)
        end do
        k = nri_sph
        call gzip_defleat_char_last(line_len,                           &
     &      layer_pwr_data_text(i_step, time, kr_sph(k), r_sph(k),      &
     &                          ntot_comp, spectr_IO(1,k)), zbuf)
      end if
!
      write(id_file) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_layer_mean_monitor
#endif
!
! -----------------------------------------------------------------------
!
      end module gz_layer_mean_monitor_IO
