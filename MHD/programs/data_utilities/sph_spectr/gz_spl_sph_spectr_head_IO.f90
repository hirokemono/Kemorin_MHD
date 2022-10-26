!>@file   gz_spl_sph_spectr_head_IO.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief gzipped spectr monitor data reading routines
!!
!!@verbatim
!!      subroutine select_input_sph_series_head(FPz_f, id_stream,       &
!!     &          flag_gzip, flag_old_fmt, flag_spectr, flag_vol_ave,   &
!!     &          sph_IN, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip, flag_old_fmt
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_spl_sph_spectr_head_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
!
      implicit none
!
      private :: sel_read_sph_spectr_name, sel_gz_read_sph_monitor_head
      private :: gz_read_sph_pwr_vol_head, gz_read_sph_pwr_layer_head
      private :: select_num_of_labels_4_monitor
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine select_input_sph_series_head(FPz_f, id_stream,         &
     &          flag_gzip, flag_old_fmt, flag_spectr, flag_vol_ave,     &
     &          sph_IN, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip, flag_old_fmt
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      call select_num_of_labels_4_monitor                               &
     &   (flag_old_fmt, flag_spectr, flag_vol_ave, sph_IN)
      call sel_gz_read_sph_monitor_head                                 &
     &   (FPz_f, id_stream, flag_gzip, flag_vol_ave, sph_IN, zbuf)
!
      call alloc_sph_espec_name(sph_IN)
      call sel_read_sph_spectr_name(FPz_f, id_stream, flag_gzip,        &
     &   sph_IN%nfield_sph_spec, sph_IN%num_labels,                     &
     &   sph_IN%ncomp_sph_spec, sph_IN%ene_sph_spec_name, zbuf)
!
      if(flag_spectr .eqv. .FALSE.) sph_IN%ltr_sph = 0
      sph_IN%nri_dat = sph_IN%nri_sph
      if(flag_vol_ave) sph_IN%nri_dat = 1
      call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
!
      end subroutine select_input_sph_series_head
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine sel_gz_read_sph_monitor_head(FPz_f, id_stream,         &
     &          flag_gzip, flag_vol_ave, sph_IN, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip, flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if(flag_vol_ave) then
        call gz_read_sph_pwr_vol_head(FPz_f, id_stream, flag_gzip,      &
     &                                sph_IN, zbuf)
      else
        call gz_read_sph_pwr_layer_head(FPz_f, id_stream, flag_gzip,    &
     &                                  sph_IN, zbuf)
      end if
!
      end subroutine sel_gz_read_sph_monitor_head
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine gz_read_sph_pwr_vol_head                               &
     &         (FPz_f, id_stream, flag_gzip, sph_IN, zbuf)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nri_sph, sph_IN%ltr_sph
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_ICB, sph_IN%kr_CMB
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_inner, sph_IN%r_inner
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_outer, sph_IN%r_outer
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nfield_sph_spec,                    &
     &                      sph_IN%ntot_sph_spec
!
      end subroutine gz_read_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_sph_pwr_layer_head                             &
     &         (FPz_f, id_stream, flag_gzip, sph_IN, zbuf)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nri_sph, sph_IN%ltr_sph
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_ICB, sph_IN%kr_CMB
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      call sel_skip_comment_gz_stream                                   &
     &   (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nfield_sph_spec,                    &
     &    sph_IN%ntot_sph_spec
!
      end subroutine gz_read_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      subroutine sel_read_sph_spectr_name                               &
     &         (FPz_f, id_stream, flag_gzip, nfield_sph_spec,           &
     &          num_labels, ncomp_sph_spec, ene_sph_spec_name, zbuf)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: nfield_sph_spec, num_labels
!
      integer(kind = kint), intent(inout)                               &
     &                     :: ncomp_sph_spec(nfield_sph_spec)
      character(len = kchara), intent(inout)                            &
     &                     :: ene_sph_spec_name(num_labels)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      do
        call sel_skip_comment_gz_stream                                 &
     &     (FPz_f, id_stream, flag_gzip, zbuf)
        read(zbuf%fixbuf(1),*) ncomp_sph_spec(ist+1:ist+zbuf%num_word)
        if(ist+zbuf%num_word .ge. nfield_sph_spec) exit
        ist = ist+zbuf%num_word
      end do
        call sel_skip_comment_gz_stream                                 &
     &     (FPz_f, id_stream, flag_gzip, zbuf)
      read(zbuf%fixbuf(1),*) ene_sph_spec_name(1:num_labels)

      end subroutine sel_read_sph_spectr_name
!
!   --------------------------------------------------------------------
!
      subroutine select_num_of_labels_4_monitor                         &
     &         (flag_old_fmt, flag_spectr, flag_vol_ave, sph_IN)
!
      logical, intent(in) :: flag_old_fmt, flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      if(flag_vol_ave) then
        sph_IN%nri_sph = 1
        if(flag_spectr) then
          sph_IN%num_time_labels = 3
        else
          sph_IN%num_time_labels = 2
        end if
      else
        if(flag_spectr) then
          if(flag_old_fmt) then
            sph_IN%num_time_labels = 4
          else
            sph_IN%num_time_labels = 5
          end if
        else
          if(flag_old_fmt) then
            sph_IN%num_time_labels = 3
          else
            sph_IN%num_time_labels = 4
          end if
        end if
      end if
!
      end subroutine select_num_of_labels_4_monitor
!
!   --------------------------------------------------------------------
!
      end module gz_spl_sph_spectr_head_IO
