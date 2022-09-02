!>@file   gz_spl_sph_spectr_head_IO.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief gzipped spectr monitor data reading routines
!!
!!@verbatim
!!      subroutine sel_gz_read_sph_monitor_head                         &
!!     &         (FPz_f, flag_vol_ave, sph_IN, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        logical, intent(in) :: flag_vol_ave
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_sph_spectr_name(FPz_f, nfield_sph_spec,      &
!!     &          num_labels, ncomp_sph_spec, ene_sph_spec_name, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: nfield_sph_spec, num_labels
!!        integer(kind = kint), intent(inout)                           &
!!     &                     :: ncomp_sph_spec(nfield_sph_spec)
!!        character(len = kchara), intent(inout)                        &
!!     &                     :: ene_sph_spec_name(num_labels)
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
      use gzip_file_access
      use skip_gz_comment
!
!
      implicit none
!
      private :: gz_read_sph_pwr_vol_head, gz_read_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sel_gz_read_sph_monitor_head                           &
     &         (FPz_f, flag_vol_ave, sph_IN, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      logical, intent(in) :: flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if(flag_vol_ave) then
        call gz_read_sph_pwr_vol_head(FPz_f, sph_IN, zbuf)
      else
        call gz_read_sph_pwr_layer_head(FPz_f, sph_IN, zbuf)
      end if
      return
!
      end subroutine sel_gz_read_sph_monitor_head
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine gz_read_sph_pwr_vol_head(FPz_f, sph_IN, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nri_sph, sph_IN%ltr_sph
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_ICB, sph_IN%kr_CMB
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_inner, sph_IN%r_inner
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_outer, sph_IN%r_outer
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nfield_sph_spec,                    &
     &                      sph_IN%ntot_sph_spec
!
      end subroutine gz_read_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_sph_pwr_layer_head(FPz_f, sph_IN, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nri_sph, sph_IN%ltr_sph
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%kr_ICB, sph_IN%kr_CMB
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      call skip_gz_comment_get_nword(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec
!
      end subroutine gz_read_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_sph_spectr_name(FPz_f, nfield_sph_spec,        &
     &          num_labels, ncomp_sph_spec, ene_sph_spec_name, zbuf)
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: nfield_sph_spec, num_labels
!
      integer(kind = kint), intent(inout)                               &
     &                     :: ncomp_sph_spec(nfield_sph_spec)
      character(len = kchara), intent(inout)                            &
     &                     :: ene_sph_spec_name(num_labels)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call get_one_line_text_from_gz(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) ncomp_sph_spec(1:nfield_sph_spec)
      call get_one_line_text_from_gz(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) ene_sph_spec_name(1:num_labels)

      end subroutine gz_read_sph_spectr_name
!
!   --------------------------------------------------------------------
!
      end module gz_spl_sph_spectr_head_IO
