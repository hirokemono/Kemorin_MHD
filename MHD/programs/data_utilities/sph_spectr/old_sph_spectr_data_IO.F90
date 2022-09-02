!>@file   old_sph_spectr_data_IO.F90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Old spectrum monitor data IO for utilities
!!
!!@verbatim
!!      subroutine  gz_read_layer_pwr_sph_old(FPz_f, sph_IN, zbuf, ierr)
!!      subroutine gz_read_layer_spectr_sph_old                         &
!!     &         (FPz_f, sph_IN, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine  read_layer_pwr_sph_old(id_file, sph_IN, ierr)
!!      subroutine  read_layer_spectr_sph_old(id_file, sph_IN, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!@endverbatim
!
      module old_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
#ifdef ZLIB_IO
!
      subroutine  gz_read_layer_pwr_sph_old(FPz_f, sph_IN, zbuf, ierr)
!
      use t_buffer_4_gzip
      use gzip_file_access
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: kr
!
!
      ierr = 0
      do kr = 1, sph_IN%nri_sph
        call get_one_line_text_from_gz(FPz_f, zbuf)
        if(check_gzfile_eof(FPz_f) .gt. 0) then
          ierr = -1
          return
        end if
!
        read(zbuf%fixbuf(1),*,err=99)                                   &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph(kr),              &
     &      sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,kr)
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine gz_read_layer_pwr_sph_old
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_layer_spectr_sph_old                           &
     &         (FPz_f, sph_IN, zbuf, ierr)
!
      use t_buffer_4_gzip
      use gzip_file_access
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: kr, lth
!
!
      ierr = 0
      do kr = 1, sph_IN%nri_sph
        do lth = 0, sph_IN%ltr_sph
          call get_one_line_text_from_gz(FPz_f, zbuf)
          if(check_gzfile_eof(FPz_f) .gt. 0) then
            ierr = -1
            return
          end if
!
          read(zbuf%fixbuf(1),*,err=99) sph_IN%i_step, sph_IN%time,     &
     &        sph_IN%kr_sph(kr), sph_IN%i_mode(lth),                    &
     &        sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,kr)
        end do
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine gz_read_layer_spectr_sph_old
#endif
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine  read_layer_pwr_sph_old(id_file, sph_IN, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: kr
!
!
      ierr = 0
      do kr = 1, sph_IN%nri_sph
        read(id_file,*,err=99,end=99)                                   &
     &      sph_IN%i_step, sph_IN%time, sph_IN%kr_sph(kr),              &
     &      sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,kr)
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_layer_pwr_sph_old
!
!   --------------------------------------------------------------------
!
      subroutine  read_layer_spectr_sph_old(id_file, sph_IN, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: kr, lth
!
!
      ierr = 0
      do kr = 1, sph_IN%nri_sph
        do lth = 0, sph_IN%ltr_sph
          read(id_file,*,err=99,end=99) sph_IN%i_step, sph_IN%time,     &
     &        sph_IN%kr_sph(kr), sph_IN%i_mode(lth),                    &
     &        sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,kr)
        end do
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_layer_spectr_sph_old
!
!   --------------------------------------------------------------------
!
      end module old_sph_spectr_data_IO
