!>@file   gz_spl_sph_spectr_data_IO.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief gzipped spectr monitor data reading routines
!!
!!@verbatim
!!      subroutine gz_read_sph_spectr_time(FPz_f, nitem_snap,           &
!!     &                                   i_step, time, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_file, nitem_snap
!!        integer(kind = kint), intent(inout) :: i_step, ierr
!!        real(kind = kreal), intent(inout) :: time
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine sel_gz_input_sph_series_data                         &
!!     &         (FPz_f, flag_old_fmt, flag_spectr, flag_vol_ave,       &
!!     &          sph_IN, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        logical, intent(in) :: flag_old_fmt, flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!
!!      subroutine gz_copy_spectr_monitor_data(FPz_f, id_ascii,         &
!!     &                                       zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_ascii
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
      module gz_spl_sph_spectr_data_IO
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
      private :: gz_read_volume_spectr_sph, gz_read_volume_pwr_sph
      private :: gz_read_layer_spectr_sph,  gz_read_layer_pwr_sph
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_sph_spectr_time(FPz_f, nitem_snap,             &
     &                                   i_step, time, zbuf, ierr)
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: nitem_snap
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: ipick
!
!
      ierr = 0
      do ipick = 1, nitem_snap
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*,err=99,end=99) i_step, time
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine gz_read_sph_spectr_time
!
! -----------------------------------------------------------------------
!
      subroutine sel_gz_input_sph_series_data                           &
     &         (FPz_f, flag_old_fmt, flag_spectr, flag_vol_ave,         &
     &          sph_IN, zbuf, ierr)
!
      use old_sph_spectr_data_IO
!
      character, pointer, intent(in) :: FPz_f
      logical, intent(in) :: flag_old_fmt, flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(flag_vol_ave) then
        if(flag_spectr) then
          call gz_read_volume_spectr_sph(FPz_f, sph_IN, zbuf, ierr)
        else
          call gz_read_volume_pwr_sph(FPz_f, sph_IN, zbuf, ierr)
        end if
      else
        if(flag_spectr) then
          if(flag_old_fmt) then
            call gz_read_layer_spectr_sph_old(FPz_f, sph_IN,            &
     &                                        zbuf, ierr)
          else
            call gz_read_layer_spectr_sph(FPz_f, sph_IN, zbuf, ierr)
          end if
        else
          if(flag_old_fmt) then
            call gz_read_layer_pwr_sph_old(FPz_f, sph_IN, zbuf, ierr)
          else
            call gz_read_layer_pwr_sph(FPz_f, sph_IN, zbuf, ierr)
          end if
        end if
      end if
!
      end subroutine sel_gz_input_sph_series_data
!
!   --------------------------------------------------------------------
!
      subroutine gz_copy_spectr_monitor_data(FPz_f, id_ascii,           &
     &                                       zbuf, ierr)
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_ascii
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 1
      call get_one_line_text_from_gz(FPz_f, zbuf)
      if(check_gzfile_eof(FPz_f) .gt. 0) return
!
      write(id_ascii,'(a)') zbuf%fixbuf(1)(1:zbuf%len_used-1)
      ierr = 0
!
      end subroutine gz_copy_spectr_monitor_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine gz_read_volume_pwr_sph(FPz_f, sph_IN, zbuf, ierr)
!
      character, pointer, intent(in) :: FPz_f
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      call get_one_line_text_from_gz(FPz_f, zbuf)
      if(check_gzfile_eof(FPz_f) .gt. 0) then
        ierr = -1
        return
      end if
!
      read(zbuf%fixbuf(1),*,err=99) sph_IN%i_step, sph_IN%time,         &
     &             sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,1)
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine gz_read_volume_pwr_sph
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_volume_spectr_sph(FPz_f, sph_IN, zbuf, ierr)
!
      character, pointer, intent(in) :: FPz_f
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: lth
!
!
      ierr = 0
      do lth = 0, sph_IN%ltr_sph
        call get_one_line_text_from_gz(FPz_f, zbuf)
        if(check_gzfile_eof(FPz_f) .gt. 0) then
          ierr = -1
          return
        end if
!
        read(zbuf%fixbuf(1),*,err=99)                                   &
     &               sph_IN%i_step, sph_IN%time, sph_IN%i_mode(lth),    &
     &               sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,1)
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine gz_read_volume_spectr_sph
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_layer_pwr_sph(FPz_f, sph_IN, zbuf, ierr)
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
        read(zbuf%fixbuf(1),*,err=99) sph_IN%i_step, sph_IN%time,       &
     &      sph_IN%kr_sph(kr), sph_IN%r_sph(kr),                        &
     &      sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,0,kr)
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine gz_read_layer_pwr_sph
!
!   --------------------------------------------------------------------
!
      subroutine gz_read_layer_spectr_sph(FPz_f, sph_IN, zbuf, ierr)
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
     &        sph_IN%kr_sph(kr), sph_IN%r_sph(kr), sph_IN%i_mode(lth),  &
     &        sph_IN%spectr_IO(1:sph_IN%ntot_sph_spec,lth,kr)
          end do
        end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine gz_read_layer_spectr_sph
!
!   --------------------------------------------------------------------
!
      end module gz_spl_sph_spectr_data_IO
