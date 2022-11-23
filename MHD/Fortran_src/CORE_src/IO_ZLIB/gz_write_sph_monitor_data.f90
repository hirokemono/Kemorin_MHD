!>@file   gz_write_sph_monitor_data.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!> @brief Time spectrum data output routines for utilities
!!
!!@verbatim
!!      subroutine gz_write_volume_spectr_monitor(id_file, i_step, time,&
!!     &          ltr, i_mode, n_comp, spectr_IO, zbuf)
!!        real(kind = kreal), intent(in) :: spectr_IO(n_comp,0:ltr)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: i_step, kr_sph
!!        real(kind = kreal), intent(in) :: time
!!        integer(kind = kint), intent(in) :: nri_sph, ltr
!!        integer(kind = kint), intent(in) :: kr_sph(nri_sph)
!!        real(kind = kreal), intent(in) :: r_sph(nri_sph)
!!        integer(kind = kint), intent(in) :: n_comp
!!        real(kind = kreal), intent(in) :: spectr_IO(n_comp,nri_sph)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!!
      module gz_write_sph_monitor_data
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
      subroutine gz_write_volume_spectr_monitor(id_file, i_step, time,  &
     &          ltr, i_mode, n_comp, spectr_IO, zbuf)
!
      use sph_monitor_data_text
      use gzip_defleate
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step, ltr
      integer(kind = kint), intent(in) :: i_mode(0:ltr)
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: n_comp
      real(kind = kreal), intent(in) :: spectr_IO(n_comp,0:ltr)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, line_len
!
!
      line_len = len(volume_spectr_data_text(i_step, time, i_mode(0),   &
     &                                      n_comp, spectr_IO(1,0)))
      zbuf%ilen_gz = int(dble((ltr+1)*line_len)*1.01 + 24,              &
     &                   KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
      call gzip_defleat_char_begin(line_len,                            &
     &    volume_spectr_data_text(i_step, time, i_mode(0),              &
     &                            n_comp, spectr_IO(1,0)),              &
     &    int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      do i = 1, ltr - 1
        call gzip_defleat_char_cont(line_len,                           &
     &      volume_spectr_data_text(i_step, time, i_mode(i),            &
     &                              n_comp, spectr_IO(1,i)), zbuf)
      end do
      i = ltr
      call gzip_defleat_char_last(line_len,                             &
     &    volume_spectr_data_text(i_step, time, i_mode(ltr),            &
     &                            n_comp, spectr_IO(1,ltr)), zbuf)
!
      write(id_file) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_volume_spectr_monitor
!
! -----------------------------------------------------------------------
!
      end module gz_write_sph_monitor_data
