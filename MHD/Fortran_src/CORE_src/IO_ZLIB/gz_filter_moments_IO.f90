!gz_filter_moments_IO.f90
!     module gz_filter_moments_IO
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!!      subroutine read_filter_elen_head_gz(nnod, nele, nf_type, zbuf)
!!      subroutine write_filter_elen_head_gz(nnod, nele, nf_type, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine read_filter_moms_head_gz                             &
!!     &         (nnod, nele, n_filter, nf_type, zbuf)
!!      subroutine write_filter_moms_head_gz(nnod, nele,                &
!!     &          n_filter, nf_type, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine read_elength_gz(num, el1, el2, el3, zbuf)
!!      subroutine read_mom_coefs_dx_gz(num, el1, el2, el3, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine write_elength_gz(num, el1, el2, el3, zbuf)
!!      subroutine write_mom_coefs_dx_gz(num, el1, el2, el3, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!
      module gz_filter_moments_IO
!
      use m_precision
      use t_buffer_4_gzip
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_elen_head_gz(nnod, nele, nf_type, zbuf)
!
      use skip_gz_comment
!
      integer (kind=kint), intent(inout) :: nnod, nele, nf_type
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call skip_gz_comment_int(nnod, zbuf)
      read(zbuf%fixbuf(1),*) nnod, nele
      call skip_gz_comment_int(nf_type, zbuf)
!
      end subroutine read_filter_elen_head_gz
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_elen_head_gz(nnod, nele, nf_type, zbuf)
!
      use skip_gz_comment
!
      integer (kind=kint), intent(in) :: nnod, nele, nf_type
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! number of node for filtering: ', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(2i16,2a1)') nnod, nele, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)') '! number of filter function ',   &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(i16,2a1)') nf_type, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      end subroutine write_filter_elen_head_gz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_moms_head_gz                               &
     &         (nnod, nele, n_filter, nf_type, zbuf)
!
      use skip_gz_comment
!
      integer (kind=kint), intent(inout) :: nnod, nele
      integer (kind=kint), intent(inout) :: n_filter, nf_type
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call skip_gz_comment_int(nnod, zbuf)
      read(zbuf%fixbuf(1),*) nnod, nele, n_filter
      call skip_gz_comment_int(nf_type, zbuf)
!
      end subroutine read_filter_moms_head_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moms_head_gz(nnod, nele,                  &
     &          n_filter, nf_type, zbuf)
!
      use skip_gz_comment
!
      integer (kind=kint), intent(in) :: nnod, nele
      integer (kind=kint), intent(in) :: n_filter, nf_type
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &            '! number of node for filtering: ', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(3i16,2a1)') nnod, nele, n_filter,          &
     &                                   char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')  '! number of filter function ',  &
     &                                 char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(i16,2a1)') nf_type, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      end subroutine write_filter_moms_head_gz
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elength_gz(num, el1, el2, el3, zbuf)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: el1(num), el2(num), el3(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i, itmp
!
!
      call skip_gz_comment_int(itmp, zbuf)
      read(zbuf%fixbuf(1),*) itmp, el1(1), el2(1), el3(1)
      do i = 2, num
        call get_one_line_text_from_gz(zbuf)
        read(zbuf%fixbuf(1),*) itmp, el1(i), el2(i), el3(i)
      end do
!
      end subroutine read_elength_gz
!
! ----------------------------------------------------------------------
!
      subroutine read_mom_coefs_dx_gz(num, el1, el2, el3, zbuf)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(inout) :: el3(num,3)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: nd, i, itmp
!
!
      do nd = 1, 3
        call skip_gz_comment_int(itmp, zbuf)
        read(zbuf%fixbuf(1),*)                                          &
     &         itmp, itmp, el1(1,nd), el2(1,nd), el3(1,nd)
        do i = 2, num
          call get_one_line_text_from_gz(zbuf)
          read(zbuf%fixbuf(1),*)                                        &
     &         itmp, itmp, el1(i,nd), el2(i,nd), el3(i,nd)
        end do
      end do
!
      end subroutine read_mom_coefs_dx_gz
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_elength_gz(num, el1, el2, el3, zbuf)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: el1(num), el2(num), el3(num)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: i
!
      do i = 1, num
        write(zbuf%fixbuf(1),'(i16,1p3E25.15e3,2a1)')                   &
     &     i, el1(i), el2(i), el3(i), char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end do
!
      end subroutine write_elength_gz
!
! ----------------------------------------------------------------------
!
      subroutine write_mom_coefs_dx_gz(num, el1, el2, el3, zbuf)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) ::  num
      real(kind = kreal), intent(in) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(in) :: el3(num,3)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: nd, i
!
        do nd = 1, 3
          do i = 1, num
            write(zbuf%fixbuf(1),'(2i16,1p3E25.15e3,2a1)')              &
     &        nd, i, el1(i,nd), el2(i,nd), el3(i,nd), char(10), char(0)
            call gz_write_textbuf_no_lf(zbuf)
          end do
        end do
!
      end subroutine write_mom_coefs_dx_gz
!
! ----------------------------------------------------------------------
!
      end module gz_filter_moments_IO
