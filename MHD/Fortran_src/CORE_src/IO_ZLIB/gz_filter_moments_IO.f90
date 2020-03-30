!gz_filter_moments_IO.f90
!     module gz_filter_moments_IO
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine read_filter_elen_head_gz(nnod, nele, nf_type)
!      subroutine write_filter_elen_head_gz(nnod, nele, nf_type)
!
!      subroutine read_filter_moms_head_gz(nnod, nele, n_filter,        &
!     &          nf_type)
!      subroutine write_filter_moms_head_gz(nnod, nele,                 &
!     &          n_filter, nf_type)
!
!      subroutine read_elength_gz(num, el1, el2, el3)
!      subroutine read_mom_coefs_dx_gz(num, el1, el2, el3)
!
!      subroutine write_elength_gz(num, el1, el2, el3)
!      subroutine write_mom_coefs_dx_gz(num, el1, el2, el3)
!
      module gz_filter_moments_IO
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_elen_head_gz(nnod, nele, nf_type)
!
      use skip_gz_comment
!
      integer (kind=kint), intent(inout) :: nnod, nele, nf_type
!
!
      call skip_gz_comment_int(nnod, zbuf1)
      read(textbuf,*) nnod, nele
      call skip_gz_comment_int(nf_type, zbuf1)
!
      end subroutine read_filter_elen_head_gz
!
! ----------------------------------------------------------------------
!
      subroutine write_filter_elen_head_gz(nnod, nele, nf_type)
!
      use skip_gz_comment
!
      integer (kind=kint), intent(in) :: nnod, nele, nf_type
!
!
      write(textbuf,'(a,2a1)') '! number of node for filtering: ',      &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(2i16,2a1)') nnod, nele, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(textbuf,'(a,2a1)')  '! number of filter function ',         &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(i16,2a1)') nf_type, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      end subroutine write_filter_elen_head_gz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_filter_moms_head_gz(nnod, nele, n_filter,         &
     &          nf_type)
!
      use skip_gz_comment
!
      integer (kind=kint), intent(inout) :: nnod, nele
      integer (kind=kint), intent(inout) :: n_filter, nf_type
!
!
      call skip_gz_comment_int(nnod, zbuf1)
      read(textbuf,*) nnod, nele, n_filter
      call skip_gz_comment_int(nf_type, zbuf1)
!
      end subroutine read_filter_moms_head_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moms_head_gz(nnod, nele,                  &
     &          n_filter, nf_type)
!
      use skip_gz_comment
!
      integer (kind=kint), intent(in) :: nnod, nele
      integer (kind=kint), intent(in) :: n_filter, nf_type
!
!
      write(textbuf,'(a,2a1)') '! number of node for filtering: ',      &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(3i16,2a1)')  nnod, nele, n_filter,                &
     &                           char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(textbuf,'(a,2a1)')  '! number of filter function ',         &
     &                         char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(i16,2a1)') nf_type, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      end subroutine write_filter_moms_head_gz
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elength_gz(num, el1, el2, el3)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: el1(num), el2(num), el3(num)
!
      integer(kind = kint) :: i, itmp
!
!
      call skip_gz_comment_int(itmp, zbuf1)
      read(textbuf,*) itmp, el1(1), el2(1), el3(1)
      do i = 2, num
        call get_one_line_from_gz_f(zbuf1)
        read(textbuf,*) itmp, el1(i), el2(i), el3(i)
      end do
!
      end subroutine read_elength_gz
!
! ----------------------------------------------------------------------
!
      subroutine read_mom_coefs_dx_gz(num, el1, el2, el3)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(inout) :: el3(num,3)
!
      integer(kind = kint) :: nd, i, itmp
!
!
      do nd = 1, 3
        call skip_gz_comment_int(itmp, zbuf1)
        read(textbuf,*) itmp, itmp, el1(1,nd), el2(1,nd), el3(1,nd)
        do i = 2, num
          call get_one_line_from_gz_f(zbuf1)
          read(textbuf,*) itmp, itmp, el1(i,nd), el2(i,nd), el3(i,nd)
        end do
      end do
!
      end subroutine read_mom_coefs_dx_gz
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_elength_gz(num, el1, el2, el3)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: el1(num), el2(num), el3(num)
!
      integer(kind = kint) :: i
!
      do i = 1, num
        write(textbuf,'(i16,1p3E25.15e3,2a1)')                          &
     &     i, el1(i), el2(i), el3(i), char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf1)
      end do
!
      end subroutine write_elength_gz
!
! ----------------------------------------------------------------------
!
      subroutine write_mom_coefs_dx_gz(num, el1, el2, el3)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) ::  num
      real(kind = kreal), intent(in) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(in) :: el3(num,3)
!
      integer(kind = kint) :: nd, i
!
        do nd = 1, 3
          do i = 1, num
            write(textbuf,'(2i16,1p3E25.15e3,2a1)')                     &
     &        nd, i, el1(i,nd), el2(i,nd), el3(i,nd), char(10), char(0)
            call gz_write_textbuf_no_lf(zbuf1)
          end do
        end do
!
      end subroutine write_mom_coefs_dx_gz
!
! ----------------------------------------------------------------------
!
      end module gz_filter_moments_IO
