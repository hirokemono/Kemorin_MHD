!gz_filter_moms_elen_data_IO.f90
!     module gz_filter_moms_elen_data_IO
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine read_ref_filter_param_gz(nf_type,                     &
!     &          filter_type, f_width, xmom_1d_org)
!      subroutine write_ref_filter_param_gz(nf_type,                    &
!     &          filter_type, f_width, xmom_1d_org)
!
!      subroutine read_elens_ele_gz(nele,                               &
!     &         e_x2_ele, e_y2_ele, e_z2_ele,                           &
!     &         e_xy_ele, e_yz_ele, e_zx_ele,                           &
!     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                  &
!     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                  &
!     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,               &
!     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!      subroutine write_elens_ele_gz(nele,                              &
!     &         e_x2_ele, e_y2_ele, e_z2_ele,                           &
!     &         e_xy_ele, e_yz_ele, e_zx_ele,                           &
!     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                  &
!     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                  &
!     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,               &
!     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!      subroutine write_elens_nod_gz(nnod,                              &
!     &          e_x2_nod, e_y2_nod, e_z2_nod,                          &
!     &          e_xy_nod, e_yz_nod, e_zx_nod,                          &
!     &          e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx,                 &
!     &          e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx)
!
!      subroutine read_filter_moms_ele_gz(nele,                         &
!     &         f_x2_ele, f_y2_ele, f_z2_ele,                           &
!     &         f_xy_ele, f_yz_ele, f_zx_ele,                           &
!     &         f_x_ele,  f_y_ele,  f_z_ele,                            &
!     &         f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx,                  &
!     &         f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx,                  &
!     &         f_x_ele_dx,  f_y_ele_dx,  f_z_ele_dx,                   &
!     &         f_x2_ele_dx2, f_y2_ele_dx2, f_z2_ele_dx2,               &
!     &         f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2,               &
!     &         f_x_ele_dx2,  f_y_ele_dx2,  f_z_ele_dx2)
!      subroutine write_filter_moms_ele_gz(nele,                        &
!     &         f_x2_ele, f_y2_ele, f_z2_ele,                           &
!     &         f_xy_ele, f_yz_ele, f_zx_ele,                           &
!     &         f_x_ele,  f_y_ele,  f_z_ele,                            &
!     &         f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx,                  &
!     &         f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx,                  &
!     &         f_x_ele_dx,  f_y_ele_dx,  f_z_ele_dx,                   &
!     &         f_x2_ele_dx2, f_y2_ele_dx2, f_z2_ele_dx2,               &
!     &         f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2,               &
!     &         f_x_ele_dx2,  f_y_ele_dx2,  f_z_ele_dx2)
!
      module gz_filter_moms_elen_data_IO
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
      subroutine read_ref_filter_param_gz(nf_type,                      &
     &          filter_type, f_width, xmom_1d_org)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: nf_type
!
      character(len=kchara), intent(inout) :: filter_type(nf_type)
      real(kind=kreal), intent(inout) :: f_width(nf_type)
      real(kind=kreal), intent(inout) :: xmom_1d_org(nf_type,0:2)
!
      integer (kind=kint) :: itmp, ifil
!
!
      do ifil = 1, nf_type
        call skip_gz_comment_int(itmp, zbuf1)
        read(textbuf,*) itmp, filter_type(ifil)
      end do
!
      call skip_gz_comment_real(f_width(1), zbuf1)
      read(textbuf,*) f_width(1:zbuf1%num_word)
      if(nf_type .gt. zbuf1%num_word) then
        call read_gz_multi_real((nf_type - zbuf1%num_word),             &
     &      f_width(zbuf1%num_word+1), zbuf1)
      end if
!
      do ifil = 1, nf_type
        call skip_gz_comment_int(itmp, zbuf1)
        read(textbuf,*) itmp, xmom_1d_org(ifil,0:2)
      end do
!
      end subroutine read_ref_filter_param_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_ref_filter_param_gz(nf_type,                     &
     &          filter_type, f_width, xmom_1d_org)
!
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: nf_type
      character(len=kchara), intent(in) :: filter_type(nf_type)
      real(kind=kreal), intent(in) :: f_width(nf_type)
      real(kind=kreal), intent(in) :: xmom_1d_org(nf_type,0:2)
!
      character(len=kchara) :: fmt_txt
      integer(kind = kint) :: ifil
!
!
      write(textbuf,'(a,2a1)') '! filter type ', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      do ifil = 1, nf_type
        write(textbuf,'(i16,a,2a1)')                                    &
     &           ifil, trim(filter_type(ifil)), char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf1)
      end do
!
      write(textbuf,'(a,2a1)') '! filter width ', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(fmt_txt,'(a3,i1,a13)') '(1p', nf_type, 'E25.15e3,2a1)'
      write(textbuf,fmt_txt) f_width(1:nf_type), char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(textbuf,'(a,2a1)')                                          &
     &        '! original 1d-moment of filters ', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &        '! (filter No., 0th, 1st, 2nd moment)', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      do ifil = 1, nf_type
        write(textbuf,'(i5,1p3E25.15e3,2a1)')                           &
     &         ifil, xmom_1d_org(ifil,0:2), char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf1)
      end do
!
      end subroutine write_ref_filter_param_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elens_ele_gz(nele,                                &
     &         e_x2_ele, e_y2_ele, e_z2_ele,                            &
     &         e_xy_ele, e_yz_ele, e_zx_ele,                            &
     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                   &
     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                   &
     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,                &
     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!
      use skip_gz_comment
      use gz_filter_moments_IO
!
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(inout) :: e_x2_ele(nele)
      real(kind = kreal), intent(inout) :: e_y2_ele(nele)
      real(kind = kreal), intent(inout) :: e_z2_ele(nele)
!
      real(kind = kreal), intent(inout) :: e_xy_ele(nele)
      real(kind = kreal), intent(inout) :: e_yz_ele(nele)
      real(kind = kreal), intent(inout) :: e_zx_ele(nele)
!
!
      real(kind = kreal), intent(inout) :: e_x2_ele_dx(nele,3)
      real(kind = kreal), intent(inout) :: e_y2_ele_dx(nele,3)
      real(kind = kreal), intent(inout) :: e_z2_ele_dx(nele,3)
!
      real(kind = kreal), intent(inout) :: e_xy_ele_dx(nele,3)
      real(kind = kreal), intent(inout) :: e_yz_ele_dx(nele,3)
      real(kind = kreal), intent(inout) :: e_zx_ele_dx(nele,3)
!
      real(kind = kreal), intent(inout) :: e_x2_ele_dx2(nele,3)
      real(kind = kreal), intent(inout) :: e_y2_ele_dx2(nele,3)
      real(kind = kreal), intent(inout) :: e_z2_ele_dx2(nele,3)
!
      real(kind = kreal), intent(inout) :: e_xy_ele_dx2(nele,3)
      real(kind = kreal), intent(inout) :: e_yz_ele_dx2(nele,3)
      real(kind = kreal), intent(inout) :: e_zx_ele_dx2(nele,3)
!
!
      call read_elength_gz(nele, e_x2_ele, e_y2_ele, e_z2_ele )
      call read_elength_gz(nele, e_xy_ele, e_yz_ele, e_zx_ele )
!
      call read_mom_coefs_dx_gz(nele,                                   &
     &      e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx)
      call read_mom_coefs_dx_gz(nele,                                   &
     &      e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx)
!
      call read_mom_coefs_dx_gz(nele,                                   &
     &      e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2)
      call read_mom_coefs_dx_gz(nele,                                   &
     &      e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!
      end subroutine read_elens_ele_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_elens_nod_gz(nnod,                               &
     &          e_x2_nod, e_y2_nod, e_z2_nod,                           &
     &          e_xy_nod, e_yz_nod, e_zx_nod,                           &
     &          e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx,                  &
     &          e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx)
!
      use skip_gz_comment
      use gz_filter_moments_IO
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: e_x2_nod(nnod)
      real(kind = kreal), intent(in) :: e_y2_nod(nnod)
      real(kind = kreal), intent(in) :: e_z2_nod(nnod)
!
      real(kind = kreal), intent(in) :: e_xy_nod(nnod)
      real(kind = kreal), intent(in) :: e_yz_nod(nnod)
      real(kind = kreal), intent(in) :: e_zx_nod(nnod)
!
!
      real(kind = kreal), intent(in) :: e_x2_nod_dx(nnod,3)
      real(kind = kreal), intent(in) :: e_y2_nod_dx(nnod,3)
      real(kind = kreal), intent(in) :: e_z2_nod_dx(nnod,3)
!
      real(kind = kreal), intent(in) :: e_xy_nod_dx(nnod,3)
      real(kind = kreal), intent(in) :: e_yz_nod_dx(nnod,3)
      real(kind = kreal), intent(in) :: e_zx_nod_dx(nnod,3)
!
!
!    output coefs for filters for each node
!
      write(textbuf,'(a,2a1)') '! dx^2 for each node',                  &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)') '! node ID, length of x, y, z',          &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_elength_gz(nnod, e_x2_nod, e_y2_nod, e_z2_nod )
!
      write(textbuf,'(a,2a1)') '! dxdy for each node',                  &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)') '! node ID, length of x, y, z',          &
     &                         char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_elength_gz(nnod, e_xy_nod, e_yz_nod, e_zx_nod)
!
!
      write(textbuf,'(a,2a1)')                                          &
     &      '! 1st derivative of dx^2 for each node', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &    '! direction of diffrenciate, node ID, length of x, y, z',    &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nnod,                                  &
     &      e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx)
!
      write(textbuf,'(a,2a1)')                                          &
     &      '! 1st derivative of dxdy for each node', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &      '! direction of diffrenciate, node ID, length of x, y, z',  &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nnod,                                  &
     &      e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx)
!
!
      end subroutine write_elens_nod_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_elens_ele_gz(nele,                               &
     &         e_x2_ele, e_y2_ele, e_z2_ele,                            &
     &         e_xy_ele, e_yz_ele, e_zx_ele,                            &
     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                   &
     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                   &
     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,                &
     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!
      use skip_gz_comment
      use gz_filter_moments_IO
!
      integer(kind = kint), intent(in) :: nele
!
      real(kind = kreal), intent(in) :: e_x2_ele(nele)
      real(kind = kreal), intent(in) :: e_y2_ele(nele)
      real(kind = kreal), intent(in) :: e_z2_ele(nele)
!
      real(kind = kreal), intent(in) :: e_xy_ele(nele)
      real(kind = kreal), intent(in) :: e_yz_ele(nele)
      real(kind = kreal), intent(in) :: e_zx_ele(nele)
!
!
      real(kind = kreal), intent(in) :: e_x2_ele_dx(nele,3)
      real(kind = kreal), intent(in) :: e_y2_ele_dx(nele,3)
      real(kind = kreal), intent(in) :: e_z2_ele_dx(nele,3)
!
      real(kind = kreal), intent(in) :: e_xy_ele_dx(nele,3)
      real(kind = kreal), intent(in) :: e_yz_ele_dx(nele,3)
      real(kind = kreal), intent(in) :: e_zx_ele_dx(nele,3)
!
      real(kind = kreal), intent(in) :: e_x2_ele_dx2(nele,3)
      real(kind = kreal), intent(in) :: e_y2_ele_dx2(nele,3)
      real(kind = kreal), intent(in) :: e_z2_ele_dx2(nele,3)
!
      real(kind = kreal), intent(in) :: e_xy_ele_dx2(nele,3)
      real(kind = kreal), intent(in) :: e_yz_ele_dx2(nele,3)
      real(kind = kreal), intent(in) :: e_zx_ele_dx2(nele,3)
!
!    output coefs for filters for each element
!
      write(textbuf,'(a,2a1)')  '! dx^2 for each element',              &
     &                         char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &      '! element ID, length of x, y, z', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_elength_gz(nele, e_x2_ele, e_y2_ele, e_z2_ele)
!
      write(textbuf,'(a,2a1)')  '! dxdy for each element',              &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &      '! element ID, length of x, y, z', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_elength_gz(nele, e_xy_ele, e_yz_ele, e_zx_ele)
!
!
      write(textbuf,'(a,2a1)')                                          &
     &      '! 1st derivative of dx^2 for each element',                &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &      '! direction of diffrenciate, ele ID, length of x, y, z',   &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nele,                                  &
     &      e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx)
!
      write(textbuf,'(a,2a1)')                                          &
     &      '! 1st derivative of dxdy for each element',                &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &      '! direction of diffrenciate, ele ID, length of x, y, z',   &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nele,                                  &
     &      e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx)
!
      write(textbuf,'(a,2a1)')                                          &
     &      '! 2nd derivative of dx^2 for each element',                &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &      '! direction of diffrenciate, ele ID, length of x, y, z',   &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nele,                                  &
     &      e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2)
!
      write(textbuf,'(a,2a1)')                                          &
     &      '! 2nd derivative of dxdy for each element',                &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &      '! direction of diffrenciate, ele ID, length of x, y, z',   &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nele,                                  &
     &      e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!
      end subroutine write_elens_ele_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moms_ele_gz(nele,                          &
     &         f_x2_ele, f_y2_ele, f_z2_ele,                            &
     &         f_xy_ele, f_yz_ele, f_zx_ele,                            &
     &         f_x_ele,  f_y_ele,  f_z_ele,                             &
     &         f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx,                   &
     &         f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx,                   &
     &         f_x_ele_dx,  f_y_ele_dx,  f_z_ele_dx,                    &
     &         f_x2_ele_dx2, f_y2_ele_dx2, f_z2_ele_dx2,                &
     &         f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2,                &
     &         f_x_ele_dx2,  f_y_ele_dx2,  f_z_ele_dx2)
!
      use skip_gz_comment
      use gz_filter_moments_IO
!
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(inout) :: f_x2_ele(nele)
      real(kind = kreal), intent(inout) :: f_y2_ele(nele)
      real(kind = kreal), intent(inout) :: f_z2_ele(nele)
!
      real(kind = kreal), intent(inout) :: f_xy_ele(nele)
      real(kind = kreal), intent(inout) :: f_yz_ele(nele)
      real(kind = kreal), intent(inout) :: f_zx_ele(nele)
!
      real(kind = kreal), intent(inout) :: f_x_ele(nele)
      real(kind = kreal), intent(inout) :: f_y_ele(nele)
      real(kind = kreal), intent(inout) :: f_z_ele(nele)
!
!
      real(kind = kreal), intent(inout) :: f_x2_ele_dx(nele,3)
      real(kind = kreal), intent(inout) :: f_y2_ele_dx(nele,3)
      real(kind = kreal), intent(inout) :: f_z2_ele_dx(nele,3)
!
      real(kind = kreal), intent(inout) :: f_xy_ele_dx(nele,3)
      real(kind = kreal), intent(inout) :: f_yz_ele_dx(nele,3)
      real(kind = kreal), intent(inout) :: f_zx_ele_dx(nele,3)
!
      real(kind = kreal), intent(inout) :: f_x_ele_dx(nele,3)
      real(kind = kreal), intent(inout) :: f_y_ele_dx(nele,3)
      real(kind = kreal), intent(inout) :: f_z_ele_dx(nele,3)
!
      real(kind = kreal), intent(inout) :: f_x2_ele_dx2(nele,3)
      real(kind = kreal), intent(inout) :: f_y2_ele_dx2(nele,3)
      real(kind = kreal), intent(inout) :: f_z2_ele_dx2(nele,3)
!
      real(kind = kreal), intent(inout) :: f_xy_ele_dx2(nele,3)
      real(kind = kreal), intent(inout) :: f_yz_ele_dx2(nele,3)
      real(kind = kreal), intent(inout) :: f_zx_ele_dx2(nele,3)
!
      real(kind = kreal), intent(inout) :: f_x_ele_dx2(nele,3)
      real(kind = kreal), intent(inout) :: f_y_ele_dx2(nele,3)
      real(kind = kreal), intent(inout) :: f_z_ele_dx2(nele,3)
!
!
      call read_elength_gz(nele, f_x2_ele, f_y2_ele, f_z2_ele )
      call read_elength_gz(nele, f_xy_ele, f_yz_ele, f_zx_ele )
      call read_elength_gz(nele, f_x_ele, f_y_ele, f_z_ele )
!
!
      call read_mom_coefs_dx_gz(nele,                                   &
     &    f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx )
      call read_mom_coefs_dx_gz(nele,                                   &
     &    f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx )
      call read_mom_coefs_dx_gz(nele,                                   &
     &    f_x_ele_dx, f_y_ele_dx, f_z_ele_dx )
!
!
      call read_mom_coefs_dx_gz(nele,                                   &
     &    f_x2_ele_dx2, f_y2_ele_dx2,f_z2_ele_dx2 )
      call read_mom_coefs_dx_gz(nele,                                   &
     &    f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2 )
      call read_mom_coefs_dx_gz(nele,                                   &
     &    f_x_ele_dx2, f_y_ele_dx2, f_z_ele_dx2 )
!
      end subroutine read_filter_moms_ele_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moms_ele_gz(nele,                         &
     &         f_x2_ele, f_y2_ele, f_z2_ele,                            &
     &         f_xy_ele, f_yz_ele, f_zx_ele,                            &
     &         f_x_ele,  f_y_ele,  f_z_ele,                             &
     &         f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx,                   &
     &         f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx,                   &
     &         f_x_ele_dx,  f_y_ele_dx,  f_z_ele_dx,                    &
     &         f_x2_ele_dx2, f_y2_ele_dx2, f_z2_ele_dx2,                &
     &         f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2,                &
     &         f_x_ele_dx2,  f_y_ele_dx2,  f_z_ele_dx2)
!
      use skip_gz_comment
      use gz_filter_moments_IO
!
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(in) :: f_x2_ele(nele)
      real(kind = kreal), intent(in) :: f_y2_ele(nele)
      real(kind = kreal), intent(in) :: f_z2_ele(nele)
!
      real(kind = kreal), intent(in) :: f_xy_ele(nele)
      real(kind = kreal), intent(in) :: f_yz_ele(nele)
      real(kind = kreal), intent(in) :: f_zx_ele(nele)
!
      real(kind = kreal), intent(in) :: f_x_ele(nele)
      real(kind = kreal), intent(in) :: f_y_ele(nele)
      real(kind = kreal), intent(in) :: f_z_ele(nele)
!
!
      real(kind = kreal), intent(in) :: f_x2_ele_dx(nele,3)
      real(kind = kreal), intent(in) :: f_y2_ele_dx(nele,3)
      real(kind = kreal), intent(in) :: f_z2_ele_dx(nele,3)
!
      real(kind = kreal), intent(in) :: f_xy_ele_dx(nele,3)
      real(kind = kreal), intent(in) :: f_yz_ele_dx(nele,3)
      real(kind = kreal), intent(in) :: f_zx_ele_dx(nele,3)
!
      real(kind = kreal), intent(in) :: f_x_ele_dx(nele,3)
      real(kind = kreal), intent(in) :: f_y_ele_dx(nele,3)
      real(kind = kreal), intent(in) :: f_z_ele_dx(nele,3)
!
      real(kind = kreal), intent(in) :: f_x2_ele_dx2(nele,3)
      real(kind = kreal), intent(in) :: f_y2_ele_dx2(nele,3)
      real(kind = kreal), intent(in) :: f_z2_ele_dx2(nele,3)
!
      real(kind = kreal), intent(in) :: f_xy_ele_dx2(nele,3)
      real(kind = kreal), intent(in) :: f_yz_ele_dx2(nele,3)
      real(kind = kreal), intent(in) :: f_zx_ele_dx2(nele,3)
!
      real(kind = kreal), intent(in) :: f_x_ele_dx2(nele,3)
      real(kind = kreal), intent(in) :: f_y_ele_dx2(nele,3)
      real(kind = kreal), intent(in) :: f_z_ele_dx2(nele,3)
!
!
!
      write(textbuf,'(a,2a1)')                                          &
     &      '! Second filter moments for each element',                 &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &      '! element ID, x, y, z direction', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_elength_gz(nele, f_x2_ele, f_y2_ele, f_z2_ele )
!
      write(textbuf,'(a,2a1)')                                          &
     &      '! product of first order moment in 2 direction',           &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &      '! element ID, x, y, z direction', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_elength_gz(nele, f_xy_ele, f_yz_ele, f_zx_ele )
!
      write(textbuf,'(a,2a1)')                                          &
     &      '! first filter moments for each element',                  &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &      '! element ID, x, y, z direction', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_elength_gz(nele, f_x_ele, f_y_ele, f_z_ele )
!
!
      write(textbuf,'(a,2a1)')                                          &
     &     '! 1st diff. of Second filter moments for each element',     &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nele,                                  &
     &    f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx )
!
      write(textbuf,'(a,2a1)')                                          &
     &     '! 1st diff. of product of first order moments in 2-dir.',   &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nele,                                  &
     &    f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx )
!
      write(textbuf,'(a,2a1)')                                          &
     &     '! 1st diff. of first filter moments for each element',      &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nele,                                  &
     &    f_x_ele_dx, f_y_ele_dx, f_z_ele_dx )
!
!
      write(textbuf,'(a,2a1)')                                          &
     &     '! 2nd diff. of Second filter moments for each element',     &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nele,                                  &
     &    f_x2_ele_dx2, f_y2_ele_dx2, f_z2_ele_dx2)
!
      write(textbuf,'(a,2a1)')                                          &
     &     '! 2nd diff. of product of first order moments in 2-dir.',   &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nele,                                  &
     &    f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2)
!
      write(textbuf,'(a,2a1)')                                          &
     &     '! 2nd diff. of first filter moments for each element',      &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(textbuf,'(a,2a1)')                                          &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_mom_coefs_dx_gz(nele,                                  &
     &    f_x_ele_dx2, f_y_ele_dx2, f_z_ele_dx2)
!
      end subroutine write_filter_moms_ele_gz
!
!  ---------------------------------------------------------------------
!
      end module gz_filter_moms_elen_data_IO
