!gz_filter_moms_elen_data_IO.f90
!     module gz_filter_moms_elen_data_IO
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!!      subroutine read_ref_filter_param_gz(filter_conf, zbuf)
!!        type(filter_config_type), intent(inout) ::  filter_conf
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine write_ref_filter_param_gz(filter_conf, zbuf)
!!        type(filter_config_type), intent(in) ::  filter_conf
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine read_elens_ele_gz                                    &
!!     &         (nele, el_e, el_ediff, el_edif2, zbuf)
!!        type(elen_on_ele_type), intent(inout) :: el_e
!!        type(elen_diffs_type), intent(inout) :: el_ediff
!!        type(elen_diffs_type), intent(inout) :: el_edif2
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine write_elens_ele_gz                                   &
!!     &         (nele, el_e, el_ediff, el_edif2, zbuf)
!!         type(elen_on_ele_type), intent(in) :: el_e
!!         type(elen_diffs_type), intent(in) :: el_ediff
!!         type(elen_diffs_type), intent(in) :: el_edif2
!!         type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine write_elens_nod_gz(nnod,                             &
!!     &          e_x2_nod, e_y2_nod, e_z2_nod,                         &
!!     &          e_xy_nod, e_yz_nod, e_zx_nod,                         &
!!     &          e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx,                &
!!     &          e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine read_filter_moms_ele_gz                              &
!!     &         (nele, moms, m_diff, m_diff2, zbuf)
!!        type(filter_mom_type), intent(inout) :: moms
!!        type(filter_mom_diffs_type), intent(inout) :: m_diff
!!        type(filter_mom_diffs_type), intent(inout) :: m_diff2
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine write_filter_moms_ele_gz                             &
!!     &         (nele, moms, m_diff, m_diff2, zbuf)
!!        type(filter_mom_type), intent(in) :: moms
!!        type(filter_mom_diffs_type), intent(in) :: m_diff
!!        type(filter_mom_diffs_type), intent(in) :: m_diff2
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!
      module gz_filter_moms_elen_data_IO
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
      subroutine read_ref_filter_param_gz(filter_conf, zbuf)
!
      use t_filter_elength
      use skip_gz_comment
!
      type(filter_config_type), intent(inout) ::  filter_conf
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer (kind=kint) :: itmp, ifil
!
!
      do ifil = 1, filter_conf%nf_type
        call skip_gz_comment_int(itmp, zbuf)
        read(zbuf%fixbuf(1),*) itmp, filter_conf%filter_type(ifil)
      end do
!
      call skip_gz_comment_real(filter_conf%f_width(1), zbuf)
      read(zbuf%fixbuf(1),*) filter_conf%f_width(1:zbuf%num_word)
      if(filter_conf%nf_type .gt. zbuf%num_word) then
        call read_gz_multi_real((filter_conf%nf_type - zbuf%num_word),  &
     &      filter_conf%f_width(zbuf%num_word+1), zbuf)
      end if
!
      do ifil = 1, filter_conf%nf_type
        call skip_gz_comment_int(itmp, zbuf)
        read(zbuf%fixbuf(1),*) itmp, filter_conf%xmom_1d_org(ifil,0:2)
      end do
!
      end subroutine read_ref_filter_param_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_ref_filter_param_gz(filter_conf, zbuf)
!
      use t_filter_elength
      use skip_gz_comment
!
      type(filter_config_type), intent(in) ::  filter_conf
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len=kchara) :: fmt_txt
      integer(kind = kint) :: ifil
!
!
      write(zbuf%fixbuf(1),'(a,2a1)') '! filter type ',                 &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      do ifil = 1, filter_conf%nf_type
        write(zbuf%fixbuf(1),'(i16,a,2a1)')                             &
     &     ifil, trim(filter_conf%filter_type(ifil)), char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end do
!
      write(zbuf%fixbuf(1),'(a,2a1)') '! filter width ',                &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      write(fmt_txt,'(a3,i1,a13)') '(1p', filter_conf%nf_type,          &
     &                            'E25.15e3,2a1)'
      write(zbuf%fixbuf(1),fmt_txt)                                     &
     &         filter_conf%f_width(1:filter_conf%nf_type),              &
     &         char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &        '! original 1d-moment of filters ', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &        '! (filter No., 0th, 1st, 2nd moment)', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      do ifil = 1, filter_conf%nf_type
        write(zbuf%fixbuf(1),'(i5,1p3E25.15e3,2a1)')                    &
     &       ifil, filter_conf%xmom_1d_org(ifil,0:2), char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end do
!
      end subroutine write_ref_filter_param_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elens_ele_gz                                      &
     &         (nele, el_e, el_ediff, el_edif2, zbuf)
!
      use t_filter_elength
      use skip_gz_comment
      use gz_filter_moments_IO
!
      integer(kind = kint), intent(in) :: nele
      type(elen_on_ele_type), intent(inout) :: el_e
      type(elen_diffs_type), intent(inout) :: el_ediff
      type(elen_diffs_type), intent(inout) :: el_edif2
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      call read_elength_gz                                              &
     &   (nele, el_e%f_x2, el_e%f_y2, el_e%f_z2, zbuf)
      call read_elength_gz                                              &
     &   (nele, el_e%f_xy, el_e%f_yz, el_e%f_zx, zbuf)
!
      call read_mom_coefs_dx_gz                                         &
     &   (nele, el_ediff%df_x2, el_ediff%df_y2, el_ediff%df_z2, zbuf)
      call read_mom_coefs_dx_gz                                         &
     &   (nele, el_ediff%df_xy, el_ediff%df_yz, el_ediff%df_zx, zbuf)
!
      call read_mom_coefs_dx_gz                                         &
     &   (nele, el_edif2%df_x2, el_edif2%df_y2, el_edif2%df_z2, zbuf)
      call read_mom_coefs_dx_gz                                         &
     &   (nele, el_edif2%df_xy, el_edif2%df_yz, el_edif2%df_zx, zbuf)
!
      end subroutine read_elens_ele_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_elens_ele_gz                                     &
     &         (nele, el_e, el_ediff, el_edif2, zbuf)
!
      use t_filter_elength
      use skip_gz_comment
      use gz_filter_moments_IO
!
      integer(kind = kint), intent(in) :: nele
!
      type(elen_on_ele_type), intent(in) :: el_e
      type(elen_diffs_type), intent(in) :: el_ediff
      type(elen_diffs_type), intent(in) :: el_edif2
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!    output coefs for filters for each element
!
      write(zbuf%fixbuf(1),'(a,2a1)')  '! dx^2 for each element',       &
     &                         char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! element ID, length of x, y, z', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_elength_gz                                             &
     &  (nele, el_e%f_x2, el_e%f_y2, el_e%f_z2, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')  '! dxdy for each element',       &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! element ID, length of x, y, z', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_elength_gz                                             &
     &   (nele, el_e%f_xy, el_e%f_yz, el_e%f_zx, zbuf)
!
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! 1st derivative of dx^2 for each element',                &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! direction of diffrenciate, ele ID, length of x, y, z',   &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nele, el_ediff%df_x2, el_ediff%df_y2, el_ediff%df_z2, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! 1st derivative of dxdy for each element',                &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! direction of diffrenciate, ele ID, length of x, y, z',   &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nele, el_ediff%df_xy, el_ediff%df_yz, el_ediff%df_zx, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! 2nd derivative of dx^2 for each element',                &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! direction of diffrenciate, ele ID, length of x, y, z',   &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nele, el_edif2%df_x2, el_edif2%df_y2, el_edif2%df_z2, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! 2nd derivative of dxdy for each element',                &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! direction of diffrenciate, ele ID, length of x, y, z',   &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nele, el_edif2%df_xy, el_edif2%df_yz, el_edif2%df_zx, zbuf)
!
      end subroutine write_elens_ele_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_elens_nod_gz(nnod,                               &
     &          e_x2_nod, e_y2_nod, e_z2_nod,                           &
     &          e_xy_nod, e_yz_nod, e_zx_nod,                           &
     &          e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx,                  &
     &          e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx, zbuf)
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
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!    output coefs for filters for each node
!
      write(zbuf%fixbuf(1),'(a,2a1)') '! dx^2 for each node',           &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '! node ID, length of x, y, z',   &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_elength_gz(nnod, e_x2_nod, e_y2_nod, e_z2_nod, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)') '! dxdy for each node',           &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)') '! node ID, length of x, y, z',   &
     &                         char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_elength_gz(nnod, e_xy_nod, e_yz_nod, e_zx_nod, zbuf)
!
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! 1st derivative of dx^2 for each node', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &    '! direction of diffrenciate, node ID, length of x, y, z',    &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nnod, e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! 1st derivative of dxdy for each node', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! direction of diffrenciate, node ID, length of x, y, z',  &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nnod, e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx, zbuf)
!
!
      end subroutine write_elens_nod_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moms_ele_gz                                &
     &         (nele, moms, m_diff, m_diff2, zbuf)
!
      use t_filter_moments
      use skip_gz_comment
      use gz_filter_moments_IO
!
      integer(kind = kint), intent(in) :: nele
      type(filter_mom_type), intent(inout) :: moms
      type(filter_mom_diffs_type), intent(inout) :: m_diff
      type(filter_mom_diffs_type), intent(inout) :: m_diff2
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call read_elength_gz(nele, moms%f_x2, moms%f_y2, moms%f_z2, zbuf)
      call read_elength_gz(nele, moms%f_xy, moms%f_yz, moms%f_zx, zbuf)
      call read_elength_gz(nele, moms%f_x,  moms%f_y,  moms%f_z,  zbuf)
!
!
      call read_mom_coefs_dx_gz                                         &
     &   (nele, m_diff%df_x2, m_diff%df_y2, m_diff%df_z2, zbuf)
      call read_mom_coefs_dx_gz                                         &
     &   (nele, m_diff%df_xy, m_diff%df_yz, m_diff%df_zx, zbuf)
      call read_mom_coefs_dx_gz                                         &
     &   (nele, m_diff%df_x,  m_diff%df_y,  m_diff%df_z,  zbuf)
!
!
      call read_mom_coefs_dx_gz                                         &
     &   (nele, m_diff2%df_x2, m_diff2%df_y2, m_diff2%df_z2, zbuf)
      call read_mom_coefs_dx_gz                                         &
     &   (nele, m_diff2%df_xy, m_diff2%df_yz, m_diff2%df_zx, zbuf)
      call read_mom_coefs_dx_gz                                         &
     &   (nele, m_diff2%df_x,  m_diff2%df_y,  m_diff2%df_z,  zbuf)
!
      end subroutine read_filter_moms_ele_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moms_ele_gz                               &
     &         (nele, moms, m_diff, m_diff2, zbuf)
!
      use skip_gz_comment
      use t_filter_moments
      use gz_filter_moments_IO
!
      integer(kind = kint), intent(in) :: nele
      type(filter_mom_type), intent(in) :: moms
      type(filter_mom_diffs_type), intent(in) :: m_diff
      type(filter_mom_diffs_type), intent(in) :: m_diff2
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! Second filter moments for each element',                 &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! element ID, x, y, z direction', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_elength_gz                                             &
     &   (nele, moms%f_x2, moms%f_y2, moms%f_z2, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! product of first order moment in 2 direction',           &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! element ID, x, y, z direction', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_elength_gz                                             &
     &   (nele, moms%f_xy, moms%f_yz, moms%f_zx, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! first filter moments for each element',                  &
     &      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &      '! element ID, x, y, z direction', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_elength_gz(nele, moms%f_x, moms%f_y, moms%f_z, zbuf)
!
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! 1st diff. of Second filter moments for each element',     &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nele, m_diff%df_x2, m_diff%df_y2, m_diff%df_z2, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! 1st diff. of product of first order moments in 2-dir.',   &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nele, m_diff%df_xy, m_diff%df_yz, m_diff%df_zx, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! 1st diff. of first filter moments for each element',      &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nele, m_diff%df_x, m_diff%df_y, m_diff%df_z, zbuf)
!
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! 2nd diff. of Second filter moments for each element',     &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nele, m_diff2%df_x2, m_diff2%df_y2, m_diff2%df_z2, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! 2nd diff. of product of first order moments in 2-dir.',   &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nele, m_diff2%df_xy, m_diff2%df_yz, m_diff2%df_zx, zbuf)
!
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! 2nd diff. of first filter moments for each element',      &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)')                                   &
     &     '! direction of difference, element ID, x, y, z direction',  &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      call write_mom_coefs_dx_gz                                        &
     &   (nele, m_diff2%df_x, m_diff2%df_y, m_diff2%df_z, zbuf)
!
      end subroutine write_filter_moms_ele_gz
!
!  ---------------------------------------------------------------------
!
      end module gz_filter_moms_elen_data_IO
