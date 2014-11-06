!filter_moms_elen_data_IO_b.f90
!     module filter_moms_elen_data_IO_b
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine read_ref_filter_param_b(id_file, nf_type,             &
!     &          filter_type, f_width, xmom_1d_org)
!      subroutine write_ref_filter_param_b(id_file, nf_type,            &
!     &          filter_type, f_width, xmom_1d_org)
!
!      subroutine read_elens_ele_b(id_file, nele,                       &
!     &         e_x2_ele, e_y2_ele, e_z2_ele,                           &
!     &         e_xy_ele, e_yz_ele, e_zx_ele,                           &
!     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                  &
!     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                  &
!     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,               &
!     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!      subroutine write_elens_ele_b(id_file, nele,                      &
!     &         e_x2_ele, e_y2_ele, e_z2_ele,                           &
!     &         e_xy_ele, e_yz_ele, e_zx_ele,                           &
!     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                  &
!     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                  &
!     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,               &
!     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!      subroutine write_elens_nod_b(id_file, nnod,                      &
!     &          e_x2_nod, e_y2_nod, e_z2_nod,                          &
!     &          e_xy_nod, e_yz_nod, e_zx_nod,                          &
!     &          e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx,                 &
!     &          e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx)
!
!      subroutine read_filter_moms_ele_b(id_file, nele,                 &
!     &         f_x2_ele, f_y2_ele, f_z2_ele,                           &
!     &         f_xy_ele, f_yz_ele, f_zx_ele,                           &
!     &         f_x_ele,  f_y_ele,  f_z_ele,                            &
!     &         f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx,                  &
!     &         f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx,                  &
!     &         f_x_ele_dx,  f_y_ele_dx,  f_z_ele_dx,                   &
!     &         f_x2_ele_dx2, f_y2_ele_dx2, f_z2_ele_dx2,               &
!     &         f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2,               &
!     &         f_x_ele_dx2,  f_y_ele_dx2,  f_z_ele_dx2)
!      subroutine write_filter_moms_ele_b(id_file, nele,                &
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
      module filter_moms_elen_data_IO_b
!
      use m_precision
!
      implicit none
!
      character(len=255), private  :: character_4_read
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ref_filter_param_b(id_file, nf_type,              &
     &          filter_type, f_width, xmom_1d_org)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nf_type
!
      character(len=kchara), intent(inout) :: filter_type(nf_type)
      real(kind=kreal), intent(inout) :: f_width(nf_type)
      real(kind=kreal), intent(inout) :: xmom_1d_org(nf_type,0:2)
!
      integer (kind=kint) :: ifil, j
!
!
      read(id_file) filter_type(1:nf_type)
      read(id_file) f_width(1:nf_type)
!
      read(id_file) ((xmom_1d_org(ifil,j),ifil=1,nf_type),j=0,2)
!
      end subroutine read_ref_filter_param_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_ref_filter_param_b(id_file, nf_type,             &
     &          filter_type, f_width, xmom_1d_org)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nf_type
      character(len=kchara), intent(in) :: filter_type(nf_type)
      real(kind=kreal), intent(in) :: f_width(nf_type)
      real(kind=kreal), intent(in) :: xmom_1d_org(nf_type,0:2)
!
      integer(kind = kint) :: ifil, j
!
!
      write(id_file) filter_type(1:nf_type)
      write(id_file) f_width(1:nf_type)
!
      write(id_file) ((xmom_1d_org(ifil,j),ifil=1,nf_type),j=0,2)
!
      end subroutine write_ref_filter_param_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elens_ele_b(id_file, nele,                        &
     &         e_x2_ele, e_y2_ele, e_z2_ele,                            &
     &         e_xy_ele, e_yz_ele, e_zx_ele,                            &
     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                   &
     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                   &
     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,                &
     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!
      use filter_moments_IO_b
!
      integer(kind = kint), intent(in) :: id_file, nele
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
      call read_elength_b(id_file, nele, e_x2_ele, e_y2_ele, e_z2_ele)
      call read_elength_b(id_file, nele, e_xy_ele, e_yz_ele, e_zx_ele)
!
      call read_mom_coefs_dx_b(id_file, nele,                           &
     &      e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx)
      call read_mom_coefs_dx_b(id_file, nele,                           &
     &      e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx)
!
      call read_mom_coefs_dx_b(id_file, nele,                           &
     &      e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2)
      call read_mom_coefs_dx_b(id_file, nele,                           &
     &      e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!
      end subroutine read_elens_ele_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_elens_nod_b(id_file, nnod,                       &
     &          e_x2_nod, e_y2_nod, e_z2_nod,                           &
     &          e_xy_nod, e_yz_nod, e_zx_nod,                           &
     &          e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx,                  &
     &          e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx)
!
      use filter_moments_IO_b
!
      integer(kind = kint), intent(in) :: id_file, nnod
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
      write(id_file,'(a)') '! dx^2 for each node'
      write(id_file,'(a)') '! node ID, length of x, y, z'
      call write_elength_b(id_file, nnod, e_x2_nod, e_y2_nod, e_z2_nod)
      write(id_file,'(a)') '! dxdy for each node'
      write(id_file,'(a)') '! node ID, length of x, y, z'
      call write_elength_b(id_file, nnod, e_xy_nod, e_yz_nod, e_zx_nod)
!
      write(id_file,'(a)') '! 1st derivative of dx^2 for each node'
      write(id_file,'(a)')                                             &
           '! direction of diffrenciate, node ID, length of x, y, z'
      call write_mom_coefs_dx_b(id_file, nnod,                         &
     &      e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx)
!
      write(id_file,'(a)') '! 1st derivative of dxdy for each node'
      write(id_file,'(a)')                                             &
           '! direction of diffrenciate, node ID, length of x, y, z'
      call write_mom_coefs_dx_b(id_file, nnod,                         &
     &      e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx)
!
!
      end subroutine write_elens_nod_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_elens_ele_b(id_file, nele,                       &
     &         e_x2_ele, e_y2_ele, e_z2_ele,                            &
     &         e_xy_ele, e_yz_ele, e_zx_ele,                            &
     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                   &
     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                   &
     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,                &
     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)

!
      use filter_moments_IO_b
!
      integer(kind = kint), intent(in) :: id_file, nele
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
!      write(id_file,'(a)')  '! dx^2 for each element'
!      write(id_file,'(a)')  '! element ID, length of x, y, z'
      call write_elength_b(id_file, nele, e_x2_ele, e_y2_ele, e_z2_ele)
!      write(id_file,'(a)')  '! dxdy for each element'
!      write(id_file,'(a)')  '! element ID, length of x, y, z'
      call write_elength_b(id_file, nele, e_xy_ele, e_yz_ele, e_zx_ele)
!
!      write(id_file,'(a)')  '! 1st derivative of dx^2 for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(id_file, nele,                          &
     &      e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx)
!
!      write(id_file,'(a)')  '! 1st derivative of dxdy for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(id_file, nele,                          &
     &      e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx)
!
!      write(id_file,'(a)')  '! 2nd derivative of dx^2 for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(id_file, nele,                          &
     &      e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2)
!
!      write(id_file,'(a)')  '! 2nd derivative of dxdy for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(id_file, nele,                          &
     &      e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!
      end subroutine write_elens_ele_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moms_ele_b(id_file, nele,                  &
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
      use filter_moments_IO_b
!
      integer(kind = kint), intent(in) :: id_file, nele
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
      call read_elength_b(id_file, nele, f_x2_ele, f_y2_ele, f_z2_ele)
      call read_elength_b(id_file, nele, f_xy_ele, f_yz_ele, f_zx_ele)
      call read_elength_b(id_file, nele, f_x_ele, f_y_ele, f_z_ele )
!
!
      call read_mom_coefs_dx_b(id_file, nele,                           &
     &    f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx )
      call read_mom_coefs_dx_b(id_file, nele,                           &
     &    f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx )
      call read_mom_coefs_dx_b(id_file, nele,                           &
     &    f_x_ele_dx, f_y_ele_dx, f_z_ele_dx )
!
!
      call read_mom_coefs_dx_b(id_file, nele,                           &
     &    f_x2_ele_dx2, f_y2_ele_dx2,f_z2_ele_dx2 )
      call read_mom_coefs_dx_b(id_file, nele,                           &
     &    f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2 )
      call read_mom_coefs_dx_b(id_file, nele,                           &
     &    f_x_ele_dx2, f_y_ele_dx2, f_z_ele_dx2 )
!
      end subroutine read_filter_moms_ele_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moms_ele_b(id_file, nele,                 &
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
      use filter_moments_IO_b
!
      integer(kind = kint), intent(in) :: id_file, nele
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
!      write(id_file,'(a)')  '! Second filter moments for each element'
!      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength_b(id_file, nele, f_x2_ele, f_y2_ele, f_z2_ele)
!     &     '! product of first order moment in 2 direction' 
!      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength_b(id_file, nele, f_xy_ele, f_yz_ele, f_zx_ele)
!      write(id_file,'(a)')  '! first filter moments for each element'
!      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength_b(id_file, nele, f_x_ele, f_y_ele, f_z_ele)
!
!     &     '! 1st diff. of Second filter moments for each element'
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(id_file, nele,                          &
     &    f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx )
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(id_file, nele,                          &
     &    f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx )
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(id_file, nele,                          &
     &    f_x_ele_dx, f_y_ele_dx, f_z_ele_dx )
!
!
!     &     '! 2nd diff. of Second filter moments for each element'
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(id_file, nele,                          &
     &    f_x2_ele_dx2, f_y2_ele_dx2, f_z2_ele_dx2)
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(id_file, nele,                          &
     &    f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2)
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(id_file, nele,                          &
     &    f_x_ele_dx2, f_y_ele_dx2, f_z_ele_dx2)
!
      end subroutine write_filter_moms_ele_b
!
!  ---------------------------------------------------------------------
!
      end module filter_moms_elen_data_IO_b
