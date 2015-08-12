!>@file   filter_moms_elen_data_IO.f90
!!@brief  module filter_moms_elen_data_IO
!!
!!@author H. Matsui
!!@date Programmed in 2004
!!@n     modified by H. Matsui in Nov., 2006
!!@n     modified by H. Matsui in May, 2008
!
!> @brief file IO for filtering structure
!!
!!@verbatim
!!      subroutine read_ref_filter_param(id_file, nf_type, filter_type, &
!!     &          f_width, xmom_1d_org)
!!      subroutine write_ref_filter_param(id_file, nf_type, filter_type,&
!!     &          f_width, xmom_1d_org)
!!
!!      subroutine read_elens_ele(id_file, nele,                        &
!!     &         e_x2_ele, e_y2_ele, e_z2_ele,                          &
!!     &         e_xy_ele, e_yz_ele, e_zx_ele,                          &
!!     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                 &
!!     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                 &
!!     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,              &
!!     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!!      subroutine write_elens_ele(id_file, nele,                       &
!!     &         e_x2_ele, e_y2_ele, e_z2_ele,                          &
!!     &         e_xy_ele, e_yz_ele, e_zx_ele,                          &
!!     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                 &
!!     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                 &
!!     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,              &
!!     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!!      subroutine write_elens_nod(id_file, nnod,                       &
!!     &          e_x2_nod, e_y2_nod, e_z2_nod,                         &
!!     &          e_xy_nod, e_yz_nod, e_zx_nod,                         &
!!     &          e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx,                &
!!     &          e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx)
!!
!!      subroutine read_filter_moms_ele(id_file, nele,                  &
!!     &         f_x2_ele, f_y2_ele, f_z2_ele,                          &
!!     &         f_xy_ele, f_yz_ele, f_zx_ele,                          &
!!     &         f_x_ele,  f_y_ele,  f_z_ele,                           &
!!     &         f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx,                 &
!!     &         f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx,                 &
!!     &         f_x_ele_dx,  f_y_ele_dx,  f_z_ele_dx,                  &
!!     &         f_x2_ele_dx2, f_y2_ele_dx2, f_z2_ele_dx2,              &
!!     &         f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2,              &
!!     &         f_x_ele_dx2,  f_y_ele_dx2,  f_z_ele_dx2)
!!      subroutine write_filter_moms_ele(id_file, nele,                 &
!!     &         f_x2_ele, f_y2_ele, f_z2_ele,                          &
!!     &         f_xy_ele, f_yz_ele, f_zx_ele,                          &
!!     &         f_x_ele,  f_y_ele,  f_z_ele,                           &
!!     &         f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx,                 &
!!     &         f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx,                 &
!!     &         f_x_ele_dx,  f_y_ele_dx,  f_z_ele_dx,                  &
!!     &         f_x2_ele_dx2, f_y2_ele_dx2, f_z2_ele_dx2,              &
!!     &         f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2,              &
!!     &         f_x_ele_dx2,  f_y_ele_dx2,  f_z_ele_dx2)
!!@verbatim
!
      module filter_moms_elen_data_IO
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
      subroutine read_ref_filter_param(id_file, nf_type, filter_type,   &
     &          f_width, xmom_1d_org)
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
      integer (kind=kint) :: itmp, ifil
!
!
      do ifil = 1, nf_type
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*) itmp, filter_type(ifil)
      end do
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) f_width(1:nf_type)
!
!
      do ifil = 1, nf_type
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*) itmp, xmom_1d_org(ifil,0:2)
      end do
!
      end subroutine read_ref_filter_param
!
!  ---------------------------------------------------------------------
!
      subroutine write_ref_filter_param(id_file, nf_type, filter_type,  &
     &          f_width, xmom_1d_org)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nf_type
      character(len=kchara), intent(in) :: filter_type(nf_type)
      real(kind=kreal), intent(in) :: f_width(nf_type)
      real(kind=kreal), intent(in) :: xmom_1d_org(nf_type,0:2)
!
      integer(kind = kint) :: ifil
!
!
      write(id_file,'(a)') '! filter type '
      do ifil = 1, nf_type
        write(id_file,'(i16,a2,a)') ifil, '  ',                         &
     &                       trim(filter_type(ifil))
      end do
      write(id_file,'(a)') '! filter width '
      write(id_file,'(1p10E25.15e3)') f_width(1:nf_type)
!
      write(id_file,'(a)') '! original 1d-moment of filters '
      write(id_file,'(a)') '! (filter No., 0th, 1st, 2nd moment)'
      do ifil = 1, nf_type
        write(id_file,'(i5,1p3E25.15e3)') ifil, xmom_1d_org(ifil,0:2)
      end do
!
      end subroutine write_ref_filter_param
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elens_ele(id_file, nele,                          &
     &         e_x2_ele, e_y2_ele, e_z2_ele,                            &
     &         e_xy_ele, e_yz_ele, e_zx_ele,                            &
     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                   &
     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                   &
     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,                &
     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!
      use filter_moments_IO
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
        call read_elength(id_file, nele, e_x2_ele, e_y2_ele, e_z2_ele )
        call read_elength(id_file, nele, e_xy_ele, e_yz_ele, e_zx_ele )
!
        call read_mom_coefs_dx(id_file, nele,                           &
     &      e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx)
        call read_mom_coefs_dx(id_file, nele,                           &
     &      e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx)
!
        call read_mom_coefs_dx(id_file, nele,                           &
     &      e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2)
        call read_mom_coefs_dx(id_file, nele,                           &
     &      e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!
      end subroutine read_elens_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_elens_nod(id_file, nnod,                         &
     &          e_x2_nod, e_y2_nod, e_z2_nod,                           &
     &          e_xy_nod, e_yz_nod, e_zx_nod,                           &
     &          e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx,                  &
     &          e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx)
!
      use filter_moments_IO
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
      call write_elength(id_file, nnod, e_x2_nod, e_y2_nod, e_z2_nod )
      write(id_file,'(a)') '! dxdy for each node'
      write(id_file,'(a)') '! node ID, length of x, y, z'
      call write_elength(id_file, nnod, e_xy_nod, e_yz_nod, e_zx_nod )
!
      write(id_file,'(a)') '! 1st derivative of dx^2 for each node'
      write(id_file,'(a)')                                             &
           '! direction of diffrenciate, node ID, length of x, y, z'
      call write_mom_coefs_dx(id_file, nnod,                           &
     &      e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx)
!
      write(id_file,'(a)') '! 1st derivative of dxdy for each node'
      write(id_file,'(a)')                                             &
           '! direction of diffrenciate, node ID, length of x, y, z'
      call write_mom_coefs_dx(id_file, nnod,                           &
     &      e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx)
!
!
      end subroutine write_elens_nod
!
!  ---------------------------------------------------------------------
!
      subroutine write_elens_ele(id_file, nele,                         &
     &         e_x2_ele, e_y2_ele, e_z2_ele,                            &
     &         e_xy_ele, e_yz_ele, e_zx_ele,                            &
     &         e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx,                   &
     &         e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx,                   &
     &         e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2,                &
     &         e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)

!
      use filter_moments_IO
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
      write(id_file,'(a)')  '! dx^2 for each element'
      write(id_file,'(a)')  '! element ID, length of x, y, z'
      call write_elength(id_file, nele, e_x2_ele, e_y2_ele, e_z2_ele )
      write(id_file,'(a)')  '! dxdy for each element'
      write(id_file,'(a)')  '! element ID, length of x, y, z'
      call write_elength(id_file, nele, e_xy_ele, e_yz_ele, e_zx_ele )
!
      write(id_file,'(a)')  '! 1st derivative of dx^2 for each element'
      write(id_file,'(a)')                                              &
     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx(id_file, nele,                            &
     &      e_x2_ele_dx, e_y2_ele_dx, e_z2_ele_dx)
!
      write(id_file,'(a)')  '! 1st derivative of dxdy for each element'
      write(id_file,'(a)')                                              &
     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx(id_file, nele,                            &
     &      e_xy_ele_dx, e_yz_ele_dx, e_zx_ele_dx)
!
      write(id_file,'(a)')  '! 2nd derivative of dx^2 for each element'
      write(id_file,'(a)')                                              &
     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx(id_file, nele,                            &
     &      e_x2_ele_dx2, e_y2_ele_dx2, e_z2_ele_dx2)
!
      write(id_file,'(a)')  '! 2nd derivative of dxdy for each element'
      write(id_file,'(a)')                                              &
     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx(id_file, nele,                            &
     &      e_xy_ele_dx2, e_yz_ele_dx2, e_zx_ele_dx2)
!
      end subroutine write_elens_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moms_ele(id_file, nele,                    &
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
      use filter_moments_IO
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
      call read_elength(id_file, nele, f_x2_ele, f_y2_ele, f_z2_ele )
      call read_elength(id_file, nele, f_xy_ele, f_yz_ele, f_zx_ele )
      call read_elength(id_file, nele, f_x_ele, f_y_ele, f_z_ele )
!
!
      call read_mom_coefs_dx(id_file, nele,                             &
     &    f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx )
      call read_mom_coefs_dx(id_file, nele,                             &
     &    f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx )
      call read_mom_coefs_dx(id_file, nele,                             &
     &    f_x_ele_dx, f_y_ele_dx, f_z_ele_dx )
!
!
      call read_mom_coefs_dx(id_file, nele,                             &
     &    f_x2_ele_dx2, f_y2_ele_dx2,f_z2_ele_dx2 )
      call read_mom_coefs_dx(id_file, nele,                             &
     &    f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2 )
      call read_mom_coefs_dx(id_file, nele,                             &
     &    f_x_ele_dx2, f_y_ele_dx2, f_z_ele_dx2 )
!
      end subroutine read_filter_moms_ele
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moms_ele(id_file, nele,                   &
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
      use filter_moments_IO
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
      write(id_file,'(a)')  '! Second filter moments for each element'
      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength(id_file, nele, f_x2_ele, f_y2_ele, f_z2_ele )
      write(id_file,'(a)')                                              &
     &     '! product of first order moment in 2 direction' 
      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength(id_file, nele, f_xy_ele, f_yz_ele, f_zx_ele )
      write(id_file,'(a)')  '! first filter moments for each element'
      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength(id_file, nele, f_x_ele, f_y_ele, f_z_ele )
!
      write(id_file,'(a)')                                              &
     &     '! 1st diff. of Second filter moments for each element'
      write(id_file,'(a)')                                              &
     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx(id_file, nele,                            &
     &    f_x2_ele_dx, f_y2_ele_dx, f_z2_ele_dx )
      write(id_file,'(a)')                                              &
     &     '! 1st diff. of product of first order moments in 2-dir.'
      write(id_file,'(a)')                                              &
     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx(id_file, nele,                            &
     &    f_xy_ele_dx, f_yz_ele_dx, f_zx_ele_dx )
      write(id_file,'(a)')                                              &
     &     '! 1st diff. of first filter moments for each element'
      write(id_file,'(a)')                                              &
     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx(id_file, nele,                            &
     &    f_x_ele_dx, f_y_ele_dx, f_z_ele_dx )
!
!
      write(id_file,'(a)')                                              &
     &     '! 2nd diff. of Second filter moments for each element'
      write(id_file,'(a)')                                              &
     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx(id_file, nele,                            &
     &    f_x2_ele_dx2, f_y2_ele_dx2, f_z2_ele_dx2)
      write(id_file,'(a)')                                              &
     &     '! 2nd diff. of product of first order moments in 2-dir.'
      write(id_file,'(a)')                                              &
     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx(id_file, nele,                            &
     &    f_xy_ele_dx2, f_yz_ele_dx2, f_zx_ele_dx2)
      write(id_file,'(a)')                                              &
     &     '! 2nd diff. of first filter moments for each element'
      write(id_file,'(a)')                                              &
     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx(id_file, nele,                            &
     &    f_x_ele_dx2, f_y_ele_dx2, f_z_ele_dx2)
!
      end subroutine write_filter_moms_ele
!
!  ---------------------------------------------------------------------
!
      end module filter_moms_elen_data_IO
