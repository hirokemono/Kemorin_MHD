!
!      module filter_moments_send_recv
!
!     Written by H. Matsui on Apr., 2008
!     Modified by H. Matsui on Apr., 2008
!
!      subroutine jacobi_nod_send_recv
!      subroutine dxidx_nod_send_recv
!
!      subroutine elength_nod_send_recv
!      subroutine diff_elen_nod_send_recv
!
!      subroutine filter_mom_nod_send_recv(ifil)
!      subroutine diff_filter_mom_nod_send_recv(ifil)
!
      module filter_moments_send_recv
!
      use m_precision
      use nodal_vector_send_recv
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine jacobi_nod_send_recv
!
      use m_filter_dxdxi
!
!
      call nod_scalar_send_recv(filter_dxi1%dxi_nod%dx%df_dxi)
      call nod_scalar_send_recv(filter_dxi1%dxi_nod%dx%df_dei)
      call nod_scalar_send_recv(filter_dxi1%dxi_nod%dx%df_dzi)
!
      call nod_scalar_send_recv(filter_dxi1%dxi_nod%dy%df_dxi)
      call nod_scalar_send_recv(filter_dxi1%dxi_nod%dy%df_dei)
      call nod_scalar_send_recv(filter_dxi1%dxi_nod%dy%df_dzi)
!
      call nod_scalar_send_recv(filter_dxi1%dxi_nod%dz%df_dxi)
      call nod_scalar_send_recv(filter_dxi1%dxi_nod%dz%df_dei)
      call nod_scalar_send_recv(filter_dxi1%dxi_nod%dz%df_dzi)
!
      end subroutine jacobi_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine dxidx_nod_send_recv
!
      use m_dxi_dxes_3d_node
!
!
      call nod_scalar_send_recv(dxidx_nod(1))
      call nod_scalar_send_recv(deidx_nod(1))
      call nod_scalar_send_recv(dzidx_nod(1))
!
      call nod_scalar_send_recv(dxidy_nod(1))
      call nod_scalar_send_recv(deidy_nod(1))
      call nod_scalar_send_recv(dzidy_nod(1))
!
      call nod_scalar_send_recv(dxidz_nod(1))
      call nod_scalar_send_recv(deidz_nod(1))
      call nod_scalar_send_recv(dzidz_nod(1))
!
      end subroutine dxidx_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elength_nod_send_recv
!
      use m_filter_elength
!
!
      call nod_scalar_send_recv(elenn%moms%f_x2)
      call nod_scalar_send_recv(elenn%moms%f_y2)
      call nod_scalar_send_recv(elenn%moms%f_z2)
!
      call nod_scalar_send_recv(elenn%moms%f_xy)
      call nod_scalar_send_recv(elenn%moms%f_yz)
      call nod_scalar_send_recv(elenn%moms%f_zx)
!
      end subroutine elength_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_elen_nod_send_recv
!
      use m_filter_elength
!
!
      call nod_vector_send_recv(elenn%diff%df_x2)
      call nod_vector_send_recv(elenn%diff%df_y2)
      call nod_vector_send_recv(elenn%diff%df_z2)
!
      call nod_vector_send_recv(elenn%diff%df_xy)
      call nod_vector_send_recv(elenn%diff%df_yz)
      call nod_vector_send_recv(elenn%diff%df_zx)
!
      end subroutine diff_elen_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine filter_mom_nod_send_recv(ifil)
!
      use m_filter_moments
!
!
      integer(kind = kint), intent(in) :: ifil
!
      call nod_scalar_send_recv(filter_x_nod(1,ifil))
      call nod_scalar_send_recv(filter_y_nod(1,ifil))
      call nod_scalar_send_recv(filter_z_nod(1,ifil))
!
      call nod_scalar_send_recv(filter_x2_nod(1,ifil))
      call nod_scalar_send_recv(filter_y2_nod(1,ifil))
      call nod_scalar_send_recv(filter_z2_nod(1,ifil))
!
      call nod_scalar_send_recv(filter_xy_nod(1,ifil))
      call nod_scalar_send_recv(filter_yz_nod(1,ifil))
      call nod_scalar_send_recv(filter_zx_nod(1,ifil))
!
      end subroutine filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_filter_mom_nod_send_recv(ifil)
!
      use m_filter_moments
!
!
      integer(kind = kint), intent(in) :: ifil
!
      call nod_vector_send_recv(filter_x_nod_dx(1,1,ifil))
      call nod_vector_send_recv(filter_y_nod_dx(1,1,ifil))
      call nod_vector_send_recv(filter_z_nod_dx(1,1,ifil))
!
      call nod_vector_send_recv(filter_x2_nod_dx(1,1,ifil))
      call nod_vector_send_recv(filter_y2_nod_dx(1,1,ifil))
      call nod_vector_send_recv(filter_z2_nod_dx(1,1,ifil))
!
      call nod_vector_send_recv(filter_xy_nod_dx(1,1,ifil))
      call nod_vector_send_recv(filter_yz_nod_dx(1,1,ifil))
      call nod_vector_send_recv(filter_zx_nod_dx(1,1,ifil))
!
      end subroutine diff_filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      end module filter_moments_send_recv
