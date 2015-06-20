!
!      module filter_moments_send_recv
!
!     Written by H. Matsui on Apr., 2008
!     Modified by H. Matsui on Apr., 2008
!
!      subroutine jacobi_nod_send_recv(filter_dxi)
!        type(dxdxi_data_type), save :: filter_dxi
!      subroutine dxidx_nod_send_recv
!
!      subroutine elength_nod_send_recv(elen_nod)
!      subroutine diff_elen_nod_send_recv(elen_nod)
!        type(elen_nod_diffs_type), intent(inout) :: elen_nod
!
!      subroutine filter_mom_nod_send_recv(ifil)
!      subroutine diff_filter_mom_nod_send_recv(ifil)
!        type(nod_mom_diffs_type), intent(inout) :: mom_nod
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
      subroutine jacobi_nod_send_recv(filter_dxi)
!
      use t_filter_dxdxi
!
      type(dxdxi_data_type), intent(inout) :: filter_dxi
!
!
      call nod_scalar_send_recv(filter_dxi%dxi_nod%dx%df_dxi)
      call nod_scalar_send_recv(filter_dxi%dxi_nod%dx%df_dei)
      call nod_scalar_send_recv(filter_dxi%dxi_nod%dx%df_dzi)
!
      call nod_scalar_send_recv(filter_dxi%dxi_nod%dy%df_dxi)
      call nod_scalar_send_recv(filter_dxi%dxi_nod%dy%df_dei)
      call nod_scalar_send_recv(filter_dxi%dxi_nod%dy%df_dzi)
!
      call nod_scalar_send_recv(filter_dxi%dxi_nod%dz%df_dxi)
      call nod_scalar_send_recv(filter_dxi%dxi_nod%dz%df_dei)
      call nod_scalar_send_recv(filter_dxi%dxi_nod%dz%df_dzi)
!
      end subroutine jacobi_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine dxidx_nod_send_recv(dx_nod)
!
      use t_filter_dxdxi
!
      type(dxidx_direction_type), intent(in) :: dx_nod
!
!
      call nod_scalar_send_recv(dx_nod%dxi%df_dx)
      call nod_scalar_send_recv(dx_nod%dxi%df_dy)
      call nod_scalar_send_recv(dx_nod%dxi%df_dz)
!
      call nod_scalar_send_recv(dx_nod%dei%df_dx)
      call nod_scalar_send_recv(dx_nod%dei%df_dy)
      call nod_scalar_send_recv(dx_nod%dei%df_dz)
!
      call nod_scalar_send_recv(dx_nod%dzi%df_dx)
      call nod_scalar_send_recv(dx_nod%dzi%df_dy)
      call nod_scalar_send_recv(dx_nod%dzi%df_dz)
!
      end subroutine dxidx_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elength_nod_send_recv(elen_nod)
!
      use t_filter_elength
!
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
!
!
      call nod_scalar_send_recv(elen_nod%moms%f_x2)
      call nod_scalar_send_recv(elen_nod%moms%f_y2)
      call nod_scalar_send_recv(elen_nod%moms%f_z2)
!
      call nod_scalar_send_recv(elen_nod%moms%f_xy)
      call nod_scalar_send_recv(elen_nod%moms%f_yz)
      call nod_scalar_send_recv(elen_nod%moms%f_zx)
!
      end subroutine elength_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_elen_nod_send_recv(elen_nod)
!
      use t_filter_elength
!
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
!
      call nod_vector_send_recv(elen_nod%diff%df_x2)
      call nod_vector_send_recv(elen_nod%diff%df_y2)
      call nod_vector_send_recv(elen_nod%diff%df_z2)
!
      call nod_vector_send_recv(elen_nod%diff%df_xy)
      call nod_vector_send_recv(elen_nod%diff%df_yz)
      call nod_vector_send_recv(elen_nod%diff%df_zx)
!
      end subroutine diff_elen_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine filter_mom_nod_send_recv(mom_nod)
!
      use t_filter_moments
!
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!
      call nod_scalar_send_recv(mom_nod%moms%f_x)
      call nod_scalar_send_recv(mom_nod%moms%f_y)
      call nod_scalar_send_recv(mom_nod%moms%f_z)
!
      call nod_scalar_send_recv(mom_nod%moms%f_x2)
      call nod_scalar_send_recv(mom_nod%moms%f_y2)
      call nod_scalar_send_recv(mom_nod%moms%f_z2)
!
      call nod_scalar_send_recv(mom_nod%moms%f_xy)
      call nod_scalar_send_recv(mom_nod%moms%f_yz)
      call nod_scalar_send_recv(mom_nod%moms%f_zx)
!
      end subroutine filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_filter_mom_nod_send_recv(mom_nod)
!
      use t_filter_moments
!
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!
      call nod_vector_send_recv(mom_nod%diff%df_x)
      call nod_vector_send_recv(mom_nod%diff%df_y)
      call nod_vector_send_recv(mom_nod%diff%df_z)
!
      call nod_vector_send_recv(mom_nod%diff%df_x2)
      call nod_vector_send_recv(mom_nod%diff%df_y2)
      call nod_vector_send_recv(mom_nod%diff%df_z2)
!
      call nod_vector_send_recv(mom_nod%diff%df_xy)
      call nod_vector_send_recv(mom_nod%diff%df_yz)
      call nod_vector_send_recv(mom_nod%diff%df_zx)
!
      end subroutine diff_filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      end module filter_moments_send_recv
