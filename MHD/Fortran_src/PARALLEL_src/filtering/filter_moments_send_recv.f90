!
!      module filter_moments_send_recv
!
!     Written by H. Matsui on Apr., 2008
!     Modified by H. Matsui on Apr., 2008
!
!!      subroutine jacobi_nod_send_recv(numnod, nod_comm, filter_dxi)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(dxdxi_data_type), save :: filter_dxi
!!      subroutine dxidx_nod_send_recv(numnod, nod_comm, dx_nod)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(dxidx_direction_type), intent(inout) :: dx_nod
!!
!!      subroutine elength_nod_send_recv(numnod, nod_comm, elen_nod)
!!      subroutine diff_elen_nod_send_recv(numnod, nod_comm, elen_nod)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(elen_nod_diffs_type), intent(inout) :: elen_nod
!!
!!      subroutine filter_mom_nod_send_recv(numnod, nod_comm, mom_nod)
!!      subroutine diff_filter_mom_nod_send_recv                        &
!!     &         (numnod, nod_comm, mom_nod)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
      module filter_moments_send_recv
!
      use m_precision
      use t_comm_table
      use nod_phys_send_recv
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine jacobi_nod_send_recv(numnod, nod_comm, filter_dxi)
!
      use t_filter_dxdxi
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(dxdxi_data_type), intent(inout) :: filter_dxi
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dx%df_dxi)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dx%df_dei)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dx%df_dzi)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dy%df_dxi)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dy%df_dei)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dy%df_dzi)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dz%df_dxi)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dz%df_dei)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dz%df_dzi)
!
      end subroutine jacobi_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine dxidx_nod_send_recv(numnod, nod_comm, dx_nod)
!
      use t_filter_dxdxi
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(dxidx_direction_type), intent(inout) :: dx_nod
!
!
      call nod_scalar_send_recv(numnod, nod_comm, dx_nod%dxi%df_dx)
      call nod_scalar_send_recv(numnod, nod_comm, dx_nod%dxi%df_dy)
      call nod_scalar_send_recv(numnod, nod_comm, dx_nod%dxi%df_dz)
!
      call nod_scalar_send_recv(numnod, nod_comm, dx_nod%dei%df_dx)
      call nod_scalar_send_recv(numnod, nod_comm, dx_nod%dei%df_dy)
      call nod_scalar_send_recv(numnod, nod_comm, dx_nod%dei%df_dz)
!
      call nod_scalar_send_recv(numnod, nod_comm, dx_nod%dzi%df_dx)
      call nod_scalar_send_recv(numnod, nod_comm, dx_nod%dzi%df_dy)
      call nod_scalar_send_recv(numnod, nod_comm, dx_nod%dzi%df_dz)
!
      end subroutine dxidx_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elength_nod_send_recv(numnod, nod_comm, elen_nod)
!
      use t_filter_elength
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
!
!
      call nod_scalar_send_recv(numnod, nod_comm, elen_nod%moms%f_x2)
      call nod_scalar_send_recv(numnod, nod_comm, elen_nod%moms%f_y2)
      call nod_scalar_send_recv(numnod, nod_comm, elen_nod%moms%f_z2)
!
      call nod_scalar_send_recv(numnod, nod_comm, elen_nod%moms%f_xy)
      call nod_scalar_send_recv(numnod, nod_comm, elen_nod%moms%f_yz)
      call nod_scalar_send_recv(numnod, nod_comm, elen_nod%moms%f_zx)
!
      end subroutine elength_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_elen_nod_send_recv(numnod, nod_comm, elen_nod)
!
      use t_filter_elength
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
!
      call nod_vector_send_recv(numnod, nod_comm, elen_nod%diff%df_x2)
      call nod_vector_send_recv(numnod, nod_comm, elen_nod%diff%df_y2)
      call nod_vector_send_recv(numnod, nod_comm, elen_nod%diff%df_z2)
!
      call nod_vector_send_recv(numnod, nod_comm, elen_nod%diff%df_xy)
      call nod_vector_send_recv(numnod, nod_comm, elen_nod%diff%df_yz)
      call nod_vector_send_recv(numnod, nod_comm, elen_nod%diff%df_zx)
!
      end subroutine diff_elen_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine filter_mom_nod_send_recv(numnod, nod_comm, mom_nod)
!
      use t_filter_moments
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!
      call nod_scalar_send_recv(numnod, nod_comm, mom_nod%moms%f_x)
      call nod_scalar_send_recv(numnod, nod_comm, mom_nod%moms%f_y)
      call nod_scalar_send_recv(numnod, nod_comm, mom_nod%moms%f_z)
!
      call nod_scalar_send_recv(numnod, nod_comm, mom_nod%moms%f_x2)
      call nod_scalar_send_recv(numnod, nod_comm, mom_nod%moms%f_y2)
      call nod_scalar_send_recv(numnod, nod_comm, mom_nod%moms%f_z2)
!
      call nod_scalar_send_recv(numnod, nod_comm, mom_nod%moms%f_xy)
      call nod_scalar_send_recv(numnod, nod_comm, mom_nod%moms%f_yz)
      call nod_scalar_send_recv(numnod, nod_comm, mom_nod%moms%f_zx)
!
      end subroutine filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_filter_mom_nod_send_recv                          &
     &         (numnod, nod_comm, mom_nod)
!
      use t_filter_moments
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!
      call nod_vector_send_recv(numnod, nod_comm, mom_nod%diff%df_x)
      call nod_vector_send_recv(numnod, nod_comm, mom_nod%diff%df_y)
      call nod_vector_send_recv(numnod, nod_comm, mom_nod%diff%df_z)
!
      call nod_vector_send_recv(numnod, nod_comm, mom_nod%diff%df_x2)
      call nod_vector_send_recv(numnod, nod_comm, mom_nod%diff%df_y2)
      call nod_vector_send_recv(numnod, nod_comm, mom_nod%diff%df_z2)
!
      call nod_vector_send_recv(numnod, nod_comm, mom_nod%diff%df_xy)
      call nod_vector_send_recv(numnod, nod_comm, mom_nod%diff%df_yz)
      call nod_vector_send_recv(numnod, nod_comm, mom_nod%diff%df_zx)
!
      end subroutine diff_filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      end module filter_moments_send_recv
