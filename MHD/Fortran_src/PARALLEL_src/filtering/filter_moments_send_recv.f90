!
!      module filter_moments_send_recv
!
!     Written by H. Matsui on Apr., 2008
!     Modified by H. Matsui on Apr., 2008
!
!!      subroutine jacobi_nod_send_recv                                 &
!!     &         (numnod, nod_comm, filter_dxi, v_sol)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(dxdxi_data_type), save :: filter_dxi
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!      subroutine dxidx_nod_send_recv(numnod, nod_comm, dx_nod, v_sol)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(dxidx_direction_type), intent(inout) :: dx_nod
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!
!!      subroutine elength_nod_send_recv                                &
!!     &         (numnod, nod_comm, elen_nod, v_sol)
!!      subroutine diff_elen_nod_send_recv                              &
!!     &         (numnod, nod_comm, elen_nod, v_sol)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(elen_nod_diffs_type), intent(inout) :: elen_nod
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!
!!      subroutine filter_mom_nod_send_recv                             &
!!     &         (numnod, nod_comm, mom_nod, v_sol)
!!      subroutine diff_filter_mom_nod_send_recv                        &
!!     &         (numnod, nod_comm, mom_nod, v_sol)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod
!!        type(vectors_4_solver), intent(inout) :: v_sol
!
      module filter_moments_send_recv
!
      use m_precision
      use m_solver_SR
      use t_comm_table
      use t_vector_for_solver
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
      subroutine jacobi_nod_send_recv                                   &
     &         (numnod, nod_comm, filter_dxi, v_sol)
!
      use t_filter_dxdxi
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(dxdxi_data_type), intent(inout) :: filter_dxi
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dx%df_dxi, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dx%df_dei, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dx%df_dzi, v_sol, SR_sig1, SR_r1)
!
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dy%df_dxi, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dy%df_dei, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dy%df_dzi, v_sol, SR_sig1, SR_r1)
!
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dz%df_dxi, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dz%df_dei, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dz%df_dzi, v_sol, SR_sig1, SR_r1)
!
      end subroutine jacobi_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine dxidx_nod_send_recv(numnod, nod_comm, dx_nod, v_sol)
!
      use t_filter_dxdxi
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(dxidx_direction_type), intent(inout) :: dx_nod
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dxi%df_dx, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dxi%df_dy, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dxi%df_dz, v_sol, SR_sig1, SR_r1)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dei%df_dx, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dei%df_dy, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dei%df_dz, v_sol, SR_sig1, SR_r1)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dzi%df_dx, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dzi%df_dy, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dzi%df_dz, v_sol, SR_sig1, SR_r1)
!
      end subroutine dxidx_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elength_nod_send_recv                                  &
     &         (numnod, nod_comm, elen_nod, v_sol)
!
      use t_filter_elength
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_x2, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_y2, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_z2, v_sol, SR_sig1, SR_r1)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_xy, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_yz, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_zx, v_sol, SR_sig1, SR_r1)
!
      end subroutine elength_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_elen_nod_send_recv                                &
     &         (numnod, nod_comm, elen_nod, v_sol)
!
      use t_filter_elength
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_x2, v_sol, SR_sig1, SR_r1)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_y2, v_sol, SR_sig1, SR_r1)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_z2, v_sol, SR_sig1, SR_r1)
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_xy, v_sol, SR_sig1, SR_r1)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_yz, v_sol, SR_sig1, SR_r1)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_zx, v_sol, SR_sig1, SR_r1)
!
      end subroutine diff_elen_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine filter_mom_nod_send_recv                               &
     &         (numnod, nod_comm, mom_nod, v_sol)
!
      use t_filter_moments
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_x, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_y, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_z, v_sol, SR_sig1, SR_r1)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_x2, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_y2, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_z2, v_sol, SR_sig1, SR_r1)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_xy, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_yz, v_sol, SR_sig1, SR_r1)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_zx, v_sol, SR_sig1, SR_r1)
!
      end subroutine filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_filter_mom_nod_send_recv                          &
     &         (numnod, nod_comm, mom_nod, v_sol)
!
      use t_filter_moments
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_x, v_sol, SR_sig1, SR_r1)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_y, v_sol, SR_sig1, SR_r1)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_z, v_sol, SR_sig1, SR_r1)
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_x2, v_sol, SR_sig1, SR_r1)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_y2, v_sol, SR_sig1, SR_r1)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_z2, v_sol, SR_sig1, SR_r1)
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_xy, v_sol, SR_sig1, SR_r1)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_yz, v_sol, SR_sig1, SR_r1)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_zx, v_sol, SR_sig1, SR_r1)
!
      end subroutine diff_filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      end module filter_moments_send_recv
