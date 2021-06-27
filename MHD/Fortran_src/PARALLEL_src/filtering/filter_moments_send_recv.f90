!>@file   filter_moments_send_recv.f90
!!@brief  module filter_moments_send_recv
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2008
!!
!>@brief Filtering for filter moments
!!
!!@verbatim
!!      subroutine jacobi_nod_send_recv                                 &
!!     &         (numnod, nod_comm, filter_dxi, v_sol, SR_sig, SR_r)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(dxdxi_data_type), save :: filter_dxi
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine dxidx_nod_send_recv                                  &
!!     &         (numnod, nod_comm, dx_nod, v_sol, SR_sig, SR_r)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(dxidx_direction_type), intent(inout) :: dx_nod
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine elength_nod_send_recv                                &
!!     &         (numnod, nod_comm, elen_nod, v_sol, SR_sig, SR_r)
!!      subroutine diff_elen_nod_send_recv                              &
!!     &         (numnod, nod_comm, elen_nod, v_sol, SR_sig, SR_r)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(elen_nod_diffs_type), intent(inout) :: elen_nod
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine filter_mom_nod_send_recv                             &
!!     &         (numnod, nod_comm, mom_nod, v_sol, SR_sig, SR_r)
!!      subroutine diff_filter_mom_nod_send_recv                        &
!!     &         (numnod, nod_comm, mom_nod, v_sol, SR_sig, SR_r)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module filter_moments_send_recv
!
      use m_precision
      use t_comm_table
      use t_vector_for_solver
      use t_solver_SR
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
     &         (numnod, nod_comm, filter_dxi, v_sol, SR_sig, SR_r)
!
      use t_filter_dxdxi
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(dxdxi_data_type), intent(inout) :: filter_dxi
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dx%df_dxi, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dx%df_dei, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dx%df_dzi, v_sol, SR_sig, SR_r)
!
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dy%df_dxi, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dy%df_dei, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dy%df_dzi, v_sol, SR_sig, SR_r)
!
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dz%df_dxi, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dz%df_dei, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv(numnod, nod_comm,                       &
     &    filter_dxi%dxi_nod%dz%df_dzi, v_sol, SR_sig, SR_r)
!
      end subroutine jacobi_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine dxidx_nod_send_recv                                    &
     &         (numnod, nod_comm, dx_nod, v_sol, SR_sig, SR_r)
!
      use t_filter_dxdxi
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(dxidx_direction_type), intent(inout) :: dx_nod
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dxi%df_dx, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dxi%df_dy, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dxi%df_dz, v_sol, SR_sig, SR_r)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dei%df_dx, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dei%df_dy, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dei%df_dz, v_sol, SR_sig, SR_r)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dzi%df_dx, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dzi%df_dy, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dzi%df_dz, v_sol, SR_sig, SR_r)
!
      end subroutine dxidx_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elength_nod_send_recv                                  &
     &         (numnod, nod_comm, elen_nod, v_sol, SR_sig, SR_r)
!
      use t_filter_elength
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_x2, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_y2, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_z2, v_sol, SR_sig, SR_r)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_xy, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_yz, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_zx, v_sol, SR_sig, SR_r)
!
      end subroutine elength_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_elen_nod_send_recv                                &
     &         (numnod, nod_comm, elen_nod, v_sol, SR_sig, SR_r)
!
      use t_filter_elength
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_x2, v_sol, SR_sig, SR_r)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_y2, v_sol, SR_sig, SR_r)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_z2, v_sol, SR_sig, SR_r)
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_xy, v_sol, SR_sig, SR_r)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_yz, v_sol, SR_sig, SR_r)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_zx, v_sol, SR_sig, SR_r)
!
      end subroutine diff_elen_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine filter_mom_nod_send_recv                               &
     &         (numnod, nod_comm, mom_nod, v_sol, SR_sig, SR_r)
!
      use t_filter_moments
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_x, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_y, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_z, v_sol, SR_sig, SR_r)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_x2, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_y2, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_z2, v_sol, SR_sig, SR_r)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_xy, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_yz, v_sol, SR_sig, SR_r)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_zx, v_sol, SR_sig, SR_r)
!
      end subroutine filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_filter_mom_nod_send_recv                          &
     &         (numnod, nod_comm, mom_nod, v_sol, SR_sig, SR_r)
!
      use t_filter_moments
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_x, v_sol, SR_sig, SR_r)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_y, v_sol, SR_sig, SR_r)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_z, v_sol, SR_sig, SR_r)
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_x2, v_sol, SR_sig, SR_r)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_y2, v_sol, SR_sig, SR_r)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_z2, v_sol, SR_sig, SR_r)
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_xy, v_sol, SR_sig, SR_r)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_yz, v_sol, SR_sig, SR_r)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_zx, v_sol, SR_sig, SR_r)
!
      end subroutine diff_filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      end module filter_moments_send_recv
