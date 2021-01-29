!
!      module filter_moments_send_recv
!
!     Written by H. Matsui on Apr., 2008
!     Modified by H. Matsui on Apr., 2008
!
!!      subroutine jacobi_nod_send_recv                                 &
!!     &         (numnod, nod_comm, filter_dxi, vect)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(dxdxi_data_type), save :: filter_dxi
!!        type(vectors_4_solver), intent(inout) :: vect
!!      subroutine dxidx_nod_send_recv(numnod, nod_comm, dx_nod, vect)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(dxidx_direction_type), intent(inout) :: dx_nod
!!        type(vectors_4_solver), intent(inout) :: vect
!!
!!      subroutine elength_nod_send_recv                                &
!!     &         (numnod, nod_comm, elen_nod, vect)
!!      subroutine diff_elen_nod_send_recv                              &
!!     &         (numnod, nod_comm, elen_nod, vect)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(elen_nod_diffs_type), intent(inout) :: elen_nod
!!        type(vectors_4_solver), intent(inout) :: vect
!!
!!      subroutine filter_mom_nod_send_recv                             &
!!     &         (numnod, nod_comm, mom_nod, vect)
!!      subroutine diff_filter_mom_nod_send_recv                        &
!!     &         (numnod, nod_comm, mom_nod, vect)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod
!!        type(vectors_4_solver), intent(inout) :: vect
!
      module filter_moments_send_recv
!
      use m_precision
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
     &         (numnod, nod_comm, filter_dxi, vect)
!
      use t_filter_dxdxi
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(dxdxi_data_type), intent(inout) :: filter_dxi
      type(vectors_4_solver), intent(inout) :: vect
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dx%df_dxi, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dx%df_dei, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dx%df_dzi, vect)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dy%df_dxi, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dy%df_dei, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dy%df_dzi, vect)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dz%df_dxi, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dz%df_dei, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, filter_dxi%dxi_nod%dz%df_dzi, vect)
!
      end subroutine jacobi_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine dxidx_nod_send_recv(numnod, nod_comm, dx_nod, vect)
!
      use t_filter_dxdxi
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(dxidx_direction_type), intent(inout) :: dx_nod
      type(vectors_4_solver), intent(inout) :: vect
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dxi%df_dx, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dxi%df_dy, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dxi%df_dz, vect)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dei%df_dx, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dei%df_dy, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dei%df_dz, vect)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dzi%df_dx, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dzi%df_dy, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, dx_nod%dzi%df_dz, vect)
!
      end subroutine dxidx_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elength_nod_send_recv                                  &
     &         (numnod, nod_comm, elen_nod, vect)
!
      use t_filter_elength
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
      type(vectors_4_solver), intent(inout) :: vect
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_x2, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_y2, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_z2, vect)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_xy, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_yz, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%moms%f_zx, vect)
!
      end subroutine elength_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_elen_nod_send_recv                                &
     &         (numnod, nod_comm, elen_nod, vect)
!
      use t_filter_elength
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(elen_nod_diffs_type), intent(inout) :: elen_nod
      type(vectors_4_solver), intent(inout) :: vect
!
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_x2, vect)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_y2, vect)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_z2, vect)
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_xy, vect)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_yz, vect)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, elen_nod%diff%df_zx, vect)
!
      end subroutine diff_elen_nod_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine filter_mom_nod_send_recv                               &
     &         (numnod, nod_comm, mom_nod, vect)
!
      use t_filter_moments
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(vectors_4_solver), intent(inout) :: vect
!
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_x, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_y, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_z, vect)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_x2, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_y2, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_z2, vect)
!
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_xy, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_yz, vect)
      call nod_scalar_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%moms%f_zx, vect)
!
      end subroutine filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine diff_filter_mom_nod_send_recv                          &
     &         (numnod, nod_comm, mom_nod, vect)
!
      use t_filter_moments
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(vectors_4_solver), intent(inout) :: vect
!
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_x, vect)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_y, vect)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_z, vect)
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_x2, vect)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_y2, vect)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_z2, vect)
!
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_xy, vect)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_yz, vect)
      call nod_vector_send_recv                                         &
     &   (numnod, nod_comm, mom_nod%diff%df_zx, vect)
!
      end subroutine diff_filter_mom_nod_send_recv
!
! ----------------------------------------------------------------------
!
      end module filter_moments_send_recv
