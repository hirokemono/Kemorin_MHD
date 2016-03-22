!
!      module cal_3d_filter_phys_smp
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine cal_3d_filter_scalar_phys_smp                        &
!!     &         (flt_comm, nod_comm, node, filter_smp,                 &
!!     &          nnod_flt, num_filter_grp, id_filter_grp,              &
!!     &          i_field, i_filter, x_flt, nod_fld)
!!      subroutine cal_3d_filter_vector_phys_smp                        &
!!     &         (flt_comm, nod_comm, node, filter_smp,                 &
!!     &          nnod_flt, num_filter_grp, id_filter_grp,              &
!!     &          i_field, i_filter, x_flt, nod_fld)
!!      subroutine cal_3d_filter_tensor_phys_smp                        &
!!     &         (flt_comm, nod_comm, node, filter_smp,                 &
!!     &          nnod_flt, num_filter_grp, id_filter_grp,              &
!!     &          i_field, i_filter, x_flt, nod_fld)
!!         i_filter: field ID for filtered field
!!         i_field:  field ID to be filtered
!!         num_filter_grp:  num. of filtereing area
!!         id_filter_grp:   table id for filtering
!!
!!      subroutine cal_3d_ez_filter_scalar_smp(flt_comm, nod_comm, node,&
!!     &          filter_smp, nnod_flt, num_filter_grp, id_filter_grp,  &
!!     &          i_field, i_filter, x_flt, nod_fld)
!!      subroutine cal_3d_ez_filter_vector_smp(flt_comm, nod_comm, node,&
!!     &          filter_smp, nnod_flt, num_filter_grp, id_filter_grp,  &
!!     &          i_field, i_filter, x_flt, nod_fld)
!!      subroutine cal_3d_ez_filter_tensor_smp(flt_comm, nod_comm, node,&
!!     &          filter_smp, nnod_flt, num_filter_grp, id_filter_grp,  &
!!     &          i_field, i_filter, x_flt, nod_fld)
!
      module cal_3d_filter_phys_smp
!
      use m_precision
!
      use m_array_for_send_recv
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_filter_coefficients
      use prepare_field_2_filter
      use send_recv_3d_filtering
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_filter_scalar_phys_smp                          &
     &         (flt_comm, nod_comm, node, filter_smp,                   &
     &          nnod_flt, num_filter_grp, id_filter_grp,                &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter_smp
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: nnod_flt, i_field, i_filter
      real(kind = kreal), intent(inout) :: x_flt(nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call prepare_scalar_2_filter                                      &
     &   (flt_comm, node%numnod, node%internal_node,                    &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld, nnod_flt, x_flt)
!
      call sum_3d_filter_scalar_phys_smp(num_filter_grp, id_filter_grp, &
     &    filter_smp%ngrp_node, filter_smp%istack_node,                 &
     &    filter_smp%ntot_nod, filter_smp%inod_filter,                  &
     &    filter_smp%istack_near_nod, filter_smp%ntot_near_nod,         &
     &    filter_smp%inod_near, filter_smp%weight,                      &
     &    filter_smp%max_nsum, filter_smp%istack_nsum,                  &
     &    filter_smp%ntot_nsum, filter_smp%ist_nsum,                    &
     &    filter_smp%ied_nsum, nnod_flt, x_flt, node%numnod, x_vec(1))
!
      call scalar_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_filter_scalar_phys_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_filter_vector_phys_smp                          &
     &         (flt_comm, nod_comm, node, filter_smp,                   &
     &          nnod_flt, num_filter_grp, id_filter_grp,                &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter_smp
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: nnod_flt, i_field, i_filter
      real(kind = kreal), intent(inout) :: x_flt(3*nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call prepare_vector_2_filter                                      &
     &   (flt_comm, node%numnod, node%internal_node,                    &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld, nnod_flt, x_flt)
!
      call sum_3d_filter_vector_phys_smp(num_filter_grp, id_filter_grp, &
     &    filter_smp%ngrp_node, filter_smp%istack_node,                 &
     &    filter_smp%ntot_nod, filter_smp%inod_filter,                  &
     &    filter_smp%istack_near_nod, filter_smp%ntot_near_nod,         &
     &    filter_smp%inod_near, filter_smp%weight,                      &
     &    filter_smp%max_nsum, filter_smp%istack_nsum,                  &
     &    filter_smp%ntot_nsum, filter_smp%ist_nsum,                    &
     &    filter_smp%ied_nsum, nnod_flt, x_flt, node%numnod, x_vec(1))
!
      call vector_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_filter_vector_phys_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_filter_tensor_phys_smp                          &
     &         (flt_comm, nod_comm, node, filter_smp,                   &
     &          nnod_flt, num_filter_grp, id_filter_grp,                &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter_smp
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: nnod_flt, i_field, i_filter
      real(kind = kreal), intent(inout) :: x_flt(6*nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call prepare_sym_tensor_2_filter                                  &
     &   (flt_comm, node%numnod, node%internal_node,                    &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld, nnod_flt, x_flt)
!
      call sum_3d_filter_tensor_phys_smp(num_filter_grp, id_filter_grp, &
     &    filter_smp%ngrp_node, filter_smp%istack_node,                 &
     &    filter_smp%ntot_nod, filter_smp%inod_filter,                  &
     &    filter_smp%istack_near_nod, filter_smp%ntot_near_nod,         &
     &    filter_smp%inod_near, filter_smp%weight,                      &
     &    filter_smp%max_nsum, filter_smp%istack_nsum,                  &
     &    filter_smp%ntot_nsum, filter_smp%ist_nsum,                    &
     &    filter_smp%ied_nsum, nnod_flt, x_flt, node%numnod, x_vec(1))
!
      call tensor_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_filter_tensor_phys_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_3d_ez_filter_scalar_smp(flt_comm, nod_comm, node,  &
     &          filter_smp, nnod_flt, num_filter_grp, id_filter_grp,    &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_ez_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter_smp
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: nnod_flt, i_field, i_filter
      real(kind = kreal), intent(inout) :: x_flt(nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call prepare_scalar_2_filter                                      &
     &   (flt_comm, node%numnod, node%internal_node,                    &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld, nnod_flt, x_flt)
!
      call sum_3d_ez_filter_scalar_smp(num_filter_grp, id_filter_grp,   &
     &    filter_smp%ngrp_node, filter_smp%istack_node,                 &
     &    filter_smp%ntot_nod, filter_smp%inod_filter,                  &
     &    filter_smp%istack_near_nod, filter_smp%ntot_near_nod,         &
     &    filter_smp%inod_near, filter_smp%weight,                      &
     &    nnod_flt, x_flt, node%numnod, x_vec(1))

!
      call scalar_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_ez_filter_scalar_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_ez_filter_vector_smp(flt_comm, nod_comm, node,  &
     &          filter_smp, nnod_flt, num_filter_grp, id_filter_grp,    &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_ez_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter_smp
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: nnod_flt, i_field, i_filter
      real(kind = kreal), intent(inout) :: x_flt(3*nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call prepare_vector_2_filter                                      &
     &   (flt_comm, node%numnod, node%internal_node,                    &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld, nnod_flt, x_flt)
!
      call sum_3d_ez_filter_vector_smp(num_filter_grp, id_filter_grp,   &
     &    filter_smp%ngrp_node, filter_smp%istack_node,                 &
     &    filter_smp%ntot_nod, filter_smp%inod_filter,                  &
     &    filter_smp%istack_near_nod, filter_smp%ntot_near_nod,         &
     &    filter_smp%inod_near, filter_smp%weight,                      &
     &    nnod_flt, x_flt, node%numnod, x_vec(1))
!
      call vector_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_ez_filter_vector_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_ez_filter_tensor_smp(flt_comm, nod_comm, node,  &
     &          filter_smp, nnod_flt, num_filter_grp, id_filter_grp,    &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_ez_filter_phys_smp
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter_smp
      integer(kind = kint), intent(in) :: num_filter_grp
      integer(kind = kint), intent(in) :: id_filter_grp(num_filter_grp)
!
      integer(kind = kint), intent(in) :: nnod_flt, i_field, i_filter
      real(kind = kreal), intent(inout) :: x_flt(6*nnod_flt)
      type(phys_data), intent(inout) :: nod_fld
!
!
      call prepare_sym_tensor_2_filter                                  &
     &   (flt_comm, node%numnod, node%internal_node,                    &
     &    nod_fld%ntot_phys, i_field, nod_fld%d_fld, nnod_flt, x_flt)
!
      call sum_3d_ez_filter_tensor_smp(num_filter_grp, id_filter_grp,   &
     &    filter_smp%ngrp_node, filter_smp%istack_node,                 &
     &    filter_smp%ntot_nod, filter_smp%inod_filter,                  &
     &    filter_smp%istack_near_nod, filter_smp%ntot_near_nod,         &
     &    filter_smp%inod_near, filter_smp%weight,                      &
     &    nnod_flt, x_flt, node%numnod, x_vec(1))
!
      call tensor_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_ez_filter_tensor_smp
!
! ----------------------------------------------------------------------
!
      end module cal_3d_filter_phys_smp
