!
!      module cal_3d_filter_phys
!
!      Written by H. Matsui on Sep., 2007
!     Modified by H. Matsui on Nov., 2008
!
!!      subroutine cal_3d_filter_scalar_phys(flt_comm, nod_comm, node,  &
!!     &          filter, nnod_flt, num_filter_grp, id_filter_grp,      &
!!     &          i_field, i_filter, x_flt, nod_fld)
!!      subroutine cal_3d_filter_vector_phys(flt_comm, nod_comm, node,  &
!!     &          filter, nnod_flt, num_filter_grp, id_filter_grp,      &
!!     &          i_field, i_filter, x_flt, nod_fld)
!!      subroutine cal_3d_filter_tensor_phys(flt_comm, nod_comm, node,  &
!!     &          filter, nnod_flt, num_filter_grp, id_filter_grp,      &
!!     &          i_field, i_filter, x_flt, nod_fld)
!!         i_filter: field ID for filtered field
!!         i_field:  field ID to be filtered
!!         num_filter_grp:  num. of filtereing area
!!         id_filter_grp:   table id for filtering
!!
!!      subroutine cal_3d_ez_filter_scalar_phys(flt_comm, nod_comm,     &
!!     &          node, filter, nnod_flt, num_filter_grp, id_filter_grp,&
!!     &          i_field, i_filter, x_flt, nod_fld) 
!!      subroutine cal_3d_ez_filter_vector_phys(flt_comm, nod_comm,     &
!!     &          node, filter, nnod_flt, num_filter_grp, id_filter_grp,&
!!     &          i_field, i_filter, x_flt, nod_fld)
!!      subroutine cal_3d_ez_filter_tensor_phys(flt_comm, nod_comm,     &
!!     &          node, filter, nnod_flt, num_filter_grp, id_filter_grp,&
!!     &          i_field, i_filter, x_flt, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm, flt_comm
!
      module cal_3d_filter_phys
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_filter_coefficients
!
      use m_array_for_send_recv
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
      subroutine cal_3d_filter_scalar_phys(flt_comm, nod_comm, node,    &
     &          filter, nnod_flt, num_filter_grp, id_filter_grp,        &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_filter_phys
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter
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
      call sum_3d_filter_scalar_phys(num_filter_grp, id_filter_grp,     &
     &    filter%ngrp_node, filter%istack_node, filter%ntot_nod,        &
     &    filter%inod_filter, filter%istack_near_nod,                   &
     &    filter%ntot_near_nod, filter%inod_near, filter%weight,        &
     &    filter%istack_nsum, filter%max_nsum,                          &
     &    filter%ntot_nsum, filter%ist_nsum, filter%ied_nsum,           &
     &    nnod_flt, x_flt, node%numnod, x_vec(1))
!
      call scalar_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_filter_scalar_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_filter_vector_phys(flt_comm, nod_comm, node,    &
     &          filter, nnod_flt, num_filter_grp, id_filter_grp,        &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_filter_phys
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter
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
      call sum_3d_filter_vector_phys(num_filter_grp, id_filter_grp,     &
     &    filter%ngrp_node, filter%istack_node, filter%ntot_nod,        &
     &    filter%inod_filter, filter%istack_near_nod,                   &
     &    filter%ntot_near_nod, filter%inod_near, filter%weight,        &
     &    filter%istack_nsum, filter%max_nsum,                          &
     &    filter%ntot_nsum, filter%ist_nsum, filter%ied_nsum,           &
     &    nnod_flt, x_flt, node%numnod, x_vec(1))
!
      call vector_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_filter_vector_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_filter_tensor_phys(flt_comm, nod_comm, node,    &
     &          filter, nnod_flt, num_filter_grp, id_filter_grp,        &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_filter_phys
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter
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
      call sum_3d_filter_tensor_phys(num_filter_grp, id_filter_grp,     &
     &    filter%ngrp_node, filter%istack_node, filter%ntot_nod,        &
     &    filter%inod_filter, filter%istack_near_nod,                   &
     &    filter%ntot_near_nod, filter%inod_near, filter%weight,        &
     &    filter%istack_nsum, filter%max_nsum,                          &
     &    filter%ntot_nsum, filter%ist_nsum, filter%ied_nsum,           &
     &    nnod_flt, x_flt, node%numnod, x_vec(1))
!
      call tensor_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_filter_tensor_phys
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_3d_ez_filter_scalar_phys(flt_comm, nod_comm,       &
     &          node, filter, nnod_flt, num_filter_grp, id_filter_grp,  &
     &          i_field, i_filter, x_flt, nod_fld) 
!
      use sum_3d_ez_filter_phys
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter
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
     &   nod_fld%ntot_phys, i_field, nod_fld%d_fld, nnod_flt, x_flt)
!
      call sum_3d_ez_filter_scalar_phys(num_filter_grp, id_filter_grp,  &
     &    filter%ngrp_node, filter%istack_node, filter%ntot_nod,        &
     &    filter%inod_filter, filter%istack_near_nod,                   &
     &    filter%ntot_near_nod, filter%inod_near, filter%weight,        &
     &    nnod_flt, x_flt, node%numnod, x_vec(1) )
!
      call scalar_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_ez_filter_scalar_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_ez_filter_vector_phys(flt_comm, nod_comm,       &
     &          node, filter, nnod_flt, num_filter_grp, id_filter_grp,  &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_ez_filter_phys
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter
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
      call sum_3d_ez_filter_vector_phys(num_filter_grp, id_filter_grp,  &
     &    filter%ngrp_node, filter%istack_node, filter%ntot_nod,        &
     &    filter%inod_filter, filter%istack_near_nod,                   &
     &    filter%ntot_near_nod, filter%inod_near, filter%weight,        &
     &    nnod_flt, x_flt, node%numnod, x_vec(1))
!
      call vector_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_ez_filter_vector_phys
!
! ----------------------------------------------------------------------
!
      subroutine cal_3d_ez_filter_tensor_phys(flt_comm, nod_comm,       &
     &          node, filter, nnod_flt, num_filter_grp, id_filter_grp,  &
     &          i_field, i_filter, x_flt, nod_fld)
!
      use sum_3d_ez_filter_phys
!
      type(communication_table), intent(in) :: nod_comm, flt_comm
      type(node_data), intent(in) :: node
      type(filter_coefficients_type), intent(in) :: filter
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
      call sum_3d_ez_filter_tensor_phys(num_filter_grp, id_filter_grp,  &
     &    filter%ngrp_node, filter%istack_node, filter%ntot_nod,        &
     &    filter%inod_filter, filter%istack_near_nod,                   &
     &    filter%ntot_near_nod, filter%inod_near, filter%weight,        &
     &    nnod_flt, x_flt, node%numnod, x_vec(1))
!
      call tensor_send_recv_3d_filter(nod_comm, node%numnod, x_vec(1),  &
     &    nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      end subroutine cal_3d_ez_filter_tensor_phys
!
! ----------------------------------------------------------------------
!
      end module cal_3d_filter_phys
