!
!      module set_ele_nod_bc_vectors
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_ele_nodal_bc_4_velo
!      subroutine set_ele_nodal_bc_4_velo_sgs
!      subroutine set_ele_nodal_bc_4_vect_p
!      subroutine set_ele_nodal_bc_4_vecp_sgs
!      subroutine set_ele_nodal_bc_4_magne
!      subroutine set_ele_nodal_bc_4_mag_sgs
!      subroutine set_ele_nodal_bc_4_current
!
      module set_ele_nod_bc_vectors
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_nodal_bc_data
      use count_bc_element
      use count_num_nodal_fields
      use set_bc_element
      use set_ele_4_nodal_bc
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_vect_fl(node, ele, nod_bc_vect)
!
      type(node_data), intent(inout) ::    node
      type(element_data), intent(inout) :: ele
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_vect
!
!
      call count_bc_element_4_vect_fl   &
     &   (nod_bc_vect%num_idx_ibc, nod_bc_vect%ibc)
      call count_bc_element_4_vect_fl                                   &
     &   (nod_bc_vect%num_idx_ibc2, nod_bc_vect%ibc2)
!
      call cal_max_int_4_vector  &
     &   (nod_bc_vect%nmax_idx_ibc, nod_bc_vect%num_idx_ibc)
      call cal_max_int_4_vector  &
     &   (nod_bc_vect%nmax_idx_ibc2, nod_bc_vect%num_idx_ibc2)
!
      call alloc_nod_bc_vector_ele_type                                 &
     &   (np_smp, ele%nnod_4_ele, nod_bc_vect)
!
      call set_ele_4_vector_nodal_bc_fl(node%numnod, ele%nnod_4_ele,    &
     &    nod_bc_vect%ibc, nod_bc_vect%ibc2,                            &
     &    nod_bc_vect%nmax_idx_ibc, nod_bc_vect%num_idx_ibc,            &
     &    nod_bc_vect%ele_bc_id, nod_bc_vect%nod_bc_id,                 &
     &    nod_bc_vect%nmax_idx_ibc2, nod_bc_vect%ele_bc2_id,            &
     &    nod_bc_vect%nod_bc2_id, nod_bc_vect%ibc_end,                  &
     &    nod_bc_vect%ibc_shape, nod_bc_vect%ibc_stack,                 &
     &    nod_bc_vect%ibc_stack_smp)
!
      call dealloc_vector_ibc_type(nod_bc_vect)
!
      end subroutine set_ele_nodal_bc_4_vect_fl
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_vect(node, ele, nod_bc_vect)
!
      type(node_data), intent(inout) ::    node
      type(element_data), intent(inout) :: ele
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_vect
!
!
      call count_bc_element_4_vect                                      &
     &   (nod_bc_vect%num_idx_ibc, nod_bc_vect%ibc)
      call count_bc_element_4_vect                                      &
     &   (nod_bc_vect%num_idx_ibc2, nod_bc_vect%ibc2)
!
      call cal_max_int_4_vector                                         &
     &   (nod_bc_vect%nmax_idx_ibc, nod_bc_vect%num_idx_ibc)
      call cal_max_int_4_vector                                         &
     &   (nod_bc_vect%nmax_idx_ibc2, nod_bc_vect%num_idx_ibc2)
!
      call alloc_nod_bc_vector_ele_type                                 &
     &   (np_smp, ele%nnod_4_ele, nod_bc_vect)
!
      call set_ele_4_vector_nodal_bc(node%numnod, ele%nnod_4_ele,       &
     &    nod_bc_vect%ibc, nod_bc_vect%ibc2, nod_bc_vect%nmax_idx_ibc,  &
     &    nod_bc_vect%num_idx_ibc, nod_bc_vect%ele_bc_id,               &
     &    nod_bc_vect%nod_bc_id, nod_bc_vect%nmax_idx_ibc2,             &
     &    nod_bc_vect%ele_bc2_id, nod_bc_vect%nod_bc2_id,               &
     &    nod_bc_vect%ibc_end, nod_bc_vect%ibc_shape,                   &
     &    nod_bc_vect%ibc_stack, nod_bc_vect%ibc_stack_smp)
!
      call dealloc_vector_ibc_type(nod_bc_vect)
!
      end subroutine set_ele_nodal_bc_4_vect
!
!  ---------------------------------------------------------------------
!
      end module set_ele_nod_bc_vectors
