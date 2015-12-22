!
!      module set_ele_nod_bc_vectors
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!!      subroutine ele_nodal_bc_vector_whole(node, ele, vector_bc)
!!      subroutine ele_nodal_bc_scalar_whole(node, ele, scalar_bc)
!!      subroutine ele_nodal_bc_potential_whole(node, ele, scalar_bc)
!!
!!      subroutine ele_nodal_bc_vector_layer                            &
!!     &         (node, ele, layer, vector_bc)
!!      subroutine ele_nodal_bc_scalar_layer                            &
!!     &         (node, ele, layer, scalar_bc)
!!      subroutine ele_nodal_bc_potential_layer                         &
!!     &         (node, ele, layer, scalar_bc)
!!
!!      subroutine set_ele_nodal_bc_mag_p_layer(node, ele,              &
!!     &          layer, magp_bc, layer_bc)
!!        type(node_data), intent(inout) ::    node
!!        type(element_data), intent(inout) :: ele
!!        type(scaler_fixed_nod_bc_type), intent(inout) :: layer_bc
!!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!!
!!      subroutine set_ele_nodal_bc_4_rotate                            &
!!     &         (node, ele, layer, nod_bc_rot)
!
      module set_ele_nod_bc_vectors
!
      use m_precision
      use m_machine_parameter
!
      use m_geometry_constants
      use t_geometry_data
      use t_geometry_data_MHD
      use t_nodal_bc_data
      use count_num_nodal_fields
      use set_bc_element
      use set_bc_element_type
!
      implicit none
!
      private :: set_ele_nodal_bc_vector_l, set_ele_nodal_bc_scalar_l
      private :: ele_nodal_bc_potential_l
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine ele_nodal_bc_vector_whole(node, ele, vector_bc)
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
!
      call set_ele_nodal_bc_vector_l                                    &
     &   (node, ele, ione, ele%numele, vector_bc)
      call dealloc_vector_ibc_type(vector_bc)
!
      end subroutine ele_nodal_bc_vector_whole
!
!  ---------------------------------------------------------------------
!
      subroutine ele_nodal_bc_scalar_whole(node, ele, scalar_bc)
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      call set_ele_nodal_bc_scalar_l                                    &
     &   (node, ele, ione, ele%numele, scalar_bc)
      call dealloc_scalar_ibc_type(scalar_bc)
!
      end subroutine ele_nodal_bc_scalar_whole
!
!  ---------------------------------------------------------------------
!
      subroutine ele_nodal_bc_potential_whole(node, ele, scalar_bc)
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      call ele_nodal_bc_potential_l                                     &
     &   (node, ele, ione, ele%numele, scalar_bc)
!
      end subroutine ele_nodal_bc_potential_whole
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine ele_nodal_bc_vector_layer                              &
     &         (node, ele, layer, vector_bc)
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: layer
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
!
      call set_ele_nodal_bc_vector_l(node, ele,                         &
     &   layer%iele_start_fld, layer%iele_end_fld, vector_bc)
      call dealloc_vector_ibc_type(vector_bc)
!
      end subroutine ele_nodal_bc_vector_layer
!
!  ---------------------------------------------------------------------
!
      subroutine ele_nodal_bc_scalar_layer                              &
     &         (node, ele, layer, scalar_bc)
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: layer
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      call set_ele_nodal_bc_scalar_l(node, ele,                         &
     &    layer%iele_start_fld, layer%iele_end_fld, scalar_bc)
      call dealloc_scalar_ibc_type(scalar_bc)
!
      end subroutine ele_nodal_bc_scalar_layer
!
!  ---------------------------------------------------------------------
!
      subroutine ele_nodal_bc_potential_layer                           &
     &         (node, ele, layer, scalar_bc)
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: layer
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      call ele_nodal_bc_potential_l(node, ele,                          &
     &    layer%iele_start_fld, layer%iele_end_fld, scalar_bc)
      call dealloc_scalar_ibc_type(scalar_bc)
!
      end subroutine ele_nodal_bc_potential_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_vector_l                              &
     &         (node, ele, iele_st, iele_ed, vector_bc)
!
      integer (kind= kint), intent(in) :: iele_st, iele_ed
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      integer (kind= kint) :: nd
!
!
      do nd = 1, 3
        call count_bc_element_layer(node, ele, iele_st, iele_ed,        &
     &    vector_bc%num_idx_ibc(nd), vector_bc%ibc(1:node%numnod,nd))
        call count_bc_element_layer(node, ele, iele_st, iele_ed,        &
     &    vector_bc%num_idx_ibc2(nd), vector_bc%ibc2(1:node%numnod,nd))
      end do
!
      call cal_max_int_4_vector                                         &
     &   (vector_bc%nmax_idx_ibc, vector_bc%num_idx_ibc)
      call cal_max_int_4_vector                                         &
     &   (vector_bc%nmax_idx_ibc2, vector_bc%num_idx_ibc2)
!
      call alloc_nod_bc_vector_ele_type                                 &
     &   (np_smp, ele%nnod_4_ele, vector_bc)
!
      call set_vect_bc_ele_type_layer(ele%nnod_4_ele, node, ele,        &
     &    iele_st, iele_ed, vector_bc)
      call set_ele_4_vector_nodal_bc_type(np_smp, ele, vector_bc)
!
      end subroutine set_ele_nodal_bc_vector_l
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_scalar_l                              &
     &         (node, ele, iele_st, iele_ed, scalar_bc)
!
      integer (kind= kint), intent(in) :: iele_st, iele_ed
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      call count_bc_element_layer(node, ele, iele_st, iele_ed,          &
     &    scalar_bc%num_idx_ibc, scalar_bc%ibc)
      call count_bc_element_layer(node, ele, iele_st, iele_ed,          &
     &    scalar_bc%num_idx_ibc2, scalar_bc%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
      &  (np_smp, ele%nnod_4_ele, scalar_bc)
!
      call set_bc_ele_type_layer(ele%nnod_4_ele, node, ele,             &
     &    iele_st, iele_ed, scalar_bc)
      call set_ele_4_scalar_nodal_bc_type                               &
     &   (np_smp, ele%nnod_4_ele, scalar_bc)
!
      end subroutine set_ele_nodal_bc_scalar_l
!
!  ---------------------------------------------------------------------
!
      subroutine ele_nodal_bc_potential_l                               &
     &         (node, ele, iele_st, iele_ed, scalar_bc)
!
      integer (kind= kint), intent(in) :: iele_st, iele_ed
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      call count_bc_element_layer(node, ele, iele_st, iele_ed,          &
     &    scalar_bc%num_idx_ibc, scalar_bc%ibc)
      call count_bc_element_layer(node, ele, iele_st, iele_ed,          &
     &    scalar_bc%num_idx_ibc2, scalar_bc%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
      &  (np_smp, num_t_linear, scalar_bc)
!
      call set_bc_ele_type_layer(num_t_linear, node, ele,               &
     &    iele_st, iele_ed, scalar_bc)
      call set_ele_4_scalar_nodal_bc_type                               &
     &   (np_smp, num_t_linear, scalar_bc)
!
      end subroutine ele_nodal_bc_potential_l
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_mag_p_layer(node, ele,                &
     &          layer, magp_bc, layer_bc)
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: layer
      type(scaler_fixed_nod_bc_type), intent(in) :: magp_bc
      type(scaler_fixed_nod_bc_type), intent(inout) :: layer_bc
!
!
      call count_bc_element_layer                                       &
     &   (node, ele, layer%iele_start_fld, layer%iele_end_fld,          &
     &    layer_bc%num_idx_ibc, magp_bc%ibc)
      call count_bc_element_layer                                       &
     &   (node, ele, layer%iele_start_fld, layer%iele_end_fld,          &
     &    layer_bc%num_idx_ibc2, magp_bc%ibc2)
!
      call alloc_nod_bc_scalar_ele_type                                 &
      &  (np_smp, num_t_linear, layer_bc)
!
      call set_bc_element_layer                                         &
     &   (node, ele, layer%iele_start_fld, layer%iele_end_fld,          &
     &    layer_bc%num_idx_ibc, magp_bc%ibc,                            &
     &    layer_bc%ele_bc_id, layer_bc%nod_bc_id, num_t_linear)
      call set_bc_element_layer                                         &
     &   (node, ele, layer%iele_start_fld, layer%iele_end_fld,          &
     &    layer_bc%num_idx_ibc2, magp_bc%ibc2,                          &
     &    layer_bc%ele_bc2_id, layer_bc%nod_bc2_id, num_t_linear)
!
      call set_ele_4_scalar_nodal_bc_type                               &
     &   (np_smp, num_t_linear, layer_bc)
!
      end subroutine set_ele_nodal_bc_mag_p_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_4_rotate                              &
     &         (node, ele, layer, nod_bc_rot)
!
      use ordering_ele_4_fix_bd
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: layer
      type(scaler_rotaion_nod_bc_type), intent(inout) :: nod_bc_rot
!
!
      call count_bc_element_layer                                       &
     &   (node, ele, layer%iele_start_fld, layer%iele_end_fld,          &
     &    nod_bc_rot%num_idx_ibc, nod_bc_rot%ibc)
      call count_bc_element_layer                                       &
     &   (node, ele, layer%iele_start_fld, layer%iele_end_fld,          &
     &    nod_bc_rot%num_idx_ibc2, nod_bc_rot%ibc2)
!
      call alloc_nod_bc_rotate_ele_type                                 &
     &  (np_smp, ele%nnod_4_ele, nod_bc_rot)
!
      call set_bc_element_layer                                         &
     &   (node, ele, layer%iele_start_fld, layer%iele_end_fld,          &
     &    nod_bc_rot%num_idx_ibc, nod_bc_rot%ibc,                       &
     &    nod_bc_rot%ele_bc_id,  nod_bc_rot%nod_bc_id,  ele%nnod_4_ele)
      call set_bc_element_layer                                         &
     &   (node, ele, layer%iele_start_fld, layer%iele_end_fld,          &
     &    nod_bc_rot%num_idx_ibc2, nod_bc_rot%ibc2,                     &
     &    nod_bc_rot%ele_bc2_id, nod_bc_rot%nod_bc2_id, ele%nnod_4_ele)
!
      call reordering_ele_4_fix_bd(np_smp, nod_bc_rot%num_idx_ibc,      &
     &    nod_bc_rot%num_idx_ibc, nod_bc_rot%ele_bc_id,                 &
     &    nod_bc_rot%nod_bc_id, nod_bc_rot%ibc_end,                     &
     &    nod_bc_rot%ibc_shape, nod_bc_rot%ibc_stack,                   &
     &    nod_bc_rot%ibc_stack_smp, ele%nnod_4_ele)
!
      call dealloc_rotate_ibc_type(nod_bc_rot)
!
      end subroutine set_ele_nodal_bc_4_rotate
!
!  ---------------------------------------------------------------------
!
      end module set_ele_nod_bc_vectors
