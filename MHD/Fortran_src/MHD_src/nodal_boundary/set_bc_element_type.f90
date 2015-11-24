!
!     module set_bc_element_type
!
!      Written by H. Matsui and H.Okuda  on July 2001
!      Modified by H. Matsui on June, 2005
!      Modified by H. Matsui on Jan., 2009
!
!!      subroutine set_bc_ele_type_layer                                &
!!     &         (num_t, node, ele, iele_st, iele_ed, scaler_bc)
!!        integer (kind= kint), intent(in)    :: num_t
!!        integer (kind= kint), intent(in)    :: iele_st, iele_ed
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!!        type(scaler_rotaion_nod_bc_type), intent(inout) :: rotate
!!
!!      subroutine set_vect_bc_ele_type_layer(num_t, node, ele, layer,  &
!!     &          iele_st, iele_ed, vector_bc)
!!        integer (kind= kint), intent(in)    :: num_t
!!        integer (kind= kint), intent(in)    :: iele_st, iele_ed
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!!
!!      subroutine set_ele_4_vector_nodal_bc_type(np_smp, ele, vector_bc)
!!        integer(kind = kint), intent(in) :: np_smp
!!        type(element_data), intent(inout) :: ele
!!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!      subroutine set_ele_4_scalar_nodal_bc_type(np_smp, num_t,         &
!     &          scaler_bc)
!        integer (kind= kint), intent(in) :: np_smp, num_t
!        type(mesh_geometry),    intent(in) :: mesh
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!
      module set_bc_element_type
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_nodal_bc_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_ele_type_layer                                  &
     &         (num_t, node, ele, iele_st, iele_ed, scaler_bc)
!
      use set_bc_element
!
      integer (kind= kint), intent(in)    :: num_t
      integer (kind= kint), intent(in)    :: iele_st, iele_ed
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!
!
      call set_bc_element_layer(node, ele, iele_st, iele_ed,            &
     &    scaler_bc%num_idx_ibc, scaler_bc%ibc,                         &
     &    scaler_bc%ele_bc_id,  scaler_bc%nod_bc_id,  num_t)
!
      call set_bc_element_layer(node, ele, iele_st, iele_ed,            &
     &    scaler_bc%num_idx_ibc2, scaler_bc%ibc2,                       &
     &    scaler_bc%ele_bc2_id, scaler_bc%nod_bc2_id, num_t)
!
      end subroutine set_bc_ele_type_layer
!
!-----------------------------------------------------------------------
!
      subroutine set_vect_bc_ele_type_layer(num_t, node, ele,           &
     &          iele_st, iele_ed, vector_bc)
!
      use t_geometry_data_MHD
      use set_bc_element
!
      integer (kind= kint), intent(in)    :: num_t
      integer (kind= kint), intent(in)    :: iele_st, iele_ed
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      integer (kind= kint) :: nd
!
!
      do nd = 1, 3
        call set_bc_element_layer(node, ele, iele_st, iele_ed,          &
     &      vector_bc%num_idx_ibc(nd),                                  &
     &      vector_bc%ibc(1:node%numnod,nd),                            &
     &      vector_bc%ele_bc_id(1:vector_bc%num_idx_ibc(nd),nd),        &
     &      vector_bc%nod_bc_id(1:vector_bc%num_idx_ibc(nd),nd),        &
     &      num_t)
!
        call set_bc_element_layer(node, ele, iele_st, iele_ed,          &
     &      vector_bc%num_idx_ibc2(nd),                                 &
     &      vector_bc%ibc2(1:node%numnod,nd),                           &
     &      vector_bc%ele_bc2_id(1:vector_bc%num_idx_ibc2(nd),nd),      &
     &      vector_bc%nod_bc2_id(1:vector_bc%num_idx_ibc2(nd),nd),      &
     &      num_t)
      end do
!
      end subroutine set_vect_bc_ele_type_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_4_vector_nodal_bc_type(np_smp, ele, vector_bc)
!
      use ordering_ele_4_fix_bd
!
      integer(kind = kint), intent(in) :: np_smp
      type(element_data), intent(in) :: ele
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      integer(kind = kint) :: nd
!
      do nd = 1, 3
        call reordering_ele_4_fix_bd(np_smp,                            &
     &      vector_bc%nmax_idx_ibc, vector_bc%num_idx_ibc(nd),          &
     &      vector_bc%ele_bc_id(1:vector_bc%nmax_idx_ibc,nd),           &
     &      vector_bc%nod_bc_id(1:vector_bc%nmax_idx_ibc,nd),           &
     &      vector_bc%ibc_end(nd),                                      &
     &      vector_bc%ibc_shape(1:ele%nnod_4_ele,nd),                   &
     &      vector_bc%ibc_stack(0:ele%nnod_4_ele,nd),                   &
     &      vector_bc%ibc_stack_smp(0:ele%nnod_4_ele*np_smp,nd),        &
     &      ele%nnod_4_ele )
      end do
!
      end subroutine set_ele_4_vector_nodal_bc_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_4_scalar_nodal_bc_type(np_smp, num_t,          &
     &          scaler_bc)
!
      use ordering_ele_4_fix_bd
!
      integer (kind= kint), intent(in) :: np_smp, num_t
      type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!
!
      call reordering_ele_4_fix_bd(np_smp, scaler_bc%num_idx_ibc,       &
     &    scaler_bc%num_idx_ibc, scaler_bc%ele_bc_id,                   &
     &    scaler_bc%nod_bc_id, scaler_bc%ibc_end, scaler_bc%ibc_shape,  &
     &    scaler_bc%ibc_stack, scaler_bc%ibc_stack_smp, num_t)
!
      end subroutine set_ele_4_scalar_nodal_bc_type
!
!  ---------------------------------------------------------------------
!
      end module set_bc_element_type
