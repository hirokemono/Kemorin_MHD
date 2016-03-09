!
!      module set_bc_vectors
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!!      subroutine set_bc_fixed_velo_id(node, nod_grp, bc_list,         &
!!     &          nod_bc_v, sgs_bc_v, nod_bc_rot)
!!        type(node_data), intent(in) :: node
!!        type(group_data), intent(in) :: nod_grp
!!        type(nod_bc_list_type), intent(in) :: bc_list
!!        type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_v
!!        type(vect_fixed_nod_bc_type), intent(inout) :: sgs_bc_v
!!        type(scaler_rotaion_nod_bc_type), intent(inout) :: nod_bc_rot
!!
!!      subroutine set_bc_fixed_vect_p_id(node, nod_grp, bc_list,       &
!!     &          nod_bc_a, sgs_bc_a)
!!        type(node_data), intent(in) :: node
!!        type(group_data), intent(in) :: nod_grp
!!        type(nod_bc_list_type), intent(in) :: bc_list
!!        type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_a
!!        type(vect_fixed_nod_bc_type), intent(inout) :: sgs_bc_a
!!      subroutine set_bc_fixed_magne_id(node, nod_grp, bc_list,        &
!!     &          i_magne, nod_bc_b, sgs_bc_b)
!!        type(node_data), intent(in) :: node
!!        type(group_data), intent(in) :: nod_grp
!!        type(nod_bc_list_type), intent(in) :: bc_list
!!        type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_b
!!        type(vect_fixed_nod_bc_type), intent(inout) :: sgs_bc_b
!!      subroutine set_bc_fixed_current_id(node, nod_grp, bc_list,      &
!!     &          nod_bc_j)
!!        type(node_data), intent(in) :: node
!!        type(group_data), intent(in) :: nod_grp
!!        type(nod_bc_list_type), intent(in) :: bc_list
!!        type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_b
!!        type(vect_fixed_nod_bc_type), intent(inout) :: sgs_bc_b
!
      module set_bc_vectors
!
      use m_precision
!
      use m_constants
      use m_boundary_condition_IDs
      use t_geometry_data
      use t_group_data
      use t_nodal_bc_data
      use set_nod_bc_vector_id
!
      implicit none
!
      private :: set_bc_fixed_vect_id, set_bc_sgs_vect_id
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_velo_id(node, nod_grp, bc_list,           &
     &          nod_bc_v, sgs_bc_v, nod_bc_rot)
!
      use m_bc_data_list
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_v
      type(vect_fixed_nod_bc_type), intent(inout) :: sgs_bc_v
      type(scaler_rotaion_nod_bc_type), intent(inout) :: nod_bc_rot
!
      integer (kind = kint) :: l_f(3), l_s(3), l_r(3)
!
!
      l_f(1:3) = 0
      call set_bc_fixed_vect_id                                         &
     &   (node, nod_grp, bc_list, nod_bc_v, l_f)
!
      l_s(1:3) = 0
      call set_bc_sgs_vect_id                                           &
     &   (node, nod_grp, bc_list, sgs_bc_v, l_s)
!
      l_r(1:3) = 0
      call set_bc_rotate_id                                             &
     &   (node, nod_grp, bc_list%num_bc, bc_list%bc_name,               &
     &    bc_list%ibc_type, bc_list%bc_magnitude,                       &
     &    nod_bc_rot%ibc, nod_bc_rot%ibc2, nod_bc_rot%num_bc_nod,       &
     &    nod_bc_rot%ibc_id, nod_bc_rot%bc_rot_apt,                     &
     &    iflag_bc_rot, l_r)
!
      end subroutine set_bc_fixed_velo_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_vect_p_id(node, nod_grp, bc_list,         &
     &          nod_bc_a, sgs_bc_a)
!
      use m_bc_data_list
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_a
      type(vect_fixed_nod_bc_type), intent(inout) :: sgs_bc_a
!
      integer (kind = kint) :: l_f(3), l_s(3)
!
!
      l_f(1:3) = 0
      call set_bc_fixed_vect_id(node, nod_grp, bc_list, nod_bc_a, l_f)
!
      l_s(1:3) = 0
      call set_bc_sgs_vect_id(node, nod_grp, bc_list, sgs_bc_a, l_s)
!
      end subroutine set_bc_fixed_vect_p_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_magne_id(node, nod_grp, bc_list,          &
     &          nod_bc_b, sgs_bc_b)
!
      use m_bc_data_list
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_b
      type(vect_fixed_nod_bc_type), intent(inout) :: sgs_bc_b
!
!
      integer (kind = kint) :: l_f(3), l_s(3)
!
!
      l_f(1:3) = 0
      call set_bc_fixed_vect_id(node, nod_grp, bc_list, nod_bc_b, l_f)
!
      call set_sph_magne_id(node, nod_grp, bc_list%num_bc,              &
     &    bc_list%bc_name, bc_list%ibc_type, nod_bc_b, l_f)
!
      l_s(1:3) = 0
      call set_bc_sgs_vect_id(node, nod_grp, bc_list, sgs_bc_b, l_s)
!
      end subroutine set_bc_fixed_magne_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_current_id(node, nod_grp, bc_list,        &
     &          nod_bc_j)
!
      use m_bc_data_list
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(vect_fixed_nod_bc_type), intent(inout) :: nod_bc_j
!
      integer (kind = kint) :: l_f(3)
!
!
      l_f(1:3) = 0
      call set_bc_fixed_vect_id(node, nod_grp, bc_list, nod_bc_j, l_f)
!
      end subroutine set_bc_fixed_current_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_vect_id                                   &
     &         (node, nod_grp, bc_list, vector_bc, l_f)
!
      use t_nodal_bc_data
      use m_bc_data_list
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
      integer (kind = kint), intent(inout) :: l_f(3)
!
!
      call set_fixed_vector_id                                          &
     &   (node, nod_grp, bc_list%num_bc, bc_list%bc_name,               &
     &    bc_list%ibc_type, bc_list%bc_magnitude,                       &
     &    vector_bc%ibc, vector_bc%ibc2, vector_bc%nmax_bc,             &
     &    vector_bc%ibc_id, vector_bc%bc_apt, vector_bc%vect_bc_name,   &
     &    l_f)
!
      end subroutine set_bc_fixed_vect_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_sgs_vect_id                                     &
     &         (node, nod_grp, bc_list, vector_bc, l_s)
!
      use t_nodal_bc_data
      use m_bc_data_list
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
      integer (kind = kint), intent(inout) :: l_s(3)
!
!
      call set_bc_vector_id                                             &
     &   (node, nod_grp, bc_list%num_bc, bc_list%bc_name,               &
     &    bc_list%ibc_type, bc_list%bc_magnitude,                       &
     &    vector_bc%ibc, vector_bc%ibc2, vector_bc%nmax_bc,             &
     &    vector_bc%ibc_id, vector_bc%bc_apt, iflag_bc_sgs, l_s)
!
      end subroutine set_bc_sgs_vect_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_vectors
