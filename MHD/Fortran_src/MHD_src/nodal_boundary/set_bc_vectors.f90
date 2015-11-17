!
!      module set_bc_vectors
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!!      subroutine set_bc_fixed_velo_id(node, nod_grp)
!!
!!      subroutine set_bc_fixed_vect_p_id(node, nod_grp)
!!      subroutine set_bc_fixed_magne_id(node, nod_grp)
!!      subroutine set_bc_fixed_current_id(node, nod_grp)
!
      module set_bc_vectors
!
      use m_precision
!
      use m_constants
      use m_boundary_condition_IDs
      use t_geometry_data
      use t_group_data
      use set_nod_bc_vector_id
!
      implicit none
!
      character(len=kchara) :: field_name(3)
      integer (kind = kint) :: l_r(3)
!
      private :: field_name, l_r
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_velo_id(node, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_velo
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: l_f(3), l_s(3)
!
!
      nod_bc1_v%vect_bc_name(1) = 'velocity_x'
      nod_bc1_v%vect_bc_name(2) = 'velocity_y'
      nod_bc1_v%vect_bc_name(3) = 'velocity_z'
!
      l_f(1:3) = 0
      call set_bc_fixed_vect_id                                         &
     &   (node, nod_grp, velo_nod, nod_bc1_v, l_f)
!
      l_s(1:3) = 0
      call set_bc_sgs_vect_id                                           &
     &   (node, nod_grp, velo_nod, sgs_bc1_v, l_s)
!
      l_r(1:3) = 0
      call set_bc_rotate_id                                             &
     &   (node, nod_grp, velo_nod%num_bc, velo_nod%bc_name,             &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    nod_bc1_rot%ibc, nod_bc1_rot%ibc2, nod_bc1_rot%num_bc_nod,    &
     &    nod_bc1_rot%ibc_id, nod_bc1_rot%bc_rot_apt,                   &
     &    iflag_bc_rot, l_r)
!
      end subroutine set_bc_fixed_velo_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_vect_p_id(node, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: l_f(3), l_s(3)
!
!
      nod_bc1_a%vect_bc_name(1) = 'vector_potential_x'
      nod_bc1_a%vect_bc_name(2) = 'vector_potential_y'
      nod_bc1_a%vect_bc_name(3) = 'vector_potential_z'
!
      l_f(1:3) = 0
      call set_bc_fixed_vect_id                                         &
     &   (node, nod_grp, a_potential_nod, nod_bc1_a, l_f)
!
      l_s(1:3) = 0
      call set_bc_sgs_vect_id                                           &
     &   (node, nod_grp, a_potential_nod, sgs_bc1_a, l_s)
!
      end subroutine set_bc_fixed_vect_p_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_magne_id(node, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: l_f(3), l_s(3)
!
!
      nod_bc1_b%vect_bc_name(1) = 'magnetic_x'
      nod_bc1_b%vect_bc_name(2) = 'magnetic_y'
      nod_bc1_b%vect_bc_name(3) = 'magnetic_z'
!
      l_f(1:3) = 0
      call set_bc_fixed_vect_id                                         &
     &   (node, nod_grp, magne_nod, nod_bc1_b, l_f)
!
      call set_sph_magne_id(node, nod_grp, magne_nod%num_bc,            &
     &    magne_nod%bc_name, magne_nod%ibc_type, l_f)
!
      l_s(1:3) = 0
      call set_bc_sgs_vect_id                                           &
     &   (node, nod_grp, magne_nod, sgs_bc1_b, l_s)
!
      end subroutine set_bc_fixed_magne_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_current_id(node, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: l_f(3)
!
!
      nod_bc1_j%vect_bc_name(1) = 'current_x'
      nod_bc1_j%vect_bc_name(2) = 'current_y'
      nod_bc1_j%vect_bc_name(3) = 'current_z'
!
      l_f(1:3) = 0
      call set_bc_fixed_vect_id                                         &
     &   (node, nod_grp, current_nod, nod_bc1_j, l_f)
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
