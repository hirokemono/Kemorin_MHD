!
!      module set_bc_vectors
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!!      subroutine set_bc_fixed_velo_id(nod_grp)
!!      subroutine set_bc_fixed_vect_p_id(nod_grp)
!!
!!      subroutine set_bc_fixed_magne_id(nod_grp)
!!      subroutine set_bc_fixed_current_id(nod_grp)
!
      module set_bc_vectors
!
      use m_precision
!
      use m_constants
      use m_boundary_condition_IDs
      use m_geometry_data
      use t_group_data
      use set_nod_bc_vector_id
!
      implicit none
!
      character(len=kchara) :: field_name(3)
      integer (kind = kint) :: l_f(3), l_s(3), l_r(3)
!
      private :: field_name, l_f, l_s, l_r
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_velo_id(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_velo
!
      type(group_data), intent(in) :: nod_grp
!
!
      field_name(1) = 'velocity_x'
      field_name(2) = 'velocity_y'
      field_name(3) = 'velocity_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_id                                          &
     &   (node1%numnod, nod_grp, velo_nod%num_bc, velo_nod%bc_name,     &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    nod_bc1_v%ibc, nod_bc1_v%ibc2, nod_bc1_v%nmax_bc,             &
     &    nod_bc1_v%ibc_id, bc_v_id_apt, field_name, l_f)
!
      l_s(1:3) = 0
      call set_bc_vector_id                                             &
     &   (node1%numnod, nod_grp, velo_nod%num_bc, velo_nod%bc_name,     &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    sgs_bc1_v%ibc, sgs_bc1_v%ibc2, sgs_bc1_v%nmax_bc,             &
     &    sgs_bc1_v%ibc_id, bc_v_sgs_apt, iflag_bc_sgs, l_s)
!
      l_r(1:3) = 0
      call set_bc_rotate_id                                             &
     &   (node1%numnod, nod_grp, velo_nod%num_bc, velo_nod%bc_name,     &
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
      subroutine set_bc_fixed_vect_p_id(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne
!
      type(group_data), intent(in) :: nod_grp
!
!
      field_name(1) = 'vector_potential_x'
      field_name(2) = 'vector_potential_y'
      field_name(3) = 'vector_potential_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_id                                          &
     &   (node1%numnod, nod_grp, a_potential_nod%num_bc,                &
     &    a_potential_nod%bc_name, a_potential_nod%ibc_type,            &
     &    a_potential_nod%bc_magnitude, nod_bc1_a%ibc, nod_bc1_a%ibc2,  &
     &    nod_bc1_a%nmax_bc, nod_bc1_a%ibc_id, bc_vp_id_apt,            &
     &    field_name, l_f)
!
      l_s(1:3) = 0
      call set_bc_vector_id                                             &
     &   (node1%numnod, nod_grp, a_potential_nod%num_bc,                &
     &    a_potential_nod%bc_name, a_potential_nod%ibc_type,            &
     &    a_potential_nod%bc_magnitude, sgs_bc1_a%ibc, sgs_bc1_a%ibc2,  &
     &    sgs_bc1_a%nmax_bc, sgs_bc1_a%ibc_id, bc_a_sgs_id_apt,         &
     &    iflag_bc_sgs, l_s)
!
      end subroutine set_bc_fixed_vect_p_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_magne_id(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne
!
      type(group_data), intent(in) :: nod_grp
!
!
      field_name(1) = 'magnetic_x'
      field_name(2) = 'magnetic_y'
      field_name(3) = 'magnetic_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_id                                          &
     &   (node1%numnod, nod_grp, magne_nod%num_bc, magne_nod%bc_name,   &
     &    magne_nod%ibc_type, magne_nod%bc_magnitude,                   &
     &    nod_bc1_b%ibc, nod_bc1_b%ibc2, nod_bc1_b%nmax_bc,             &
     &    nod_bc1_b%ibc_id, bc_b_id_apt, field_name, l_f)
!
      call set_sph_magne_id(node1, nod_grp, magne_nod%num_bc,           &
     &    magne_nod%bc_name, magne_nod%ibc_type, l_f)
!
      l_s(1:3) = 0
      call set_bc_vector_id                                             &
     &   (node1%numnod, nod_grp, magne_nod%num_bc, magne_nod%bc_name,   &
     &    magne_nod%ibc_type, magne_nod%bc_magnitude,                   &
     &    sgs_bc1_b%ibc, sgs_bc1_b%ibc2, sgs_bc1_b%nmax_bc,             &
     &    sgs_bc1_b%ibc_id, bc_b_sgs_id_apt, iflag_bc_sgs, l_s)
!
      end subroutine set_bc_fixed_magne_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_current_id(nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne
!
      type(group_data), intent(in) :: nod_grp
!
!
      field_name(1) = 'current_x'
      field_name(2) = 'current_y'
      field_name(3) = 'current_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_id(node1%numnod,                            &
     &    nod_grp, current_nod%num_bc, current_nod%bc_name,             &
     &    current_nod%ibc_type, current_nod%bc_magnitude,               &
     &    nod_bc1_j%ibc, nod_bc1_j%ibc2, nod_bc1_j%nmax_bc,             &
     &    nod_bc1_j%ibc_id, bc_j_id_apt, field_name, l_f)
!
      end subroutine set_bc_fixed_current_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_vectors
