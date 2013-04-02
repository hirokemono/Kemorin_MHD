!
!      module set_bc_vectors
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_bc_fixed_velo_id
!      subroutine set_bc_fixed_vect_p_id
!
!      subroutine set_bc_fixed_magne_id
!      subroutine set_bc_fixed_current_id
!
      module set_bc_vectors
!
      use m_precision
!
      use m_constants
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
      subroutine set_bc_fixed_velo_id
!
      use m_bc_data_list
      use m_bc_data_velo
      use m_bc_velo_sgs
      use m_bc_data_rotate
!
!
      field_name(1) = 'velocity_x'
      field_name(2) = 'velocity_y'
      field_name(3) = 'velocity_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_id(num_bc_v, bc_v_name, ibc_v_type,         &
     &    bc_v_magnitude, ibc_velo, ibc2_velo, nmax_bc_v_nod,           &
     &    ibc_v_id, bc_v_id_apt, field_name, l_f)
!
      l_s(1:3) = 0
      call set_bc_vector_id(num_bc_v, bc_v_name, ibc_v_type,            &
     &    bc_v_magnitude, ibc_v_sgs, ibc2_v_sgs, nmax_bc_v_sgs_nod,     &
     &    ibc_v_sgs_id, bc_v_sgs_apt, iflag_bc_sgs, l_s)
!
      l_r(1:3) = 0
      call set_bc_rotate_id(num_bc_v, bc_v_name, ibc_v_type,            &
     &    bc_v_magnitude, ibc_velo_rot, ibc2_velo_rot, num_bc_v10_nod,  &
     &          ibc_v10_id, bc_v10_id_apt, iflag_bc_rot, l_r)
!
      end subroutine set_bc_fixed_velo_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_vect_p_id
!
      use m_bc_data_list
      use m_bc_data_vect_p
      use m_bc_vecp_sgs
!
!
      field_name(1) = 'vector_potential_x'
      field_name(2) = 'vector_potential_y'
      field_name(3) = 'vector_potential_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_id(num_bc_vp, bc_vp_name, ibc_vp_type,      &
     &    bc_vp_magnitude, ibc_vp, ibc2_vp, nmax_bc_vp_nod,             &
     &    ibc_vp_id, bc_vp_id_apt, field_name, l_f)
!
      l_s(1:3) = 0
      call set_bc_vector_id(num_bc_vp, bc_vp_name, ibc_vp_type,         &
     &    bc_vp_magnitude, ibc_a_sgs, ibc2_a_sgs, nmax_bc_a_sgs_nod,    &
     &    ibc_a_sgs_id, bc_a_sgs_id_apt, iflag_bc_sgs, l_s)
!
      end subroutine set_bc_fixed_vect_p_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_magne_id
!
      use m_bc_data_list
      use m_bc_data_magne
      use m_bc_magne_sgs
!
!
      field_name(1) = 'magnetic_x'
      field_name(2) = 'magnetic_y'
      field_name(3) = 'magnetic_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_id(num_bc_b, bc_b_name, ibc_b_type,         &
     &    bc_b_magnitude, ibc_magne, ibc2_magne, nmax_bc_b_nod,         &
     &    ibc_b_id, bc_b_id_apt, field_name, l_f)
!
      call set_sph_magne_id(num_bc_b, bc_b_name, ibc_b_type, l_f)
!
      l_s(1:3) = 0
      call set_bc_vector_id(num_bc_b, bc_b_name, ibc_b_type,            &
     &    bc_b_magnitude, ibc_b_sgs, ibc2_b_sgs, nmax_bc_b_sgs_nod,     &
     &    ibc_b_sgs_id, bc_b_sgs_id_apt, iflag_bc_sgs, l_s)
!
      end subroutine set_bc_fixed_magne_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_current_id
!
      use m_bc_data_list
      use m_bc_data_current
!
!
      field_name(1) = 'current_x'
      field_name(2) = 'current_y'
      field_name(3) = 'current_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_id(num_bc_j, bc_j_name, ibc_j_type,         &
     &    bc_j_magnitude, ibc_j, ibc2_j, nmax_bc_j_nod,                 &
     &    ibc_j_id, bc_j_id_apt, field_name, l_f)
!
      end subroutine set_bc_fixed_current_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_vectors
