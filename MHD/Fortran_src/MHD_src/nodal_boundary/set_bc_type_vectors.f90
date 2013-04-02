!
!      module set_bc_type_vectors
!
!      Written by H. Matsui and H. Okuda
!      Modified by H. Matsui on Oct., 2005
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine set_bc_fixed_velo_type_id(nod_grp, nodal_bc)
!      subroutine set_bc_fixed_vect_p_type_id(nod_grp, nodal_bc)
!      subroutine set_bc_fixed_magne_type_id(nod_grp, nodal_bc)
!      subroutine set_bc_fixed_current_type_id(nod_grp, nodal_bc)
!        type(group_data),          intent(in) :: nod_grp
!        type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      module set_bc_type_vectors
!
      use m_precision
!
      use m_constants
      use m_boundary_field_IO
      use t_group_data
      use t_nodal_bc_data
      use m_bc_data_list
      use set_bc_vector_type_id
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_velo_type_id(nod_grp, nodal_bc)
!
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      integer (kind = kint) :: l_f(3), l_s(3), l_r(3)
      character(len=kchara) :: field_name(3)
!
!
      field_name(1) = 'velocity_x'
      field_name(2) = 'velocity_y'
      field_name(3) = 'velocity_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_type_id(num_bc_v, bc_v_name, ibc_v_type,    &
     &    bc_v_magnitude, nod_grp, nodal_bc%velocity, field_name, l_f)
!
      l_s(1:3) = 0
      call s_set_bc_vector_type_id(num_bc_v, bc_v_name, ibc_v_type,     &
     &    bc_v_magnitude, nod_grp, nodal_bc%sgs_velo, iflag_bc_sgs, l_s)
!
      l_r(1:3) = 0
      call set_bc_rotate_type_id(num_bc_v, bc_v_name, ibc_v_type,       &
     &    bc_v_magnitude, nod_grp, nodal_bc%rotation, iflag_bc_rot, l_r)
!
      end subroutine set_bc_fixed_velo_type_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_vect_p_type_id(nod_grp, nodal_bc)
!
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      integer (kind = kint) :: l_f(3), l_s(3)
      character(len=kchara) :: field_name(3)
!
!
      field_name(1) = 'vector_potential_x'
      field_name(2) = 'vector_potential_y'
      field_name(3) = 'vector_potential_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_type_id(num_bc_vp, bc_vp_name, ibc_vp_type, &
     &    bc_vp_magnitude, nod_grp, nodal_bc%vector_p, field_name, l_f)
!
      l_s(1:3) = 0
      call s_set_bc_vector_type_id(num_bc_vp, bc_vp_name, ibc_vp_type,  &
     &    bc_vp_magnitude, nod_grp, nodal_bc%sgs_vect_p,                &
     &    iflag_bc_sgs, l_s)
!
      end subroutine set_bc_fixed_vect_p_type_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_magne_type_id(nod_grp, nodal_bc)
!
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      integer (kind = kint) :: l_f(3), l_s(3)
      character(len=kchara) :: field_name(3)
!
!
      field_name(1) = 'magnetic_x'
      field_name(2) = 'magnetic_y'
      field_name(3) = 'magnetic_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_type_id(num_bc_b, bc_b_name, ibc_b_type,    &
     &    bc_b_magnitude, nod_grp, nodal_bc%magne, field_name, l_f)
!
      call set_sph_magne_type_id(num_bc_b, bc_b_name, ibc_b_type,       &
     &    nod_grp, nodal_bc%magne, l_f)
!
      l_s(1:3) = 0
      call s_set_bc_vector_type_id(num_bc_b, bc_b_name, ibc_b_type,     &
     &    bc_b_magnitude, nod_grp, nodal_bc%sgs_magne,                  &
     &    iflag_bc_sgs, l_s)
!
      end subroutine set_bc_fixed_magne_type_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_current_type_id(nod_grp, nodal_bc)
!
      type(group_data),          intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      integer (kind = kint) :: l_f(3)
      character(len=kchara) :: field_name(3)
!
!
      field_name(1) = 'current_x'
      field_name(2) = 'current_y'
      field_name(3) = 'current_z'
!
      l_f(1:3) = 0
      call set_fixed_vector_type_id(num_bc_j, bc_j_name, ibc_j_type,    &
     &    bc_j_magnitude, nod_grp, nodal_bc%current, field_name, l_f)
!
      end subroutine set_bc_fixed_current_type_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_type_vectors
