!
!      module set_bc_type_scalars
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine set_bc_type_velo_4_sphere_id(nod_grp, nodal_bc)
!        type(group_data), intent(in) :: nod_grp
!        type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
!      subroutine set_bc_type_fixed_temp_id(nod_grp, nodal_bc)
!      subroutine set_bc_type_fixed_press_id(nod_grp, nodal_bc)
!      subroutine set_bc_type_fixed_mag_p_id(nod_grp, nodal_bc)
!      subroutine set_bc_type_fixed_composit_id(nod_grp, nodal_bc)
!        type(group_data), intent(in) :: nod_grp
!        type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      module set_bc_type_scalars
!
      use m_precision
!
      use m_constants
      use m_phys_labels
      use m_bc_data_list
      use t_group_data
      use t_nodal_bc_data
      use set_bc_scalar_type_id
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_type_velo_4_sphere_id(nod_grp, nodal_bc)
!
      type(group_data), intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      integer (kind = kint) :: l_fr, l_r0, l_vsp
!
!
      l_fr = 0
      call s_set_bc_scalar_type_id(num_bc_v, bc_v_name, ibc_v_type,     &
     &    bc_v_magnitude, nod_grp, nodal_bc%free_plane,                 &
     &   iflag_free_sph, l_fr)
!
      l_r0 = 0
      call s_set_bc_scalar_type_id(num_bc_v, bc_v_name, ibc_v_type,     &
     &    bc_v_magnitude, nod_grp, nodal_bc%no_radial_v,                &
     &    iflag_no_vr, l_r0)
!
      l_vsp = 0
      call s_set_bc_scalar_type_id(num_bc_v, bc_v_name, ibc_v_type,     &
     &    bc_v_magnitude, nod_grp, nodal_bc%free_sphere,                &
     &    iflag_bc_special, l_vsp)
!
      end subroutine set_bc_type_velo_4_sphere_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_type_fixed_temp_id(nod_grp, nodal_bc)
!
      use m_control_parameter
      use m_boundary_field_IO
      use set_nodal_bc_type
!
      type(group_data), intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      integer (kind = kint) :: ii, i0
!
      ii = 0
      call set_fixed_bc_scalar_type_id(num_bc_e, bc_e_name, ibc_e_type, &
     &    bc_e_magnitude, fhd_temp, nod_grp, nodal_bc%temp, ii)
!
      i0 = 0
      call s_set_bc_scalar_type_id(num_bc_e, bc_e_name, ibc_e_type,     &
     &    bc_e_magnitude, nod_grp, nodal_bc%sgs_temp,                   &
     &    iflag_bc_sgs_s, i0)
!
!
      end subroutine set_bc_type_fixed_temp_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_type_fixed_press_id(nod_grp, nodal_bc)
!
      use m_boundary_field_IO
      use set_nodal_bc_type
!
      type(group_data), intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      integer (kind = kint) :: ii, i0
!
!
      ii = 0
      call set_fixed_bc_scalar_type_id(num_bc_p, bc_p_name, ibc_p_type, &
     &    bc_p_magnitude, fhd_press, nod_grp, nodal_bc%press, ii)
!
      i0 = 0
      call s_set_bc_scalar_type_id(num_bc_p, bc_p_name, ibc_p_type,     &
     &    bc_p_magnitude, nod_grp, nodal_bc%sgs_press,                  &
     &    iflag_bc_sgs_s, i0)
!
!      call set_potential_4_fix_press_type(nodal_bc%press)
!      call set_potential_4_sgs_press_type(nodal_bc%sgs_press)
!
      end subroutine set_bc_type_fixed_press_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_type_fixed_mag_p_id(nod_grp, nodal_bc)
!
      use m_boundary_field_IO
!
      type(group_data), intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      integer (kind = kint) :: ii, i0
!
!
      ii = 0
      call set_fixed_bc_scalar_type_id(num_bc_mag_p, bc_mag_p_name,     &
     &    ibc_mag_p_type, bc_mag_p_magnitude, fhd_mag_potential,        &
     &    nod_grp, nodal_bc%magne_p, ii)
!
      i0 = 0
      call s_set_bc_scalar_type_id(num_bc_mag_p, bc_mag_p_name,         &
     &    ibc_mag_p_type, bc_mag_p_magnitude, nod_grp,                  &
     &    nodal_bc%sgs_mag_p, iflag_bc_sgs_s, i0)
!
      call set_bc_sph_magp_type_id(num_bc_mag_p, bc_mag_p_name,         &
     &    ibc_mag_p_type, nod_grp, nodal_bc%magne_p, ii)
!
      end subroutine set_bc_type_fixed_mag_p_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_type_fixed_composit_id(nod_grp, nodal_bc)
!
      use m_boundary_field_IO
!
      type(group_data), intent(in) :: nod_grp
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      integer (kind = kint) :: ii
!
!
      ii = 0
      call set_fixed_bc_scalar_type_id(num_bc_composit,                 &
     &    bc_composit_name, ibc_composit_type,  bc_composit_magnitude,  &
     &    fhd_light, nod_grp, nodal_bc%composition, ii)
!
      end subroutine set_bc_type_fixed_composit_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_type_scalars
