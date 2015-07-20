!set_bc_scalars.f90
!      module set_bc_scalars
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_bc_velo_4_sphere_id(numnod, nod_grp)
!      subroutine set_bc_fixed_temp_id(numnod, nod_grp)
!      subroutine set_bc_fixed_press_id(numnod, nod_grp)
!      subroutine set_bc_fixed_m_potential_id(numnod, nod_grp)
!      subroutine set_bc_fixed_composition_id(numnod, nod_grp)
!
      module set_bc_scalars
!
      use m_precision
!
      use m_constants
      use m_phys_labels
      use m_boundary_condition_IDs
      use t_group_data
      use set_nod_bc_scalar_id
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_velo_4_sphere_id(numnod, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_vfree
      use m_bc_data_vr0
      use m_bc_data_vsp
!
      integer (kind=kint), intent(in) :: numnod
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: l_fr, l_r0, l_vsp
!
!
      l_fr = 0
      call set_bc_scalar_id                                             &
     &   (nod_grp, numnod, velo_nod%num_bc, velo_nod%bc_name,           &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    ibc_velo_fr, ibc2_velo_fr, num_bc_fr_nod,                     &
     &    ibc_fr_id, bc_fr_id_apt, iflag_free_sph, l_fr)
!
      l_r0 = 0
      call set_bc_scalar_id                                             &
     &   (nod_grp, numnod, velo_nod%num_bc, velo_nod%bc_name,           &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    ibc_velo_r0, ibc2_velo_r0, num_bc_vr0_nod,                    &
     &    ibc_vr0_id, bc_vr0_id_apt, iflag_no_vr, l_r0)
!
      l_vsp = 0
      call set_bc_scalar_id                                             &
     &   (nod_grp, numnod, velo_nod%num_bc, velo_nod%bc_name,           &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    ibc_velo_vsp, ibc2_velo_vsp, num_bc_vsp_nod,                  &
     &    ibc_vsp_id, bc_vsp_id_apt, iflag_bc_special, l_vsp)
!
      end subroutine set_bc_velo_4_sphere_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_temp_id(numnod, nod_grp)
!
      use m_control_parameter
      use m_bc_data_list
      use m_bc_data_ene
      use m_bc_temp_sgs
      use set_nodal_boundary
!
      integer (kind=kint), intent(in) :: numnod
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: ii, i0
!
!
      ii = 0
      call set_fixed_bc_scalar_id                                       &
     &   (nod_grp, numnod, temp_nod%num_bc, temp_nod%bc_name,           &
     &    temp_nod%ibc_type, temp_nod%bc_magnitude,                     &
     &    ibc_temp, ibc2_temp, num_bc_e_nod,                            &
     &    ibc_e_id, bc_e_id_apt, fhd_temp, ii)
!
      i0 = 0
      call set_bc_scalar_id                                             &
     &   (nod_grp, numnod, temp_nod%num_bc, temp_nod%bc_name,           &
     &    temp_nod%ibc_type, temp_nod%bc_magnitude,                     &
     &    ibc_t_sgs, ibc2_t_sgs, num_bc_t_sgs_nod,                      &
     &    ibc_t_sgs_id, bc_t_sgs_id_apt, iflag_bc_sgs_s, i0)
!
!
      if (iflag_4_ref_temp .ne. id_no_ref_temp) then
        call set_fixed_bc_4_par_temp
      end if
!
      end subroutine set_bc_fixed_temp_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_press_id(numnod, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_press
      use m_bc_press_sgs
      use set_nodal_boundary
!
      integer (kind=kint), intent(in) :: numnod
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: ii, i0
!
!
      ii = 0
      call set_fixed_bc_scalar_id                                       &
     &   (nod_grp, numnod, press_nod%num_bc, press_nod%bc_name,         &
     &    press_nod%ibc_type, press_nod%bc_magnitude,                   &
     &    ibc_press, ibc2_press, num_bc_p_nod,                          &
     &    ibc_p_id, bc_p_id_apt, fhd_press, ii)
!
      i0 = 0
      call set_bc_scalar_id                                             &
     &   (nod_grp, numnod, press_nod%num_bc, press_nod%bc_name,         &
     &    press_nod%ibc_type, press_nod%bc_magnitude,                   &
     &    ibc_press_sgs, ibc2_press_sgs, num_bc_ps_nod,                 &
     &    ibc_ps_id, bc_ps_id_apt, iflag_bc_sgs_s, i0)
!
      call set_potential_4_fixed_press
      call set_potential_4_sgs_press
!
      end subroutine set_bc_fixed_press_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_m_potential_id(numnod, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne_p
      use m_bc_mag_p_sgs
!
      integer (kind=kint), intent(in) :: numnod
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: ii, i0
!
!
      ii = 0
      call set_fixed_bc_scalar_id                                       &
     &   (nod_grp, numnod, e_potential_nod%num_bc,                      &
     &    e_potential_nod%bc_name, e_potential_nod%ibc_type,            &
     &    e_potential_nod%bc_magnitude, ibc_mag_p, ibc2_mag_p,          &
     &    num_bc_mag_p_nod, ibc_mag_p_id, bc_mag_p_id_apt,              &
     &    fhd_mag_potential, ii)
!
      call set_bc_sph_magne_p_id(nod_grp, e_potential_nod%num_bc,       &
     &    e_potential_nod%bc_name, e_potential_nod%ibc_type, ii)
!
      i0 = 0
      call set_bc_scalar_id(nod_grp, numnod, e_potential_nod%num_bc,    &
     &    e_potential_nod%bc_name, e_potential_nod%ibc_type,            &
     &    e_potential_nod%bc_magnitude, ibc_mp_sgs, ibc2_mp_sgs,        &
     &    num_bc_mp_sgs_nod, ibc_mp_sgs_id, bc_mp_sgs_id_apt,           &
     &    iflag_bc_sgs_s, i0)
!
      end subroutine set_bc_fixed_m_potential_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_composition_id(numnod, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_composition
!
      integer (kind=kint), intent(in) :: numnod
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: ii
!
      ii = 0
      call set_fixed_bc_scalar_id                                       &
     &   (nod_grp, numnod, light_nod%num_bc, light_nod%bc_name,         &
     &    light_nod%ibc_type,  light_nod%bc_magnitude, ibc_composit,    &
     &    ibc2_composit, num_bc_composition_nod, ibc_composit_id,       &
     &    bc_composit_id_apt, fhd_light, ii)
!
      end subroutine set_bc_fixed_composition_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module set_bc_scalars
