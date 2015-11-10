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
      use m_bc_data_velo
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
     &    nod_bc1_vfree%ibc, nod_bc1_vfree%ibc2,        &
     &    nod_bc1_vfree%num_bc_nod,       &
     &    nod_bc1_vfree%ibc_id, bc_fr_id_apt, iflag_free_sph, l_fr)
!
      l_r0 = 0
      call set_bc_scalar_id                                             &
     &   (nod_grp, numnod, velo_nod%num_bc, velo_nod%bc_name,           &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    nod_bc1_vr0%ibc, nod_bc1_vr0%ibc2, nod_bc1_vr0%num_bc_nod,    &
     &    nod_bc1_vr0%ibc_id, bc_vr0_id_apt, iflag_no_vr, l_r0)
!
      l_vsp = 0
      call set_bc_scalar_id                                             &
     &   (nod_grp, numnod, velo_nod%num_bc, velo_nod%bc_name,           &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    nod_bc1_vsp%ibc, nod_bc1_vsp%ibc2, nod_bc1_vsp%num_bc_nod,    &
     &    nod_bc1_vsp%ibc_id, bc_vsp_id_apt, iflag_bc_special, l_vsp)
!
      end subroutine set_bc_velo_4_sphere_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_temp_id(numnod, nod_grp)
!
      use m_node_phys_address
      use m_node_phys_data
      use m_control_parameter
      use m_bc_data_list
      use m_bc_data_ene
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
     &    nod_bc1_t%ibc, nod_bc1_t%ibc2, nod_bc1_t%num_bc_nod,          &
     &    nod_bc1_t%ibc_id, bc_e_id_apt, fhd_temp, ii)
!
      i0 = 0
      call set_bc_scalar_id                                             &
     &   (nod_grp, numnod, temp_nod%num_bc, temp_nod%bc_name,           &
     &    temp_nod%ibc_type, temp_nod%bc_magnitude,                     &
     &    sgs_bc1_t%ibc, sgs_bc1_t%ibc2, sgs_bc1_t%num_bc_nod,          &
     &    sgs_bc1_t%ibc_id, bc_t_sgs_id_apt, iflag_bc_sgs_s, i0)
!
!
      if (iflag_4_ref_temp .ne. id_no_ref_temp) then
        call set_fixed_bc_4_par_temp(numnod, nod_fld1%ntot_phys,        &
     &      iphys%i_ref_t, nod_fld1%d_fld)
      end if
!
      end subroutine set_bc_fixed_temp_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_press_id(numnod, nod_grp)
!
      use m_node_phys_address
      use m_node_phys_data
      use m_bc_data_list
      use m_bc_data_velo
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
     &    nod_bc1_p%ibc, nod_bc1_p%ibc2, nod_bc1_p%num_bc_nod,          &
     &    nod_bc1_p%ibc_id, bc_p_id_apt, fhd_press, ii)
!
      i0 = 0
      call set_bc_scalar_id                                             &
     &   (nod_grp, numnod, press_nod%num_bc, press_nod%bc_name,         &
     &    press_nod%ibc_type, press_nod%bc_magnitude,                   &
     &    sgs_bc1_p%ibc, sgs_bc1_p%ibc2, sgs_bc1_p%num_bc_nod,          &
     &    sgs_bc1_p%ibc_id, bc_ps_id_apt, iflag_bc_sgs_s, i0)
!
      call set_potential_4_fixed_press(numnod, nod_fld1%ntot_phys,      &
     &    iphys%i_press, iphys%i_p_phi, nod_fld1%d_fld)

      call set_potential_4_sgs_press(numnod, nod_fld1%ntot_phys,        &
     &    iphys%i_press, iphys%i_p_phi, nod_fld1%d_fld)
!
      end subroutine set_bc_fixed_press_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_m_potential_id(numnod, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne
!
      integer (kind=kint), intent(in) :: numnod
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: ii
!
!
      ii = 0
      call set_fixed_bc_scalar_id                                       &
     &   (nod_grp, numnod, e_potential_nod%num_bc,                      &
     &    e_potential_nod%bc_name, e_potential_nod%ibc_type,            &
     &    e_potential_nod%bc_magnitude, nod_bc1_f%ibc, nod_bc1_f%ibc2,  &
     &    nod_bc1_f%num_bc_nod, nod_bc1_f%ibc_id, bc_mag_p_id_apt,      &
     &    fhd_mag_potential, ii)
!
      call set_bc_sph_magne_p_id(nod_grp, e_potential_nod%num_bc,       &
     &    e_potential_nod%bc_name, e_potential_nod%ibc_type, ii)
!
      ii = 0
      call set_bc_scalar_id(nod_grp, numnod, e_potential_nod%num_bc,    &
     &    e_potential_nod%bc_name, e_potential_nod%ibc_type,            &
     &    e_potential_nod%bc_magnitude, sgs_bc1_f%ibc, sgs_bc1_f%ibc2,  &
     &    sgs_bc1_f%num_bc_nod, sgs_bc1_f%ibc_id, bc_mp_sgs_id_apt,     &
     &    iflag_bc_sgs_s, ii)
!
      end subroutine set_bc_fixed_m_potential_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_composition_id(numnod, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_ene
!
      integer (kind=kint), intent(in) :: numnod
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: ii
!
      ii = 0
      call set_fixed_bc_scalar_id                                       &
     &   (nod_grp, numnod, light_nod%num_bc, light_nod%bc_name,         &
     &    light_nod%ibc_type,  light_nod%bc_magnitude, nod_bc1_c%ibc,   &
     &    nod_bc1_c%ibc2, nod_bc1_c%num_bc_nod, nod_bc1_c%ibc_id,       &
     &    bc_composit_id_apt, fhd_light, ii)
!
      end subroutine set_bc_fixed_composition_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module set_bc_scalars
