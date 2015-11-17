!set_bc_scalars.f90
!      module set_bc_scalars
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_bc_velo_4_sphere_id(numnod, nod_grp)
!      subroutine set_bc_fixed_temp_id(node, nod_grp)
!      subroutine set_bc_fixed_press_id(node, nod_grp)
!      subroutine set_bc_fixed_m_potential_id(node, nod_grp)
!      subroutine set_bc_fixed_composition_id(node, nod_grp)
!
      module set_bc_scalars
!
      use m_precision
!
      use m_constants
      use m_phys_labels
      use m_boundary_condition_IDs
      use t_geometry_data
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
      subroutine set_bc_velo_4_sphere_id(node, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_velo
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: l_fr, l_r0, l_vsp
!
!
      l_fr = 0
      call set_bc_scalar_id                                             &
     &   (node, nod_grp, velo_nod%num_bc, velo_nod%bc_name,             &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    nod_bc1_vfree%ibc, nod_bc1_vfree%ibc2,                        &
     &    nod_bc1_vfree%num_bc_nod, nod_bc1_vfree%ibc_id,               &
     &    nod_bc1_vfree%bc_apt, iflag_free_sph, l_fr)
!
      l_r0 = 0
      call set_bc_scalar_id                                             &
     &   (node, nod_grp, velo_nod%num_bc, velo_nod%bc_name,             &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    nod_bc1_vr0%ibc, nod_bc1_vr0%ibc2, nod_bc1_vr0%num_bc_nod,    &
     &    nod_bc1_vr0%ibc_id, nod_bc1_vr0%bc_apt, iflag_no_vr, l_r0)
!
      l_vsp = 0
      call set_bc_scalar_id                                             &
     &   (node, nod_grp, velo_nod%num_bc, velo_nod%bc_name,             &
     &    velo_nod%ibc_type, velo_nod%bc_magnitude,                     &
     &    nod_bc1_vsp%ibc, nod_bc1_vsp%ibc2, nod_bc1_vsp%num_bc_nod,    &
     &    nod_bc1_vsp%ibc_id, nod_bc1_vsp%bc_apt,                       &
     &    iflag_bc_special, l_vsp)
!
      end subroutine set_bc_velo_4_sphere_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_temp_id(node, nod_grp)
!
      use m_node_phys_address
      use m_node_phys_data
      use m_control_parameter
      use m_bc_data_list
      use m_bc_data_ene
      use set_nodal_boundary
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: ii, i0
!
!
      nod_bc1_t%scalar_bc_name = fhd_temp
      ii = 0
      call set_bc_fixed_scalar_id                                       &
     &   (node, nod_grp, temp_nod, nod_bc1_t, ii)
!
      i0 = 0
      call set_bc_sgs_scalar_id                                         &
     &   (node, nod_grp, temp_nod, sgs_bc1_t, i0)
!
!
      if (iflag_4_ref_temp .ne. id_no_ref_temp) then
        call set_fixed_bc_4_par_temp(node%numnod, nod_fld1%ntot_phys,   &
     &      iphys%i_ref_t, nod_fld1%d_fld)
      end if
!
      end subroutine set_bc_fixed_temp_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_press_id(node, nod_grp)
!
      use m_node_phys_address
      use m_node_phys_data
      use m_bc_data_list
      use m_bc_data_velo
      use set_nodal_boundary
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: ii, i0
!
!
      nod_bc1_p%scalar_bc_name = fhd_press
      ii = 0
      call set_bc_fixed_scalar_id                                       &
     &   (node, nod_grp, press_nod, nod_bc1_p, ii)
!
      i0 = 0
      call set_bc_sgs_scalar_id                                         &
     &   (node, nod_grp, press_nod, sgs_bc1_p, i0)
!
      call set_potential_4_fixed_press(node%numnod, nod_fld1%ntot_phys, &
     &    iphys%i_press, iphys%i_p_phi, nod_fld1%d_fld)

      call set_potential_4_sgs_press(node%numnod, nod_fld1%ntot_phys,   &
     &    iphys%i_press, iphys%i_p_phi, nod_fld1%d_fld)
!
      end subroutine set_bc_fixed_press_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_m_potential_id(node, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_magne
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: ii
!
!
      nod_bc1_f%scalar_bc_name = fhd_mag_potential
      ii = 0
      call set_bc_fixed_scalar_id                                       &
     &   (node, nod_grp, e_potential_nod, nod_bc1_f, ii)
!
      call set_bc_sph_magne_p_id(nod_grp, e_potential_nod%num_bc,       &
     &    e_potential_nod%bc_name, e_potential_nod%ibc_type, ii)
!
      ii = 0
      call set_bc_sgs_scalar_id                                         &
     &   (node, nod_grp, e_potential_nod, sgs_bc1_f, ii)
!
      end subroutine set_bc_fixed_m_potential_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_composition_id(node, nod_grp)
!
      use m_bc_data_list
      use m_bc_data_ene
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint) :: ii
!
      nod_bc1_c%scalar_bc_name = fhd_light
      ii = 0
      call set_bc_fixed_scalar_id                                       &
     &   (node, nod_grp, light_nod, nod_bc1_c, ii)
!
      end subroutine set_bc_fixed_composition_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_bc_fixed_scalar_id                                 &
     &         (node, nod_grp, bc_list, scalar_bc, ii)
!
      use t_nodal_bc_data
      use m_bc_data_list
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
      integer (kind = kint), intent(inout) :: ii
!
!
      call set_fixed_bc_scalar_id                                       &
     &   (node, nod_grp, bc_list%num_bc, bc_list%bc_name,               &
     &    bc_list%ibc_type,  bc_list%bc_magnitude, scalar_bc%ibc,       &
     &    scalar_bc%ibc2, scalar_bc%num_bc_nod, scalar_bc%ibc_id,       &
     &    scalar_bc%bc_apt, scalar_bc%scalar_bc_name, ii)
!
      end subroutine set_bc_fixed_scalar_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_sgs_scalar_id                                   &
     &         (node, nod_grp, bc_list, scalar_bc, ii)
!
      use t_nodal_bc_data
      use m_bc_data_list
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(nod_bc_list_type), intent(in) :: bc_list
!
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
      integer (kind = kint), intent(inout) :: ii
!
!
      call set_bc_scalar_id(node, nod_grp, bc_list%num_bc,              &
     &    bc_list%bc_name, bc_list%ibc_type, bc_list%bc_magnitude,      &
     &    scalar_bc%ibc, scalar_bc%ibc2, scalar_bc%num_bc_nod,          &
     &    scalar_bc%ibc_id, scalar_bc%bc_apt, iflag_bc_sgs_s, ii)
!
      end subroutine set_bc_sgs_scalar_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_scalars
