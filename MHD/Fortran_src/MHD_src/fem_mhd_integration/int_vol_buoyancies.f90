!>@file   int_vol_buoyancies.f90
!!@brief  module int_vol_buoyancies
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!        modified by H. Matsui in Oct., 2005
!!        modified by H. Matsui in Aug., 2007
!!        modified by H. Matsui in Aug., 2025
!!
!>@brief  Finite elememt integration for Coriolis force
!!
!!@verbatim
!!      subroutine int_buoyancy_nod_exp(node, fl_prop, mlump_fl,        &
!!     &          iphys, iphys_LES, nod_fld, f_nl)
!!      subroutine set_boussinesq_density_at_node                       &
!!     &         (node, fl_prop, iphys, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module int_vol_buoyancies
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_physical_property
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_finite_element_mat
      use t_jacobians
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_buoyancy_nod_exp(node, fl_prop, mlump_fl,          &
     &          iphys, iphys_LES, nod_fld, f_nl)
!
      use copy_nodal_fields
      use set_buoyancy_at_node
!
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type (lumped_mass_matrices), intent(in) :: mlump_fl
      type(phys_data), intent(inout) :: nod_fld
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      if(fl_prop%iflag_FEM_gravity .ne. id_FORCE_at_node) return
!
! ---------  set buoyancy at each node
!
      call clear_field_data(nod_fld, n_vector, iphys%forces%i_buoyancy)
!
      if (fl_prop%flag_thermal_buoyancy) then
        call add_gravity_2_each_node                                    &
     &     (iphys%base%i_temp, iphys%forces%i_buoyancy,                 &
     &      fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,             &
     &      node, nod_fld)
      end if
!
      if (fl_prop%flag_comp_buoyancy) then
        call add_gravity_2_each_node                                    &
     &     (iphys%base%i_light, iphys%forces%i_buoyancy,                &
     &      fl_prop%i_grav, fl_prop%coef_comp_buo, fl_prop%grav,        &
     &      node, nod_fld)
      end if
!
      if(fl_prop%flag_filter_thermal_buo) then
        call add_gravity_2_each_node                                    &
     &     (iphys_LES%filter_fld%i_temp, iphys%forces%i_buoyancy,       &
     &      fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,             &
     &      node, nod_fld)
      end if
!
      if(fl_prop%flag_filter_comp_buo) then
        call add_gravity_2_each_node                                    &
     &     (iphys_LES%filter_fld%i_light, iphys%forces%i_buoyancy,      &
     &      fl_prop%i_grav, fl_prop%coef_comp_buo, fl_prop%grav,        &
     &      node, nod_fld)
      end if
!
      call int_vol_buoyancy_nod(node%numnod, node%istack_nod_smp,       &
     &    nod_fld%ntot_phys, iphys%forces%i_buoyancy, nod_fld%d_fld,    &
     &    mlump_fl%ml_o, f_nl%ff)
!
      end subroutine int_buoyancy_nod_exp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_boussinesq_density_at_node                         &
     &         (node, fl_prop, iphys, nod_fld)
!
      use set_buoyancy_at_node
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(inout) :: nod_fld
!
!
      call set_boussinesq_density_2_node                                &
     &   (node%numnod, node%istack_nod_smp,                             &
     &    fl_prop%coef_buo, fl_prop%coef_comp_buo,                      &
     &    nod_fld%ntot_phys, iphys%base%i_temp, iphys%base%i_light,     &
     &    iphys%base%i_density, nod_fld%d_fld)
!
      end subroutine set_boussinesq_density_at_node
!
!  ---------------------------------------------------------------------
!
      end module int_vol_buoyancies
