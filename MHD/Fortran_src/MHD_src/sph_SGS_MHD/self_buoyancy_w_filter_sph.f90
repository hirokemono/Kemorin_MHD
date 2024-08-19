!>@file   self_buoyancy_w_filter_sph.f90
!!@brief  module self_buoyancy_w_filter_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate divergence of buoyancy for pressure evaluation
!!
!!@verbatim
!!      subroutine cal_self_buoyancy_sph_SGS_MHD                        &
!!     &         (sph, leg, ipol, ipol_LES, MHD_prop, sph_bc_U, rj_fld)
!!      subroutine sel_rot_filter_buoyancy_sph                          &
!!     &         (sph, ipol_LES, MHD_prop, sph_bc_U, rj_fld)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_grids), intent(in) ::  sph
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!
      module self_buoyancy_w_filter_sph
!
      use m_precision
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_boundary_params_sph_MHD
!
      use t_physical_property
      use t_reference_scalar_param
      use t_base_field_labels
      use t_base_force_labels
      use t_grad_field_labels
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_self_buoyancy_sph_SGS_MHD                          &
     &         (sph, leg, ipol, ipol_LES, MHD_prop, sph_bc_U, rj_fld)
!
      use t_schmidt_poly_on_rtm
      use cal_self_buoyancies_sph
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_grids), intent(in) :: sph
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ibuo_temp,  ibuo_comp
!
!
      call sel_field_address_for_buoyancies                             &
     &   (ipol%base, MHD_prop%ref_param_T, MHD_prop%ref_param_C,        &
     &    ibuo_temp, ibuo_comp)
      call sel_buoyancies_sph_MHD                                       &
     &   (sph%sph_rj, leg, ipol%forces, MHD_prop%fl_prop,               &
     &    sph_bc_U, ibuo_temp, ibuo_comp, rj_fld)
!
      call sel_buoyancies_sph_MHD(sph%sph_rj, leg,                      &
     &    ipol_LES%force_by_filter, MHD_prop%fl_prop, sph_bc_U,         &
     &    ipol_LES%filter_fld%i_temp, ipol_LES%filter_fld%i_light,      &
     &    rj_fld)
!
      end subroutine cal_self_buoyancy_sph_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sel_rot_filter_buoyancy_sph                            &
     &         (sph, ipol_LES, MHD_prop, sph_bc_U, rj_fld)
!
      use rot_self_buoyancies_sph
! 
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_grids), intent(in) ::  sph
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_rot_buoyancy_sph_MHD                                     &
     &   (sph%sph_rj, ipol_LES%filter_fld, ipol_LES%rot_frc_by_filter,  &
     &    MHD_prop%fl_prop, sph_bc_U, rj_fld)
!
      end subroutine sel_rot_filter_buoyancy_sph
!
!-----------------------------------------------------------------------
!
      end module self_buoyancy_w_filter_sph
