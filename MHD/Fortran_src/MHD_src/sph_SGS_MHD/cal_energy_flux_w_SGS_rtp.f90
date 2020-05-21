!> @file  cal_energy_flux_w_SGS_rtp.f90
!!      module cal_energy_flux_w_SGS_rtp
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine cal_filtered_energy_flux_rtp                         &
!!     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C, bs_trns,  &
!!     &          f_trns_LES, bs_trns_LES, be_trns_LES, fe_trns_LES,    &
!!     &          trns_b_snap, trns_f_fil_MHD, trns_b_fil_snap,         &
!!     &          trns_b_fil_eflux, trns_f_fil_eflux)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(phys_address), intent(in) :: bs_trns
!!        type(SGS_model_addresses), intent(in) :: f_trns_LES
!!        type(SGS_model_addresses), intent(in) :: bs_trns_LES
!!        type(SGS_model_addresses), intent(in) :: be_trns_LES
!!        type(SGS_model_addresses), intent(in) :: fe_trns_LES
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(in) :: trns_f_fil_MHD
!!        type(spherical_transform_data), intent(in) :: trns_b_fil_snap
!!        type(spherical_transform_data), intent(in) :: trns_b_fil_eflux
!!        type(spherical_transform_data), intent(inout)                 &
!!@endverbatim
!
      module cal_energy_flux_w_SGS_rtp
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_phys_address
      use t_SGS_model_addresses
      use t_spheric_rtp_data
      use t_physical_property
      use t_reference_scalar_param
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_filtered_energy_flux_rtp                           &
     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C, bs_trns,    &
     &          f_trns_LES, bs_trns_LES, be_trns_LES, fe_trns_LES,      &
     &          trns_b_snap, trns_f_fil_MHD, trns_b_fil_snap,           &
     &          trns_b_fil_eflux, trns_f_fil_eflux)
!
      use cal_energy_flux_rtp
      use cal_buoyancy_flux_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
!
      type(phys_address), intent(in) :: bs_trns
      type(SGS_model_addresses), intent(in) :: f_trns_LES
      type(SGS_model_addresses), intent(in) :: bs_trns_LES
      type(SGS_model_addresses), intent(in) :: be_trns_LES
      type(SGS_model_addresses), intent(in) :: fe_trns_LES
!
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_f_fil_MHD
      type(spherical_transform_data), intent(in) :: trns_b_fil_snap
      type(spherical_transform_data), intent(in) :: trns_b_fil_eflux
!
      type(spherical_transform_data), intent(inout) :: trns_f_fil_eflux
!
!
      call cal_energy_fluxes_on_node(bs_trns%base,                      &
     &    f_trns_LES%force_by_filter, be_trns_LES%force_by_filter,      &
     &    fe_trns_LES%eflux_by_filter, sph_rtp%nnod_rtp,                &
     &    trns_b_snap%ncomp, trns_b_snap%fld_rtp,                       &
     &    trns_f_fil_MHD%ncomp, trns_f_fil_MHD%fld_rtp,                 &
     &    trns_b_fil_eflux%ncomp, trns_b_fil_eflux%fld_rtp,             &
     &    trns_f_fil_eflux%ncomp, trns_f_fil_eflux%fld_rtp)
      call cal_energy_fluxes_on_node(bs_trns%base,                      &
     &    f_trns_LES%force_by_filter, be_trns_LES%force_by_filter,      &
     &    fe_trns_LES%eflux_by_filter, sph_rtp%nnod_pole,               &
     &    trns_b_snap%ncomp, trns_b_snap%fld_pole,                      &
     &    trns_f_fil_MHD%ncomp, trns_f_fil_MHD%fld_pole,                &
     &    trns_b_fil_eflux%ncomp, trns_b_fil_eflux%fld_pole,            &
     &    trns_f_fil_eflux%ncomp, trns_f_fil_eflux%fld_pole)
!
      call cal_buoyancy_flux_rtp                                        &
     &   (sph_rtp, fl_prop, ref_param_T, ref_param_C,                   &
     &    bs_trns%base, bs_trns_LES%filter_fld,                         &
     &    fe_trns_LES%eflux_by_filter,                                  &
     &    trns_b_snap, trns_b_fil_snap, trns_f_fil_eflux)
      call pole_buoyancy_flux_rtp                                       &
     &   (sph_rtp, fl_prop, ref_param_T, ref_param_C,                   &
     &    bs_trns%base, bs_trns_LES%filter_fld,                         &
     &    fe_trns_LES%eflux_by_filter,                                  &
     &    trns_b_snap, trns_b_fil_snap, trns_f_fil_eflux)
!
      end subroutine cal_filtered_energy_flux_rtp
!
!-----------------------------------------------------------------------
!
      end module cal_energy_flux_w_SGS_rtp
