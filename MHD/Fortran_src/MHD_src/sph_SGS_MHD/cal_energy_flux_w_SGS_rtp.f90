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
!!      subroutine cal_filterd_buo_flux_rtp                             &
!!     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C,           &
!!     &          b_trns_base, b_trns_fil, f_trns_fefx,                 &
!!     &          trns_b_snap, trns_bs_SGS, trns_f_eflux)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(base_field_address), intent(in) :: b_trns_base
!!        type(base_field_address), intent(in) :: b_trns_fil
!!        type(energy_flux_address), intent(in) :: f_trns_fefx
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(in) :: trns_bs_SGS
!!        type(spherical_transform_data), intent(inout) :: trns_f_eflux
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
!      call cal_filter_nonlinear_pole_MHD(sph_rtp, MHD_prop,             &
!     &    trns_fil_snap%f_trns_LES, trns_fil_MHD%f_trns_LES, trns_b_snap, trns_f_MHD)
!      subroutine cal_filter_nonlinear_pole_MHD(sph_rtp, MHD_prop,       &
!     &          bs_trns, f_trns, trns_b_snap, trns_f_MHD)
!
!      use cal_nonlinear_sph_MHD
!      use const_wz_coriolis_rtp
!      use cal_products_smp
!
!      type(sph_rtp_grid), intent(in) :: sph_rtp
!      type(MHD_evolution_param), intent(in) :: MHD_prop
!      type(SGS_model_addresses), intent(in) :: bs_trns_LES
!      type(SGS_model_addresses), intent(in) :: f_trns_LES
!      type(spherical_transform_data), intent(in) :: trns_b_snap
!
!      type(spherical_transform_data), intent(inout) :: trns_f_MHD
!
!
!      call nonlinear_terms_on_node(MHD_prop,                            &
!     &    bs_trns_LES%filter_fld, f_trns_LES%force_by_filter, sph_rtp%nnod_pole,               &
!     &    trns_b_snap%ncomp, trns_b_snap%fld_pole,                      &
!     &    trns_f_MHD%ncomp, trns_f_MHD%fld_pole)
!
!      end subroutine cal_filter_nonlinear_pole_MHD
!
!-----------------------------------------------------------------------
!
      subroutine cal_filterd_buo_flux_rtp                               &
     &         (sph_rtp, fl_prop, ref_param_T, ref_param_C,             &
     &          b_trns_base, b_trns_fil, f_trns_fefx,                   &
     &          trns_b_snap, trns_bs_SGS, trns_f_eflux)
!
      use cal_buoyancy_flux_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(base_field_address), intent(in) :: b_trns_base
      type(base_field_address), intent(in) :: b_trns_fil
      type(energy_flux_address), intent(in) :: f_trns_fefx
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_bs_SGS
!
      type(spherical_transform_data), intent(inout) :: trns_f_eflux
!
!
      call cal_buoyancy_flux_rtp                                        &
     &   (sph_rtp, fl_prop, ref_param_T, ref_param_C,                   &
     &    b_trns_base, b_trns_fil, f_trns_fefx,                         &
     &    trns_b_snap, trns_bs_SGS, trns_f_eflux)
      call pole_buoyancy_flux_rtp                                       &
     &   (sph_rtp, fl_prop, ref_param_T, ref_param_C,                   &
     &    b_trns_base, b_trns_fil, f_trns_fefx,                         &
     &    trns_b_snap, trns_bs_SGS, trns_f_eflux)
!
      end subroutine cal_filterd_buo_flux_rtp
!
!-----------------------------------------------------------------------
!
      end module cal_energy_flux_w_SGS_rtp
