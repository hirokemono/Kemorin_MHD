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
!!      subroutine cal_filterd_buo_flux_rtp(sph_rtp, fl_prop,           &
!!     &          b_trns_base, b_trns_fil, f_trns_fefx,                 &
!!     &          trns_b_snap, trns_bs_SGS, trns_f_snap)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(base_field_address), intent(in) :: b_trns_base
!!        type(base_field_address), intent(in) :: b_trns_fil
!!        type(energy_flux_address), intent(in) :: f_trns_fefx
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(in) :: trns_bs_SGS
!!        type(spherical_transform_data), intent(inout) :: trns_f_snap
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
      subroutine cal_filterd_buo_flux_rtp(sph_rtp, fl_prop,             &
     &          b_trns_base, b_trns_fil, f_trns_fefx,                   &
     &          trns_b_snap, trns_bs_SGS, trns_f_snap)
!
      use cal_buoyancy_flux_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(base_field_address), intent(in) :: b_trns_base
      type(base_field_address), intent(in) :: b_trns_fil
      type(energy_flux_address), intent(in) :: f_trns_fefx
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_bs_SGS
!
      type(spherical_transform_data), intent(inout) :: trns_f_snap
!
!
      call cal_buoyancy_flux_rtp                                        &
     &   (sph_rtp, fl_prop, ref_param_T, ref_param_C,                   &
     &    b_trns_base, b_trns_fil, fs_trns_eflux,                       &
     &    trns_b_snap, trns_bs_SGS, trns_f_eflux)
      call pole_buoyancy_flux_rtp                                       &
     &   (sph_rtp, fl_prop, ref_param_T, ref_param_C,                   &
     &    b_trns_base, b_trns_fil, fs_trns_eflux,                       &
     &    trns_b_snap, trns_bs_SGS, trns_f_eflux)
!
      end subroutine cal_filterd_buo_flux_rtp
!
!-----------------------------------------------------------------------
!
      end module cal_energy_flux_w_SGS_rtp
