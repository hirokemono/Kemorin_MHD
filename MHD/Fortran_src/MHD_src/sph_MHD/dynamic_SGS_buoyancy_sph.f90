!>@file   dynamic_SGS_buoyancy_sph.f90
!!@brief  module dynamic_SGS_buoyancy_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2016
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine const_dynamic_SGS_4_buo_sph(sph_rtp, fl_prop,        &
!!     &          trns_MHD, trns_snap, trns_SGS, dynamic_SPH)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!@endverbatim
!
      module dynamic_SGS_buoyancy_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_phys_address
      use t_addresses_sph_transform
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use sph_filtering
!
      implicit none
!
      private :: cal_SGS_buo_coefs_sph_MHD
      private :: prod_SGS_buoyancy_to_Reynolds
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_dynamic_SGS_4_buo_sph(sph_rtp, fl_prop,          &
     &          trns_MHD, trns_snap, trns_SGS, dynamic_SPH)
!
      use t_physical_property
      use cal_SGS_buo_flux_sph_MHD
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(address_4_sph_trans), intent(in) :: trns_MHD
!
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
      integer(kind = kint) :: nnod_med
!
!
      call calypso_mpi_barrier
      write(*,*) 'SGS_fluxes_for_buo_coefs'
      nnod_med = sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)
      call SGS_fluxes_for_buo_coefs(nnod_med, sph_rtp, fl_prop,         &
     &    trns_MHD%b_trns, trns_SGS%f_trns, trns_snap%f_trns,           &
     &    trns_MHD%ncomp_rj_2_rtp, trns_SGS%ncomp_rtp_2_rj,             &
     &    trns_snap%ncomp_rtp_2_rj, trns_MHD%fld_rtp, trns_SGS%frc_rtp, &
     &    trns_snap%frc_rtp)
!
      call calypso_mpi_barrier
      write(*,*) 'cal_SGS_buo_coefs_sph_MHD'
      call cal_SGS_buo_coefs_sph_MHD(sph_rtp, nnod_med,                 &
     &    trns_snap%f_trns, trns_snap%ncomp_rtp_2_rj,                   &
     &    trns_snap%frc_rtp, dynamic_SPH%ifld_sgs,                      &
     &    dynamic_SPH%icomp_sgs, dynamic_SPH%wk_sgs)
!
      call calypso_mpi_barrier
      write(*,*) 'prod_SGS_buoyancy_to_Reynolds'
      call prod_SGS_buoyancy_to_Reynolds(sph_rtp, trns_SGS%f_trns,      &
     &    dynamic_SPH%ifld_sgs, dynamic_SPH%wk_sgs,                     &
     &    nnod_med, trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
!
      end subroutine const_dynamic_SGS_4_buo_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_SGS_buo_coefs_sph_MHD(sph_rtp, nnod_med, fs_trns,  &
     &          ncomp_snap_rtp_2_rj, frs_rtp, ifld_sgs, icomp_sgs,      &
     &          wk_sgs)
!
      use zonal_lsq_4_model_coefs
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: fs_trns
      type(SGS_terms_address), intent(in) :: ifld_sgs, icomp_sgs
      integer(kind = kint), intent(in) :: nnod_med
      integer(kind = kint), intent(in) :: ncomp_snap_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &               :: frs_rtp(sph_rtp%nnod_rtp,ncomp_snap_rtp_2_rj)
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
!
      if(ifld_sgs%i_buoyancy .gt. 0) then
        call sel_int_zonal_for_buo_coefs                                &
     &     (sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),            &
     &      frs_rtp(1,fs_trns%i_reynolds_wk),                           &
     &      frs_rtp(1,fs_trns%i_SGS_buo_wk),                            &
     &      wk_sgs%comp_coef(1,icomp_sgs%i_buoyancy),                   &
     &      wk_sgs%comp_clip(1,icomp_sgs%i_buoyancy),                   &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_buoyancy))
      end if
!
      if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call sel_int_zonal_for_buo_coefs                                &
     &     (sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),            &
     &      frs_rtp(1,fs_trns%i_reynolds_wk),                           &
     &      frs_rtp(1,fs_trns%i_SGS_comp_buo_wk),                       &
     &      wk_sgs%comp_coef(1,icomp_sgs%i_comp_buoyancy),              &
     &      wk_sgs%comp_clip(1,icomp_sgs%i_comp_buoyancy),              &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_comp_buoyancy))
      end if
!
      end subroutine cal_SGS_buo_coefs_sph_MHD
!
! ----------------------------------------------------------------------
!
      subroutine prod_SGS_buoyancy_to_Reynolds(sph_rtp, fg_trns,        &
     &          ifld_sgs, wk_sgs, nnod_med, nc_SGS_rtp_2_rj, fSGS_rtp)
!
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: fg_trns
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(dynamic_model_data), intent(in) :: wk_sgs
      integer(kind = kint), intent(in) :: nnod_med
      integer(kind = kint), intent(in) :: nc_SGS_rtp_2_rj
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: fSGS_rtp(sph_rtp%nnod_rtp,nc_SGS_rtp_2_rj)
!
!
      if     (ifld_sgs%i_buoyancy .gt. 0                                &
     &  .and. ifld_sgs%i_comp_buoyancy .gt. 0) then
        call sel_product_double_buo_coefs                               &
     &     (n_vector, sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),  &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_buoyancy),                     &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_comp_buoyancy),                &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia))
      else if(ifld_sgs%i_buoyancy .gt. 0) then
        call sel_product_single_buo_coefs                               &
     &     (n_vector, sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),  &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_buoyancy),                     &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia))
      else if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call sel_product_single_buo_coefs                               &
     &     (n_vector, sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),  &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_comp_buoyancy),                &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia))
      end if
!
      end subroutine prod_SGS_buoyancy_to_Reynolds
!
! ----------------------------------------------------------------------
!
      end module dynamic_SGS_buoyancy_sph
 