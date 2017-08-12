!>@file   copy_Csim_4_sph_MHD.f90
!!@brief  module copy_Csim_4_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine copy_Csim_buo_4_sph_trans                            &
!!     &         (sph_rtp, ifld_sgs,  wk_sgs, trns_SGS)
!!      subroutine copy_model_coefs_4_sph_snap                          &
!!     &         (sph_rtp, ifld_sgs,  wk_sgs, trns_snap)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: bg_trns, fg_trns
!!        type(SGS_terms_address), intent(in) :: ifld_sgs, icomp_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!@endverbatim
!
      module copy_Csim_4_sph_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_spheric_rtp_data
      use t_phys_address
      use t_addresses_sph_transform
      use t_ele_info_4_dynamic
      use t_addresses_sph_transform
      use t_SGS_model_coefs
!
      implicit none
!
      private :: set_model_coefs_sph_snap
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_Csim_buo_4_sph_trans                              &
     &         (sph_rtp, ifld_sgs,  wk_sgs, trns_SGS)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_terms_address), intent(in) :: ifld_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ifld_sgs%i_buoyancy',                               &
     &        ifld_sgs%i_buoyancy, trns_SGS%f_trns%i_Csim_SGS_buoyancy
        write(*,*) 'ifld_sgs%i_comp_buoyancy',                          &
     &        ifld_sgs%i_comp_buoyancy,                                 &
     &        trns_SGS%f_trns%i_Csim_SGS_comp_buo
      end if
!
      if(ifld_sgs%i_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_SGS%f_trns%i_Csim_SGS_buoyancy,                        &
     &      ifld_sgs%i_buoyancy, wk_sgs, trns_SGS)
      end if
!
      if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_SGS%f_trns%i_Csim_SGS_comp_buo,                        &
     &      ifld_sgs%i_comp_buoyancy, wk_sgs, trns_SGS)
      end if
!
      end subroutine copy_Csim_buo_4_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine copy_model_coefs_4_sph_snap                            &
     &         (sph_rtp, ifld_sgs,  wk_sgs, trns_snap)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_terms_address), intent(in) :: ifld_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_snap
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ifld_sgs%i_mom_flux',                               &
     &        ifld_sgs%i_mom_flux, trns_snap%f_trns%i_Csim_SGS_m_flux
        write(*,*) 'ifld_sgs%i_lorentz',                                &
     &        ifld_sgs%i_lorentz, trns_snap%f_trns%i_Csim_SGS_Lorentz
        write(*,*) 'ifld_sgs%i_induction', ifld_sgs%i_induction,        &
     &        trns_snap%f_trns%i_Csim_SGS_induction
        write(*,*) 'ifld_sgs%i_heat_flux',                              &
     &        ifld_sgs%i_heat_flux, trns_snap%f_trns%i_Csim_SGS_h_flux
        write(*,*) 'ifld_sgs%i_comp_flux',                              &
     &        ifld_sgs%i_comp_flux, trns_snap%f_trns%i_Csim_SGS_c_flux
        write(*,*) 'ifld_sgs%i_buoyancy',                               &
     &        ifld_sgs%i_buoyancy, trns_snap%f_trns%i_Csim_SGS_buoyancy
        write(*,*) 'ifld_sgs%i_comp_buoyancy',                          &
     &        ifld_sgs%i_comp_buoyancy,                                 &
     &        trns_snap%f_trns%i_Csim_SGS_comp_buo
      end if
!
      if(ifld_sgs%i_mom_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_m_flux, ifld_sgs%i_mom_flux,    &
     &      wk_sgs, trns_snap)
      end if
!
      if(ifld_sgs%i_lorentz .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_Lorentz, ifld_sgs%i_lorentz,    &
     &      wk_sgs, trns_snap)
      end if
!
      if(ifld_sgs%i_induction .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &     trns_snap%f_trns%i_Csim_SGS_induction, ifld_sgs%i_induction, &
     &     wk_sgs, trns_snap)
      end if
!
      if(ifld_sgs%i_heat_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_h_flux, ifld_sgs%i_heat_flux,   &
     &      wk_sgs, trns_snap)
      end if
!
      if(ifld_sgs%i_comp_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_c_flux, ifld_sgs%i_comp_flux,   &
     &      wk_sgs, trns_snap)
      end if
!
!
      if(ifld_sgs%i_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_buoyancy,                       &
     &      ifld_sgs%i_buoyancy, wk_sgs, trns_snap)
      end if
!
      if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_comp_buo,                       &
     &      ifld_sgs%i_comp_buoyancy, wk_sgs, trns_snap)
      end if
!
      end subroutine copy_model_coefs_4_sph_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_model_coefs_sph_snap                               &
     &         (sph_rtp, irtp_sgs, ifld_sgs, wk_sgs, trns_SGS)
!
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: irtp_sgs
      integer(kind = kint), intent(in) :: ifld_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
      integer(kind = kint) :: nnod_med
!
!
!$omp parallel workshare
      trns_SGS%frc_rtp(1:sph_rtp%nnod_rtp,irtp_sgs) = one
!$omp end parallel workshare
!
      nnod_med = sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)
!$omp parallel
      call product_model_coefs_pout(ifld_sgs, sph_rtp%nidx_rtp(3),      &
     &   nnod_med, wk_sgs%num_kinds, wk_sgs%fld_coef,                   &
     &   irtp_sgs, ione, sph_rtp%nnod_rtp, trns_SGS%ncomp_rtp_2_rj,     &
     &   trns_SGS%frc_rtp)
!$omp end parallel
!
      end subroutine set_model_coefs_sph_snap
!
! ----------------------------------------------------------------------
!
      end module copy_Csim_4_sph_MHD
