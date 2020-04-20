!>@file   copy_Csim_4_sph_MHD.f90
!!@brief  module copy_Csim_4_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine copy_Csim_buo_4_sph_trans(sph_rtp, sph_d_grp,        &
!!     &          ifld_sgs, f_trns_Csim, wk_sgs, fwd_Csim)
!!      subroutine copy_model_coefs_4_sph_snap(sph_rtp, sph_d_grp,      &
!!     &          ifld_sgs, f_trns_Csim, wk_sgs, fwd_Csim)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!        type(SGS_terms_address), intent(in) :: ifld_sgs
!!        type(SGS_term_address), intent(in) :: f_trns_Csim
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!        type(address_each_sph_trans), intent(inout) :: fwd_Csim
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
      use t_SGS_model_coef_labels
      use t_ele_info_4_dynamic
      use t_addresses_sph_transform
      use t_SGS_model_coefs
      use t_groups_sph_dynamic
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
      subroutine copy_Csim_buo_4_sph_trans(sph_rtp, sph_d_grp,          &
     &          ifld_sgs, f_trns_Csim, wk_sgs, fwd_Csim)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(SGS_term_address), intent(in) :: f_trns_Csim
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_each_sph_trans), intent(inout) :: fwd_Csim
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ifld_sgs%SGS_term%i_SGS_buoyancy',                  &
     &        ifld_sgs%SGS_term%i_SGS_buoyancy, f_trns_Csim%i_SGS_buoyancy
        write(*,*) 'ifld_sgs%SGS_term%i_SGS_comp_buo',                  &
     &     ifld_sgs%SGS_term%i_SGS_comp_buo, f_trns_Csim%i_SGS_comp_buo
      end if
!
      if(ifld_sgs%SGS_term%i_SGS_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_buoyancy, ifld_sgs%SGS_term%i_SGS_buoyancy,            &
     &      wk_sgs, fwd_Csim)
      end if
!
      if(ifld_sgs%SGS_term%i_SGS_comp_buo .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_comp_buo, ifld_sgs%SGS_term%i_SGS_comp_buo,       &
     &      wk_sgs, fwd_Csim)
      end if
!
      end subroutine copy_Csim_buo_4_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine copy_model_coefs_4_sph_snap(sph_rtp, sph_d_grp,        &
     &          ifld_sgs, f_trns_Csim, wk_sgs, fwd_Csim)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(SGS_term_address), intent(in) :: f_trns_Csim
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_each_sph_trans), intent(inout) :: fwd_Csim
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ifld_sgs%i_mom_flux',                               &
     &        ifld_sgs%i_mom_flux, f_trns_Csim%i_SGS_m_flux
        write(*,*) 'ifld_sgs%SGS_term%i_SGS_Lorentz',                   &
     &        ifld_sgs%SGS_term%i_SGS_Lorentz, f_trns_Csim%i_SGS_Lorentz
        write(*,*) 'ifld_sgs%SGS_term%i_SGS_induction',                 &
     &        ifld_sgs%SGS_term%i_SGS_induction,                        &
     &        f_trns_Csim%i_SGS_vp_induct
        write(*,*) 'ifld_sgs%i_heat_flux',                              &
     &        ifld_sgs%i_heat_flux, f_trns_Csim%i_SGS_h_flux
        write(*,*) 'ifld_sgs%i_comp_flux',                              &
     &        ifld_sgs%i_comp_flux, f_trns_Csim%i_SGS_c_flux
        write(*,*) 'ifld_sgs%SGS_term%i_SGS_buoyancy',                  &
     &        ifld_sgs%SGS_term%i_SGS_buoyancy, f_trns_Csim%i_SGS_buoyancy
        write(*,*) 'ifld_sgs%SGS_term%i_SGS_comp_buo',                  &
     &        ifld_sgs%SGS_term%i_SGS_comp_buo, f_trns_Csim%i_SGS_comp_buo
      end if
!
      if(ifld_sgs%i_mom_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_m_flux, ifld_sgs%i_mom_flux,              &
     &      wk_sgs, fwd_Csim)
      end if
!
      if(ifld_sgs%SGS_term%i_SGS_Lorentz .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_Lorentz, ifld_sgs%SGS_term%i_SGS_Lorentz, &
     &      wk_sgs, fwd_Csim)
      end if
!
      if(ifld_sgs%SGS_term%i_SGS_induction .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &     f_trns_Csim%i_SGS_vp_induct, ifld_sgs%SGS_term%i_SGS_induction, &
     &     wk_sgs, fwd_Csim)
      end if
!
      if(ifld_sgs%i_heat_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_h_flux, ifld_sgs%i_heat_flux,             &
     &      wk_sgs, fwd_Csim)
      end if
!
      if(ifld_sgs%i_comp_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_c_flux, ifld_sgs%i_comp_flux,             &
     &      wk_sgs, fwd_Csim)
      end if
!
!
      if(ifld_sgs%SGS_term%i_SGS_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_buoyancy, ifld_sgs%SGS_term%i_SGS_buoyancy,            &
     &      wk_sgs, fwd_Csim)
      end if
!
      if(ifld_sgs%SGS_term%i_SGS_comp_buo .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_comp_buo, ifld_sgs%SGS_term%i_SGS_comp_buo,       &
     &      wk_sgs, fwd_Csim)
      end if
!
      end subroutine copy_model_coefs_4_sph_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_model_coefs_sph_snap(sph_rtp, sph_d_grp,           &
     &          irtp_sgs, ifld_sgs, wk_sgs, trns_fwd)
!
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      integer(kind = kint), intent(in) :: irtp_sgs
      integer(kind = kint), intent(in) :: ifld_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_each_sph_trans), intent(inout) :: trns_fwd
!
!
!$omp parallel workshare
      trns_fwd%fld_rtp(1:sph_rtp%nnod_rtp,irtp_sgs) = one
!$omp end parallel workshare
!
      call product_model_coefs_pout(one, ifld_sgs, sph_rtp, sph_d_grp,  &
     &    wk_sgs%num_kinds, wk_sgs%fld_coef, irtp_sgs, ione,            &
     &    trns_fwd%ncomp, trns_fwd%fld_rtp)
!
      end subroutine set_model_coefs_sph_snap
!
! ----------------------------------------------------------------------
!
      end module copy_Csim_4_sph_MHD
