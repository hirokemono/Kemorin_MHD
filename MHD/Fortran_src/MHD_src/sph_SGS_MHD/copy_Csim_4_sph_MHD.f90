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
!!     &          iak_sgs_term, f_trns_Csim, wk_sph_sgs, fwd_Csim)
!!      subroutine copy_model_coefs_4_sph_snap(sph_rtp, sph_d_grp,      &
!!     &          iak_sgs_term, f_trns_Csim, wk_sph_sgs, fwd_Csim)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(SGS_term_address), intent(in) :: f_trns_Csim
!!        type(SPH_dynamic_model_coefs), intent(inout) :: wk_sph_sgs
!!        type(spherical_transform_data), intent(inout) :: fwd_Csim
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
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
      use t_ele_info_4_dynamic
      use t_addresses_sph_transform
      use t_groups_sph_dynamic
      use t_SPH_dynamic_model_coefs
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
     &          iak_sgs_term, f_trns_Csim, wk_sph_sgs, fwd_Csim)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: f_trns_Csim
!
      type(SPH_dynamic_model_coefs), intent(inout) :: wk_sph_sgs
      type(spherical_transform_data), intent(inout) :: fwd_Csim
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'iak_sgs_term%i_SGS_buoyancy',                       &
     &        iak_sgs_term%i_SGS_buoyancy, f_trns_Csim%i_SGS_buoyancy
        write(*,*) 'iak_sgs_term%i_SGS_comp_buo',                       &
     &     iak_sgs_term%i_SGS_comp_buo, f_trns_Csim%i_SGS_comp_buo
      end if
!
      if(iak_sgs_term%i_SGS_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_buoyancy, iak_sgs_term%i_SGS_buoyancy,    &
     &      wk_sph_sgs, fwd_Csim)
      end if
!
      if(iak_sgs_term%i_SGS_comp_buo .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_comp_buo, iak_sgs_term%i_SGS_comp_buo,    &
     &      wk_sph_sgs, fwd_Csim)
      end if
!
      end subroutine copy_Csim_buo_4_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine copy_model_coefs_4_sph_snap(sph_rtp, sph_d_grp,        &
     &          iak_sgs_term, f_trns_Csim, wk_sph_sgs, fwd_Csim)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: f_trns_Csim
!
      type(SPH_dynamic_model_coefs), intent(inout) :: wk_sph_sgs
      type(spherical_transform_data), intent(inout) :: fwd_Csim
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'iak_sgs_term%i_SGS_m_flux',                         &
     &        iak_sgs_term%i_SGS_m_flux, f_trns_Csim%i_SGS_m_flux
        write(*,*) 'iak_sgs_term%i_SGS_Lorentz',                        &
     &        iak_sgs_term%i_SGS_Lorentz, f_trns_Csim%i_SGS_Lorentz
        write(*,*) 'iak_sgs_term%i_SGS_induction',                      &
     &        iak_sgs_term%i_SGS_induction, f_trns_Csim%i_SGS_vp_induct
        write(*,*) 'iak_sgs_term%i_SGS_h_flux',                         &
     &        iak_sgs_term%i_SGS_h_flux, f_trns_Csim%i_SGS_h_flux
        write(*,*) 'iak_sgs_term%i_SGS_c_flux',                         &
     &        iak_sgs_term%i_SGS_c_flux, f_trns_Csim%i_SGS_c_flux
        write(*,*) 'iak_sgs_term%i_SGS_buoyancy',                       &
     &        iak_sgs_term%i_SGS_buoyancy, f_trns_Csim%i_SGS_buoyancy
        write(*,*) 'iak_sgs_term%i_SGS_comp_buo',                       &
     &        iak_sgs_term%i_SGS_comp_buo, f_trns_Csim%i_SGS_comp_buo
      end if
!
      if(iak_sgs_term%i_SGS_m_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_m_flux, iak_sgs_term%i_SGS_m_flux,        &
     &      wk_sph_sgs, fwd_Csim)
      end if
!
      if(iak_sgs_term%i_SGS_Lorentz .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_Lorentz, iak_sgs_term%i_SGS_Lorentz,      &
     &      wk_sph_sgs, fwd_Csim)
      end if
!
      if(iak_sgs_term%i_SGS_induction .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &     f_trns_Csim%i_SGS_vp_induct, iak_sgs_term%i_SGS_induction,   &
     &     wk_sph_sgs, fwd_Csim)
      end if
!
      if(iak_sgs_term%i_SGS_h_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_h_flux, iak_sgs_term%i_SGS_h_flux,        &
     &      wk_sph_sgs, fwd_Csim)
      end if
!
      if(iak_sgs_term%i_SGS_c_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_c_flux, iak_sgs_term%i_SGS_c_flux,        &
     &      wk_sph_sgs, fwd_Csim)
      end if
!
!
      if(iak_sgs_term%i_SGS_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_buoyancy, iak_sgs_term%i_SGS_buoyancy,    &
     &      wk_sph_sgs, fwd_Csim)
      end if
!
      if(iak_sgs_term%i_SGS_comp_buo .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp, sph_d_grp,               &
     &      f_trns_Csim%i_SGS_comp_buo, iak_sgs_term%i_SGS_comp_buo,    &
     &      wk_sph_sgs, fwd_Csim)
      end if
!
      end subroutine copy_model_coefs_4_sph_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_model_coefs_sph_snap(sph_rtp, sph_d_grp,           &
     &          irtp_sgs, ifld_sgs, wk_sph_sgs, trns_fwd)
!
      use product_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      integer(kind = kint), intent(in) :: irtp_sgs
      integer(kind = kint), intent(in) :: ifld_sgs
!
      type(SPH_dynamic_model_coefs), intent(inout) :: wk_sph_sgs
      type(spherical_transform_data), intent(inout) :: trns_fwd
!
!
!$omp parallel workshare
      trns_fwd%fld_rtp(1:sph_rtp%nnod_rtp,irtp_sgs) = one
!$omp end parallel workshare
!
      call sel_product_model_coefs                                      &
     &   (one, sph_rtp, sph_d_grp, wk_sph_sgs%num_kinds, irtp_sgs,      &
     &    ifld_sgs, wk_sph_sgs, trns_fwd)
!
      end subroutine set_model_coefs_sph_snap
!
! ----------------------------------------------------------------------
!
      end module copy_Csim_4_sph_MHD
