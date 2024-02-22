!>@file   SGS_buo_coefs_sph_MHD .f90
!!@brief  module SGS_buo_coefs_sph_MHD 
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2016
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine cal_SGS_buo_coef_sph_MHD                             &
!!     &         (sph_rtp, sph_d_grp, stablize_weight, frc_rtp,         &
!!     &          ncomp_snap_rtp_2_rj, if_trns_reynolds, if_trns_buo_wk,&
!!     &          ifld_SGS_buo, icomp_SGS_buo, wk_sgs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: fg_trns
!!        type(spherical_transform_data), intent(inout) :: trns_f_SGS
!!      subroutine prod_SGS_buoyancy_to_Reynolds(sph_rtp, sph_d_grp,    &
!!     &          fg_trns_LES, iak_sgs_term, wk_sgs, trns_f_SGS)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!        type(SGS_model_addresses), intent(in) :: fg_trns_LES
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!
!!      subroutine magnify_sph_ave_SGS_buoyancy(sph_rtp, iak_sgs_term,  &
!!     &          wk_sgs_buo, fg_trns_LES, trns_f_SGS)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(work_4_sph_SGS_buoyancy), intent(in) :: wk_sgs_buo
!!        type(SGS_model_addresses), intent(in) :: fg_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_f_SGS
!!@endverbatim
!
      module SGS_buo_coefs_sph_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
      use m_FFT_selector
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_spheric_rtp_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_SGS_term_labels
      use t_addresses_sph_transform
      use t_ele_info_4_dynamic
      use t_groups_sph_dynamic
      use t_sph_filtering
!
      implicit none
!
      private :: sel_prod_sgl_radial_buo_coefs
      private :: sel_prod_dbl_radial_buo_coefs
!
      private :: sel_int_zonal_4_buo_coef
      private :: sel_product_single_buo_coefs
      private :: sel_product_double_buo_coefs
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine magnify_sph_ave_SGS_buoyancy(sph_rtp, iak_sgs_term,    &
     &          wk_sgs_buo, fg_trns_LES, trns_f_SGS)
!
      use t_rms_4_sph_spectr
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_buo_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(work_4_sph_SGS_buoyancy), intent(in) :: wk_sgs_buo
      type(SGS_model_addresses), intent(in) :: fg_trns_LES
!
      type(spherical_transform_data), intent(inout) :: trns_f_SGS
!
!
      if     (iak_sgs_term%i_SGS_buoyancy                               &
     &         * iak_sgs_term%i_SGS_comp_buo .gt. 0) then
        call sel_prod_dbl_radial_buo_coefs(sph_rtp,                     &
     &      wk_sgs_buo%Cbuo_ave_sph_rtp, fg_trns_LES, trns_f_SGS)
      else if(iak_sgs_term%i_SGS_buoyancy .gt. 0) then
        call sel_prod_sgl_radial_buo_coefs(sph_rtp,                     &
     &      wk_sgs_buo%Cbuo_ave_sph_rtp(1,1), fg_trns_LES, trns_f_SGS)
      else if(iak_sgs_term%i_SGS_comp_buo .gt. 0) then
        call sel_prod_sgl_radial_buo_coefs(sph_rtp,                     &
     &      wk_sgs_buo%Cbuo_ave_sph_rtp(1,2), fg_trns_LES, trns_f_SGS)
      end if
!
      end subroutine magnify_sph_ave_SGS_buoyancy
!
! ----------------------------------------------------------------------
!
      subroutine cal_SGS_buo_coef_sph_MHD                               &
     &         (sph_rtp, sph_d_grp, stablize_weight, frc_rtp,           &
     &          ncomp_snap_rtp_2_rj, if_trns_reynolds, if_trns_buo_wk,  &
     &          ifld_SGS_buo, icomp_SGS_buo, wk_sph_sgs)
!
      use m_FFT_selector
      use zonal_lsq_4_model_coefs
      use cal_sph_model_coefs
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: stablize_weight
      integer(kind = kint), intent(in)  :: ncomp_snap_rtp_2_rj
      integer(kind = kint), intent(in)  :: if_trns_reynolds
      integer(kind = kint), intent(in)  :: if_trns_buo_wk
      integer(kind = kint), intent(in)  :: icomp_SGS_buo
      integer(kind = kint), intent(in)  :: ifld_SGS_buo
      real(kind = kreal), intent(in)                                    &
     &               :: frc_rtp(sph_rtp%nnod_rtp,ncomp_snap_rtp_2_rj)
!
      type(SPH_dynamic_model_data), intent(inout) :: wk_sph_sgs
!
!
      call sel_int_zonal_4_buo_coef(sph_rtp, sph_d_grp,                 &
     &    frc_rtp(1,if_trns_reynolds), frc_rtp(1,if_trns_buo_wk),       &
     &    wk_sph_sgs%comp_coef(1,icomp_SGS_buo),                        &
     &    wk_sph_sgs%comp_clip(1,icomp_SGS_buo))
!
      call sel_sph_model_coef                                           &
     &   (ione, sph_d_grp%ngrp_dynamic,  stablize_weight,               &
     &    wk_sph_sgs%comp_coef(1,icomp_SGS_buo),                        &
     &    wk_sph_sgs%comp_clip(1,icomp_SGS_buo),                        &
     &    wk_sph_sgs%fld_coef(1,ifld_SGS_buo))
!
      end subroutine cal_SGS_buo_coef_sph_MHD
!
! ----------------------------------------------------------------------
!
      subroutine prod_SGS_buoyancy_to_Reynolds(sph_rtp, sph_d_grp,      &
     &          fg_trns_LES, iak_sgs_term, wk_sph_sgs, trns_f_SGS)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      type(SGS_model_addresses), intent(in) :: fg_trns_LES
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SPH_dynamic_model_data), intent(in) :: wk_sph_sgs
!
      type(spherical_transform_data), intent(inout) :: trns_f_SGS
!
!
      if     (iak_sgs_term%i_SGS_buoyancy                               &
     &          * iak_sgs_term%i_SGS_comp_buo .gt. 0) then
        call sel_product_double_buo_coefs                               &
     &     (sph_rtp, sph_d_grp, wk_sph_sgs%num_kinds,                   &
     &      iak_sgs_term%i_SGS_buoyancy, iak_sgs_term%i_SGS_comp_buo,   &
     &      wk_sph_sgs%fld_coef, trns_f_SGS%ncomp,                      &
     &      fg_trns_LES%SGS_term%i_SGS_inertia, trns_f_SGS%fld_rtp)
      else if(iak_sgs_term%i_SGS_buoyancy .gt. 0) then
        call sel_product_single_buo_coefs                               &
     &     (sph_rtp, sph_d_grp, wk_sph_sgs%num_kinds,                   &
     &      iak_sgs_term%i_SGS_buoyancy, wk_sph_sgs%fld_coef,           &
     &      trns_f_SGS%ncomp, fg_trns_LES%SGS_term%i_SGS_inertia,       &
     &      trns_f_SGS%fld_rtp)
      else if(iak_sgs_term%i_SGS_comp_buo .gt. 0) then
        call sel_product_single_buo_coefs                               &
     &     (sph_rtp, sph_d_grp, wk_sph_sgs%num_kinds,                   &
     &      iak_sgs_term%i_SGS_comp_buo, wk_sph_sgs%fld_coef,           &
     &      trns_f_SGS%ncomp, fg_trns_LES%SGS_term%i_SGS_inertia,       &
     &      trns_f_SGS%fld_rtp)
      end if
!
      end subroutine prod_SGS_buoyancy_to_Reynolds
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_int_zonal_4_buo_coef(sph_rtp, sph_d_grp,           &
     &          frc_simi, frc_wide, sgs_zl, sgs_zt)
!
      use zonal_int_4_sph_Csim_pin
      use zonal_int_4_sph_Csim_pout
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: frc_simi(sph_rtp%nnod_rtp)
      real(kind = kreal), intent(in) :: frc_wide(sph_rtp%nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(sph_d_grp%ngrp_dynamic)
!
!
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call int_zonal_buo_coefs_pin(sph_rtp, sph_d_grp,                &
     &      frc_simi, frc_wide, sgs_zl, sgs_zt)
      else
        call int_zonal_buo_coefs_rin(sph_rtp, sph_d_grp,                &
     &      frc_simi, frc_wide, sgs_zl, sgs_zt)
      end if
!
      end subroutine sel_int_zonal_4_buo_coef
!
! ----------------------------------------------------------------------
!
      subroutine sel_prod_sgl_radial_buo_coefs                          &
     &         (sph_rtp, sgs_c, fg_trns_LES, trns_f_SGS)
!
      use m_FFT_selector
      use prod_buo_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      real(kind = kreal), intent(in) :: sgs_c(sph_rtp%nidx_rtp(1))
      type(SGS_model_addresses), intent(in) :: fg_trns_LES
!
      type(spherical_transform_data), intent(inout) :: trns_f_SGS
!
!
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call prod_sgl_radial_buo_coefs_pin                              &
     &     (sph_rtp%nidx_rtp, sph_rtp%istep_rtp, sgs_c,                 &
     &      fg_trns_LES%SGS_term%i_SGS_inertia, sph_rtp%nnod_rtp,       &
     &      trns_f_SGS%ncomp, trns_f_SGS%fld_rtp)
      else
        call prod_sgl_radial_buo_coefs_rin                              &
     &     (sph_rtp%nidx_rtp, sph_rtp%istep_rtp, sgs_c,                 &
     &      fg_trns_LES%SGS_term%i_SGS_inertia, sph_rtp%nnod_rtp,       &
     &      trns_f_SGS%ncomp, trns_f_SGS%fld_rtp)
      end if
!
      end subroutine sel_prod_sgl_radial_buo_coefs
!
! ----------------------------------------------------------------------
!
      subroutine sel_prod_dbl_radial_buo_coefs                          &
     &         (sph_rtp, sgs_c, fg_trns_LES, trns_f_SGS)
!
      use m_FFT_selector
      use prod_buo_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      real(kind = kreal), intent(in) :: sgs_c(sph_rtp%nidx_rtp(1),2)
      type(SGS_model_addresses), intent(in) :: fg_trns_LES
!
      type(spherical_transform_data), intent(inout) :: trns_f_SGS
!
!
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call prod_dbl_radial_buo_coefs_pin                              &
     &     (sph_rtp%nidx_rtp, sph_rtp%istep_rtp, sgs_c,                 &
     &      fg_trns_LES%SGS_term%i_SGS_inertia, sph_rtp%nnod_rtp,       &
     &      trns_f_SGS%ncomp, trns_f_SGS%fld_rtp)
      else
        call prod_dbl_radial_buo_coefs_rin                              &
     &     (sph_rtp%nidx_rtp, sph_rtp%istep_rtp, sgs_c,                 &
     &      fg_trns_LES%SGS_term%i_SGS_inertia, sph_rtp%nnod_rtp,       &
     &      trns_f_SGS%ncomp, trns_f_SGS%fld_rtp)
      end if
!
      end subroutine sel_prod_dbl_radial_buo_coefs
!
! ----------------------------------------------------------------------
!
      subroutine sel_product_single_buo_coefs                           &
     &         (sph_rtp, sph_d_grp, nfld_sgs, isgs_buo,                 &
     &          fld_coef, nc_SGS_rtp_2_rj, i_SGS_inertia, fSGS_rtp)
!
      use m_FFT_selector
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      integer(kind = kint), intent(in) :: nfld_sgs, isgs_buo
      integer(kind = kint), intent(in) :: i_SGS_inertia
      integer(kind = kint), intent(in) :: nc_SGS_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: fld_coef(sph_d_grp%ngrp_dynamic,nfld_sgs)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: fSGS_rtp(sph_rtp%nnod_rtp,nc_SGS_rtp_2_rj)
!
!
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call product_single_buo_coefs_pin(sph_rtp, sph_d_grp,           &
     &      fld_coef(1,isgs_buo), fSGS_rtp(1,i_SGS_inertia))
      else
        call product_single_buo_coefs_rin(sph_rtp, sph_d_grp,           &
     &      fld_coef(1,isgs_buo), fSGS_rtp(1,i_SGS_inertia))
      end if
!
      end subroutine sel_product_single_buo_coefs
!
! ----------------------------------------------------------------------
!
      subroutine sel_product_double_buo_coefs                           &
     &         (sph_rtp, sph_d_grp, nfld_sgs, isgs_buo1, isgs_buo2,     &
     &          fld_coef, nc_SGS_rtp_2_rj, i_SGS_inertia, fSGS_rtp)
!
      use m_FFT_selector
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      integer(kind = kint), intent(in) :: nfld_sgs
      integer(kind = kint), intent(in) :: isgs_buo1, isgs_buo2
      integer(kind = kint), intent(in) :: i_SGS_inertia
      integer(kind = kint), intent(in) :: nc_SGS_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: fld_coef(sph_d_grp%ngrp_dynamic,nfld_sgs)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: fSGS_rtp(sph_rtp%nnod_rtp,nc_SGS_rtp_2_rj)
!
!
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call product_double_buo_coefs_pin(sph_rtp, sph_d_grp,           &
     &      fld_coef(1,isgs_buo1), fld_coef(1,isgs_buo2),               &
     &      fSGS_rtp(1,i_SGS_inertia))
      else
        call product_double_buo_coefs_rin(sph_rtp, sph_d_grp,           &
     &      fld_coef(1,isgs_buo1),  fld_coef(1,isgs_buo2),              &
     &      fSGS_rtp(1,i_SGS_inertia))
      end if
!
      end subroutine sel_product_double_buo_coefs
!
! ----------------------------------------------------------------------
!
      end module SGS_buo_coefs_sph_MHD
 
