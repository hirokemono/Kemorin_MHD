!>@file   SGS_buo_coefs_sph_MHD .f90
!!@brief  module SGS_buo_coefs_sph_MHD 
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2016
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine cal_SGS_buo_coefs_sph_MHD                            &
!!     &         (sph_rtp, sph_d_grp, stablize_weight, frc_rtp,         &
!!     &          ncomp_snap_rtp_2_rj, if_trns_reynolds, if_trns_buo_wk,&
!!     &          ifld_SGS_buo, icomp_SGS_buo, wk_sgs)
!!      subroutine sel_mag_sph_ave_SGS_buo_rtp                          &
!!     &         (sph_rtp, ifld_sgs, Cbuo_ave_sph_rtp, f_trns,          &
!!     &          ncomp_SGS_rtp_2_rj, frc_rtp)
!!      subroutine prod_SGS_buoyancy_to_Reynolds(sph_rtp, sph_d_grp,    &
!!     &          fg_trns, ifld_sgs, wk_sgs, nc_SGS_rtp_2_rj, fSGS_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!        type(phys_address), intent(in) :: fg_trns
!!        type(SGS_terms_address), intent(in) :: ifld_sgs
!!        type(dynamic_model_data), intent(in) :: wk_sgs
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
      use t_spheric_rtp_data
      use t_phys_address
      use t_addresses_sph_transform
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_groups_sph_dynamic
!
      implicit none
!
      private :: sel_product_single_buo_coefs
      private :: sel_product_double_buo_coefs
      private :: sel_prod_sgl_radial_buo_coefs
      private :: sel_prod_dbl_radial_buo_coefs
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_SGS_buo_coefs_sph_MHD                              &
     &         (sph_rtp, sph_d_grp, stablize_weight, frc_rtp,           &
     &          ncomp_snap_rtp_2_rj, if_trns_reynolds, if_trns_buo_wk,  &
     &          ifld_SGS_buo, icomp_SGS_buo, wk_sgs)
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
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
!
      call sel_int_zonal_4_buo_coefs(sph_rtp, sph_d_grp,                &
     &    frc_rtp(1,if_trns_reynolds), frc_rtp(1,if_trns_buo_wk),       &
     &    wk_sgs%comp_coef(1,icomp_SGS_buo),                            &
     &    wk_sgs%comp_clip(1,icomp_SGS_buo))
!
      call sel_sph_model_coefs(ione, sph_d_grp%ngrp_dynamic,            &
     &    stablize_weight, ifld_SGS_buo, icomp_SGS_buo,                 &
     &    wk_sgs%num_kinds, wk_sgs%ntot_comp, wk_sgs%comp_coef,         &
     &    wk_sgs%comp_clip, wk_sgs%fld_coef)
!
      end subroutine cal_SGS_buo_coefs_sph_MHD
!
! ----------------------------------------------------------------------
!
      subroutine sel_mag_sph_ave_SGS_buo_rtp                            &
     &         (sph_rtp, ifld_sgs, Cbuo_ave_sph_rtp, trns_SGS)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_terms_address), intent(in) :: ifld_sgs
      real(kind = kreal), intent(in)                                    &
     &                   :: Cbuo_ave_sph_rtp(sph_rtp%nidx_rtp(1),2)
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if     (ifld_sgs%i_buoyancy*ifld_sgs%i_comp_buoyancy .gt. 0) then
        call sel_prod_dbl_radial_buo_coefs                              &
     &     (sph_rtp, Cbuo_ave_sph_rtp, trns_SGS)
      else if(ifld_sgs%i_buoyancy .gt. 0) then
        call sel_prod_sgl_radial_buo_coefs                              &
     &     (sph_rtp, Cbuo_ave_sph_rtp(1,1), trns_SGS)
      else if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call sel_prod_sgl_radial_buo_coefs                              &
     &     (sph_rtp, Cbuo_ave_sph_rtp(1,2), trns_SGS)
      end if
!
      end subroutine sel_mag_sph_ave_SGS_buo_rtp
!
! ----------------------------------------------------------------------
!
      subroutine prod_SGS_buoyancy_to_Reynolds(sph_rtp, sph_d_grp,      &
     &          fg_trns, ifld_sgs, wk_sgs, nc_SGS_rtp_2_rj, fSGS_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      type(phys_address), intent(in) :: fg_trns
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(dynamic_model_data), intent(in) :: wk_sgs
      integer(kind = kint), intent(in) :: nc_SGS_rtp_2_rj
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: fSGS_rtp(sph_rtp%nnod_rtp,nc_SGS_rtp_2_rj)
!
!
      if     (ifld_sgs%i_buoyancy*ifld_sgs%i_comp_buoyancy .gt. 0) then
        call sel_product_double_buo_coefs                               &
     &     (sph_rtp, sph_d_grp, wk_sgs%num_kinds,                       &
     &      ifld_sgs%i_buoyancy, ifld_sgs%i_comp_buoyancy,              &
     &      wk_sgs%fld_coef, nc_SGS_rtp_2_rj, fg_trns%i_SGS_inertia,    &
     &      fSGS_rtp)
      else if(ifld_sgs%i_buoyancy .gt. 0) then
        call sel_product_single_buo_coefs                               &
     &     (sph_rtp, sph_d_grp, wk_sgs%num_kinds,                       &
     &      ifld_sgs%i_buoyancy, wk_sgs%fld_coef,                       &
     &      nc_SGS_rtp_2_rj, fg_trns%i_SGS_inertia, fSGS_rtp)
      else if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call sel_product_single_buo_coefs                               &
     &     (sph_rtp, sph_d_grp, wk_sgs%num_kinds,                       &
     &      ifld_sgs%i_comp_buoyancy, wk_sgs%fld_coef,                  &
     &      nc_SGS_rtp_2_rj, fg_trns%i_SGS_inertia, fSGS_rtp)
      end if
!
      end subroutine prod_SGS_buoyancy_to_Reynolds
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_prod_sgl_radial_buo_coefs                          &
     &         (sph_rtp, sgs_c, trns_SGS)
!
      use m_FFT_selector
      use prod_buo_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      real(kind = kreal), intent(in) :: sgs_c(sph_rtp%nidx_rtp(1))
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call prod_sgl_radial_buo_coefs_pin(sph_rtp%nidx_rtp, sgs_c,     &
     &      trns_SGS%f_trns%i_SGS_inertia, sph_rtp%nnod_rtp,            &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      else
        call prod_sgl_radial_buo_coefs_pout(sph_rtp%nidx_rtp, sgs_c,    &
     &      trns_SGS%f_trns%i_SGS_inertia, sph_rtp%nnod_rtp,            &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      end if
!
      end subroutine sel_prod_sgl_radial_buo_coefs
!
! ----------------------------------------------------------------------
!
      subroutine sel_prod_dbl_radial_buo_coefs                          &
     &         (sph_rtp, sgs_c, trns_SGS)
!
      use m_FFT_selector
      use prod_buo_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      real(kind = kreal), intent(in) :: sgs_c(sph_rtp%nidx_rtp(1),2)
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call prod_dbl_radial_buo_coefs_pin(sph_rtp%nidx_rtp, sgs_c,     &
     &      trns_SGS%f_trns%i_SGS_inertia, sph_rtp%nnod_rtp,            &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      else
        call prod_dbl_radial_buo_coefs_pout(sph_rtp%nidx_rtp, sgs_c,    &
     &      trns_SGS%f_trns%i_SGS_inertia, sph_rtp%nnod_rtp,            &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
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
      if(iflag_FFT .eq. iflag_FFTW) then
        call product_single_buo_coefs_pin(sph_rtp, sph_d_grp,           &
     &      fld_coef(1,isgs_buo), fSGS_rtp(1,i_SGS_inertia))
      else
        call product_single_buo_coefs_pout(sph_rtp, sph_d_grp,          &
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
      if(iflag_FFT .eq. iflag_FFTW) then
        call product_double_buo_coefs_pin(sph_rtp, sph_d_grp,           &
     &      fld_coef(1,isgs_buo1), fld_coef(1,isgs_buo2),               &
     &      fSGS_rtp(1,i_SGS_inertia))
      else
        call product_double_buo_coefs_pout(sph_rtp, sph_d_grp,          &
     &      fld_coef(1,isgs_buo1),  fld_coef(1,isgs_buo2),              &
     &      fSGS_rtp(1,i_SGS_inertia))
      end if
!
      end subroutine sel_product_double_buo_coefs
!
! ----------------------------------------------------------------------
!
      end module SGS_buo_coefs_sph_MHD
 