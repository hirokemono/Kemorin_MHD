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
!!     &         (nnod_med, sph_rtp, stablize_weight, frc_rtp,          &
!!     &          ncomp_snap_rtp_2_rj, if_trns_reynolds, if_trns_buo_wk,&
!!     &          ifld_SGS_buo, icomp_SGS_buo, wk_sgs)
!!      subroutine sel_mag_sph_ave_SGS_buo_rtp                          &
!!     &         (sph_rtp, Cbuo_ave_sph_rtp, trns_SGS)
!!      subroutine prod_SGS_buoyancy_to_Reynolds(sph_rtp, fg_trns,      &
!!     &          ifld_sgs, wk_sgs, nnod_med, nc_SGS_rtp_2_rj, fSGS_rtp)
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
!
      implicit none
!
      private :: SGS_buoyancies_to_Reynolds_pin
      private :: SGS_buoyancies_to_Reynolds_pout
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_SGS_buo_coefs_sph_MHD                              &
     &         (nnod_med, sph_rtp, stablize_weight, frc_rtp,            &
     &          ncomp_snap_rtp_2_rj, if_trns_reynolds, if_trns_buo_wk,  &
     &          ifld_SGS_buo, icomp_SGS_buo, wk_sgs)
!
      use m_FFT_selector
      use zonal_lsq_4_model_coefs
      use cal_sph_model_coefs
!
      integer(kind = kint), intent(in) :: nnod_med
!
      real(kind = kreal), intent(in) :: stablize_weight
      integer(kind = kint), intent(in)  :: ncomp_snap_rtp_2_rj
      integer(kind = kint), intent(in)  :: if_trns_reynolds
      integer(kind = kint), intent(in)  :: if_trns_buo_wk
      integer(kind = kint), intent(in)  :: icomp_SGS_buo
      integer(kind = kint), intent(in)  :: ifld_SGS_buo
      type(sph_rtp_grid), intent(in) :: sph_rtp
      real(kind = kreal), intent(in)                                    &
     &               :: frc_rtp(sph_rtp%nnod_rtp,ncomp_snap_rtp_2_rj)
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
!
      if(ifld_SGS_buo .eq. 0) return
      call calypso_mpi_barrier
      call sel_int_zonal_for_buo_coefs                                  &
     &   (sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),              &
     &    frc_rtp(1,if_trns_reynolds), frc_rtp(1,if_trns_buo_wk),       &
     &    icomp_SGS_buo, wk_sgs%ntot_comp,                              &
     &    wk_sgs%comp_coef, wk_sgs%comp_clip)
      call calypso_mpi_barrier
!
      call sel_sph_model_coefs                                          &
     &   (ione, nnod_med, stablize_weight, ifld_SGS_buo, icomp_SGS_buo, &
     &    wk_sgs%num_kinds, wk_sgs%ntot_comp, wk_sgs%comp_coef,         &
     &    wk_sgs%comp_clip, wk_sgs%fld_coef)
!
      end subroutine cal_SGS_buo_coefs_sph_MHD
!
! ----------------------------------------------------------------------
!
      subroutine sel_mag_sph_ave_SGS_buo_rtp                            &
     &         (sph_rtp, Cbuo_ave_sph_rtp, trns_SGS)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      real(kind = kreal), intent(in)                                    &
     &                   :: Cbuo_ave_sph_rtp(sph_rtp%nidx_rtp(1),2)
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
!$omp parallel
      if(iflag_FFT .eq. iflag_FFTW) then
        call prod_dbl_radial_buo_coefs_pin                              &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      Cbuo_ave_sph_rtp(1,1), Cbuo_ave_sph_rtp(1,2),               &
     &      trns_SGS%frc_rtp(1,trns_SGS%f_trns%i_SGS_inertia))
      else
        call prod_dbl_radial_buo_coefs_pout                             &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      Cbuo_ave_sph_rtp(1,1), Cbuo_ave_sph_rtp(1,2),               &
     &      trns_SGS%frc_rtp(1,trns_SGS%f_trns%i_SGS_inertia))
      end if
!$omp end parallel
!
      end subroutine sel_mag_sph_ave_SGS_buo_rtp
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
      if(iflag_FFT .eq. iflag_FFTW) then
        call SGS_buoyancies_to_Reynolds_pin(sph_rtp, fg_trns,           &
     &          ifld_sgs, wk_sgs, nnod_med, nc_SGS_rtp_2_rj, fSGS_rtp)
      else
        call SGS_buoyancies_to_Reynolds_pout(sph_rtp, fg_trns,          &
     &          ifld_sgs, wk_sgs, nnod_med, nc_SGS_rtp_2_rj, fSGS_rtp)
      end if
!
      end subroutine prod_SGS_buoyancy_to_Reynolds
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SGS_buoyancies_to_Reynolds_pin(sph_rtp, fg_trns,       &
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
!$omp parallel
      if     (ifld_sgs%i_buoyancy .gt. 0                                &
     &  .and. ifld_sgs%i_comp_buoyancy .gt. 0) then
        call product_double_buo_coefs_pin                               &
     &     (sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),            &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_buoyancy),                     &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_comp_buoyancy),                &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia))
      else if(ifld_sgs%i_buoyancy .gt. 0) then
        call product_single_buo_coefs_pin                               &
     &     (sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),            &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_buoyancy),                     &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia))
      else if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call product_single_buo_coefs_pin                               &
     &     (sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),            &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_comp_buoyancy),                &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia))
      end if
!
!$omp end parallel
!
      end subroutine SGS_buoyancies_to_Reynolds_pin
!
! ----------------------------------------------------------------------
!
      subroutine SGS_buoyancies_to_Reynolds_pout(sph_rtp, fg_trns,      &
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
!$omp parallel
      if     (ifld_sgs%i_buoyancy .gt. 0                                &
     &  .and. ifld_sgs%i_comp_buoyancy .gt. 0) then
        call product_double_buo_coefs_pout                              &
     &     (sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),            &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_buoyancy),                     &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_comp_buoyancy),                &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia))
      else if(ifld_sgs%i_buoyancy .gt. 0) then
        call product_single_buo_coefs_pout                              &
     &     (sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),            &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_buoyancy),                     &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia))
      else if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call product_single_buo_coefs_pout                              &
     &     (sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),            &
     &      wk_sgs%fld_coef(1,ifld_sgs%i_comp_buoyancy),                &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia))
      end if
!$omp end parallel
!
      end subroutine SGS_buoyancies_to_Reynolds_pout
!
! ----------------------------------------------------------------------
!
      end module SGS_buo_coefs_sph_MHD 
 