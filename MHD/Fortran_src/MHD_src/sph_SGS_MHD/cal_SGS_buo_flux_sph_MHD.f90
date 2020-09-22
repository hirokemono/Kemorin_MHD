!>@file   cal_SGS_buo_flux_sph_MHD.f90
!!@brief  module cal_SGS_buo_flux_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine SGS_fluxes_for_buo_coefs(sph_rtp, fl_prop,           &
!!     &          b_trns_base, fg_trns_SGS, fs_trns_sef,                &
!!     &          trns_b_MHD, trns_f_SGS, trns_f_DYNS)
!!      subroutine SGS_fluxes_for_snapshot(sph_rtp, fl_prop,            &
!!     &          b_trns_base, fg_trns_SGS, bs_trns_SGS, fs_trns_sef,   &
!!     &          trns_b_snap, trns_f_SGS, trns_bs_SGS, trns_f_snap)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(base_field_address), intent(in) :: b_trns_base
!!        type(SGS_term_address), intent(in) :: fg_trns_SGS
!!        type(SGS_term_address), intent(in) :: bs_trns_SGS
!!        type(SGS_ene_flux_address), intent(in) :: fs_trns_sef
!!        type(spherical_transform_data), intent(in) :: trns_b_MHD
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(in) :: trns_f_SGS
!!        type(spherical_transform_data), intent(in) :: trns_bs_SGS
!!        type(spherical_transform_data), intent(inout) :: trns_f_snap
!!@endverbatim
!
      module cal_SGS_buo_flux_sph_MHD
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_physical_property
      use t_spheric_rtp_data
      use t_base_field_labels
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_addresses_sph_transform
!
      implicit none
!
      private :: sel_SGS_buoyancy_flux_rtp
      private :: cal_buoyancy_flux_rtp_pin,  cal_buoyancy_flux_rtp_rin
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine SGS_fluxes_for_buo_coefs(sph_rtp, fl_prop,             &
     &          b_trns_base, fg_trns_SGS, fs_trns_sef,                  &
     &          trns_b_MHD, trns_f_SGS, trns_f_DYNS)
!
      use cal_products_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(base_field_address), intent(in) :: b_trns_base
      type(SGS_term_address), intent(in) :: fg_trns_SGS
      type(SGS_ene_flux_address), intent(in) :: fs_trns_sef
!
      type(spherical_transform_data), intent(in) :: trns_b_MHD
      type(spherical_transform_data), intent(in) :: trns_f_SGS
!
      type(spherical_transform_data), intent(inout) :: trns_f_DYNS
!
!
!$omp parallel
      call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                   &
     &    trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_inertia),              &
     &    trns_b_MHD%fld_rtp(1,b_trns_base%i_velo),                     &
     &    trns_f_DYNS%fld_rtp(1,fs_trns_sef%i_reynolds_wk))
!$omp end parallel
!
      if(fl_prop%iflag_4_gravity) then
        call sel_SGS_buoyancy_flux_rtp                                  &
     &    (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, sph_rtp%istep_rtp,       &
     &     sph_rtp%radius_1d_rtp_r, fl_prop%coef_buo,                   &
     &     trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_h_flux),              &
     &     trns_f_DYNS%fld_rtp(1,fs_trns_sef%i_SGS_buo_wk))
      end if
      if(fl_prop%iflag_4_composit_buo) then
        call sel_SGS_buoyancy_flux_rtp                                  &
     &    (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, sph_rtp%istep_rtp,       &
     &   sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,                &
     &   trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_c_flux),                &
     &   trns_f_DYNS%fld_rtp(1,fs_trns_sef%i_SGS_comp_buo_wk))
      end if
!
      end subroutine SGS_fluxes_for_buo_coefs
!
!-----------------------------------------------------------------------
!
      subroutine SGS_fluxes_for_snapshot(sph_rtp, fl_prop,              &
     &          b_trns_base, fg_trns_SGS, bs_trns_SGS, fs_trns_sef,     &
     &          trns_b_snap, trns_f_SGS, trns_bs_SGS, trns_f_snap)
!
      use cal_products_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(base_field_address), intent(in) :: b_trns_base
      type(SGS_term_address), intent(in) :: fg_trns_SGS
      type(SGS_term_address), intent(in) :: bs_trns_SGS
      type(SGS_ene_flux_address), intent(in) :: fs_trns_sef
!
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_f_SGS
      type(spherical_transform_data), intent(in) :: trns_bs_SGS
!
      type(spherical_transform_data), intent(inout) :: trns_f_snap
!
!
!$omp parallel
      if(fs_trns_sef%i_reynolds_wk .gt. 0) then
        call cal_dot_prod_w_coef_smp(sph_rtp%nnod_rtp, dminus,          &
     &      trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_inertia),            &
     &      trns_b_snap%fld_rtp(1,b_trns_base%i_velo),                  &
     &      trns_f_snap%fld_rtp(1,fs_trns_sef%i_reynolds_wk))
      end if
!
      if(fs_trns_sef%i_SGS_Lor_wk .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_Lorentz),            &
     &      trns_b_snap%fld_rtp(1,b_trns_base%i_velo),                  &
     &      trns_f_snap%fld_rtp(1,fs_trns_sef%i_SGS_Lor_wk))
      end if
!
      if(fs_trns_sef%i_SGS_me_gen .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      trns_bs_SGS%fld_rtp(1,bs_trns_SGS%i_SGS_induction),         &
     &      trns_b_snap%fld_rtp(1,b_trns_base%i_magne),                 &
     &      trns_f_snap%fld_rtp(1,fs_trns_sef%i_SGS_me_gen))
      end if
!$omp end parallel
!
!$omp parallel
      if(fs_trns_sef%i_SGS_buo_wk .gt. 0) then
        call cal_buoyancy_flux_rtp_rin                                  &
     &    (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, sph_rtp%istep_rtp,       &
     &     sph_rtp%radius_1d_rtp_r, fl_prop%coef_buo,                   &
     &     trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_h_flux),              &
     &     trns_f_snap%fld_rtp(1,fs_trns_sef%i_SGS_buo_wk))
      end if
      if(fs_trns_sef%i_SGS_comp_buo_wk .gt. 0) then
        call cal_buoyancy_flux_rtp_rin                                  &
     &    (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, sph_rtp%istep_rtp,       &
     &   sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,                &
     &   trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_c_flux),                &
     &   trns_f_snap%fld_rtp(1,fs_trns_sef%i_SGS_comp_buo_wk))
      end if
!$omp end parallel
!
      end subroutine SGS_fluxes_for_snapshot
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_SGS_buoyancy_flux_rtp                              &
     &         (nnod_rtp, nidx_rtp, istep_rtp, radius,                  &
     &          coef, frc_hf, frc_buo)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: istep_rtp(3)
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: radius(nidx_rtp(1))
      real(kind = kreal), intent(in) :: frc_hf(nnod_rtp)
      real(kind = kreal), intent(inout) :: frc_buo(nnod_rtp)
!
!
!$omp parallel
      if(istep_rtp(3) .eq. 1) then
        call cal_buoyancy_flux_rtp_pin                                  &
     &     (nnod_rtp, nidx_rtp, istep_rtp, radius,                      &
     &      coef, frc_hf, frc_buo)
      else
        call cal_buoyancy_flux_rtp_rin                                  &
     &     (nnod_rtp, nidx_rtp, istep_rtp, radius,                      &
     &      coef, frc_hf, frc_buo)
      end if
!$omp end parallel
!
      end subroutine sel_SGS_buoyancy_flux_rtp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_buoyancy_flux_rtp_pin                              &
     &         (nnod_rtp, nidx_rtp, istep_rtp, radius,                  &
     &          coef, frc_hf, frc_buo)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: istep_rtp(3)
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: radius(nidx_rtp(1))
      real(kind = kreal), intent(in) :: frc_hf(nnod_rtp)
      real(kind = kreal), intent(inout) :: frc_buo(nnod_rtp)
!
      integer(kind = kint) :: k, l, m, i1
!
!
!$omp do private(k,l,m,i1)
      do l = 1, nidx_rtp(2)
        do k = 1, nidx_rtp(1)
          do m = 1, nidx_rtp(3)
            i1 = m + (k-1) * istep_rtp(1) + (l-1) * istep_rtp(2)
            frc_buo(i1) = coef * frc_hf(i1) * radius(k)
          end do
        end do
      end do
!$omp end do
!
      end subroutine cal_buoyancy_flux_rtp_pin
!
!  ---------------------------------------------------------------------
!
      subroutine cal_buoyancy_flux_rtp_rin                              &
     &         (nnod_rtp, nidx_rtp, istep_rtp, radius,                  &
     &          coef, frc_hf, frc_buo)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: istep_rtp(3)
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: radius(nidx_rtp(1))
      real(kind = kreal), intent(in) :: frc_hf(nnod_rtp)
      real(kind = kreal), intent(inout) :: frc_buo(nnod_rtp)
!
      integer(kind = kint) :: k, l, m, i1
!
!
      do m = 1, nidx_rtp(3)
!$omp do private(k,l,i1)
        do l = 1, nidx_rtp(2)
          do k = 1, nidx_rtp(1)
            i1 = k + (l-1) * istep_rtp(2) + (m-1) * istep_rtp(3)
            frc_buo(i1) = coef * frc_hf(i1) * radius(k)
          end do
        end do
!$omp end do
      end do
 !
      end subroutine cal_buoyancy_flux_rtp_rin
!
!  ---------------------------------------------------------------------
!
      end module cal_SGS_buo_flux_sph_MHD
