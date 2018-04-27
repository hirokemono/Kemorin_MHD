!>@file   cal_SGS_terms_sph_MHD.f90
!!@brief  module cal_SGS_terms_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine similarity_SGS_terms_rtp(sph_rtp, MHD_prop,          &
!!     &          bg_trns, fg_trns, trns_b_SGS, trns_f_SGS)
!!       Input ::  fil_rtp(1,il_frc)
!!               il_frc = i_SGS_inertia, i_SGS_Lorentz,
!!                        i_SGS_vp_induct, i_SGS_h_flux, i_SGS_c_flux
!!                 frc_rtp(1,if_frc)
!!               if_frc = i_SGS_inertia, i_SGS_Lorentz,
!!                        i_SGS_vp_induct, i_SGS_h_flux, i_SGS_c_flux
!!       Output ::  fSGS_rtp(1,ig_frc)
!!               ig_frc = i_SGS_inertia, i_SGS_Lorentz,
!!                        i_SGS_vp_induct, i_SGS_h_flux, i_SGS_c_flux
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: f_trns
!!        type(phys_address), intent(in) :: bg_trns, fg_trns
!!        type(address_each_sph_trans), intent(inout) :: trns_b_SGS
!!        type(address_each_sph_trans), intent(inout) :: trns_f_SGS
!!
!!      subroutine wider_similarity_SGS_rtp                             &
!!     &         (sph_rtp, MHD_prop, bd_trns, trns_b_DYNS)
!!       Input ::  fil_rtp(1,il_frc)
!!               il_frc = i_wide_SGS_inertia, i_wide_SGS_Lorentz,
!!                        i_wide_SGS_vp_induct, i_wide_SGS_h_flux, 
!!                        i_wide_SGS_c_flux
!!                 fld_rtp(1,ib_frc)
!!               ib_frc = i_wide_fil_velo, i_wide_fil_vort,
!!                        i_wide_fil_magne, i_wide_fil_current
!!                        i_wide_fil_temp, i_wide_fil_comp
!!       Output ::  fil_rtp(1,il_frc)
!!               il_frc = i_wide_SGS_inertia, i_wide_SGS_Lorentz,
!!                        i_wide_SGS_vp_induct, i_wide_SGS_h_flux, 
!!                        i_wide_SGS_c_flux
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_address), intent(in) :: b_trns
!!        type(phys_address), intent(in) :: bg_trns
!!@endverbatim
!
      module cal_SGS_terms_sph_MHD
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_SGS_control_parameter
      use t_control_parameter
      use t_spheric_rtp_data
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
      private :: subcract_X_product_w_coef_smp
      private :: subtract_scl_flux_w_coef_smp
      private :: overwrt_sub_X_prod_w_coef_smp
      private :: overwrt_sub_scl_flux_w_coef_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
!>      Get @f$ e_{ijk} \overline{\tilde{\omega}_{j}\tilde{u}_{k}}
!!             - e_{ijk} \overline{\tilde{\omega}}_{j}
!!                      \overline{\tilde{u}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\tilde{J}_{j} \tilde{B}_{k}}
!!         - e_{ijk}\overline{\tilde{J}}_{j}\overline{\tilde{B}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\tilde{u}_{j} \tilde{B}_{k}}
!!         - e_{ijk}\overline{\tilde{u}}_{j}\overline{\tilde{B}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\tilde{u}_{j} \tilde{T}}
!!         - e_{ijk}\overline{\tilde{u}}_{j}\overline{\tilde{T}} @f$, and
!!      @f$ e_{ijk}\overline{\tilde{u}_{j} \tilde{C}}
!!         - e_{ijk}\overline{\tilde{u}}_{j}\overline{\tilde{C}} @f$,
      subroutine similarity_SGS_terms_rtp(sph_rtp, MHD_prop,            &
     &          bg_trns, fg_trns, trns_b_SGS, trns_f_SGS)
!
      use cal_subtract_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: bg_trns, fg_trns
      type(address_each_sph_trans), intent(inout) :: trns_b_SGS
!
      type(address_each_sph_trans), intent(inout) :: trns_f_SGS
!
!$omp parallel
      if(fg_trns%i_SGS_inertia .gt. 0) then
        call subcract_X_product_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, MHD_prop%fl_prop%coef_velo,               &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_SGS_inertia),                &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_filter_vort),                &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_filter_velo),                &
     &      trns_f_SGS%fld_rtp(1,fg_trns%i_SGS_inertia))
      end if
!
      if(fg_trns%i_SGS_Lorentz .gt. 0) then
        call subcract_X_product_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, MHD_prop%fl_prop%coef_lor,                &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_SGS_Lorentz),                &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_filter_current),             &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_filter_magne),               &
     &      trns_f_SGS%fld_rtp(1,fg_trns%i_SGS_Lorentz))
      end if
!
      if(fg_trns%i_SGS_vp_induct .gt. 0) then
        call subcract_X_product_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, MHD_prop%cd_prop%coef_induct,             &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_SGS_vp_induct),              &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_filter_velo),                &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_filter_magne),               &
     &      trns_f_SGS%fld_rtp(1,fg_trns%i_SGS_vp_induct))
      end if
!
      if(fg_trns%i_SGS_h_flux .gt. 0) then
        call subtract_scl_flux_w_coef_smp                               &
     &     (sph_rtp%nnod_rtp, MHD_prop%ht_prop%coef_advect,             &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_SGS_h_flux),                 &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_filter_velo),                &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_filter_temp),                &
     &      trns_f_SGS%fld_rtp(1,fg_trns%i_SGS_h_flux))
      end if
!
      if(fg_trns%i_SGS_c_flux .gt. 0) then
        call subtract_scl_flux_w_coef_smp                               &
     &     (sph_rtp%nnod_rtp, MHD_prop%cp_prop%coef_advect,             &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_SGS_c_flux),                 &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_filter_velo),                &
     &      trns_b_SGS%fld_rtp(1,bg_trns%i_filter_temp),                &
     &      trns_f_SGS%fld_rtp(1,fg_trns%i_SGS_c_flux))
      end if
!$omp end parallel
!
      end subroutine similarity_SGS_terms_rtp
!
!-----------------------------------------------------------------------
!
!>      Get 
!!      @f$ e_{ijk} \overline{\overline{\tilde{\omega}_{j}\tilde{u}_{k}}}
!!         - e_{ijk} \overline{\overline{\tilde{\omega}}}_{j}
!!                  \overline{\overline{\tilde{u}}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\overline{\tilde{J}_{j} \tilde{B}_{k}}}
!!         - e_{ijk} \overline{\overline{\tilde{J}}}_{j}
!!                  \overline{\overline{\tilde{B}}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\overline{\tilde{u}_{j} \tilde{B}_{k}}}
!!         - e_{ijk} \overline{\overline{\tilde{u}}}_{j}
!!                  \overline{\overline{\tilde{B}}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\overline{\tilde{u}_{j} \tilde{T}}}
!!         - e_{ijk} \overline{\overline{\tilde{u}}}_{j}
!!                  \overline{\overline{\tilde{T}}} @f$, and
!!      @f$ e_{ijk}\overline{\overline{\tilde{u}_{j} \tilde{C}}}
!!         - e_{ijk} \overline{\overline{\tilde{u}}}_{j}
!!                  \overline{\overline{\tilde{C}}} @f$,
      subroutine wider_similarity_SGS_rtp                               &
     &         (sph_rtp, MHD_prop, bd_trns, trns_b_DYNS)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: bd_trns
!
      type(address_each_sph_trans), intent(inout) :: trns_b_DYNS
!
!
!$omp parallel
      if(bd_trns%i_wide_SGS_inertia .gt. 0) then
        call overwrt_sub_X_prod_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, MHD_prop%fl_prop%coef_velo,               &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_fil_vort),             &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_fil_velo),             &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_SGS_inertia))
      end if
!
      if(bd_trns%i_wide_SGS_Lorentz .gt. 0) then
        call overwrt_sub_X_prod_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, MHD_prop%fl_prop%coef_lor,                &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_fil_current),          &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_fil_magne),            &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_SGS_Lorentz))
      end if
!
      if(bd_trns%i_wide_SGS_vp_induct .gt. 0) then
        call overwrt_sub_X_prod_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, MHD_prop%cd_prop%coef_induct,             &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_fil_velo),             &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_fil_magne),            &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_SGS_vp_induct))
      end if
!
      if(bd_trns%i_wide_SGS_h_flux .gt. 0) then
        call overwrt_sub_scl_flux_w_coef_smp                            &
     &     (sph_rtp%nnod_rtp, MHD_prop%ht_prop%coef_advect,             &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_fil_velo),             &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_fil_temp),             &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_SGS_h_flux))
      end if
!
      if(bd_trns%i_wide_SGS_c_flux .gt. 0) then
        call overwrt_sub_scl_flux_w_coef_smp                            &
     &     (sph_rtp%nnod_rtp, MHD_prop%cp_prop%coef_advect,             &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_fil_velo),             &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_fil_comp),             &
     &      trns_b_DYNS%fld_rtp(1,bd_trns%i_wide_SGS_c_flux))
      end if
!$omp end parallel
!
      end subroutine wider_similarity_SGS_rtp
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine subcract_X_product_w_coef_smp                          &
     &         (numnod, coef, force, vect1, vect2, sgs_X)
!
      integer (kind=kint), intent(in) :: numnod
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: force(numnod,3)
      real (kind=kreal), intent(in) :: vect1(numnod,3), vect2(numnod,3)
!
      real (kind=kreal), intent(inout) :: sgs_X(numnod,3)
!
!
!$omp workshare
      sgs_X(1:numnod,1) = force(1:numnod,1)                             &
     &                 - (vect1(1:numnod,2)*vect2(1:numnod,3)           &
     &                  - vect1(1:numnod,3)*vect2(1:numnod,2)) * coef
      sgs_X(1:numnod,2) = force(1:numnod,2)                             &
     &                 - (vect1(1:numnod,3)*vect2(1:numnod,1)           &
     &                  - vect1(1:numnod,1)*vect2(1:numnod,3)) * coef
      sgs_X(1:numnod,3) = force(1:numnod,3)                             &
     &                 - (vect1(1:numnod,1)*vect2(1:numnod,2)           &
     &                  - vect1(1:numnod,2)*vect2(1:numnod,1)) * coef
!$omp end workshare nowait
!
      end subroutine subcract_X_product_w_coef_smp
!
!-----------------------------------------------------------------------
!
      subroutine subtract_scl_flux_w_coef_smp                           &
     &         (numnod, coef, flux, vect1, scalar, sgs_X)
!
      integer (kind=kint), intent(in) :: numnod
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: flux(numnod,3)
      real (kind=kreal), intent(in) :: vect1(numnod,3), scalar(numnod)
!
      real (kind=kreal), intent(inout) :: sgs_X(numnod,3)
!
!
!$omp workshare
      sgs_X(1:numnod,1) = flux(1:numnod,1)                              &
     &                 -  vect1(1:numnod,1)*scalar(1:numnod) * coef
      sgs_X(1:numnod,2) = flux(1:numnod,2)                              &
     &                 -  vect1(1:numnod,2)*scalar(1:numnod) * coef
      sgs_X(1:numnod,3) = flux(1:numnod,3)                              &
     &                 -  vect1(1:numnod,3)*scalar(1:numnod) * coef
!$omp end workshare nowait
!
      end subroutine subtract_scl_flux_w_coef_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine overwrt_sub_X_prod_w_coef_smp                          &
     &         (numnod, coef, vect1, vect2, sgs_X)
!
      integer (kind=kint), intent(in) :: numnod
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vect1(numnod,3), vect2(numnod,3)
!
      real (kind=kreal), intent(inout) :: sgs_X(numnod,3)
!
!
!$omp workshare
      sgs_X(1:numnod,1) = sgs_X(1:numnod,1)                             &
     &                 - (vect1(1:numnod,2)*vect2(1:numnod,3)           &
     &                  - vect1(1:numnod,3)*vect2(1:numnod,2)) * coef
      sgs_X(1:numnod,2) = sgs_X(1:numnod,2)                             &
     &                 - (vect1(1:numnod,3)*vect2(1:numnod,1)           &
     &                  - vect1(1:numnod,1)*vect2(1:numnod,3)) * coef
      sgs_X(1:numnod,3) = sgs_X(1:numnod,3)                             &
     &                 - (vect1(1:numnod,1)*vect2(1:numnod,2)           &
     &                  - vect1(1:numnod,2)*vect2(1:numnod,1)) * coef
!$omp end workshare nowait
!
      end subroutine overwrt_sub_X_prod_w_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine overwrt_sub_scl_flux_w_coef_smp                        &
     &         (numnod, coef, vect1, scalar, sgs_X)
!
      integer (kind=kint), intent(in) :: numnod
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: vect1(numnod,3), scalar(numnod)
!
      real (kind=kreal), intent(inout) :: sgs_X(numnod,3)
!
!
!$omp workshare
      sgs_X(1:numnod,1) = sgs_X(1:numnod,1)                             &
     &                 -  vect1(1:numnod,1)*scalar(1:numnod) * coef
      sgs_X(1:numnod,2) = sgs_X(1:numnod,2)                             &
     &                 -  vect1(1:numnod,2)*scalar(1:numnod) * coef
      sgs_X(1:numnod,3) = sgs_X(1:numnod,3)                             &
     &                 -  vect1(1:numnod,3)*scalar(1:numnod) * coef
!$omp end workshare nowait
!
      end subroutine overwrt_sub_scl_flux_w_coef_smp
!
! ----------------------------------------------------------------------
!
      end module cal_SGS_terms_sph_MHD
