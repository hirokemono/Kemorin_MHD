!>@file   cal_SGS_terms_sph_MHD.f90
!!@brief  module cal_SGS_terms_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine filtered_nonlinear_in_rtp(SGS_param, sph_rtp,        &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop, b_trns, f_trns,   &
!!     &          ncomp_rj_2_rtp, ncomp_rtp_2_rj, fld_rtp, frc_rtp)
!!       Input ::  fld_rtp(1,ib_fld)
!!               ib_fld = i_filter_velo, i_filter_vort, i_filter_magne, 
!!                        i_filter_current, i_filter_temp, i_filter_comp
!!       Output :: frc_rtp(1,if_frc)
!!               if_frc = i_SGS_inertia, i_SGS_Lorentz,
!!                        i_SGS_vp_induct, i_SGS_h_flux, i_SGS_c_flux
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: b_trns, f_trns
!!        type(phys_address), intent(in) :: b_trns, f_trns
!!
!!      subroutine similarity_SGS_terms_rtp                             &
!!     &         (sph_rtp, f_trns, bg_trns, fg_trns,                    &
!!     &          nc_SGS_rj_2_rtp, ncomp_rtp_2_rj, nc_SGS_rtp_2_rj,     &
!!     &          fil_rtp, frc_rtp, fSGS_rtp)
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
!!
!!      subroutine wider_similarity_SGS_rtp                             &
!!     &         (sph_rtp, fl_prop, cd_prop, ht_prop, cp_prop,          &
!!     &          b_trns, bg_trns, ncomp_rj_2_rtp, nc_SGS_rj_2_rtp,     &
!!     &          fld_rtp, fil_rtp)
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
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
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
      use t_physical_property
      use t_spheric_rtp_data
      use t_phys_address
!
      implicit none
!
      private :: subcract_X_product_w_coef_smp
      private :: sub_vec_scalar_prod_w_coef_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
!>      Get @f$ e_{ijk} \overline{\tilde{\omega}}_{j}
!!                      \overline{\tilde{u}}_{k} @f$,
!>      @f$ e_{ijk}\overline{\tilde{J}}_{j}\overline{\tilde{B}}_{k} @f$,
!>      @f$ e_{ijk}\overline{\tilde{u}}_{j}\overline{\tilde{B}}_{k} @f$,
!>      @f$ \overline{\tilde{u}}_{i}\overline{\tilde{T}} @f$,
!>      @f$ \overline{\tilde{u}}_{i}\overline{\tilde{C}} @f$,
!!        from @f$ \overline{\tilde{u}}_{i} @f$,
!!      @f$ \overline{\tilde{\omega}_{i} @f$, 
!!      @f$ \overline{\tilde{B}}_{i} @f$, 
!!      @f$ \overline{\tilde{J}}_{i} @f$, 
!!      @f$ \overline{\tilde{T}} @f$, and @f$ \overline{\tilde{C}} @f$
      subroutine filtered_nonlinear_in_rtp(SGS_param, sph_rtp,          &
     &          fl_prop, cd_prop, ht_prop, cp_prop, b_trns, f_trns,     &
     &          ncomp_rj_2_rtp, ncomp_rtp_2_rj, fld_rtp, frc_rtp)
!
      use const_wz_coriolis_rtp
      use cal_products_smp
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: b_trns, f_trns
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: fld_rtp(sph_rtp%nnod_rtp,ncomp_rj_2_rtp)
      real(kind = kreal), intent(inout)                                 &
     &                   :: frc_rtp(sph_rtp%nnod_rtp,ncomp_rtp_2_rj)
!
!
!$omp parallel
      if( (f_trns%i_SGS_inertia * SGS_param%iflag_SGS_m_flux) .gt. 0)   &
     & then
        call cal_cross_prod_w_coef_smp                                  &
     &     (sph_rtp%nnod_rtp, fl_prop%coef_velo,                        &
     &      fld_rtp(1,b_trns%i_filter_vort),                            &
     &      fld_rtp(1,b_trns%i_filter_velo),                            &
     &      frc_rtp(1,f_trns%i_SGS_inertia) )
      end if
!
      if( (f_trns%i_SGS_Lorentz * SGS_param%iflag_SGS_lorentz) .gt. 0)  &
     & then
        call cal_cross_prod_w_coef_smp                                  &
     &     (sph_rtp%nnod_rtp, fl_prop%coef_lor,                         &
     &      fld_rtp(1,b_trns%i_filter_current),                         &
     &      fld_rtp(1,b_trns%i_filter_magne),                           &
     &      frc_rtp(1,f_trns%i_SGS_Lorentz) )
      end if
!
!
!
      if((f_trns%i_SGS_vp_induct * SGS_param%iflag_SGS_uxb) .gt. 0)     &
     & then
        call cal_cross_prod_w_coef_smp                                  &
     &     (sph_rtp%nnod_rtp, cd_prop%coef_induct,                      &
     &      fld_rtp(1,b_trns%i_filter_velo),                            &
     &      fld_rtp(1,b_trns%i_filter_magne),                           &
     &      frc_rtp(1,f_trns%i_SGS_vp_induct) )
      end if
!
!
      if( (f_trns%i_SGS_h_flux * SGS_param%iflag_SGS_h_flux) .gt. 0)    &
     & then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_rtp, ht_prop%coef_advect,                      &
     &      fld_rtp(1,b_trns%i_filter_velo),                            &
     &      fld_rtp(1,b_trns%i_filter_temp),                            &
     &      frc_rtp(1,f_trns%i_SGS_h_flux) )
      end if
!
      if( (f_trns%i_SGS_c_flux * SGS_param%iflag_SGS_c_flux) .gt. 0)    &
     & then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_rtp, cp_prop%coef_advect,                      &
     &      fld_rtp(1,b_trns%i_filter_velo),                            &
     &      fld_rtp(1,b_trns%i_filter_comp),                            &
     &      frc_rtp(1,f_trns%i_SGS_c_flux) )
      end if
!$omp end parallel
!
      end subroutine filtered_nonlinear_in_rtp
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
      subroutine similarity_SGS_terms_rtp                               &
     &         (sph_rtp, f_trns, bg_trns, fg_trns,                      &
     &          nc_SGS_rj_2_rtp, ncomp_rtp_2_rj, nc_SGS_rtp_2_rj,       &
     &          fil_rtp, frc_rtp, fSGS_rtp)
!
      use cal_subtract_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: f_trns
      type(phys_address), intent(in) :: bg_trns, fg_trns
!
      integer(kind = kint), intent(in) :: ncomp_rtp_2_rj
      integer(kind = kint), intent(in) :: nc_SGS_rj_2_rtp
      integer(kind = kint), intent(in) :: nc_SGS_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: frc_rtp(sph_rtp%nnod_rtp,ncomp_rtp_2_rj)
      real(kind = kreal), intent(in)                                    &
     &                   :: fil_rtp(sph_rtp%nnod_rtp,nc_SGS_rj_2_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: fSGS_rtp(sph_rtp%nnod_rtp,nc_SGS_rtp_2_rj)
!
!$omp parallel
      if(fg_trns%i_SGS_inertia .gt. 0) then
        call subtract_vectors_smp(sph_rtp%nnod_rtp,                     &
     &      fil_rtp(1,bg_trns%i_SGS_inertia),                           &
     &      frc_rtp(1,f_trns%i_SGS_inertia),                            &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia))
      end if
!
      if(fg_trns%i_SGS_Lorentz .gt. 0) then
        call subtract_vectors_smp(sph_rtp%nnod_rtp,                     &
     &      fil_rtp(1,bg_trns%i_SGS_Lorentz),                           &
     &      frc_rtp(1,f_trns%i_SGS_Lorentz),                            &
     &      fSGS_rtp(1,fg_trns%i_SGS_Lorentz))
      end if
!
      if(fg_trns%i_SGS_vp_induct .gt. 0) then
        call subtract_vectors_smp(sph_rtp%nnod_rtp,                     &
     &      fil_rtp(1,bg_trns%i_SGS_vp_induct),                         &
     &      frc_rtp(1,f_trns%i_SGS_vp_induct),                          &
     &      fSGS_rtp(1,fg_trns%i_SGS_vp_induct))
      end if
!
      if(fg_trns%i_SGS_h_flux .gt. 0) then
        call subtract_vectors_smp(sph_rtp%nnod_rtp,                     &
     &      fil_rtp(1,bg_trns%i_SGS_h_flux),                            &
     &      frc_rtp(1,f_trns%i_SGS_h_flux),                             &
     &      fSGS_rtp(1,fg_trns%i_SGS_h_flux))
      end if
!
      if(fg_trns%i_SGS_c_flux .gt. 0) then
        call subtract_vectors_smp(sph_rtp%nnod_rtp,                     &
     &      fil_rtp(1,bg_trns%i_SGS_c_flux),                            &
     &      frc_rtp(1,f_trns%i_SGS_c_flux),                             &
     &      fSGS_rtp(1,fg_trns%i_SGS_c_flux))
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
     &         (sph_rtp, fl_prop, cd_prop, ht_prop, cp_prop,            &
     &          b_trns, bg_trns, ncomp_rj_2_rtp, nc_SGS_rj_2_rtp,       &
     &          fld_rtp, fil_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: b_trns
      type(phys_address), intent(in) :: bg_trns
!
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: nc_SGS_rj_2_rtp
      real(kind = kreal), intent(in)                                    &
     &                   :: fld_rtp(sph_rtp%nnod_rtp,ncomp_rj_2_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: fil_rtp(sph_rtp%nnod_rtp,nc_SGS_rj_2_rtp)
!
!
!$omp parallel
      if(bg_trns%i_wide_SGS_inertia .gt. 0) then
        call subcract_X_product_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, fl_prop%coef_velo,                        &
     &      fld_rtp(1,b_trns%i_wide_fil_vort),                          &
     &      fld_rtp(1,b_trns%i_wide_fil_velo),                          &
     &      fil_rtp(1,bg_trns%i_wide_SGS_inertia))
      end if
!
      if(bg_trns%i_wide_SGS_Lorentz .gt. 0) then
        call subcract_X_product_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, fl_prop%coef_lor,                         &
     &      fld_rtp(1,b_trns%i_wide_fil_current),                       &
     &      fld_rtp(1,b_trns%i_wide_fil_magne),                         &
     &      fil_rtp(1,bg_trns%i_wide_SGS_Lorentz))
      end if
!
      if(bg_trns%i_wide_SGS_vp_induct .gt. 0) then
        call subcract_X_product_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, cd_prop%coef_induct,                      &
     &      fld_rtp(1,b_trns%i_wide_fil_velo),                          &
     &      fld_rtp(1,b_trns%i_wide_fil_magne),                         &
     &      fil_rtp(1,bg_trns%i_wide_SGS_vp_induct))
      end if
!
      if(bg_trns%i_wide_SGS_h_flux .gt. 0) then
        call sub_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_rtp, ht_prop%coef_advect,                      &
     &      fld_rtp(1,b_trns%i_wide_fil_velo),                          &
     &      fld_rtp(1,b_trns%i_wide_fil_temp),                          &
     &      fil_rtp(1,bg_trns%i_wide_SGS_h_flux))
      end if
!
      if(bg_trns%i_wide_SGS_c_flux .gt. 0) then
        call sub_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_rtp, cp_prop%coef_advect,                      &
     &      fld_rtp(1,b_trns%i_wide_fil_velo),                          &
     &      fld_rtp(1,b_trns%i_wide_fil_comp),                          &
     &      fil_rtp(1,bg_trns%i_wide_SGS_c_flux))
      end if
!$omp end parallel
!
      end subroutine wider_similarity_SGS_rtp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine subcract_X_product_w_coef_smp                          &
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
      end subroutine subcract_X_product_w_coef_smp
!
! ----------------------------------------------------------------------
!
      subroutine sub_vec_scalar_prod_w_coef_smp                         &
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
      end subroutine sub_vec_scalar_prod_w_coef_smp
!
! ----------------------------------------------------------------------
!
      end module cal_SGS_terms_sph_MHD
