!>@file   cal_SGS_terms_sph_MHD.f90
!!@brief  module cal_SGS_terms_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine filtered_nonlinear_in_rtp(sph_rtp, b_trns, f_trns,   &
!!     &          ncomp_rj_2_rtp, ncomp_rtp_2_rj, fld_rtp, frc_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: b_trns, f_trns
!!
!!      subroutine similarity_SGS_terms_rtp                             &
!!     &         (sph_rtp, f_trns, bg_trns, fg_trns,                    &
!!     &          ncomp_rtp_2_rj, nc_SGS_rj_2_rtp, nc_SGS_rtp_2_rj,     &
!!     &          frc_rtp, fil_rtp, fSGS_rtp)
!!      subroutine wider_similarity_SGS_rtp(sph_rtp, b_trns, bg_trns,   &
!!     &          ncomp_rj_2_rtp, nc_SGS_rj_2_rtp, fld_rtp, fil_rtp)
!!@endverbatim
!
      module cal_SGS_terms_sph_MHD
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_control_parameter
      use m_physical_property
!
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
      subroutine filtered_nonlinear_in_rtp(sph_rtp, b_trns, f_trns,     &
     &          ncomp_rj_2_rtp, ncomp_rtp_2_rj, fld_rtp, frc_rtp)
!
      use const_wz_coriolis_rtp
      use cal_products_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
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
      if( (f_trns%i_SGS_inertia*iflag_SGS_inertia) .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (sph_rtp%nnod_rtp, fl_prop1%coef_velo,                       &
     &      fld_rtp(1,b_trns%i_filter_vort),                            &
     &      fld_rtp(1,b_trns%i_filter_velo),                            &
     &      frc_rtp(1,f_trns%i_SGS_inertia) )
      end if
!
      if( (f_trns%i_SGS_Lorentz*iflag_SGS_lorentz) .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (sph_rtp%nnod_rtp, fl_prop1%coef_lor,                        &
     &      fld_rtp(1,b_trns%i_filter_current),                         &
     &      fld_rtp(1,b_trns%i_filter_magne),                           &
     &      frc_rtp(1,f_trns%i_SGS_Lorentz) )
      end if
!
!
!
      if( (f_trns%i_SGS_vp_induct*iflag_SGS_induction) .gt. 0) then
        call cal_cross_prod_w_coef_smp                                  &
     &     (sph_rtp%nnod_rtp, cd_prop1%coef_induct,                     &
     &      fld_rtp(1,b_trns%i_filter_velo),                            &
     &      fld_rtp(1,b_trns%i_filter_magne),                           &
     &      frc_rtp(1,f_trns%i_SGS_vp_induct) )
      end if
!
!
      if( (f_trns%i_SGS_h_flux*iflag_SGS_heat) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_rtp, ht_prop1%coef_advect,                     &
     &      fld_rtp(1,b_trns%i_filter_velo),                            &
     &      fld_rtp(1,b_trns%i_filter_temp),                            &
     &      frc_rtp(1,f_trns%i_SGS_h_flux) )
      end if
!
      if( (f_trns%i_SGS_c_flux*iflag_SGS_comp_flux) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_rtp, cp_prop1%coef_advect,                     &
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
      subroutine similarity_SGS_terms_rtp                               &
     &         (sph_rtp, f_trns, bg_trns, fg_trns,                      &
     &          ncomp_rtp_2_rj, nc_SGS_rj_2_rtp, nc_SGS_rtp_2_rj,       &
     &          frc_rtp, fil_rtp, fSGS_rtp)
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
      subroutine wider_similarity_SGS_rtp(sph_rtp, b_trns, bg_trns,     &
     &          ncomp_rj_2_rtp, nc_SGS_rj_2_rtp, fld_rtp, fil_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
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
     &     (sph_rtp%nnod_rtp, fl_prop1%coef_velo,                       &
     &      fld_rtp(1,b_trns%i_wide_fil_vort),                          &
     &      fld_rtp(1,b_trns%i_wide_fil_velo),                          &
     &      fil_rtp(1,bg_trns%i_wide_SGS_inertia))
      end if
!
      if(bg_trns%i_wide_SGS_Lorentz .gt. 0) then
        call subcract_X_product_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, fl_prop1%coef_lor,                        &
     &      fld_rtp(1,b_trns%i_wide_fil_current),                       &
     &      fld_rtp(1,b_trns%i_wide_fil_magne),                         &
     &      fil_rtp(1,bg_trns%i_wide_SGS_Lorentz))
      end if
!
      if(bg_trns%i_wide_SGS_vp_induct .gt. 0) then
        call subcract_X_product_w_coef_smp                              &
     &     (sph_rtp%nnod_rtp, cd_prop1%coef_induct,                     &
     &      fld_rtp(1,b_trns%i_wide_fil_velo),                          &
     &      fld_rtp(1,b_trns%i_wide_fil_magne),                         &
     &      fil_rtp(1,bg_trns%i_wide_SGS_vp_induct))
      end if
!
      if(bg_trns%i_wide_SGS_h_flux .gt. 0) then
        call sub_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_rtp, ht_prop1%coef_advect,                     &
     &      fld_rtp(1,b_trns%i_wide_fil_velo),                          &
     &      fld_rtp(1,b_trns%i_wide_fil_temp),                          &
     &      fil_rtp(1,bg_trns%i_wide_SGS_h_flux))
      end if
!
      if(bg_trns%i_wide_SGS_c_flux .gt. 0) then
        call sub_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_rtp, cp_prop1%coef_advect,                     &
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
