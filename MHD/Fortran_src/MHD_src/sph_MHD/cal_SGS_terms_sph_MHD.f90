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
!!@endverbatim
!
      module cal_SGS_terms_sph_MHD
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_physical_property
!
      use t_spheric_rtp_data
      use t_phys_address
!
      implicit none
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
      use m_machine_parameter
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
        call cal_cross_prod_w_coef_smp(np_smp, sph_rtp%nnod_rtp,        &
     &      sph_rtp%istack_inod_rtp_smp, coef_velo,                     &
     &      fld_rtp(1,b_trns%i_filter_vort),                            &
     &      fld_rtp(1,b_trns%i_filter_velo),                            &
     &      frc_rtp(1,f_trns%i_SGS_inertia) )
      end if
!
      if( (f_trns%i_SGS_Lorentz*iflag_SGS_lorentz) .gt. 0) then
        call cal_cross_prod_w_coef_smp(np_smp, sph_rtp%nnod_rtp,        &
     &      sph_rtp%istack_inod_rtp_smp, coef_lor,                      &
     &      fld_rtp(1,b_trns%i_filter_current),                         &
     &      fld_rtp(1,b_trns%i_filter_magne),                           &
     &      frc_rtp(1,f_trns%i_SGS_Lorentz) )
      end if
!
!
!
      if( (f_trns%i_SGS_vp_induct*iflag_SGS_induction) .gt. 0) then
        call cal_cross_prod_w_coef_smp(np_smp, sph_rtp%nnod_rtp,        &
     &      sph_rtp%istack_inod_rtp_smp, coef_induct,                   &
     &      fld_rtp(1,b_trns%i_filter_velo),                            &
     &      fld_rtp(1,b_trns%i_filter_magne),                           &
     &      frc_rtp(1,f_trns%i_SGS_vp_induct) )
      end if
!
!
      if( (f_trns%i_SGS_h_flux*iflag_SGS_heat) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp(np_smp, sph_rtp%nnod_rtp,   &
     &      sph_rtp%istack_inod_rtp_smp, coef_temp,                     &
     &      fld_rtp(1,b_trns%i_filter_velo),                            &
     &      fld_rtp(1,b_trns%i_filter_temp),                            &
     &      frc_rtp(1,f_trns%i_SGS_h_flux) )
      end if
!
      if( (f_trns%i_SGS_c_flux*iflag_SGS_comp_flux) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp(np_smp, sph_rtp%nnod_rtp,   &
     &      sph_rtp%istack_inod_rtp_smp, coef_light,                    &
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
      end module cal_SGS_terms_sph_MHD
