!>@file   cal_nonlinear_sph_MHD.f90
!!@brief  module cal_nonlinear_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine nonlinear_terms_in_rtp(sph_rtp, b_trns, f_trns,      &
!!     &          ncomp_rj_2_rtp, ncomp_rtp_2_rj, fld_rtp, frc_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: b_trns, f_trns
!!      subroutine add_reftemp_advect_sph_MHD(kr_in, kr_out,            &
!!     &          nidx_rj, ar_1d_rj, g_sph_rj, is_h_advect, is_velo,    &
!!     &          nnod_rj, ntot_phys_rj, reftemp_rj, d_rj)
!!@endverbatim
!!
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out      Radial ID for outer boundary
!
      module cal_nonlinear_sph_MHD
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
      subroutine nonlinear_terms_in_rtp(sph_rtp, b_trns, f_trns,        &
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
      if( (f_trns%i_m_advect*iflag_t_evo_4_velo) .gt. 0) then
        call cal_cross_prod_w_coef_smp(sph_rtp%nnod_rtp, coef_velo,     &
     &      fld_rtp(1,b_trns%i_vort), fld_rtp(1,b_trns%i_velo),         &
     &      frc_rtp(1,f_trns%i_m_advect) )
      end if
!
      if( (f_trns%i_lorentz*iflag_4_lorentz) .gt. 0) then
        call cal_cross_prod_w_coef_smp(sph_rtp%nnod_rtp, coef_lor,      &
     &      fld_rtp(1,b_trns%i_current), fld_rtp(1,b_trns%i_magne),     &
     &      frc_rtp(1,f_trns%i_lorentz) )
      end if
!
!
!
      if( (f_trns%i_vp_induct*iflag_t_evo_4_magne) .gt. 0) then
        call cal_cross_prod_w_coef_smp(sph_rtp%nnod_rtp, coef_induct,   &
     &      fld_rtp(1,b_trns%i_velo), fld_rtp(1,b_trns%i_magne),        &
     &      frc_rtp(1,f_trns%i_vp_induct) )
      end if
!
!
      if( (f_trns%i_h_flux*iflag_t_evo_4_temp) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_rtp, coef_temp,                                &
     &      fld_rtp(1,b_trns%i_velo), fld_rtp(1,b_trns%i_temp),         &
     &      frc_rtp(1,f_trns%i_h_flux) )
      end if
!
      if( (f_trns%i_c_flux*evo_comp%iflag_scheme) .gt. 0) then
        call cal_vec_scalar_prod_w_coef_smp                             &
     &     (sph_rtp%nnod_rtp, coef_light,                               &
     &      fld_rtp(1,b_trns%i_velo), fld_rtp(1,b_trns%i_light),        &
     &      frc_rtp(1,f_trns%i_c_flux) )
      end if
!
!      if( (f_trns%i_Coriolis*iflag_4_coriolis) .gt. 0) then
!        call cal_wz_coriolis_rtp                                       &
!     &     (sph_rtp%sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                &
!     &      fld_rtp(1,b_trns%i_velo), frc_rtp(1,f_trns%i_Coriolis))
!      end if
!$omp end parallel
!
      end subroutine nonlinear_terms_in_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_reftemp_advect_sph_MHD(kr_in, kr_out,              &
     &          nidx_rj, ar_1d_rj, g_sph_rj, is_h_advect, is_velo,      &
     &          nnod_rj, ntot_phys_rj, reftemp_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: is_h_advect, is_velo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),13)
      real(kind = kreal), intent(in) :: ar_1d_rj(nidx_rj(1),3)
      real(kind = kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:1)
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d_rj(inod,is_h_advect) = d_rj(inod,is_h_advect)                 &
     &                      + coef_temp * g_sph_rj(j,3) * ar_1d_rj(k,2) &
     &                       * reftemp_rj(k,1) * d_rj(inod,is_velo)
      end do
!$omp end parallel do
!
      end subroutine add_reftemp_advect_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module cal_nonlinear_sph_MHD
