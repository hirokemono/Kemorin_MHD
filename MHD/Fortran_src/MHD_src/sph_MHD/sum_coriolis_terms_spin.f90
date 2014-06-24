!>@file   sum_coriolis_terms_spin.f90
!!@brief  module sum_coriolis_terms_spin
!!
!!@author H. Matsui
!!@date Programmed in 1995
!@n     Modified in Oct., 2009
!
!>@brief  Evaluate curl of Coriolis term by Gaunt integrals
!!
!!@verbatim
!!************************************************
!!
!!      subroutine sum_rot_coriolis_10_spin(ncomp, nvector, sp_rlm_spin)
!!      subroutine sum_div_coriolis_10_spin(ncomp, nvector, sp_rlm_spin)
!!      subroutine sum_r_coriolis_bc_10_spin(ncomp, nvector,           &
!!     &          kr, sp_rlm_spin, d_cor_bc)
!!
!!************************************************
!!
!!  Rotation of the Coriolos term
!!     (wss) = wss(jc,1,j3)*w*dyb/r**2
!!            + wss(jc,2,j3)*dw*yb/r**2
!!
!!     (wts) = wts(j3)*w*yb/r**2
!!
!!     (wst) = wst(1,j3)*( dw*dyb/r**2 + w*d2yb/r**2 - 2*w*dyb/r**3 )
!!            + wst(2,j3)*( d2w/r**2 - 2*dw/r**3 )*yb
!!
!!     (wtt) = wtt(jc,1,j3)*dw*yb/r**2
!!            + wtt(jc,2,j3)*w*( dyb/r**2 - 2*yb/r**3 )
!!
!!   Divergence of the Coriolis term
!!     (wsd) = wsd(jc,1,j3)*w*wsb/r**4
!!            + wsd(jc,2,j3)*dw*dwsb/r**2
!!     (wtd) = wtd(j3)*dw*dwtb/r**2
!!
!!  Radial componenet of the Coriolis term
!!     (wsr) = wsr(jc,1,j3)*dw*dusb/r**2
!!     (wtr) = wtr(j3)*dw*wtb/r**2
!!
!!************************************************
!!
!!************************************************
!!
!!     wss(jc,1,j3) = sw_rlm(jc,1,j3)
!!     wss(jc,2,j3) = sw_rlm(jc,2,j3)
!!     wts(jc,j3)   = sw_rlm(jc,3,j3)
!!     wst(jc,1,j3) = tw_rlm(jc,1,j3)
!!     wst(jc,2,j3) = tw_rlm(jc,2,j3)
!!     wtt(jc,1,j3) = tw_rlm(jc,3,j3)
!!     wtt(jc,2,j3) = tw_rlm(jc,4,j3)
!!
!!     wsd(jc,1,j3) = sd_rlm(jc,1,j3)
!!     wsd(jc,2,j3) = sd_rlm(jc,2,j3)
!!     wtd(jc,j3)   = td_rlm(jc,j3)
!!
!!     wsr(jc,j3) =   sr_rlm(jc,j3)
!!     wtr(jc,j3) =   tr_rlm(jc,j3)
!!
!!************************************************
!!@endverbatim
!!
!
      module sum_coriolis_terms_spin
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      use m_spheric_parameter
      use m_physical_property
!
      use m_poloidal_rotation
      use m_schmidt_poly_on_rtm
      use m_gaunt_coriolis_rlm
!
      implicit none
!
!   ------------------------------------------------------------------
!
      contains
!
!   ------------------------------------------------------------------
!
      subroutine sum_rot_coriolis_10_spin(ncomp, nvector, sp_rlm_spin)
!
      use m_coriolis_terms_rlm
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in)                                    &
     &    :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1),ncomp)
!
      integer(kind = kint) :: k_rlm, j_rlm
      integer(kind = kint) :: i_rlm,i11,i21,i12
      real(kind = kreal) :: sp_dvp_k1, sp_dvp_k2, sp_vp_k1, sp_vp_k2
      real(kind = kreal) :: sp_wp_l1, sp_d2vp_l1, sp_vp_l1
      real(kind = kreal) :: sp_wp_k1, sp_wp_k2, sp_dwp_k1, sp_dwp_k2
!
!
!$omp  parallel do                                                      &
!$omp& private(k_rlm,j_rlm,i_rlm,i11,i21,i12,                           &
!$omp&         sp_dvp_k1,sp_dvp_k2,sp_vp_k1,sp_vp_k2,sp_wp_l1,          &
!$omp&         sp_d2vp_l1,sp_vp_l1,sp_wp_k1,sp_wp_k2,sp_dwp_k1,         &
!$omp&         sp_dwp_k2)
      do k_rlm = 1, nidx_rlm(1)
        do j_rlm = 1, nidx_rlm(2)
          i_rlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
          i11 = jgi_cor_rlm(j_rlm,1)
          i21 = jgi_cor_rlm(j_rlm,2)
          i12 = jei_cor_rlm(j_rlm,1)
!
          sp_dvp_k1 = sp_rlm_spin(i11,k_rlm,b_trns%i_velo+nvector)
          sp_dvp_k2 = sp_rlm_spin(i21,k_rlm,b_trns%i_velo+nvector)
          sp_wp_l1 = sp_rlm_spin(i12,k_rlm,b_trns%i_vort)
          sp_vp_k1 = sp_rlm_spin(i11,k_rlm,b_trns%i_velo)
          sp_vp_k2 = sp_rlm_spin(i21,k_rlm,b_trns%i_velo)
!
          sp_d2vp_l1 = (a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)         &
     &        * g_sph_rlm(j_rlm,3)*sp_rlm_spin(i12,k_rlm,b_trns%i_velo) &
     &         - sp_rlm_spin(i12,k_rlm,b_trns%i_vort+2*nvector) )
          sp_vp_l1 = sp_rlm_spin(i12,k_rlm,b_trns%i_velo)
          sp_wp_k1 = sp_rlm_spin(i11,k_rlm,b_trns%i_vort)
          sp_wp_k2 = sp_rlm_spin(i21,k_rlm,b_trns%i_vort)
          sp_dwp_k1 = (sp_rlm_spin(i11,k_rlm,b_trns%i_vort+nvector)     &
     &               - two*a_r_1d_rlm_r(k_rlm)                          &
     &                *sp_rlm_spin(i11,k_rlm,b_trns%i_vort))
          sp_dwp_k2 = (sp_rlm_spin(i21,k_rlm,b_trns%i_vort+nvector)     &
     &               - two*a_r_1d_rlm_r(k_rlm)                          &
     &                *sp_rlm_spin(i21,k_rlm,b_trns%i_vort))
!
!
          d_cor_rlm(ip_rlm_rot_cor,i_rlm)                               &
     &     =  sw_rlm(1,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k1        &
     &      + sw_rlm(2,1,j_rlm) * omega_rlm(k_rlm,0) * sp_dvp_k2        &
     &      + sw_rlm(1,3,j_rlm) * omega_rlm(k_rlm,0) * sp_wp_l1         &
     &      + sw_rlm(1,2,j_rlm) * omega_rlm(k_rlm,1) * sp_vp_k1         &
     &      + sw_rlm(2,2,j_rlm) * omega_rlm(k_rlm,1) * sp_vp_k2
!*
          d_cor_rlm(it_rlm_rot_cor,i_rlm)                               &
     &     =  tw_rlm(1,1,j_rlm) * omega_rlm(k_rlm,0) * sp_d2vp_l1       &
     &      - tw_rlm(1,2,j_rlm) * omega_rlm(k_rlm,2) * sp_vp_l1         &
     &      + tw_rlm(1,3,j_rlm) * omega_rlm(k_rlm,1) * sp_wp_k1         &
     &      + tw_rlm(2,3,j_rlm) * omega_rlm(k_rlm,1) * sp_wp_k2         &
     &      + tw_rlm(1,4,j_rlm) * omega_rlm(k_rlm,0) * sp_dwp_k1        &
     &      + tw_rlm(2,4,j_rlm) * omega_rlm(k_rlm,0) * sp_dwp_k2
!
!
          d_cor_rlm(ip_rlm_rot_cor,i_rlm)                               &
     &          = -coef_cor * d_cor_rlm(ip_rlm_rot_cor,i_rlm)           &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
          d_cor_rlm(it_rlm_rot_cor,i_rlm)                               &
     &          = -coef_cor * d_cor_rlm(it_rlm_rot_cor,i_rlm)           &
     &                      * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_rot_coriolis_10_spin
!*
!*   ------------------------------------------------------------------
!*
      subroutine sum_div_coriolis_10_spin(ncomp, nvector, sp_rlm_spin)
!
      use m_coriolis_terms_rlm
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in)                                    &
     &    :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1),ncomp)
!
      integer(kind = kint) :: k_rlm, j_rlm, j11
      integer(kind = kint) :: i_rlm, i11,i21,i12
      real(kind = kreal) :: sp_wt_l1, sp_wp_k1, sp_wp_k2
      real(kind = kreal) :: sp_dwp_k1, sp_dwp_k2
!
!
!$omp  parallel do                                                      &
!$omp& private(k_rlm,j_rlm,i_rlm,i11,i21,i12,                           &
!$omp&         sp_wt_l1,sp_wp_k1,sp_wp_k2,sp_dwp_k1,sp_dwp_k2)
      do k_rlm = 1, nidx_rlm(1)
        do j_rlm = 1, nidx_rlm(2)
          i_rlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
!
          i11 = jgi_cor_rlm(j_rlm,1)
          i21 = jgi_cor_rlm(j_rlm,2)
          i12 = jei_cor_rlm(j_rlm,1)
!
          sp_wt_l1 = sp_rlm_spin(i12,k_rlm,b_trns%i_vort+2*nvector)
          sp_wp_k1 = half * sp_rlm_spin(i11,k_rlm,b_trns%i_vort)
          sp_wp_k2 = half * sp_rlm_spin(i21,k_rlm,b_trns%i_vort)
          sp_dwp_k1 = sp_rlm_spin(i11,k_rlm,b_trns%i_vort+nvector)
          sp_dwp_k2 = sp_rlm_spin(i21,k_rlm,b_trns%i_vort+nvector)
!
          d_cor_rlm(ip_rlm_div_cor,i_rlm)                               &
     &     =  td_rlm(1,j_rlm) *   omega_rlm(k_rlm,1) * sp_wt_l1         &
     &      + sd_rlm(1,1,j_rlm) * omega_rlm(k_rlm,2) * sp_wp_k1         &
     &      + sd_rlm(2,1,j_rlm) * omega_rlm(k_rlm,2) * sp_wp_k2         &
     &      + sd_rlm(1,2,j_rlm) * omega_rlm(k_rlm,1) * sp_dwp_k1        &
     &      + sd_rlm(2,2,j_rlm) * omega_rlm(k_rlm,1) * sp_dwp_k2
!
          d_cor_rlm(ip_rlm_div_cor,i_rlm)                               &
     &     = -coef_cor * d_cor_rlm(ip_rlm_div_cor,i_rlm)                &
     &                 * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
      end do
!$omp end parallel do
!
!  Degree 0
      j_rlm = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,  &
     &       izero)
      j11 = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,    &
     &       itwo)
!
      if( (j_rlm*j11) .gt. 0) then
!$omp  parallel do private(k_rlm,i_rlm,sp_wp_k1,sp_dwp_k1)
        do k_rlm = 1, nidx_rlm(1)
          i_rlm = j_rlm + (k_rlm-1)*nidx_rlm(2)
!
          sp_wp_k1 = half * sp_rlm_spin(j11,k_rlm,b_trns%i_vort)
          sp_dwp_k1 = sp_rlm_spin(j11,k_rlm,b_trns%i_vort+nvector)
!
          d_cor_rlm(ip_rlm_div_cor,i_rlm)                               &
     &       =  four*(two/three) * sp_wp_k1                             &
     &        + four*(two/three) * omega_rlm(k_rlm,1) * sp_dwp_k1
!
          d_cor_rlm(ip_rlm_div_cor,i_rlm)                               &
     &       = -coef_cor * d_cor_rlm(ip_rlm_div_cor,i_rlm)              &
     &                   * a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        end do
!$omp end parallel do
      end if
!
      end subroutine sum_div_coriolis_10_spin
!*
!*   ------------------------------------------------------------------
!*
      subroutine sum_r_coriolis_bc_10_spin(ncomp, nvector,              &
     &          kr, sp_rlm_spin, d_cor_bc)
!
      use m_addresses_trans_sph_MHD
      use m_coriolis_terms_rlm
!
      integer(kind = kint), intent(in) :: ncomp, nvector, kr
      real(kind = kreal), intent(in)                                    &
     &    :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1),ncomp)
      real(kind = kreal), intent(inout) :: d_cor_bc(nidx_rlm(2))
!
      integer(kind = kint) :: j_rlm, j11, i11,i21,i12
      real(kind = kreal) :: sp_dwp_l1, sp_vt_k1, sp_vt_k2
!
!
      if(kr .le. 0) return
!
!$omp  parallel do                                                      &
!$omp& private(j_rlm,i11,i21,i12,sp_dwp_l1,sp_vt_k1,sp_vt_k2)
      do j_rlm = 1, nidx_rlm(2)
        i11 = jgi_cor_rlm(j_rlm,1)
        i21 = jgi_cor_rlm(j_rlm,2)
        i12 = jei_cor_rlm(j_rlm,1)
!
        sp_dwp_l1 = sp_rlm_spin(i12,kr,b_trns%i_velo+nvector  )
        sp_vt_k1 =  sp_rlm_spin(i11,kr,b_trns%i_velo+2*nvector)
        sp_vt_k2 =  sp_rlm_spin(i21,kr,b_trns%i_velo+2*nvector)
!
        d_cor_bc(j_rlm)                                                 &
     &       =  sr_rlm(1,j_rlm) * omega_rlm(kr,1) * sp_dwp_l1           &
     &        + tr_rlm(1,j_rlm) * omega_rlm(kr,1) * sp_vt_k1            &
     &        + tr_rlm(2,j_rlm) * omega_rlm(kr,1) * sp_vt_k2
!
        d_cor_bc(j_rlm) = -coef_cor*a_r_1d_rlm_r(kr)*a_r_1d_rlm_r(kr)   &
     &                   * d_cor_bc(j_rlm)
      end do
!$omp end parallel do
!
!  Degree 0
      j_rlm = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,  &
     &       izero)
      j11 = find_local_sph_rlm_address(nidx_rlm(2), idx_gl_1d_rlm_j,    &
     &       itwo)
      if( (j_rlm*j11) .gt. 0) then
        sp_vt_k1 = sp_rlm_spin(j11,kr,b_trns%i_velo+2*nvector)
!
!        d_cor_bc(j_rlm) = -four*pi*(two/three)                         &
!     &                   * omega_rlm(kr,1) * sp_vt_k1
        d_cor_bc(j_rlm) = -(two/three)                                  &
     &                   * omega_rlm(kr,1) * sp_vt_k1
        d_cor_bc(j_rlm) = -coef_cor*a_r_1d_rlm_r(kr)*a_r_1d_rlm_r(kr)   &
     &                   * d_cor_bc(j_rlm)
      end if
!
      end subroutine sum_r_coriolis_bc_10_spin
!*
!*   ------------------------------------------------------------------
!
      subroutine inner_core_rot_z_coriolis_rlm(ncomp, sp_rlm_spin)
!
      use m_coriolis_terms_rlm
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &    :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1),ncomp)
!
      integer(kind = kint) :: i11s, i10c, i11c, j11s, j11c
      real(kind = kreal) :: sp_wp_11c, sp_wp_11s
!
!
      if(idx_rlm_ICB .eq. 0) return
      if((idx_rlm_degree_one(-1)*idx_rlm_degree_one(1)) .eq. 0) return
!
      j11s = idx_rlm_degree_one(-1)
      j11c = idx_rlm_degree_one( 1)
!
      i11s = idx_rlm_degree_one(-1) + (idx_rlm_ICB-1)*nidx_rlm(2)
      i10c = idx_rlm_degree_one( 0) + (idx_rlm_ICB-1)*nidx_rlm(2)
      i11c = idx_rlm_degree_one( 1) + (idx_rlm_ICB-1)*nidx_rlm(2)
!
      sp_wp_11c = sp_rlm_spin(j11c,idx_rlm_ICB,b_trns%i_vort)
      sp_wp_11s = sp_rlm_spin(j11s,idx_rlm_ICB,b_trns%i_vort)
!
      d_cor_rlm(ip_rlm_rot_cor,i10c) = zero
      d_cor_rlm(ip_rlm_rot_cor,i11s)                                    &
     &       = -two*coef_cor*radius_1d_rj_r(idx_rlm_ICB)                &
     &        * omega_rlm(idx_rlm_ICB,0)*sp_wp_11c
      d_cor_rlm(ip_rlm_rot_cor,i11c)                                    &
     &       =  two*coef_cor*radius_1d_rj_r(idx_rlm_ICB)                &
     &        * omega_rlm(idx_rlm_ICB,0)*sp_wp_11s
!
      end subroutine inner_core_rot_z_coriolis_rlm
!
! ----------------------------------------------------------------------
!
      end module sum_coriolis_terms_spin
