!>@file   sph_exp_rigid_ICB_s4t2.f90
!!@brief  module sph_exp_rigid_ICB_s4t2
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at ICB
!!        using 4-th order FDM for poloidal velocity
!!
!!@verbatim
!!      subroutine cal_sph_icb_rigid_v_and_w_s4t2(nri, jmax,            &
!!     &          g_sph_rj, kr_in, r_ICB, r_ICB1, d1nod_mat_fdm_2,      &
!!     &          fdm2_fix_fld_ICB, fdm4_noslip_ICB, fdm4_noslip_ICB1,  &
!!     &          Vt_ICB, n_point, d_rj_fld, d_rj_rot)
!!        integer(kind = kint), intent(in) :: nri, jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2), r_ICB1(0:2)
!!        real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        real(kind = kreal), intent(in) :: fdm4_noslip_ICB(0:2,2:4)
!!        real(kind = kreal), intent(in) :: fdm4_noslip_ICB1(-1:2,5)
!!        real(kind = kreal), intent(in) :: Vt_ICB(jmax)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!!      subroutine cal_sph_icb_rigid_rot_s4t2(nri, jmax,                &
!!     &          g_sph_rj, kr_in, r_ICB1, d1nod_mat_fdm_2,             &
!!     &          fdm2_fix_fld_ICB, fdm4_noslip_ICB, fdm4_noslip_ICB1,  &
!!     &          n_point, d_rj_fld, d_rj_rot)
!!        integer(kind = kint), intent(in) :: nri, jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB1(0:2)
!!        real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        real(kind = kreal), intent(in) :: fdm4_noslip_ICB(0:2,2:4)
!!        real(kind = kreal), intent(in) :: fdm4_noslip_ICB1(-1:2,5)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!!      subroutine cal_sph_icb_rigid_diffuse_s4t2(nri, jmax,            &
!!     &          g_sph_rj, kr_in, r_ICB, r_ICB1, d2nod_mat_fdm_2,      &
!!     &          fdm2_fix_fld_ICB, fdm4_noslip_ICB, fdm4_noslip_ICB1,  &
!!     &          coef_d, n_point, d_rj_fld, d_rj_diffuse)
!!        integer(kind = kint), intent(in) :: nri, jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2), r_ICB1(0:2)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        real(kind = kreal), intent(in) :: fdm4_noslip_ICB(0:2,2:4)
!!        real(kind = kreal), intent(in) :: fdm4_noslip_ICB1(-1:2,5)
!!        integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_diffuse(n_point,3)
!!@endverbatim
!!
!!@n @param n_point  Number of points for spectrum data
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param r_ICB(0:2)    Radius at ICB
!!@n @param r_ICB1(0:2)   Radius at the next of ICB
!!
!!@n @param fdm2_fix_fld_ICB(0:2,3)
!!         Matrix to evaluate radial derivative at ICB with fixed field
!!@n @param  fdm4_noslip_ICB(0:2,2:4)
!!         Matrix for poloidal velocity with non-slip boundary at ICB
!!@n @param  fdm4_noslip_ICB1(-1:2,5)
!!          Matrix for poloidal velocity with non-slip boundary
!!          at next of ICB
!!
!!@n @param Vt_ICB(jmax) Spectr data for toroidal velocity ICB
!!@n @param coef_d     Coefficient for diffusion term
!!
!
!
      module sph_exp_rigid_ICB_s4t2
!
      use m_precision
!
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_icb_rigid_v_and_w_s4t2(nri, jmax,              &
     &          g_sph_rj, kr_in, r_ICB, r_ICB1, d1nod_mat_fdm_2,        &
     &          fdm2_fix_fld_ICB, fdm4_noslip_ICB, fdm4_noslip_ICB1,    &
     &          Vt_ICB, n_point, d_rj_fld, d_rj_rot)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2), r_ICB1(0:2)
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB(0:2,2:4)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB1(-1:2,5)
      real(kind = kreal), intent(in) :: Vt_ICB(jmax)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2, i_p3
      real(kind = kreal) :: d1s_dr1, d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_p1,i_p2,i_p3,j,d1s_dr1,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
        i_p3 = i_p2 + jmax
!
        d_rj_fld(inod,1) = zero
        d_rj_fld(inod,2) = zero
        d_rj_fld(inod,3) = Vt_ICB(j)
!
        d2s_dr2 =  fdm4_noslip_ICB( 2,3) * d_rj_fld(i_p2,1)             &
     &           + fdm4_noslip_ICB( 1,3) * d_rj_fld(i_p1,1)
        d1t_dr1 =  fdm2_fix_fld_ICB(2,2) * d_rj_fld(i_p2,3)             &
     &           + fdm2_fix_fld_ICB(1,2) * d_rj_fld(i_p1,3)             &
     &           + fdm2_fix_fld_ICB(0,2) * d_rj_fld(inod,3)
!
        d_rj_rot(inod,1) =  d_rj_fld(inod,3)
        d_rj_rot(inod,2) =  d1t_dr1
        d_rj_rot(inod,3) = -d2s_dr2
!
!
        d1s_dr1 =  fdm4_noslip_ICB1( 2,2) * d_rj_fld(i_p3,1)            &
     &           + fdm4_noslip_ICB1( 1,2) * d_rj_fld(i_p2,1)            &
     &           + fdm4_noslip_ICB1( 0,2) * d_rj_fld(i_p1,1)            &
     &           + fdm4_noslip_ICB1(-1,2) * d_rj_fld(inod,1)
        d2s_dr2 =  fdm4_noslip_ICB1( 2,3) * d_rj_fld(i_p3,1)            &
     &           + fdm4_noslip_ICB1( 1,3) * d_rj_fld(i_p2,1)            &
     &           + fdm4_noslip_ICB1( 0,3) * d_rj_fld(i_p1,1)            &
     &           + fdm4_noslip_ICB1(-1,3) * d_rj_fld(inod,1)
        d1t_dr1 =  d1nod_mat_fdm_2(kr_in+1,-1) * d_rj_fld(inod,3)       &
     &           + d1nod_mat_fdm_2(kr_in+1, 0) * d_rj_fld(i_p1,3)       &
     &           + d1nod_mat_fdm_2(kr_in+1, 1) * d_rj_fld(i_p2,3)
!
        d_rj_fld(i_p1,2) =  d1s_dr1
        d_rj_rot(i_p1,1) =  d_rj_fld(i_p1,3)
        d_rj_rot(i_p1,2) =  d1t_dr1
        d_rj_rot(i_p1,3) = - (d2s_dr2 - g_sph_rj(j,3)                   &
     &                      * r_ICB1(2) * d_rj_fld(i_p1,1))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_icb_rigid_v_and_w_s4t2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_icb_rigid_rot_s4t2(nri, jmax,                  &
     &          g_sph_rj, kr_in, r_ICB1, d1nod_mat_fdm_2,               &
     &          fdm2_fix_fld_ICB, fdm4_noslip_ICB, fdm4_noslip_ICB1,    &
     &          n_point, d_rj_fld, d_rj_rot)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB1(0:2)
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB(0:2,2:4)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB1(-1:2,5)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2, i_p3
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_p1,i_p2,i_p3,j,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
        i_p3 = i_p2 + jmax
!
        d2s_dr2 =  fdm4_noslip_ICB( 2,3) * d_rj_fld(i_p2,1)            &
     &           + fdm4_noslip_ICB( 1,3) * d_rj_fld(i_p1,1)
        d1t_dr1 =  fdm2_fix_fld_ICB(2,2) * d_rj_fld(i_p2,3)            &
     &           + fdm2_fix_fld_ICB(1,2) * d_rj_fld(i_p1,3)            &
     &           + fdm2_fix_fld_ICB(0,2) * d_rj_fld(inod,3)
!
        d_rj_rot(inod,1) =  d_rj_fld(inod,3)
        d_rj_rot(inod,2) =  d1t_dr1
        d_rj_rot(inod,3) = -d2s_dr2
!
!
        d2s_dr2 =  fdm4_noslip_ICB1( 2,3) * d_rj_fld(i_p3,1)           &
     &           + fdm4_noslip_ICB1( 1,3) * d_rj_fld(i_p2,1)           &
     &           + fdm4_noslip_ICB1( 0,3) * d_rj_fld(i_p1,1)           &
     &           + fdm4_noslip_ICB1(-1,3) * d_rj_fld(inod,1)
        d1t_dr1 =  d1nod_mat_fdm_2(kr_in+1,-1) * d_rj_fld(inod,3)      &
     &           + d1nod_mat_fdm_2(kr_in+1, 0) * d_rj_fld(i_p1,3)      &
     &           + d1nod_mat_fdm_2(kr_in+1, 1) * d_rj_fld(i_p2,3)
!
        d_rj_rot(i_p1,1) =  d_rj_fld(i_p1,3)
        d_rj_rot(i_p1,2) =  d1t_dr1
        d_rj_rot(i_p1,3) = - ( d2s_dr2 - g_sph_rj(j,3)                  &
     &                                * r_ICB1(2) * d_rj_fld(i_p1,1))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_icb_rigid_rot_s4t2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_icb_rigid_diffuse_s4t2(nri, jmax,              &
     &          g_sph_rj, kr_in, r_ICB, r_ICB1, d2nod_mat_fdm_2,        &
     &          fdm2_fix_fld_ICB, fdm4_noslip_ICB, fdm4_noslip_ICB1,    &
     &          coef_d, n_point, d_rj_fld, d_rj_diffuse)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2), r_ICB1(0:2)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB(0:2,2:4)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB1(-1:2,5)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_diffuse(n_point,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2, i_p3
      real(kind = kreal) :: d2s_dr2, d2t_dr2
!
!
!$omp parallel do private(inod,i_p1,i_p2,i_p3,j,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
        i_p3 = i_p2 + jmax
!
        d2s_dr2 =  fdm4_noslip_ICB( 2,3) * d_rj_fld(i_p2,1)             &
     &           + fdm4_noslip_ICB( 1,3) * d_rj_fld(i_p1,1)
        d2t_dr2 =  fdm2_fix_fld_ICB(2,3) * d_rj_fld(i_p2,3)             &
     &           + fdm2_fix_fld_ICB(1,3) * d_rj_fld(i_p1,3)             &
     &           + fdm2_fix_fld_ICB(0,3) * d_rj_fld(inod,3)
!
        d_rj_diffuse(inod,1) =  coef_d * (d2s_dr2                       &
     &               - g_sph_rj(j,3) * r_ICB(2) * d_rj_fld(inod,1))
        d_rj_diffuse(inod,3) =  coef_d * (d2t_dr2                       &
     &               - g_sph_rj(j,3) * r_ICB(2) * d_rj_fld(inod,3))
!
        d2s_dr2 =  fdm4_noslip_ICB1( 2,3) * d_rj_fld(i_p3,1)            &
     &           + fdm4_noslip_ICB1( 1,3) * d_rj_fld(i_p2,1)            &
     &           + fdm4_noslip_ICB1( 0,3) * d_rj_fld(i_p1,1)            &
     &           + fdm4_noslip_ICB1(-1,3) * d_rj_fld(inod,1)
        d2t_dr2 =  d2nod_mat_fdm_2(kr_in+1,-1) * d_rj_fld(inod,3)       &
     &           + d2nod_mat_fdm_2(kr_in+1, 0) * d_rj_fld(i_p1,3)       &
     &           + d2nod_mat_fdm_2(kr_in+1, 1) * d_rj_fld(i_p2,3)
!
        d_rj_diffuse(i_p1,1) =  coef_d * (d2s_dr2                       &
     &               - g_sph_rj(j,3) * r_ICB1(2) * d_rj_fld(i_p1,1))
        d_rj_diffuse(i_p1,3) =  coef_d * (d2t_dr2                       &
     &               - g_sph_rj(j,3) * r_ICB1(2) * d_rj_fld(i_p1,3))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_icb_rigid_diffuse_s4t2
!
! -----------------------------------------------------------------------
!
      end module sph_exp_rigid_ICB_s4t2
