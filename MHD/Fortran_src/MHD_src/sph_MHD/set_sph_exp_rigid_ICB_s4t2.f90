!>@file   set_sph_exp_rigid_ICB_s4t2.f90
!!@brief  module set_sph_exp_rigid_ICB_s4t2
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
!!     &          Vt_ICB, is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_icb_rigid_rot_s4t2(nri, jmax,                &
!!     &          g_sph_rj, kr_in, r_ICB1, d1nod_mat_fdm_2,             &
!!     &          fdm2_fix_fld_ICB, fdm4_noslip_ICB, fdm4_noslip_ICB1,  &
!!     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_icb_rigid_diffuse_s4t2(nri, jmax,            &
!!     &          g_sph_rj, kr_in, r_ICB, r_ICB1, d2nod_mat_fdm_2,      &
!!     &          fdm2_fix_fld_ICB, fdm4_noslip_ICB, fdm4_noslip_ICB1,  &
!!     &          coef_d, is_fld, is_diffuse,                           &
!!     &          n_point, ntot_phys_rj, d_rj)
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
!!@n @param  fdm4_noslip_ICB(0:2,3:5)
!!         Matrix for poloidal velocity with non-slip boundary at ICB
!!@n @param  fdm4_noslip_ICB1(-1:2,5)
!!          Matrix for poloidal velocity with non-slip boundary
!!          at next of ICB
!!
!!@n @param Vt_ICB(jmax) Spectr data for toroidal velocity ICB
!!@n @param coef_d     Coefficient for diffusion term
!!@n @param is_fld     Address of poloidal velocity in d_rj
!!@n @param is_rot     Address of poloidal vorticity in d_rj
!!@n @param is_diffuse Address of poloidal viscousity in d_rj
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!!
!
!
      module set_sph_exp_rigid_ICB_s4t2
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
     &          Vt_ICB, is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2), r_ICB1(0:2)
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB(0:2,3:5)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB1(-1:2,5)
      real(kind = kreal), intent(in) :: Vt_ICB(jmax)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
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
        d_rj(inod,is_fld  ) = zero
        d_rj(inod,is_fld+1) = zero
        d_rj(inod,is_fld+2) = Vt_ICB(j)*r_ICB(1)
!
        d2s_dr2 =  fdm4_noslip_ICB( 2,3) * d_rj(i_p2,is_fld  )          &
     &           + fdm4_noslip_ICB( 1,3) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_ICB(2,2) * d_rj(i_p2,is_fld+2)          &
     &           + fdm2_fix_fld_ICB(1,2) * d_rj(i_p1,is_fld+2)          &
     &           + fdm2_fix_fld_ICB(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) =  d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =  d1t_dr1
        d_rj(inod,is_rot+2) = -d2s_dr2
!
!
        d1s_dr1 =  fdm4_noslip_ICB1( 2,2) * d_rj(i_p3,is_fld  )         &
     &           + fdm4_noslip_ICB1( 1,2) * d_rj(i_p2,is_fld  )         &
     &           + fdm4_noslip_ICB1( 0,2) * d_rj(i_p1,is_fld  )         &
     &           + fdm4_noslip_ICB1(-1,2) * d_rj(inod,is_fld  )
        d2s_dr2 =  fdm4_noslip_ICB1( 2,3) * d_rj(i_p3,is_fld  )         &
     &           + fdm4_noslip_ICB1( 1,3) * d_rj(i_p2,is_fld  )         &
     &           + fdm4_noslip_ICB1( 0,3) * d_rj(i_p1,is_fld  )         &
     &           + fdm4_noslip_ICB1(-1,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(kr_in+1,-1) * d_rj(inod,is_fld+2)    &
     &           + d1nod_mat_fdm_2(kr_in+1, 0) * d_rj(i_p1,is_fld+2)    &
     &           + d1nod_mat_fdm_2(kr_in+1, 1) * d_rj(i_p2,is_fld+2)
!
        d_rj(i_p1,is_fld+1) =  d1s_dr1
        d_rj(i_p1,is_rot  ) =  d_rj(i_p1,is_fld+2)
        d_rj(i_p1,is_rot+1) =  d1t_dr1
        d_rj(i_p1,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                        * r_ICB1(2)*d_rj(i_p1,is_fld  ) )
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
     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB1(0:2)
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB(0:2,3:5)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB1(-1:2,5)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
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
        d2s_dr2 =  fdm4_noslip_ICB( 2,3) * d_rj(i_p2,is_fld  )          &
     &           + fdm4_noslip_ICB( 1,3) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_ICB(2,2) * d_rj(i_p2,is_fld+2)          &
     &           + fdm2_fix_fld_ICB(1,2) * d_rj(i_p1,is_fld+2)          &
     &           + fdm2_fix_fld_ICB(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) =  d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =  d1t_dr1
        d_rj(inod,is_rot+2) = -d2s_dr2
!
!
        d2s_dr2 =  fdm4_noslip_ICB1( 2,3) * d_rj(i_p3,is_fld  )         &
     &           + fdm4_noslip_ICB1( 1,3) * d_rj(i_p2,is_fld  )         &
     &           + fdm4_noslip_ICB1( 0,3) * d_rj(i_p1,is_fld  )         &
     &           + fdm4_noslip_ICB1(-1,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(kr_in+1,-1) * d_rj(inod,is_fld+2)    &
     &           + d1nod_mat_fdm_2(kr_in+1, 0) * d_rj(i_p1,is_fld+2)    &
     &           + d1nod_mat_fdm_2(kr_in+1, 1) * d_rj(i_p2,is_fld+2)
!
        d_rj(i_p1,is_rot  ) =  d_rj(i_p1,is_fld+2)
        d_rj(i_p1,is_rot+1) =  d1t_dr1
        d_rj(i_p1,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                       * r_ICB1(2) * d_rj(i_p1,is_fld  ) )
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
     &          coef_d, is_fld, is_diffuse,                             &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2), r_ICB1(0:2)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB(0:2,3:5)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB1(-1:2,5)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
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
        d2s_dr2 =  fdm4_noslip_ICB( 2,3) * d_rj(i_p2,is_fld  )          &
     &           + fdm4_noslip_ICB( 1,3) * d_rj(i_p1,is_fld  )
        d2t_dr2 =  fdm2_fix_fld_ICB(2,3) * d_rj(i_p2,is_fld+2)          &
     &           + fdm2_fix_fld_ICB(1,3) * d_rj(i_p1,is_fld+2)          &
     &           + fdm2_fix_fld_ICB(0,3) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =  coef_d * (d2s_dr2                    &
     &               - g_sph_rj(j,3)*r_ICB(2)*d_rj(inod,is_fld  ) )
        d_rj(inod,is_diffuse+2) =  coef_d * (d2t_dr2                    &
     &               - g_sph_rj(j,3)*r_ICB(2)*d_rj(inod,is_fld+2) )
!
        d2s_dr2 =  fdm4_noslip_ICB1( 2,3) * d_rj(i_p3,is_fld  )         &
     &           + fdm4_noslip_ICB1( 1,3) * d_rj(i_p2,is_fld  )         &
     &           + fdm4_noslip_ICB1( 0,3) * d_rj(i_p1,is_fld  )         &
     &           + fdm4_noslip_ICB1(-1,3) * d_rj(inod,is_fld  )
        d2t_dr2 =  d2nod_mat_fdm_2(kr_in+1,-1) * d_rj(inod,is_fld+2)    &
     &           + d2nod_mat_fdm_2(kr_in+1, 0) * d_rj(i_p1,is_fld+2)    &
     &           + d2nod_mat_fdm_2(kr_in+1, 1) * d_rj(i_p2,is_fld+2)
!
        d_rj(i_p1,is_diffuse  ) =  coef_d * (d2s_dr2                    &
     &               - g_sph_rj(j,3)*r_ICB1(2)*d_rj(i_p1,is_fld  ) )
        d_rj(i_p1,is_diffuse+2) =  coef_d * (d2t_dr2                    &
     &               - g_sph_rj(j,3)*r_ICB1(2)*d_rj(i_p1,is_fld+2) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_icb_rigid_diffuse_s4t2
!
! -----------------------------------------------------------------------
!
      end module set_sph_exp_rigid_ICB_s4t2
