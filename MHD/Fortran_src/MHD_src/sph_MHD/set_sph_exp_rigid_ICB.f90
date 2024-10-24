!>@file   set_sph_exp_rigid_ICB.f90
!!@brief  module set_sph_exp_rigid_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at ICB
!!
!!@verbatim
!!      subroutine cal_sph_nod_icb_rigid_velo2(jmax, kr_in,             &
!!     &          is_fld, n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_icb_rotate_velo2(idx_rj_degree_zero,     &
!!     &          idx_rj_degree_one, nidx_rj, kr_in, r_ICB, radius_rj,  &
!!     &          is_fld, n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_icb_rigid_rot2(jmax, g_sph_rj,           &
!!     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,      &
!!     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_icb_rigid_diffuse2(jmax, g_sph_rj, kr_in,&
!!     &          r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB, coef_d,     &
!!     &          is_fld, is_diffuse, n_point, ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@n @param n_point  Number of points for spectrum data
!!@n @param idx_rj_degree_zero    Local address for degree 0
!!@n @param idx_rj_degree_one(-1:1)    Local address for degree 1
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!
!!@n @param fdm2_fix_fld_ICB(0:2,3)
!!         Matrix to evaluate radial derivative at ICB with fixed field
!!@n @param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fixed radial derivative
!!
!!@n @param coef_d     Coefficient for diffusion term
!!@n @param is_fld     Address of poloidal velocity in d_rj
!!@n @param is_rot     Address of poloidal vorticity in d_rj
!!@n @param is_diffuse Address of poloidal viscousity in d_rj
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module set_sph_exp_rigid_ICB
!
      use m_precision
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
      subroutine cal_sph_nod_icb_rigid_velo2(jmax, kr_in,               &
     &          is_fld, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, k
!
!
!$omp parallel do private(k,inod)
      do j = 1, jmax
        do k = 1, kr_in
          inod = j + (k-1) * jmax
!
          d_rj(inod,is_fld  ) = zero
          d_rj(inod,is_fld+1) = zero
          d_rj(inod,is_fld+2) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_rigid_velo2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_rotate_velo2(idx_rj_degree_zero,       &
     &          idx_rj_degree_one, nidx_rj, kr_in, r_ICB, radius_rj,    &
     &          is_fld, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: idx_rj_degree_one(-1:1)
      integer(kind = kint), intent(in) :: kr_in
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: nidx_rj(2)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: radius_rj(nidx_rj(1))
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, iICB, j, k, m
      real(kind = kreal) :: bc_l1_tmp(-1:1)
!
!
      do m = -1, 1
        if(idx_rj_degree_one(m) .gt. 0) then
          iICB = idx_rj_degree_one(m) + (kr_in-1) * nidx_rj(2)
          bc_l1_tmp(m) = d_rj(iICB,is_fld+2)
        end if
      end do
!
!$omp parallel do private(k,inod)
      do j = 1, nidx_rj(2)
        do k = 1, kr_in
          inod = j + (k-1) * nidx_rj(2)
!
          d_rj(inod,is_fld  ) = zero
          d_rj(inod,is_fld+1) = zero
          d_rj(inod,is_fld+2) = zero
        end do
      end do
!$omp end parallel do
!
      do m = -1, 1
        if(idx_rj_degree_one(m) .gt. 0) then
          iICB = idx_rj_degree_one(m) + (kr_in-1) * nidx_rj(2)
          d_rj(iICB,is_fld+2) = bc_l1_tmp(m)
          do k = 1, kr_in-1
            inod = idx_rj_degree_one(m) + (k-1) * nidx_rj(2)
            d_rj(inod,is_fld+2) = bc_l1_tmp(m)*r_ICB(2)                 &
     &                           * radius_rj(k)*radius_rj(k)
          end do
        end if
      end do
 !
      end subroutine cal_sph_nod_icb_rotate_velo2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_rigid_rot2(jmax, g_sph_rj,             &
     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,        &
     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2 =  fdm2_fix_dr_ICB( 0,3) * d_rj(inod,is_fld  )          &
     &           + fdm2_fix_dr_ICB( 1,3) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_ICB( 0,2) * d_rj(inod,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 1,2) * d_rj(i_p1,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 2,2) * d_rj(i_p2,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - (d2s_dr2                                &
     &                 - g_sph_rj(j,3) * r_ICB(2)*d_rj(inod,is_fld  ))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_rigid_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_rigid_diffuse2(jmax, g_sph_rj, kr_in,  &
     &          r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB, coef_d,       &
     &          is_fld, is_diffuse, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2,d2t_dr2
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2 =  fdm2_fix_dr_ICB( 1,3) * d_rj(i_p1,is_fld  )
        d2t_dr2 =  fdm2_fix_fld_ICB( 1,3) * d_rj(i_p1,is_fld+2)         &
     &           + fdm2_fix_fld_ICB( 2,3) * d_rj(i_p2,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =  coef_d * d2s_dr2
        d_rj(inod,is_diffuse+2) =  coef_d * d2t_dr2
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_rigid_diffuse2
!
! -----------------------------------------------------------------------
!
      end module set_sph_exp_rigid_ICB
