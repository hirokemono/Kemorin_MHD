!>@file   set_sph_exp_rigid_CMB.f90
!!@brief  module set_sph_exp_rigid_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at CMB
!!
!!@verbatim
!!      subroutine cal_sph_nod_cmb_rigid_v_and_w(jmax, kr_out,          &
!!     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,                    &
!!     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_cmb_rigid_velo(jmax, kr_out,             &
!!     &          is_fld, n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_cmb_rigid_rot2                           &
!!     &         (jmax, kr_out, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,      &
!!     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_cmb_rigid_diffuse2(jmax, kr_out,         &
!!     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB, coef_d,            &
!!     &          is_fld, is_diffuse, n_point, ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@n @param n_point  Number of points for spectrum data
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_CMB(0:2)   Radius at CMB
!!
!!@n @param fdm2_fix_fld_CMB(0:2,3)
!!         Matrix to evaluate radial derivative at CMB with fixed field
!!@n @param fdm2_fix_dr_CMB(-1:1,3)
!!         Matrix to evaluate field at CMB with fixed radial derivative
!!
!!@n @param coef_d     Coefficient for diffusion term
!!@n @param is_fld     Address of poloidal velocity in d_rj
!!@n @param is_rot     Address of poloidal vorticity in d_rj
!!@n @param is_diffuse Address of poloidal viscousity in d_rj
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module set_sph_exp_rigid_CMB
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
      subroutine cal_sph_nod_cmb_rigid_v_and_w(jmax, kr_out,            &
     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,                      &
     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d_rj(inod,is_fld  ) = zero
        d_rj(inod,is_fld+1) = zero
        d_rj(inod,is_fld+2) = zero
!
        d2s_dr2 =  fdm2_fix_dr_CMB(-1,3) * d_rj(i_n1,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_CMB(2,2) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,2) * d_rj(i_n1,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) =  zero
        d_rj(inod,is_rot+1) =  d1t_dr1
        d_rj(inod,is_rot+2) = -d2s_dr2
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_rigid_v_and_w
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_rigid_velo(jmax, kr_out,               &
     &          is_fld, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
!
        d_rj(inod,is_fld  ) = zero
        d_rj(inod,is_fld+1) = zero
        d_rj(inod,is_fld+2) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_rigid_velo
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_rigid_rot2                             &
     &         (jmax, kr_out, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,        &
     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d2s_dr2 =  fdm2_fix_dr_CMB(-1,3) * d_rj(i_n1,is_fld  )          &
     &           + fdm2_fix_dr_CMB( 0,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_CMB(2,2) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,2) * d_rj(i_n1,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =  d1t_dr1
        d_rj(inod,is_rot+2) = -d2s_dr2
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_rigid_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_rigid_diffuse2(jmax, kr_out,           &
     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB, coef_d,              &
     &          is_fld, is_diffuse, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d2t_dr2
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d2s_dr2 =  fdm2_fix_dr_CMB(-1,3) * d_rj(i_n1,is_fld  )
        d2t_dr2 =  fdm2_fix_fld_CMB(2,3) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,3) * d_rj(i_n1,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =  coef_d * d2s_dr2
        d_rj(inod,is_diffuse+2) =  coef_d * d2t_dr2
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_rigid_diffuse2
!
! -----------------------------------------------------------------------
!
      end module set_sph_exp_rigid_CMB
