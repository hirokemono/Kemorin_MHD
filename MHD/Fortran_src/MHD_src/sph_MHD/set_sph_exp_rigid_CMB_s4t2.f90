!>@file   set_sph_exp_rigid_CMB_s4t2.f90
!!@brief  module set_sph_exp_rigid_CMB_s4t2
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at CMB
!!        using 4-th order FDM for poloidal velocity
!!
!!@verbatim
!!      subroutine cal_sph_cmb_rigid_v_and_w_s4t2(nnod_rj, jmax, kr_out,&
!!     &          r_CMB, r_CMB1, fdm2_fix_fld_CMB, fdm4_noslip_CMB,     &
!!     &          fdm4_noslip_CMB1, Vt_CMB, is_fld, is_rot,             &
!!     &          ntot_phys_rj, d_rj)
!!      subroutine cal_sph_cmb_rigid_rot_s4t2(nnod_rj, jmax, kr_out,    &
!!     &          r_CMB1, fdm2_fix_fld_CMB, fdm4_noslip_CMB,            &
!!     &          fdm4_noslip_CMB1, is_fld, is_rot, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_cmb_rigid_diffuse_s4t2(nnod_rj, jmax, kr_out,&
!!     &          r_CMB, r_CMB1, fdm2_fix_fld_CMB, fdm4_noslip_CMB,     &
!!     &          fdm4_noslip_CMB1, coef_d, is_fld, is_diffuse,         &
!!     &          ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@n @param nnod_rj  Number of points for spectrum data
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_CMB(0:2)   Radius at CMB
!!@n @param r_CMB1(0:2)   Radius at the next of CMB
!!
!!@n @param fdm2_fix_fld_CMB(0:2,3)
!!         Matrix to evaluate radial derivative at CMB with fixed field
!!@n @param  fdm4_noslip_CMB(-2:0,3:4)
!!         Matrix for poloidal velocity with non-slip boundary at CMB
!!@n @param  fdm4_noslip_CMB1(-2:1,5)
!!          Matrix for poloidal velocity with non-slip boundary
!!          at next of CMB
!!
!!@n @param Vt_CMB(jmax) Spectr data for toroidal velocity CMB
!!@n @param coef_d     Coefficient for diffusion term
!!@n @param is_fld     Address of poloidal velocity in d_rj
!!@n @param is_rot     Address of poloidal vorticity in d_rj
!!@n @param is_diffuse Address of poloidal viscousity in d_rj
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module set_sph_exp_rigid_CMB_s4t2
!
      use m_precision
!
      use m_constants
      use m_schmidt_poly_on_rtm
      use m_fdm_coefs
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_cmb_rigid_v_and_w_s4t2(nnod_rj, jmax, kr_out,  &
     &          r_CMB, r_CMB1, fdm2_fix_fld_CMB, fdm4_noslip_CMB,       &
     &          fdm4_noslip_CMB1, Vt_CMB, is_fld, is_rot,               &
     &          ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: r_CMB(0:2), r_CMB1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
      real(kind = kreal), intent(in) :: fdm4_noslip_CMB(-2:0,3:4)
      real(kind = kreal), intent(in) :: fdm4_noslip_CMB1(-2:1,5)
      real(kind = kreal), intent(in) :: Vt_CMB(jmax)
!
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2, i_n3
      real(kind = kreal) :: d1s_dr1, d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,i_n3,j,d1s_dr1,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
        i_n3 = i_n2 - jmax
!
        d_rj(inod,is_fld  ) = zero
        d_rj(inod,is_fld+1) = zero
        d_rj(inod,is_fld+2) = Vt_CMB(j)*r_CMB(1)
!
        d2s_dr2 =  fdm4_noslip_CMB(-2,3) * d_rj(i_n2,is_fld  )          &
     &           + fdm4_noslip_CMB(-1,3) * d_rj(i_n1,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_CMB(2,2) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,2) * d_rj(i_n1,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) =  d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =  d1t_dr1
        d_rj(inod,is_rot+2) = -d2s_dr2
!
!
        d1s_dr1 =  fdm4_noslip_CMB1(-2,2) * d_rj(i_n3,is_fld  )         &
     &           + fdm4_noslip_CMB1(-1,2) * d_rj(i_n2,is_fld  )         &
     &           + fdm4_noslip_CMB1( 0,2) * d_rj(i_n1,is_fld  )         &
     &           + fdm4_noslip_CMB1( 1,2) * d_rj(inod,is_fld  )
        d2s_dr2 =  fdm4_noslip_CMB1(-2,3) * d_rj(i_n3,is_fld  )         &
     &           + fdm4_noslip_CMB1(-1,3) * d_rj(i_n2,is_fld  )         &
     &           + fdm4_noslip_CMB1( 0,3) * d_rj(i_n1,is_fld  )         &
     &           + fdm4_noslip_CMB1( 1,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(-1,kr_out-1) * d_rj(i_n2,is_fld+2)   &
     &           + d1nod_mat_fdm_2( 0,kr_out-1) * d_rj(i_n1,is_fld+2)   &
     &           + d1nod_mat_fdm_2( 1,kr_out-1) * d_rj(inod,is_fld+2)
!
        d_rj(i_n1,is_fld+1) =  d1s_dr1
        d_rj(i_n1,is_rot  ) =  d_rj(i_n1,is_fld+2)
        d_rj(i_n1,is_rot+1) =  d1t_dr1
        d_rj(i_n1,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                         * r_CMB1(2)*d_rj(i_n1,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_cmb_rigid_v_and_w_s4t2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_cmb_rigid_rot_s4t2(nnod_rj, jmax, kr_out,      &
     &          r_CMB1, fdm2_fix_fld_CMB, fdm4_noslip_CMB,              &
     &          fdm4_noslip_CMB1, is_fld, is_rot, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: r_CMB1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
      real(kind = kreal), intent(in) :: fdm4_noslip_CMB(-2:0,3:4)
      real(kind = kreal), intent(in) :: fdm4_noslip_CMB1(-2:1,5)
!
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2, i_n3
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,i_n3,j,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
        i_n3 = i_n2 - jmax
!
        d2s_dr2 =  fdm4_noslip_CMB(-2,3) * d_rj(i_n2,is_fld  )          &
     &           + fdm4_noslip_CMB(-1,3) * d_rj(i_n1,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_CMB(2,2) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,2) * d_rj(i_n1,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) =  d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =  d1t_dr1
        d_rj(inod,is_rot+2) = -d2s_dr2
!
!
        d2s_dr2 =  fdm4_noslip_CMB1(-2,3) * d_rj(i_n3,is_fld  )         &
     &           + fdm4_noslip_CMB1(-1,3) * d_rj(i_n2,is_fld  )         &
     &           + fdm4_noslip_CMB1( 0,3) * d_rj(i_n1,is_fld  )         &
     &           + fdm4_noslip_CMB1( 1,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(-1,kr_out-1) * d_rj(i_n2,is_fld+2)   &
     &           + d1nod_mat_fdm_2( 0,kr_out-1) * d_rj(i_n1,is_fld+2)   &
     &           + d1nod_mat_fdm_2( 1,kr_out-1) * d_rj(inod,is_fld+2)
!
        d_rj(i_n1,is_rot  ) =  d_rj(i_n1,is_fld+2)
        d_rj(i_n1,is_rot+1) =  d1t_dr1
        d_rj(i_n1,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                         * r_CMB1(2)*d_rj(i_n1,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_cmb_rigid_rot_s4t2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_cmb_rigid_diffuse_s4t2(nnod_rj, jmax, kr_out,  &
     &          r_CMB, r_CMB1, fdm2_fix_fld_CMB, fdm4_noslip_CMB,       &
     &          fdm4_noslip_CMB1, coef_d, is_fld, is_diffuse,           &
     &          ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: r_CMB(0:2), r_CMB1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
      real(kind = kreal), intent(in) :: fdm4_noslip_CMB(-2:0,3:4)
      real(kind = kreal), intent(in) :: fdm4_noslip_CMB1(-2:1,5)
!
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2, i_n3
      real(kind = kreal) :: d2s_dr2, d2t_dr2
!
!
!$omp parallel do private(inod,i_n1,i_n2,i_n3,j,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
        i_n3 = i_n2 - jmax
!
        d2s_dr2 =  fdm4_noslip_CMB(-2,3) * d_rj(i_n2,is_fld  )          &
     &           + fdm4_noslip_CMB(-1,3) * d_rj(i_n1,is_fld  )
        d2t_dr2 =  fdm2_fix_fld_CMB(2,3) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,3) * d_rj(i_n1,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(0,3) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =  coef_d * (d2s_dr2                    &
     &               - g_sph_rj(j,3)*r_CMB(2)*d_rj(inod,is_fld  ) )
        d_rj(inod,is_diffuse+2) =  coef_d * (d2t_dr2                    &
     &               - g_sph_rj(j,3)*r_CMB(2)*d_rj(inod,is_fld+2) )
!
        d2s_dr2 =  fdm4_noslip_CMB1(-2,3) * d_rj(i_n3,is_fld  )         &
     &           + fdm4_noslip_CMB1(-1,3) * d_rj(i_n2,is_fld  )         &
     &           + fdm4_noslip_CMB1( 0,3) * d_rj(i_n1,is_fld  )         &
     &           + fdm4_noslip_CMB1( 1,3) * d_rj(inod,is_fld  )
        d2t_dr2 =  d2nod_mat_fdm_2(-1,kr_out-1) * d_rj(i_n2,is_fld+2)   &
     &           + d2nod_mat_fdm_2( 0,kr_out-1) * d_rj(i_n1,is_fld+2)   &
     &           + d2nod_mat_fdm_2( 1,kr_out-1) * d_rj(inod,is_fld+2)
!
        d_rj(i_n1,is_diffuse  ) =  coef_d * (d2s_dr2                    &
     &               - g_sph_rj(j,3)*r_CMB1(2)*d_rj(i_n1,is_fld  ) )
        d_rj(i_n1,is_diffuse+2) =  coef_d * (d2t_dr2                    &
     &               - g_sph_rj(j,3)*r_CMB1(2)*d_rj(i_n1,is_fld+2) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_cmb_rigid_diffuse_s4t2
!
! -----------------------------------------------------------------------
!
      end module set_sph_exp_rigid_CMB_s4t2
