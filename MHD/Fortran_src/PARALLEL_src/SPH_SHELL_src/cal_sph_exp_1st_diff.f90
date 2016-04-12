!>@file   cal_sph_exp_1st_diff.f90
!!@brief  module cal_sph_exp_1st_diff
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate first radial derivative for spectr data
!!
!!@verbatim
!!      subroutine cal_sph_nod_gradient_2(kr_in, kr_out,                &
!!     &          is_fld, is_grad, ntot_phys_rj, d_rj)
!!      subroutine normalize_sph_average_grad                           &
!!     &         (is_fld, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_nod_vect_dr_2(kr_in, kr_out, is_fld, is_dr,  &
!!     &          ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@n @param kr_in    radial ID for inner boundary
!!@n @param kr_out   radial ID for outer boundary
!!@n @param dnod_rj(nnod_rj)      Input spectr data
!!@n @param dnod_dr(nnod_rj,nd)   Gradient of field
!!@n                 dnod_dr(nnod_rj,1) = r^2 l(l+1) d phi / dr
!!@n                 dnod_dr(nnod_rj,2) = phi
!!@n                 dnod_dr(nnod_rj,3) = 0
!
      module cal_sph_exp_1st_diff
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
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
      subroutine cal_sph_nod_gradient_2(kr_in, kr_out,                  &
     &          is_fld, is_grad, ntot_phys_rj, d_rj)
!
      use m_schmidt_poly_on_rtm
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_grad
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
      real(kind = kreal) :: d1sdr
!
!
      ist  = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k,d1sdr)
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d1sdr =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld)              &
     &         + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld)              &
     &         + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld)
!
        d_rj(inod,is_grad  ) = d1sdr * g_sph_rj(j,13)                   &
     &                   * radius_1d_rj_r(k)**2
        d_rj(inod,is_grad+1) = d_rj(inod,is_fld)
        d_rj(inod,is_grad+2) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_gradient_2
!
! -----------------------------------------------------------------------
!
      subroutine normalize_sph_average_grad                             &
     &         (is_fld, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: ntot_phys_rj, is_fld
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, k
!
!
      if(idx_rj_degree_zero .eq. 0) return
!
!$omp parallel do private(inod,k)
      do k = 1, nidx_rj(1)
        inod = (k-1) * nidx_rj(2) + idx_rj_degree_zero
        d_rj(inod,is_fld  ) = two * d_rj(inod,is_fld)
        d_rj(inod,is_fld+1) = zero
        d_rj(inod,is_fld+2) = zero
      end do
!$omp end parallel do
!
      end subroutine normalize_sph_average_grad
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_vect_dr_2(kr_in, kr_out, is_fld, is_dr,    &
     &          ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_dr
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, i_p1, i_n1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist  = kr_in * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,i_n1,j,k)
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod-j) / nidx_rj(2)
!
        d_rj(inod,is_dr) =  d1nod_mat_fdm_2(k,-1) * d_rj(i_n1,is_fld)   &
     &                    + d1nod_mat_fdm_2(k, 0) * d_rj(inod,is_fld)   &
     &                    + d1nod_mat_fdm_2(k, 1) * d_rj(i_p1,is_fld)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_vect_dr_2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_1st_diff
