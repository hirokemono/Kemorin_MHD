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
!!     &          dnod_rj, dnod_dr)
!!      subroutine cal_sph_nod_vect_dr_2(kr_in, kr_out, dnod_rj, dnod_dr)
!!@endverbatim
!!
!!@n @param kr_in    radial ID for inner boundary
!!@n @param kr_out   radial ID for outer boundary
!!@n @param dnod_rj(nnod_rj)      Input spectr data
!!@n @param dnod_dr(nnod_rj,nd)   Gradient or radial derivative of field
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
     &          dnod_rj, dnod_dr)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: dnod_rj(nnod_rj)
!
      real(kind = kreal), intent(inout) :: dnod_dr(nnod_rj,3)
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
        k = 1 + (inod- j) / nidx_rj(2)
!
        dnod_dr(inod,1) =  d1nod_mat_fdm_2(k,-1) * dnod_rj(i_n1)        &
     &                 + d1nod_mat_fdm_2(k, 0) * dnod_rj(inod)          &
     &                 + d1nod_mat_fdm_2(k, 1) * dnod_rj(i_p1)
        dnod_dr(inod,2) = dnod_rj(inod)
        dnod_dr(inod,3) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_gradient_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_vect_dr_2(kr_in, kr_out, dnod_rj, dnod_dr)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: dnod_rj(nnod_rj)
!
      real(kind = kreal), intent(inout) :: dnod_dr(nnod_rj)
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
        dnod_dr(inod) =  d1nod_mat_fdm_2(k,-1) * dnod_rj(i_n1)          &
     &                 + d1nod_mat_fdm_2(k, 0) * dnod_rj(inod)          &
     &                 + d1nod_mat_fdm_2(k, 1) * dnod_rj(i_p1)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_vect_dr_2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_1st_diff
