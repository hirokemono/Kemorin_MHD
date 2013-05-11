!>@file   cal_sph_exp_1st_diff_ele.f90
!!@brief  module cal_sph_exp_1st_diff_ele
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate first radial derivative for spectr data
!!        for center of the element
!!
!!@verbatim
!!      subroutine cal_sph_vect_dr_ele_2(kr_in, kr_out,                 &
!!     &          dele_rj, dnod_dr)
!!@endverbatim
!!
!!@n @param kr_in      Address of inner boundary
!!@n @param kr_out     Address of outer boundary
!!@n @param dnod_rj(nnod_rj)      Input spectr data
!!@n @param dnod_dr(nnod_rj,nd)   Gradient or radial derivative of field
!
      module cal_sph_exp_1st_diff_ele
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_vect_dr_ele_2(kr_in, kr_out,                   &
     &          dele_rj, dnod_dr)
!
      use m_fdm_2e_coefs
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: dele_rj(nnod_rj)
!
      real(kind = kreal), intent(inout) :: dnod_dr(nnod_rj)
!
      integer(kind = kint) :: inod, i_p1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = (kr_in-1) * nidx_rj(2) + 1
      ied = (kr_out-1) * nidx_rj(2)
!$omp parallel do private(inod,i_p1,j,k)
      do inod = ist, ied
        i_p1 = inod + nidx_rj(2)
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        dnod_dr(inod) =  d1nod_mat_fdm_2e(k, 0) * dele_rj(inod)         &
     &                 + d1nod_mat_fdm_2e(k, 1) * dele_rj(i_p1)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_vect_dr_ele_2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_1st_diff_ele
