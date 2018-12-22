!>@file   fem_skl_grad.f90
!!@brief  module fem_skl_grad
!!
!!@author H. Matsui and H.Okuda
!!@date Programmed in July 2000 (ver 1.1)
!!      Modified in Oct., 2006
!!      Modified in Oct., 2011
!!      Modified in Dec., 2018
!!
!> @brief FEM integration for 1-D gradient
!!
!!@verbatim
!!      subroutine fem_skl_all_grad                                     &
!!     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,&
!!     &          max_int_point, maxtot_int_1d, int_start1, owe1d,      &
!!     &          n_int, k2, ntot_int_1d, xjac_1d, an_1d, dnx_1d,       &
!!     &          scalar_1, sk)
!!      subroutine fem_skl_grp_grad(numele, nnod_4_e1, nnod_4_e2,       &
!!     &          np_smp, iele_fsmp_stack, nele_grp, iele_grp,          &
!!     &          max_int_point, maxtot_int_1d, int_start1, owe1d,      &
!!     &          n_int, k2, ntot_int_1d, xjac_1d, an_1d, dnx_1d,       &
!!     &          scalar_1, sk)
!!@endverbatim
!
      module fem_skl_grad
!
      use m_precision
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skl_all_grad                                       &
     &         (numele, nnod_4_e1, nnod_4_e2, np_smp, iele_fsmp_stack,  &
     &          max_int_point, maxtot_int_1d, int_start1, owe1d,        &
     &          n_int, k2, ntot_int_1d, xjac_1d, an_1d, dnx_1d,         &
     &          scalar_1, sk)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_1d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_1d
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
      real(kind = kreal),   intent(in) :: owe1d(maxtot_int_1d)
!
      real(kind=kreal),   intent(in) :: xjac_1d(numele,ntot_int_1d)
      real(kind=kreal),   intent(in) :: an_1d(nnod_4_e1, ntot_int_1d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx_1d(numele,nnod_4_e2,ntot_int_1d,3)
      real(kind=kreal),   intent(in) :: scalar_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk(numele,nnod_4_e1)
!
      integer(kind=kint) :: k1, iele, ii, ix
!
!
      do ii= 1, n_int
        ix = int_start1(n_int) + ii
        do k1 = 1, nnod_4_e1
!
!cdir nodep
          do iele = iele_fsmp_stack(0)+1, iele_fsmp_stack(np_smp)
            sk(iele,k1) = sk(iele,k1) + scalar_1(iele)                  &
     &                   * an_1d(k1,ix) * dnx_1d(iele,k2,ix,1)          &
     &                   * xjac_1d(iele,ix) * owe1d(ix)
          end do
        end do
      end do
!
      end subroutine fem_skl_all_grad
!
!-----------------------------------------------------------------------
!
      subroutine fem_skl_grp_grad(numele, nnod_4_e1, nnod_4_e2,         &
     &          np_smp, iele_fsmp_stack, nele_grp, iele_grp,            &
     &          max_int_point, maxtot_int_1d, int_start1, owe1d,        &
     &          n_int, k2, ntot_int_1d, xjac_1d, an_1d, dnx_1d,         &
     &          scalar_1, sk)
!
      integer(kind=kint), intent(in) :: numele, nnod_4_e1, nnod_4_e2
      integer(kind=kint), intent(in) :: np_smp, ntot_int_1d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_1d
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
      real(kind = kreal),   intent(in) :: owe1d(maxtot_int_1d)
!
      real(kind=kreal),   intent(in) :: xjac_1d(numele, ntot_int_1d)
      real(kind=kreal),   intent(in) :: an_1d(nnod_4_e1, ntot_int_1d)
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx_1d(numele,nnod_4_e2,ntot_int_1d)
      real(kind=kreal),   intent(in) :: scalar_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk(numele,nnod_4_e1)
!
      integer(kind=kint) :: k1, inum, iele, ii, ix
!
!
      do ii = 1, n_int
        ix = int_start1(n_int) + ii
        do k1 = 1, nnod_4_e1
          do inum = iele_fsmp_stack(0)+1, iele_fsmp_stack(np_smp)
            iele = iele_grp(inum)
            sk(iele,k1) = sk(iele,k1) + scalar_1(iele)                  &
     &                   * an_1d(k1,ix) * dnx_1d(iele,k2,ix)            &
     &                   * xjac_1d(iele,ix) * owe1d(ix)
          end do
        end do
      end do
!
      end subroutine fem_skl_grp_grad
!
!-----------------------------------------------------------------------
!
      end module fem_skl_grad
