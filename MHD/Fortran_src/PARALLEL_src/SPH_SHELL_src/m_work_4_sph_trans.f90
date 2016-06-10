!>@file   m_work_4_sph_trans.f90
!!@brief  module m_work_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  global addresses for spherical transform
!!        communication test
!!
!!@verbatim
!!      subroutine allocate_work_4_sph_trans(nidx_rtm, nidx_rlm)
!!      subroutine allocate_l_rtm_block
!!
!!      subroutine deallocate_work_4_sph_trans
!!      subroutine deallocate_l_rtm_block
!!@endverbatim
!!
      module m_work_4_sph_trans
!
      use m_precision
      use t_work_4_sph_trans
!
      implicit none
!
      type(index_4_sph_trans), save :: idx_trns1
!idx_trns1%lstack_rlm
!
!>      total number of components for spherical harmonics transform
      integer(kind = kint) :: ncomp_sph_trans
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint) :: nvector_sph_trans
!>      total number of svalars for spherical harmonics transform
      integer(kind = kint) :: nscalar_sph_trans
!
!
!>      Spectr harmonics order for Legendre transform
      integer(kind = kint), allocatable :: mdx_p_rlm_rtm(:)
!>      Spectr harmonics order for Legendre transform
      integer(kind = kint), allocatable :: mdx_n_rlm_rtm(:)
!>      @f$ 1 / \sin \theta @f$  for Legendre transform
      real(kind = kreal), allocatable :: asin_theta_1d_rtm(:)
!
!>      Number of block for grid in @f$ \theta @f$-direction
      integer(kind = kint) :: nblock_l_rtm = 1
!>      End point of each block for grid in @f$ \theta @f$-direction
      integer(kind = kint), allocatable :: lstack_block_rtm(:)
!>      Maximum point of each block for grid in @f$ \theta @f$-direction
      integer(kind = kint) :: lmax_block_rtm
!
!>      Number of block for grid in hermonics degree
      integer(kind = kint) :: nblock_j_rlm = 1
!
!
!>      End address of spherical harmonics order for SMP parallelization
!      integer(kind = kint), allocatable :: lstack_rlm(:)
!>      Maximum point of each block for grid in  hermonics degree
      integer(kind = kint) :: maxdegree_rlm
!>      End address of spherical harmonics order for SMP parallelization
!      integer(kind = kint), allocatable :: lstack_even_rlm(:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_4_sph_trans(nidx_rtm, nidx_rlm)
!
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
!
!
      allocate(idx_trns1%lstack_rlm(0:nidx_rtm(3)))
      allocate(idx_trns1%lstack_even_rlm(0:nidx_rtm(3)))
!
      allocate(mdx_p_rlm_rtm(nidx_rlm(2)))
      allocate(mdx_n_rlm_rtm(nidx_rlm(2)))
      allocate(asin_theta_1d_rtm(nidx_rtm(2)))
!
      idx_trns1%lstack_rlm = 0
      idx_trns1%lstack_even_rlm = 0
      maxdegree_rlm = 0
      mdx_p_rlm_rtm = 0
      mdx_n_rlm_rtm = 0
      asin_theta_1d_rtm = 0.0d0
!
      end subroutine allocate_work_4_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine allocate_l_rtm_block
!
!
      allocate(lstack_block_rtm(0:nblock_l_rtm))
      lstack_block_rtm = 0
      lmax_block_rtm = 0
!
      end subroutine allocate_l_rtm_block
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_4_sph_trans
!
!
      deallocate(idx_trns1%lstack_rlm, idx_trns1%lstack_even_rlm)
      deallocate(mdx_p_rlm_rtm, mdx_n_rlm_rtm)
      deallocate(asin_theta_1d_rtm)
!
      maxdegree_rlm =   0
!
      end subroutine deallocate_work_4_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_l_rtm_block
!
!
      deallocate(lstack_block_rtm)
      lmax_block_rtm = 0
!
      end subroutine deallocate_l_rtm_block
!
! ----------------------------------------------------------------------
!
      end module m_work_4_sph_trans
