!>@file   m_sph_trans_comm_tbl_1D.f90
!!@brief  module m_sph_trans_comm_tbl_1D
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Copy data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine allocate_spherical_1D_SR
!!      subroutine deallocate_spherical_1D_SR
!!      subroutine set_spherical_1D_comm_table
!!@endverbatim
!
      module m_sph_trans_comm_tbl_1D
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      implicit none
!
      integer(kind = kint), allocatable :: isend_rtm_1D(:,:)
      integer(kind = kint), allocatable :: isend_rlm_1D(:,:)
!
      integer(kind = kint), allocatable :: irev_rlm_1D(:,:)
      integer(kind = kint), allocatable :: irev_rtm_1D(:,:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_spherical_1D_SR
!
!
      allocate(irev_rlm_1D(nidx_rlm(2),nidx_rlm(1)))
      allocate(isend_rtm_1D(nnod_rtm,3))
!
      allocate(irev_rtm_1D(nidx_rtm(2),nidx_rtm(1),nidx_rtm(3)))
      allocate(isend_rlm_1D(nnod_rlm,2))
!
      irev_rlm_1D =  0
      isend_rtm_1D = 0
      irev_rtm_1D =  0
      isend_rlm_1D = 0
!
      end subroutine allocate_spherical_1D_SR
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_spherical_1D_SR
!
!
      deallocate(irev_rlm_1D, isend_rtm_1D)
      deallocate(irev_rtm_1D, isend_rlm_1D)
!
      end subroutine deallocate_spherical_1D_SR
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_spherical_1D_comm_table
!
      integer(kind = kint) :: inum, inod, j_rlm, k_rlm
      integer(kind = kint) :: lnod, k_rtm, l_rtm, m_rtm
!
!
      do inum = 1, istack_sr_rlm(nneib_domain_rlm)
        inod = item_sr_rlm(inum)
        j_rlm = 1 + mod((inod-1),nidx_rlm(2))
        k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
        isend_rlm_1D(inum,1) = k_rlm
        isend_rlm_1D(inum,2) = j_rlm
!
        irev_rlm_1D(j_rlm,k_rlm) = inum
      end do
!
!
      do inum = 1, istack_sr_rtm(nneib_domain_rtm)
        inod = item_sr_rtm(inum)
        l_rtm = 1 + mod((inod-1),nidx_rtm(2))
        lnod = 1 + (inod - l_rtm) / nidx_rtm(2)
        k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
        m_rtm = 1 + (lnod - k_rtm) / nidx_rtm(1)
!
        isend_rtm_1D(inum,1) = k_rtm
        isend_rtm_1D(inum,2) = l_rtm
        isend_rtm_1D(inum,3) = m_rtm
!
        irev_rtm_1D(l_rtm,k_rtm,m_rtm) = inum
      end do
!
      end subroutine set_spherical_1D_comm_table
!
! -----------------------------------------------------------------------
!
      end module m_sph_trans_comm_tbl_1D
