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
!!      subroutine const_spherical_1D_comm_table                        &
!!     &         (sph_rtm, sph_rlm, comm_rtm, comm_rlm)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
!!
!!      subroutine allocate_spherical_1D_SR                             &
!!     &         (nnod_rtm, nidx_rtm, nnod_rlm, nidx_rlm)
!!      subroutine deallocate_spherical_1D_SR
!!
!!      subroutine set_spherical_1D_comm_table                          &
!!     &         (nidx_rtm, nneib_domain_rtm, ntot_item_sr_rtm,         &
!!     &          istack_sr_rtm, item_sr_rtm,                           &
!!     &          nidx_rlm, nneib_domain_rlm, ntot_item_sr_rlm,         &
!!     &          istack_sr_rlm, item_sr_rlm)
!!@endverbatim
!
      module m_sph_trans_comm_tbl_1D
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: isend_rtm_1D(:,:)
      integer(kind = kint), allocatable :: isend_rlm_1D(:,:)
!
      integer(kind = kint), allocatable :: irev_rlm_1D(:,:)
      integer(kind = kint), allocatable :: irev_rtm_1D(:,:,:)
!
      private :: allocate_spherical_1D_SR, set_spherical_1D_comm_table
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_spherical_1D_comm_table                          &
     &         (sph_rtm, sph_rlm, comm_rtm, comm_rlm)
!
      use t_sph_trans_comm_tbl
      use t_spheric_rtm_data
      use t_spheric_rlm_data
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
!
!
      call allocate_spherical_1D_SR(sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, &
     &                              sph_rlm%nnod_rlm, sph_rlm%nidx_rlm)
!
      call set_spherical_1D_comm_table                                  &
     &   (sph_rtm%nidx_rtm, comm_rtm%nneib_domain,                      &
     &    comm_rtm%ntot_item_sr, comm_rtm%istack_sr, comm_rtm%item_sr,  &
     &    sph_rlm%nidx_rlm, comm_rlm%nneib_domain,                      &
     &    comm_rlm%ntot_item_sr, comm_rlm%istack_sr, comm_rlm%item_sr)
!
      end subroutine const_spherical_1D_comm_table
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
      subroutine allocate_spherical_1D_SR                               &
     &         (nnod_rtm, nidx_rtm, nnod_rlm, nidx_rlm)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
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
! -----------------------------------------------------------------------
!
      subroutine set_spherical_1D_comm_table                            &
     &         (nidx_rtm, nneib_domain_rtm, ntot_item_sr_rtm,           &
     &          istack_sr_rtm, item_sr_rtm,                             &
     &          nidx_rlm, nneib_domain_rlm, ntot_item_sr_rlm,           &
     &          istack_sr_rlm, item_sr_rlm)
!
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: nneib_domain_rtm
      integer(kind = kint), intent(in) :: ntot_item_sr_rtm
      integer(kind = kint), intent(in)                                  &
     &              :: istack_sr_rtm(0:nneib_domain_rtm)
      integer(kind = kint), intent(in) :: item_sr_rtm(ntot_item_sr_rtm)
!
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: nneib_domain_rlm
      integer(kind = kint), intent(in) :: ntot_item_sr_rlm
      integer(kind = kint), intent(in)                                  &
     &              :: istack_sr_rlm(0:nneib_domain_rlm)
      integer(kind = kint), intent(in) :: item_sr_rlm(ntot_item_sr_rlm)
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
