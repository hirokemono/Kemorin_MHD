!>@file   m_spheric_param_smp.f90
!!@brief  module m_spheric_param_smp
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!!
!!@brief  indexing table of speherical harmonics transform
!!
!!@verbatim
!!      subroutine allocate_sph_param_smp
!!      subroutine deallocate_sph_param_smp
!!@endverbatim
!
      module m_spheric_param_smp
!
      use m_precision
      use m_spheric_parameter
!
      implicit none
!
      integer(kind = kint), allocatable :: inod_rtp_smp_stack(:)
      integer(kind = kint), allocatable :: inod_rtm_smp_stack(:)
      integer(kind = kint), allocatable :: inod_rlm_smp_stack(:)
!      integer(kind = kint), allocatable :: inod_rj_smp_stack(:)
!
      integer(kind = kint), allocatable :: idx_rtp_smp_stack(:,:)
      integer(kind = kint), allocatable :: idx_rtm_smp_stack(:,:)
      integer(kind = kint), allocatable :: idx_rlm_smp_stack(:,:)
!
      integer(kind = kint), allocatable :: irt_rtp_smp_stack(:)
      integer(kind = kint), allocatable :: irt_rtm_smp_stack(:)
!
      integer( kind=kint )  ::  maxnod_rtp_smp = 0
      integer( kind=kint )  ::  maxnod_rtm_smp = 0
      integer( kind=kint )  ::  maxnod_rlm_smp = 0
!
      integer( kind=kint )  ::  maxidx_rtp_smp(3) = (/0,0,0/)
      integer( kind=kint )  ::  maxidx_rtm_smp(3) = (/0,0,0/)
      integer( kind=kint )  ::  maxidx_rlm_smp(2) = (/0,0/)
!
      integer( kind=kint )  ::  maxirt_rtp_smp =  0
      integer( kind=kint )  ::  maxirt_rtm_smp =  0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_param_smp
!
      use m_machine_parameter
!
      allocate(inod_rtp_smp_stack(0:np_smp))
      allocate(inod_rtm_smp_stack(0:np_smp))
      allocate(inod_rlm_smp_stack(0:np_smp))
      allocate(sph_rj1%istack_inod_rj_smp(0:np_smp))
!
      allocate(idx_rtp_smp_stack(0:np_smp,3))
      allocate(idx_rtm_smp_stack(0:np_smp,3))
      allocate(idx_rlm_smp_stack(0:np_smp,2))
      allocate(sph_rj1%istack_rj_kr_smp(0:np_smp))
      allocate(sph_rj1%istack_rj_j_smp(0:np_smp))
!
      allocate(irt_rtp_smp_stack(0:np_smp))
      allocate(irt_rtm_smp_stack(0:np_smp))
!
      inod_rtp_smp_stack = 0
      inod_rtm_smp_stack = 0
      inod_rlm_smp_stack = 0
      sph_rj1%istack_inod_rj_smp = 0
!
      idx_rtp_smp_stack = 0
      idx_rtm_smp_stack = 0
      idx_rlm_smp_stack = 0
      sph_rj1%istack_rj_kr_smp =  0
      sph_rj1%istack_rj_j_smp =  0
!
      irt_rtp_smp_stack = 0
      irt_rtm_smp_stack = 0
!
      end subroutine allocate_sph_param_smp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_param_smp
!
      deallocate(inod_rtp_smp_stack)
      deallocate(inod_rtm_smp_stack)
      deallocate(inod_rlm_smp_stack)
      deallocate(sph_rj1%istack_inod_rj_smp)
!
      deallocate(idx_rtp_smp_stack)
      deallocate(idx_rtm_smp_stack)
      deallocate(idx_rlm_smp_stack)
      deallocate(sph_rj1%istack_rj_kr_smp, sph_rj1%istack_rj_j_smp)
!
      deallocate(irt_rtm_smp_stack)
      deallocate(irt_rtp_smp_stack)
!
      maxnod_rtp_smp = 0
      maxnod_rtm_smp = 0
      maxnod_rlm_smp = 0
!
      maxidx_rtp_smp = 0
      maxidx_rtm_smp = 0
      maxidx_rlm_smp = 0
!
      maxirt_rtm_smp = 0
      maxirt_rtp_smp = 0
!
      end subroutine deallocate_sph_param_smp
!
! -----------------------------------------------------------------------
!
      end module m_spheric_param_smp
