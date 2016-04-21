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
!
      implicit none
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
      use m_spheric_parameter
!
      allocate(sph_rtp1%istack_inod_rtp_smp(0:np_smp))
!
      allocate(sph_rtp1%istack_rtp_kr_smp(0:np_smp))
      allocate(sph_rtp1%istack_rtp_lt_smp(0:np_smp))
      allocate(sph_rtp1%istack_rtp_mp_smp(0:np_smp))
!
      allocate(sph_rtp1%istack_rtp_rt_smp(0:np_smp))
!
      sph_rtp1%istack_inod_rtp_smp = 0
!
      sph_rtp1%istack_rtp_kr_smp = 0
      sph_rtp1%istack_rtp_lt_smp = 0
      sph_rtp1%istack_rtp_mp_smp = 0
!
      sph_rtp1%istack_rtp_rt_smp = 0
!
      allocate(sph_rtm1%istack_inod_rtm_smp(0:np_smp))
!
      allocate(sph_rtm1%istack_rtm_kr_smp(0:np_smp))
      allocate(sph_rtm1%istack_rtm_lt_smp(0:np_smp))
      allocate(sph_rtm1%istack_rtm_m_smp(0:np_smp))
!
      allocate(sph_rtm1%istack_rtm_rt_smp(0:np_smp))
!
      sph_rtm1%istack_inod_rtm_smp = 0
!
      sph_rtm1%istack_rtm_kr_smp = 0
      sph_rtm1%istack_rtm_lt_smp = 0
      sph_rtm1%istack_rtm_m_smp = 0
!
      sph_rtm1%istack_rtm_rt_smp = 0
!
      allocate(sph_rlm1%istack_inod_rlm_smp(0:np_smp))
!
      allocate(sph_rlm1%istack_rlm_kr_smp(0:np_smp))
      allocate(sph_rlm1%istack_rlm_j_smp(0:np_smp))
!
      sph_rlm1%istack_inod_rlm_smp = 0
!
      sph_rlm1%istack_rlm_kr_smp = 0
      sph_rlm1%istack_rlm_j_smp = 0
!
      allocate(sph_rj1%istack_inod_rj_smp(0:np_smp))
!
      allocate(sph_rj1%istack_rj_kr_smp(0:np_smp))
      allocate(sph_rj1%istack_rj_j_smp(0:np_smp))
!
      sph_rj1%istack_inod_rj_smp = 0
!
      sph_rj1%istack_rj_kr_smp =  0
      sph_rj1%istack_rj_j_smp =  0
!
      end subroutine allocate_sph_param_smp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_param_smp
!
      use m_spheric_parameter
!
      deallocate(sph_rtp1%istack_inod_rtp_smp)
      deallocate(sph_rtm1%istack_inod_rtm_smp)
      deallocate(sph_rlm1%istack_inod_rlm_smp)
      deallocate(sph_rj1%istack_inod_rj_smp)
!
      deallocate(sph_rtp1%istack_rtp_kr_smp)
      deallocate(sph_rtp1%istack_rtp_lt_smp, sph_rtp1%istack_rtp_mp_smp)
      deallocate(sph_rtm1%istack_rtm_kr_smp)
      deallocate(sph_rtm1%istack_rtm_lt_smp, sph_rtm1%istack_rtm_m_smp)
      deallocate(sph_rlm1%istack_rlm_kr_smp, sph_rlm1%istack_rlm_j_smp)
      deallocate(sph_rj1%istack_rj_kr_smp, sph_rj1%istack_rj_j_smp)
!
      deallocate(sph_rtm1%istack_rtm_rt_smp)
      deallocate(sph_rtp1%istack_rtp_rt_smp)
!
      end subroutine deallocate_sph_param_smp
!
! -----------------------------------------------------------------------
!
      end module m_spheric_param_smp
