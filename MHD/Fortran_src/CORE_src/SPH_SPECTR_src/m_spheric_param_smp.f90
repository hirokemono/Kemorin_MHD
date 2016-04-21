!>@file   m_spheric_param_smp.f90
!!@brief  module m_spheric_param_smp
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!!
!!@brief  indexing table of speherical harmonics transform
!!
!!@verbatim
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
      subroutine deallocate_sph_param_smp
!
      use m_spheric_parameter
!
!
      call dealloc_rtp_param_smp(sph_rtp1)
      call dealloc_rtm_param_smp(sph_rtm1)
      call dealloc_rlm_param_smp(sph_rlm1)
      call dealloc_rj_param_smp(sph_rj1)
!
      end subroutine deallocate_sph_param_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_rtp_param_smp(sph_rtp)
!
      use m_machine_parameter
      use t_spheric_parameter
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
!
      allocate(sph_rtp%istack_inod_rtp_smp(0:np_smp))
!
      allocate(sph_rtp%istack_rtp_kr_smp(0:np_smp))
      allocate(sph_rtp%istack_rtp_lt_smp(0:np_smp))
      allocate(sph_rtp%istack_rtp_mp_smp(0:np_smp))
!
      allocate(sph_rtp%istack_rtp_rt_smp(0:np_smp))
!
      sph_rtp%istack_inod_rtp_smp = 0
!
      sph_rtp%istack_rtp_kr_smp = 0
      sph_rtp%istack_rtp_lt_smp = 0
      sph_rtp%istack_rtp_mp_smp = 0
!
      sph_rtp%istack_rtp_rt_smp = 0
!
      end subroutine alloc_rtp_param_smp
!
! -----------------------------------------------------------------------
!
      subroutine alloc_rtm_param_smp(sph_rtm)
!
      use m_machine_parameter
      use t_spheric_parameter
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
!
      allocate(sph_rtm%istack_inod_rtm_smp(0:np_smp))
!
      allocate(sph_rtm%istack_rtm_kr_smp(0:np_smp))
      allocate(sph_rtm%istack_rtm_lt_smp(0:np_smp))
      allocate(sph_rtm%istack_rtm_m_smp(0:np_smp))
!
      allocate(sph_rtm%istack_rtm_rt_smp(0:np_smp))
!
      sph_rtm%istack_inod_rtm_smp = 0
!
      sph_rtm%istack_rtm_kr_smp = 0
      sph_rtm%istack_rtm_lt_smp = 0
      sph_rtm%istack_rtm_m_smp = 0
!
      sph_rtm%istack_rtm_rt_smp = 0
!
      end subroutine alloc_rtm_param_smp
!
! -----------------------------------------------------------------------
!
      subroutine alloc_rlm_param_smp(sph_rlm)
!
      use m_machine_parameter
      use t_spheric_parameter
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
!
      allocate(sph_rlm%istack_inod_rlm_smp(0:np_smp))
!
      allocate(sph_rlm%istack_rlm_kr_smp(0:np_smp))
      allocate(sph_rlm%istack_rlm_j_smp(0:np_smp))
!
      sph_rlm%istack_inod_rlm_smp = 0
!
      sph_rlm%istack_rlm_kr_smp = 0
      sph_rlm%istack_rlm_j_smp = 0
!
      end subroutine alloc_rlm_param_smp
!
! -----------------------------------------------------------------------
!
      subroutine alloc_rj_param_smp(sph_rj)
!
      use m_machine_parameter
      use t_spheric_parameter
!
      type(sph_rj_grid), intent(inout) :: sph_rj
!
!
      allocate(sph_rj%istack_inod_rj_smp(0:np_smp))
!
      allocate(sph_rj%istack_rj_kr_smp(0:np_smp))
      allocate(sph_rj%istack_rj_j_smp(0:np_smp))
!
      sph_rj%istack_inod_rj_smp = 0
!
      sph_rj%istack_rj_kr_smp =  0
      sph_rj%istack_rj_j_smp =  0
!
      end subroutine alloc_rj_param_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_rtp_param_smp(sph_rtp)
!
      use t_spheric_parameter
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
!
      deallocate(sph_rtp%istack_inod_rtp_smp)
      deallocate(sph_rtp%istack_rtp_kr_smp)
      deallocate(sph_rtp%istack_rtp_lt_smp, sph_rtp%istack_rtp_mp_smp)
      deallocate(sph_rtp%istack_rtp_rt_smp)
!
      end subroutine dealloc_rtp_param_smp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_rtm_param_smp(sph_rtm)
!
      use t_spheric_parameter
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
!
      deallocate(sph_rtm%istack_inod_rtm_smp)
      deallocate(sph_rtm%istack_rtm_kr_smp)
      deallocate(sph_rtm%istack_rtm_lt_smp, sph_rtm%istack_rtm_m_smp)
      deallocate(sph_rtm%istack_rtm_rt_smp)
!
      end subroutine dealloc_rtm_param_smp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_rlm_param_smp(sph_rlm)
!
      use t_spheric_parameter
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
!
      deallocate(sph_rlm%istack_inod_rlm_smp)
      deallocate(sph_rlm%istack_rlm_kr_smp, sph_rlm%istack_rlm_j_smp)
!
      end subroutine dealloc_rlm_param_smp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_rj_param_smp(sph_rj)
!
      use t_spheric_parameter
!
      type(sph_rj_grid), intent(inout) :: sph_rj
!
!
      deallocate(sph_rj%istack_inod_rj_smp)
      deallocate(sph_rj%istack_rj_kr_smp, sph_rj%istack_rj_j_smp)
!
      end subroutine dealloc_rj_param_smp
!
! -----------------------------------------------------------------------
!
      end module m_spheric_param_smp
