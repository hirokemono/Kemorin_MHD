!>@file   count_num_sph_smp.f90
!!@brief  module count_num_sph_smp
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  
!!
!!@verbatim
!!      subroutine s_count_num_sph_smp(ierr)
!!@endverbatim
!!
!!@param  ierr  Error flag
!
      module count_num_sph_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Error message
      character(len=kchara), parameter :: e_message_Rsmp                &
     &  = '# of r-grid for SPH trans. should be more than SMP threads'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_count_num_sph_smp(ierr)
!
      use m_machine_parameter
      use m_spheric_parameter
!
      use t_spheric_rtp_data
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_spheric_rj_data
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: num
!
      integer(kind = kint)  ::  maxnod_rtp_smp = 0
      integer(kind = kint)  ::  maxnod_rtm_smp = 0
      integer(kind = kint)  ::  maxnod_rlm_smp = 0
      integer(kind = kint)  ::  maxnod_rj_smp =  0
!
      integer(kind = kint) :: maxirt_rtm_smp =  0
!
      integer(kind = kint)  ::  maxidx_rtp_smp(3) = (/0,0,0/)
      integer(kind = kint)  ::  maxidx_rlm_smp(2) = (/0,0/)
!
      integer(kind = kint) :: maxidx_rj_smp(2) =  (/0,0/)
!
!
      ierr = 0
      if(nidx_rtm(1) .lt. np_smp) then
        ierr = 72
        return
      end if
!
      call alloc_rtp_param_smp(sph_rtp1)
      call alloc_rtm_param_smp(sph_rtm1)
      call alloc_rlm_param_smp(sph_rlm1)
      call alloc_rj_param_smp(sph_rj1)
!
      call count_number_4_smp(np_smp, ione, nnod_rtp,                   &
     &    sph_rtp1%istack_inod_rtp_smp, maxnod_rtp_smp)
      call count_number_4_smp(np_smp, ione, nnod_rtm,                   &
     &    sph_rtm1%istack_inod_rtm_smp, maxnod_rtm_smp)
      call count_number_4_smp(np_smp, ione, nnod_rlm,                   &
     &    sph_rlm1%istack_inod_rlm_smp, maxnod_rlm_smp)
      call count_number_4_smp(np_smp, ione, nnod_rj,                    &
     &    sph_rj1%istack_inod_rj_smp, maxnod_rj_smp)
!
      call count_number_4_smp(np_smp, ione, nidx_rtp(1),                &
     &    sph_rtp1%istack_rtp_kr_smp, maxidx_rtp_smp(1) )
      call count_number_4_smp(np_smp, ione, nidx_rtp(2),                &
     &    sph_rtp1%istack_rtp_lt_smp, maxidx_rtp_smp(2) )
      call count_number_4_smp(np_smp, ione, nidx_rtp(3),                &
     &    sph_rtp1%istack_rtp_mp_smp, maxidx_rtp_smp(3) )
!
      call count_number_4_smp(np_smp, ione, nidx_rtm(1),                &
     &    sph_rtm1%istack_rtm_kr_smp, sph_rtm1%maxidx_rtm_smp(1))
      call count_number_4_smp(np_smp, ione, nidx_rtm(2),                &
     &    sph_rtm1%istack_rtm_lt_smp, sph_rtm1%maxidx_rtm_smp(2))
      call count_number_4_smp(np_smp, ione, nidx_rtm(3),                &
     &    sph_rtm1%istack_rtm_m_smp,  sph_rtm1%maxidx_rtm_smp(3))
!
      call count_number_4_smp(np_smp, ione, nidx_rlm(1),                &
     &    sph_rlm1%istack_rlm_kr_smp, maxidx_rlm_smp(1) )
      call count_number_4_smp(np_smp, ione, nidx_rlm(2),                &
     &    sph_rlm1%istack_rlm_j_smp, maxidx_rlm_smp(2) )
!
      call count_number_4_smp(np_smp, ione, nidx_rj(1),                 &
     &    sph_rj1%istack_rj_kr_smp, maxidx_rj_smp(1) )
      call count_number_4_smp(np_smp, ione, nidx_rj(2),                 &
     &    sph_rj1%istack_rj_j_smp, maxidx_rj_smp(2) )
!
      num = nidx_rtp(1)*nidx_rtp(2)
      call count_number_4_smp(np_smp, ione, num,                        &
     &    sph_rtp1%istack_rtp_rt_smp, sph_rtp1%maxirt_rtp_smp)
!
      num = nidx_rtm(1)*nidx_rtm(2)
      call count_number_4_smp(np_smp, ione, num,                        &
     &    sph_rtm1%istack_rtm_rt_smp, maxirt_rtm_smp )
!
      end subroutine s_count_num_sph_smp
!
!  ---------------------------------------------------------------------
!
      end module count_num_sph_smp
