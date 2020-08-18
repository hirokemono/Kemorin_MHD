!>@file   copy_para_sph_global_params.f90
!!@brief  module copy_para_sph_global_params
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!
!>@brief  Set control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine copy_para_sph_param_from_ctl(sph_org, num_pe, sph)
!!      subroutine copy_para_global_sph_resolution(sph_org, num_pe, sph)
!!        integer(kind = kint), intent(in) :: num_pe
!!        type(sph_grids), intent(in) :: sph_org
!!        type(sph_grids), intent(inout) :: sph(num_pe)
!!@endverbatim
!
      module copy_para_sph_global_params
!
      use m_precision
!
      use t_spheric_parameter
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_para_sph_param_from_ctl(sph_org, num_pe, sph)
!
      integer(kind = kint), intent(in) :: num_pe
      type(sph_grids), intent(in) :: sph_org
      type(sph_grids), intent(inout) :: sph(num_pe)
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, num_pe
        sph(ip)%sph_params%iflag_shell_mode                             &
     &           = sph_org%sph_params%iflag_shell_mode
        sph(ip)%sph_params%iflag_radial_grid                            &
     &           = sph_org%sph_params%iflag_radial_grid
!
        sph(ip)%sph_rj%iflag_rj_center = sph_org%sph_rj%iflag_rj_center
!
        sph(ip)%sph_params%l_truncation                                 &
     &           = sph_org%sph_params%l_truncation
        sph(ip)%sph_params%m_folding                                    &
     &           = sph_org%sph_params%m_folding
!
        sph(ip)%sph_params%nlayer_2_center                              &
     &           = sph_org%sph_params%nlayer_2_center
        sph(ip)%sph_params%nlayer_ICB                                   &
     &           = sph_org%sph_params%nlayer_ICB
        sph(ip)%sph_params%nlayer_CMB                                   &
     &           = sph_org%sph_params%nlayer_CMB
        sph(ip)%sph_params%nlayer_mid_OC                                &
     &           = sph_org%sph_params%nlayer_mid_OC
!
        sph(ip)%sph_params%radius_ICB                                   &
     &           = sph_org%sph_params%radius_ICB
        sph(ip)%sph_params%radius_CMB                                   &
     &           = sph_org%sph_params%radius_CMB
!
        sph(ip)%sph_rtp%nidx_global_rtp(1:2)                            &
     &           = sph_org%sph_rtp%nidx_global_rtp(1:2)
      end do
!$omp end parallel do
!
      end subroutine copy_para_sph_param_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_para_global_sph_resolution(sph_org, num_pe, sph)
!
      integer(kind = kint), intent(in) :: num_pe
      type(sph_grids), intent(in) :: sph_org
      type(sph_grids), intent(inout) :: sph(num_pe)
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, num_pe
        sph(ip)%sph_rtp%nidx_global_rtp(1:3)                            &
     &      = sph_org%sph_rtp%nidx_global_rtp(1:3)
        sph(ip)%sph_rtm%nidx_global_rtm(1:3)                            &
     &      = sph_org%sph_rtm%nidx_global_rtm(1:3)
        sph(ip)%sph_rlm%nidx_global_rlm(1:2)                            &
     &      = sph_org%sph_rlm%nidx_global_rlm(1:2)
        sph(ip)%sph_rj%nidx_global_rj(1:2)                              &
     &      = sph_org%sph_rj%nidx_global_rj(1:2)
      end do
!$omp end parallel do
!
      end subroutine copy_para_global_sph_resolution
!
! -----------------------------------------------------------------------
!
      end module copy_para_sph_global_params
