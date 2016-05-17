!>@file   init_sph_trans.f90
!!@brief  module init_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Initialize spherical harmonics transform
!!
!!@verbatim
!!      subroutine initialize_sph_trans                                 &
!!     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,        &
!!     &          comm_rtp, comm_rtm, comm_rlm, comm_rj)
!!      subroutine initialize_legendre_trans                            &
!!     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,        &
!!     &          comm_rtp, comm_rtm, comm_rlm, comm_rj)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rj
!!@endverbatim
!
      module init_sph_trans
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
!
      implicit none
!
      private :: set_blocks_4_leg_trans
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine initialize_sph_trans                                   &
     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,          &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj)
!
      use init_FFT_4_sph
      use m_work_4_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
!
      call initialize_legendre_trans                                    &
     &   (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,                &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj)
      call init_fourier_transform_4_sph                                 &
     &   (ncomp_sph_trans, sph_rtp, comm_rtp)
!
      end subroutine initialize_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine initialize_legendre_trans                              &
     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,          &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj)
!
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_FFT_selector
      use schmidt_poly_on_rtm_grid
      use set_legendre_matrices
      use set_params_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
!
      call allocate_work_4_sph_trans                                    &
     &   (sph_rtp%nidx_rtp, sph_rtm%nidx_rtm, sph_rlm%nidx_rlm)
!
      call radial_4_sph_trans(sph_rtp, sph_rtm, sph_rlm, sph_rj)
      call set_mdx_rlm_rtm(sph_params%l_truncation,                     &
     &    sph_rtm%nidx_rtm, sph_rlm%nidx_rlm,                           &
     &    sph_rtm%idx_gl_1d_rtm_m, sph_rlm%idx_gl_1d_rlm_j)
!
      call s_cal_schmidt_poly_rtm                                       &
     &   (sph_params%l_truncation, sph_rj, sph_rtm, sph_rlm)
!
      call set_sin_theta_rtm(sph_rtm%nidx_rtm(2))
      call set_sin_theta_rtp                                            &
     &   (sph_rtp%nidx_rtp(2), sph_rtp%idx_gl_1d_rtp_t)
!
      call allocate_trans_schmidt_rtm                                   &
     &   (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2))
      call set_trans_legendre_rtm                                       &
     &   (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2))
!
      call allocate_hemi_schmidt_rtm                                    &
     &   (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2))
      call set_legendre_hemispher_rtm(sph_rtm%nidx_rtm(3))
!
      call set_blocks_4_leg_trans                                       &
     &   (sph_rtp, sph_rtm, sph_rlm, sph_rj,                            &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj)
!
      end subroutine initialize_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_blocks_4_leg_trans                                 &
     &         (sph_rtp, sph_rtm, sph_rlm, sph_rj,                      &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj)
!
      use calypso_mpi
      use m_machine_parameter
      use m_sph_communicators
      use m_work_4_sph_trans
      use init_spherical_SRs
      use cal_minmax_and_stacks
      use legendre_transform_select
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
!
      if(nvector_legendre .le. 0                                        &
     &     .or. nvector_legendre .gt. sph_rtm%nidx_rtm(2)) then
        nblock_l_rtm =  1
      else
        nblock_l_rtm =  sph_rtm%nidx_rtm(2) / nvector_legendre
      end if
      if(nvector_legendre .le. 0                                        &
     &     .or. nvector_legendre .gt. sph_rlm%nidx_rlm(2)) then
        nblock_j_rlm =  1
      else
        nblock_j_rlm =  sph_rlm%nidx_rlm(2) / nvector_legendre
      end if
!
      call allocate_l_rtm_block
      call count_number_4_smp(nblock_l_rtm, ione, sph_rtm%nidx_rtm(2),  &
     &    lstack_block_rtm, lmax_block_rtm)
      call count_number_4_smp(nblock_j_rlm, ione, sph_rlm%nidx_rlm(2),  &
     &    jstack_block_rlm, jmax_block_rlm)
!
!
      call split_rtp_comms(comm_rtp%nneib_domain, comm_rtp%id_domain,   &
     &    comm_rj%nneib_domain) 
      call init_sph_send_recv_N                                         &
     &   (ncomp_sph_trans, sph_rtp, sph_rtm, sph_rlm, sph_rj,           &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj)
!
      if(my_rank .ne. 0) return
      write(*,*) 'Vector length for Legendre transform:',               &
     &          nvector_legendre
      write(*,*) 'Block number for meridinal grid: ', nblock_l_rtm
      write(*,*) 'Block number for Legendre transform: ', nblock_j_rlm
!
      end subroutine set_blocks_4_leg_trans
!
! -----------------------------------------------------------------------
!
      end module init_sph_trans
