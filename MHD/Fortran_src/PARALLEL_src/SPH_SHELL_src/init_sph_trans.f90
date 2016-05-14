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
!!     &         (sph_param1, sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1,    &
!!     &          comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1)
!!      subroutine initialize_legendre_trans                            &
!!     &         (sph_param1, sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1,    &
!!     &          comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1)
!!        type(sph_shell_parameters), intent(in) :: sph_param1
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp1
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm1
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm1
!!        type(sph_rj_grid), intent(inout) :: sph_rj1
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp1
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm1
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm1
!!        type(sph_comm_tbl), intent(inout) :: comm_rj1
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
     &         (sph_param1, sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1,      &
     &          comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1)
!
      use init_FFT_4_sph
      use m_work_4_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_param1
      type(sph_rtp_grid), intent(inout) :: sph_rtp1
      type(sph_rtm_grid), intent(inout) :: sph_rtm1
      type(sph_rlm_grid), intent(inout) :: sph_rlm1
      type(sph_rj_grid), intent(inout) :: sph_rj1
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp1
      type(sph_comm_tbl), intent(inout) :: comm_rtm1
      type(sph_comm_tbl), intent(inout) :: comm_rlm1
      type(sph_comm_tbl), intent(inout) :: comm_rj1
!
!
      call initialize_legendre_trans                                    &
     &   (sph_param1, sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1,            &
     &    comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1)
      call init_fourier_transform_4_sph                                 &
     &   (ncomp_sph_trans, sph_rtp1, comm_rtp1)
!
      end subroutine initialize_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine initialize_legendre_trans                              &
     &         (sph_param1, sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1,      &
     &          comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1)
!
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_FFT_selector
      use schmidt_poly_on_rtm_grid
      use set_legendre_matrices
      use set_params_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_param1
      type(sph_rtp_grid), intent(inout) :: sph_rtp1
      type(sph_rtm_grid), intent(inout) :: sph_rtm1
      type(sph_rlm_grid), intent(inout) :: sph_rlm1
      type(sph_rj_grid), intent(inout) :: sph_rj1
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp1
      type(sph_comm_tbl), intent(inout) :: comm_rtm1
      type(sph_comm_tbl), intent(inout) :: comm_rlm1
      type(sph_comm_tbl), intent(inout) :: comm_rj1
!
!
      call allocate_work_4_sph_trans                                    &
     &   (sph_rtp1%nidx_rtp, sph_rtm1%nidx_rtm, sph_rlm1%nidx_rlm)
!
      call radial_4_sph_trans(sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1)
      call set_mdx_rlm_rtm(sph_param1%l_truncation,                     &
     &    sph_rtm1%nidx_rtm, sph_rlm1%nidx_rlm,                         &
     &    sph_rtm1%idx_gl_1d_rtm_m, sph_rlm1%idx_gl_1d_rlm_j)
!
      call s_cal_schmidt_poly_rtm                                       &
     &   (sph_param1%l_truncation, sph_rj1, sph_rtm1, sph_rlm1)
!
      call set_sin_theta_rtm(sph_rtm1%nidx_rtm(2))
      call set_sin_theta_rtp                                            &
     &   (sph_rtp1%nidx_rtp(2), sph_rtp1%idx_gl_1d_rtp_t)
!
      call allocate_trans_schmidt_rtm                                   &
     &   (sph_rtm1%nidx_rtm(2), sph_rlm1%nidx_rlm(2))
      call set_trans_legendre_rtm                                       &
     &   (sph_rtm1%nidx_rtm(2), sph_rlm1%nidx_rlm(2))
!
      call allocate_hemi_schmidt_rtm                                    &
     &   (sph_rtm1%nidx_rtm(2), sph_rlm1%nidx_rlm(2))
      call set_legendre_hemispher_rtm(sph_rtm1%nidx_rtm(3))
!
      call set_blocks_4_leg_trans                                       &
     &   (sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1,                        &
     &    comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1)
!
      end subroutine initialize_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_blocks_4_leg_trans                                 &
     &         (sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1,                  &
     &          comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1)
!
      use calypso_mpi
      use m_machine_parameter
      use m_sph_communicators
      use m_work_4_sph_trans
      use init_spherical_SRs
      use cal_minmax_and_stacks
      use legendre_transform_select
!
      type(sph_rtp_grid), intent(in) :: sph_rtp1
      type(sph_rtm_grid), intent(in) :: sph_rtm1
      type(sph_rlm_grid), intent(in) :: sph_rlm1
      type(sph_rj_grid), intent(in) :: sph_rj1
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp1
      type(sph_comm_tbl), intent(inout) :: comm_rtm1
      type(sph_comm_tbl), intent(inout) :: comm_rlm1
      type(sph_comm_tbl), intent(inout) :: comm_rj1
!
!
      if(nvector_legendre .le. 0                                        &
     &     .or. nvector_legendre .gt. sph_rtm1%nidx_rtm(2)) then
        nblock_l_rtm =  1
      else
        nblock_l_rtm =  sph_rtm1%nidx_rtm(2) / nvector_legendre
      end if
      if(nvector_legendre .le. 0                                        &
     &     .or. nvector_legendre .gt. sph_rlm1%nidx_rlm(2)) then
        nblock_j_rlm =  1
      else
        nblock_j_rlm =  sph_rlm1%nidx_rlm(2) / nvector_legendre
      end if
!
      call allocate_l_rtm_block
      call count_number_4_smp(nblock_l_rtm, ione, sph_rtm1%nidx_rtm(2), &
     &    lstack_block_rtm, lmax_block_rtm)
      call count_number_4_smp(nblock_j_rlm, ione, sph_rlm1%nidx_rlm(2), &
     &    jstack_block_rlm, jmax_block_rlm)
!
!
      call split_rtp_comms(comm_rtp1%nneib_domain, comm_rtp1%id_domain, &
     &    comm_rj1%nneib_domain) 
      call init_sph_send_recv_N                                         &
     &   (ncomp_sph_trans, sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1,       &
     &    comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1)
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
