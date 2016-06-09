!>@file   init_sph_trans.f90
!!@brief  module init_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Initialize spherical harmonics transform
!!
!!@verbatim
!!      subroutine initialize_sph_trans(sph, comms_sph, leg)
!!      subroutine initialize_legendre_trans(sph, comms_sph, leg)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(legendre_4_sph_trans), intent(inout) :: leg
!!@endverbatim
!
      module init_sph_trans
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
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
      subroutine initialize_sph_trans(sph, comms_sph, leg)
!
      use init_FFT_4_sph
      use m_work_4_sph_trans
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(legendre_4_sph_trans), intent(inout) :: leg
!
!
      call initialize_legendre_trans(sph, comms_sph, leg)
      call init_fourier_transform_4_sph                                 &
     &   (ncomp_sph_trans, sph%sph_rtp, comms_sph%comm_rtp)
!
      end subroutine initialize_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine initialize_legendre_trans(sph, comms_sph, leg)
!
      use m_work_4_sph_trans
      use m_FFT_selector
      use schmidt_poly_on_rtm_grid
      use set_legendre_matrices
      use set_params_sph_trans
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(legendre_4_sph_trans), intent(inout) :: leg
!
!
      call allocate_work_4_sph_trans                                    &
     &   (sph%sph_rtm%nidx_rtm, sph%sph_rlm%nidx_rlm)
!
      call radial_4_sph_trans                                           &
     &   (sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
      call set_mdx_rlm_rtm(sph%sph_params%l_truncation,                 &
     &    sph%sph_rtm%nidx_rtm, sph%sph_rlm%nidx_rlm,                   &
     &    sph%sph_rtm%idx_gl_1d_rtm_m, sph%sph_rlm%idx_gl_1d_rlm_j)
!
      call s_cal_schmidt_poly_rtm(sph%sph_params%l_truncation,          &
     &    sph%sph_rj, sph%sph_rtm, sph%sph_rlm, leg)
!
      call set_sin_theta_rtm(sph%sph_rtm%nidx_rtm(2), leg%g_colat_rtm)
!
      call const_sin_theta_rtp(leg, sph%sph_rtm, sph%sph_rtp)
!
      call set_sym_legendre_stack                                       &
     &   (sph%sph_rtm%nidx_rtm(3), lstack_rlm, lstack_even_rlm)
!
      call set_blocks_4_leg_trans(sph, comms_sph)
!
      end subroutine initialize_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_blocks_4_leg_trans(sph, comms_sph)
!
      use calypso_mpi
      use m_machine_parameter
      use m_sph_communicators
      use m_work_4_sph_trans
      use init_spherical_SRs
      use cal_minmax_and_stacks
      use legendre_transform_select
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
!
      if(nvector_legendre .le. 0                                        &
     &     .or. nvector_legendre .gt. sph%sph_rtm%nidx_rtm(2)) then
        nblock_l_rtm =  1
      else
        nblock_l_rtm =  sph%sph_rtm%nidx_rtm(2) / nvector_legendre
      end if
      if(nvector_legendre .le. 0                                        &
     &     .or. nvector_legendre .gt. sph%sph_rlm%nidx_rlm(2)) then
        nblock_j_rlm =  1
      else
        nblock_j_rlm =  sph%sph_rlm%nidx_rlm(2) / nvector_legendre
      end if
!
      call allocate_l_rtm_block
      call count_number_4_smp                                           &
     &   (nblock_l_rtm, ione, sph%sph_rtm%nidx_rtm(2),                  &
     &    lstack_block_rtm, lmax_block_rtm)
!
!
      call split_rtp_comms(comms_sph%comm_rtp%nneib_domain,             &
     &    comms_sph%comm_rtp%id_domain, comms_sph%comm_rj%nneib_domain)
      call init_sph_send_recv_N(ncomp_sph_trans, sph, comms_sph)
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
