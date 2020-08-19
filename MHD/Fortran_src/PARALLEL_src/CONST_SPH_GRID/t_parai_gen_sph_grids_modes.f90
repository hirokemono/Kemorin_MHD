!>@file   t_parai_gen_sph_grids_modes.f90
!!@brief  module t_parai_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine alloc_parallel_sph_params(num_pe, para_sph)
!!      subroutine dealloc_parallel_sph_params(para_sph)
!!        type(parallel_sph_params), intent(inout) :: para_sph
!!
!!      subroutine para_gen_sph_rlm_rj_modes(gen_sph, para_sph)
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(parallel_sph_params), intent(inout) :: para_sph
!!      subroutine para_gen_sph_rtm_rtp_grids(gen_sph, para_sph)
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(parallel_sph_params), intent(inout) :: para_sph
!!@endverbatim
!
      module t_parai_gen_sph_grids_modes
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_const_spherical_grid
      use t_sph_local_index
!
      implicit none
!
      type parallel_sph_params
        integer(kind = kint) :: num_pe
        type(sph_shell_parameters), allocatable :: sph_params(:)
!
        type(sph_rtp_grid), allocatable :: sph_rtp(:)
        type(sph_rtm_grid), allocatable :: sph_rtm(:)
        type(sph_rlm_grid), allocatable :: sph_rlm(:)
        type(sph_rj_grid), allocatable :: sph_rj(:)
!
        type(sph_comm_tbl), allocatable :: comm_rlm(:)
        type(sph_comm_tbl), allocatable :: comm_rj(:)
        type(sph_comm_tbl), allocatable :: comm_rtm(:)
        type(sph_comm_tbl), allocatable :: comm_rtp(:)
!
        type(sph_group_data), allocatable :: sph_grp(:)
      end type parallel_sph_params
!
      type(sph_local_1d_index), save, private :: sph_lcx_m
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_parallel_sph_params(num_pe, para_sph)
!
      integer(kind = kint) :: num_pe
      type(parallel_sph_params), intent(inout) :: para_sph
!
      para_sph%num_pe = num_pe
      allocate(para_sph%sph_grp(para_sph%num_pe))
      allocate(para_sph%sph_params(para_sph%num_pe))
      allocate(para_sph%sph_rlm(para_sph%num_pe))
      allocate(para_sph%sph_rj(para_sph%num_pe))
      allocate(para_sph%sph_rtm(para_sph%num_pe))
      allocate(para_sph%sph_rtp(para_sph%num_pe))
      allocate(para_sph%comm_rlm(para_sph%num_pe))
      allocate(para_sph%comm_rj(para_sph%num_pe))
      allocate(para_sph%comm_rtm(para_sph%num_pe))
      allocate(para_sph%comm_rtp(para_sph%num_pe))
!
      end subroutine alloc_parallel_sph_params
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_parallel_sph_params(para_sph)
!
      type(parallel_sph_params), intent(inout) :: para_sph
!
      deallocate(para_sph%comm_rtp, para_sph%comm_rj)
      deallocate(para_sph%comm_rtm, para_sph%comm_rlm)
      deallocate(para_sph%sph_rj, para_sph%sph_rlm)
      deallocate(para_sph%sph_rtm, para_sph%sph_rtp)
      deallocate(para_sph%sph_params, para_sph%sph_grp)
      para_sph%num_pe = 0
!
      end subroutine dealloc_parallel_sph_params
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rlm_rj_modes(gen_sph, para_sph)
!
      use m_elapsed_labels_gen_SPH
      use set_comm_table_rtp_rj
      use gen_sph_grids_modes
      use bcast_comm_stacks_sph
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
      type(parallel_sph_params), intent(inout) :: para_sph
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
      integer :: ip, id_rank
!
!
      allocate(comm_rlm_mul(para_sph%num_pe))
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
      do ip = 1, para_sph%num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &      'start rlm table generation for', id_rank, ' on ', my_rank
        call const_sph_rlm_modes                                        &
     &   (id_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    para_sph%sph_rlm(ip), para_sph%comm_rlm(ip))
        call copy_sph_comm_neib                                         &
     &     (para_sph%comm_rlm(ip), comm_rlm_mul(ip))
      end do
!
      call s_bcast_comm_stacks_sph(para_sph%num_pe, comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      call alloc_rj_1d_local_idx(para_sph%sph_rj(1), sph_lcx_m)
      do ip = 1, para_sph%num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &     'Construct spherical modes for domain ', id_rank,            &
     &     ' on ', my_rank
        call const_sph_rj_modes(id_rank, para_sph%num_pe, comm_rlm_mul, &
     &      gen_sph%added_radial_grp, gen_sph%s3d_ranks,                &
     &      gen_sph%s3d_radius, gen_sph%sph_lcp,                        &
     &      gen_sph%stk_lc1d, gen_sph%sph_gl1d,                         &
     &      para_sph%sph_params(ip), para_sph%sph_rtp(ip),              &
     &      para_sph%sph_rj(ip), para_sph%comm_rj(ip),                  &
     &      para_sph%sph_grp(ip), sph_lcx_m)
      end do
      call dealloc_rj_1d_local_idx(sph_lcx_m)
      call dealloc_comm_stacks_sph(para_sph%num_pe, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
      end subroutine para_gen_sph_rlm_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtm_rtp_grids(gen_sph, para_sph)
!
      use m_elapsed_labels_gen_SPH
      use gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use bcast_comm_stacks_sph
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
      type(parallel_sph_params), intent(inout) :: para_sph
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable:: comm_rtm_mul(:)
      integer :: ip, id_rank
!
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
      allocate(comm_rtm_mul(para_sph%num_pe))
      do ip = 1, para_sph%num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &      'start rtm table generation for', id_rank, ' on ', my_rank
        call const_sph_rtm_grids                                        &
     &     (id_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,             &
     &      gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,        &
     &      para_sph%sph_rtm(ip), para_sph%comm_rtm(ip))
        call copy_sph_comm_neib(para_sph%comm_rtm(ip), comm_rtm_mul(ip))
      end do
      call s_bcast_comm_stacks_sph(para_sph%num_pe, comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      call alloc_rtp_1d_local_idx(para_sph%sph_rtp(1), sph_lcx_m)
      do ip = 1, para_sph%num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical grids for domain ',  id_rank,   &
     &              ' on ',  my_rank
        call const_sph_rtp_grids                                        &
     &     (id_rank, para_sph%num_pe, comm_rtm_mul,                     &
     &      gen_sph%added_radial_grp, gen_sph%r_layer_grp,              &
     &      gen_sph%med_layer_grp, gen_sph%s3d_ranks,                   &
     &      gen_sph%s3d_radius, gen_sph%sph_lcp,                        &
     &      gen_sph%stk_lc1d, gen_sph%sph_gl1d,                         &
     &      para_sph%sph_params(ip), para_sph%sph_rtp(ip),              &
     &      para_sph%comm_rtp(ip), para_sph%sph_grp(ip),                &
     &      sph_lcx_m)
      end do
      call dealloc_rtp_1d_local_idx(sph_lcx_m)
      call dealloc_comm_stacks_sph(para_sph%num_pe, comm_rtm_mul)
      deallocate(comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
      end subroutine para_gen_sph_rtm_rtp_grids
!
! ----------------------------------------------------------------------
!
      end module t_parai_gen_sph_grids_modes
