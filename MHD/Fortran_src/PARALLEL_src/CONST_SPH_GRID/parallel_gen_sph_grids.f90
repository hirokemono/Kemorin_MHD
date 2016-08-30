!>@file   parallel_gen_sph_grids.f90
!!@brief  module parallel_gen_sph_grids
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine para_gen_sph_grids(sph)
!!      subroutine deallocate_gen_mesh_params
!!        type(sph_grids), intent(inout) :: sph
!!@endverbatim
!
      module parallel_gen_sph_grids
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
!
      implicit none
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rtm_mul(:)
!
      private :: comm_rlm_mul, comm_rtm_mul
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_grids(sph)
!
      use m_spheric_global_ranks
      use set_global_spherical_param
      use para_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
!
      type(sph_grids), intent(inout) :: sph
!
!
!  =========  Set global resolutions ===================================
!
      call set_global_sph_resolution                                    &
     &   (sph%sph_params%l_truncation, sph%sph_params%m_folding,        &
     &    sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
      if(my_rank .eq. 0) then
        call check_global_spheric_parameter                             &
     &     (sph%sph_params, sph%sph_rtp)
        call output_set_radial_grid                                     &
     &     (sph%sph_params, sph%sph_rtp)
      end if
!
!  ========= Generate spherical harmonics table ========================
!
      call s_const_global_sph_grids_modes                               &
     &   (sph%sph_params, sph%sph_rtp, sph%sph_rtm, sph%sph_rj)
!
      call start_eleps_time(2)
      allocate(comm_rlm_mul(ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_grids'
      call para_gen_sph_rlm_grids                                       &
     &   (ndomain_sph, sph%sph_params%l_truncation,                     &
     &    sph%sph_rlm, comm_rlm_mul)
      call end_eleps_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rj_modes'
      call start_eleps_time(3)
      call para_gen_sph_rj_modes(ndomain_sph, comm_rlm_mul,             &
     &    sph%sph_params, sph%sph_rlm, sph%sph_rj)
      call dealloc_comm_stacks_sph(ndomain_sph, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      call end_eleps_time(3)
!
      call start_eleps_time(2)
      allocate(comm_rtm_mul(ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtm_grids'
      call para_gen_sph_rtm_grids                                       &
     &   (ndomain_sph, sph%sph_params%l_truncation,                     &
     &    sph%sph_rtm, comm_rtm_mul)
      call end_eleps_time(2)
      call start_eleps_time(3)
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtp_grids'
      call para_gen_sph_rtp_grids(ndomain_sph, comm_rtm_mul,            &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rtm)
      call dealloc_comm_stacks_sph(ndomain_sph, comm_rtm_mul)
!
      deallocate(comm_rtm_mul)
      call calypso_MPI_barrier
      call end_eleps_time(3)
!
      end subroutine para_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_gen_mesh_params
!
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use m_sph_1d_global_index
!
!
      call deallocate_sph_ranks
      call deallocate_sph_1d_domain_id
!
      call deallocate_sph_gl_bc_param
      call deallocate_sph_gl_parameter
!
      call deallocate_sph_1d_global_idx
      call deallocate_sph_1d_global_stack
!
      end subroutine deallocate_gen_mesh_params
!
! -----------------------------------------------------------------------
!
      end module parallel_gen_sph_grids
