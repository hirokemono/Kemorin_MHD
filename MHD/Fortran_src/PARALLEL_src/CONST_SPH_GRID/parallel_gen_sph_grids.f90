!>@file   parallel_gen_sph_grids.f90
!!@brief  module parallel_gen_sph_grids
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine s_para_gen_sph_grids(sph_file_param, sph, gen_sph)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
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
      use t_parai_gen_sph_grids_modes
      use t_const_spherical_grid
!
      implicit none
!
      type(parallel_sph_params), private :: para_sph
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_para_gen_sph_grids(sph_file_param, sph, gen_sph)
!
      use m_elapsed_labels_gen_SPH
      use mpi_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use copy_para_sph_global_params
      use output_gen_sph_grid_modes
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!  =========  Set global resolutions ===================================
      call const_sph_global_parameters(gen_sph, sph)
!
      call alloc_parallel_sph_params                                    &
     &   (gen_sph%s3d_ranks%ndomain_sph, para_sph)
      call copy_para_sph_param_from_ctl                                 &
     &   (sph, para_sph%num_pe, para_sph%sph_params,                    &
     &    para_sph%sph_rtp, para_sph%sph_rj)
      call copy_para_global_sph_resolution                              &
     &   (sph, para_sph%num_pe, para_sph%sph_rtp, para_sph%sph_rtm,     &
     &    para_sph%sph_rlm, para_sph%sph_rj)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_rj_modes'
      call para_gen_sph_rlm_rj_modes(gen_sph, para_sph)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtm_rtp_grids'
      call para_gen_sph_rtm_rtp_grids(gen_sph, para_sph)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_output_sph_mode_grids'
      call para_output_sph_mode_grids(sph_file_param,                   &
     &    para_sph%num_pe, para_sph%sph_params,                         &
     &    para_sph%sph_rj, para_sph%sph_rlm,                            &
     &    para_sph%sph_rtm, para_sph%sph_rtp,                           &
     &    para_sph%comm_rj, para_sph%comm_rlm,                          &
     &    para_sph%comm_rtm, para_sph%comm_rtp, para_sph%sph_grp)
!
      call dealloc_parallel_sph_params(para_sph)
!
      end subroutine s_para_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine s_para_gen_sph_rj_mode(sph_file_param, sph, gen_sph)
!
      use m_elapsed_labels_gen_SPH
      use mpi_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use copy_para_sph_global_params
      use output_gen_sph_grid_modes
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!  =========  Set global resolutions ===================================
      call const_sph_global_parameters(gen_sph, sph)
!
      call alloc_parallel_sph_params                                    &
     &   (gen_sph%s3d_ranks%ndomain_sph, para_sph)
      call copy_para_sph_param_from_ctl                                 &
     &   (sph, para_sph%num_pe, para_sph%sph_params,                    &
     &    para_sph%sph_rtp, para_sph%sph_rj)
      call copy_para_global_sph_resolution                              &
     &   (sph, para_sph%num_pe, para_sph%sph_rtp, para_sph%sph_rtm,     &
     &    para_sph%sph_rlm, para_sph%sph_rj)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_rj_modes'
      call para_gen_sph_rlm_rj_modes(gen_sph, para_sph)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_output_sph_rj_modes'
      call para_output_sph_rj_modes(sph_file_param, para_sph%num_pe,    &
     &    para_sph%sph_params, para_sph%sph_rj, para_sph%sph_rlm,       &
     &    para_sph%comm_rj, para_sph%comm_rlm, para_sph%sph_grp)
!
      call dealloc_parallel_sph_params(para_sph)
!
      end subroutine s_para_gen_sph_rj_mode
!
! ----------------------------------------------------------------------
!
      end module parallel_gen_sph_grids
