!>@file   parallel_gen_sph_grids.f90
!!@brief  module parallel_gen_sph_grids
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine check_and_make_para_SPH_mesh                         &
!!     &         (sph_file_param, sph_maker, sph_array)
!!      subroutine check_and_make_para_rj_mode                          &
!!     &         (sph_file_param, sph_maker, sph_array)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!        type(sph_mesh_array), intent(inout) :: sph_array
!!@endverbatim
!
      module parallel_gen_sph_grids
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_error_IDs
      use calypso_mpi
!
      use m_work_time
!
      use t_SPH_mesh_field_data
      use t_SPH_mesh_field_array
      use t_const_spherical_grid
!
      implicit none
!
      private :: para_gen_sph_grids, para_gen_sph_rj_mode
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine check_and_make_para_SPH_mesh                           &
     &         (sph_file_param, sph_maker, sph_array)
!
      use m_elapsed_labels_gen_SPH
      use output_gen_sph_grid_modes
!      use check_sph_file_access
!      use calypso_mpi_logical
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(sph_mesh_array), intent(inout) :: sph_array
!
      logical :: iflag_lc
!
!
!      if(my_rank .eq. izero) then
!        iflag_lc =    check_exsist_rtp_file(my_rank, sph_file_param)   &
!     &          .and. check_exsist_rtm_file(my_rank, sph_file_param)   &
!     &          .and. check_exsist_rlm_file(my_rank, sph_file_param)   &
!     &          .and. check_exsist_rj_file(my_rank, sph_file_param)
!      end if
!      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
      iflag_lc = .FALSE.
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
      else if(sph_maker%make_SPH_flag .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_grids'
        call para_gen_sph_grids(sph_maker, sph_array)
!
        if(iflag_debug .gt. 0) write(*,*) 'para_output_sph_mode_grids'
        call para_output_sph_mode_grids(sph_file_param, sph_array)
      end if
!
      end subroutine check_and_make_para_SPH_mesh
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_grids(sph_maker, sph_array)
!
      use m_elapsed_labels_gen_SPH
      use mpi_gen_sph_grids_modes
      use para_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use copy_para_sph_global_params
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(sph_mesh_array), intent(inout) :: sph_array
!
!
!  =========  Set global resolutions ===================================
      call const_sph_global_parameters                                  &
     &   (sph_maker%gen_sph, sph_maker%sph_tmp)
!
      call copy_para_sph_param_from_ctl(sph_maker%sph_tmp, sph_array)
      call copy_para_global_sph_resolution(sph_maker%sph_tmp,           &
     &                                     sph_array)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_rj_modes'
      call para_gen_sph_rlm_rj_modes(sph_maker%gen_sph, sph_array)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtm_rtp_grids'
      call para_gen_sph_rtm_rtp_grids(sph_maker%gen_sph, sph_array)
      call dealloc_gen_mesh_params(sph_maker%gen_sph)
!
      end subroutine para_gen_sph_grids
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_and_make_para_rj_mode                            &
     &         (sph_file_param, sph_maker, sph_array)
!
      use m_elapsed_labels_gen_SPH
      use para_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use copy_para_sph_global_params
      use output_gen_sph_grid_modes
      use sph_file_IO_select
      use check_sph_file_access
      use calypso_mpi_logical
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(sph_mesh_array), intent(inout) :: sph_array
!
      logical :: iflag_lc
!
!
      if(my_rank .eq. izero) then
        iflag_lc = check_exsist_rj_file(my_rank, sph_file_param)
!        write(*,*) 'iflag_lc: ',         iflag_lc
!        write(*,*) 'make_SPH_flag: ',    sph_maker%make_SPH_flag
!        write(*,*) 'mesh_output_flag: ', sph_maker%mesh_output_flag
      end if
      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
        call load_local_rj_mesh_4_merge(sph_file_param, sph_array)
      else if(sph_maker%make_SPH_flag .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rj_mode'
        call para_gen_sph_rj_mode(sph_maker, sph_array)
!
        if(sph_maker%mesh_output_flag) then
          if(iflag_debug .gt. 0) write(*,*) 'para_output_sph_rj_modes'
          call para_output_sph_rj_modes(sph_file_param, sph_array)
        end if
      end if
      call calypso_mpi_barrier
!
      end subroutine check_and_make_para_rj_mode
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rj_mode(sph_maker, sph_array)
!
      use m_elapsed_labels_gen_SPH
      use mpi_gen_sph_grids_modes
      use para_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use copy_para_sph_global_params
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(sph_mesh_array), intent(inout) :: sph_array
!
!  =========  Set global resolutions ===================================
      call const_sph_global_parameters                                  &
     &   (sph_maker%gen_sph, sph_maker%sph_tmp)
!
      call copy_para_sph_param_from_ctl(sph_maker%sph_tmp, sph_array)
      call copy_para_global_sph_resolution(sph_maker%sph_tmp,           &
     &                                     sph_array)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_rj_modes'
      call para_gen_sph_rlm_rj_modes(sph_maker%gen_sph, sph_array)
      call dealloc_gen_mesh_params(sph_maker%gen_sph)
!
      end subroutine para_gen_sph_rj_mode
!
! ----------------------------------------------------------------------
!
      end module parallel_gen_sph_grids
