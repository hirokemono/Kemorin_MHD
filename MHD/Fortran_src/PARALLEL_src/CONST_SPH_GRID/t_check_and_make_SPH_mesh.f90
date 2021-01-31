!>@file   t_check_and_make_SPH_mesh.f90
!!@brief  module t_check_and_make_SPH_mesh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine load_para_SPH_and_FEM_mesh(FEM_mesh_flags,           &
!!     &          sph_file_param, SPH_MHD, geofem, mesh_file, sph_maker)
!!      subroutine const_FEM_mesh_4_SPH(FEM_mesh_flags,                 &
!!     &          sph_file_param, SPH_MHD, geofem, mesh_file, sph_maker)
!!      subroutine check_and_make_SPH_mesh                              &
!!     &         (sph_file_param, sph_maker, SPH_MHD)
!!        type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
!!        type(field_IO_params), intent(in) ::  sph_file_param
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!        type(mesh_data), intent(inout) :: geofem
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!      subroutine check_and_make_SPH_rj_mode                           &
!!     &         (sph_file_param, sph_maker, SPH_MHD)
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!@endverbatim
!
      module t_check_and_make_SPH_mesh
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_gen_SPH
!
      use t_SPH_mesh_field_data
      use t_sph_grid_maker_in_sim
      use t_file_IO_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_para_SPH_and_FEM_mesh(FEM_mesh_flags,             &
     &          sph_file_param, SPH_MHD, geofem, mesh_file, sph_maker)
!
      use calypso_mpi
      use t_mesh_data
      use copy_mesh_structures
      use mesh_file_name_by_param
      use mpi_load_mesh_data
      use parallel_load_data_4_sph
      use const_FEM_mesh_sph_mhd
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  sph_file_param
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(mesh_data), intent(inout) :: geofem
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
!  Check and construct spherical shell table
      call check_and_make_SPH_mesh(sph_file_param, sph_maker, SPH_MHD)
!
!  --  load geofem mesh data
      if(check_exist_mesh(my_rank, mesh_file)) then
        if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
        call mpi_input_mesh(mesh_file, nprocs, geofem)
        call set_fem_center_mode_4_SPH                                  &
     &     (geofem%mesh%node%internal_node,                             &
     &      SPH_MHD%sph%sph_rtp, SPH_MHD%sph%sph_params)
      else
        call copy_sph_radial_groups(SPH_MHD%groups, sph_maker%gen_sph)
!  --  Construct FEM mesh
        mesh_file%file_prefix = sph_file_param%file_prefix
        call load_FEM_mesh_4_SPH(FEM_mesh_flags, mesh_file,             &
     &      SPH_MHD%sph, geofem, sph_maker%gen_sph)
        call dealloc_gen_sph_radial_groups(sph_maker%gen_sph)
      end if
!
      end subroutine load_para_SPH_and_FEM_mesh
!
! -----------------------------------------------------------------------
!
      subroutine const_FEM_mesh_4_SPH(FEM_mesh_flags,                   &
     &          sph_file_param, SPH_MHD, geofem, mesh_file, sph_maker)
!
      use calypso_mpi
      use t_mesh_data
      use copy_mesh_structures
      use mesh_file_name_by_param
      use mpi_load_mesh_data
      use parallel_load_data_4_sph
      use const_FEM_mesh_sph_mhd
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  sph_file_param
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(mesh_data), intent(inout) :: geofem
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
!
      call copy_sph_radial_groups(SPH_MHD%groups, sph_maker%gen_sph)
!  --  Construct FEM mesh
      mesh_file%file_prefix = sph_file_param%file_prefix
      call load_FEM_mesh_4_SPH(FEM_mesh_flags, mesh_file,               &
     &                         SPH_MHD%sph, geofem, sph_maker%gen_sph)
      call dealloc_gen_sph_radial_groups(sph_maker%gen_sph)
!
      end subroutine const_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine check_and_make_SPH_mesh                                &
     &         (sph_file_param, sph_maker, SPH_MHD)
!
      use m_error_IDs
      use calypso_mpi_logical
      use output_gen_sph_grid_modes
      use mpi_gen_sph_grids_modes
      use sph_file_IO_select
      use check_sph_file_access
      use parallel_load_data_4_sph
      use check_sph_mhd_openmp_size
!
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
      logical :: iflag_lc
!
!
      if(my_rank .eq. izero) then
        iflag_lc =    check_exsist_rtp_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rtm_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rlm_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rj_file(my_rank, sph_file_param)
      end if
      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
        call load_sph_mesh(sph_file_param,                              &
     &      SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
      else if(sph_maker%make_SPH_flag .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
        call mpi_gen_sph_grids(sph_maker%gen_sph, sph_maker%sph_tmp,    &
     &      SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
        if(sph_maker%mesh_output_flag) then
          call output_sph_mesh(sph_file_param,                          &
     &        SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
        end if
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'sph_index_flags_and_params'
      call sph_index_flags_and_params                                   &
     &   (SPH_MHD%groups, SPH_MHD%sph, SPH_MHD%comms)
!
      call s_check_sph_mhd_openmp_size(SPH_MHD%sph)
!
      call calypso_mpi_barrier
!
      end subroutine check_and_make_SPH_mesh
!
! ----------------------------------------------------------------------
!
      subroutine check_and_make_SPH_rj_mode                             &
     &         (sph_file_param, sph_maker, SPH_MHD)
!
      use m_error_IDs
      use calypso_mpi_logical
      use mpi_gen_sph_grids_modes
      use output_gen_sph_grid_modes
      use sph_file_IO_select
      use check_sph_file_access
      use parallel_load_data_4_sph
      use set_from_recv_buf_rev
!
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
      logical :: iflag_lc
!
!
      if(my_rank .eq. izero) then
        iflag_lc =    check_exsist_rtp_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rtm_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rlm_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rj_file(my_rank, sph_file_param)
      end if
      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
        call load_sph_rj_mesh(sph_file_param,                           &
     &      SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
      else if(sph_maker%make_SPH_flag .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
        call mpi_gen_sph_grids(sph_maker%gen_sph, sph_maker%sph_tmp,    &
     &      SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
!
        if(sph_maker%mesh_output_flag) then
          call output_sph_mesh(sph_file_param,                          &
     &        SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups)
        end if
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
      end if
!
      call sph_rj_index_flags_and_params                                &
     &   (SPH_MHD%groups, SPH_MHD%sph%sph_params,                       &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%comms%comm_rj)
!
      call calypso_mpi_barrier
!
      end subroutine check_and_make_SPH_rj_mode
!
! ----------------------------------------------------------------------
!
      end module t_check_and_make_SPH_mesh
