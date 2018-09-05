!>@file   analyzer_gen_sph_grids.f90
!!@brief  module analyzer_gen_sph_grids
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine init_gen_sph_grids
!!      subroutine analyze_gen_sph_grids
!!@endverbatim
!
      module analyzer_gen_sph_grids
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
!
      use t_mesh_data
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_file_IO_parameter
      use t_ctl_data_const_sph_mesh
      use t_const_spherical_grid
      use t_ctl_params_gen_sph_shell
!
      implicit none
!
      character (len = kchara)                                          &
     &         :: control_file_name = 'control_sph_shell'
!
!
!>      Structure for file settings
      type(sph_mesh_generation_ctl), save :: SPH_MAKE_ctl
!
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph_const
!>      Structure of mesh file name and formats
      type(gen_sph_file_IO_params), save ::  sph_files1
!
!>      Structure to construct grid
      type(construct_spherical_grid), save :: gen_sph_G
!
      private :: control_file_name
      private :: sph_const, SPH_MAKE_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_gen_sph_grids
!
      use m_error_IDs
!
      num_elapsed = 4
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                  '
      elapse_labels(2) = 'Generation of spherical transform table'
      elapse_labels(3) = 'Generation of spherical mode and grid'
      elapse_labels(4) = 'Generation of FEM mesh data'
!
!
      call start_elapsed_time(1)
      call read_control_4_const_shell(control_file_name, SPH_MAKE_ctl)
      call set_control_4_gen_shell_grids                                &
     &   (SPH_MAKE_ctl%plt, SPH_MAKE_ctl%psph_ctl,                      &
     &    sph_const, sph_files1, gen_sph_G, ierr_MPI)
      if(ierr_MPI .gt. 0) call calypso_mpi_abort(ierr_MPI, e_message)
!
      if(gen_sph_G%s3d_ranks%ndomain_sph .ne. nprocs) then
        if(my_rank .eq. 0) write(*,*) 'The number of MPI processes ',   &
     &      'must be equal to the number of subdomains.', char(10),     &
     &      'Current subdomains: ', gen_sph_G%s3d_ranks%ndomain_sph
        write(e_message,'(a)') 'Parallellization error'
        call calypso_mpi_abort(ierr_P_MPI, e_message)
      end if
!
      end subroutine init_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine analyze_gen_sph_grids
!
      use parallel_gen_sph_grids
      use mpi_gen_sph_grids_modes
      use parallel_load_data_4_sph
!
      type(sph_comm_tables) :: comms_sph
      type(sph_group_data) ::  sph_grps
      type(mesh_data) :: geofem
      type(element_geometry) :: ele_mesh
!
!  ========= Generate spherical harmonics table ========================
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_grids'
      call para_gen_sph_grids(sph_const, gen_sph_G)
      call dealloc_gen_mesh_params(gen_sph_G)
!
      call start_elapsed_time(4)
      call load_para_SPH_and_FEM_mesh                                  &
     &   (sph_files1%FEM_mesh_flags, sph_const, comms_sph, sph_grps,   &
     &    geofem, ele_mesh, sph_files1%mesh_file_IO, gen_sph_G)
      call dealloc_gen_sph_fem_mesh_param(gen_sph_G)
      call end_elapsed_time(4)
      call end_elapsed_time(1)
!
      call output_elapsed_times
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_sph_grids
