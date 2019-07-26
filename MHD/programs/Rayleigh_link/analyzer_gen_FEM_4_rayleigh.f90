!>@file   analyzer_gen_FEM_4_rayleigh.f90
!!@brief  module analyzer_gen_FEM_4_rayleigh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine init_gen_FEM_rayleigh
!!      subroutine analyze_FEM_rayleigh
!!@endverbatim
!
      module analyzer_gen_FEM_4_rayleigh
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_gen_SPH
!
      use t_mesh_data
      use t_spheric_parameter
      use t_spheric_group
      use t_sph_trans_comm_tbl
      use t_file_IO_parameter
      use t_ctl_data_const_sph_mesh
      use t_const_spherical_grid
      use t_ctl_params_gen_sph_shell
      use t_rayleigh_resolution
      use const_fem_nodes_4_rayleigh
!
      implicit none
!
!
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph_const
!>      Structure of mesh file name and formats
      type(gen_sph_file_IO_params), save ::  sph_files1
!
!>      Structure to construct grid
      type(construct_spherical_grid), save :: gen_sph_G
!
      type(mesh_data), save, private :: geofem
!
      type(Rayleigh_grid_param), save, private :: r_reso0
!
      private :: sph_const
      private :: shell_params_from_rayleigh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_gen_FEM_rayleigh
!
      use m_error_IDs
      use m_file_format_switch
      use m_array_for_send_recv
      use parallel_gen_sph_grids
      use mpi_gen_sph_grids_modes
      use parallel_load_data_4_sph
      use parallel_FEM_mesh_init
!
      use const_FEM_mesh_sph_mhd
!
!
      type(field_IO_params) ::  rayleigh_mesh_file
!
!
      rayleigh_mesh_file%file_prefix = 'Rayleigh_in'
      rayleigh_mesh_file%iflag_format = id_ascii_file_fmt
      call output_fem_nodes_4_rayleigh(rayleigh_mesh_file, r_reso0)
      call shell_params_from_rayleigh(r_reso0, sph_const, gen_sph_G)
!
      if(iflag_debug .gt. 0) write(*,*) 'load_para_SPH_and_FEM_mesh2'
      sph_files1%mesh_file_IO%file_prefix = 'aho/in'
      sph_files1%mesh_file_IO%iflag_format = id_ascii_file_fmt
!
      if (iflag_debug.gt.0) write(*,*) 'const_FEM_mesh_4_sph_mhd'
      call base_FEM_mesh_sph_mhd                                        &
     &   (sph_const%sph_params, sph_const%sph_rtp, sph_const%sph_rj,    &
     &    geofem%mesh, geofem%group, gen_sph_G)
!
      end subroutine init_gen_FEM_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_FEM_rayleigh
!
      use mpi_load_mesh_data
!
!
! Output mesh data
      call mpi_output_mesh                                              &
     &   (sph_files1%mesh_file_IO, geofem%mesh, geofem%group)
      write(*,'(a,i6,a)')                                               &
     &          'FEM mesh for domain', my_rank, ' is done.'
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_FEM_rayleigh
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_FEM_4_rayleigh
