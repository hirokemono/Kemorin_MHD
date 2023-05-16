!>@file   analyzer_gen_sph_grid_t.f90
!!@brief  module analyzer_gen_sph_grid_t
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine init_gen_sph_grid_t
!!      subroutine analyze_gen_sph_grid_t
!!@endverbatim
!
      module analyzer_gen_sph_grid_t
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
      use t_SPH_mesh_field_array
      use t_file_IO_parameter
      use t_ctl_data_const_sph_mesh
      use t_SPH_mesh_field_data
      use t_ctl_params_gen_sph_shell
!
      use para_const_kemoview_mesh
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
      type(sph_mesh_array), save :: sph_array_g
!>      Structure of mesh file name and formats
      type(gen_sph_file_IO_params), save ::  sph_files1
!
!>      Structure to check and construct spherical shell mesh
      type(sph_grid_maker_in_sim), save :: sph_maker_G
!
!      type(sph_comm_tables), save, private :: comms_sph
!      type(sph_group_data), save, private ::  sph_grps
!      type(mesh_data), save, private :: geofem
!
!      type(parallel_make_vierwer_mesh), save, private :: para_v1
!
      private :: control_file_name
      private :: sph_array_g, SPH_MAKE_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_gen_sph_grid_t
!
      use m_error_IDs
      use input_control_const_shell
!
      integer(kind = kint) :: ierr = 0
!
! 
      call init_elapse_time_by_TOTAL
      call elpsed_label_gen_sph_grid
!
      call start_elapsed_time(ied_total_elapsed)
      call s_input_control_const_shell(control_file_name, SPH_MAKE_ctl, &
     &                                 sph_files1, sph_maker_G)
!
      end subroutine init_gen_sph_grid_t
!
! ----------------------------------------------------------------------
!
      subroutine analyze_gen_sph_grid_t
!
      use parallel_gen_sph_grids
      use parallel_load_data_4_sph
      use parallel_FEM_mesh_init
!
!  ========= Generate spherical harmonics table ========================
!
      sph_files1%sph_file_param%iflag_format = id_ascii_file_fmt
      if(iflag_debug .gt. 0) write(*,*) 's_para_gen_sph_grids'
      call alloc_sph_mesh_array                                         &
     &   (sph_maker_G%gen_sph%s3d_ranks%ndomain_sph, sph_array_g)
      call check_and_make_para_SPH_mesh                                 &
     &   (sph_files1%sph_file_param, sph_maker_G, sph_array_g)
      call dealloc_sph_mesh_array(sph_array_g)
!
      call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_gen_sph_grid_t
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_sph_grid_t
