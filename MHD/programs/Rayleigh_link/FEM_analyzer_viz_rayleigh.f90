!>@file   FEM_analyzer_viz_rayleigh.f90
!!@brief  module FEM_analyzer_viz_rayleigh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2019
!
!>@brief  Main loop of visualization of Rayleigh data
!!
!!@verbatim
!!      subroutine FEM_initialize_viz_rayleigh(ucd_param, viz_step)
!!      subroutine FEM_analyze_viz_rayleigh                             &
!!     &         (i_step, ucd_param, time_VIZ, viz_step, visval)
!!        type(time_step_param), intent(inout) :: time_VIZ
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!@endverbatim
!
      module FEM_analyzer_viz_rayleigh
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_step_parameter
      use t_VIZ_step_parameter
      use t_IO_step_parameter
      use t_mesh_data
      use t_file_IO_parameter
!
      use t_spheric_parameter
      use t_const_spherical_grid
      use t_rayleigh_resolution
      use m_viz_4_rayleigh
!
      implicit none
!
!>      Structure to construct grid
      type(construct_spherical_grid), save :: gen_sph_R
      type(Rayleigh_grid_param), save :: r_reso_V
      type(mesh_data), save :: rayleigh_fem
!
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph_const
!
      private :: gen_sph_R, r_reso_V, rayleigh_fem, sph_const
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_viz_rayleigh(ucd_param, viz_step)
!
      use const_fem_nodes_4_rayleigh
      use const_FEM_mesh_sph_mhd
      use mpi_load_mesh_data
!
      type(field_IO_params), intent(in) :: ucd_param
      type(VIZ_step_params), intent(inout) :: viz_step
!
      type(field_IO_params) ::  rayleigh_mesh_file
      type(field_IO_params) ::  mesh_file
!
      integer(kind = kint) :: iflag
!
!   --------------------------------
!       setup Rayleigh information
!   --------------------------------
!
      rayleigh_mesh_file%file_prefix = 'aho_viz/Rayleigh_in'
      rayleigh_mesh_file%iflag_format = id_ascii_file_fmt
      call load_resolution_4_rayleigh(r_reso_V)
!
!      call s_const_fem_nodes_4_rayleigh                                &
!     &   (r_reso_V, rayleigh_fem%mesh, rayleigh_fem%group)
      call fem_nodes_4_rayleigh_file                                    &
     &   (r_reso_V, rayleigh_fem%mesh, rayleigh_fem%group)
!
      call shell_params_from_rayleigh(r_reso_V, sph_const, gen_sph_R)
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_FEM_mesh_4_sph_mhd'
      call base_FEM_mesh_sph_mhd                                        &
     &   (sph_const%sph_params, sph_const%sph_rtp, sph_const%sph_rj,    &
     &    femmesh_VIZ%mesh, femmesh_VIZ%group, gen_sph_R)
      mesh_file%file_prefix = 'aho_viz/tako'
      mesh_file%iflag_format = id_ascii_file_fmt
      call mpi_output_mesh                                              &
     &   (mesh_file, femmesh_VIZ%mesh, femmesh_VIZ%group)
!
      call dealloc_gen_sph_fem_mesh_param(gen_sph_R)
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ2(mesh_file_VIZ, ucd_param, t_VIZ,           &
     &    femmesh_VIZ, VIZ_time_IO, ucd_VIZ, field_VIZ)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      iflag = viz_step%FLINE_t%increment + viz_step%PVR_t%increment     &
     &       + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        call element_normals_4_VIZ                                      &
     &     (femmesh_VIZ, ele_4_nod_VIZ, spfs_VIZ, jacobians_VIZ)
      end if
!
!     --------------------- 
!
      call dealloc_edge_geometory(femmesh_VIZ%mesh%edge)
!
!     ---------------------
!
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_viz_rayleigh
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_viz_rayleigh                               &
     &         (i_step, ucd_param, time_VIZ, viz_step, visval)
!
      use t_ucd_data
!
      integer (kind =kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_step_param), intent(inout) :: time_VIZ
      integer(kind=kint ), intent(inout) :: visval
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      visval = iflag_vizs_w_fix_step(i_step, viz_step)
      call istep_viz_w_fix_dt(i_step, viz_step)
      call set_field_data_4_VIZ(visval, i_step, ucd_param,              &
     &   femmesh_VIZ, VIZ_time_IO, ucd_VIZ, time_VIZ%time_d, field_VIZ)
!
      end subroutine FEM_analyze_viz_rayleigh
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mesh_setup_4_VIZ2(mesh_file, ucd_param, time_v,         &
     &          fem, t_IO, ucd, field)
!
      use m_array_for_send_recv
      use mpi_load_mesh_data
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use set_parallel_file_name
      use set_ucd_data_to_type
      use ucd_IO_select
!
      type(field_IO_params), intent(in) :: mesh_file
      type(field_IO_params), intent(in) :: ucd_param
      type(time_step_param), intent(in) :: time_v
      type(mesh_data), intent(inout) :: fem
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      type(phys_data), intent(inout) :: field
!
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!       load mesh informations
!      call mpi_input_mesh(mesh_file, nprocs, fem)
!
       if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
       call FEM_mesh_initialization(fem%mesh, fem%group)
!
!     ---------------------
!
      ucd%nnod = fem%mesh%node%numnod
      call sel_read_udt_param(my_rank, time_v%init_d%i_time_step,       &
     &    ucd_param, t_IO, ucd)
      call alloc_phys_data_type_by_output                               &
     &   (ucd, fem%mesh%node, field)
!
      end subroutine mesh_setup_4_VIZ2
!
! ----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_rayleigh
