!>@file   FEM_analyzer_viz_rayleigh.f90
!!@brief  module FEM_analyzer_viz_rayleigh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2019
!
!>@brief  Main loop of visualization of Rayleigh data
!!
!!@verbatim
!!      subroutine FEM_initialize_viz_rayleigh(viz_step)
!!        type(time_data), intent(in) :: init_d
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!      subroutine FEM_analyze_viz_rayleigh(visval, i_step, time_d)
!!        type(time_data), intent(inout) :: time_d
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
      use t_rayleigh_field_IO
      use t_rayleigh_field_address
      use t_comm_table_4_assemble
      use m_viz_4_rayleigh
!
      implicit none
!
!>      Structure to construct grid
      type(construct_spherical_grid), save :: gen_sph_R
      type(rayleigh_field), save :: rayleigh_rtp_V
      type(mesh_data), save :: rayleigh_fem
!
      type(rayleigh_field_address), save :: rayleigh_ftbl1
!
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph_const
!
!>       Structure of parallel Rayleigh mesh
      type(mesh_geometry), allocatable, save :: rayleigh_pmesh(:)
!>       Structure of parallel Rayleigh field
      type(field_IO), allocatable, save :: rayleigh_fIO(:)
!>       Structure of data asssemble table
      type(comm_table_4_assemble), save :: asbl_comm_R
!
      private :: gen_sph_R, rayleigh_fem, sph_const
      private :: set_field_data_4_VIZ2
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_viz_rayleigh(init_d, viz_step)
!
      use const_fem_nodes_4_rayleigh
      use const_FEM_mesh_sph_mhd
      use parallel_FEM_mesh_init
      use const_element_comm_tables
      use copy_mesh_structures
      use mpi_load_mesh_data
!
      type(time_data), intent(in) :: init_d
      type(VIZ_step_params), intent(inout) :: viz_step
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: iflag
!
!   --------------------------------
!       setup Rayleigh information
!   --------------------------------
!
      file_name = 'Spherical_3D/00007000_grid'
      write(file_name,'(a,a1,i8.8,a5)')                                 &
     &                     trim(rayleigh_ftbl1%field_dir), '/',         &
     &                     init_d%i_time_step, '_grid'
      call read_rayleigh_field_param(file_name, rayleigh_rtp_V)
      call bcast_rayleigh_field_param(rayleigh_rtp_V)
      call set_rayleigh_parallel_param(rayleigh_rtp_V)
!
      call fem_nodes_4_rayleigh_file                                    &
     &   (rayleigh_rtp_V, rayleigh_fem%mesh, rayleigh_fem%group)
!
      call shell_params_from_rayleigh                                   &
     &   (rayleigh_rtp_V, sph_const, gen_sph_R)
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_FEM_mesh_4_sph_mhd'
      call base_FEM_mesh_sph_mhd                                        &
     &   (sph_const%sph_params, sph_const%sph_rtp, sph_const%sph_rj,    &
     &    femmesh_VIZ%mesh, femmesh_VIZ%group, gen_sph_R)
!
      call dealloc_gen_sph_fem_mesh_param(gen_sph_R)
!
      if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization                                      &
     &   (femmesh_VIZ%mesh, femmesh_VIZ%group)
      call const_global_numele_list(femmesh_VIZ%mesh%ele)
!
!   --------------------------------
!       Construct data shuffle table
!   --------------------------------
!
      allocate(rayleigh_pmesh(nprocs))
      allocate(rayleigh_fIO(nprocs))
!
      call copy_node_geometry_types                                     &
     &   (rayleigh_fem%mesh%node, rayleigh_pmesh(my_rank+1)%node)
      call const_global_numnod_list(rayleigh_pmesh(my_rank+1)%node)
      call dealloc_node_geometry_base(rayleigh_fem%mesh%node)
      call dealloc_groups_data(rayleigh_fem%group)
!
      call s_search_original_domain_node(nprocs, rayleigh_pmesh,        &
     &    femmesh_VIZ%mesh%node, asbl_comm_R)
!
!   --------------------------------
!       setup field information
!   --------------------------------
!
      call init_fields_by_rayleigh                                      &
     &   (rayleigh_ftbl1, femmesh_VIZ%mesh, field_VIZ)
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
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_viz_rayleigh
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_viz_rayleigh(visval, i_step, time_d)
!
      use t_ucd_data
!
      logical, intent(in) :: visval
      integer(kind = kint), intent(in) :: i_step
      type(time_data), intent(inout) :: time_d
!
!
      time_d%i_time_step = i_step
      time_d%time = 0.0d0
      time_d%dt = 0.0d0
      if(visval) call set_field_data_4_VIZ2(i_step,                     &
     &                                      femmesh_VIZ, field_VIZ)
!
      end subroutine FEM_analyze_viz_rayleigh
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_field_data_4_VIZ2(i_step, fem, field)
!
      use assemble_nodal_fields
      use nod_phys_send_recv
!
      use coordinate_convert_4_sph
      use share_field_data
!
      integer(kind = kint), intent(in) :: i_step
      type(mesh_data), intent(in) :: fem
!
      type(phys_data), intent(inout) :: field
!
      character(len=kchara) :: file_name
!
      integer(kind = kint) :: nd
!
!
      call init_fields_IO_by_rayleigh(rayleigh_ftbl1,                   &
     &    rayleigh_pmesh(my_rank+1), rayleigh_fIO(my_rank+1))
!
      call alloc_rayleigh_component                                     &
     &   (rayleigh_pmesh(my_rank+1)%node%numnod,                        &
     &    rayleigh_pmesh(my_rank+1)%node%istack_numnod(my_rank),        &
     &    rayleigh_rtp_V)
      do nd = 1, rayleigh_ftbl1%ntot_comp
        write(file_name,'(a,a1,i8.8,a1,i4.4)')                          &
     &                     trim(rayleigh_ftbl1%field_dir), '/',         &
     &                     i_step, '_', rayleigh_ftbl1%id_rayleigh(nd)
        call read_each_rayleigh_component(file_name, nd,                &
     &      rayleigh_pmesh(my_rank+1)%node%numnod,                      &
     &      rayleigh_fIO(my_rank+1)%ntot_comp_IO,                       &
     &      rayleigh_fIO(my_rank+1)%d_IO, rayleigh_rtp_V)
      end do
      call dealloc_rayleigh_component(rayleigh_rtp_V)
!
      call assemble_field_data                                          &
     &   (nprocs, asbl_comm_R, field, rayleigh_fIO)
!
      call overwrite_nodal_sph_2_xyz(fem%mesh%node, field)
!
      if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(fem%mesh, field)
!
      end subroutine set_field_data_4_VIZ2
!
! ----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_rayleigh
