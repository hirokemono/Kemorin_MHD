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
!!      subroutine FEM_analyze_viz_rayleigh                             &
!!     &         (i_step, time_VIZ, viz_step, visval)
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
      use t_rayleigh_field_IO
      use t_comm_table_4_assemble
      use m_viz_4_rayleigh
!
      implicit none
!
!>      Structure to construct grid
      type(construct_spherical_grid), save :: gen_sph_R
      type(Rayleigh_grid_param), save :: r_reso_V
      type(mesh_data), save :: rayleigh_fem
!
      type(rayleigh_field), save :: rayleigh_fld
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
      private :: gen_sph_R, r_reso_V, rayleigh_fem, sph_const
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_viz_rayleigh(viz_step)
!
      use const_fem_nodes_4_rayleigh
      use const_FEM_mesh_sph_mhd
      use parallel_FEM_mesh_init
      use const_element_comm_tables
      use copy_mesh_structures
      use mpi_load_mesh_data
!
      type(VIZ_step_params), intent(inout) :: viz_step
!
      integer(kind = kint) :: iflag
!
!   --------------------------------
!       setup Rayleigh information
!   --------------------------------
!
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
      call mesh_setup_4_VIZ2(femmesh_VIZ%mesh, field_VIZ)
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
      subroutine FEM_analyze_viz_rayleigh                               &
     &         (i_step, time_VIZ, viz_step, visval)
!
      use t_ucd_data
!
      integer (kind =kint), intent(in) :: i_step
      type(time_step_param), intent(inout) :: time_VIZ
      integer(kind=kint ), intent(inout) :: visval
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      visval = iflag_vizs_w_fix_step(i_step, viz_step)
      call istep_viz_w_fix_dt(i_step, viz_step)
!
      call set_field_data_4_VIZ2                                        &
     &   (visval, femmesh_VIZ, VIZ_time_IO, time_VIZ%time_d, field_VIZ)
!
      end subroutine FEM_analyze_viz_rayleigh
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mesh_setup_4_VIZ2(mesh, field)
!
      use m_array_for_send_recv
      use nod_phys_send_recv
      use set_parallel_file_name
      use set_ucd_data_to_type
      use ucd_IO_select
      use share_field_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(inout) :: field
!
      character(len=kchara) :: file_name
!
!
      file_name = 'Spherical_3D/00007000_grid'
      call read_rayleigh_field_param(file_name, rayleigh_fld)
!
      if(my_rank .eq. 0) then
        field%num_phys = 1
        call alloc_phys_name_type(field)
        field%phys_name(1) = 'temperature'
        field%num_component(1) = 1
        field%istack_component(1) = 1
        field%ntot_phys = 1
      end if
!
      call share_phys_field_names(field)
      field%num_phys_viz =  field%num_phys
      field%ntot_phys_viz = field%ntot_phys
!
      call alloc_phys_data_type(mesh%node%numnod, field)
!
      end subroutine mesh_setup_4_VIZ2
!
! ----------------------------------------------------------------------
!
      subroutine set_field_data_4_VIZ2                                  &
     &         (iflag, fem, t_IO, time_d, field)
!
      use assemble_nodal_fields
      use nod_phys_send_recv
!
      use set_ucd_data_to_type
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
!
      integer(kind = kint), intent(in) :: iflag
      type(mesh_data), intent(in) :: fem
!
      type(time_data), intent(inout) :: t_IO
      type(time_data), intent(inout) :: time_d
      type(phys_data), intent(inout) :: field
!
      character(len=kchara) :: file_name
      integer(kind = kint_gl) :: nnod_r, istart_pe
!
      type(field_IO_params) :: new_fld_file
      integer(kind = kint) :: istep = 1
      type(ucd_data), save :: ucd_m
      type(merged_ucd_data), save :: mucd_m
!
!
      if(iflag .ne. 0) return
        nnod_r = rayleigh_pmesh(my_rank+1)%node%numnod
        istart_pe                                                       &
     &         = rayleigh_pmesh(my_rank+1)%node%istack_numnod(my_rank)
        file_name = 'Spherical_3D/00007000_0501'
        call alloc_rayleigh_component(nnod_r, istart_pe, rayleigh_fld)
!
        call read_each_rayleigh_component(file_name, rayleigh_fld)
        call load_local_field_from_rayleigh                             &
     &     (rayleigh_fld, t_IO, rayleigh_fIO(my_rank+1))
        call dealloc_rayleigh_component(rayleigh_fld)
!
        call copy_time_step_size_data(t_IO, time_d)
        call assemble_field_data                                        &
     &     (nprocs, asbl_comm_R, field, t_IO, rayleigh_fIO)
!
!
        if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
        call nod_fields_send_recv(fem%mesh, field)
        call calypso_mpi_barrier
!
     return
!
      new_fld_file%iflag_format = iflag_fld
      new_fld_file%file_prefix =  'aho_viz/tako'
!
      call link_num_field_2_ucd(field, ucd_m)
      call link_local_mesh_2_ucd                                        &
     &   (fem%mesh%node, fem%mesh%ele, ucd_m)
      call link_field_data_to_ucd(field, ucd_m)
!
      if(new_fld_file%iflag_format/icent .eq. iflag_single/icent) then
!        write(*,*) 'init_merged_ucd'
        call init_merged_ucd(new_fld_file%iflag_format,                 &
     &      fem%mesh%node, fem%mesh%ele, fem%mesh%nod_comm,             &
     &      ucd_m, mucd_m)
      end if
!
      call sel_write_parallel_ucd_mesh(new_fld_file, ucd_m, mucd_m)
!        write(*,*) 'sel_write_parallel_ucd_file'
        call sel_write_parallel_ucd_file                                &
     &     (istep, new_fld_file, t_IO, ucd_m, mucd_m)
!
      end subroutine set_field_data_4_VIZ2
!
! ----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_rayleigh
