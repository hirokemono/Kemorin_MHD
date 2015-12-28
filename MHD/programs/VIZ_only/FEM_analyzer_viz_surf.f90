!FEM_analyzer_viz_surf.f90
!
!      module FEM_analyzer_viz_surf
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_surface(ucd)
!!      subroutine FEM_analyze_surface(i_step, istep_pvr, iflag_viz, ucd)
!
      module FEM_analyzer_viz_surf
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
      use m_t_step_parameter
      use m_nod_comm_table
      use m_geometry_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_surface(ucd)
!
      use t_ucd_data
      use m_node_phys_data
      use m_read_mesh_data
      use m_control_params_2nd_files
      use m_array_for_send_recv
!
      use set_control_visualizer
      use const_mesh_info
      use load_mesh_data
      use set_parallel_file_name
      use nod_phys_send_recv
      use set_ucd_data_to_type
      use ucd_IO_select
      use const_element_comm_tables
!
      type(ucd_data), intent(inout) :: ucd
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh_1st'
      call input_mesh_1st(my_rank)
!
      call allocate_vector_for_solver(isix, node1%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv(nod_comm)
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbls(node1, ele1, surf1, edge1,           &
     &    nod_comm, ele_comm, surf_comm, edge_comm)
!
!     --------------------- 
!
      call deallocate_surface_geom_type(surf1)
      call deallocate_edge_geom_type(edge1)
!
!     ---------------------
!
      ucd%nnod =      node1%numnod
      call sel_read_udt_param(my_rank, i_step_init, ucd)
      call alloc_phys_data_type_by_output(ucd, node1, nod_fld1)
!
      end subroutine FEM_initialize_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_surface(i_step, istep_psf, istep_iso, ucd)
!
      use t_ucd_data
      use m_node_phys_data
      use m_control_params_2nd_files
      use set_ucd_data_to_type
      use set_exit_flag_4_visualizer
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
      integer (kind =kint), intent(inout) :: istep_psf, istep_iso
      type(ucd_data), intent(inout) :: ucd
!
      integer (kind =kint) :: visval
!
!
      call set_viz_file_step(i_step, i_step_output_psf,                 &
     &    visval, istep_psf)
      call set_viz_file_step(i_step, i_step_output_iso,                 &
     &    visval, istep_iso)
!
      if(istep_psf.ge.0 .or. istep_iso.ge.0) then
        call set_data_by_read_ucd(my_rank, i_step, ucd, nod_fld1)
        call nod_fields_send_recv(node1, nod_comm, nod_fld1)
      end if
!
      end subroutine FEM_analyze_surface
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_surf
