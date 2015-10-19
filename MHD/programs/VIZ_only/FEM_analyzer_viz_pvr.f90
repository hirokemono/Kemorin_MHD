!FEM_analyzer_viz_pvr.f90
!
!      module FEM_analyzer_viz_pvr
!
!       Written by H. Matsui
!
!      subroutine FEM_initialize_pvr
!      subroutine FEM_analyze_pvr(i_step, istep_pvr)
!
      module FEM_analyzer_viz_pvr
!
      use m_precision
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
      subroutine FEM_initialize_pvr
!
      use m_array_for_send_recv
      use m_read_mesh_data
      use m_group_data
      use m_control_params_2nd_files
      use m_element_id_4_node
      use m_jacobians
      use m_ucd_input_data
      use m_ele_sf_eg_comm_tables
!
      use const_mesh_info
      use set_ele_id_4_node_type
      use load_mesh_data
      use set_parallel_file_name
!
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use nod_phys_send_recv
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!       set mesh informations
!
      if (iflag_debug.gt.0) write(*,*) 'load_mesh_data', mesh_file_head
      call load_mesh_data
!
      call allocate_vector_for_solver(isix, node1%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv(nod_comm)
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tables_1st'
      call const_element_comm_tables_1st
!
!     --------------------- init for PVR
!
      if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
      call set_ele_id_4_node(node1, ele1, ele_4_nod1)
!
      call set_max_int_point_by_etype
      if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_element'
      call cal_jacobian_element
!
      call dealloc_dxi_dx_type(jac1_3d_q)
      call dealloc_dxi_dx_type(jac1_3d_l)
!
!     --------------------- 
!
      call deallocate_edge_geom_type(edge1)
!
!     ---------------------
!
      call allocate_phys_data_by_output(my_rank, i_step_init)
!
      end subroutine FEM_initialize_pvr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_pvr(i_step, istep_pvr)
!
      use m_node_phys_data
      use m_control_params_2nd_files
      use m_ucd_input_data
      use set_exit_flag_4_visualizer
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
      integer (kind =kint), intent(inout) :: istep_pvr
!
      integer (kind =kint) :: visval
!
!
      call set_viz_file_step(i_step, i_step_output_pvr,                 &
     &    visval, istep_pvr)
!
      if(istep_pvr .ge. 0) then
!*  ----------   Count steps for visualization
!*
        call set_data_by_read_ucd(my_rank, i_step)
!
        if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
        call nod_fields_send_recv(node1, nod_comm, nod_fld1)
      end if
!
      end subroutine FEM_analyze_pvr
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_pvr
