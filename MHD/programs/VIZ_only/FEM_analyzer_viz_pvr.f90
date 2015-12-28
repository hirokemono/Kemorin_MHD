!FEM_analyzer_viz_pvr.f90
!
!      module FEM_analyzer_viz_pvr
!
!       Written by H. Matsui
!
!      subroutine FEM_initialize_pvr(jac_3d_l, jac_3d_q, ucd)
!      subroutine FEM_analyze_pvr(i_step, istep_pvr, ucd)
!        type(ucd_data), intent(inout) :: ucd
!        type(jacobians_3d), intent(inout) :: jac_3d_l, jac_3d_q
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
      subroutine FEM_initialize_pvr(jac_3d_l, jac_3d_q, ucd)
!
      use t_ucd_data
      use t_jacobian_3d
!
      use m_node_phys_data
      use m_array_for_send_recv
      use m_read_mesh_data
      use m_group_data
      use m_control_params_2nd_files
!
      use const_mesh_info
      use load_mesh_data
      use set_parallel_file_name
!
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use nod_phys_send_recv
      use set_ucd_data_to_type
      use ucd_IO_select
      use const_jacobians_3d
      use const_element_comm_tables
!
      type(ucd_data), intent(inout) :: ucd
      type(jacobians_3d), intent(inout) :: jac_3d_l, jac_3d_q
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!       set mesh informations
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh_1st', mesh_file_head
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
!     --------------------- init for PVR
!
      if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_element'
      call max_int_point_by_etype(ele1%nnod_4_ele)
      call cal_jacobian_element                                         &
     &   (node1, ele1, sf_grp1, infty_list, jac_3d_l, jac_3d_q)
!
      call dealloc_dxi_dx_type(jac_3d_q)
      call dealloc_dxi_dx_type(jac_3d_l)
!
!     --------------------- 
!
      call deallocate_edge_geom_type(edge1)
!
!     ---------------------
!
      ucd%nnod =      node1%numnod
      call sel_read_udt_param(my_rank, i_step_init, ucd)
      call alloc_phys_data_type_by_output(ucd, node1, nod_fld1)
!
!
      end subroutine FEM_initialize_pvr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_pvr(i_step, istep_pvr, ucd)
!
      use t_ucd_data
      use m_node_phys_data
      use m_control_params_2nd_files
      use set_ucd_data_to_type
      use set_exit_flag_4_visualizer
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
      integer (kind =kint), intent(inout) :: istep_pvr
      type(ucd_data), intent(inout) :: ucd
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
        call set_data_by_read_ucd(my_rank, i_step, ucd, nod_fld1)
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
