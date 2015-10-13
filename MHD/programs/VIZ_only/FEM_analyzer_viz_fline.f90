!FEM_analyzer_viz_fline.f90
!
!      module FEM_analyzer_viz_fline
!
!       Written by H. Matsui
!
!      subroutine FEM_initialize_fline
!      subroutine FEM_analyze_fline(i_step, istep_fline)
!
      module FEM_analyzer_viz_fline
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_t_step_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_fline
!
      use m_array_for_send_recv
      use m_read_mesh_data
      use m_geometry_data
      use m_group_data
      use m_control_params_2nd_files
      use m_element_id_4_node
      use m_jacobians
      use m_jacobians_4_surface
      use m_ucd_input_data
      use m_ele_sf_eg_comm_tables
!
      use const_mesh_info
      use load_mesh_data
      use set_parallel_file_name
!
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use nodal_vector_send_recv
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!       load mesh informations
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      call allocate_vector_for_solver(isix, node1%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tables_1st'
      call const_element_comm_tables_1st
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
      call set_ele_id_4_node
!
      call set_max_int_point_by_etype
      if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_element'
      call cal_jacobian_element
!
      call dealloc_dxi_dx_type(jac1_3d_q)
      call dealloc_dxi_dx_type(jac1_3d_l)
!
!     --------------------- Surface jacobian for fieldline
!
      if (iflag_debug.gt.0) write(*,*) 's_int_whole_volume_only'
      call s_int_whole_volume_only
!
      if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_surface'
      call cal_jacobian_surface
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector'
      call s_cal_normal_vector(surf1, jac1_2d_q, jac1_2d_l)
!
      if (iflag_debug.eq.1)  write(*,*) 'pick_normal_of_surf_group'
      call pick_normal_of_surf_group                                    &
     &   (surf1, sf_grp1, sf_grp_tbl1, sf_grp_v1)
!
      if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
      call s_sum_normal_4_surf_group(sf_grp1, sf_grp_v1)
!
!     --------------------- 
!
      call deallocate_edge_geom_type(edge1)
!
!     ---------------------
!
      call allocate_phys_data_by_output(my_rank, i_step_init)
!
      end subroutine FEM_initialize_fline
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_fline(i_step, istep_fline)
!
      use m_control_params_2nd_files
      use m_ucd_input_data
      use set_exit_flag_4_visualizer
      use nod_phys_send_recv
!
      integer (kind =kint), intent(in) :: i_step
      integer (kind =kint), intent(inout) :: istep_fline
!
      integer (kind =kint) :: visval
!
!
      call set_viz_file_step(i_step, i_step_output_fline,               &
     &    visval, istep_fline)
!
      if(istep_fline .ge. 0) then
!*  ----------   Count steps for visualization
!*
        call set_data_by_read_ucd(my_rank, i_step)
!
        if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
        call phys_send_recv_all
      end if
!
      end subroutine FEM_analyze_fline
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_fline
