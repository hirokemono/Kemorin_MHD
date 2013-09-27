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
      use m_parallel_var_dof
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
      use m_read_mesh_data
      use m_geometry_parameter
      use m_surface_group
      use m_control_params_2nd_files
      use m_surface_geometry_data
      use m_edge_geometry_data
      use m_jacobians
      use m_ucd_input_data
!
      use const_mesh_info
      use load_mesh_data
      use set_parallel_file_name
      use const_RHS_assemble_list
!
      use cal_jacobian
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
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
      call allocate_iccgN_matrix(isix, numnod)
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
        if (iflag_debug.gt.0) write(*,*) 'set_connect_for_fieldline'
        call set_connect_for_fieldline
!
        call set_max_int_point_by_etype
        if (iflag_debug.gt.0) write(*,*) 'cal_jacobian_element'
        call cal_jacobian_element
!
        call deallocate_dxi_dx_quad
        call deallocate_dxi_dx_linear
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
        call s_cal_normal_vector
!
        if (iflag_debug.eq.1)  write(*,*) 'pick_normal_of_surf_group'
        call pick_normal_of_surf_group
!
        if (iflag_debug.eq.1)  write(*,*) 's_sum_normal_4_surf_group'
        call s_sum_normal_4_surf_group
!
!     --------------------- 
!
      call deallocate_edge_geometry
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
