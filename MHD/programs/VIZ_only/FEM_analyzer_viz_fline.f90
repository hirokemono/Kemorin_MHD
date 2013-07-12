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
      use m_ucd_data
      use m_surface_geometry_data
      use m_edge_geometry_data
      use m_jacobians
!
      use const_mesh_info
      use load_mesh_data
      use set_parallel_file_name
      use set_ucd_data
      use const_RHS_assemble_list
!
      use cal_jacobian
      use int_volume_of_domain
      use set_normal_vectors
      use set_surf_grp_vectors
      use sum_normal_4_surf_group
      use ucd_IO_select
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
      call time_prog_barrier
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
      call time_prog_barrier
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_udt_param'
        fem_ucd%file_prefix = org_ucd_header
      call sel_read_udt_param(my_rank, i_step_init, fem_ucd)
      call allocate_phys_data_by_output
!
      end subroutine FEM_initialize_fline
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_fline(i_step, istep_fline)
!
      use m_ucd_data
      use set_exit_flag_4_visualizer
      use nod_phys_send_recv
      use ucd_IO_select
      use set_ucd_data
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
        fem_ucd%file_prefix = org_ucd_header
        call sel_read_udt_file(my_rank, i_step, fem_ucd)
        call set_ucd_data_from_IO
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
