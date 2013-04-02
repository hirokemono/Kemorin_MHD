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
      subroutine FEM_initialize_pvr
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
!       set mesh informations
!
      if (iflag_debug.gt.0) write(*,*) 'load_mesh_data', mesh_file_head
      call load_mesh_data
!
      call allocate_iccgN_matrix(isix, numnod)
!
      call time_prog_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
!     --------------------- init for PVR
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
!     --------------------- 
!
      call deallocate_edge_geometry
      call time_prog_barrier
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_udt_param'
        ucd_header_name = org_ucd_header
      call sel_read_udt_param(my_rank, i_step_init)
      call allocate_phys_data_by_output
!
      end subroutine FEM_initialize_pvr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_pvr(i_step, istep_pvr)
!
      use set_exit_flag_4_visualizer
      use nod_phys_send_recv
      use field_IO_select
      use set_ucd_data
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
        if (iflag_debug.gt.0)  write(*,*) 'sel_read_udt_file'
        ucd_header_name = org_ucd_header
        call sel_read_udt_file(my_rank,i_step)
        call set_ucd_data_from_IO
!
        if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
        call phys_send_recv_all
      end if
!
      end subroutine FEM_analyze_pvr
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_pvr
