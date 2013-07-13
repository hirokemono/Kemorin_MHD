!FEM_analyzer_viz_surf.f90
!
!      module FEM_analyzer_viz_surf
!
!       Written by H. Matsui
!
!      subroutine FEM_initialize_surface
!      subroutine FEM_analyze_surface(i_step, istep_pvr, iflag_viz)
!
      module FEM_analyzer_viz_surf
!
      use m_precision
      use m_constants
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
      subroutine FEM_initialize_surface
!
      use m_read_mesh_data
      use m_geometry_parameter
      use m_surface_geometry_data
      use m_edge_geometry_data
      use m_control_params_2nd_files
      use ucd_IO_select
!
      use set_control_visualizer
      use const_mesh_info
      use load_mesh_data
      use set_parallel_file_name
      use set_ucd_data
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
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
!     --------------------- 
!
      call deallocate_surface_geometry
      call deallocate_edge_geometry
!
      call time_prog_barrier
!
!     ---------------------
!
      call allocate_phys_data_by_output(my_rank, i_step_init)
!
      end subroutine FEM_initialize_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_surface(i_step, istep_psf, istep_iso)
!
      use m_control_params_2nd_files
      use set_exit_flag_4_visualizer
      use nod_phys_send_recv
      use set_ucd_data
!
      integer (kind =kint), intent(in) :: i_step
      integer (kind =kint), intent(inout) :: istep_psf, istep_iso
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
!*  ----------   Count steps for visualization
!*
        call set_ucd_data_from_IO(my_rank, i_step)
!
        if (iflag_debug.gt.0)  write(*,*) 'phys_send_recv_all'
        call phys_send_recv_all
      end if
!
      end subroutine FEM_analyze_surface
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_surf
