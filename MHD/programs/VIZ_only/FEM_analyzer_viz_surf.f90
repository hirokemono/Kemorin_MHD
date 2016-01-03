!FEM_analyzer_viz_surf.f90
!
!      module FEM_analyzer_viz_surf
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_surface
!!      subroutine FEM_analyze_surface(i_step, istep_pvr, iflag_viz)
!
      module FEM_analyzer_viz_surf
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
      use m_t_step_parameter
      use m_visualization
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
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ
!
!     ---------------------
!
      call deallocate_surface_geom_type(surfmesh_VIZ%surf)
      call deallocate_edge_geom_type(edgemesh_VIZ%edge)
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
!
      integer (kind =kint), intent(in) :: i_step
      integer (kind =kint), intent(inout) :: istep_psf, istep_iso
!
      integer (kind =kint) :: visval, iflag
!
!
      call set_viz_file_step(i_step, i_step_output_psf,                 &
     &    visval, istep_psf)
      call set_viz_file_step(i_step, i_step_output_iso,                 &
     &    visval, istep_iso)
!
      iflag = istep_psf * istep_psf
      call set_field_data_4_VIZ(iflag, i_step)
!
      end subroutine FEM_analyze_surface
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_surf
