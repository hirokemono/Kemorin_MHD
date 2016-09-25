!FEM_analyzer_viz_pvr.f90
!
!      module FEM_analyzer_viz_pvr
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_pvr
!!      subroutine FEM_analyze_pvr(i_step, istep_pvr)
!
      module FEM_analyzer_viz_pvr
!
      use m_precision
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
      subroutine FEM_initialize_pvr
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ
!
!     --------------------- init for PVR
!
      call element_normals_4_VIZ
!
!     ---------------------
!
      call deallocate_edge_geom_type(elemesh_VIZ%edge)
!
      end subroutine FEM_initialize_pvr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_pvr(i_step, istep_pvr)
!
      use m_control_params_2nd_files
      use set_exit_flag_4_visualizer
!
      integer (kind =kint), intent(in) :: i_step
      integer (kind =kint), intent(inout) :: istep_pvr
!
      integer (kind =kint) :: visval
!
!
      call set_viz_file_step(i_step, i_step_output_pvr,                 &
     &    visval, istep_pvr)
      call set_field_data_4_VIZ(istep_pvr, i_step)
!
      end subroutine FEM_analyze_pvr
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_pvr
