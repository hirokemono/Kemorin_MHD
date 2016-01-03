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
      subroutine FEM_initialize_fline
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      call element_normals_4_VIZ
!
      end subroutine FEM_initialize_fline
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_fline(i_step, istep_fline)
!
      use m_control_params_2nd_files
      use set_exit_flag_4_visualizer
!
      integer (kind =kint), intent(in) :: i_step
      integer (kind =kint), intent(inout) :: istep_fline
!
      integer (kind =kint) :: visval
!
!
      call set_viz_file_step(i_step, i_step_output_fline,               &
     &    visval, istep_fline)
      call set_field_data_4_VIZ(istep_fline, i_step)
!
      end subroutine FEM_analyze_fline
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_fline
