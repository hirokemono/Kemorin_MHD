!FEM_analyzer_viz_fline.f90
!
!      module FEM_analyzer_viz_fline
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_fline
!!      subroutine FEM_analyze_fline(i_step, fline_step)
!!        type(IO_step_param), intent(inout) :: fline_step
!
      module FEM_analyzer_viz_fline
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_t_step_parameter
      use m_visualization
      use t_IO_step_parameter
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
      subroutine FEM_analyze_fline(i_step, fline_step)
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(inout) :: fline_step
!
      integer (kind =kint) :: visval
!
!
      visval = ione
      call accum_flag_to_visualization(i_step, fline_step, visval)
      call set_field_data_4_VIZ(fline_step%istep_file, i_step)
!
      end subroutine FEM_analyze_fline
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_fline
