!FEM_analyzer_viz_pvr.f90
!
!      module FEM_analyzer_viz_pvr
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_pvr(ucd_param)
!!      subroutine FEM_analyze_pvr(i_step, ucd_param, t_VIZ, pvr_step)
!!        type(time_step_param), intent(in) :: t_VIZ
!!        type(IO_step_param), intent(inout)  :: pvr_step
!
      module FEM_analyzer_viz_pvr
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!      use m_visualization
      use t_step_parameter
      use t_VIZ_step_parameter
      use t_IO_step_parameter
      use t_file_IO_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_pvr(ucd_param)
!
      type(field_IO_params), intent(in) :: ucd_param
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ(ucd_param)
!
!     --------------------- init for PVR
!
      call element_normals_4_VIZ
!
!     ---------------------
!
      call dealloc_edge_geometory(femmesh_VIZ%mesh%edge)
!
      end subroutine FEM_initialize_pvr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_pvr(i_step, ucd_param, t_VIZ, pvr_step)
!
      integer (kind =kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_step_param), intent(in) :: t_VIZ
      type(IO_step_param), intent(inout)  :: pvr_step
!
      integer (kind =kint) :: visval
!
!
      visval = output_IO_flag(i_step, pvr_step)
      call istep_file_w_fix_dt(i_step, pvr_step)
      call set_field_data_4_VIZ                                         &
     &   (pvr_step%istep_file, i_step, ucd_param, t_VIZ%time_d)
!
      end subroutine FEM_analyze_pvr
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_pvr
