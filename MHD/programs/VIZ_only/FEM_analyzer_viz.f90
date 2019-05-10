!FEM_analyzer_viz.f90
!
!      module FEM_analyzer_viz
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_vizs(ucd_param, viz_step)
!!      subroutine FEM_analyze_vizs                                     &
!!     &         (i_step, ucd_param, time_VIZ, viz_step, visval)
!!        type(time_step_param), intent(inout) :: time_VIZ
!!        type(VIZ_step_params), intent(inout) :: viz_step
!
      module FEM_analyzer_viz
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_step_parameter
      use t_VIZ_step_parameter
      use t_IO_step_parameter
      use t_mesh_data
      use t_file_IO_parameter
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
      subroutine FEM_initialize_vizs(ucd_param, viz_step)
!
      type(field_IO_params), intent(in) :: ucd_param
      type(VIZ_step_params), intent(inout) :: viz_step
!
      integer(kind = kint) :: iflag
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ(ucd_param)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      iflag = viz_step%FLINE_t%increment + viz_step%PVR_t%increment     &
     &       + viz_step%LIC_t%increment
      if(iflag .gt. 0) call element_normals_4_VIZ
!
!     --------------------- 
!
      call dealloc_edge_geometory(femmesh_VIZ%mesh%edge)
!
!     ---------------------
!
      call calypso_mpi_barrier
!
      end subroutine FEM_initialize_vizs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_vizs                                       &
     &         (i_step, ucd_param, time_VIZ, viz_step, visval)
!
      use t_ucd_data
!
      integer (kind =kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_step_param), intent(inout) :: time_VIZ
      integer(kind=kint ), intent(inout) :: visval
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      visval = iflag_vizs_w_fix_step(i_step, viz_step)
      call istep_viz_w_fix_dt(i_step, viz_step)
      call set_field_data_4_VIZ                                         &
     &   (visval, i_step, ucd_param, time_VIZ%time_d)
!
      end subroutine FEM_analyze_vizs
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz
