!FEM_analyzer_viz.f90
!
!      module FEM_analyzer_viz
!
!       Written by H. Matsui
!
!      subroutine FEM_initialize_vizs
!      subroutine FEM_analyze_vizs(i_step, viz_step, visval)
!!      type(VIZ_step_params), intent(inout) :: viz_step
!
      module FEM_analyzer_viz
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
      subroutine FEM_initialize_vizs
!
      integer(kind = kint) :: iflag
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
      iflag = viz_step1%FLINE_t%increment + viz_step1%PVR_t%increment
      if(iflag .gt. 0) call element_normals_4_VIZ
!
!     --------------------- 
!
      call deallocate_edge_geom_type(elemesh_VIZ%edge)
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
      subroutine FEM_analyze_vizs(i_step, viz_step, visval)
!
      use t_ucd_data
      use t_VIZ_step_parameter
      use t_IO_step_parameter
!
      integer (kind =kint), intent(in) :: i_step
      integer(kind=kint ), intent(inout) :: visval
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      visval = viz_file_step_4_fix(i_step, viz_step)
      call set_field_data_4_VIZ(visval, i_step)
!
      end subroutine FEM_analyze_vizs
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz
