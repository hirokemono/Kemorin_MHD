!FEM_analyzer_viz.f90
!
!      module FEM_analyzer_viz
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_vizs(viz_step, viz)
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_4_viz), intent(inout) :: viz
!!      subroutine FEM_analyze_vizs                                     &
!!     &         (i_step, time_d, viz_step, viz, visval)
!!        type(time_data), intent(in) :: init_d
!!        type(time_data), intent(inout) :: time_d
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!        type(FEM_mesh_field_4_viz), intent(inout) :: viz
!
      module FEM_analyzer_viz
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_VIZ_step_parameter
      use t_IO_step_parameter
      use t_mesh_data
      use t_file_IO_parameter
      use t_ucd_data
      use t_visualization
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_vizs(init_d, viz_step, viz)
!
      use load_mesh_and_field_4_viz
!
      type(time_data), intent(in) :: init_d
      type(VIZ_step_params), intent(inout) :: viz_step
      type(FEM_mesh_field_4_viz), intent(inout) :: viz
!
      integer(kind = kint) :: iflag
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ(viz%mesh_file_IO, viz%ucd_file_IO, init_d,  &
     &    viz%geofem, viz%ucd_time, viz%ucd, viz%nod_fld)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      iflag = viz_step%FLINE_t%increment + viz_step%PVR_t%increment     &
     &       + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        call element_normals_4_VIZ                                      &
     &     (viz%geofem, viz%ele_4_nod, viz%spfs, viz%jacobians)
      end if
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
     &         (i_step, time_d, viz_step, viz, visval)
!
      use t_ucd_data
      use load_mesh_and_field_4_viz
!
      integer (kind =kint), intent(in) :: i_step
      type(time_data), intent(inout) :: time_d
      type(VIZ_step_params), intent(inout) :: viz_step
      type(FEM_mesh_field_4_viz), intent(inout) :: viz
      integer(kind=kint ), intent(inout) :: visval
!
!
      visval = iflag_vizs_w_fix_step(i_step, viz_step)
      call istep_viz_w_fix_dt(i_step, viz_step)
      call set_field_data_4_VIZ(visval, i_step, viz%ucd_file_IO,        &
     &   viz%geofem, viz%ucd_time, viz%ucd, time_d, viz%nod_fld)
!
      end subroutine FEM_analyze_vizs
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz
