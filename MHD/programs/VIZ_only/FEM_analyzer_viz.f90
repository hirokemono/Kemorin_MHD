!FEM_analyzer_viz.f90
!
!      module FEM_analyzer_viz
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_vizs(ucd_param, viz_step, read_ucd)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(in) :: init_d
!!        type(ucd_data), intent(inout) :: read_ucd
!!      subroutine FEM_analyze_vizs                                     &
!!     &         (i_step, ucd_param, time_d, viz_step, read_ucd, visval)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(in) :: init_d
!!        type(time_data), intent(inout) :: time_d
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!        type(ucd_data), intent(inout) :: read_ucd
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
      subroutine FEM_initialize_vizs                                    &
     &         (ucd_param, init_d, viz_step, read_ucd)
!
      use load_mesh_and_field_4_viz
!
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: init_d
      type(VIZ_step_params), intent(inout) :: viz_step
      type(ucd_data), intent(inout) :: read_ucd
!
      integer(kind = kint) :: iflag
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ(mesh_file_VIZ, ucd_param, init_d,           &
     &    femmesh_VIZ, VIZ_time_IO, read_ucd, field_VIZ)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      iflag = viz_step%FLINE_t%increment + viz_step%PVR_t%increment     &
     &       + viz_step%LIC_t%increment
      if(iflag .gt. 0) then
        call element_normals_4_VIZ                                      &
     &     (femmesh_VIZ, ele_4_nod_VIZ, spfs_VIZ, jacobians_VIZ)
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
     &         (i_step, ucd_param, time_d, viz_step, read_ucd, visval)
!
      use t_ucd_data
      use load_mesh_and_field_4_viz
!
      integer (kind =kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: time_d
      type(VIZ_step_params), intent(inout) :: viz_step
      type(ucd_data), intent(inout) :: read_ucd
      integer(kind=kint ), intent(inout) :: visval
!
!
      visval = iflag_vizs_w_fix_step(i_step, viz_step)
      call istep_viz_w_fix_dt(i_step, viz_step)
      call set_field_data_4_VIZ(visval, i_step, ucd_param,              &
     &   femmesh_VIZ, VIZ_time_IO, read_ucd, time_d, field_VIZ)
!
      end subroutine FEM_analyze_vizs
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz
