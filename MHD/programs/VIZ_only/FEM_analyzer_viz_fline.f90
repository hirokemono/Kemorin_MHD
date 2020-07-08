!FEM_analyzer_viz_fline.f90
!
!      module FEM_analyzer_viz_fline
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_fline(ucd_step, init_d, viz)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(in) :: init_d
!!        type(FEM_mesh_field_4_viz), intent(inout) :: viz
!!      subroutine FEM_analyze_fline                                    &
!!     &         (i_step, ucd_step, time_d, viz_step, viz)
!!        type(IO_step_param), intent(in) :: ucd_step
!!        type(time_data), intent(inout) :: time_d
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!        type(FEM_mesh_field_4_viz), intent(inout) :: viz
!
      module FEM_analyzer_viz_fline
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_VIZ_step_parameter
      use t_IO_step_parameter
      use t_file_IO_parameter
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
      subroutine FEM_initialize_fline(ucd_step, init_d, viz)
!
      use load_mesh_and_field_4_viz
!
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(in) :: init_d
      type(FEM_mesh_field_4_viz), intent(inout) :: viz
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ                                             &
     &   (init_d, ucd_step, viz%ucd_file_IO, viz%mesh_file_IO,          &
     &    viz%geofem, viz%ucd_time, viz%ucd, viz%nod_fld)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      call element_normals_4_VIZ                                        &
     &   (viz%geofem, viz%ele_4_nod, viz%spfs, viz%jacobians)
!
      end subroutine FEM_initialize_fline
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_fline                                      &
     &         (i_step, ucd_step, time_d, viz_step, viz)
!
      use load_mesh_and_field_4_viz
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
      type(time_data), intent(inout) :: time_d
      type(VIZ_step_params), intent(inout) :: viz_step
      type(FEM_mesh_field_4_viz), intent(inout) :: viz
!
!
      call istep_file_w_fix_dt(i_step, viz_step%FLINE_t)
      call set_field_data_4_VIZ(viz_step%FLINE_t%istep_file,            &
     &    i_step, ucd_step, viz%ucd_file_IO,                            &
     &    viz%geofem, viz%ucd_time, viz%ucd, time_d, viz%nod_fld)
!
      end subroutine FEM_analyze_fline
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_fline
