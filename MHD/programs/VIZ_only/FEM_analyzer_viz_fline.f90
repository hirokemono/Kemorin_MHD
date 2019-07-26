!FEM_analyzer_viz_fline.f90
!
!      module FEM_analyzer_viz_fline
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_fline(ucd_param)
!!      subroutine FEM_analyze_fline                                    &
!!     &         (i_step, ucd_param, time_VIZ, fline_step)
!!        type(time_step_param), intent(inout) :: time_VIZ
!!        type(IO_step_param), intent(inout) :: fline_step
!
      module FEM_analyzer_viz_fline
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_visualization
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
      subroutine FEM_initialize_fline(ucd_param)
!
      type(field_IO_params), intent(in) :: ucd_param
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ(mesh_file_VIZ, ucd_param, t_VIZ,            &
     &    femmesh_VIZ, VIZ_time_IO, ucd_VIZ, field_VIZ)
!
!     --------------------- Connection information for PVR and fieldline
!     --------------------- init for fieldline and PVR
!
      call element_normals_4_VIZ                                        &
     &   (femmesh_VIZ, ele_4_nod_VIZ, spfs_VIZ, jacobians_VIZ)
!
      end subroutine FEM_initialize_fline
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_fline                                      &
     &         (i_step, ucd_param, time_VIZ, fline_step)
!
      integer (kind =kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_step_param), intent(inout) :: time_VIZ
      type(IO_step_param), intent(inout) :: fline_step
!
      integer (kind =kint) :: visval
!
!
      visval = output_IO_flag(i_step, fline_step)
      call istep_file_w_fix_dt(i_step, fline_step)
      call set_field_data_4_VIZ                                         &
     &  (fline_step%istep_file, i_step, ucd_param,                      &
     &   femmesh_VIZ, VIZ_time_IO, ucd_VIZ, time_VIZ%time_d, field_VIZ)
!
      end subroutine FEM_analyze_fline
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_fline
