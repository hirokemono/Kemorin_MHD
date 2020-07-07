!FEM_analyzer_viz_pvr.f90
!
!      module FEM_analyzer_viz_pvr
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_pvr(init_d, ucd_param, ucd)
!!        type(time_data), intent(in) :: init_d
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(ucd_data), intent(inout) :: ucd
!!      subroutine FEM_analyze_pvr                                      &
!!     &         (i_step, ucd_param, time_d, viz_step, ucd)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_data), intent(in) :: time_d
!!        type(VIZ_step_params), intent(inout)  :: viz_step
!!        type(ucd_data), intent(inout) :: ucd
!
      module FEM_analyzer_viz_pvr
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_step_parameter
      use t_VIZ_step_parameter
      use t_IO_step_parameter
      use t_file_IO_parameter
      use t_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_initialize_pvr(init_d, ucd_param, ucd)
!
      use load_mesh_and_field_4_viz
!
      type(time_data), intent(in) :: init_d
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(inout) :: ucd
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ(mesh_file_VIZ, ucd_param, init_d,           &
     &    femmesh_VIZ, VIZ_time_IO, ucd, field_VIZ)
!
!     --------------------- init for PVR
!
      call element_normals_4_VIZ                                        &
     &   (femmesh_VIZ, ele_4_nod_VIZ, spfs_VIZ, jacobians_VIZ)
!
      end subroutine FEM_initialize_pvr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_pvr                                        &
     &         (i_step, ucd_param, time_d, viz_step, ucd)
!
      use load_mesh_and_field_4_viz
!
      integer (kind =kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(in) :: time_d
!
      type(VIZ_step_params), intent(inout)  :: viz_step
      type(ucd_data), intent(inout) :: ucd
!
      integer (kind =kint) :: visval
!
!
      visval = output_IO_flag(i_step, viz_step%PVR_t)
      call istep_file_w_fix_dt(i_step, viz_step%PVR_t)
      call set_field_data_4_VIZ                                         &
     &   (viz_step%PVR_t%istep_file, i_step, ucd_param,                 &
     &   femmesh_VIZ, VIZ_time_IO, ucd, time_d, field_VIZ)
!
      end subroutine FEM_analyze_pvr
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_pvr
