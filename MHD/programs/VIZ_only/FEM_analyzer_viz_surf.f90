!FEM_analyzer_viz_surf.f90
!
!      module FEM_analyzer_viz_surf
!
!       Written by H. Matsui
!
!!      subroutine FEM_initialize_surface(ucd_param, ucd)
!!      subroutine FEM_analyze_surface                                  &
!!     &         (i_step, ucd_param, time_VIZ, viz_step, ucd)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(time_step_param), intent(inout) :: time_VIZ
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!        type(ucd_data), intent(inout) :: ucd
!
      module FEM_analyzer_viz_surf
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
      use t_step_parameter
      use t_VIZ_step_parameter
      use t_IO_step_parameter
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
      subroutine FEM_initialize_surface(ucd_param, ucd)
!
      type(field_IO_params), intent(in) :: ucd_param
      type(ucd_data), intent(inout) :: ucd
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
      call mesh_setup_4_VIZ(mesh_file_VIZ, ucd_param, t_VIZ,            &
     &    femmesh_VIZ, VIZ_time_IO, ucd, field_VIZ)
!
!     ---------------------
!
      call deallocate_surface_geom_type(femmesh_VIZ%mesh%surf)
      call dealloc_edge_geometory(femmesh_VIZ%mesh%edge)
!
      end subroutine FEM_initialize_surface
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_analyze_surface                                    &
     &         (i_step, ucd_param, time_VIZ, viz_step, ucd)
!
      integer (kind =kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: ucd_param
      type(time_step_param), intent(inout) :: time_VIZ
      type(VIZ_step_params), intent(inout) :: viz_step
      type(ucd_data), intent(inout) :: ucd
!
      integer (kind =kint) :: visval, iflag
!
!
      visval = output_IO_flag(i_step, viz_step%PSF_t)                   &
     &      * output_IO_flag(i_step, viz_step%ISO_t)
      call istep_file_w_fix_dt(i_step, viz_step%PSF_t)
      call istep_file_w_fix_dt(i_step, viz_step%ISO_t)
!
      iflag = viz_step%PSF_t%istep_file * viz_step%ISO_t%istep_file
      call set_field_data_4_VIZ(iflag, i_step, ucd_param,               &
     &   femmesh_VIZ, VIZ_time_IO, ucd, time_VIZ%time_d, field_VIZ)
!
      end subroutine FEM_analyze_surface
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_viz_surf
