!t_visualizer.f90
!      module t_visualizer
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine init_visualize                                       &
!!     &         (femmesh, ele_mesh, nod_fld, viz_ctls, vizs)
!!      subroutine visualize_all(viz_step, time_d,                     &
!!     &          femmesh, ele_mesh, nod_fld, ele_4_nod, jacs, vizs)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_data), intent(in) :: nod_fld
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(jacobians_type), intent(in) :: jacs
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(visualize_modules), intent(inout) :: vizs
!
      module t_visualizer
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_VIZ_step_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_next_node_ele_4_node
      use t_jacobians
!
      use t_control_data_vizs
      use t_cross_section
      use t_isosurface
      use t_volume_rendering
      use t_fieldline
      use t_lic_rendering
!
      implicit  none
!
      type visualize_modules
        type(sectioning_module) :: psf
        type(isosurface_module) :: iso
        type(volume_rendering_module) :: pvr
        type(fieldline_module) :: fline
        type(volume_rendering_module) :: lic
      end type visualize_modules
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize                                         &
     &         (femmesh, ele_mesh, nod_fld, viz_ctls, vizs)
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
!
      type(visualization_controls), intent(inout) :: viz_ctls
      type(visualize_modules), intent(inout) :: vizs
!
!
      call start_elapsed_time(60)
      call SECTIONING_initialize                                        &
     &   (femmesh, ele_mesh, nod_fld, viz_ctls%psf_ctls, vizs%psf)
      call end_elapsed_time(60)
!
      call start_elapsed_time(61)
      call ISOSURF_initialize                                           &
     &   (femmesh, ele_mesh, nod_fld, viz_ctls%iso_ctls, vizs%iso)
      call end_elapsed_time(61)
!
      call start_elapsed_time(62)
      call PVR_initialize                                               &
     &   (femmesh, ele_mesh, nod_fld, viz_ctls%pvr_ctls, vizs%pvr)
      call calypso_MPI_barrier
      call end_elapsed_time(62)
!
      call start_elapsed_time(63)
      call FLINE_initialize                                             &
     &   (femmesh, nod_fld, viz_ctls%fline_ctls, vizs%fline)
      call end_elapsed_time(63)
!
      call start_elapsed_time(64)
      call LIC_initialize                                               &
     &   (femmesh, ele_mesh, nod_fld, viz_ctls%lic_ctls, vizs%lic)
      call end_elapsed_time(64)
!
      end subroutine init_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_all(viz_step, time_d,                        &
     &          femmesh, ele_mesh, nod_fld, ele_4_nod, jacs, vizs)
!
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
!
      type(phys_data), intent(in) :: nod_fld
      type(element_around_node), intent(in) :: ele_4_nod
      type(jacobians_type), intent(in) :: jacs
!
      type(visualize_modules), intent(inout) :: vizs
!
!
      call start_elapsed_time(65)
      call SECTIONING_visualize(viz_step%PSF_t%istep_file, time_d,      &
     &    ele_mesh, nod_fld, vizs%psf)
      call calypso_MPI_barrier
      call end_elapsed_time(65)
!
      call start_elapsed_time(66)
      call ISOSURF_visualize(viz_step%ISO_t%istep_file, time_d,         &
     &    femmesh, ele_mesh, nod_fld, vizs%iso)
      call calypso_MPI_barrier
      call end_elapsed_time(66)
!
      call start_elapsed_time(67)
      call PVR_visualize(viz_step%PVR_t%istep_file,                     &
     &    femmesh, ele_mesh, jacs, nod_fld, vizs%pvr)
      call calypso_MPI_barrier
      call end_elapsed_time(67)
!
      call start_elapsed_time(68)
      call FLINE_visualize(viz_step%FLINE_t%istep_file,                 &
     &    femmesh, ele_mesh, ele_4_nod, nod_fld, vizs%fline)
      call calypso_MPI_barrier
      call end_elapsed_time(68)
!
      call start_elapsed_time(69)
      call LIC_visualize(viz_step%LIC_t%istep_file,                     &
     &    femmesh, ele_mesh, jacs, nod_fld, vizs%lic)
      call calypso_MPI_barrier
      call end_elapsed_time(69)
!
      end subroutine visualize_all
!
!  ---------------------------------------------------------------------
!
      end module t_visualizer
