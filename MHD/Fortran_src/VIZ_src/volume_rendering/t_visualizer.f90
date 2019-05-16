!t_visualizer.f90
!      module t_visualizer
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine init_visualize(fem, nod_fld, viz_ctls, vizs)
!!      subroutine visualize_all(viz_step, time_d,                     &
!!     &          fem, nod_fld, ele_4_nod, jacs, vizs)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: fem
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
      use m_elapsed_labels_4_VIZ
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
        type(lic_volume_rendering_module) :: lic
      end type visualize_modules
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize(fem, nod_fld, viz_ctls, vizs)
!
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: nod_fld
!
      type(visualization_controls), intent(inout) :: viz_ctls
      type(visualize_modules), intent(inout) :: vizs
!
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+1)
      call SECTIONING_initialize                                        &
     &   (fem, nod_fld, viz_ctls%psf_ctls, vizs%psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+1)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+2)
      call ISOSURF_initialize                                           &
     &   (fem, nod_fld, viz_ctls%iso_ctls, vizs%iso)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+2)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+3)
      call PVR_initialize                                               &
     &   (fem, nod_fld, viz_ctls%pvr_ctls, vizs%pvr)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+3)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+4)
      call FLINE_initialize                                             &
     &   (fem, nod_fld, viz_ctls%fline_ctls, vizs%fline)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+4)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+5)
      call LIC_initialize                                               &
     &   (fem, nod_fld, viz_ctls%lic_ctls, vizs%lic)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+5)
!
      end subroutine init_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_all(viz_step, time_d,                        &
     &          fem, nod_fld, ele_4_nod, jacs, vizs)
!
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: fem
!
      type(phys_data), intent(in) :: nod_fld
      type(element_around_node), intent(in) :: ele_4_nod
      type(jacobians_type), intent(in) :: jacs
!
      type(visualize_modules), intent(inout) :: vizs
!
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+6)
      call SECTIONING_visualize(viz_step%PSF_t%istep_file, time_d,      &
     &    fem, nod_fld, vizs%psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+6)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+7)
      call ISOSURF_visualize(viz_step%ISO_t%istep_file, time_d,         &
     &    fem, nod_fld, vizs%iso)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+7)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+8)
      call PVR_visualize(viz_step%PVR_t%istep_file,                     &
     &    fem, jacs, nod_fld, vizs%pvr)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+8)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+9)
      call FLINE_visualize(viz_step%FLINE_t%istep_file,                 &
     &    fem, ele_4_nod, nod_fld, vizs%fline)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+9)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+10)
      call LIC_visualize(viz_step%LIC_t%istep_file,                     &
     &    fem, jacs, nod_fld, vizs%lic)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+10)
!
      end subroutine visualize_all
!
!  ---------------------------------------------------------------------
!
      end module t_visualizer
