!>@file   t_four_visualizers.f90
!!@brief  module t_four_visualizers
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module to access surfaceing, isosurfaceing,
!!       fieldline, and volume rendering modules
!!
!!@verbatim
!!      subroutine init_four_visualize(viz_step, geofem, nod_fld,       &
!!     &                               VIZ_DAT, viz_ctls, vizs)
!!      subroutine visualize_four(viz_step, time_d, geofem,             &
!!     &                          nod_fld, VIZ_DAT, vizs)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(four_visualize_modules), intent(inout) :: vizs
!!@endverbatim
!
      module t_four_visualizers
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
      use t_comm_table
      use t_phys_data
      use t_next_node_ele_4_node
      use t_VIZ_mesh_field
!
      use t_control_data_vizs
      use t_cross_section
      use t_isosurface
      use t_volume_rendering
      use t_fieldline
!
      implicit  none
!
      type four_visualize_modules
        type(sectioning_module) :: psf
        type(isosurface_module) :: iso
        type(volume_rendering_module) :: pvr
        type(fieldline_module) :: fline
      end type four_visualize_modules
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_four_visualize(viz_step, geofem, nod_fld,         &
     &                               VIZ_DAT, viz_ctls, vizs)
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!
      type(visualization_controls), intent(inout) :: viz_ctls
      type(four_visualize_modules), intent(inout) :: vizs
!
!
      if(viz_step%PSF_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+1)
        call SECTIONING_initialize(geofem, VIZ_DAT%edge_comm, nod_fld,  &
     &      viz_ctls%psf_ctls, vizs%psf)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+1)
        call calypso_mpi_barrier
      end if
!
      if(viz_step%ISO_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+2)
        call ISOSURF_initialize                                         &
     &     (geofem, nod_fld, viz_ctls%iso_ctls, vizs%iso)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+2)
        call calypso_mpi_barrier
      end if
!
      if(viz_step%PVR_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+3)
        call PVR_initialize                                             &
     &   (geofem, nod_fld, viz_ctls%pvr_ctls, vizs%pvr)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+3)
        call calypso_mpi_barrier
      end if
!
      if(viz_step%FLINE_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+4)
        call FLINE_initialize                                           &
     &     (geofem, nod_fld, viz_ctls%fline_ctls, vizs%fline)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+4)
        call calypso_mpi_barrier
      end if
!
      call dealloc_viz_controls(viz_ctls)
!
      end subroutine init_four_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_four(viz_step, time_d, geofem,               &
     &                          nod_fld, VIZ_DAT, vizs)
!
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!
      type(phys_data), intent(in) :: nod_fld
!
      type(four_visualize_modules), intent(inout) :: vizs
!
!
      if(viz_step%PSF_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+6)
        call SECTIONING_visualize                                     &
     &     (viz_step%istep_psf, time_d, geofem, nod_fld, vizs%psf)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+6)
      end if
!
      if(viz_step%ISO_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+7)
        call ISOSURF_visualize(viz_step%istep_iso, time_d, geofem,    &
     &      VIZ_DAT%edge_comm, nod_fld, vizs%iso)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+7)
      end if
!
      if(viz_step%PVR_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+8)
        call PVR_visualize(viz_step%istep_pvr, time_d%time,           &
     &      geofem, VIZ_DAT%jacobians, nod_fld, vizs%pvr)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+8)
      end if
!
      if(viz_step%FLINE_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+9)
        call FLINE_visualize(viz_step%istep_fline, geofem,            &
     &      VIZ_DAT%ele_4_nod, nod_fld, vizs%fline)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+9)
      end if
!
      end subroutine visualize_four
!
!  ---------------------------------------------------------------------
!
      end module t_four_visualizers
