!>@file   t_visualizer.f90
!!@brief  module t_visualizer
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module to access all visualization programs
!!
!!@verbatim
!!      subroutine init_visualize(viz_step, geofem, nod_fld, VIZ_DAT,   &
!!     &                          viz_ctls, vizs)
!!      subroutine visualize_all(viz_step, time_d, geofem, nod_fld,     &
!!     &                         VIZ_DAT, vizs, v_sol)
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(visualize_modules), intent(inout) :: vizs
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!@endverbatim
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
      use t_VIZ_mesh_field
      use t_time_data
      use t_mesh_data
      use t_comm_table
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
      use t_vector_for_solver
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
      subroutine init_visualize(viz_step, geofem, nod_fld, VIZ_DAT,     &
     &                          viz_ctls, vizs)
!
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!
      type(visualization_controls), intent(inout) :: viz_ctls
      type(visualize_modules), intent(inout) :: vizs
!
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+1)
      call SECTIONING_initialize                                        &
     &   (viz_step%PSF_t%increment, geofem, VIZ_DAT%edge_comm, nod_fld, &
     &    viz_ctls%psf_ctls, vizs%psf)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+1)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+2)
      call ISOSURF_initialize                                           &
     &   (viz_step%ISO_t%increment , geofem, nod_fld,                   &
     &    viz_ctls%iso_ctls, vizs%iso)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+2)
      call calypso_mpi_barrier
!
      if(viz_step%PVR_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+3)
        call PVR_initialize                                             &
     &     (geofem, nod_fld, viz_ctls%pvr_ctls, vizs%pvr)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+3)
        call calypso_mpi_barrier
      end if
!
      if(viz_step%LIC_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+4)
        call FLINE_initialize                                           &
     &     (geofem, nod_fld, viz_ctls%fline_ctls, vizs%fline)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+4)
        call calypso_mpi_barrier
      end if
!
      if(viz_step%FLINE_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+5)
        call LIC_initialize(VIZ_DAT%repart_p, VIZ_DAT%viz_fem,          &
     &      geofem, nod_fld, viz_ctls%lic_ctls, vizs%lic)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+5)
        call calypso_mpi_barrier
      end if
!
      call dealloc_viz_controls(viz_ctls)
!
      end subroutine init_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_all(viz_step, time_d, geofem, nod_fld,       &
     &                         VIZ_DAT, vizs, v_sol)
!
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
!
      type(phys_data), intent(in) :: nod_fld
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!
      type(visualize_modules), intent(inout) :: vizs
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if(viz_step%PSF_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+6)
        call SECTIONING_visualize                                       &
     &     (viz_step%istep_psf, time_d, geofem, nod_fld, vizs%psf)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+6)
      end if
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+7)
      call ISOSURF_visualize(viz_step%istep_iso, time_d,                &
     &    geofem, VIZ_DAT%edge_comm, nod_fld, vizs%iso)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+7)
!
      if(viz_step%PVR_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+8)
        call PVR_visualize(viz_step%istep_pvr, time_d%time,             &
     &      geofem, VIZ_DAT%jacobians, nod_fld, vizs%pvr)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+8)
      end if
!
      if(viz_step%LIC_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+10)
        call LIC_visualize(viz_step%istep_lic, time_d%time,             &
     &      VIZ_DAT%repart_p, VIZ_DAT%viz_fem, VIZ_DAT%mesh_to_viz_tbl, &
     &      geofem, nod_fld, vizs%lic, v_sol)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+10)
      end if
!
      if(viz_step%FLINE_t%increment .gt. 0) then
        if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+9)
        call FLINE_visualize(viz_step%istep_fline, geofem,              &
     &      VIZ_DAT%ele_4_nod, nod_fld, vizs%fline)
        if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+9)
      end if
!
      end subroutine visualize_all
!
!  ---------------------------------------------------------------------
!
      end module t_visualizer
