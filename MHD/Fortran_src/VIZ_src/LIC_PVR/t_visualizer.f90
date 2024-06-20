!>@file   t_visualizer.f90
!!@brief  module t_visualizer
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Main module to access all visualization programs
!!
!!@verbatim
!!      subroutine init_visualize(elps_VIZ, viz_step, geofem, nod_fld,  &
!!     &                          tracer, VIZ_DAT, viz_ctls, vizs, m_SR)
!!      subroutine visualize_all(elps_VIZ, viz_step, time_d, geofem,    &
!!     &                         nod_fld, tracer, VIZ_DAT, vizs, m_SR)
!!      subroutine visualize_fin(elps_VIZ, viz_step, time_d, vizs)
!!        type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
!!        type(VIZ_step_params), intent(in) :: viz_step
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(tracer_module), intent(in) :: tracer
!!        type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(visualize_modules), intent(inout) :: vizs
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module t_visualizer
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_elapsed_labels_4_VIZ
      use t_VIZ_step_parameter
      use t_VIZ_mesh_field
      use t_time_data
      use t_mesh_data
      use t_comm_table
      use t_phys_data
      use t_particle_trace
      use t_next_node_ele_4_node
      use t_jacobians
!
      use t_control_data_vizs
      use t_cross_section
      use t_isosurface
      use t_map_projection
      use t_volume_rendering
      use t_fieldline
      use t_lic_rendering
      use t_mesh_SR
!
      implicit  none
!
      type visualize_modules
        type(sectioning_module) :: psf
        type(isosurface_module) :: iso
        type(map_rendering_module) :: maps
        type(volume_rendering_module) :: pvr
        type(lic_volume_rendering_module) :: lic
        type(fieldline_module) :: fline
      end type visualize_modules
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize(elps_VIZ, viz_step, geofem, nod_fld,    &
     &                          tracer, VIZ_DAT, viz_ctls, vizs, m_SR)
!
      use t_fem_gauss_int_coefs
      use t_shape_functions
      use t_jacobians
      use volume_rendering
      use map_projection
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
!
      type(VIZ_mesh_field), intent(inout) :: VIZ_DAT
      type(visualization_controls), intent(inout) :: viz_ctls
      type(visualize_modules), intent(inout) :: vizs
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+1)
      call SECTIONING_initialize                                        &
     &   (viz_step%PSF_t%increment, elps_VIZ%elps_PSF,                  &
     &    geofem, VIZ_DAT%edge_comm, nod_fld, viz_ctls%psf_ctls,        &
     &    vizs%psf, m_SR%SR_sig, m_SR%SR_il)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+1)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+3)
      call ISOSURF_initialize                                           &
     &   (viz_step%ISO_t%increment, geofem, nod_fld,                    &
     &    viz_ctls%iso_ctls, vizs%iso)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+3)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+5)
      call MAP_PROJECTION_initialize(viz_step%MAP_t%increment,          &
     &    elps_VIZ%elps_PSF, elps_VIZ%elps_MAP,                         &
     &    geofem, VIZ_DAT%edge_comm, nod_fld,                           &
     &    viz_ctls%map_ctls, vizs%maps, m_SR%SR_sig, m_SR%SR_il)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+5)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+11)
      call FLINE_initialize                                             &
     &   (viz_step%FLINE_t%increment, geofem, nod_fld, tracer,          &
     &    viz_ctls%fline_ctls, vizs%fline)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+11)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+7)
      call PVR_initialize                                               &
     &   (viz_step%PVR_t%increment, elps_VIZ%elps_PVR, geofem, nod_fld, &
     &    tracer, vizs%fline, viz_ctls%pvr_ctls, vizs%pvr, m_SR)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+7)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+9)
      call LIC_initialize(viz_step%LIC_t%increment,                     &
     &    elps_VIZ%elps_PVR, elps_VIZ%elps_LIC,                         &
     &    geofem, VIZ_DAT%ele_comm, VIZ_DAT%next_tbl, nod_fld,          &
     &    viz_ctls%repart_ctl, viz_ctls%lic_ctls, vizs%lic, m_SR)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+9)
!
      call calypso_mpi_barrier
!
      end subroutine init_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_all(elps_VIZ, viz_step, time_d, geofem,      &
     &                         nod_fld, tracer, VIZ_DAT, vizs, m_SR)
!
      use volume_rendering
      use map_projection
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
      type(mesh_data), intent(in) :: geofem
!
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(VIZ_mesh_field), intent(in) :: VIZ_DAT
!
      type(visualize_modules), intent(inout) :: vizs
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(viz_step%PSF_t%increment .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+2)
        call SECTIONING_visualize                                       &
     &     (viz_step%istep_psf, elps_VIZ%elps_PSF,                      &
     &      time_d, geofem, nod_fld, vizs%psf)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+2)
      end if
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+4)
      call ISOSURF_visualize(viz_step%istep_iso, elps_VIZ%elps_ISO,     &
     &    time_d, geofem, VIZ_DAT%edge_comm, nod_fld,                   &
     &    vizs%iso, m_SR%SR_sig, m_SR%SR_il)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+4)
!
      if(viz_step%MAP_t%increment .gt. 0) then
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+6)
        call MAP_PROJECTION_visualize                                   &
     &     (viz_step%istep_map, elps_VIZ%elps_PSF, elps_VIZ%elps_MAP,   &
     &      time_d, geofem, nod_fld, vizs%maps, m_SR%SR_sig)
        if(elps_VIZ%flag_elapsed_V)                                     &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+6)
      end if
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+12)
      call FLINE_visualize                                              &
     &   (viz_step%istep_fline, elps_VIZ%elps_FLINE, time_d, geofem,    &
     &    VIZ_DAT%para_surf, nod_fld, tracer, vizs%fline, m_SR)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+12)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+8)
      call PVR_visualize(viz_step%istep_pvr, time_d%time,               &
     &    elps_VIZ%elps_PVR, geofem, VIZ_DAT%jacobians,                 &
     &    nod_fld, tracer, vizs%fline, vizs%pvr, m_SR)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+8)
!
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call start_elapsed_time(elps_VIZ%ist_elapsed_V+10)
      call LIC_visualize(viz_step%istep_lic, time_d%time,               &
     &    elps_VIZ%elps_PVR, elps_VIZ%elps_LIC,                         &
     &    geofem, VIZ_DAT%ele_comm, VIZ_DAT%next_tbl, nod_fld,          &
     &    vizs%lic, m_SR)
      if(elps_VIZ%flag_elapsed_V)                                       &
     &           call end_elapsed_time(elps_VIZ%ist_elapsed_V+10)
!
      call calypso_mpi_barrier
!
      end subroutine visualize_all
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_fin(elps_VIZ, viz_step, time_d, vizs)
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(time_data), intent(in) :: time_d
      type(VIZ_step_params), intent(in) :: viz_step
!
      type(visualize_modules), intent(inout) :: vizs
!
!
      call LIC_finalize(viz_step%istep_lic, time_d%time,                &
     &                  elps_VIZ%elps_LIC, vizs%lic)
!
      end subroutine visualize_fin
!
!  ---------------------------------------------------------------------
!
      end module t_visualizer
