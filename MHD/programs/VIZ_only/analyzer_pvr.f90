!analyzer_pvr.f90
!      module analyzer_pvr
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine initialize_pvr
!!      subroutine analyze_pvr
!
      module analyzer_pvr
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use input_control_four_vizs
      use t_volume_rendering
      use t_particle_trace
      use t_fieldline
      use t_VIZ_only_step_parameter
      use t_FEM_mesh_field_4_viz
      use t_VIZ_mesh_field
      use t_mesh_SR
      use t_control_data_all_vizs
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_viz_ctl = "control_viz"
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ3
!>      Structure of control data for visualization
      type(control_data_vizs), save :: pvr_ctl3
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz3
!>      Structure of work area for mesh communications
      type(mesh_SR) :: m_SR13
!>      Structure of mesh and field for visualization only
      type(VIZ_mesh_field), save :: FEM_pvr3
!>      Structure of volume rendering
      type(volume_rendering_module), save :: vizs_pvr3
!>      Structure of fieldline module
      type(fieldline_module), save :: vizs_fline3
!>      Structure of tracer module
      type(tracer_module), save ::    vizs_tracer3
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_pvr
!
      use m_elapsed_labels_4_VIZ
      use m_elapsed_labels_SEND_RECV
      use input_control_all_vizs
      use volume_rendering
      use FEM_analyzer_viz
      use FEM_to_VIZ_bridge
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv

      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!  Load controls
      if (iflag_debug.gt.0) write(*,*) 's_inoput_control_four_vizs'
      call s_input_control_all_vizs(fname_viz_ctl, pvr_ctl3,           &
     &                              FEM_viz3, t_VIZ3)
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_viz'
      call FEM_initialize_viz(t_VIZ3%init_d,  t_VIZ3%ucd_step,          &
     &                        FEM_viz3, m_SR13)
      if(iflag_debug .gt. 0)  write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge                                       &
     &   (t_VIZ3%viz_step, FEM_viz3%geofem, FEM_pvr3, m_SR13)
!
!  Tracer Initialization
      call TRACER_initialize                                            &
     &   (t_VIZ3%init_d, t_VIZ3%finish_d, t_VIZ3%ucd_step,              &
     &    FEM_viz3%geofem, FEM_pvr3%para_surf, FEM_viz3%field,          &
     &    pvr_ctl3%tracer_ctls%tracer_controls, vizs_tracer3)
      call dealloc_tracer_controls(pvr_ctl3%tracer_ctls)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FLINE_initialize'
      call FLINE_initialize(t_VIZ3%viz_step%FLINE_t%increment,          &
     &    FEM_viz3%geofem, FEM_viz3%field, vizs_tracer3,                &
     &    pvr_ctl3%viz_ctl_v%fline_ctls, vizs_fline3)
!
      if(iflag_debug .gt. 0)  write(*,*) 'PVR_initialize'
      call PVR_initialize(t_VIZ3%viz_step%PVR_t%increment,              &
     &    FEM_viz3%geofem, FEM_viz3%field, vizs_tracer3, vizs_fline3,   &
     &    pvr_ctl3%viz_ctl_v%pvr_ctls, vizs_pvr3, m_SR13)
      call dealloc_viz_controls(pvr_ctl3%viz_ctl_v)
!
      end subroutine initialize_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_pvr
!
      use t_IO_step_parameter
      use volume_rendering
      use FEM_analyzer_viz
      use FEM_to_VIZ_bridge
!
      integer(kind = kint) :: i_step
!
!
      do i_step = t_VIZ3%init_d%i_time_step, t_VIZ3%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ3%ucd_step) .eqv. .FALSE.) cycle
        if(output_IO_flag(i_step,t_VIZ3%viz_step%PVR_t)                 &
     &       .eqv. .FALSE.) cycle
!
!  Load field data
        call FEM_analyze_viz(i_step, t_VIZ3%ucd_step, t_VIZ3%time_d,    &
     &                       FEM_viz3, m_SR13)
!
!  Load tracer data
        call TRACER_visualize(t_VIZ3%viz_step%istep_tracer,             &
     &      t_VIZ3%time_d, t_VIZ3%ucd_step, vizs_tracer3)
!
!  Const fieldlines
        if(iflag_debug .gt. 0)  write(*,*) 'PVR_visualize', i_step
        call istep_viz_w_fix_dt(i_step, t_VIZ3%viz_step)
        call FLINE_visualize                                            &
     &     (t_VIZ3%viz_step%istep_fline, t_VIZ3%time_d,                 &
     &      FEM_viz3%geofem, FEM_pvr3%para_surf, FEM_viz3%field,        &
     &      vizs_fline3, m_SR13)
!
!  Rendering
        call PVR_visualize                                              &
     &     (t_VIZ3%viz_step%istep_pvr, t_VIZ3%time_d%time,              &
     &     FEM_viz3%geofem, FEM_pvr3%jacobians, FEM_viz3%field,         &
     &     vizs_tracer3, vizs_fline3, vizs_pvr3, m_SR13)
      end do
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_pvr
!
!  ---------------------------------------------------------------------
!
      end module analyzer_pvr
