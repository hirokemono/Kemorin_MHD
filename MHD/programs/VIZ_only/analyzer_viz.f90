!analyzer_viz.f90
!      module analyzer_viz
!
!     Written by H. Matsui on July, 2006
!
!      subroutine initialize_vizs
!      subroutine analyze_vizs
!
      module analyzer_viz
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_control_data_all_vizs
      use t_visualizer
      use t_VIZ_only_step_parameter
      use t_FEM_mesh_field_4_viz
      use t_VIZ_mesh_field
      use t_mesh_SR
      use t_particle_trace
      use FEM_analyzer_viz
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_viz_ctl = "control_viz"
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ1
!
!>      Structure of control data for visualization
      type(control_data_vizs), save :: vizs_ctl1
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz1
!>      Structure of work area for mesh communications
      type(mesh_SR) :: m_SR11
!>      Structure of data for visualization
      type(VIZ_mesh_field), save :: VIZ_DAT1
!>      Structure of viualization modules
      type(visualize_modules), save :: vizs_v
!>      Structure of viualization modules
      type(tracer_module), save :: tracers_v
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_vizs
!
      use m_elapsed_labels_4_VIZ
      use m_elapsed_labels_SEND_RECV
      use m_elapsed_labels_4_REPART
      use m_work_time_4_sleeve_extend
      use FEM_to_VIZ_bridge
      use input_control_all_vizs
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv
      call elpsed_label_4_repartition
      call elpsed_label_4_sleeve_ext
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!  Load controls
      if (iflag_debug.gt.0) write(*,*) 's_input_control_all_vizs'
      call s_input_control_all_vizs(fname_viz_ctl, vizs_ctl1,           &
     &                              FEM_viz1, t_VIZ1)
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_viz'
      call FEM_initialize_viz(t_VIZ1%init_d, t_VIZ1%ucd_step,           &
     &                        FEM_viz1, m_SR11)
!
!  Tracer Initialization
      call TRACER_initialize                                            &
     &   (t_VIZ1%init_d, t_VIZ1%finish_d, t_VIZ1%ucd_step,              &
     &    FEM_viz1%geofem, VIZ_DAT1%para_surf, FEM_viz1%field,          &
     &    vizs_ctl1%tracer_ctls%tracer_controls, tracers_v)
      call dealloc_tracer_controls(vizs_ctl1%tracer_ctls)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge                                       &
     &   (t_VIZ1%viz_step, FEM_viz1%geofem, VIZ_DAT1, m_SR11)
      if(iflag_debug .gt. 0)  write(*,*) 'init_visualize'
      call init_visualize                                               &
     &   (t_VIZ1%viz_step, FEM_viz1%geofem, FEM_viz1%field, tracers_v,  &
     &    VIZ_DAT1, vizs_ctl1%viz_ctl_v, vizs_v, m_SR11)
      call dealloc_viz_controls(vizs_ctl1%viz_ctl_v)
!
      end subroutine initialize_vizs
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_vizs
!
      use FEM_to_VIZ_bridge
!
      integer(kind=kint ) :: i_step
!
!
      do i_step = t_VIZ1%init_d%i_time_step, t_VIZ1%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ1%ucd_step) .eqv. .FALSE.) cycle
        if(iflag_vizs_w_fix_step(i_step, t_VIZ1%viz_step)               &
     &        .eqv. .FALSE.) cycle
!
!  Load field data
        if(iflag_debug .gt. 0)  write(*,*) 'FEM_analyze_viz', i_step
        call FEM_analyze_viz(i_step, t_VIZ1%ucd_step, t_VIZ1%time_d,    &
     &                       FEM_viz1, m_SR11)
!
!  Rendering
        if(iflag_debug .gt. 0)  write(*,*) 'visualize_all', i_step
        call istep_viz_w_fix_dt(i_step, t_VIZ1%viz_step)
        call visualize_all(t_VIZ1%viz_step, t_VIZ1%time_d,              &
     &      FEM_viz1%geofem, FEM_viz1%field, tracers_v, VIZ_DAT1,       &
     &      vizs_v, m_SR11)
      end do
!
      call visualize_fin(t_VIZ1%viz_step, t_VIZ1%time_d, vizs_v)
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_vizs
!
!  ---------------------------------------------------------------------
!
      end module analyzer_viz
