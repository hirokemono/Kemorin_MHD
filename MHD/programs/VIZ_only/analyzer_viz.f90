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
      use FEM_analyzer_viz
      use t_control_data_all_vizs
      use t_visualizer
      use t_VIZ_only_step_parameter
      use t_visualization
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ1
!
!>      Structure of control data for visualization
      type(control_data_vizs), save :: vizs_ctl1
!>      Structure of mesh and field for visualization only
      type(FEM_mesh_field_4_viz), save :: viz1
!>      Structure of viualization modules
      type(visualize_modules), save :: vizs_v
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
      use load_mesh_and_field_4_viz
!
      integer(kind = kint) :: ierr
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv

      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_file_vizs'
      call read_control_file_vizs(vizs_ctl1)
      call set_control_params_4_viz(vizs_ctl1%t_viz_ctl,                &
     &    vizs_ctl1%viz_plt, vizs_ctl1%viz_field_ctl,                   &
     &    viz1%mesh_file_IO, viz1%ucd_file_IO, viz1%viz_fld_list,       &
     &    t_VIZ1, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_vizs'
      call FEM_initialize_vizs                                          &
     &   (t_VIZ1%ucd_step, t_VIZ1%init_d, t_VIZ1%viz_step, viz1)
      call dealloc_field_lists_for_vizs(viz1%viz_fld_list)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_visualize'
      call init_visualize                                               &
     &   (viz1%geofem, viz1%nod_fld, vizs_ctl1%viz_ctl_v, vizs_v)
!
      end subroutine initialize_vizs
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_vizs
!
      use load_mesh_and_field_4_viz
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
        if(iflag_debug .gt. 0)  write(*,*) 'FEM_analyze_vizs', i_step
        call FEM_analyze_vizs                                           &
     &     (i_step, t_VIZ1%ucd_step, t_VIZ1%time_d, viz1)
!
!  Rendering
        if(iflag_debug .gt. 0)  write(*,*) 'visualize_all', i_step
        call istep_viz_w_fix_dt(i_step, t_VIZ1%viz_step)
        call visualize_all(t_VIZ1%viz_step, t_VIZ1%time_d,              &
     &     viz1%geofem, viz1%nod_fld, viz1%ele_4_nod, viz1%jacobians,   &
     &     vizs_v)
      end do
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_vizs
!
!  ---------------------------------------------------------------------
!
      end module analyzer_viz
