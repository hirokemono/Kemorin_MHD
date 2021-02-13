!analyzer_four_vizs.f90
!      module analyzer_four_vizs
!
!     Written by H. Matsui on July, 2006
!
!      subroutine initialize_four_vizs
!      subroutine analyze_four_vizs
!
      module analyzer_four_vizs
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_control_data_four_vizs
      use t_four_visualizers
      use t_VIZ_only_step_parameter
      use t_FEM_mesh_field_4_viz
      use t_VIZ_mesh_field
      use FEM_analyzer_four_vizs
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ4
!
!>      Structure of control data for visualization
      type(control_data_four_vizs), save :: vizs_ctl4
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz4
!>      Structure of data for visualization
      type(VIZ_mesh_field), save :: VIZ_DAT4
!>      Structure of viualization modules
      type(four_visualize_modules), save :: vizs_m4
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_four_vizs
!
      use m_elapsed_labels_4_VIZ
      use m_elapsed_labels_SEND_RECV
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
      call read_control_file_four_vizs(vizs_ctl4)
      call set_ctl_params_four_vizs(vizs_ctl4, FEM_viz4, t_VIZ4, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_viz'
      call FEM_initialize_four_vizs(t_VIZ4%init_d, t_VIZ4%ucd_step,     &
     &    t_VIZ4%viz_step, FEM_viz4, VIZ_DAT4)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_four_visualize'
      call init_four_visualize(VIZ_DAT4%viz_fem, VIZ_DAT4%viz_fld,      &
     &                         vizs_ctl4%viz_ctl_v, vizs_m4)
!
      end subroutine initialize_four_vizs
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_four_vizs
!
      integer(kind=kint ) :: i_step
!
!
      do i_step = t_VIZ4%init_d%i_time_step, t_VIZ4%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ4%ucd_step) .eqv. .FALSE.) cycle
        if(iflag_vizs_w_fix_step(i_step, t_VIZ4%viz_step)               &
     &        .eqv. .FALSE.) cycle
!
!  Load field data
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'FEM_analyze_four_vizs', i_step
        call FEM_analyze_four_vizs                                      &
     &     (i_step, t_VIZ4%ucd_step, t_VIZ4%time_d, FEM_viz4)
!
!  Rendering
        if(iflag_debug .gt. 0)  write(*,*) 'visualize_four', i_step
        call istep_viz_w_fix_dt(i_step, t_VIZ4%viz_step)
        call visualize_four(t_VIZ4%viz_step, t_VIZ4%time_d,             &
     &      VIZ_DAT4%viz_fem, VIZ_DAT4%viz_fld,                         &
     &      VIZ_DAT4%ele_4_nod, VIZ_DAT4%jacobians, vizs_m4)
      end do
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_four_vizs
!
!  ---------------------------------------------------------------------
!
      end module analyzer_four_vizs
