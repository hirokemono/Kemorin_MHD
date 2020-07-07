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
!
      use m_work_time
      use m_visualization
!
      use FEM_analyzer_viz
      use t_control_data_all_vizs
      use t_visualizer
      use t_VIZ_only_step_parameter
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ1
!
      type(control_data_vizs), save :: vizs_ctl1
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
      use calypso_mpi
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
      call set_control_params_4_viz                                     &
     &   (vizs_ctl1%t_viz_ctl, vizs_ctl1%viz_plt,                       &
     &    mesh_file_VIZ, ucd_file_VIZ, t_VIZ1, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_vizs'
      call FEM_initialize_vizs                                          &
     &   (ucd_file_VIZ, t_VIZ1%init_d, t_VIZ1%viz_step, ucd_VIZ)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_visualize'
      call init_visualize                                               &
     &   (femmesh_VIZ, field_VIZ, vizs_ctl1%viz_ctl_v, vizs_v)
!
      end subroutine initialize_vizs
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_vizs
!
      use load_mesh_and_field_4_viz
!
      integer(kind=kint ) :: i_step, visval
!
!
      do i_step = t_VIZ1%init_d%i_time_step, t_VIZ1%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ1%ucd_step) .ne. izero) cycle
        call set_IO_step_flag(i_step,t_VIZ1%ucd_step)
!
!  Load field data
        if(iflag_debug .gt. 0)  write(*,*) 'FEM_analyze_vizs', i_step
        call FEM_analyze_vizs(i_step, ucd_file_VIZ,                     &
     &      t_VIZ1%time_d, t_VIZ1%viz_step, ucd_VIZ, visval)
!
!  Rendering
        if(visval .eq. 0) then
          if(iflag_debug .gt. 0)  write(*,*) 'visualize_all', i_step
          call visualize_all(t_VIZ1%viz_step, t_VIZ1%time_d,            &
     &        femmesh_VIZ, field_VIZ, ele_4_nod_VIZ, jacobians_VIZ,     &
     &        vizs_v)
        end if
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
