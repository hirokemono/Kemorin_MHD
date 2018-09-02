!analyzer_viz.f90
!      module analyzer_viz
!
!     Written by H. Matsui on July, 2006
!
!      subroutine init_analyzer
!      subroutine analyze
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
      use t_visualizer
!
      implicit none
!
      type(visualize_modules), save :: vizs_v
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use calypso_mpi
      use m_control_data_vizs
      use set_viz_time_labels
!
      integer(kind = kint) :: ierr
!
      num_elapsed = 80
      call allocate_elapsed_times
      call s_set_viz_time_labels
!
      elapse_labels(num_elapsed) = 'Communication time        '
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_file_vizs'
      call read_control_file_vizs
      call set_control_params_4_viz(my_rank, t_viz_ctl, viz_plt,        &
     &    mesh_file_VIZ, ucd_file_VIZ, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_vizs'
      call FEM_initialize_vizs(ucd_file_VIZ, viz_step_V)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_visualize'
      call init_visualize(femmesh_VIZ, elemesh_VIZ, field_VIZ,          &
     &    viz_ctl_v, vizs_v)
!
      end subroutine init_analyzer
!
!  ---------------------------------------------------------------------
!
      subroutine analyze
!
      integer(kind=kint ) :: i_step, visval
!
!
      do i_step = t_VIZ%init_d%i_time_step, t_VIZ%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ%ucd_step) .ne. izero) cycle
        call set_IO_step_flag(i_step,t_VIZ%ucd_step)
!
!  Load field data
        if(iflag_debug .gt. 0)  write(*,*) 'FEM_analyze_vizs', i_step
        call FEM_analyze_vizs                                           &
     &     (i_step, ucd_file_VIZ, t_VIZ, viz_step_V, visval)
!
!  Rendering
        if(visval .eq. 0) then
          if(iflag_debug .gt. 0)  write(*,*) 'visualize_all', i_step
          call start_elapsed_time(12)
          call visualize_all(viz_step_V, t_VIZ%time_d,                  &
     &        femmesh_VIZ, elemesh_VIZ, field_VIZ,                      &
     &        ele_4_nod_VIZ, jacobians_VIZ, vizs_v)
          call end_elapsed_time(12)
        end if
      end do
!
      call output_elapsed_times
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_viz
