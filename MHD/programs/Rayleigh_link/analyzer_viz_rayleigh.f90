!>@file   analyzer_viz_rayleigh.f90
!!@brief  module analyzer_viz_rayleigh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2019
!
!>@brief  Main loop of visualization of Rayleigh data
!!
!!@verbatim
!!      subroutine init_viz_rayleigh
!!      subroutine analyze_viz_rayleigh
!!@endverbatim
!
      module analyzer_viz_rayleigh
!
      use m_precision
      use m_machine_parameter
!
      use m_work_time
      use m_viz_4_rayleigh
!
      use FEM_analyzer_viz_rayleigh
      use t_control_data_all_vizs
      use t_visualizer
!
      implicit none
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
      subroutine init_viz_rayleigh
!
      use calypso_mpi
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
      call read_control_file_vizs(vizs_ctl1)
      call set_control_params_4_viz                                     &
     &   (vizs_ctl1%t_viz_ctl, vizs_ctl1%viz_plt,                       &
     &    mesh_file_VIZ, ucd_file_VIZ, t_VIZ, viz_step_V, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_viz_rayleigh'
      call FEM_initialize_viz_rayleigh(viz_step_V)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_visualize'
      call init_visualize                                               &
     &   (femmesh_VIZ, field_VIZ, vizs_ctl1%viz_ctl_v, vizs_v)
!
      end subroutine init_viz_rayleigh
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_viz_rayleigh
!
      integer(kind=kint ) :: i_step, visval
!
!
      do i_step = t_VIZ%init_d%i_time_step, t_VIZ%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ%ucd_step) .ne. izero) cycle
        call set_IO_step_flag(i_step,t_VIZ%ucd_step)
!
!  Load field data
        if(iflag_debug .gt. 0)                                          &
     &      write(*,*) 'FEM_analyze_viz_rayleigh', i_step
        call FEM_analyze_viz_rayleigh                                   &
     &     (i_step, t_VIZ, viz_step_V, visval)
!
!  Rendering
        if(visval .eq. 0) then
          if(iflag_debug .gt. 0)  write(*,*) 'visualize_all', i_step
          call visualize_all                                            &
     &       (viz_step_V, t_VIZ%time_d, femmesh_VIZ, field_VIZ,         &
     &        ele_4_nod_VIZ, jacobians_VIZ, vizs_v)
        end if
      end do
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_viz_rayleigh
!
!  ---------------------------------------------------------------------
!
      end module analyzer_viz_rayleigh
