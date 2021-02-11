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
      use t_control_data_vizs_pvr
      use t_visualizer
      use t_VIZ_only_step_parameter
      use t_FEM_mesh_field_4_viz
      use FEM_analyzer_viz_pvr
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ3
!>      Structure of control data for visualization
      type(control_data_pvr_vizs), save :: vizs_ctl3
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz3
!>      Structure of mesh and field for visualization only
      type(FEM_mesh_field_4_pvr), save :: pvr3
!>      Structure of viualization modules
      type(visualize_modules), save :: vizs_v3
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
!
      integer(kind = kint) :: ierr
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv

      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!     read controls
      if (iflag_debug.gt.0) write(*,*) 'read_control_file_pvr_vizs'
      call read_control_file_pvr_vizs(vizs_ctl3)
      call set_control_params_4_pvr(vizs_ctl3, pvr3, t_VIZ3, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_pvr                                           &
     &   (t_VIZ3%init_d, t_VIZ3%ucd_step, t_VIZ3%viz_step,              &
     &    FEM_viz3, pvr3)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_visualize'
      call init_visualize(FEM_viz3%geofem, FEM_viz3%field,              &
     &                    vizs_ctl3%viz_ctl_v, vizs_v3)
!
      end subroutine initialize_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_pvr
!
      use t_IO_step_parameter
!
      integer(kind = kint) :: i_step, istep_pvr
!
!
      do i_step = t_VIZ3%init_d%i_time_step, t_VIZ3%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ3%ucd_step) .eqv. .FALSE.) cycle
        if(output_IO_flag(i_step,t_VIZ3%viz_step%PVR_t)                 &
     &       .eqv. .FALSE.) cycle
!
!  Load field data
        call FEM_analyze_pvr                                            &
     &     (i_step, t_VIZ3%ucd_step, t_VIZ3%time_d, FEM_viz3, pvr3)
!
!  Rendering
        if(iflag_debug .gt. 0)  write(*,*) 'visualize_all', i_step
        call istep_viz_w_fix_dt(i_step, t_VIZ3%viz_step)
        call visualize_all(t_VIZ3%viz_step, t_VIZ3%time_d,              &
     &      FEM_viz3%geofem, FEM_viz3%field,                            &
     &      pvr3%ele_4_nod, pvr3%jacobians, vizs_v3)
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
