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
      use t_control_data_four_vizs
      use t_volume_rendering
      use t_VIZ_only_step_parameter
      use t_FEM_mesh_field_4_viz
      use t_VIZ_mesh_field
      use FEM_analyzer_four_vizs
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ3
!>      Structure of control data for visualization
      type(control_data_four_vizs), save :: pvr_ctl3
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz3
!>      Structure of mesh and field for visualization only
      type(VIZ_mesh_field), save :: pvr3
!>      Structure of viualization modules
      type(volume_rendering_module), save :: vizs_pvr3
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
      use volume_rendering
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
      if (iflag_debug.gt.0) write(*,*) 'read_control_file_four_vizs'
      call read_control_file_four_vizs(pvr_ctl3)
      call set_ctl_params_four_vizs(pvr_ctl3, FEM_viz3, t_VIZ3, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_four_vizs(t_VIZ3%init_d, t_VIZ3%ucd_step,     &
     &                              t_VIZ3%viz_step, FEM_viz3, pvr3)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'PVR_initialize'
      call PVR_initialize                                               &
     &   (t_VIZ3%viz_step%PVR_t%increment, FEM_viz3%geofem,             &
     &    FEM_viz3%field, pvr_ctl3%viz_ctl_v%pvr_ctls, vizs_pvr3)
!
      end subroutine initialize_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_pvr
!
      use t_IO_step_parameter
      use volume_rendering
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
        call FEM_analyze_four_vizs                                      &
     &     (i_step, t_VIZ3%ucd_step, t_VIZ3%time_d, FEM_viz3)
!
!  Rendering
        if(iflag_debug .gt. 0)  write(*,*) 'PVR_visualize', i_step
        call istep_viz_w_fix_dt(i_step, t_VIZ3%viz_step)
        call PVR_visualize                                              &
     &     (t_VIZ3%viz_step%istep_pvr, t_VIZ3%time_d%time,              &
     &     FEM_viz3%geofem, pvr3%jacobians, FEM_viz3%field, vizs_pvr3)
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
