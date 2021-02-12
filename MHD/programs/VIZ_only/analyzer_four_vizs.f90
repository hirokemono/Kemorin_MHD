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
      use t_control_data_vizs_pvr
      use t_four_visualizers
      use t_VIZ_only_step_parameter
      use t_FEM_mesh_field_4_viz
      use FEM_analyzer_viz_pvr
      use t_VIZ_mesh_field
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ6
!
!>      Structure of control data for visualization
      type(control_data_pvr_vizs), save :: pvr_ctls6
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz6
!>      Structure of data for visualization
      type(VIZ_mesh_field), save :: viz6
!>      Structure of viualization modules
      type(four_visualize_modules), save :: vizs_v6
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
      call read_control_file_pvr_vizs(pvr_ctls6)
      call set_control_params_4_pvr(pvr_ctls6, FEM_viz6, t_VIZ6, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_viz'
      call FEM_initialize_pvr(t_VIZ6%init_d, t_VIZ6%ucd_step,           &
     &                        t_VIZ6%viz_step, FEM_viz6, viz6)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_visualize'
      call init_four_visualize                                          &
     &   (viz6%viz_fem, viz6%viz_fld, pvr_ctls6%viz_ctl_v, vizs_v6)
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
      do i_step = t_VIZ6%init_d%i_time_step, t_VIZ6%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ6%ucd_step) .eqv. .FALSE.) cycle
        if(iflag_vizs_w_fix_step(i_step, t_VIZ6%viz_step)               &
     &        .eqv. .FALSE.) cycle
!
!  Load field data
        if(iflag_debug .gt. 0)  write(*,*) 'FEM_analyze_viz', i_step
        call FEM_analyze_pvr                                            &
     &     (i_step, t_VIZ6%ucd_step, t_VIZ6%time_d, FEM_viz6)
!
!  Rendering
        if(iflag_debug .gt. 0)  write(*,*) 'visualize_four', i_step
        call istep_viz_w_fix_dt(i_step, t_VIZ6%viz_step)
        call visualize_four(t_VIZ6%viz_step, t_VIZ6%time_d,             &
     &      viz6%viz_fem, viz6%viz_fld, viz6%ele_4_nod, viz6%jacobians, &
     &      vizs_v6)
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
