!analyzer_lic.f90
!      module analyzer_lic
!
!     Written by H. Matsui on July, 2006
!
!      subroutine initialize_lic
!      subroutine analyze_lic
!
      module analyzer_lic
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_control_data_all_vizs
      use t_LIC_visualizer
      use t_VIZ_only_step_parameter
      use t_FEM_mesh_field_4_viz
      use t_VIZ_mesh_field
      use t_vector_for_solver
      use t_mesh_SR
      use m_solver_SR
      use FEM_analyzer_viz
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ1
!
!>      Structure of control data for visualization
      type(control_data_vizs), save :: vizs_ctl1
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz1
!>        Structure for vectors for solver
      type(vectors_4_solver) :: v_sol11
!>      Structure of data for visualization
      type(VIZ_mesh_field), save :: VIZ_DAT1
!>      Structure of viualization modules
      type(lic_visualize_modules), save :: lic_v1
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_lic
!
      use m_elapsed_labels_4_VIZ
      use m_elapsed_labels_SEND_RECV
      use m_elapsed_labels_4_REPART
      use m_work_time_4_sleeve_extend
      use FEM_to_VIZ_bridge
      use lic_rendering_test
!
      integer(kind = kint) :: ierr
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv
      call elpsed_label_4_repartition
      call elpsed_label_4_sleeve_ext
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_file_vizs'
      call read_control_file_vizs(vizs_ctl1)
      call set_control_params_4_viz(vizs_ctl1, FEM_viz1, t_VIZ1, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_viz'
      call FEM_initialize_viz(t_VIZ1%init_d, t_VIZ1%ucd_step,           &
     &    t_VIZ1%viz_step, FEM_viz1, v_sol11,                           &
     &    SR_sig1, SR_r1, SR_i1, SR_il1)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge                                       &
     &   (t_VIZ1%viz_step, FEM_viz1%geofem, VIZ_DAT1,                   &
     &    SR_sig1, SR_r1, SR_i1, SR_il1)
      if(iflag_debug .gt. 0)  write(*,*) 'init_LIC_visualize'
      call init_LIC_visualize(t_VIZ1%viz_step, FEM_viz1%geofem,         &
     &    FEM_viz1%field, VIZ_DAT1, vizs_ctl1%viz_ctl_v, lic_v1,        &
     &    SR_sig1, SR_r1, SR_i1, SR_il1)
!
      end subroutine initialize_lic
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_lic
!
      use FEM_to_VIZ_bridge
      use lic_rendering_test
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
     &                       FEM_viz1, v_sol11, SR_sig1, SR_r1)
!
!  Rendering
        if(iflag_debug .gt. 0)  write(*,*) 'visualize_LIC', i_step
        call istep_viz_w_fix_dt(i_step, t_VIZ1%viz_step)
        call visualize_LIC(t_VIZ1%viz_step, t_VIZ1%time_d,              &
     &      FEM_viz1%geofem, FEM_viz1%field, VIZ_DAT1, lic_v1,          &
     &      v_sol11, SR_sig1, SR_r1, SR_i1, SR_il1)
      end do
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_lic
!
!  ---------------------------------------------------------------------
!
      end module analyzer_lic
