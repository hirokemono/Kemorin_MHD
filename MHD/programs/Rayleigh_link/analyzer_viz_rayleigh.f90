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
!
      use FEM_analyzer_viz_rayleigh
      use t_ctl_data_rayleigh_vizs
      use t_rayleigh_field_address
      use t_visualizer
      use t_VIZ_mesh_field
      use t_VIZ_only_step_parameter
      use t_viz_4_rayleigh
      use t_vector_for_solver
      use t_mesh_SR
      use m_solver_SR
!
      implicit none
!
!>       Structure for time stepping parameters
!!        with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ_r
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_rayleigh_viz), save :: FEM_Rayleigh1
!>        Structure for vectors for solver
      type(vectors_4_solver) :: v_sol31
!
      type(control_data_rayleigh_vizs), save :: rayleigh_vizs_ctl1
      type(visualize_modules), save :: vizs_v
      type(VIZ_mesh_field), save :: VIZ_DAT_r
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
      use FEM_to_VIZ_bridge
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
      if (iflag_debug.gt.0) write(*,*) 'read_ctl_file_rayleigh_viz'
      call read_ctl_file_rayleigh_viz(rayleigh_vizs_ctl1)
      call set_ctl_params_rayleigh_viz(rayleigh_vizs_ctl1,              &
     &    t_VIZ_r, FEM_Rayleigh1, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!      call check_rayleigh_field_address(FEM_Rayleigh1%iphys_ftb)
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_viz_rayleigh'
      call FEM_initialize_viz_rayleigh                                  &
     &   (FEM_Rayleigh1, v_sol31, SR_sig1, SR_r1, SR_i1, SR_il1)
!
!  -------------------------------------------
!  ----   Mesh setting for visualization -----
!  -------------------------------------------
      if(iflag_debug .gt. 0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge                                       &
     &   (t_VIZ_r%viz_step, FEM_Rayleigh1%geofem, VIZ_DAT_r,            &
     &    SR_sig1, SR_r1, SR_i1, SR_il1)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_visualize'
      call init_visualize                                               &
     &   (t_VIZ_r%viz_step, FEM_Rayleigh1%geofem, FEM_Rayleigh1%field,  &
     &    VIZ_DAT_r, rayleigh_vizs_ctl1%viz_ctl_v, vizs_v,              &
     &    SR_sig1, SR_r1, SR_i1, SR_il1)
!
      end subroutine init_viz_rayleigh
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_viz_rayleigh
!
      use FEM_to_VIZ_bridge
!
      integer(kind = kint) :: i_step
      logical :: visval
!
!
      do i_step = t_VIZ_r%init_d%i_time_step,                           &
     &              t_VIZ_r%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ_r%ucd_step) .eqv. .FALSE.) cycle
!
!  Load field data
        if(iflag_debug .gt. 0)                                          &
     &      write(*,*) 'FEM_analyze_viz_rayleigh', i_step
        visval = iflag_vizs_w_fix_step(i_step, t_VIZ_r%viz_step)
        call FEM_analyze_viz_rayleigh(visval, i_step, t_VIZ_r%time_d,   &
     &      FEM_Rayleigh1, v_sol31, SR_sig1, SR_r1)
!
!  Rendering
        if(visval) then
          if(iflag_debug .gt. 0)  write(*,*) 'visualize_all', i_step
          call istep_viz_w_fix_dt(i_step, t_VIZ_r%viz_step)
          call visualize_all(t_VIZ_r%viz_step, t_VIZ_r%time_d,          &
     &      FEM_Rayleigh1%geofem, FEM_Rayleigh1%field, VIZ_DAT_r,       &
     &      vizs_v, v_sol31, SR_sig1, SR_r1, SR_i1, SR_il1)
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
