!analyzer_fline.f90
!      module analyzer_fline
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine initialize_fline
!!      subroutine analyze_fline
!
      module analyzer_fline
!
      use m_precision
!
      use calypso_mpi
!
      use t_fieldline
      use t_control_data_four_vizs
      use t_VIZ_only_step_parameter
      use t_FEM_mesh_field_4_viz
      use t_VIZ_mesh_field
      use t_vector_for_solver
      use t_mesh_SR
      use t_particle_trace
      use FEM_analyzer_four_vizs
!
      use m_elapsed_labels_4_VIZ
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_viz_ctl = "control_viz"
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ6
!>      Structure of control data for visualization
      type(control_data_four_vizs), save :: vizs_ctl6
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz6
!>      Structure of work area for mesh communications
      type(mesh_SR) :: m_SR16
!>      Structure of mesh and field for visualization only
      type(VIZ_mesh_field), save :: VIZ_DAT6
!>      Structure of field line module
      type(fieldline_module), save :: fline_v6
!>      Structure of field line module
      type(tracer_module), save :: dummy_tracer
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_fline
!
      use t_control_data_vizs
      use input_control_four_vizs
!
      call init_elapse_time_by_TOTAL
      call set_elpsed_label_4_VIZ(elps_VIZ1, elps1)
!
!       set controls
      if (iflag_debug.gt.0) write(*,*) 's_input_control_four_vizs'
      call s_input_control_four_vizs(fname_viz_ctl, vizs_ctl6,          &
     &                               FEM_viz6, t_VIZ6)
!
!  FEM Initialization
      call FEM_initialize_four_vizs                                     &
     &   (elps_VIZ1, t_VIZ6%init_d, t_VIZ6%ucd_step,                    &
     &    t_VIZ6%viz_step, FEM_viz6, VIZ_DAT6, m_SR16)
!
      dummy_tracer%num_trace = 0
!
!  VIZ Initialization
      call FLINE_initialize(t_VIZ6%viz_step%FLINE_t%increment,          &
     &    FEM_viz6%geofem, FEM_viz6%field, dummy_tracer,                &
     &    vizs_ctl6%viz4_ctl%fline_ctls, fline_v6)
!
      end subroutine initialize_fline
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_fline
!
      integer(kind = kint) :: i_step, istep_fline, istep_tracer
!
!
      do i_step = t_VIZ6%init_d%i_time_step, t_VIZ6%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ6%ucd_step) .eqv. .FALSE.) cycle
        if(output_IO_flag(i_step,t_VIZ6%viz_step%FLINE_t)               &
     &       .eqv. .FALSE.) cycle
!
!  Load field data
        call FEM_analyze_four_vizs                                      &
     &     (i_step, t_VIZ6%ucd_step, t_VIZ6%time_d, FEM_viz6, m_SR16)
!
!  Generate field lines
        if(elps_VIZ1%flag_elapsed_V)                                    &
     &           call start_elapsed_time(elps_VIZ1%ist_elapsed_V+12)
        istep_fline                                                     &
     &      = istep_file_w_fix_dt(i_step, t_VIZ6%viz_step%FLINE_t)
        call FLINE_visualize                                            &
     &     (istep_fline, elps_VIZ1%elps_FLINE, t_VIZ6%time_d,           &
     &      FEM_viz6%geofem, VIZ_DAT6%para_surf, FEM_viz6%field,        &
     &      dummy_tracer, fline_v6, m_SR16)
        if(elps_VIZ1%flag_elapsed_V)                                    &
     &           call end_elapsed_time(elps_VIZ1%ist_elapsed_V+12)
      end do
!
      end subroutine analyze_fline
!
!  ---------------------------------------------------------------------
!
      end module analyzer_fline
