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
      use FEM_analyzer_four_vizs
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
!       set controls
      if (iflag_debug.gt.0) write(*,*) 's_input_control_four_vizs'
      call s_input_control_four_vizs(fname_viz_ctl, vizs_ctl6,          &
     &                               FEM_viz6, t_VIZ6)
!
!  FEM Initialization
      call FEM_initialize_four_vizs(t_VIZ6%init_d, t_VIZ6%ucd_step,     &
     &    t_VIZ6%viz_step, FEM_viz6, VIZ_DAT6, m_SR16)
!
!  VIZ Initialization
      call FLINE_initialize(t_VIZ6%viz_step%FLINE_t%increment,          &
     &    FEM_viz6%geofem, FEM_viz6%field,                              &
     &    vizs_ctl6%viz4_ctl%fline_ctls, fline_v6)
!
      end subroutine initialize_fline
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_fline
!
      integer(kind = kint) :: i_step, istep_fline
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
        istep_fline                                                     &
     &      = istep_file_w_fix_dt(i_step, t_VIZ6%viz_step%FLINE_t)
        call FLINE_visualize                                            &
     &     (istep_fline, t_VIZ6%time_d, FEM_viz6%geofem,                &
     &      VIZ_DAT6%inod_dbl, VIZ_DAT6%iele_dbl, VIZ_DAT6%next_tbl,    &
     &      VIZ_DAT6%para_surf%iele_4_surf_dbl, FEM_viz6%field, fline_v6)
      end do
!
      end subroutine analyze_fline
!
!  ---------------------------------------------------------------------
!
      end module analyzer_fline
