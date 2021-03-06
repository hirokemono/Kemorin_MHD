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
      use FEM_analyzer_four_vizs
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ6
!>      Structure of control data for visualization
      type(control_data_four_vizs), save :: vizs_ctl6
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz6
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
!
      integer(kind = kint) :: ierr
!
!     read controls
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_params_four_vizs'
      call read_control_file_four_vizs(vizs_ctl6)
      call set_ctl_params_four_vizs(vizs_ctl6, FEM_viz6, t_VIZ6, ierr)
!
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_four_vizs(t_VIZ6%init_d, t_VIZ6%ucd_step,     &
     &    t_VIZ6%viz_step, FEM_viz6, VIZ_DAT6)
!
!  VIZ Initialization
      call FLINE_initialize(VIZ_DAT6%viz_fem, VIZ_DAT6%viz_fld,         &
     &    vizs_ctl6%viz_ctl_v%fline_ctls, fline_v6)
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
     &     (i_step, t_VIZ6%ucd_step, t_VIZ6%time_d, FEM_viz6)
!
!  Generate field lines
        istep_fline                                                     &
     &      = istep_file_w_fix_dt(i_step, t_VIZ6%viz_step%FLINE_t)
        call FLINE_visualize(istep_fline, VIZ_DAT6%viz_fem,             &
     &      VIZ_DAT6%ele_4_nod, VIZ_DAT6%viz_fld, fline_v6)
      end do
!
      end subroutine analyze_fline
!
!  ---------------------------------------------------------------------
!
      end module analyzer_fline
