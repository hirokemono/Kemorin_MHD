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
      use calypso_mpi
!
      use m_visualization
!
      use FEM_analyzer_viz_pvr
      use t_control_data_all_vizs
      use t_volume_rendering
      use t_VIZ_only_step_parameter
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ3
!
      type(control_data_vizs), save :: vizs_ctl3
!
      type(volume_rendering_module), save :: pvr_v
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_pvr
!
      use load_mesh_and_field_4_viz
!
      integer(kind = kint) :: ierr
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_file_vizs'
      call read_control_file_vizs(vizs_ctl3)
      call set_control_params_4_viz                                     &
     &   (vizs_ctl3%t_viz_ctl, vizs_ctl3%viz_plt,                       &
     &    mesh_file_VIZ, ucd_file_VIZ, t_VIZ3, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_pvr(t_VIZ3%init_d, ucd_file_VIZ, ucd_VIZ)
!
!  VIZ Initialization
      call PVR_initialize(femmesh_VIZ, field_VIZ,                       &
     &    vizs_ctl3%viz_ctl_v%pvr_ctls, pvr_v)
      call calypso_MPI_barrier
!
      end subroutine initialize_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_pvr
!
      use t_IO_step_parameter
!
      integer(kind=kint ) :: i_step
!
!
      do i_step = t_VIZ3%init_d%i_time_step, t_VIZ3%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ3%ucd_step) .ne. izero) cycle
        call set_IO_step_flag(i_step,t_VIZ3%ucd_step)
!
!  Load field data
        call FEM_analyze_pvr(i_step, ucd_file_VIZ,                      &
     &      t_VIZ3%time_d, t_VIZ3%viz_step, ucd_VIZ)
!
!  Rendering
        call PVR_visualize(t_VIZ3%viz_step%PVR_t,                       &
     &      femmesh_VIZ, jacobians_VIZ, field_VIZ, pvr_v)
      end do
!
      end subroutine analyze_pvr
!
!  ---------------------------------------------------------------------
!
      end module analyzer_pvr
