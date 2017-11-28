!analyzer_pvr.f90
!      module analyzer_pvr
!
!     Written by H. Matsui on July, 2006
!
      module analyzer_pvr
!
      use m_precision
!
      use m_visualization
!
      use FEM_analyzer_viz_pvr
      use t_volume_rendering
!
      implicit none
!
      type(volume_rendering_module), save :: pvr_v
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialization
!
      use calypso_mpi
      use m_control_data_vizs
!
      integer(kind = kint) :: ierr
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_file_vizs'
      call read_control_file_vizs
      call set_control_params_4_viz(my_rank, t_viz_ctl, viz_plt,        &
     &   mesh_file_VIZ, ucd_file_VIZ, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_pvr
!
!  VIZ Initialization
      call PVR_initialize(femmesh_VIZ, elemesh_VIZ, field_VIZ,          &
     &    viz_ctl_v%fline_ctls, pvr_v)
      call calypso_MPI_barrier
!
      end subroutine initialization
!
!  ---------------------------------------------------------------------
!
      subroutine analyze
!
      use t_IO_step_parameter
!
      integer(kind=kint ) :: i_step
!
!
      do i_step = t_VIZ%init_d%i_time_step, t_VIZ%finish_d%i_end_step
        if(set_IO_step_flag(i_step,t_VIZ%ucd_step) .ne. izero) cycle
!
!  Load field data
        call FEM_analyze_pvr(i_step, t_VIZ, viz_step_V%PVR_t)
!
!  Rendering
        call PVR_visualize(viz_step_V%PVR_t%istep_file,                 &
     &      femmesh_VIZ, elemesh_VIZ, jacobians_VIZ, field_VIZ, pvr_v)
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_pvr
