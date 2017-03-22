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
      use volume_rendering
!
      implicit none
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
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_data_vizs
      call set_control_params_4_viz(my_rank, t_viz_ctl, viz_plt,        &
     &   mesh_file_VIZ, ucd_VIZ, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_pvr
!
!  VIZ Initialization
      call PVR_initialize(femmesh_VIZ%mesh%node, femmesh_VIZ%mesh%ele,  &
     &    elemesh_VIZ%surf, femmesh_VIZ%group, field_VIZ)
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
        if(output_IO_flag(i_step,t_VIZ%ucd_step) .ne. izero) cycle
        t_VIZ%ucd_step%istep_file = i_step / t_VIZ%ucd_step%increment
!
!  Load field data
        call FEM_analyze_pvr(i_step, t_VIZ, viz_step_V%PVR_t)
!
!  Rendering
        call PVR_visualize(viz_step_V%PVR_t%istep_file,                 &
     &      femmesh_VIZ%mesh%node, femmesh_VIZ%mesh%ele,                &
     &      elemesh_VIZ%surf, femmesh_VIZ%group, jac_VIZ_q, field_VIZ)
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_pvr
