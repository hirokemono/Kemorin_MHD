!analyzer_zm_streamfunc.f90
!      module analyzer_zm_streamfunc
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine init_zm_streamfunc
!      subroutine analyze_zm_streamfunc
!
      module analyzer_zm_streamfunc
!
      use m_precision
      use m_parallel_var_dof
      use m_work_time
!
      use SPH_analyzer_back_trans
      use FEM_analyzer_back_trans
      use SPH_analyzer_zm_streamfunc
      use visualizer_all
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_zm_streamfunc
!
      use m_ctl_data_4_sph_trans
      use m_ctl_params_sph_trans
!
!
      num_elapsed = 30
      call allocate_elapsed_times
!
!   ----  read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
      call set_ctl_data_4_zm_streamline
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_back_trans'
      call SPH_initialize_back_trans
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_visualize'
      call init_visualize(ierr)
!
      end subroutine init_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zm_streamfunc
!
      use m_t_step_parameter
!
      integer(kind=kint ) :: visval, i_step
      integer(kind=kint ) :: istep_psf, istep_iso
      integer(kind=kint ) :: istep_pvr, istep_fline
!
!
      do i_step = i_step_init, i_step_number
        if (iflag_debug.gt.0) write(*,*) 'step ', i_step, 'start...'
!
        call SPH_analyze_zm_streamfunc(i_step, visval)
!
        call FEM_analyze_back_trans(i_step, istep_psf, istep_iso,       &
     &          istep_pvr, istep_fline, visval)
!
        if(visval .eq. 0) then
          call visualize_all(istep_psf, istep_iso,                      &
     &        istep_pvr, istep_fline, ierr)
        end if
      end do
!
      end subroutine analyze_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      end module analyzer_zm_streamfunc
