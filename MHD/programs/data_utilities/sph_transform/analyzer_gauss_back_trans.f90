!analyzer_gauss_back_trans.f90
!      module analyzer_gauss_back_trans
!..................................................
!
      module analyzer_gauss_back_trans
!
!      modified by H. Matsui on Jan., 2008
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
      use m_work_time
!
      use FEM_analyzer_back_trans
      use SPH_analyzer_gauss_b_trans
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
      subroutine init_analyzer
!
      use m_ctl_data_4_sph_trans
      use m_ctl_params_sph_trans
!
!
      num_elapsed = 30
      call allocate_elapsed_times
!
!   -----  read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans
!
!  ------  initialize FEM data
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_init_gauss_back_trans'
      call SPH_init_gauss_back_trans
!
      call  init_visualize(ierr)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
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
        call SPH_analyze_gauss_back_trans(i_step, visval)
!
        call FEM_analyze_back_trans(i_step, istep_psf, istep_iso,       &
     &          istep_pvr, istep_fline, visval)
!
        if (visval .eq. 0) then
          call visualize_all(istep_psf, istep_iso,                      &
     &        istep_pvr, istep_fline, ierr)
        end if
      end do
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_gauss_back_trans
