!analyzer_sph_snap.f90
!      module analyzer_sph_snap
!..................................................
!
!      Written by H. Matsui
!      modified by H. Matsui on June, 2005 
!
!      subroutine initialization
!      subroutine evolution
!
      module analyzer_sph_snap
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
      use m_work_time
      use m_control_parameter
      use m_control_params_sph_MHD
      use m_t_int_parameter
      use m_t_step_parameter
!
      use const_coriolis_sph
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap
      use sections_for_1st
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialization
!
      use set_control_sph_mhd
      use set_control_SPH_to_FEM
      use m_ctl_data_sph_MHD
!
!
      total_start = MPI_WTIME()
      write(*,*) 'Simulation start: PE. ', my_rank
!
!     --------------------- 
!
      num_elapsed = 18
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                  '
      elapse_labels(2) = 'Initialization time         '
      elapse_labels(3) = 'Time evolution loop time    '
      elapse_labels(4) = 'Data IO time                '
      elapse_labels(5) = 'Communication for RHS       '
!
      elapse_labels( 6) = 'snapshots_control          '
      elapse_labels( 7) = 'lead_fields_4_sph_mhd      '
      elapse_labels( 8) = 'output_sph_restart_control '
      elapse_labels( 9) = 'Field data output         '
      elapse_labels(10) = 'output_rms_sph_mhd_control '
      elapse_labels(11) = 'PSF_time                   '
      elapse_labels(12) = 'Nonliner_terms             '
!
      elapse_labels(13) =  'Coriolis term             '
      elapse_labels(14) =  'sph backward transform    '
      elapse_labels(15) = 'cal nonlinear terms        '
      elapse_labels(16) = 'sph forward transform      '
      elapse_labels(17) = 'obtain explicit terms      '
!
      elapse_labels(18) = 'Communication time         '
!
!   Load parameter file
!
      call start_eleps_time(1)
      call start_eleps_time(4)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_snap'
      call read_control_4_sph_snap
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_sph_mhd'
      call set_control_4_sph_mhd
      call set_control_4_SPH_to_FEM
!
!    IO elapsed end
!    precondition elaps start
!
      call end_eleps_time(4)
      call start_eleps_time(2)
!
!     --------------------- 
!
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize'
      call FEM_initialize
      call time_prog_barrier
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_snap'
      call SPH_init_sph_snap
      if(iflag_debug .gt. 0) write(*,*) 'SPH_to_FEM_initialize'
      call SPH_to_FEM_initialize
      call time_prog_barrier
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize_surface'
      call init_visualize_surface(ierr)
!
      call end_eleps_time(2)
!
      end subroutine initialization
!
! ----------------------------------------------------------------------
!
      subroutine evolution
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: istep_psf, istep_iso
      integer(kind = kint) :: istep_pvr, istep_fline
!
!     ---------------------
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      i_step_MHD = i_step_init - 1
!*
!*  -------  time evelution loop start -----------
!*
      do
        i_step_MHD = i_step_MHD + 1
        istep_max_dt = i_step_MHD
!
        if( mod(i_step_MHD,i_step_output_rst) .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap'
        call SPH_analyze_snap(i_step_MHD)
!*
!*  -----------  output field data --------------
!*
        call start_eleps_time(1)
        call start_eleps_time(4)
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge'
        call SPH_to_FEM_bridge
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze'
        call FEM_analyze(i_step_MHD, istep_psf, istep_iso,              &
     &      istep_pvr, istep_fline, visval)
!
        call end_eleps_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface'
          call start_eleps_time(11)
          call visualize_surface(istep_psf, istep_iso, ierr)
          call end_eleps_time(11)
        end if
        call end_eleps_time(1)
!
!*  -----------  exit loop --------------
!*
        if(i_step_MHD .ge. i_step_number) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize
!
      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_eleps(18)
      call end_eleps_time(1)
!
      call output_elapsed_times
!
      call time_prog_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_snap
