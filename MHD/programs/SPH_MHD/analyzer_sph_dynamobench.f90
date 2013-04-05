!>@file   analyzer_sph_dynamobench.f90
!!        module analyzer_sph_dynamobench
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!! @n      modified in 2013
!
!> @brief Initialzation and evolution loop for dynamo benchmark check
!!
!!@verbatim
!!      subroutine initialization
!!      subroutine evolution
!!@endverbatim
!
      module analyzer_sph_dynamobench
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
      use SPH_analyzer_d_bench
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
      use m_ctl_data_noviz_MHD
      use set_control_sph_mhd
      use set_control_sph_data_MHD
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
      elapse_labels(10) = 'const_data_4_dynamobench   '
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_snap_noviz'
      call read_control_4_snap_noviz
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_sph_mhd'
      call set_control_4_sph_mhd
      call set_ctl_params_dynamobench
!
!    IO elapsed end
!    precondition elaps start
!
      call end_eleps_time(4)
      call start_eleps_time(2)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_dbench'
      call SPH_init_sph_dbench
      call time_prog_barrier
!
      call end_eleps_time(2)
!
      end subroutine initialization
!
! ----------------------------------------------------------------------
!
      subroutine evolution
!
!
!*  -----------  set initial step data --------------
!*
      call start_eleps_time(3)
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
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_dbench'
        call SPH_analyze_dbench(i_step_MHD)
!*
!*  -----------  exit loop --------------
!*
        if(i_step_MHD .ge. i_step_number) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_dbench'
      call SPH_finalize_dbench
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
      end module analyzer_sph_dynamobench
