!>@file   analyzer_sph_licv.f90
!!@brief  module analyzer_sph_licv
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for linear convection model in spherical shell
!!
!!@verbatim
!!      subroutine initialize_sph_licv
!!      subroutine evolution_sph_licv
!!@endverbatim
!
      module analyzer_sph_licv
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
      use m_work_time
      use m_control_parameter
      use m_control_params_sph_MHD
!
      use const_coriolis_sph
      use SPH_analyzer_licv
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_licv
!
      use set_control_sph_mhd
      use m_ctl_data_noviz_MHD
      use init_sph_MHD_elapsed_label
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
!
!   Load parameter file
!
      call start_eleps_time(1)
      call start_eleps_time(4)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_MHD_noviz'
      call read_control_4_MHD_noviz
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_sph_mhd'
      call set_control_4_sph_mhd
!
!    IO elapsed end 
!    precondition elaps start
!
      call end_eleps_time(4)
      call start_eleps_time(2)
!
!     --------------------- 
!
      call time_prog_barrier
!
!   matrix assembling
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_linear_conv'
      call SPH_initialize_linear_conv
      call time_prog_barrier
!
      call time_prog_barrier
!
      call end_eleps_time(2)
!
      end subroutine initialize_sph_licv
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_licv
!
      use m_t_step_parameter
      use m_t_int_parameter
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use cal_momentum_eq_explicit
      use sph_mhd_rst_IO_control
      use set_reference_sph_mhd
!
        integer(kind=kint ) :: istep
!
!     ---------------------
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      time =       time_init
      i_step_MHD = i_step_init
!
!*  -------  time evelution  -----------
!*
      do istep = 1, i_step_number
        time = time + dt
        i_step_MHD = i_step_MHD + 1
        istep_max_dt = i_step_MHD
!
!*  ----------  add time evolution -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 's_cal_expricit_sph_euler'
        call SPH_analyze_linear_conv(i_step_MHD)
!*
!*  -----------  exit loop --------------
!*
        if(i_step_MHD .ge. i_step_number) exit
      end do
!
      call end_eleps_time(3)
!
!  time evolution end
!
      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_licv'
      call SPH_finalize_licv
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
!
      call output_elapsed_times
!
      call time_prog_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
        end subroutine evolution_sph_licv
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_licv
