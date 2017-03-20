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
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_sph_trans_arrays_MHD
      use m_MHD_step_parameter
!
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
      use t_ctl_data_sph_MHD_psf
      use m_ctl_data_sph_MHD
      use m_SGS_control_parameter
      use m_node_phys_data
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
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
      call read_control_4_sph_MHD_noviz(MHD_ctl_name, MHD_ctl1)
!
      call input_control_4_SPH_MHD_nosnap(MHD_ctl1, sph1, comms_sph1,   &
     &    sph_grps1, rj_fld1, pwr1, SGS_par1, trns_WK1%dynamic_SPH)
      time_d1%dt = init_d1%dt
      call end_eleps_time(4)
!
!    precondition elaps start
!
      call start_eleps_time(2)
!
!   matrix assembling
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_linear_conv'
      call SPH_initialize_linear_conv(iphys, MHD_step1)
      call calypso_MPI_barrier
!
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_licv
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_licv
!
      use m_t_step_parameter
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use cal_momentum_eq_explicit
      use sph_mhd_rst_IO_control
      use set_reference_sph_mhd
!
        integer(kind=kint ) :: istep, iflag_finish
!
!     ---------------------
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(init_d1, time_d1)
      iflag_finish = 0
!
!*  -------  time evelution  -----------
!*
      do istep = 1, i_step_number
        call evolve_time_data(time_d1)
!
!*  ----------  add time evolution -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_linear_conv'
        call SPH_analyze_linear_conv                                    &
     &     (time_d1%i_time_step, iflag_finish, MHD_step1)
!*
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. izero) exit
      end do
!
      call end_eleps_time(3)
!
!  time evolution end
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_licv'
!      call SPH_finalize_licv
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
        end subroutine evolution_sph_licv
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_licv
