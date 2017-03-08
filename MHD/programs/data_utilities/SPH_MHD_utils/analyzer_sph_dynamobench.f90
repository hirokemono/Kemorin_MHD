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
!!      subroutine initialize_sph_dynamobench
!!      subroutine evolution_sph_dynamobench
!!@endverbatim
!
      module analyzer_sph_dynamobench
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_t_step_parameter
!
      use SPH_analyzer_d_bench
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: snap_ctl_name = 'control_snapshot'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_dynamobench
!
      use t_ctl_data_sph_MHD_psf
      use m_ctl_data_sph_MHD
      use m_SGS_control_parameter
      use m_spheric_parameter
      use m_node_phys_data
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_noviz'
      call read_control_4_sph_MHD_noviz(snap_ctl_name, MHD_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_dynamobench'
      call input_control_SPH_dynamobench(MHD_ctl1, sph1, comms_sph1,    &
     &    sph_grps1, rj_fld1, nod_fld1, pwr1, SGS_par1)
      call end_eleps_time(4)
!
!    precondition elaps start
!
      call start_eleps_time(2)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_dbench'
      call SPH_init_sph_dbench(iphys)
      call calypso_MPI_barrier
!
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_dynamobench
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_dynamobench
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
!
        if(output_IO_flag(i_step_MHD,rst_step1) .ne. 0) cycle
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
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_dbench'
!      call SPH_finalize_dbench
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_dynamobench
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_dynamobench
