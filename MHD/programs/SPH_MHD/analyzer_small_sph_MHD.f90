!>@file   analyzer_small_sph_MHD.f90
!!@brief  module analyzer_small_sph_MHD
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!        without visualization and snapshot routines
!!
!!@verbatim
!!      subroutine initialize_sph_mhd_only
!!      subroutine evolution_sph_mhd_only
!!@endverbatim
!
!
      module analyzer_small_sph_MHD
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
!
      use SPH_analyzer_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd_only
!
      use set_control_sph_mhd
      use m_ctl_data_sph_MHD_psf
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_noviz'
      call read_control_4_sph_MHD_noviz
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_sph_mhd'
      call set_control_4_sph_mhd
!
!    IO elapsed end 
!    precondition elaps start
!
      call end_eleps_time(4)
      call start_eleps_time(2)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD
      call calypso_MPI_barrier
!
      call end_eleps_time(2)
!
      end subroutine initialize_sph_mhd_only
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd_only
!
      integer(kind = kint) :: iflag_finish
!
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      time =       time_init
      i_step_MHD = i_step_init
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        time = time + dt
        i_step_MHD = i_step_MHD + 1
        istep_max_dt = i_step_MHD
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(i_step_MHD, iflag_finish)
!
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd_only
!
! ----------------------------------------------------------------------
!
      end module analyzer_small_sph_MHD
