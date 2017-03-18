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
      use m_t_step_parameter
      use m_sph_trans_arrays_MHD
      use m_MHD_step_parameter
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
      use t_ctl_data_sph_MHD_psf
      use m_ctl_data_sph_MHD
      use m_SGS_control_parameter
      use m_node_phys_data
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use set_control_sph_mhd
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
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD(iphys, MHD_step1)
!
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
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
      time_d1%time =        time_init
      time_d1%i_time_step = i_step_init
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        time_d1%time = time_d1%time + time_d1%dt
        time_d1%i_time_step = time_d1%i_time_step + 1
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD                                            &
     &     (time_d1%i_time_step, iflag_finish, MHD_step1)
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
