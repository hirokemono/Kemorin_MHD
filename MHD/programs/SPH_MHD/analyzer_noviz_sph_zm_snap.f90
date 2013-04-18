!analyzer_noviz_sph_zm_snap.f90
!      module analyzer_noviz_sph_zm_snap
!..................................................
!
!      Written by H. Matsui
!      modified by H. Matsui on June, 2005 
!
!      subroutine initialization
!      subroutine evolution
!
      module analyzer_noviz_sph_zm_snap
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
      use SPH_analyzer_zm_snap
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_snap_noviz'
      call read_control_4_snap_noviz
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
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_zm_snap'
      call SPH_init_sph_zm_snap
      if(iflag_debug .gt. 0) write(*,*) 'SPH_to_FEM_initialize'
      call SPH_to_FEM_initialize
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
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_zm_snap'
        call SPH_analyze_zm_snap(i_step_MHD)
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
      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_zm_snap'
      call SPH_finalize_zm_snap
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
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
      end module analyzer_noviz_sph_zm_snap
