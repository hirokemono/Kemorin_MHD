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
      use m_elapsed_labels_4_MHD
      use m_SPH_MHD_model_data
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
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
      call append_COMM_TIME_to_elapsed
!
!   Load parameter file
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
      call read_control_4_sph_MHD_noviz(MHD_ctl_name, DNS_MHD_ctl1)
!
      call input_control_4_SPH_MHD_nosnap                               &
     &   (MHD_files1, DNS_MHD_ctl1, MHD_step1, SPH_model1,              &
     &    SPH_WK1%trns_WK, SPH_WK1%monitor, SPH_MHD1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
!
!    precondition elaps start
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!   matrix assembling
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_linear_conv'
      call SPH_initialize_linear_conv                                   &
     &   (MHD_files1, FEM_d1%iphys, SPH_model1,                         &
     &    MHD_step1, MHD_IO1%rst_IO, SPH_MHD1, SPH_WK1)
      call calypso_MPI_barrier
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_licv
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_licv
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
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(MHD_step1%init_d, MHD_step1%time_d)
      iflag_finish = 0
!
!*  -------  time evelution  -----------
!*
      do istep = 1, MHD_step1%finish_d%i_end_step
        call evolve_time_data(MHD_step1%time_d)
!
!*  ----------  add time evolution -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_linear_conv'
        call SPH_analyze_linear_conv(MHD_step1%time_d%i_time_step,      &
     &      MHD_files1, SPH_model1, iflag_finish,                       &
     &      MHD_step1, MHD_IO1%rst_IO, SPH_MHD1, SPH_WK1)
!*
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. izero) exit
      end do
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!  time evolution end
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_licv'
!      call SPH_finalize_licv
!
      call copy_COMM_TIME_to_elaps
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
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
