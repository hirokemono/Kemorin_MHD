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
      use m_sph_trans_arrays_MHD
      use m_MHD_step_parameter
      use m_physical_property
!
      use SPH_analyzer_MHD
      use init_sph_MHD_elapsed_label
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
      use m_node_phys_data
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_bc_data_list
      use set_control_sph_mhd
      use input_control_sph_MHD
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
!
!   Load parameter file
!
      call start_elapsed_time(1)
      call start_elapsed_time(4)
      call read_control_4_sph_MHD_noviz(MHD_ctl_name, DNS_MHD_ctl1)
!
      call input_control_4_SPH_MHD_nosnap(MHD_files1, bc_sph_IO1,       &
     &    DNS_MHD_ctl1, sph1, comms_sph1, sph_grps1, rj_fld1, pwr1,     &
     &    MHD_step1, MHD_prop1, MHD_BC1, trns_WK1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
      call end_elapsed_time(4)
!
!    precondition elaps start
!
      call start_elapsed_time(2)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD(MHD_files1, bc_sph_IO1,                   &
     &    MHD_prop1, sph_MHD_bc1, iphys_nod1, MHD_step1)
!
      call end_elapsed_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd_only
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd_only
!
      use m_spheric_parameter
!
      integer(kind = kint) :: iflag_finish
!
!
      call start_elapsed_time(3)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(MHD_step1%init_d, MHD_step1%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(MHD_step1%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(MHD_step1%time_d%i_time_step,              &
     &      MHD_files1, MHD_prop1, sph_MHD_bc1, iflag_finish,           &
     &      MHD_step1)
!
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      call end_elapsed_time(3)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      call copy_COMM_TIME_to_elaps(num_elapsed)
      call end_elapsed_time(1)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(sph1%sph_params,                       &
     &     sph1%sph_rtp, sph1%sph_rtm, sph1%sph_rlm, sph1%sph_rj)
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
