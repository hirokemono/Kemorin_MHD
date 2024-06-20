!>@file   analyzer_sph_forward_trans.f90
!!@brief  module analyzer_sph_forward_trans
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!        without visualization routines
!!
!!@verbatim
!!      subroutine initialize_sph_forward_trans(control_file_name)
!!      subroutine evolution_sph_forward_trans
!!        character(len=kchara), intent(in) :: control_file_name
!!@endverbatim
!
      module analyzer_sph_forward_trans
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_4_VIZ
      use m_elapsed_labels_SEND_RECV
      use t_spherical_MHD
      use t_sph_SGS_MHD
      use t_ctl_data_MHD
      use t_control_data_tracers
      use t_control_data_dynamo_vizs
!
      use FEM_analyzer_sph_SGS_MHD
      use SPH_analyzer_SGS_snap
!
      implicit none
!
!>      Structure of the all data of program
      type(spherical_MHD), save, private :: SNAPs
!>      Structure for visualization in spherical MHD
      type(sph_SGS_MHD), save, private :: SVIZ_m
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save, private  :: MHD_ctl_t
!
      type(tracers_control), save, private :: tracer_ctls_t
!>        Control structures for visualization modules
      type(visualization_controls), save, private ::  viz_ctls_t
!>        Control structures for zonal mean and trancated magnetic field
      type(sph_dynamo_viz_controls), save, private :: zm_ctls_t
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_forward_trans(control_file_name)
!
      use init_sph_MHD_elapsed_label
      use input_control_sph_SGS_MHD
!
      character(len=kchara), intent(in) :: control_file_name
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call set_elpsed_label_4_VIZ(elps_VIZ1, elps1)
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_SGS_dynamo'
      call input_control_SPH_SGS_dynamo                                 &
     &   (control_file_name, SNAPs%MHD_files, MHD_ctl_t, tracer_ctls_t, &
     &    viz_ctls_t, zm_ctls_t, SNAPs%MHD_step, SNAPs%SPH_model,       &
     &    SNAPs%SPH_WK, SVIZ_m%SPH_SGS, SNAPs%SPH_MHD, SVIZ_m%FEM_DAT)
      call dealloc_tracer_controls(tracer_ctls_t)
      call dealloc_viz_controls(viz_ctls_t)
      call dealloc_dynamo_viz_control(zm_ctls_t)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!     --------------------- 
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_SGS_MHD'
      call FEM_initialize_sph_SGS_MHD(SNAPs%MHD_files, SNAPs%MHD_step,  &
     &    SVIZ_m%SPH_SGS%iphys_LES, SNAPs%MHD_IO, SVIZ_m%FEM_DAT,       &
     &    SNAPs%m_SR)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_SGS_snap'
      call SPH_init_SGS_snap(SNAPs%MHD_files, SVIZ_m%FEM_DAT,           &
     &    SNAPs%SPH_model, SNAPs%MHD_step, SVIZ_m%SPH_SGS,              &
     &    SNAPs%SPH_MHD, SNAPs%SPH_WK, SNAPs%m_SR)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_forward_trans
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_forward_trans
!
      use FEM_analyzer_sph_MHD
      use set_time_step_params
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call set_from_initial_step(SNAPs%MHD_step%init_d,                 &
     &                           SNAPs%MHD_step%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(SNAPs%MHD_step%time_d)
!
        if(output_IO_flag(SNAPs%MHD_step%time_d%i_time_step,            &
     &                   SNAPs%MHD_step%rst_step) .eqv. .FALSE.) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_SGS_snap'
        call SPH_analyze_SGS_snap                                       &
     &     (SNAPs%MHD_files, SNAPs%SPH_model, SNAPs%MHD_step,           &
     &      SVIZ_m%SPH_SGS, SNAPs%SPH_MHD, SNAPs%SPH_WK, SNAPs%m_SR)
!*
!*  -----------  exit loop --------------
!*
        if(SNAPs%MHD_step%time_d%i_time_step                            &
     &        .ge. SNAPs%MHD_step%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(SNAPs%MHD_files, SNAPs%MHD_step, SNAPs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_SGS_snap'
!      call SPH_finalize_SGS_snap
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_forward_trans
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_forward_trans
