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
!!      subroutine initialize_sph_forward_trans
!!      subroutine evolution_sph_forward_trans
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
      use m_elapsed_labels_SEND_RECV
      use m_SPH_MHD_model_data
      use m_MHD_step_parameter
      use m_SPH_SGS_structure
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_control_data_dynamo_vizs
      use t_SPH_mesh_field_data
      use t_step_parameter
      use t_VIZ_mesh_field
      use t_mesh_SR
      use t_field_on_circle
      use t_field_4_dynamobench
!
      use FEM_analyzer_sph_SGS_MHD
      use SPH_analyzer_SGS_snap
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: snap_ctl_name = 'control_snapshot'
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save, private :: MHD_ctl1
!>        Additional structures for spherical SGS MHD dynamo
      type(add_sgs_sph_mhd_ctl), save, private :: add_SSMHD_ctl1
!
!>      Structure of field on mid-depth and equator
      type(circle_fld_maker), save, private :: cdat4
!>      Structure of benchmark result data
      type(dynamobench_monitor), save, private :: bench4
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_forward_trans
!
      use init_sph_MHD_elapsed_label
      use input_control_sph_SGS_MHD
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_SGS_dynamo'
      call input_control_SPH_SGS_dynamo                                 &
     &   (snap_ctl_name, MHD_files1, MHD_ctl1, add_SSMHD_ctl1,          &
     &    MHD_step1, SPH_model1, SPH_WK1, SPH_SGS1, SPH_MHD1, FEM_d1,   &
     &    cdat4, bench4)
      call dealloc_sph_SGS_MHD_viz_ctl(add_SSMHD_ctl1)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!     --------------------- 
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_SGS_MHD'
      call FEM_initialize_sph_SGS_MHD(MHD_files1, MHD_step1,            &
     &    SPH_SGS1%iphys_LES, MHD_IO1, FEM_d1, m_SR1)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_SGS_snap'
      call SPH_init_SGS_snap(MHD_files1, FEM_d1%iphys, SPH_model1,      &
     &    MHD_step1, SPH_SGS1, SPH_MHD1, SPH_WK1,                       &
     &    m_SR1%SR_sig, m_SR1%SR_r)
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
      call set_from_initial_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
!
        if(output_IO_flag(MHD_step1%time_d%i_time_step,                 &
     &                    MHD_step1%rst_step) .eqv. .FALSE.) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_SGS_snap'
        call SPH_analyze_SGS_snap(MHD_step1%time_d%i_time_step,         &
     &      MHD_files1, SPH_model1, MHD_step1, SPH_SGS1, SPH_MHD1,      &
     &      SPH_WK1, m_SR1%SR_sig, m_SR1%SR_r)
!*
!*  -----------  exit loop --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, MHD_IO1)
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
