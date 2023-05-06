!>@file   analyzer_sph_pickup_circle.f90
!!        module analyzer_sph_pickup_circle
!!
!!@author H. Matsui
!!@date   Programmed in 2012
!!@n      modified in 2013
!
!>@brief Initialzation and evolution loop to pick up data on circle
!!
!!@verbatim
!!      subroutine initialize_sph_pick_circle
!!      subroutine evolution_sph_pick_circle
!!@endverbatim
!
      module analyzer_sph_pickup_circle
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_SPH_MHD_model_data
      use m_SPH_SGS_structure
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_field_on_circle
      use t_field_4_dynamobench
      use t_spheric_parameter
      use t_file_IO_parameter
      use t_SPH_mesh_field_data
      use t_step_parameter
      use t_mesh_SR
!
      use SPH_analyzer_sph_pick_circ
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: snap_ctl_name = 'control_snapshot'
!>        Control struture for MHD simulation
      type(mhd_simulation_control), save, private :: MHD_ctl1
!>        Additional structures for spherical SGS MHD dynamo
      type(add_sgs_sph_mhd_ctl), save, private :: add_SSMHD_ctl1
!
      type(sph_grid_maker_in_sim), save, private :: sph_maker1
      type(circle_fld_maker), save, private :: cdat1
      type(circle_fld_maker), save, private :: cdat0
      type(dynamobench_monitor), save, private :: bench0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_pick_circle
!
      use t_const_spherical_grid
      use sph_mhd_rst_IO_control
      use input_control_sph_SGS_MHD
      use init_sph_MHD_elapsed_label
      use parallel_load_data_4_sph
      use input_control_sph_MHD
      use nod_phys_send_recv
      use set_field_data_w_SGS
!
      integer(kind = kint) :: ierr = 0
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
      if (iflag_debug.eq.1) write(*,*) 's_load_control_sph_SGS_MHD'
      call input_control_sph_pick_circle(snap_ctl_name,                 &
     &    MHD_files1, MHD_ctl1, add_SSMHD_ctl1, MHD_step1,              &
     &    SPH_model1, SPH_WK1, SPH_SGS1, SPH_MHD1, cdat0, bench0)
      call dealloc_sph_SGS_MHD_viz_ctl(add_SSMHD_ctl1)
!
      call set_control_circle_def                                       &
     &   (MHD_ctl1%smonitor_ctl%meq_ctl, cdat1%circle)
      call set_SGS_field_ctl_by_viz                                     &
     &   (MHD_ctl1%model_ctl%fld_ctl%field_ctl, cdat1%d_circle, ierr)
!
      call dealloc_sph_sgs_mhd_ctl_data(MHD_ctl1, add_SSMHD_ctl1)
!
!   Load spherical harmonics data
!
      if (iflag_debug.eq.1) write(*,*) 'load_sph_mesh'
      call load_sph_mesh(MHD_files1%sph_file_param,                     &
     &                   SPH_MHD1%sph, SPH_MHD1%comms, SPH_MHD1%groups)
      if (iflag_debug.gt.0) write(*,*) 'sph_index_flags_and_params'
      call sph_index_flags_and_params                                   &
     &   (SPH_MHD1%groups, SPH_MHD1%sph, SPH_MHD1%comms)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!        Initialize spherical transform dynamo
!
!      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_real_send_recv(FEM_d1%geofem%mesh%nod_comm,             &
     &                         m_SR1%SR_sig, m_SR1%SR_r)
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_pick_circle'
      call SPH_init_sph_pick_circle(MHD_files1, FEM_d1%iphys,           &
     &    SPH_model1, MHD_step1, SPH_SGS1, SPH_MHD1, SPH_WK1,           &
     &    m_SR1%SR_sig, m_SR1%SR_r, cdat1)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_pick_circle
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_pick_circle
!
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
        if(output_IO_flag(MHD_step1%time_d%i_time_step,                 &
     &                    MHD_step1%rst_step) .eqv. .FALSE.) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_pick_circle'
        call SPH_analyze_pick_circle(MHD_step1%time_d%i_time_step,      &
     &      MHD_files1, SPH_model1, SPH_SGS1, SPH_MHD1, SPH_WK1,        &
     &      m_SR1%SR_sig, m_SR1%SR_r, cdat1)
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
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_pick_circle'
!      call SPH_finalize_pick_circle
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_pick_circle
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_pickup_circle
