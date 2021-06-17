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
      use t_ctl_data_SGS_MHD
      use t_field_on_circle
      use t_spheric_parameter
      use t_file_IO_parameter
      use t_SPH_mesh_field_data
      use t_step_parameter
      use m_solver_SR
!
      use SPH_analyzer_sph_pick_circ
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!>      Control struture for MHD simulation
      type(sph_sgs_mhd_control), save :: MHD_ctl1
      private :: snap_ctl_name, MHD_ctl1
!
      type(sph_grid_maker_in_sim), save, private :: sph_maker1
      type(circle_fld_maker), save, private :: cdat1
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
      use set_control_sph_SGS_MHD
      use init_sph_MHD_elapsed_label
      use parallel_load_data_4_sph
      use input_control_sph_MHD
      use nod_phys_send_recv
!
      type(phys_data), save :: nod_fld_c
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_SGS_MHD'
      call read_control_4_sph_SGS_MHD(snap_ctl_name, MHD_ctl1)
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_SGS_MHD'
      call set_control_4_SPH_SGS_MHD                                    &
     &   (MHD_ctl1%plt, MHD_ctl1%org_plt, MHD_ctl1%model_ctl,           &
     &    MHD_ctl1%smctl_ctl, MHD_ctl1%nmtr_ctl, MHD_ctl1%psph_ctl,     &
     &    MHD_files1, SPH_model1%bc_IO, SPH_SGS1%SGS_par,               &
     &    SPH_SGS1%dynamic, MHD_step1, SPH_model1%MHD_prop,             &
     &    SPH_model1%MHD_BC, SPH_WK1%trans_p, SPH_WK1%trns_WK,          &
     &    sph_maker1)
      call set_control_SGS_SPH_MHD_field(MHD_ctl1%model_ctl,            &
     &    MHD_ctl1%psph_ctl, MHD_ctl1%smonitor_ctl, MHD_ctl1%zm_ctls,   &
     &    SPH_SGS1%SGS_par, SPH_model1%MHD_prop, SPH_MHD1%sph,          &
     &    SPH_MHD1%fld, nod_fld_c, SPH_WK1%monitor)
!
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
      call set_ctl_params_pick_circle                                   &
     &   (MHD_ctl1%model_ctl%fld_ctl%field_ctl,                         &
     &    MHD_ctl1%smonitor_ctl%meq_ctl, cdat1%circle, cdat1%d_circle)
!
      call dealloc_sph_sgs_mhd_ctl_data(MHD_ctl1)
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
      if (iflag_debug.gt.0 ) write(*,*) 'alloc_iccgN_vector'
      call alloc_iccgN_vector                                           &
     &   (isix, SPH_MHD1%sph%sph_rtp%nnod_rtp, v_sol1)
!      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(FEM_d1%geofem%mesh,                       &
     &                        SR_sig1, SR_r1, SR_i1, SR_il1)
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_pick_circle'
      call SPH_init_sph_pick_circle(MHD_files1, FEM_d1%iphys,           &
     &    SPH_model1, SPH_SGS1, SPH_MHD1, SPH_WK1,                      &
     &    SR_sig1, SR_r1, cdat1)
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
     &      SR_sig1, SR_r1, cdat1)
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
