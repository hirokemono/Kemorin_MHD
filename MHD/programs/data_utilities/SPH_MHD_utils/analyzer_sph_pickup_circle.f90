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
      use m_SPH_MHD_model_data
      use m_bc_data_list
      use m_SPH_SGS_structure
      use t_field_on_circle
      use t_spheric_parameter
      use t_file_IO_parameter
      use t_step_parameter
!
      use SPH_analyzer_sph_pick_circ
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &                      :: snap_ctl_name = 'control_snapshot'
!
      type(sph_grids), save, private :: sph_gen
!
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
      use t_ctl_data_SGS_MHD
      use t_const_spherical_grid
      use m_mesh_data
      use m_ctl_data_sph_SGS_MHD
      use m_node_phys_data
      use sph_mhd_rst_IO_control
      use set_control_sph_SGS_MHD
      use set_control_sph_data_MHD
      use init_sph_MHD_elapsed_label
      use parallel_load_data_4_sph
      use input_control_sph_MHD
!
      type(construct_spherical_grid), save :: gen_sph_c
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_SGS_MHD'
      call read_control_4_sph_SGS_MHD(snap_ctl_name, MHD_ctl1)
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_SGS_MHD'
      call set_control_4_SPH_SGS_MHD                                    &
     &   (MHD_ctl1%plt, MHD_ctl1%org_plt, MHD_ctl1%model_ctl,           &
     &    MHD_ctl1%smctl_ctl, MHD_ctl1%smonitor_ctl,                    &
     &    MHD_ctl1%nmtr_ctl, MHD_ctl1%psph_ctl, sph_gen, SPH_MHD1%fld,  &
     &    MHD_files1, bc_sph_IO1, SPH_SGS1%SGS_par, SPH_SGS1%dynamic,   &
     &    MHD_step1, SPH_model1%MHD_prop, MHD_BC1,                      &
     &    SPH_WK1%trns_WK%WK_sph, gen_sph_c, SPH_WK1%monitor)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
      call set_ctl_params_pick_circle                                   &
     &   (MHD_ctl1%model_ctl%fld_ctl%field_ctl,                         &
     &    MHD_ctl1%smonitor_ctl%meq_ctl, cdat1%circle, cdat1%d_circle)
!
!   Load spherical harmonics data
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh                                           &
     &   (SPH_MHD1%sph, SPH_MHD1%comms, SPH_MHD1%groups)
!
      call end_elapsed_time(4)
!
!        Initialize spherical transform dynamo
!
      call start_elapsed_time(2)
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_pick_circle'
      call SPH_init_sph_pick_circle(MHD_files1, bc_sph_IO1,             &
     &    femmesh1, iphys_nod1, SPH_model1, SPH_SGS1,                   &
     &    SPH_MHD1, SPH_WK1, cdat1)
      call calypso_MPI_barrier
!
      call end_elapsed_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_pick_circle
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_pick_circle
!
      integer(kind = kint) :: iflag
!
!*  -----------  set initial step data --------------
!*
      call start_elapsed_time(3)
      call s_initialize_time_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
!
        iflag = output_IO_flag(MHD_step1%time_d%i_time_step,            &
     &                         MHD_step1%rst_step)
        if(iflag .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_pick_circle'
        call SPH_analyze_pick_circle(MHD_step1%time_d%i_time_step,      &
     &      MHD_files1, SPH_model1, SPH_SGS1, SPH_MHD1, SPH_WK1, cdat1)
!*
!*  -----------  exit loop --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      call end_elapsed_time(3)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_pick_circle'
!      call SPH_finalize_pick_circle
!
      call copy_COMM_TIME_to_elaps(num_elapsed)
      call end_elapsed_time(1)
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
