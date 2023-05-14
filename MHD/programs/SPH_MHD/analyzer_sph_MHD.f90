!>@file   analyzer_sph_MHD.f90
!!@brief  module analyzer_sph_MHD
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mhd(control_file_name, SSMHDs)
!!      subroutine evolution_sph_mhd(SSMHDs)
!!        character(len=kchara), intent(in) :: control_file_name
!!        type(sph_SGS_MHD), intent(inout) :: SSMHDs
!!@endverbatim
!
      module analyzer_sph_MHD
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_SPH_MHD_model_data
      use t_MHD_IO_data
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_SPH_mesh_field_data
      use t_SPH_SGS_structure
      use t_SPH_MHD_zonal_mean_viz
      use t_VIZ_mesh_field
      use t_visualizer
      use t_sph_trans_arrays_MHD
      use t_mesh_SR
!
      use SPH_analyzer_SGS_MHD
      use FEM_analyzer_sph_SGS_MHD
      use init_sph_MHD_elapsed_label
!
      implicit none
!
!>      Structure of the all data of program
      type sph_SGS_MHD
!>        Structure of time and step informations
        type(MHD_step_param) :: MHD_step
!>        Structure of spectr grid and data
        type(SPH_mesh_field_data) :: SPH_MHD
!>        Structures of SGS model in Spherical shell dynamo
        type(SPH_SGS_structure) :: SPH_SGS
!
!>        Structure of FEM data for visualization
        type(VIZ_mesh_field) :: VIZ_FEM
!>        Structures of visualizations
        type(visualize_modules) :: VIZs
!
!>        Structure of file name and format for MHD
        type(MHD_file_IO_params) :: MHD_files
!>        Structure for data file IO
        type(MHD_IO_data) :: MHD_IO
      end type sph_SGS_MHD
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd(control_file_name, SSMHDs)
!
      use m_elapsed_labels_4_REPART
      use input_control_sph_SGS_MHD
      use FEM_to_VIZ_bridge
!
      character(len=kchara), intent(in) :: control_file_name
      type(sph_SGS_MHD), intent(inout) :: SSMHDs
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: MHD_ctl1
!>        Additional structures for spherical SGS MHD dynamo
      type(add_sgs_sph_mhd_ctl), save :: add_SSMHD_ctl1
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      SSMHDs%MHD_step%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_4_repartition
!
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_SGS_dynamo'
      call input_control_SPH_SGS_dynamo(control_file_name,              &
     &    SSMHDs%MHD_files, MHD_ctl1, add_SSMHD_ctl1,                   &
     &    SSMHDs%MHD_step, SPH_model1, SPH_WK1,                         &
     &    SSMHDs%SPH_SGS, SSMHDs%SPH_MHD, FEM_d1)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!    IO elapsed end
!    precondition elaps start
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_SGS_MHD'
      call FEM_initialize_sph_SGS_MHD                                   &
     &   (SSMHDs%MHD_files, SSMHDs%MHD_step, SSMHDs%SPH_SGS%iphys_LES,  &
     &    SSMHDs%MHD_IO, FEM_d1, SPH_WK1%nod_mntr, m_SR1)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_SGS_MHD'
      call SPH_initialize_SGS_MHD                                       &
     &   (SSMHDs%MHD_files, FEM_d1, SSMHDs%MHD_step,                    &
     &    SSMHDs%MHD_IO%rst_IO, SPH_model1, SSMHDs%SPH_SGS,             &
     &    SSMHDs%SPH_MHD, SPH_WK1, m_SR1%SR_sig, m_SR1%SR_r)
!
!  -------------------------------------------
!  ----   Mesh setting for visualization -----
!  -------------------------------------------
      if(iflag_debug .gt. 0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge                                       &
      &  (SSMHDs%MHD_step%viz_step, FEM_d1%geofem,                      &
     &    SSMHDs%VIZ_FEM, m_SR1)
!
!        Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(SSMHDs%MHD_step%viz_step, FEM_d1%geofem,      &
     &    FEM_d1%field, SSMHDs%VIZ_FEM, add_SSMHD_ctl1%viz_ctls,        &
     &    SSMHDs%VIZs, m_SR1)
      call init_zonal_mean_sections                                     &
     &   (SSMHDs%MHD_step%viz_step, FEM_d1%geofem,                      &
     &    SSMHDs%VIZ_FEM%edge_comm, FEM_d1%field,                       &
     &    add_SSMHD_ctl1%zm_ctls, zmeans1, m_SR1%SR_sig, m_SR1%SR_il)
      call dealloc_sph_SGS_MHD_viz_ctl(add_SSMHD_ctl1)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd(SSMHDs)
!
      use FEM_analyzer_sph_SGS_MHD
      use SGS_MHD_zonal_mean_viz
      use output_viz_file_control
      use t_sph_trans_arrays_SGS_MHD
      use FEM_to_VIZ_bridge
!
      type(sph_SGS_MHD), intent(inout) :: SSMHDs
!
      integer(kind = kint) :: iflag_finish
!
!     ---------------------
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(SSMHDs%MHD_step%init_d,                  &
     &                         SSMHDs%MHD_step%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(SSMHDs%MHD_step%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(SSMHDs%MHD_step%time_d%i_time_step,     &
     &                          SSMHDs%MHD_step)) then
          call alloc_sph_trans_area_snap                                &
     &       (SSMHDs%SPH_MHD%sph, SPH_WK1%trns_WK)
          call alloc_SGS_sph_trns_area_snap                             &
     &       (SSMHDs%SPH_MHD%sph, SSMHDs%SPH_SGS%trns_WK_LES)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_SGS_MHD'
        call SPH_analyze_SGS_MHD(SSMHDs%MHD_files, iflag_finish,        &
     &      SPH_model1, SSMHDs%MHD_step, SSMHDs%MHD_IO%rst_IO,          &
     &      SSMHDs%SPH_SGS, SSMHDs%SPH_MHD, SPH_WK1,                    &
     &      m_SR1%SR_sig, m_SR1%SR_r)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(SSMHDs%MHD_step%time_d%i_time_step,     &
     &                          SSMHDs%MHD_step)) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
          call SPH_to_FEM_bridge_SGS_MHD(SSMHDs%SPH_SGS%SGS_par,        &
     &        SSMHDs%SPH_MHD%sph, SPH_WK1%trns_WK,                      &
     &        SSMHDs%SPH_SGS%trns_WK_LES, FEM_d1%geofem, FEM_d1%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_SGS_MHD'
        call FEM_analyze_sph_SGS_MHD(SSMHDs%MHD_files, SSMHDs%MHD_step, &
     &                               SSMHDs%MHD_IO, FEM_d1, m_SR1)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(SSMHDs%MHD_step%time_d%i_time_step,    &
     &                           SSMHDs%MHD_step%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all', my_rank
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(SSMHDs%MHD_step%time_d%i_time_step,   &
     &                          SSMHDs%MHD_step%viz_step)
          call visualize_all                                            &
     &       (SSMHDs%MHD_step%viz_step, SSMHDs%MHD_step%time_d,         &
     &        FEM_d1%geofem, FEM_d1%field, SSMHDs%VIZ_FEM,              &
     &        SSMHDs%VIZs, m_SR1)
!*
!*  ----------- Zonal means --------------
!*
          if(SSMHDs%MHD_step%viz_step%istep_psf .ge. 0) then
            call SGS_MHD_zmean_sections(SSMHDs%MHD_step%viz_step,       &
     &          SSMHDs%MHD_step%time_d, SSMHDs%SPH_MHD%sph,             &
     &          FEM_d1%geofem, SPH_WK1%trns_WK, SSMHDs%SPH_SGS,         &
     &          FEM_d1%field, zmeans1, m_SR1)
          end if
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!
        if(lead_field_data_flag(SSMHDs%MHD_step%time_d%i_time_step,     &
     &                          SSMHDs%MHD_step)) then
          call dealloc_sph_trans_area_snap(SPH_WK1%trns_WK)
          call dealloc_SGS_sph_trns_area_snap                           &
     &       (SSMHDs%SPH_SGS%trns_WK_LES)
        end if
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'visualize_fin'
      call visualize_fin(SSMHDs%MHD_step%viz_step,                      &
     &                   SSMHDs%MHD_step%time_d, SSMHDs%VIZs)
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize_sph_SGS_MHD'
      call FEM_finalize_sph_SGS_MHD(SSMHDs%MHD_files, SSMHDs%MHD_step,  &
     &                              SSMHDs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(SSMHDs%SPH_MHD%sph)
      if (iflag_debug.eq.1) write(*,*) 'output_elapsed_times '
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD
