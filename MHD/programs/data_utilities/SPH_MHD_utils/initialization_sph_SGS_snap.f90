!>@file   initialization_sph_SGS_snap.f90
!!@brief  module initialization_sph_SGS_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine initialize_sph_SGS_snap(control_file_name,           &
!!     &                                   SSNAPs, SVIZs)
!!@endverbatim
!
      module initialization_sph_SGS_snap
!
      use m_precision
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_machine_parameter
      use t_spherical_MHD
      use t_sph_SGS_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_SGS_snap(control_file_name,             &
     &                                   SSNAPs, SVIZs)
!
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_visualizer
      use t_SPH_MHD_zonal_mean_viz
      use SPH_analyzer_SGS_snap
      use FEM_analyzer_sph_SGS_MHD
      use FEM_to_VIZ_bridge
      use input_control_sph_SGS_MHD
      use init_sph_MHD_elapsed_label
!
      character(len=kchara), intent(in) :: control_file_name
      type(spherical_MHD), intent(inout) :: SSNAPs
      type(sph_SGS_MHD), intent(inout) :: SVIZs
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: MHD_ctl1
!>        Additional structures for spherical SGS MHD dynamo
      type(add_sgs_sph_mhd_ctl), save :: add_SSMHD_ctl1
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_SGS_dynamo'
      call input_control_SPH_SGS_dynamo(control_file_name,              &
     &    SSNAPs%MHD_files, MHD_ctl1, add_SSMHD_ctl1, SSNAPs%MHD_step,  &
     &    SSNAPs%SPH_model, SSNAPs%SPH_WK, SVIZs%SPH_SGS,               &
     &    SSNAPs%SPH_MHD, SVIZs%FEM_DAT)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!     --------------------- 
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_SGS_MHD'
      call FEM_initialize_sph_SGS_MHD(SSNAPs%MHD_files,                 &
     &    SSNAPs%MHD_step, SVIZs%SPH_SGS%iphys_LES, SSNAPs%MHD_IO,      &
     &    SVIZs%FEM_DAT, SSNAPs%m_SR)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_SGS_snap'
      call SPH_init_SGS_snap(SSNAPs%MHD_files, SVIZs%FEM_DAT,           &
     &    SSNAPs%SPH_model, SSNAPs%MHD_step, SVIZs%SPH_SGS,             &
     &    SSNAPs%SPH_MHD, SSNAPs%SPH_WK, SSNAPs%m_SR)
!
!  -------------------------------------------
!  ----   Mesh setting for visualization -----
!  -------------------------------------------
      if(iflag_debug .gt. 0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge(SSNAPs%MHD_step%viz_step,             &
     &    SVIZs%FEM_DAT%geofem, SVIZs%VIZ_FEM, SSNAPs%m_SR)
!
      call FLINE_initialize(SSNAPs%MHD_step%viz_step%FLINE_t%increment, &
     &    SVIZs%FEM_DAT%geofem, SVIZs%FEM_DAT%field,                    &
     &    add_SSMHD_ctl1%tracer_ctls%tracer_controls, SVIZs%tracers)
!
!        Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(SSNAPs%MHD_step%viz_step,                     &
     &    SVIZs%FEM_DAT%geofem, SVIZs%FEM_DAT%field, SVIZs%VIZ_FEM,     &
     &    add_SSMHD_ctl1%viz_ctls, SVIZs%VIZs, SSNAPs%m_SR)
      call init_zonal_mean_vizs(SSNAPs%MHD_step%viz_step,               &
     &    SVIZs%FEM_DAT%geofem, SVIZs%VIZ_FEM%edge_comm,                &
     &    SVIZs%FEM_DAT%field, add_SSMHD_ctl1%zm_ctls,                  &
     &    SVIZs%zmeans, SSNAPs%m_SR)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_SGS_snap
!
! ----------------------------------------------------------------------
!
      end module initialization_sph_SGS_snap
