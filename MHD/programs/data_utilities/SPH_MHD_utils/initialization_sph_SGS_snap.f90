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
      use t_particle_trace
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_control_data_dynamo_vizs
!
      implicit none
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: MHD_ctl1
!
!>        Control structures for tracer modules
      type(tracers_control), save, private :: tracer_ctls1
!>        Control structures for visualization modules
      type(visualization_controls), save, private ::  viz_ctls1
!>        Control structures for zonal mean and trancated magnetic field
      type(sph_dynamo_viz_controls), save, private :: zm_ctls1
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
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_SGS_dynamo'
      call input_control_SPH_SGS_dynamo(control_file_name,              &
     &   SSNAPs%MHD_files, MHD_ctl1, tracer_ctls1, viz_ctls1, zm_ctls1, &
     &   SSNAPs%MHD_step, SSNAPs%SPH_model, SSNAPs%SPH_WK,              &
     &   SVIZs%SPH_SGS, SSNAPs%SPH_MHD, SVIZs%FEM_DAT)
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
!  -----   Initialize tracer
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+13)
      call TRACER_initialize                                            &
     &   (SSNAPs%MHD_step%init_d,  SSNAPs%MHD_step%finish_d,            &
     &    SSNAPs%MHD_step%rst_step, SVIZs%FEM_DAT%geofem,               &
     &    SVIZs%VIZ_FEM%para_surf, SVIZs%FEM_DAT%field,                 &
     &    tracer_ctls1%tracer_controls, SVIZs%tracers)
      call dealloc_tracer_controls(tracer_ctls1)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+13)
!
!  -----   Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(SSNAPs%MHD_step%viz_step,                     &
     &    SVIZs%FEM_DAT%geofem, SVIZs%FEM_DAT%field, SVIZs%tracers,     &
     &    SVIZs%VIZ_FEM, viz_ctls1, SVIZs%VIZs, SSNAPs%m_SR)
      call dealloc_viz_controls(viz_ctls1)

      call init_zonal_mean_vizs(SSNAPs%MHD_step%viz_step,               &
     &    SVIZs%FEM_DAT%geofem, SVIZs%VIZ_FEM%edge_comm,                &
     &    SVIZs%FEM_DAT%field, zm_ctls1, SVIZs%zmeans, SSNAPs%m_SR)
      call dealloc_dynamo_viz_control(zm_ctls1)
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
