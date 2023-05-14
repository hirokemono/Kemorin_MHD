!>@file   analyzer_sph_MHD_w_vizs.f90
!!@brief  module analyzer_sph_MHD_w_vizs
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mhd_w_vizs(control_file_name, SMHDVs)
!!      subroutine evolution_sph_mhd_w_vizs(SMHDVs)
!!        character(len=kchara), intent(in) :: control_file_name
!!        type(sph_MHD_w_vizs), intent(inout) :: SMHDVs
!!@endverbatim
!
      module analyzer_sph_MHD_w_vizs
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use t_MHD_step_parameter
      use t_SPH_MHD_model_data
      use t_FEM_mesh_field_data
      use t_SPH_mesh_field_data
      use t_work_SPH_MHD
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_vizs
      use t_three_visualizers
      use t_SPH_MHD_zonal_mean_viz
      use t_VIZ_mesh_field
      use t_sph_trans_arrays_MHD
      use t_MHD_file_parameter
      use t_MHD_IO_data
      use t_mesh_SR
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_MHD
!
      implicit none
!
!
      type sph_MHD_w_vizs
!>        Parameters for spectr dynamo model
        type(SPH_MHD_model_data) :: SPH_model
!
!>        Structure of time and step informations
        type(MHD_step_param) :: MHD_step
!>        Structure of spectr grid and data
        type(SPH_mesh_field_data) :: SPH_MHD
!
!>        Structure of FEM mesh and field structures
        type(FEM_mesh_field_data) :: FEM_DAT
!>        Structure of geometry informations for visualization
        type(VIZ_mesh_field) :: VIZ_DAT
!>        Structure of sectioning and isosurfaceing modules
        type(three_visualize_modules) :: VIZ3s
!>        Structures of zonal mean controls
        type(sph_zonal_mean_sectioning) :: zmeans
!
!>        Structures of work area for spherical shell dynamo
        type(work_SPH_MHD) :: SPH_WK
!>        Structure of work area for mesh communications
        type(mesh_SR) :: m_SR
!
!>        Structure of file name and format for MHD
        type(MHD_file_IO_params) :: MHD_files
!>        Structure for data file IO
        type(MHD_IO_data) :: MHD_IO
      end type sph_MHD_w_vizs
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd_w_vizs(control_file_name, SMHDVs)
!
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD_vizs
      use FEM_to_VIZ_bridge
!
      character(len=kchara), intent(in) :: control_file_name
      type(sph_MHD_w_vizs), intent(inout) :: SMHDVs
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: MHD_ctl1
!>      Additional structures for spherical MHD dynamo with viz module
      type(add_vizs_sph_mhd_ctl), save :: add_VMHD_ctl1
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      SMHDVs%MHD_step%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 's_input_control_SPH_MHD_vizs'
      call s_input_control_SPH_MHD_vizs(control_file_name,              &
     &    SMHDVs%MHD_files, MHD_ctl1, add_VMHD_ctl1,                    &
     &    SMHDVs%MHD_step, SMHDVs%SPH_model, SMHDVs%SPH_WK,             &
     &    SMHDVs%SPH_MHD, SMHDVs%FEM_DAT)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD                                       &
     &   (SMHDVs%MHD_files, SMHDVs%MHD_step, SMHDVs%FEM_DAT,            &
     &    SMHDVs%MHD_IO, SMHDVs%SPH_WK%nod_mntr, SMHDVs%m_SR)
      call init_FEM_to_VIZ_bridge(SMHDVs%MHD_step%viz_step,             &
     &    SMHDVs%FEM_DAT%geofem, SMHDVs%VIZ_DAT, SMHDVs%m_SR)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD                                           &
     &   (SMHDVs%MHD_files, SMHDVs%SPH_model, SMHDVs%FEM_DAT,           &
     &    SMHDVs%MHD_step, SMHDVs%MHD_IO%rst_IO, SMHDVs%SPH_MHD,        &
     &    SMHDVs%SPH_WK, SMHDVs%m_SR)
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0) write(*,*) 'init_three_visualize'
      call init_three_visualize(SMHDVs%MHD_step%viz_step,               &
     &    SMHDVs%FEM_DAT%geofem, SMHDVs%FEM_DAT%field, SMHDVs%VIZ_DAT,  &
     &    add_VMHD_ctl1%viz3_ctls, SMHDVs%VIZ3s, SMHDVs%m_SR)
!
      call init_zonal_mean_sections                                     &
     &   (SMHDVs%MHD_step%viz_step, SMHDVs%FEM_DAT%geofem,              &
     &    SMHDVs%VIZ_DAT%edge_comm, SMHDVs%FEM_DAT%field,               &
     &    add_VMHD_ctl1%zm_ctls, SMHDVs%zmeans, SMHDVs%m_SR)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd_w_vizs
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd_w_vizs(SMHDVs)
!
      use init_sph_MHD_elapsed_label
      use output_viz_file_control
!
      type(sph_MHD_w_vizs), intent(inout) :: SMHDVs
      integer(kind = kint) :: iflag_finish
!
!     ---------------------
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(SMHDVs%MHD_step%init_d,                  &
     &                         SMHDVs%MHD_step%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(SMHDVs%MHD_step%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(SMHDVs%MHD_step%time_d%i_time_step,     &
     &                          SMHDVs%MHD_step)) then
          call alloc_sph_trans_area_snap(SMHDVs%SPH_MHD%sph,            &
     &                                   SMHDVs%SPH_WK%trns_WK)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(SMHDVs%MHD_files, iflag_finish,            &
     &      SMHDVs%SPH_model, SMHDVs%MHD_step, SMHDVs%MHD_IO%rst_IO,    &
     &      SMHDVs%SPH_MHD, SMHDVs%SPH_WK, SMHDVs%m_SR)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(SMHDVs%MHD_step%time_d%i_time_step,     &
     &                          SMHDVs%MHD_step)) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD                                    &
     &       (SMHDVs%SPH_MHD%sph, SMHDVs%SPH_WK%trns_WK,                &
     &        SMHDVs%FEM_DAT%geofem, SMHDVs%FEM_DAT%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(SMHDVs%MHD_files, SMHDVs%FEM_DAT,      &
     &      SMHDVs%MHD_step, SMHDVs%MHD_IO, SMHDVs%m_SR)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(SMHDVs%MHD_step%time_d%i_time_step,    &
     &                           SMHDVs%MHD_step%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface', my_rank
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(SMHDVs%MHD_step%time_d%i_time_step,   &
     &                            SMHDVs%MHD_step%viz_step)
          call visualize_three                                          &
     &       (SMHDVs%MHD_step%viz_step, SMHDVs%MHD_step%time_d,         &
     &        SMHDVs%FEM_DAT%geofem, SMHDVs%FEM_DAT%field,              &
     &        SMHDVs%VIZ_DAT, SMHDVs%VIZ3s, SMHDVs%m_SR)
!*
!*  ----------- Zonal means --------------
!*
          if(SMHDVs%MHD_step%viz_step%istep_psf .ge. 0) then
            call SPH_MHD_zmean_sections                                 &
     &         (SMHDVs%MHD_step%viz_step, SMHDVs%MHD_step%time_d,       &
     &          SMHDVs%SPH_MHD%sph, SMHDVs%FEM_DAT%geofem,              &
     &          SMHDVs%SPH_WK%trns_WK, SMHDVs%FEM_DAT%field,            &
     &          SMHDVs%zmeans, SMHDVs%m_SR)
          end if
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!*
        if(lead_field_data_flag(SMHDVs%MHD_step%time_d%i_time_step,     &
     &                          SMHDVs%MHD_step)) then
          call dealloc_sph_trans_area_snap(SMHDVs%SPH_WK%trns_WK)
        end if
!
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(SMHDVs%MHD_files, SMHDVs%MHD_step,              &
     &                  SMHDVs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(SMHDVs%SPH_MHD%sph)
      if (iflag_debug.eq.1) write(*,*) 'output_elapsed_times '
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd_w_vizs
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD_w_vizs
