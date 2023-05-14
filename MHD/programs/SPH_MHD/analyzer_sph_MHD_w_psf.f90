!>@file   analyzer_sph_MHD_w_psf.f90
!!@brief  module analyzer_sph_MHD_w_psf
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mhd_w_psf(control_file_name, MHDSs)
!!      subroutine evolution_sph_mhd_w_psf(MHDSs)
!!        character(len=kchara), intent(in) :: control_file_name
!!        type(sph_MHD_w_psf), intent(inout) :: MHDSs
!!@endverbatim
!
      module analyzer_sph_MHD_w_psf
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_SPH_MHD_model_data
      use t_MHD_step_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_MHD_file_parameter
      use t_MHD_IO_data
      use t_FEM_mesh_field_data
      use t_work_SPH_MHD
      use t_mesh_SR
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_psf
      use t_viz_sections
      use t_SPH_MHD_zonal_mean_viz
      use t_sph_trans_arrays_MHD
      use t_comm_table
      use t_mesh_SR
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_MHD
      use init_sph_MHD_elapsed_label
!
      implicit none
!
      type sph_MHD_w_psf
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
!>        Structure of edge communication table
        type(communication_table) :: edge_comm
!>        Structure of sectioning and isosurfaceing modules
        type(surfacing_modules) :: PSFs
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
      end type sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd_w_psf(control_file_name, MHDSs)
!
      use input_control_sph_MHD
      use FEM_to_PSF_bridge
!
      character(len=kchara), intent(in) :: control_file_name
      type(sph_MHD_w_psf), intent(inout) :: MHDSs
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: DNS_MHD_ctl1
!>      Additional structures for spherical MHD dynamo with viz module
      type(add_psf_sph_mhd_ctl), save :: add_SMHD_ctl1
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      MHDSs%MHD_step%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_MHD_psf'
      call input_control_SPH_MHD_psf(control_file_name,                 &
     &    MHDSs%MHD_files, DNS_MHD_ctl1, add_SMHD_ctl1, MHDSs%MHD_step, &
     &    MHDSs%SPH_model, MHDSs%SPH_WK, MHDSs%SPH_MHD, MHDSs%FEM_DAT)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD                                       &
     &   (MHDSs%MHD_files, MHDSs%MHD_step, MHDSs%FEM_DAT, MHDSs%MHD_IO, &
     &    MHDSs%SPH_WK%nod_mntr, MHDSs%m_SR)
      call init_FEM_to_PSF_bridge(MHDSs%MHD_step%viz_step,              &
     &    MHDSs%FEM_DAT%geofem, MHDSs%edge_comm, MHDSs%m_SR)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD                                           &
     &   (MHDSs%MHD_files, MHDSs%SPH_model, MHDSs%FEM_DAT,              &
     &    MHDSs%MHD_step, MHDSs%MHD_IO%rst_IO, MHDSs%SPH_MHD,           &
     &    MHDSs%SPH_WK, MHDSs%m_SR)
!
!        Initialize visualization
!
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize_surface'
      call init_visualize_surface(MHDSs%MHD_step%viz_step,              &
     &    MHDSs%FEM_DAT%geofem, MHDSs%edge_comm, MHDSs%FEM_DAT%field,   &
     &    add_SMHD_ctl1%surfacing_ctls, MHDSs%PSFs, MHDSs%m_SR)
!
      call init_zonal_mean_sections(MHDSs%MHD_step%viz_step,            &
     &    MHDSs%FEM_DAT%geofem, MHDSs%edge_comm, MHDSs%FEM_DAT%field,   &
     &    add_SMHD_ctl1%zm_ctls, MHDSs%zmeans, MHDSs%m_SR)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd_w_psf(MHDSs)
!
      use output_viz_file_control
!
      type(sph_MHD_w_psf), intent(inout) :: MHDSs
      integer(kind = kint) :: iflag_finish
!
!     ---------------------
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(MHDSs%MHD_step%init_d,                   &
     &                         MHDSs%MHD_step%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(MHDSs%MHD_step%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(MHDSs%MHD_step%time_d%i_time_step,      &
     &                          MHDSs%MHD_step)) then
          call alloc_sph_trans_area_snap(MHDSs%SPH_MHD%sph,             &
     &                                   MHDSs%SPH_WK%trns_WK)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(MHDSs%MHD_files, iflag_finish,             &
     &     MHDSs%SPH_model, MHDSs%MHD_step, MHDSs%MHD_IO%rst_IO,        &
     &     MHDSs%SPH_MHD, MHDSs%SPH_WK, MHDSs%m_SR)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(MHDSs%MHD_step%time_d%i_time_step,      &
     &                          MHDSs%MHD_step)) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD                                    &
     &       (MHDSs%SPH_MHD%sph, MHDSs%SPH_WK%trns_WK,                  &
     &        MHDSs%FEM_DAT%geofem, MHDSs%FEM_DAT%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHDSs%MHD_files, MHDSs%FEM_DAT,        &
     &      MHDSs%MHD_step, MHDSs%MHD_IO, MHDSs%m_SR)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(MHDSs%MHD_step%time_d%i_time_step,     &
     &                           MHDSs%MHD_step%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface', my_rank
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(MHDSs%MHD_step%time_d%i_time_step,    &
     &                            MHDSs%MHD_step%viz_step)
          call visualize_surface                                        &
     &       (MHDSs%MHD_step%viz_step, MHDSs%MHD_step%time_d,           &
     &        MHDSs%FEM_DAT%geofem, MHDSs%edge_comm,                    &
     &        MHDSs%FEM_DAT%field, MHDSs%PSFs, MHDSs%m_SR)
!*
!*  ----------- Zonal means --------------
!*
          if(MHDSs%MHD_step%viz_step%istep_psf .ge. 0) then
            call SPH_MHD_zmean_sections                                 &
     &         (MHDSs%MHD_step%viz_step, MHDSs%MHD_step%time_d,         &
     &          MHDSs%SPH_MHD%sph, MHDSs%FEM_DAT%geofem,                &
     &          MHDSs%SPH_WK%trns_WK, MHDSs%FEM_DAT%field,              &
     &          MHDSs%zmeans, MHDSs%m_SR)
          end if
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!*
        if(lead_field_data_flag(MHDSs%MHD_step%time_d%i_time_step,      &
     &                          MHDSs%MHD_step)) then
          call dealloc_sph_trans_area_snap(MHDSs%SPH_WK%trns_WK)
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
      call FEM_finalize(MHDSs%MHD_files, MHDSs%MHD_step, MHDSs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(MHDSs%SPH_MHD%sph)
      if (iflag_debug.eq.1) write(*,*) 'output_elapsed_times '
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd_w_psf
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD_w_psf
