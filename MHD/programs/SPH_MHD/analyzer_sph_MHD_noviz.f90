!>@file   analyzer_sph_MHD_noviz.f90
!!@brief  module analyzer_sph_MHD_noviz
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!        without cross sectioning routines
!!
!!@verbatim
!!      subroutine initialize_sph_MHD_noviz(control_file_name, MHDN)
!!      subroutine evolution_sph_MHD_noviz(MHDN)
!!        character(len=kchara), intent(in) :: control_file_name
!!        type(sph_MHD_noviz), intent(inout) :: MHDN
!!@endverbatim
!
      module analyzer_sph_MHD_noviz
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
!
      use t_SPH_MHD_model_data
      use t_MHD_step_parameter
      use t_SPH_mesh_field_data
      use t_ctl_data_MHD
      use t_MHD_file_parameter
      use t_MHD_IO_data
      use t_FEM_mesh_field_data
      use t_ctl_data_sph_MHD_w_psf
      use t_sph_trans_arrays_MHD
      use t_work_SPH_MHD
      use t_mesh_SR
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_MHD
      use init_sph_MHD_elapsed_label
!
      implicit none
!
      type sph_MHD_noviz
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
      end type sph_MHD_noviz
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_MHD_noviz(control_file_name, MHDN)
!
      use input_control_sph_MHD
      use parallel_FEM_mesh_init
!
      character(len=kchara), intent(in) :: control_file_name
      type(sph_MHD_noviz), intent(inout) :: MHDN
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: DNS_MHD_ctl1
!>      Additional structures for spherical MHD dynamo with viz module
      type(add_psf_sph_mhd_ctl), save :: add_SMHD_ctl1
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      MHDN%MHD_step%finish_d%started_time = MPI_WTIME()
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
     &    MHDN%MHD_files, DNS_MHD_ctl1, add_SMHD_ctl1, MHDN%MHD_step,   &
     &    MHDN%SPH_model, MHDN%SPH_WK, MHDN%SPH_MHD, MHDN%FEM_DAT)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!        Initialize FEM mesh data for field data IO
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_MHD'
      call FEM_initialize_sph_MHD(MHDN%MHD_files, MHDN%MHD_step,        &
     &    MHDN%FEM_DAT, MHDN%MHD_IO, MHDN%SPH_WK%nod_mntr, MHDN%m_SR)
      call FEM_mesh_initialization                                      &
     &   (MHDN%FEM_DAT%geofem%mesh, MHDN%FEM_DAT%geofem%group,          &
     &    MHDN%m_SR%SR_sig, MHDN%m_SR%SR_i)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD                                           &
     &   (MHDN%MHD_files, MHDN%SPH_model, MHDN%FEM_DAT, MHDN%MHD_step,  &
     &    MHDN%MHD_IO%rst_IO, MHDN%SPH_MHD, MHDN%SPH_WK, MHDN%m_SR)
!
      call calypso_MPI_barrier
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_MHD_noviz
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_MHD_noviz(MHDN)
!
      use output_viz_file_control
!
      type(sph_MHD_noviz), intent(inout) :: MHDN
      integer(kind = kint) :: iflag_finish
!
!     ---------------------
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(MHDN%MHD_step%init_d,                    &
     &                         MHDN%MHD_step%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(MHDN%MHD_step%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(MHDN%MHD_step%time_d%i_time_step,       &
     &                          MHDN%MHD_step)) then
          call alloc_sph_trans_area_snap(MHDN%SPH_MHD%sph,              &
     &                                   MHDN%SPH_WK%trns_WK)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(MHDN%MHD_files, iflag_finish,              &
     &      MHDN%SPH_model, MHDN%MHD_step, MHDN%MHD_IO%rst_IO,          &
     &      MHDN%SPH_MHD, MHDN%SPH_WK, MHDN%m_SR)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(MHDN%MHD_step%time_d%i_time_step,       &
     &                          MHDN%MHD_step)) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_MHD'
          call SPH_to_FEM_bridge_MHD                                    &
     &       (MHDN%SPH_MHD%sph, MHDN%SPH_WK%trns_WK,                    &
     &        MHDN%FEM_DAT%geofem, MHDN%FEM_DAT%field)
          call dealloc_sph_trans_area_snap(MHDN%SPH_WK%trns_WK)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHDN%MHD_files, MHDN%FEM_DAT,          &
     &      MHDN%MHD_step, MHDN%MHD_IO, MHDN%m_SR)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
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
      call FEM_finalize(MHDN%MHD_files, MHDN%MHD_step, MHDN%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(MHDN%SPH_MHD%sph)
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_MHD_noviz
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD_noviz
