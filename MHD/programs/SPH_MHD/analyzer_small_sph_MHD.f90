!>@file   analyzer_small_sph_MHD.f90
!!@brief  module analyzer_small_sph_MHD
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!        without visualization and snapshot routines
!!
!!@verbatim
!!      subroutine initialize_sph_mhd_only(control_file_name, MHDM)
!!      subroutine evolution_sph_mhd_only(MHDM)
!!        character(len=kchara), intent(in) :: control_file_name
!!        type(sph_MHD_mini), intent(inout) :: MHDM
!!@endverbatim
!
!
      module analyzer_small_sph_MHD
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use t_SPH_MHD_model_data
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_SPH_mesh_field_data
      use t_FEM_mesh_field_data
      use t_MHD_IO_data
      use t_ctl_data_MHD
      use t_control_data_surfacings
      use t_work_SPH_MHD
      use t_mesh_SR
!
      use SPH_analyzer_MHD
      use init_sph_MHD_elapsed_label
!
      implicit none
!
      type sph_MHD_mini
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
      end type sph_MHD_mini
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd_only(control_file_name, MHDM)
!
      use input_control_sph_MHD
      use bcast_control_sph_MHD
      use set_control_sph_mhd
!
      character(len=kchara), intent(in) :: control_file_name
      type(sph_MHD_mini), intent(inout) :: MHDM
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: DNS_MHD_ctl1
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      MHDM%MHD_step%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      call input_control_4_SPH_MHD_nosnap(control_file_name,            &
     &    MHDM%MHD_files, DNS_MHD_ctl1, MHDM%MHD_step,                  &
     &    MHDM%SPH_model, MHDM%SPH_WK, MHDM%SPH_MHD)
      call copy_delta_t(MHDM%MHD_step%init_d, MHDM%MHD_step%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!    precondition elaps start
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD                                           &
     &   (MHDM%MHD_files, MHDM%SPH_model, MHDM%FEM_DAT, MHDM%MHD_step,  &
     &    MHDM%MHD_IO%rst_IO, MHDM%SPH_MHD, MHDM%SPH_WK, MHDM%m_SR)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd_only
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd_only(MHDM)
!
      type(sph_MHD_mini), intent(inout) :: MHDM
      integer(kind = kint) :: iflag_finish
!
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(MHDM%MHD_step%init_d,                    &
     &                         MHDM%MHD_step%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(MHDM%MHD_step%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(MHDM%MHD_files, iflag_finish,              &
     &      MHDM%SPH_model, MHDM%MHD_step, MHDM%MHD_IO%rst_IO,          &
     &      MHDM%SPH_MHD, MHDM%SPH_WK, MHDM%m_SR)
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
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(MHDM%SPH_MHD%sph)
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd_only
!
! ----------------------------------------------------------------------
!
      end module analyzer_small_sph_MHD
