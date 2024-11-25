!>@file   analyzer_sph_licv.f90
!!@brief  module analyzer_sph_licv
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for linear convection model in spherical shell
!!
!!@verbatim
!!      subroutine initialize_sph_licv(control_file_name)
!!      subroutine evolution_sph_licv
!!@endverbatim
!
      module analyzer_sph_licv
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use t_spherical_MHD
      use t_FEM_mesh_field_data
!
      use SPH_analyzer_licv
!
      implicit none
!
!
      type sph_linear_convection
!>        Parameters for spectr dynamo model
        type(SPH_MHD_model_data) :: SPH_model
!
!>        Structure of time and step informations
        type(MHD_step_param) :: MHD_step
!>        Structure of spectr grid and data
        type(SPH_mesh_field_data) :: SPH_MHD
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
      end type sph_linear_convection
!
!>      Structure of the all data of program
      type(spherical_MHD), save, private :: LICVs
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_data), save, private :: FEM_DATs
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_licv(control_file_name)
!
      use t_ctl_data_MHD
      use t_SPH_mesh_field_data
      use input_control_sph_MHD
      use set_control_sph_mhd
      use set_control_4_SPH_to_FEM
      use SPH_analyzer_licv
      use init_sph_MHD_elapsed_label
!
      character(len=kchara), intent(in) :: control_file_name
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: DNS_MHD_ctl1
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      LICVs%MHD_step%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      call input_control_4_SPH_MHD_nosnap(control_file_name,            &
     &    LICVs%MHD_files, DNS_MHD_ctl1, LICVs%MHD_step,                &
     &    LICVs%SPH_model,  LICVs%SPH_WK, LICVs%SPH_MHD)
      call copy_delta_t(LICVs%MHD_step%init_d, LICVs%MHD_step%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!   matrix assembling
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_linear_conv'
      call SPH_initialize_linear_conv(LICVs%MHD_files, FEM_DATs,        &
     &    LICVs%SPH_model, LICVs%MHD_step, LICVs%MHD_IO%rst_IO,         &
     &    LICVs%SPH_MHD, LICVs%SPH_WK, LICVs%m_SR)
      call calypso_MPI_barrier
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_licv
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_licv
!
      use t_time_data
      use SPH_analyzer_licv
      use init_sph_MHD_elapsed_label
!
      integer(kind = kint) :: istep, iflag_finish
!
!     ---------------------
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -------  time evelution  -----------
!*
      iflag_finish = 0
      do istep = 1, LICVs%MHD_step%finish_d%i_end_step
        call evolve_time_data(LICVs%MHD_step%time_d)
!
!*  ----------  add time evolution -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_linear_conv'
        call SPH_analyze_linear_conv(LICVs%MHD_files, iflag_finish,     &
     &      LICVs%SPH_model, LICVs%MHD_step, LICVs%MHD_IO%rst_IO,       &
     &      LICVs%SPH_MHD, LICVs%SPH_WK, LICVs%m_SR)
!*
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. izero) exit
      end do
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
!  time evolution end
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_licv'
!      call SPH_finalize_licv
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
        end subroutine evolution_sph_licv
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_licv
