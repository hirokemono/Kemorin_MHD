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
!!      subroutine initialize_sph_mhd_only(control_file_name)
!!      subroutine evolution_sph_mhd_only
!!        character(len=kchara), intent(in) :: control_file_name
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
      use t_spherical_MHD
      use t_FEM_mesh_field_data
!
      implicit none
!
!>      Structure of the all data of program
      type(spherical_MHD), save, private :: MHDMs
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_data), save, private :: FEM_DATs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd_only(control_file_name)
!
      use t_time_data
      use t_ctl_data_MHD
      use t_SPH_mesh_field_data
      use input_control_sph_MHD
      use set_control_sph_mhd
      use set_control_4_SPH_to_FEM
      use SPH_analyzer_MHD
      use init_sph_MHD_elapsed_label
!
      character(len=kchara), intent(in) :: control_file_name
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: DNS_MHD_ctl1
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      MHDMs%MHD_step%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      call input_control_4_SPH_MHD_nosnap(control_file_name,            &
     &    MHDMs%MHD_files, DNS_MHD_ctl1, MHDMs%MHD_step,                &
     &    MHDMs%SPH_model, MHDMs%SPH_WK, MHDMs%SPH_MHD)
      call copy_delta_t(MHDMs%MHD_step%init_d, MHDMs%MHD_step%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!    precondition elaps start
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_MHD'
      call SPH_initialize_MHD(MHDMs%MHD_files, MHDMs%SPH_model,         &
     &    FEM_DATs, MHDMs%MHD_step, MHDMs%MHD_IO%rst_IO,                &
     &    MHDMs%SPH_MHD, MHDMs%SPH_WK, MHDMs%m_SR)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd_only
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd_only
!
      use t_time_data
      use SPH_analyzer_MHD
      use init_sph_MHD_elapsed_label
!
      integer(kind = kint) :: iflag_finish
!
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -------  time evelution loop start -----------
!*
      iflag_finish = 0
      do
        call evolve_time_data(MHDMs%MHD_step%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_MHD'
        call SPH_analyze_MHD(MHDMs%MHD_files, iflag_finish,             &
     &      MHDMs%SPH_model, MHDMs%MHD_step, MHDMs%MHD_IO%rst_IO,       &
     &      MHDMs%SPH_MHD, MHDMs%SPH_WK, MHDMs%m_SR)
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
      call write_resolution_data(MHDMs%SPH_MHD%sph)
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
