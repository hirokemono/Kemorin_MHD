!>@file   analyzer_noviz_sph_snap.f90
!!@brief  module analyzer_noviz_sph_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!        without visualization routines
!!
!!@verbatim
!!      subroutine initialize_noviz_sph_snap(control_file_name)
!!      subroutine evolution_noviz_sph_snap
!!        character(len=kchara), intent(in) :: control_file_name
!!@endverbatim
!
      module analyzer_noviz_sph_snap
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
!
      use t_spherical_MHD
      use t_FEM_mesh_field_data
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap_w_vizs
!
      implicit none
!
!>      Structure of the all data of program
      type(spherical_MHD), save, private :: SNAPs
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_data), save, private :: FEM_DATs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_noviz_sph_snap(control_file_name)
!
      use m_elapsed_labels_SEND_RECV
      use init_sph_MHD_elapsed_label
      use initialize_sph_snap_noviz
!
      character(len=kchara), intent(in) :: control_file_name
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
      call s_initialize_sph_snap_noviz(control_file_name,               &
     &                                 SNAPs, FEM_DATs)
!
      end subroutine initialize_noviz_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine evolution_noviz_sph_snap
!
      use m_elapsed_labels_SEND_RECV
      use FEM_analyzer_sph_SGS_MHD
      use output_viz_file_control
      use set_time_step_params
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call set_from_initial_step(SNAPs%MHD_step%init_d,                 &
     &                           SNAPs%MHD_step%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(SNAPs%MHD_step%time_d)
        if(output_IO_flag(SNAPs%MHD_step%time_d%i_time_step,            &
     &                    SNAPs%MHD_step%rst_step) .eqv. .FALSE.) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(SNAPs%MHD_step%time_d%i_time_step,      &
     &                          SNAPs%MHD_step)) then
          call alloc_sph_trans_area_snap(SNAPs%SPH_MHD%sph,             &
     &                                   SNAPs%SPH_WK%trns_WK)
!
          if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_snap_vizs'
          call SPH_analyze_snap_vizs                                    &
     &       (SNAPs%MHD_files, SNAPs%SPH_model, SNAPs%MHD_step,         &
     &        SNAPs%SPH_MHD, SNAPs%SPH_WK, SNAPs%m_SR)
        end if
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(SNAPs%MHD_step%time_d%i_time_step,      &
     &                          SNAPs%MHD_step)) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
          call SPH_to_FEM_bridge_MHD                                    &
     &       (SNAPs%SPH_MHD%sph, SNAPs%SPH_WK%trns_WK,                  &
     &        FEM_DATs%geofem, FEM_DATs%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(SNAPs%MHD_files, FEM_DATs,             &
     &      SNAPs%MHD_step, SNAPs%MHD_IO, SNAPs%m_SR)
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
        if(lead_field_data_flag(SNAPs%MHD_step%time_d%i_time_step,      &
     &                          SNAPs%MHD_step)) then
          call dealloc_sph_trans_area_snap(SNAPs%SPH_WK%trns_WK)
        end if
!
!*  -----------  exit loop --------------
!*
        if(SNAPs%MHD_step%time_d%i_time_step                            &
     &        .ge. SNAPs%MHD_step%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize_sph_SGS_MHD'
      call FEM_finalize_sph_SGS_MHD(SNAPs%MHD_files, SNAPs%MHD_step,    &
     &                              SNAPs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_SGS_snap'
!      call SPH_finalize_SGS_snap
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_noviz_sph_snap
!
! ----------------------------------------------------------------------
!
      end module analyzer_noviz_sph_snap
