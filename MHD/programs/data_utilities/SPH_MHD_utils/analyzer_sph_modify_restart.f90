!>@file   analyzer_sph_modify_restart.f90
!!@brief  module analyzer_sph_modify_restart
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to modifity spectr data for new simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mod_restart(control_file_name)
!!      subroutine evolution_sph_mod_restart
!!        character(len=kchara), intent(in) :: control_file_name
!!@endverbatim
!
      module analyzer_sph_modify_restart
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_phys_constants
      use t_spherical_MHD
      use t_FEM_mesh_field_data
!
!
      implicit none
!
!>      Structure of the all data of program
      type(spherical_MHD), save, private :: SNAPs
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_data), save, private :: FEM_DATs
!
      private :: SPH_analyze_mod_restart, set_modify_rj_fields
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mod_restart(control_file_name)
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
      end subroutine initialize_sph_mod_restart
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mod_restart
!
      use m_elapsed_labels_SEND_RECV
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_SGS_snap
      use set_time_step_params
!
      type(field_IO), save  :: sph_fst_IO
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
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_mod_restart'
        call SPH_analyze_mod_restart                                    &
     &     (SNAPs%SPH_model, SNAPs%MHD_files, SNAPs%MHD_step,           &
     &      SNAPs%SPH_MHD, SNAPs%SPH_WK, sph_fst_IO,                    &
     &      SNAPs%m_SR%SR_sig, SNAPs%m_SR%SR_r)
!*
!*  -----------  output field data --------------
!*
        if(SNAPs%MHD_step%time_d%i_time_step                            &
     &        .ge. SNAPs%MHD_step%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(SNAPs%MHD_files, SNAPs%MHD_step, SNAPs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mod_restart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_mod_restart(SPH_model, MHD_files,          &
     &          MHD_step, SPH_MHD, SPH_WK, sph_fst_IO, SR_sig, SR_r)
!
      use m_work_time
!
      use init_spherical_SRs
      use select_copy_from_recv
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use set_sph_restart_IO
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use input_control_sph_MHD
      use set_sph_restart_IO
      use sph_SGS_mhd_monitor_data_IO
      use sph_radial_grad_4_magne
!
      type(SPH_MHD_model_data), intent(in) :: SPH_model
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(field_IO), intent(inout) :: sph_fst_IO
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      MHD_files%org_rst_file_IO%iflag_format                            &
     &    = MHD_files%fst_file_IO%iflag_format
      call read_alloc_sph_rst_4_snap(MHD_step%time_d%i_time_step,       &
     &    MHD_files%org_rj_file_IO, MHD_files%org_rst_file_IO,          &
     &    MHD_step%rst_step, SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld,    &
     &    MHD_step%time_d, SPH_WK%rj_itp)
      call extend_by_potential_with_j                                   &
     &   (SPH_MHD%sph%sph_rj, SPH_model%sph_MHD_bc%sph_bc_B,            &
     &    SPH_MHD%ipol%base%i_magne, SPH_MHD%ipol%base%i_current,       &
     &    SPH_MHD%fld)
!
      call init_sph_send_recv_N                                         &
     &   (n_vector, SPH_MHD%sph, SPH_MHD%comms,                         &
     &    SPH_WK%trans_p%iflag_SPH_recv, SR_sig, SR_r)
!
!*  ----------------Modify spectr data ... ----------
!*
      call set_modify_rj_fields(SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_debug.gt.0) write(*,*) 'output_sph_restart_control'
      call set_sph_restart_num_to_IO(SPH_MHD%fld, sph_fst_IO)
!
      if(output_IO_flag(MHD_step%time_d%i_time_step,                    &
     &                  MHD_step%rst_step)) then
        call output_sph_restart_control(MHD_step%time_d%i_time_step,    &
     &      MHD_files%fst_file_IO, MHD_step%time_d, SPH_MHD%fld,        &
     &      MHD_step%rst_step, sph_fst_IO)
      end if
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine SPH_analyze_mod_restart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_modify_rj_fields(sph, ipol, rj_fld)
!
      use cal_zonal_mean_sph_spectr
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      integer (kind =kint), allocatable :: ipick_degree(:)
      integer(kind = kint) :: ltr_half
!
!
      ltr_half = 1
      allocate(ipick_degree(ltr_half))
      ipick_degree(1) = 0
!
      if (my_rank.eq.0) write(*,*) 'Take zonam mean of light element'
      call take_zonal_mean_rj_field                                     &
     &   (ione, ipol%base%i_light, sph%sph_rj, rj_fld)
!      if (my_rank.eq.0) write(*,*) 'Take sphere average of light element'
!      call pick_degree_sph_spectr(ltr_half, ipick_degree,               &
!     &    ione, ipol%base%i_light, sph%sph_rj, rj_fld)
!
      end subroutine set_modify_rj_fields
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_modify_restart
