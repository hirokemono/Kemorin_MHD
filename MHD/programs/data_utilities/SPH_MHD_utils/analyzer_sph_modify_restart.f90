!>@file   analyzer_sph_modify_restart.f90
!!@brief  module analyzer_sph_modify_restart
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to modifity spectr data for new simulation
!!
!!@verbatim
!!      subroutine evolution_sph_mod_restart
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
      use m_SPH_MHD_model_data
      use m_MHD_step_parameter
      use m_phys_constants
      use t_SPH_mesh_field_data
      use t_step_parameter
      use t_MHD_file_parameter
      use t_SPH_mesh_field_data
      use t_field_data_IO
      use t_sph_mhd_monitor_data_IO
!
      implicit none
!
!>      Structure of spectr grid and data
      type(SPH_mesh_field_data), save, private :: SPH_MHD1
!
      private :: SPH_analyze_mod_restart, set_modify_rj_fields
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mod_restart
!
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_SGS_snap
      use set_time_step_params
!
      type(field_IO), save  :: sph_fst_IO
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call set_from_initial_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
        if(output_IO_flag(MHD_step1%time_d%i_time_step,                 &
     &                    MHD_step1%rst_step) .eqv. .FALSE.) cycle
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_mod_restart'
        call SPH_analyze_mod_restart(MHD_step1%time_d%i_time_step,      &
     &      MHD_files1%fst_file_IO, SPH_model1, MHD_files1,             &
     &      MHD_step1, SPH_MHD1, SPH_WK1, sph_fst_IO,                   &
     &      m_SR1%SR_sig, m_SR1%SR_r)
!*
!*  -----------  output field data --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, MHD_IO1)
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
      subroutine SPH_analyze_mod_restart                                &
     &         (i_step, fst_file_IO, SPH_model, MHD_files, MHD_step,    &
     &          SPH_MHD, SPH_WK, sph_fst_IO, SR_sig, SR_r)
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
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: fst_file_IO
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
     &    = fst_file_IO%iflag_format
      call read_alloc_sph_rst_4_snap(i_step,                            &
     &    MHD_files%org_rj_file_IO, MHD_files%org_rst_file_IO,          &
     &    MHD_step%rst_step, SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld,    &
     &    MHD_step%init_d)
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
      call copy_time_step_data(MHD_step%init_d, MHD_step%time_d)
      call set_sph_restart_num_to_IO(SPH_MHD%fld, sph_fst_IO)
!
      if(output_IO_flag(MHD_step%time_d%i_time_step,                    &
     &                  MHD_step%rst_step)) then
        call output_sph_restart_control(MHD_step%time_d%i_time_step,    &
     &      fst_file_IO, MHD_step%time_d, SPH_MHD%fld,                  &
     &      MHD_step%rst_step, sph_fst_IO)
      end if
!*
!*  -----------  lead energy data --------------
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+7)
      if(output_IO_flag(MHD_step%time_d%i_time_step,                    &
     &                  MHD_step%rms_step)) then
        if(iflag_debug .gt. 0)                                          &
     &                write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control                                 &
     &     (MHD_step%time_d, SPH_MHD, SPH_model%sph_MHD_bc,             &
     &      SPH_WK%trans_p%leg, SPH_WK%monitor, m_SR1%SR_sig)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+7)
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
