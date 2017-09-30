!>@file   SPH_analyzer_SGS_MHD
!!@brief  module SPH_analyzer_SGS_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_initialize_SGS_MHD                               &
!!     &         (MHD_files, bc_IO, iphys, MHD_step, SPH_model,         &
!!     &          sph_MHD_bc, SPH_SGS, SPH_MHD, SPH_WK)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(phys_address), intent(in) :: iphys
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!      subroutine SPH_analyze_SGS_MHD                                  &
!!     &         (i_step, MHD_files, SPH_model, sph_MHD_bc,             &
!!     &          iflag_finish, MHD_step, SPH_SGS, SPH_MHD, SPH_WK)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!@endverbatim
!
      module SPH_analyzer_SGS_MHD
!
      use m_precision
      use m_constants
      use m_MHD_step_parameter
      use m_radial_matrices_sph
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_SPH_SGS_structure
      use t_boundary_data_sph_MHD
      use t_work_SPH_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_SGS_MHD                                 &
     &         (MHD_files, bc_IO, iphys, MHD_step, SPH_model,           &
     &          sph_MHD_bc, SPH_SGS, SPH_MHD, SPH_WK)
!
      use calypso_mpi
      use m_machine_parameter
!
      use m_fdm_coefs
      use m_rms_4_sph_spectr
      use m_bc_data_list
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use set_sph_phys_address
      use const_fdm_coefs
      use set_initial_sph_dynamo
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use cal_SGS_nonlinear
      use init_sph_trans_SGS_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use sph_mhd_rms_IO
      use sph_SGS_MHD_rst_IO_control
      use sgs_ini_model_coefs_IO
      use cal_sol_sph_MHD_crank
      use cal_SGS_nonlinear
      use sph_filtering
      use check_dependency_SGS_MHD
      use input_control_sph_MHD
!
      use m_work_time
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(boundary_spectra), intent(in) :: bc_IO
      type(phys_address), intent(in) :: iphys
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
!
!   Allocate spectr field data
!
      call set_sph_SGS_MHD_spectr_data                                  &
     &   (SPH_SGS%SGS_par, SPH_model%MHD_prop, SPH_MHD)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo                                     &
     &   (bc_IO, SPH_MHD%groups, MHD_BC1, SPH_MHD%ipol, SPH_MHD%sph,    &
     &    SPH_model, sph_MHD_bc, r_2nd, SPH_MHD%fld)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_SGS_MHD'
      call init_sph_transform_SGS_MHD                                   &
     &   (SPH_SGS%SGS_par%model_p, SPH_model, sph_MHD_bc,               &
     &    iphys, trans_p1, SPH_WK%trns_WK, SPH_MHD)
!
!  -------------------------------
!
      call init_SGS_model_sph_mhd                                       &
     &   (SPH_SGS%SGS_par, SPH_MHD%sph, SPH_MHD%groups,                 &
     &    SPH_model%MHD_prop, SPH_SGS%dynamic)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_data_control'
      call sph_initial_data_control                                     &
     &   (MHD_files, SPH_model, SPH_MHD%sph,                            &
     &    sph_MHD_bc%sph_bc_B, SPH_MHD%ipol, SPH_MHD%idpdr,             &
     &    SPH_MHD%itor, SPH_MHD%fld, MHD_step)
      call set_initial_Csim_control                                     &
     &   (MHD_files, MHD_step, SPH_SGS%SGS_par, SPH_SGS%dynamic)
      MHD_step%iflag_initial_step = 0
!
      if(iflag_debug.gt.0) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%fld)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' const_radial_mat_sph_mhd'
      call const_radial_mat_sph_mhd                                     &
     &   (MHD_step%time_d%dt, SPH_model%MHD_prop,                       &
     &    sph_MHD_bc, SPH_MHD%sph%sph_rj, r_2nd, trans_p1%leg,          &
     &    sph_MHD_mat1)
!*
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start                                       &
     &   (SPH_MHD%sph%sph_rj, r_2nd, SPH_model%MHD_prop, sph_MHD_bc,    &
     &    trans_p1%leg, SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
!
!* obtain nonlinear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'first nonlinear'
      call nonlinear_SGS_first(MHD_step%init_d%i_time_step,             &
     &    r_2nd, SPH_model, sph_MHD_bc, trans_p1, SPH_WK%trns_WK,       &
     &    SPH_SGS%SGS_par, SPH_SGS%dynamic, SPH_MHD)
!
!* -----  Open Volume integration data files -----------------
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      call start_elapsed_time(4)
      call open_sph_vol_rms_file_mhd                                    &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld, pwr1, WK_pwr)
      call end_elapsed_time(4)
!
      end subroutine SPH_initialize_SGS_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_SGS_MHD                                    &
     &         (i_step, MHD_files, SPH_model, sph_MHD_bc,               &
     &          iflag_finish, MHD_step, SPH_SGS, SPH_MHD, SPH_WK)
!
      use m_work_time
      use m_fdm_coefs
      use m_rms_4_sph_spectr
!
      use momentum_w_SGS_explicit
      use cal_sol_sph_MHD_crank
      use cal_SGS_nonlinear
      use adjust_reference_fields
      use lead_fields_SPH_SGS_MHD
      use sph_SGS_MHD_rst_IO_control
      use sph_mhd_rms_IO
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(in) :: SPH_model
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      integer(kind = kint), intent(inout) :: iflag_finish
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      integer(kind = kint) :: iflag
      real(kind = kreal) :: total_max
!
!*  ----------  add time evolution -----------------
!*
!
      call start_elapsed_time(5)
      call start_elapsed_time(6)
      if(iflag_debug.gt.0) write(*,*) 'sel_explicit_sph_SGS_MHD'
      call sel_explicit_sph_SGS_MHD                                     &
     &   (i_step, MHD_step%time_d%dt, SPH_SGS%SGS_par%model_p,          &
     &    SPH_model%MHD_prop, sph_MHD_bc, SPH_MHD)
!*
!*  ----------  time evolution by inplicit method ----------
!*
      call start_elapsed_time(7)
      call s_cal_sol_sph_MHD_crank                                      &
     &   (MHD_step%time_d%dt, SPH_MHD%sph%sph_rj,                       &
     &    r_2nd, SPH_model%MHD_prop, sph_MHD_bc, trans_p1%leg,          &
     &    SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%itor,                    &
     &    sph_MHD_mat1, SPH_MHD%fld)
      call end_elapsed_time(7)
      call end_elapsed_time(6)
!*
!*  ----------------lead nonlinear term ... ----------
!*
      call start_elapsed_time(8)
      call nonlinear_with_SGS                                           &
     &   (i_step, SPH_SGS%SGS_par, r_2nd, SPH_model, sph_MHD_bc,        &
     &    trans_p1, SPH_WK%trns_WK, SPH_SGS%dynamic, SPH_MHD)
      call end_elapsed_time(8)
      call end_elapsed_time(5)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_elapsed_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(SPH_model,                        &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%fld)
!*
      iflag = lead_field_data_flag(i_step, MHD_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 'lead_fields_4_SPH_SGS_MHD'
        call lead_fields_4_SPH_SGS_MHD                                  &
     &     (SPH_SGS%SGS_par, r_2nd, SPH_model%MHD_prop, sph_MHD_bc,     &
     &      trans_p1, sph_MHD_mat1, SPH_WK%trns_WK, SPH_SGS%dynamic,    &
     &      SPH_MHD)
      end if
      call end_elapsed_time(9)
!
!*  -----------  output restart data --------------
!*
      call start_elapsed_time(4)
      call start_elapsed_time(10)
      iflag = set_IO_step_flag(MHD_step%time_d%i_time_step,             &
     &                         MHD_step%rst_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &     'output_sph_SGS_MHD_rst_control'
        call output_sph_SGS_MHD_rst_control                             &
     &     (MHD_files, MHD_step%time_d, SPH_MHD%fld, MHD_step%rst_step, &
     &      SPH_SGS%SGS_par%i_step_sgs_coefs, SPH_SGS%SGS_par%model_p,  &
     &      SPH_SGS%dynamic)
      end if
!
      total_time = MPI_WTIME() - total_start
      call MPI_allREDUCE (total_time, total_max, ione, CALYPSO_REAL,    &
     &    MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if      (MHD_step%finish_d%i_end_step .eq. -1                     &
     &   .and. total_max .gt. MHD_step%finish_d%elapsed_time) then
        MHD_step%rst_step%istep_file = MHD_step%finish_d%i_end_step
        iflag_finish = 1
        call output_sph_SGS_MHD_rst_control                             &
     &     (MHD_files, MHD_step%time_d, SPH_MHD%fld, MHD_step%rst_step, &
     &      SPH_SGS%SGS_par%i_step_sgs_coefs, SPH_SGS%SGS_par%model_p,  &
     &      SPH_SGS%dynamic)
      end if
      call end_elapsed_time(10)
!
!*  -----------  lead energy data --------------
!*
      call start_elapsed_time(11)
      iflag = output_IO_flag(i_step, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control(MHD_step%time_d, SPH_MHD%sph,   &
     &      sph_MHD_bc%sph_bc_U, trans_p1%leg, SPH_MHD%ipol,            &
     &      SPH_MHD%fld, pwr1, WK_pwr)
      end if
      call end_elapsed_time(11)
!
      if(iflag_debug.gt.0) write(*,*) 'sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%fld)
!
      if(i_step .ge. MHD_step%finish_d%i_end_step                       &
     &    .and. MHD_step%finish_d%i_end_step .gt. 0) then
        iflag_finish = 1
      end if
      call end_elapsed_time(4)
!
      end subroutine SPH_analyze_SGS_MHD
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_MHD
!
!      end subroutine SPH_finalize_MHD
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_SGS_MHD
