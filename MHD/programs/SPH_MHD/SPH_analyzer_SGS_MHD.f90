!>@file   SPH_analyzer_SGS_MHD
!!@brief  module SPH_analyzer_SGS_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_initialize_SGS_MHD(MHD_files, bc_IO,             &
!!     &          iphys, MHD_step, SPH_model, sph_MHD_bc, SPH_SGS)
!!      subroutine SPH_analyze_SGS_MHD                                  &
!!     &         (i_step, MHD_files, SPH_model, sph_MHD_bc,             &
!!     &          iflag_finish, MHD_step, SPH_SGS)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!@endverbatim
!
      module SPH_analyzer_SGS_MHD
!
      use m_precision
      use m_constants
      use m_MHD_step_parameter
      use m_radial_matrices_sph
      use t_control_parameter
      use t_phys_address
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_SPH_SGS_structure
      use t_boundary_data_sph_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_SGS_MHD(MHD_files, bc_IO,               &
     &          iphys, MHD_step, SPH_model, sph_MHD_bc, SPH_SGS)
!
      use calypso_mpi
      use m_machine_parameter
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_rms_4_sph_spectr
      use m_sph_trans_arrays_MHD
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
!
!
!   Allocate spectr field data
!
      call set_sph_SGS_MHD_sprctr_data                                  &
     &   (SPH_SGS%SGS_par%model_p, sph1%sph_rj,                         &
     &    SPH_model%MHD_prop, ipol, idpdr, itor, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo                                     &
     &   (bc_IO, sph_grps1, MHD_BC1, ipol, sph1,                        &
     &    SPH_model, sph_MHD_bc, r_2nd, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_SGS_MHD'
      call init_sph_transform_SGS_MHD                                   &
     &   (SPH_SGS%SGS_par%model_p, SPH_model%MHD_prop, sph_MHD_bc,      &
     &    ipol, idpdr, itor, iphys, sph1, comms_sph1,                   &
     &    SPH_model%omega_sph, trans_p1, trns_WK1, rj_fld1)
!
!  -------------------------------
!
      call init_SGS_model_sph_mhd(SPH_SGS%SGS_par, sph1, sph_grps1,     &
     &    SPH_model%MHD_prop, SPH_SGS%dynamic)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_data_control'
      call sph_initial_data_control                                     &
     &   (MHD_files, SPH_model%ref_temp, SPH_model%ref_comp,            &
     &    sph1%sph_params, sph1%sph_rj, SPH_model%MHD_prop,             &
     &    sph_MHD_bc%sph_bc_B, ipol, idpdr, itor, rj_fld1, MHD_step)
      call set_initial_Csim_control                                     &
     &   (MHD_files, MHD_step, SPH_SGS%SGS_par, SPH_SGS%dynamic)
      MHD_step%iflag_initial_step = 0
!
      if(iflag_debug.gt.0) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' const_radial_mat_sph_mhd'
      call const_radial_mat_sph_mhd                                     &
     &   (MHD_step%time_d%dt, SPH_model%MHD_prop,                       &
     &    sph_MHD_bc, sph1%sph_rj, r_2nd, trans_p1%leg, sph_MHD_mat1)
!*
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start                                       &
     &   (sph1%sph_rj, r_2nd, SPH_model%MHD_prop,                       &
     &    sph_MHD_bc, trans_p1%leg, ipol, itor, rj_fld1)
!
!* obtain nonlinear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'first nonlinear'
      call nonlinear_SGS_first(MHD_step%init_d%i_time_step,             &
     &    sph1, comms_sph1, SPH_model%omega_sph, r_2nd,                 &
     &    SPH_model%MHD_prop, sph_MHD_bc, trans_p1,                     &
     &    SPH_model%ref_temp, SPH_model%ref_comp, ipol, itor, trns_WK1, &
     &    SPH_SGS%SGS_par, SPH_SGS%dynamic, rj_fld1)
!
!* -----  Open Volume integration data files -----------------
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      call start_elapsed_time(4)
      call open_sph_vol_rms_file_mhd                                    &
     &   (sph1%sph_params, sph1%sph_rj, ipol, rj_fld1, pwr1, WK_pwr)
      call end_elapsed_time(4)
!
      end subroutine SPH_initialize_SGS_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_SGS_MHD                                    &
     &         (i_step, MHD_files, SPH_model, sph_MHD_bc,               &
     &          iflag_finish, MHD_step, SPH_SGS)
!
      use m_work_time
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_sph_trans_arrays_MHD
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
     &    SPH_model%MHD_prop, sph_MHD_bc, sph1%sph_rj,                  &
     &    ipol, itor, rj_fld1)
!*
!*  ----------  time evolution by inplicit method ----------
!*
      call start_elapsed_time(7)
      call s_cal_sol_sph_MHD_crank(MHD_step%time_d%dt, sph1%sph_rj,     &
     &    r_2nd, SPH_model%MHD_prop, sph_MHD_bc, trans_p1%leg,          &
     &    ipol, idpdr, itor, sph_MHD_mat1, rj_fld1)
      call end_elapsed_time(7)
      call end_elapsed_time(6)
!*
!*  ----------------lead nonlinear term ... ----------
!*
      call start_elapsed_time(8)
      call nonlinear_w_SGS(i_step, SPH_SGS%SGS_par, sph1, comms_sph1,   &
     &    SPH_model%omega_sph, r_2nd, SPH_model%MHD_prop, sph_MHD_bc,   &
     &    trans_p1, SPH_model%ref_temp, SPH_model%ref_comp, ipol, itor, &
     &    trns_WK1, SPH_SGS%dynamic, rj_fld1)
      call end_elapsed_time(8)
      call end_elapsed_time(5)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_elapsed_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(SPH_model,                        &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      iflag = lead_field_data_flag(i_step, MHD_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 'lead_fields_4_SPH_SGS_MHD'
        call lead_fields_4_SPH_SGS_MHD(SPH_SGS%SGS_par%model_p, sph1,   &
     &      comms_sph1, r_2nd, SPH_model%MHD_prop,                      &
     &      sph_MHD_bc, trans_p1, ipol, sph_MHD_mat1, trns_WK1,         &
     &      SPH_SGS%dynamic, rj_fld1)
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
     &     (MHD_files, MHD_step%time_d, rj_fld1, MHD_step%rst_step,     &
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
     &     (MHD_files, MHD_step%time_d, rj_fld1, MHD_step%rst_step,     &
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
        call output_rms_sph_mhd_control                                 &
     &     (MHD_step%time_d, sph1%sph_params, sph1%sph_rj,              &
     &      sph_MHD_bc%sph_bc_U, trans_p1%leg, ipol, rj_fld1,           &
     &      pwr1, WK_pwr)
      end if
      call end_elapsed_time(11)
!
      if(iflag_debug.gt.0) write(*,*) 'sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
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
