!
!     module SPH_analyzer_zrms_snap
!
!      Written by H. Matsui
!
!>@file   SPH_analyzer_zrms_snap.f90
!!@brief  module SPH_analyzer_zrms_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  main routines to evaluate zonal root mean square field
!!
!!@verbatim
!!      subroutine SPH_analyze_zRMS_snap                                &
!!     &          (i_step, MHD_files, SPH_model, sph_MHD_bc,            &
!!     &           MHD_step, SPH_SGS, SPH_MHD, SPH_WK)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!@endverbatim
!!
!!@param i_step  time step number
!
      module SPH_analyzer_zrms_snap
!
      use m_precision
!
      use m_machine_parameter
      use t_MHD_file_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_SPH_SGS_structure
      use t_control_parameter
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
      subroutine SPH_analyze_zRMS_snap                                  &
     &          (i_step, MHD_files, SPH_model, sph_MHD_bc,              &
     &           MHD_step, SPH_SGS, SPH_MHD, SPH_WK)
!
      use m_work_time
!
      use cal_SGS_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_SPH_SGS_MHD
      use input_control_sph_MHD
      use sph_SGS_MHD_rst_IO_control
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(in) :: SPH_model
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      integer(kind = kint) :: iflag
!
!
      call read_alloc_sph_rst_SGS_snap                                  &
     &   (i_step, MHD_files%org_rj_file_IO, MHD_files,                  &
     &    SPH_MHD, MHD_step%rst_step, MHD_step%init_d,                  &
     &    SPH_SGS%SGS_par%i_step_sgs_coefs, SPH_SGS%SGS_par%model_p,    &
     &    SPH_SGS%dynamic)
      call copy_time_data(MHD_step%init_d, MHD_step%time_d)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%fld)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(SPH_MHD%sph%sph_rj, SPH_WK%r_2nd,     &
     &    SPH_model%MHD_prop, sph_MHD_bc, SPH_WK%trans_p%leg,           &
     &    SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_elapsed_time(8)
      call nonlinear_with_SGS                                           &
     &   (i_step, SPH_SGS%SGS_par, SPH_WK%r_2nd, SPH_model, sph_MHD_bc, &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_SGS%dynamic, SPH_MHD)
      call end_elapsed_time(8)
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
        call lead_fields_4_SPH_SGS_MHD(SPH_SGS%SGS_par, SPH_WK%r_2nd,   &
     &      SPH_model%MHD_prop, sph_MHD_bc, SPH_WK%trans_p,             &
     &      SPH_WK%MHD_mats, SPH_WK%trns_WK, SPH_SGS%dynamic, SPH_MHD)
      end if
      call end_elapsed_time(9)
!
      end subroutine SPH_analyze_zRMS_snap
!
!-----------------------------------------------------------------------
!
      end module SPH_analyzer_zrms_snap
