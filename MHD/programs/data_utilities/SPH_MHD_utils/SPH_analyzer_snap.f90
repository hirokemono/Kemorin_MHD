!>@file   SPH_analyzer_snap
!!@brief  module SPH_analyzer_snap
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_init_sph_snap(iphys)
!!        type(phys_address), intent(in) :: iphys
!!      subroutine SPH_analyze_snap(i_step, MHD_step)
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!@endverbatim
!
      module SPH_analyzer_snap
!
      use m_precision
      use m_SGS_control_parameter
      use m_MHD_step_parameter
      use m_physical_property
      use t_phys_address
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_snap(iphys)
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_rms_4_sph_spectr
      use m_physical_property
      use m_sph_trans_arrays_MHD
      use m_boundary_data_sph_MHD
!
      use set_control_sph_mhd
      use set_sph_phys_address
      use const_fdm_coefs
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use init_sphrical_transform_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use r_interpolate_sph_data
      use sph_mhd_rms_IO
      use sph_mhd_rst_IO_control
      use sph_filtering
      use check_dependency_for_MHD
      use input_control_sph_MHD
!
      type(phys_address), intent(in) :: iphys
!
!
!   Allocate spectr field data
!
      call set_sph_MHD_sprctr_data(SGS_par1%model_p, sph1%sph_rj,       &
     &    MHD_prop1, ipol, idpdr, itor, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(sph_grps1, ipol, sph1,              &
     &    omega_sph1, ref_temp1, ref_comp1,MHD_prop1, sph_MHD_bc1,      &
     &    r_2nd, rj_fld1)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD                                       &
     &   (SGS_par1%model_p, MHD_prop1, sph_MHD_bc1%sph_bc_U,            &
     &    ipol, idpdr, itor, iphys, sph1, comms_sph1, omega_sph1,       &
     &    trans_p1, trns_WK1, rj_fld1)
!
! ---------------------------------
!
      if(SGS_par1%model_p%iflag_SGS .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)' init_SGS_model_sph_mhd'
        call init_SGS_model_sph_mhd(SGS_par1, sph1, sph_grps1,          &
     &      MHD_prop1, trns_WK1%dynamic_SPH)
      end if
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'const_radial_mat_sph_snap'
      call const_radial_mat_sph_snap                                    &
     &   (MHD_prop1, sph_MHD_bc1, sph1%sph_rj, r_2nd, trans_p1%leg)
!
!     --------------------- 
!  set original spectr mesh data for extension of B
!
      call init_radial_sph_interpolation                                &
     &   (MHD1_org_files%rj_file_param, sph1%sph_params, sph1%sph_rj)
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      call open_sph_vol_rms_file_mhd                                    &
     &   (sph1%sph_params, sph1%sph_rj, ipol, rj_fld1, pwr1, WK_pwr)
!
      end subroutine SPH_init_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_snap(i_step, MHD_step)
!
      use m_work_time
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_sph_trans_arrays_MHD
      use m_rms_4_sph_spectr
      use m_boundary_data_sph_MHD
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
      use input_control_sph_MHD
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: iflag
!
!
      call read_alloc_sph_rst_4_snap                                    &
     &   (i_step, MHD1_org_files%rj_file_param, sph1%sph_rj,            &
     &    ipol, rj_fld1, MHD_step%rst_step, MHD_step1%init_d)
      call copy_time_data(MHD_step1%init_d, MHD_step1%time_d)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(ref_temp1, ref_comp1, MHD_prop1,   &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(sph1%sph_rj, r_2nd,                   &
     &    MHD_prop1, sph_MHD_bc1, trans_p1%leg, ipol, itor, rj_fld1)
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear                                                    &
     &   (SGS_par1%model_p, sph1, comms_sph1, omega_sph1, r_2nd,        &
     &    MHD_prop1, sph_MHD_bc1, trans_p1, ref_temp1, ref_comp1,       &
     &    ipol, itor, trns_WK1, rj_fld1)
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(ref_temp1, ref_comp1, MHD_prop1,  &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      iflag = lead_field_data_flag(i_step, MHD_step,                    &
     &                             SGS_par1%sgs_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
        call s_lead_fields_4_sph_mhd(SGS_par1%model_p, sph1,            &
     &      comms_sph1, r_2nd, MHD_prop1, sph_MHD_bc1, trans_p1,        &
     &      ipol, rj_fld1, trns_WK1)
      end if
      call end_eleps_time(9)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(11)
      if(output_IO_flag(i_step, MHD_step%rms_step) .eq. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control                                 &
     &     (MHD_step1%time_d, sph1%sph_params, sph1%sph_rj,             &
     &      sph_MHD_bc1%sph_bc_U, trans_p1%leg, ipol, rj_fld1,          &
     &       pwr1, WK_pwr)
      end if
      call end_eleps_time(11)
!
!*  -----------  Output spectr data --------------
!*
      if(iflag_debug.gt.0)  write(*,*) 'output_spectr_4_snap'
      call output_spectr_4_snap(i_step, MHD_step1%time_d,               &
     &    sph_file_param1, rj_fld1, MHD_step%ucd_step)
      call end_eleps_time(4)
!
      end subroutine SPH_analyze_snap
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_snap
!
!      end subroutine SPH_finalize_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_snap
