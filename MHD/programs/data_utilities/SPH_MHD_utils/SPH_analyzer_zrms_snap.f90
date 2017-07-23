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
!!      subroutine SPH_analyze_zRMS_snap(i_step, MHD_files, MHD_step)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!@endverbatim
!!
!!@param i_step  time step number
!
      module SPH_analyzer_zrms_snap
!
      use m_precision
!
      use m_machine_parameter
      use m_MHD_step_parameter
      use m_physical_property
      use m_radial_matrices_sph
      use t_MHD_file_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_zRMS_snap(i_step, MHD_files, MHD_step)
!
      use m_work_time
      use m_SGS_control_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_sph_trans_arrays_MHD
      use m_boundary_data_sph_MHD
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use input_control_sph_MHD
      use sph_mhd_rst_IO_control
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: iflag
!
!
      call read_alloc_sph_rst_SGS_snap                                  &
     &   (i_step, MHD_files%org_rj_file_IO, MHD_files, sph1%sph_rj,     &
     &    ipol, rj_fld1, MHD_step%rst_step, MHD_step%init_d,            &
     &    SGS_par1%i_step_sgs_coefs, SGS_par1%model_p, dynamic_SPH1)
      call copy_time_data(MHD_step%init_d, MHD_step%time_d)
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
      call nonlinear(i_step, SGS_par1, sph1, comms_sph1,                &
     &    omega_sph1, r_2nd, MHD_prop1, sph_MHD_bc1, trans_p1,          &
     &    ref_temp1, ref_comp1, ipol, itor,                             &
     &    trns_WK1, dynamic_SPH1, rj_fld1)
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(ref_temp1, ref_comp1, MHD_prop1,  &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      iflag = lead_field_data_flag                                      &
     &      (i_step, MHD_step, SGS_par1%sgs_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
        call s_lead_fields_4_sph_mhd(SGS_par1%model_p, sph1,            &
     &      comms_sph1, r_2nd, MHD_prop1, sph_MHD_bc1, trans_p1,        &
     &      ipol, sph_MHD_mat1, trns_WK1, dynamic_SPH1, rj_fld1)
      end if
      call end_eleps_time(9)
!
      end subroutine SPH_analyze_zRMS_snap
!
!-----------------------------------------------------------------------
!
      end module SPH_analyzer_zrms_snap
