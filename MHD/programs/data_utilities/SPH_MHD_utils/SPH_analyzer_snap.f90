!
!     module SPH_analyzer_snap
!
!      Written by H. Matsui
!
!>@file   SPH_analyzer_MHD
!!@brief  module SPH_analyzer_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_init_sph_snap(iphys, sph_filters)
!!        type(phys_address), intent(in) :: iphys
!!        type(sph_filters_type), intent(inout) :: sph_filters(3)
!!      subroutine SPH_analyze_snap(sph_filters, i_step)
!!        type(sph_filters_type), intent(in) :: sph_filters(3)
!!@endverbatim
!
      module SPH_analyzer_snap
!
      use m_precision
      use t_phys_address
      use t_sph_filtering_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_snap(iphys, sph_filters)
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_rms_4_sph_spectr
      use m_node_id_spherical_IO
      use m_physical_property
      use m_sph_trans_arrays_MHD
      use m_boundary_params_sph_MHD
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
!
      type(phys_address), intent(in) :: iphys
      type(sph_filters_type), intent(inout) :: sph_filters(3)
!
!
!   Allocate spectr field data
!
      call set_sph_sprctr_data_address                                  &
     &   (sph1%sph_rj, ipol, idpdr, itor, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(sph_grps1, ipol, sph1,              &
     &    omega_sph1, ref_temp1, r_2nd, rj_fld1)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD(ipol, idpdr, itor, iphys,             &
     &    sph1, comms_sph1, omega_sph1, trans_p1, trns_WK1, rj_fld1)
!
! ---------------------------------
!
      if(iflag_SGS_model .gt. 0) then
      if(iflag_debug.gt.0) write(*,*)' init_SGS_model_sph_mhd'
      call init_SGS_model_sph_mhd(sph1, sph_grps1, sph_filters)
      end if
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'const_radial_mat_sph_snap'
      call const_radial_mat_sph_snap(sph1%sph_rj, r_2nd, trans_p1%leg)
!
!     --------------------- 
!  set original spectr mesh data for extension of B
!
      call init_radial_sph_interpolation(sph1%sph_params, sph1%sph_rj)
!
!* -----  set integrals for coriolis -----------------
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      call open_sph_vol_rms_file_mhd                                    &
     &   (sph1%sph_params, sph1%sph_rj, ipol, rj_fld1, pwr1, WK_pwr)
!
      end subroutine SPH_init_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_snap(sph_filters, i_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_node_id_spherical_IO
      use m_sph_trans_arrays_MHD
      use m_rms_4_sph_spectr
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
!
      integer(kind = kint), intent(in) :: i_step
      type(sph_filters_type), intent(in) :: sph_filters(3)
!
!
      call read_alloc_sph_rst_4_snap                                    &
     &   (i_step, sph1%sph_rj, ipol, rj_fld1)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph                                    &
     &   (ref_temp1%t_rj, sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start                                       &
     &   (sph1%sph_rj, r_2nd, trans_p1%leg, ipol, itor, rj_fld1)
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear(sph1, comms_sph1, omega_sph1, r_2nd, trans_p1,     &
     &    ref_temp1%t_rj, sph_filters, ipol, itor, trns_WK1, rj_fld1)
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph                                   &
     &   (ref_temp1%t_rj, sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd                                      &
     &   (sph1, comms_sph1, r_2nd, trans_p1, ipol, rj_fld1, trns_WK1)
      call end_eleps_time(9)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control(sph1%sph_params, sph1%sph_rj,     &
     &    trans_p1%leg, ipol, rj_fld1, pwr1, WK_pwr)
      call end_eleps_time(11)
!
!*  -----------  Output spectr data --------------
!*
      if(iflag_debug.gt.0)  write(*,*) 'output_spectr_4_snap'
      call output_spectr_4_snap(i_step, sph1%sph_rj, rj_fld1)
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
