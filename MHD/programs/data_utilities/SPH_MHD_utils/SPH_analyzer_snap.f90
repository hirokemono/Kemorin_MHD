!
!     module SPH_analyzer_snap
!
!      Written by H. Matsui
!
!      subroutine SPH_init_sph_snap
!      subroutine SPH_analyze_snap(i_step)
!
      module SPH_analyzer_snap
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_snap
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_rms_4_sph_spectr
      use m_node_id_spherical_IO
      use m_physical_property
!
      use set_control_sph_mhd
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use sph_transforms_4_MHD
      use set_radius_func
      use const_radial_mat_4_sph
      use r_interpolate_sph_data
      use sph_mhd_rms_IO
      use sph_mhd_rst_IO_control
!
!
!   Allocate spectr field data
!
      call set_sph_sprctr_data_address(sph1%sph_rj, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'set_radius_rot_reft_dat_4_sph'
      call set_radius_rot_reft_dat_4_sph(depth_high_t, depth_low_t,     &
     &    high_temp, low_temp, angular, sph1%sph_rlm, sph1%sph_rj,      &
     &    sph_grps1%radial_rj_grp, sph1%sph_params, rj_fld1)
!
      if (iflag_debug.gt.0) write(*,*) 'const_2nd_fdm_matrices'
      call const_2nd_fdm_matrices(sph1%sph_params, sph1%sph_rj)
!
! ---------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'set_material_property'
      call set_material_property
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_set_bc_sph_mhd'
      call s_set_bc_sph_mhd                                             &
     &   (sph1%sph_params, sph1%sph_rj, sph_grps1%radial_rj_grp,        &
     &    CTR_nod_grp_name, CTR_sf_grp_name)
      call init_reference_fields(sph1%sph_params, sph1%sph_rj)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD(sph1, comms_sph1, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'const_radial_mat_sph_snap'
      call const_radial_mat_sph_snap(sph1%sph_rj)
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
     &   (sph1%sph_params, sph1%sph_rj, rj_fld1)
!
      end subroutine SPH_init_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_snap(i_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_node_id_spherical_IO
      use m_addresses_trans_sph_MHD
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap(i_step, sph1%sph_rj, rj_fld1)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(reftemp_rj, sph1%sph_rj, rj_fld1)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(sph1%sph_rj, rj_fld1)
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear(sph1, comms_sph1, reftemp_rj, trns_MHD, rj_fld1)
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(reftemp_rj, sph1%sph_rj, rj_fld1)
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd(sph1, comms_sph1, rj_fld1)
      call end_eleps_time(9)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control                                   &
     &   (sph1%sph_params, sph1%sph_rj, rj_fld1)
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
