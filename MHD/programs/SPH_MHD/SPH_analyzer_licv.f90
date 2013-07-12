!
!     module SPH_analyzer_licv
!
!      subroutine SPH_initialize_linear_conv
!      subroutine SPH_analyze_linear_conv(i_step)
!
!      Written by H. Matsui
!
      module SPH_analyzer_licv
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
      subroutine SPH_initialize_linear_conv
!
      use m_constants
      use m_parallel_var_dof
      use m_machine_parameter
      use m_control_parameter
!
      use m_geometry_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_rms_4_sph_spectr
!
      use set_control_sph_mhd
      use load_data_for_sph_IO
      use set_initial_sph_dynamo
      use set_reference_sph_mhd
      use set_bc_sph_mhd
      use material_property
      use sph_transforms_4_MHD
      use set_radius_func
      use cal_sph_bc_fdm_matrix
      use const_radial_mat_4_sph
      use cal_rms_fields_by_sph
      use const_coriolis_sph
      use cvt_nod_data_to_sph_data
      use sph_mhd_rms_IO
      use cal_sol_sph_MHD_crank
      use cal_nonlinear
!
      use m_work_time
!
!
!   Load spherical harmonics data
!
      if (iflag_debug.eq.1) write(*,*) 'input_sph_trans_grids'
      call input_sph_trans_grids(my_rank)
!
!   Allocate spectr field data
!
      call allocate_phys_rj_data
      call allocate_phys_rtp_data
      call allocate_rot_rj_data
      call set_sph_sprctr_data_address
      call set_sph_nod_data_address
!
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_iccgN_matrix'
      call allocate_iccgN_matrix(isix, nnod_rtp)
      call time_prog_barrier
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_rms_4_sph_spectr'
      call init_rms_4_sph_spectr
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'set_radius_rot_reft_dat_4_sph'
      call set_radius_rot_reft_dat_4_sph(depth_high_t, depth_low_t,     &
     &    high_temp, low_temp, angular)
!
      if (iflag_debug.gt.0) write(*,*) 'const_2nd_fdm_matrices'
      call const_2nd_fdm_matrices
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_sph_bc_fdm_matrices'
      call s_cal_sph_bc_fdm_matrices
!
      if (iflag_debug.gt.0) write(*,*) 'const_2nd_fdm_coefs'
      call const_2nd_fdm_coefs
!
!* -----  set integrals for coriolis term -----------------
!*
      if(iflag_4_coriolis .gt. 0) then
        if ( iflag_debug.gt.0 ) write(*,*) 'init_sum_coriolis_sph'
        call init_sum_coriolis_sph
      end if
!
      call time_prog_barrier
!
! --------- set reference temperature 
!
      call allocate_reft_rj_data
      call s_set_ref_temp_sph_mhd
!      call check_reference_temp(my_rank)
!
      call time_prog_barrier
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD
!
! ---------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' set_material_property'
      call set_material_property
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_data_control'
      call sph_initial_data_control
!
      if(iflag_debug.gt.0) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*) 's_set_bc_sph_mhd'
      call s_set_bc_sph_mhd
      call time_prog_barrier
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' s_const_radial_mat_4_sph'
      call s_const_radial_mat_4_sph
      call time_prog_barrier
!*
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start
!
!*  ----------------lead nonlinear term ... ----------
!*
      if(iflag_debug .gt. 0) write(*,*) 'first licv_exp'
      call licv_exp
!
!* -----  Open Volume integration data files -----------------
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      call open_sph_vol_rms_file_mhd
!
      end subroutine SPH_initialize_linear_conv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_linear_conv(i_step)
!
      use m_work_time
      use m_t_step_parameter
!
      use cal_momentum_eq_explicit
      use cal_sol_sph_MHD_crank
      use set_reference_sph_mhd
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
      use cal_nonlinear
!
      integer(kind = kint), intent(in) :: i_step
!
!*  ----------  add time evolution -----------------
!*
      if(i_step .eq. 1) then
        if(iflag_debug.gt.0) write(*,*) 'cal_expricit_sph_euler'
        call cal_expricit_sph_euler(i_step)
      else
        if(iflag_debug.gt.0) write(*,*) 'cal_expricit_sph_adams'
        call cal_expricit_sph_adams
      end if
!*
!*  ----------  time evolution by inplicit method ----------
!*
      call s_cal_sol_sph_MHD_crank
!*
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(7)
!
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd
      call end_eleps_time(7)
!
!*  ----------------lead nonlinear term ... ----------
!*
        call licv_exp
!
!*  -----------  output restart data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(8)
      if(iflag_debug.gt.0) write(*,*) 'output_sph_restart_control'
      call output_sph_restart_control
      call end_eleps_time(8)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(10)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control
      call end_eleps_time(10)
!
      if(iflag_debug.gt.0) write(*,*) 'sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph
      call end_eleps_time(4)
!
      end subroutine SPH_analyze_linear_conv
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_licv
!
!
!      end subroutine SPH_finalize_licv
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_licv
