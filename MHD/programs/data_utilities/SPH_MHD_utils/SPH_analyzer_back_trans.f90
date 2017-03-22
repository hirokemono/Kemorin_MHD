!>@file   SPH_analyzer_back_trans
!!@brief  module SPH_analyzer_back_trans
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_init_sph_back_trans(iphys)
!!        type(phys_address), intent(in) :: iphys
!!      subroutine SPH_analyze_back_trans(i_step, MHD_step)
!!        type(MHD_IO_step_param), intent(inout) :: MHD_step
!!@endverbatim
!
      module SPH_analyzer_back_trans
!
      use m_precision
      use calypso_mpi
      use t_phys_address
      use t_MHD_step_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_back_trans(iphys)
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
      use cal_rms_fields_by_sph
      use input_control_sph_MHD
!
      type(phys_address), intent(in) :: iphys
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
     &    omega_sph1, ref_temp1, ref_comp1, r_2nd, rj_fld1)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_back_transform'
      call init_sph_back_transform(ipol, idpdr, itor, iphys,            &
     &    sph1, comms_sph1, omega_sph1, trans_p1, trns_WK1, rj_fld1)
!
! ---------------------------------
!
      call init_radial_sph_interpolation                                &
     &   (MHD1_org_files%rj_file_param, sph1%sph_params, sph1%sph_rj)
!
! ---------------------------------
!
      call init_rms_4_sph_spectr                                        &
     &   (sph1%sph_params, sph1%sph_rj, rj_fld1, pwr1, WK_pwr)
!
      end subroutine SPH_init_sph_back_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_back_trans(i_step, MHD_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_sph_trans_arrays_MHD
      use m_rms_4_sph_spectr
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
      use input_control_sph_MHD
!
      use sph_transforms_snapshot
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_IO_step_param), intent(inout) :: MHD_step
!
!
      call read_alloc_sph_spectr                                        &
     &   (i_step, MHD1_org_files%rj_file_param, sph_file_param1,        &
     &    sph1%sph_rj, ipol, rj_fld1,                                   &
     &    MHD_step%ucd_step, MHD_step%init_d)
      call copy_time_data(MHD_step%init_d, time_d1)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if (iflag_debug.eq.1) write(*,*) 'sph_all_back_transform'
      call sph_all_back_transform(sph1, comms_sph1, trans_p1,           &
     &    ipol, rj_fld1, trns_WK1%trns_MHD)
       call end_eleps_time(9)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_back_trans'
      call output_rms_sph_back_trans                                    &
     &   (MHD_step, sph1%sph_params, sph1%sph_rj,                       &
     &    trans_p1%leg, ipol, rj_fld1, pwr1, WK_pwr)
      call end_eleps_time(11)
!
      end subroutine SPH_analyze_back_trans
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_snap
!
!      end subroutine SPH_finalize_snap
!
! ----------------------------------------------------------------------
!
      subroutine init_sph_back_transform(ipol, idpdr, itor, iphys,      &
     &          sph, comms_sph, omega_sph, trans_p, WK, rj_fld)
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_data
      use t_addresses_sph_transform
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
!
      use set_address_sph_trans_MHD
      use pole_sph_transform
      use MHD_FFT_selector
      use init_sphrical_transform_MHD
      use set_address_all_sph_trans
      use sph_mhd_rms_IO
      use cal_rms_fields_by_sph
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: rj_fld
!
!>      total number of components for spherical harmonics transform
      integer(kind = kint), save :: ncomp_max_trans
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint), save :: nvector_max_trans
!>      total number of svalars for spherical harmonics transform
      integer(kind = kint), save :: nscalar_max_trans
!
      integer(kind = kint) :: i_fld
!
      call init_pole_transform(sph%sph_rtp)
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_backward_trans(rj_fld, WK%trns_MHD,            &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      if(iflag_debug .ge. iflag_routine_msg) then
        call check_address_trans_sph_MHD(ipol, idpdr, itor, iphys,      &
     &      WK%trns_MHD, ncomp_max_trans)
        do i_fld = 1, rj_fld%num_phys_viz
          write(*,*) i_fld, trim(WK%trns_MHD%b_trns_name(i_fld))
        end do
      end if
!
      call alloc_sph_trans_address(sph%sph_rtp, WK)
!
      call sel_sph_transform_MHD(ipol, sph, comms_sph, omega_sph,       &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    trans_p, WK, rj_fld)
!
      end subroutine init_sph_back_transform
!
!-----------------------------------------------------------------------
!
      subroutine sph_all_back_transform(sph, comms_sph, trans_p,        &
     &          ipol, rj_fld, trns_MHD)
!
      use m_solver_SR
      use sph_transforms
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
!
      use legendre_transform_select
      use copy_all_trans_send_recv
      use sph_mhd_rms_IO
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
      integer(kind = kint) :: nscalar_trans
!
!
      if(trns_MHD%ncomp_rj_2_rtp .le. 0) return
!
      nscalar_trans = trns_MHD%nscalar_rj_2_rtp                         &
     &               + 6*trns_MHD%ntensor_rj_2_rtp
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      call copy_all_spectr_to_send                                      &
     &   (sph%sph_rtp%nnod_pole, trns_MHD%ncomp_rj_2_rtp,               &
     &    sph%sph_rj, comms_sph%comm_rj, rj_fld, trns_MHD,              &
     &    n_WS, WS, trns_MHD%flc_pole)
!
      call sph_b_trans_w_poles                                          &
     &   (trns_MHD%ncomp_rj_2_rtp, trns_MHD%nvector_rj_2_rtp,           &
     &    nscalar_trans, sph, comms_sph, trans_p,                       &
     &    n_WS, n_WR, WS(1), WR(1),                                     &
     &    trns_MHD%fld_rtp, trns_MHD%flc_pole, trns_MHD%fld_pole)
!
      end subroutine sph_all_back_transform
!
!-----------------------------------------------------------------------
!
      subroutine output_rms_sph_back_trans(MHD_step,                    &
     &          sph_params, sph_rj, leg, ipol, rj_fld, pwr, WK_pwr)
!
      use m_machine_parameter
      use m_t_step_parameter
      use m_boundary_params_sph_MHD
      use t_MHD_step_parameter
      use t_schmidt_poly_on_rtm
!
      use cal_rms_fields_by_sph
      use volume_average_4_sph
      use output_sph_m_square_file
!
      type(MHD_IO_step_param), intent(in) :: MHD_step
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: iflag
!
!
      iflag = output_IO_flag(time_d1%i_time_step, MHD_step%rms_step)
      if(iflag .ne. 0) return
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph_params%l_truncation, sph_rj, ipol, rj_fld, leg%g_sph_rj,  &
     &    pwr, WK_pwr)
!
      call write_sph_vol_ave_file                                       &
     &   (time_d1%i_time_step, time_d1%time, sph_params, sph_rj, pwr)
      call write_sph_vol_ms_file                                        &
     &   (my_rank, time_d1%i_time_step, time_d1%time,                   &
     &    sph_params, sph_rj, pwr)
      call write_sph_vol_ms_spectr_file                                 &
     &   (my_rank, time_d1%i_time_step, time_d1%time,                   &
     &    sph_params, sph_rj, pwr)
      call write_sph_layer_ms_file                                      &
     &   (my_rank, time_d1%i_time_step, time_d1%time, sph_params, pwr)
      call write_sph_layer_spectr_file                                  &
     &   (my_rank, time_d1%i_time_step, time_d1%time, sph_params, pwr)
!
      end subroutine output_rms_sph_back_trans
!
!  --------------------------------------------------------------------
!
      end module SPH_analyzer_back_trans
