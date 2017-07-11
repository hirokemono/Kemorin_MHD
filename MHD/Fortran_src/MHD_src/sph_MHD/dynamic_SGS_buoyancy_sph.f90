!>@file   dynamic_SGS_buoyancy_sph.f90
!!@brief  module dynamic_SGS_buoyancy_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2016
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine const_dynamic_SGS_4_buo_sph                          &
!!     &         (iflag_SGS_buo_usage, stab_weight, sph_rtp, fl_prop,   &
!!     &          trns_MHD, trns_snap, trns_SGS, dynamic_SPH)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!
!!      subroutine sphere_averaged_SGS_buoyancy                         &
!!     &         (sph_rj, sph_rtp, ipol, rj_fld, trns_SGS, dynamic_SPH)
!!      subroutine volume_averaged_SGS_buoyancy                         &
!!     &         (sph_params, sph_rj, ipol, rj_fld, dynamic_SPH)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!
!!      subroutine magnify_sph_ave_SGS_buoyancy                         &
!!     &         (sph_rj, sph_rtp, ipol, dynamic_SPH, rj_fld, trns_SGS)
!!      subroutine magnify_vol_ave_SGS_buoyancy                         &
!!     &         (sph_rtp, ipol, dynamic_SPH, rj_fld, trns_SGS)
!!@endverbatim
!
      module dynamic_SGS_buoyancy_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
      use m_FFT_selector
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_spheric_rtp_data
      use t_phys_address
      use t_addresses_sph_transform
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use sph_filtering
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_dynamic_SGS_4_buo_sph                            &
     &         (iflag_SGS_buo_usage, stab_weight, sph_rtp, fl_prop,     &
     &          trns_MHD, trns_snap, trns_SGS, dynamic_SPH)
!
      use t_physical_property
      use SGS_buo_coefs_sph_MHD
      use cal_SGS_buo_flux_sph_MHD
!
      integer(kind = kint), intent(in) :: iflag_SGS_buo_usage
      real(kind = kreal), intent(in) :: stab_weight
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(address_4_sph_trans), intent(in) :: trns_MHD
!
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
      integer(kind = kint) :: nnod_med
!
!
      nnod_med = sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)
      call SGS_fluxes_for_buo_coefs(nnod_med, sph_rtp, fl_prop,         &
     &    trns_MHD%b_trns, trns_SGS%f_trns, trns_snap%f_trns,           &
     &    trns_MHD%ncomp_rj_2_rtp, trns_SGS%ncomp_rtp_2_rj,             &
     &    trns_snap%ncomp_rtp_2_rj, trns_MHD%fld_rtp, trns_SGS%frc_rtp, &
     &    trns_snap%frc_rtp)
!
      call cal_SGS_buo_coefs_sph_MHD(nnod_med, sph_rtp, stab_weight,    &
     &    trns_snap%frc_rtp, trns_snap%ncomp_rtp_2_rj,                  &
     &    trns_snap%f_trns%i_reynolds_wk,                               &
     &    trns_snap%f_trns%i_SGS_buo_wk,                                &
     &    dynamic_SPH%ifld_sgs%i_buoyancy,                              &
     &    dynamic_SPH%icomp_sgs%i_buoyancy, dynamic_SPH%wk_sgs)
!
      call cal_SGS_buo_coefs_sph_MHD(nnod_med, sph_rtp, stab_weight,    &
     &    trns_snap%frc_rtp, trns_snap%ncomp_rtp_2_rj,                  &
     &    trns_snap%f_trns%i_reynolds_wk,                               &
     &    trns_snap%f_trns%i_SGS_comp_buo_wk,                           &
     &    dynamic_SPH%ifld_sgs%i_comp_buoyancy,                         &
     &    dynamic_SPH%icomp_sgs%i_comp_buoyancy, dynamic_SPH%wk_sgs)
!
      if(iflag_SGS_buo_usage .eq. id_use_zonal) then
        call prod_SGS_buoyancy_to_Reynolds(sph_rtp, trns_SGS%f_trns,    &
     &    dynamic_SPH%ifld_sgs, dynamic_SPH%wk_sgs,                     &
     &    nnod_med, trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      end if
!
      end subroutine const_dynamic_SGS_4_buo_sph
!
! ----------------------------------------------------------------------
!
      subroutine sphere_averaged_SGS_buoyancy                           &
     &         (sph_rj, sph_rtp, ipol, rj_fld, dynamic_SPH)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
      integer(kind = kint) :: k, k_gl, num
!
!
      dynamic_SPH%Cbuo_ave_sph_lc(0:sph_rj%nidx_rj(1),1:2) = 0.0d0
      dynamic_SPH%Cbuo_ave_sph_gl(0:sph_rj%nidx_rj(1),1:2) = 0.0d0
      if(sph_rj%idx_rj_degree_zero .gt. izero) then
!
        if(ipol%i_Csim_SGS_buoyancy .gt. 0) then
          call cal_ave_scalar_sph_spectr                                &
     &       (ione, ione, rj_fld%n_point, sph_rj%nidx_rj,               &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        ione, rj_fld%d_fld(1,ipol%i_Csim_SGS_buoyancy),           &
     &        sph_rj%radius_1d_rj_r,  sph_rj%nidx_rj(1), ione,          &
     &        dynamic_SPH%Cbuo_ave_sph_lc(0,1))
        end if
!
        if(ipol%i_Csim_SGS_comp_buo .gt. 0) then
          call cal_ave_scalar_sph_spectr                                &
     &       (ione, ione, rj_fld%n_point, sph_rj%nidx_rj,               &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        ione, rj_fld%d_fld(1,ipol%i_Csim_SGS_comp_buo),           &
     &        sph_rj%radius_1d_rj_r,  sph_rj%nidx_rj(1), ione,          &
     &        dynamic_SPH%Cbuo_ave_sph_lc(0,2))
        end if
      end if
!
      num = itwo * (sph_rtp%nidx_rtp(1) + 1)
      call MPI_allREDUCE                                                &
     &   (dynamic_SPH%Cbuo_ave_sph_lc, dynamic_SPH%Cbuo_ave_sph_gl,     &
     &    num, CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      do k = 1, sph_rtp%nidx_rtp(1)
        k_gl = sph_rtp%idx_gl_1d_rtp_r(k)
        dynamic_SPH%Cbuo_ave_sph_rtp(k,1)                               &
     &       = dynamic_SPH%Cbuo_ave_sph_gl(k_gl,1)
        dynamic_SPH%Cbuo_ave_sph_rtp(k,2)                               &
     &       = dynamic_SPH%Cbuo_ave_sph_gl(k_gl,2)
      end do
!
      end subroutine sphere_averaged_SGS_buoyancy
!
! ----------------------------------------------------------------------
!
      subroutine volume_averaged_SGS_buoyancy                           &
     &         (sph_params, sph_rj, ipol, rj_fld, dynamic_SPH)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      dynamic_SPH%Cbuo_vol_lc(1:2) = 0.0d0
      dynamic_SPH%Cbuo_vol_gl(1:2) = 0.0d0
      if(sph_rj%idx_rj_degree_zero .gt. izero) then
        dynamic_SPH%Cbuo_ave_sph_lc(1:sph_rj%nidx_rj(1),1:2) = 0.0d0
!
        if(ipol%i_Csim_SGS_buoyancy .gt. 0) then
          call cal_ave_scalar_sph_spectr                                &
     &       (ione, ione, rj_fld%n_point, sph_rj%nidx_rj,               &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        ione, rj_fld%d_fld(1,ipol%i_Csim_SGS_buoyancy),           &
     &        sph_rj%radius_1d_rj_r,  sph_rj%nidx_rj(1), ione,          &
     &        dynamic_SPH%Cbuo_ave_sph_lc(0,1))
        end if
!
        if(ipol%i_Csim_SGS_comp_buo .gt. 0) then
          call cal_ave_scalar_sph_spectr                                &
     &       (ione, ione, rj_fld%n_point, sph_rj%nidx_rj,               &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        ione, rj_fld%d_fld(1,ipol%i_Csim_SGS_comp_buo),           &
     &        sph_rj%radius_1d_rj_r,  sph_rj%nidx_rj(1), ione,          &
     &        dynamic_SPH%Cbuo_ave_sph_lc(0,2))
        end if
!
        call radial_integration                                         &
     &     (sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, itwo,             &
     &      dynamic_SPH%Cbuo_ave_sph_lc, dynamic_SPH%Cbuo_vol_lc)
!
         dynamic_SPH%Cbuo_vol_lc(1:2) = dynamic_SPH%avol_SGS_buo        &
     &                                * dynamic_SPH%Cbuo_vol_lc(1:2)
      end if
!
!
      call MPI_allREDUCE                                                &
     &   (dynamic_SPH%Cbuo_vol_lc, dynamic_SPH%Cbuo_vol_gl, itwo,       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine volume_averaged_SGS_buoyancy
!
! -----------------------------------------------------------------------
!
      subroutine magnify_sph_ave_SGS_buoyancy                           &
     &         (sph_rj, sph_rtp, ipol, dynamic_SPH, rj_fld, trns_SGS)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
      use SGS_buo_coefs_sph_MHD
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: ipol
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!
      type(phys_data), intent(inout) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
!$omp parallel
      call prod_dbl_radial_buo_coefs_rj(rj_fld%n_point, sph_rj%nidx_rj, &
     &    dynamic_SPH%Cbuo_ave_sph_gl(0,1),                             &
     &    dynamic_SPH%Cbuo_ave_sph_gl(0,2),                             &
     &    rj_fld%d_fld(1,ipol%i_SGS_inertia))
!$omp end parallel
!
      call sel_mag_sph_ave_SGS_buo_rtp                                  &
     &   (sph_rtp, dynamic_SPH, trns_SGS)
!
      end subroutine magnify_sph_ave_SGS_buoyancy
!
! ----------------------------------------------------------------------
!
      subroutine magnify_vol_ave_SGS_buoyancy                           &
     &         (sph_rtp, ipol, dynamic_SPH, rj_fld, trns_SGS)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: ipol
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!
      type(phys_data), intent(inout) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
!$omp parallel
      call product_double_vol_buo_coefs(rj_fld%n_point,                 &
     &    dynamic_SPH%Cbuo_vol_gl(1), dynamic_SPH%Cbuo_vol_gl(2),       &
     &    rj_fld%d_fld(1,ipol%i_SGS_inertia))
!$omp end parallel
!$omp parallel
      call product_double_vol_buo_coefs(sph_rtp%nnod_rtp,               &
     &    dynamic_SPH%Cbuo_vol_gl(1), dynamic_SPH%Cbuo_vol_gl(2),       &
     &    trns_SGS%frc_rtp(1,trns_SGS%f_trns%i_SGS_inertia))
!$omp end parallel
!
      end subroutine magnify_vol_ave_SGS_buoyancy
!
! ----------------------------------------------------------------------
!
      end module dynamic_SGS_buoyancy_sph
 