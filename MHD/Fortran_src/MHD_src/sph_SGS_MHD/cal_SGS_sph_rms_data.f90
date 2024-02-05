!>@file   cal_SGS_sph_rms_data.f90
!!@brief      module cal_SGS_sph_rms_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!> @brief  Evaluate mean square by spherical hermonics coefficients
!!
!!@verbatim
!!      subroutine cal_SGS_sph_monitor_data                             &
!!     &         (time_d, sph, MHD_prop, sph_MHD_bc, r_2nd, trans_p,    &
!!     &          MHD_mats, ipol, ipol_LES, rj_fld, monitor, cdat, bench)
!!        type(sph_grids), intent(in) :: sph
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(MHD_radial_matrices), intent(in) :: MHD_mats
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(circle_fld_maker), intent(inout) :: cdat
!!        type(dynamobench_monitor), intent(inout) :: bench
!!      subroutine cal_mean_squre_w_SGS_in_shell(sph_params, sph_rj,    &
!!     &          ipol, ipol_LES, rj_fld, leg, pwr, WK_pwr)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!      subroutine cvt_filtered_ene_one_mode                            &
!!     &        (sph_rj, ipol_LES, ncomp_rj, icomp_rj, rms_sph_r)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!@endverbatim
!
      module cal_SGS_sph_rms_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_physical_property
      use t_time_data
      use t_phys_data
      use t_phys_address
      use t_work_4_sph_trans
      use t_SGS_model_addresses
      use t_boundary_data_sph_MHD
      use t_boundary_sph_spectr
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
      use t_sum_sph_rms_data
      use t_no_heat_Nusselt
      use t_CMB_dipolarity
      use t_sph_typical_scales
      use t_sph_mhd_monitor_data_IO
      use t_fdm_coefs
      use t_radial_matrices_sph_MHD
      use t_sph_matrix
!
      implicit none
!
      private :: sum_SGS_sph_layerd_rms, cvt_filtered_ene_spectr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_SGS_sph_monitor_data                               &
     &         (time_d, sph, MHD_prop, sph_MHD_bc, r_2nd, trans_p,      &
     &          MHD_mats, ipol, ipol_LES, rj_fld, monitor)
!
      use t_field_on_circle
      use t_field_4_dynamobench
      use calypso_mpi
      use cal_rms_fields_by_sph
      use pickup_sph_spectr_data
      use pickup_gauss_coefficients
      use cal_heat_source_Nu
      use cal_CMB_dipolarity
      use cal_typical_scale
      use const_data_4_dynamobench
      use sph_fwd_trans_on_circles
      use lorentz_spctr_in_shell
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(MHD_radial_matrices), intent(in) :: MHD_mats
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_mean_squre_w_SGS_in_shell'
      call cal_mean_squre_w_SGS_in_shell                                &
     &   (sph%sph_params, sph%sph_rj, ipol, ipol_LES, rj_fld,           &
     &    trans_p%leg, monitor%pwr, monitor%WK_pwr)
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_lorentz_spctr_in_shell'
      call cal_lorentz_spctr_in_shell                                   &
     &   (sph%sph_params, sph%sph_rj, ipol, ipol_LES, rj_fld,           &
     &    trans_p%leg, monitor%lor_spectr, monitor%WK_lor_spectr)
!
       if(monitor%heat_Nusselt%iflag_Nusselt .ne. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'sel_Nusselt_routine'
        call sel_Nusselt_routine(ipol%base%i_temp,                      &
     &      ipol%base%i_heat_source, ipol%grad_fld%i_grad_temp,         &
     &      sph, r_2nd, MHD_prop%ht_prop,                               &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_T, &
     &      sph_MHD_bc%fdm2_center, MHD_mats%band_T00_poisson_fixT,     &
     &      rj_fld, monitor%heat_Nusselt)
      end if
!
      if(monitor%comp_Nusselt%iflag_Nusselt .ne. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'sel_Nusselt_routine'
        call sel_Nusselt_routine(ipol%base%i_light,                     &
     &      ipol%base%i_light_source, ipol%grad_fld%i_grad_composit,    &
     &      sph, r_2nd, MHD_prop%cp_prop,                               &
     &      sph_MHD_bc%sph_bc_C, sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_C, &
     &      sph_MHD_bc%fdm2_center, MHD_mats%band_C00_poisson_fixC,     &
     &      rj_fld, monitor%comp_Nusselt)
      end if
!!
      if(iflag_debug.gt.0)  write(*,*) 's_cal_CMB_dipolarity'
      call s_cal_CMB_dipolarity(my_rank, rj_fld,                        &
     &                          monitor%pwr, monitor%dip)
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_typical_scales'
      call cal_typical_scales(monitor%pwr, monitor%tsl)
!
      call const_dynamobench_data                                       &
     &  (time_d, sph%sph_params, sph%sph_rj, sph_MHD_bc, trans_p, ipol, &
     &   rj_fld, monitor%pwr, monitor%circ_mid_eq, monitor%bench)
!
      call sph_forward_trans_on_circles(trans_p%iflag_FFT,              &
     &    sph%sph_rj, rj_fld, monitor%mul_circle%num_circles,           &
     &    monitor%mul_circle%cdat(1))
!
      end subroutine cal_SGS_sph_monitor_data
!
!  --------------------------------------------------------------------
!
      subroutine cal_mean_squre_w_SGS_in_shell(sph_params, sph_rj,      &
     &          ipol, ipol_LES, rj_fld, leg, pwr, WK_pwr)
!
      use calypso_mpi
!
      use volume_average_4_sph
      use cal_ave_4_rms_vector_sph
      use sum_sph_rms_data
      use cal_rms_fields_by_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      if(pwr%ntot_comp_sq .eq. 0) return
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_SGS_sph_layerd_rms'
      call sum_SGS_sph_layerd_rms(sph_params%l_truncation, sph_rj, pwr, &
     &    ipol, ipol_LES, leg%g_sph_rj, rj_fld, WK_pwr)
!
      if(iflag_debug .gt. 0) write(*,*) 'global_sum_sph_layerd_square'
      call global_sum_sph_layerd_square                                 &
     &    (sph_params%l_truncation, WK_pwr, pwr)
      call global_sum_sph_volume_square                                 &
     &    (sph_params%l_truncation, pwr%ntot_comp_sq, WK_pwr,           &
     &     pwr%num_vol_spectr, pwr%v_spectr)
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_volume_average_sph'
      call cal_volume_average_sph(sph_rj, rj_fld, pwr)
!
      call sum_mean_square_on_sphere(sph_params, pwr)
      call sum_mean_square_on_volume(sph_params, pwr%ntot_comp_sq,      &
     &    pwr%num_vol_spectr, pwr%v_spectr)
!
      end subroutine cal_mean_squre_w_SGS_in_shell
!
! ----------------------------------------------------------------------
!
      subroutine sum_SGS_sph_layerd_rms(l_truncation, sph_rj, pwr,      &
     &          ipol, ipol_LES, g_sph_rj, rj_fld, WK_pwr)
!
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
      use sum_sph_rms_by_degree
      use sum_sph_rms_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: l_truncation
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(sph_mean_squares), intent(in) :: pwr
!
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: j_fld, i_fld
      integer(kind = kint) :: icomp_rj, jcomp_st, ncomp_rj
      integer(kind = kint) :: num, i
!
!
!$omp parallel workshare
      WK_pwr%shl_l_local =  zero
      WK_pwr%shl_m_local =  zero
      WK_pwr%shl_lm_local = zero
      WK_pwr%vol_l_local =  zero
      WK_pwr%vol_m_local =  zero
      WK_pwr%vol_lm_local = zero
!$omp end parallel workshare
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld = pwr%id_field(j_fld)
        icomp_rj = rj_fld%istack_component(i_fld-1) + 1
        jcomp_st = pwr%istack_comp_sq(j_fld-1) + 1
        ncomp_rj = pwr%istack_comp_sq(j_fld)                            &
     &            - pwr%istack_comp_sq(j_fld-1)
        num = sph_rj%nidx_rj(2) * ncomp_rj
        call cal_rms_sph_spec_one_field                                 &
     &     (sph_rj, ncomp_rj, g_sph_rj, icomp_rj, icomp_rj,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,             &
     &      WK_pwr%shl_rj(0,1,1))
!
        if(ncomp_rj .eq. n_vector) then
          call cvt_filtered_ene_spectr(sph_rj, ipol, ipol_LES,          &
     &      ncomp_rj, icomp_rj, WK_pwr%shl_rj(0,1,1))
        end if
!
        call sum_each_sph_layerd_pwr(l_truncation, sph_rj, pwr,         &
     &                               ncomp_rj, jcomp_st, WK_pwr)
      end do
!
      end subroutine sum_SGS_sph_layerd_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cvt_filtered_ene_spectr                                &
     &         (sph_rj, ipol, ipol_LES, ncomp_rj, icomp_rj, rms_sph_rj)
!
      use cal_rms_by_sph_spectr
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      integer(kind = kint), intent(in) :: ncomp_rj, icomp_rj
!
      real(kind = kreal), intent(inout)                                 &
     &    :: rms_sph_rj(0:sph_rj%nidx_rj(1),sph_rj%nidx_rj(2),ncomp_rj)
!
!
      call cvt_mag_or_kin_ene_spectr(sph_rj, ipol%base,                 &
     &                               icomp_rj, rms_sph_rj(0,1,1))
      call cvt_mag_or_kin_ene_spectr(sph_rj, ipol_LES%filter_fld,       &
     &                               icomp_rj, rms_sph_rj(0,1,1))
      call cvt_mag_or_kin_ene_spectr(sph_rj, ipol_LES%wide_filter_fld,  &
     &                               icomp_rj, rms_sph_rj(0,1,1))
      call cvt_mag_or_kin_ene_spectr(sph_rj, ipol_LES%dbl_filter_fld,   &
     &                               icomp_rj, rms_sph_rj(0,1,1))
!
      end subroutine cvt_filtered_ene_spectr
!
! -----------------------------------------------------------------------
!
      subroutine cvt_filtered_ene_one_mode                              &
     &        (sph_rj, ipol_LES, ncomp_rj, icomp_rj, rms_sph_r)
!
      use cal_rms_by_sph_spectr
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES
      integer(kind = kint), intent(in) :: ncomp_rj, icomp_rj
!
      real(kind = kreal), intent(inout)                                 &
     &    :: rms_sph_r(0:sph_rj%nidx_rj(1),ncomp_rj)
!
!
      if(ncomp_rj .ne. n_scalar) return
      call cvt_mag_or_kin_ene_one_mode                                  &
     &   (sph_rj, ipol_LES%filter_fld, icomp_rj, rms_sph_r(0,1))
      call cvt_mag_or_kin_ene_one_mode                                  &
     &   (sph_rj, ipol_LES%wide_filter_fld, icomp_rj, rms_sph_r(0,1))
      call cvt_mag_or_kin_ene_one_mode                                  &
     &   (sph_rj, ipol_LES%dbl_filter_fld, icomp_rj, rms_sph_r(0,1))
!
      end subroutine cvt_filtered_ene_one_mode
!
! -----------------------------------------------------------------------
!
      end module cal_SGS_sph_rms_data
