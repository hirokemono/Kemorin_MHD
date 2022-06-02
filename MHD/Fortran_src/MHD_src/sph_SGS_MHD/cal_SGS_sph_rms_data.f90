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
!!     &         (sph_params, sph_rj, sph_bc_U, leg, ipol, ipol_LES,    &
!!     &          rj_fld, pwr, WK_pwr, Nusselt, dip, tsl)
!!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!        type(nusselt_number_data), intent(inout) :: Nusselt
!!        type(dipolarity_data), intent(inout) :: dip
!!        type(typical_scale_data), intent(inout) :: tsl
!!      subroutine cal_mean_squre_w_SGS_in_shell(sph_params, sph_rj,    &
!!     &          ipol, ipol_LES, rj_fld, g_sph_rj, pwr, WK_pwr)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
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
      use t_schmidt_poly_on_rtm
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_boundary_params_sph_MHD
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
      use t_sum_sph_rms_data
      use t_no_heat_Nusselt
      use t_CMB_dipolarity
      use t_sph_typical_scales
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
     &         (sph_params, sph_rj, sph_bc_U, leg, ipol, ipol_LES,      &
     &          rj_fld, pwr, WK_pwr, Nusselt, dip, tsl)
!
      use calypso_mpi
      use cal_rms_fields_by_sph
      use pickup_sph_spectr_data
      use pickup_gauss_coefficients
      use cal_heat_source_Nu
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
      type(nusselt_number_data), intent(inout) :: Nusselt
      type(dipolarity_data), intent(inout) :: dip
      type(typical_scale_data), intent(inout) :: tsl
!
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_mean_squre_w_SGS_in_shell'
      call cal_mean_squre_w_SGS_in_shell(sph_params, sph_rj,            &
      &   ipol, ipol_LES, rj_fld, leg%g_sph_rj, pwr, WK_pwr)
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_no_heat_source_Nu'
        call cal_no_heat_source_Nu                                      &
!     &     (ipol%base%i_temp, ipol%base%i_heat_source,                  &
!     &      ipol%grad_fld%i_grad_temp,               &
     &     (ipol%base%i_temp, ipol%grad_fld%i_grad_temp,                &
     &      sph_rj, sph_bc_U, rj_fld, Nusselt)
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_CMB_dipolarity'
      call cal_CMB_dipolarity(my_rank, rj_fld, pwr, dip)
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_typical_scales'
      call cal_typical_scales(rj_fld, pwr, tsl)
!
      end subroutine cal_SGS_sph_monitor_data
!
!  --------------------------------------------------------------------
!
      subroutine cal_mean_squre_w_SGS_in_shell(sph_params, sph_rj,      &
     &          ipol, ipol_LES, rj_fld, g_sph_rj, pwr, WK_pwr)
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
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      if(pwr%ntot_comp_sq .eq. 0) return
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_SGS_sph_layerd_rms'
      call sum_SGS_sph_layerd_rms                                       &
     &   (sph_params%l_truncation, sph_rj, ipol, ipol_LES, g_sph_rj,    &
     &    rj_fld, pwr%nri_rms, pwr%num_fld_sq, pwr%istack_comp_sq,      &
     &    pwr%id_field, pwr%kr_4_rms, pwr%num_vol_spectr,               &
     &    pwr%v_spectr, WK_pwr)
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
      call sum_mean_square_on_sphere(sph_params, sph_rj, pwr)
      call sum_mean_square_on_volume(sph_params, pwr%ntot_comp_sq,      &
     &    pwr%num_vol_spectr, pwr%v_spectr)
!
      end subroutine cal_mean_squre_w_SGS_in_shell
!
! ----------------------------------------------------------------------
!
      subroutine sum_SGS_sph_layerd_rms(l_truncation, sph_rj,           &
     &          ipol, ipol_LES, g_sph_rj, rj_fld, nri_rms, num_rms_rj,  &
     &          istack_rms_comp_rj, ifield_rms_rj, kr_for_rms,          &
     &          num_vol_spectr, v_pwr, WK_pwr)
!
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
      use sum_sph_rms_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nri_rms
      integer(kind = kint), intent(in) :: num_rms_rj
      integer(kind = kint), intent(in)                                  &
     &            :: istack_rms_comp_rj(0:num_rms_rj)
      integer(kind = kint), intent(in) :: ifield_rms_rj(num_rms_rj)
      integer(kind = kint), intent(in) :: kr_for_rms(nri_rms)
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      integer(kind = kint), intent(in) :: num_vol_spectr
      type(sph_vol_mean_squares), intent(in) :: v_pwr(num_vol_spectr)
!
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: j_fld, i_fld
      integer(kind = kint) :: icomp_rj, jcomp_st, ncomp_rj
      integer(kind = kint) :: num, inum
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
      do j_fld = 1, num_rms_rj
        i_fld = ifield_rms_rj(j_fld)
        icomp_rj = rj_fld%istack_component(i_fld-1) + 1
        jcomp_st = istack_rms_comp_rj(j_fld-1) + 1
        ncomp_rj = istack_rms_comp_rj(j_fld)                            &
     &            - istack_rms_comp_rj(j_fld-1)
        num = sph_rj%nidx_rj(2) * ncomp_rj
        call cal_rms_sph_spec_one_field                                 &
     &     (sph_rj, ipol, ncomp_rj, g_sph_rj, icomp_rj,                 &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,             &
     &      WK_pwr%shl_rj(0,1,1))
        call cvt_filtered_ene_spectr                                    &
     &     (sph_rj, ipol_LES, ncomp_rj, icomp_rj, WK_pwr%shl_rj(0,1,1))
!
        do inum = 1, num_vol_spectr
          call radial_integration                                       &
     &       (v_pwr(inum)%kr_inside, v_pwr(inum)%kr_outside,            &
     &        sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, num,            &
     &        WK_pwr%shl_rj(0,1,1), WK_pwr%volume_j(1,1))
!
          call sum_sph_v_rms_by_degree(l_truncation, sph_rj%nidx_rj(2), &
     &        WK_pwr%istack_mode_sum_l,  WK_pwr%item_mode_sum_l,        &
     &        ncomp_rj, WK_pwr%volume_j(1,1),                           &
     &        WK_pwr%vol_l_local(0,jcomp_st,inum))
          call sum_sph_v_rms_by_degree(l_truncation, sph_rj%nidx_rj(2), &
     &        WK_pwr%istack_mode_sum_m,  WK_pwr%item_mode_sum_m,        &
     &        ncomp_rj, WK_pwr%volume_j(1,1),                           &
     &        WK_pwr%vol_m_local(0,jcomp_st,inum))
          call sum_sph_v_rms_by_degree(l_truncation, sph_rj%nidx_rj(2), &
     &        WK_pwr%istack_mode_sum_lm, WK_pwr%item_mode_sum_lm,       &
     &        ncomp_rj, WK_pwr%volume_j(1,1),                           &
     &        WK_pwr%vol_lm_local(0,jcomp_st,inum))
        end do
!
        if(nri_rms .le. 0) cycle
        call sum_sph_rms_by_degree                                      &
     &     (l_truncation, sph_rj%nidx_rj, nri_rms, kr_for_rms,          &
     &      WK_pwr%istack_mode_sum_l,  WK_pwr%item_mode_sum_l,          &
     &      ncomp_rj, WK_pwr%shl_rj, WK_pwr%shl_l_local(1,0,jcomp_st))
        call sum_sph_rms_by_degree                                      &
     &     (l_truncation, sph_rj%nidx_rj, nri_rms, kr_for_rms,          &
     &      WK_pwr%istack_mode_sum_m,  WK_pwr%item_mode_sum_m,          &
     &      ncomp_rj, WK_pwr%shl_rj, WK_pwr%shl_m_local(1,0,jcomp_st))
        call sum_sph_rms_by_degree                                      &
     &     (l_truncation, sph_rj%nidx_rj, nri_rms, kr_for_rms,          &
     &      WK_pwr%istack_mode_sum_lm, WK_pwr%item_mode_sum_lm,         &
     &      ncomp_rj, WK_pwr%shl_rj, WK_pwr%shl_lm_local(1,0,jcomp_st))
      end do
!
      end subroutine sum_SGS_sph_layerd_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cvt_filtered_ene_spectr                                &
     &        (sph_rj, ipol_LES, ncomp_rj, icomp_rj, rms_sph_rj)
!
      use cal_rms_by_sph_spectr
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES
      integer(kind = kint), intent(in) :: ncomp_rj, icomp_rj
!
      real(kind = kreal), intent(inout)                                 &
     &    :: rms_sph_rj(0:sph_rj%nidx_rj(1),sph_rj%nidx_rj(2),ncomp_rj)
!
!
      if(ncomp_rj .ne. n_scalar) return
      call cvt_mag_or_kin_ene_spectr(sph_rj, ipol_LES%filter_fld,       &
     &    icomp_rj, rms_sph_rj(0,1,1))
      call cvt_mag_or_kin_ene_spectr(sph_rj, ipol_LES%wide_filter_fld,  &
     &    icomp_rj, rms_sph_rj(0,1,1))
      call cvt_mag_or_kin_ene_spectr(sph_rj, ipol_LES%dbl_filter_fld,   &
     &    icomp_rj, rms_sph_rj(0,1,1))
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
