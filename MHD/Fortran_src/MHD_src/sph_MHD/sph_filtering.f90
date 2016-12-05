!>@file   sph_filtering.f90
!!@brief  module sph_filtering
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine init_SGS_model_sph_mhd(sph, sph_grps, dynamic_SPH)
!!
!!      subroutine vector_sph_filter(i_field, i_filter,                 &
!!     &          sph_rj, r_filter, sph_filter, rj_fld)
!!      subroutine scalar_sph_filter(i_field, i_filter,                 &
!!     &          sph_rj, r_filter, sph_filter, rj_fld)
!!      subroutine sym_tensor_sph_filter(i_field, i_filter,             &
!!     &          sph_rj, r_filter, sph_filter, rj_fld)
!!@endverbatim
!!
!
      module sph_filtering
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_phys_data
      use t_sph_filtering_data
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
!
      implicit none
!
      type dynamic_SGS_data_4_sph
!>         Field adddress for dynamic SGS model
        type(SGS_terms_address) :: ifld_sgs
!>         Component adddress for dynamic SGS model
        type(SGS_terms_address) :: icomp_sgs
!>         Data array for dynamic SGS model
        type(SGS_coefficients_type) :: sgs_coefs
!>         Work area for dynamic SGS model
        type(dynamic_model_data) :: wk_sgs
!
!>         Filter functions
        type(sph_filters_type) :: sph_filters(1)
      end type dynamic_SGS_data_4_sph
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_SGS_model_sph_mhd(sph, sph_grps, dynamic_SPH)
!
      use calypso_mpi
!
      type(sph_grids), intent(in) ::  sph
      type(sph_group_data), intent(in) :: sph_grps
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      call init_filter_4_SPH_MHD(sph%sph_params, sph%sph_rj, sph_grps,  &
     &    dynamic_SPH%sph_filters)
!
      call init_work_4_SGS_sph_mhd                                      &
     &   (sph%sph_rtp, dynamic_SPH%ifld_sgs, dynamic_SPH%icomp_sgs,     &
     &    dynamic_SPH%sgs_coefs, dynamic_SPH%wk_sgs)
!
      end subroutine init_SGS_model_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine init_filter_4_SPH_MHD(sph_params, sph_rj, sph_grps,    &
     &          sph_filters)
!
      use calypso_mpi
      use wider_radial_filter_data
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_group_data), intent(in) :: sph_grps
      type(sph_filters_type), intent(inout) :: sph_filters(1)
!
!
      if(iflag_debug.gt.0) write(*,*)' const_sph_radial_filter'
      call const_sph_radial_filter(sph_rj, sph_grps, sph_filters(1))
      call calypso_mpi_barrier
      if(iflag_debug.gt.0) write(*,*)' const_sph_gaussian_filter'
      call const_sph_gaussian_filter(sph_params%l_truncation,           &
     &    sph_filters(1)%sph_moments, sph_filters(1)%sph_filter)
      call calypso_mpi_barrier
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'check_radial_filter sph_filters(1)'
        call check_radial_filter(sph_rj, sph_filters(1)%r_filter)
        write(*,*) 'check_horiz_filter_weight sph_filters(1)'
        call check_horiz_filter_weight(sph_filters(1)%sph_filter)
      end if
!
!   Second filter
!      call const_sph_radial_filter(sph_rj, sph_grps, sph_filters(2))
!
!      if(iflag_debug .gt. 0) then
!        call check_radial_filter(sph_rj, sph_filters(2)%r_filter)
!      end if
!
!      call const_sph_gaussian_filter(sph_params%l_truncation,          &
!     &    sph_filters(2)%sph_moments, sph_filters(2)%sph_filter)
!
!
!   Multiplied filter
!      call cal_wider_fileters(sph_rj, sph_filters(1)%r_filter,         &
!     &   sph_filters(2)%r_filter, sph_filters(3)%r_filter)
!
!      if(iflag_debug .gt. 0) then
!        call check_radial_filter(sph_rj, sph_filters(3)%r_filter)
!      end if
!
!      call const_sph_gaussian_filter(sph_params%l_truncation,          &
!     &    sph_filters(3)%sph_moments, sph_filters(3)%sph_filter)
!
      end subroutine init_filter_4_SPH_MHD
!
! ----------------------------------------------------------------------
!
      subroutine init_work_4_SGS_sph_mhd                                &
     &         (sph_rtp, ifld_sgs, icomp_sgs, sgs_coefs, wk_sgs)
!
      use count_sgs_components
!
      type(sph_rtp_grid), intent(in) ::  sph_rtp
      type(SGS_terms_address), intent(inout) :: ifld_sgs, icomp_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
      integer(kind = kint) :: num_med
!
!
      num_med = sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)
      call s_count_sgs_components(sgs_coefs)
      call alloc_sgs_coefs_layer(num_med,                               &
     &    sgs_coefs%num_field, sgs_coefs%ntot_comp, wk_sgs)
!
      call alloc_SGS_num_coefs(sgs_coefs)
!
      call set_sgs_addresses                                            &
     &   (ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs)
      call check_sgs_addresses                                          &
     &   (ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs)
!
      end subroutine init_work_4_SGS_sph_mhd
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_sph_radial_filter(sph_rj, sph_grps,              &
     &          sph_filters)
!
      use calypso_mpi
      use wider_radial_filter_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_group_data), intent(in) :: sph_grps
      type(sph_filters_type), intent(inout) :: sph_filters
!
!
      call alloc_sph_filter_moms(sph_filters%r_moments)
      call cal_r_gaussian_moments(sph_filters%width,                    &
     &    sph_filters%r_moments)
!
      call const_radial_filter(sph_rj, sph_grps,                        &
     &    sph_filters%r_moments, sph_filters%r_filter)
!
      end subroutine const_sph_radial_filter
!
! ----------------------------------------------------------------------
!
      subroutine const_sph_gaussian_filter                              &
     &         (l_truncation, sph_moments, sph_filter)
!
      integer(kind = kint), intent(in) :: l_truncation
!
      type(sph_gaussian_filter), intent(inout) :: sph_filter
      type(sph_filter_moment), intent(inout) :: sph_moments
!
!
      call calypso_mpi_barrier
      if(iflag_debug.gt.0) write(*,*)' alloc_sph_filter_weights'
      call alloc_sph_filter_weights(l_truncation, sph_filter)
      call calypso_mpi_barrier
      if(iflag_debug.gt.0) write(*,*)' alloc_sph_filter_moms'
      call alloc_sph_filter_moms(sph_moments)
      call calypso_mpi_barrier
      if(iflag_debug.gt.0) write(*,*)' set_sph_gaussian_filter'
      call set_sph_gaussian_filter(sph_filter%l_truncation,             &
     &    sph_filter%f_width, sph_filter%weight,                        &
     &    sph_moments%num_momentum, sph_moments%filter_mom)
!
      end subroutine const_sph_gaussian_filter
!
! ----------------------------------------------------------------------
!
      subroutine const_radial_filter                                    &
     &         (sph_rj, sph_grps, r_moments, r_filter)
!
      use cal_radial_filtering_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_group_data), intent(in) :: sph_grps
      type(sph_filter_moment), intent(in) :: r_moments
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: num_OC, kmin_OC, kmax_OC
!
!
      r_filter%ngrp_node = 1
      call alloc_num_filtering_comb(np_smp, r_filter)
      r_filter%group_name(1) = 'outer_core'
!
      call count_radial_point_4_filter(sph_rj, r_filter)
!
      call alloc_inod_filter_comb(r_filter)
!
      call count_fiiltering_area(r_moments%num_momentum, sph_rj,        &
     &    sph_grps%radial_rj_grp, r_filter, num_OC, kmin_OC, kmax_OC)
!
      call alloc_3d_filter_comb(r_filter)
      call alloc_3d_filter_func(r_filter)
!
      call set_filtering_points(num_OC, kmin_OC, kmax_OC,               &
     &    r_moments%num_momentum, r_moments%nfilter_sides,              &
     &    sph_rj, r_filter)

      call cal_radial_fileters(kmin_OC, kmax_OC,                        &
     &    r_moments%num_momentum, r_moments%nfilter_sides,              &
     &    r_moments%filter_mom, sph_rj, r_filter)
!
      end subroutine const_radial_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine vector_sph_filter(i_field, i_filter,                   &
     &          sph_rj, r_filter, sph_filter, rj_fld)
!
      use sph_radial_filtering
!
      integer(kind = kint),  intent(in) :: i_field, i_filter
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(in) :: r_filter
      type(sph_gaussian_filter), intent(in) :: sph_filter
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(i_filter*i_field .eq. 0) return
      call vector_sph_radial_filter(r_filter%num_node(1),               &
     &    r_filter%ntot_near_nod, r_filter%istack_near_nod,             &
     &    r_filter%inod_filter, r_filter%inod_near, r_filter%weight,    &
     &    sph_rj%nidx_rj, i_field, i_filter,                            &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call overwrt_vect_sph_horiz_filter                                &
     &   (sph_filter%l_truncation, sph_filter%weight,                   &
     &    sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j, i_filter,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine vector_sph_filter
!
! ----------------------------------------------------------------------
!
      subroutine scalar_sph_filter(i_field, i_filter,                   &
     &          sph_rj, r_filter, sph_filter, rj_fld)
!
      use sph_radial_filtering
!
      integer(kind = kint),  intent(in) :: i_field, i_filter
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(in) :: r_filter
      type(sph_gaussian_filter), intent(in) :: sph_filter
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(i_filter*i_field .eq. 0) return
      call scalar_sph_radial_filter(r_filter%num_node(1),               &
     &    r_filter%ntot_near_nod, r_filter%istack_near_nod,             &
     &    r_filter%inod_filter, r_filter%inod_near, r_filter%weight,    &
     &    sph_rj%nidx_rj, i_field, i_filter,                            &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call overwrt_scl_sph_horiz_filter                                 &
     &   (sph_filter%l_truncation, sph_filter%weight,                   &
     &    sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j, i_filter,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine scalar_sph_filter
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_sph_filter(i_field, i_filter,               &
     &          sph_rj, r_filter, sph_filter, rj_fld)
!
      use sph_radial_filtering
!
      integer(kind = kint),  intent(in) :: i_field, i_filter
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(in) :: r_filter
      type(sph_gaussian_filter), intent(in) :: sph_filter
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(i_filter*i_field .eq. 0) return
      call sym_tensor_sph_radial_filter(r_filter%num_node(1),           &
     &    r_filter%ntot_near_nod, r_filter%istack_near_nod,             &
     &    r_filter%inod_filter, r_filter%inod_near, r_filter%weight,    &
     &    sph_rj%nidx_rj, i_field, i_filter,                            &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call overwrt_tsr_sph_horiz_filter                                 &
     &   (sph_filter%l_truncation, sph_filter%weight,                   &
     &    sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j, i_filter,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine sym_tensor_sph_filter
!
! ----------------------------------------------------------------------
!
      end module sph_filtering
