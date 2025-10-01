!>@file   const_filter_on_sph.f90
!!@brief  module const_filter_on_sph
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine const_sph_radial_filter(sph_rj, sph_grps,            &
!!     &          sph_filters)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(sph_filters_type), intent(inout) :: sph_filters
!!      subroutine const_filter_on_sphere                               &
!!     &         (itype_sph_filter,  l_truncation,                      &
!!     &          ref1_moments, ref2_moments, ref1_filter, ref2_filter, &
!!     &          sph_moments, sph_filter)
!!        integer(kind = kint), intent(in) :: itype_sph_filter
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_gaussian_filter), intent(in) :: ref1_filter
!!        type(sph_gaussian_filter), intent(in) :: ref2_filter
!!        type(sph_filter_moment), intent(in) :: ref1_moments
!!        type(sph_filter_moment), intent(in) :: ref2_moments
!!        type(sph_gaussian_filter), intent(inout) :: sph_filter
!!        type(sph_filter_moment), intent(inout) :: sph_moments
!!@endverbatim
!!
!
      module const_filter_on_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_spheric_group
      use t_sph_filtering_data
      use t_sph_filter_moment
!
      implicit none
!
      private :: const_radial_filter
!
! ----------------------------------------------------------------------
!
      contains
!
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
      call const_radial_filter(sph_rj, sph_grps, sph_filters%r_moments, &
     &    sph_filters%kr_SGS_in, sph_filters%kr_SGS_out,                &
     &    sph_filters%r_filter)
!
      end subroutine const_sph_radial_filter
!
! ----------------------------------------------------------------------
!
      subroutine const_filter_on_sphere                                 &
     &         (itype_sph_filter,  l_truncation,                        &
     &          ref1_moments, ref2_moments, ref1_filter, ref2_filter,   &
     &          sph_moments, sph_filter)
!
      use cal_sph_filtering_data
!
      integer(kind = kint), intent(in) :: itype_sph_filter
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_gaussian_filter), intent(in) :: ref1_filter
      type(sph_gaussian_filter), intent(in) :: ref2_filter
      type(sph_filter_moment), intent(in) :: ref1_moments
      type(sph_filter_moment), intent(in) :: ref2_moments
!
      type(sph_gaussian_filter), intent(inout) :: sph_filter
      type(sph_filter_moment), intent(inout) :: sph_moments
!
!
      if(itype_sph_filter .eq. iflag_recursive_filter) then
        sph_moments%num_momentum                                        &
     &      = max(ref1_moments%num_momentum, ref2_moments%num_momentum)
      end if
!
      call alloc_sph_filter_weights(l_truncation, sph_filter)
      call alloc_sph_filter_moms(sph_moments)
!
!
      if(itype_sph_filter .eq. iflag_recursive_filter) then
        if(iflag_debug.gt.0) write(*,*)' set_sph_recursive_filter'
        call set_sph_recursive_filter                                   &
     &     (sph_filter%l_truncation, sph_moments%num_momentum,          &
     &      ref1_moments%num_momentum, ref2_moments%num_momentum,       &
     &      ref1_filter%weight, ref1_moments%filter_mom,                &
     &      ref2_filter%weight, ref2_moments%filter_mom,                &
     &      sph_filter%weight, sph_moments%filter_mom)
!
      else if(itype_sph_filter .eq. iflag_cutoff_filter) then
        if(iflag_debug.gt.0) write(*,*)' set_sph_cutoff_filter'
        call set_sph_cutoff_filter(sph_filter%l_truncation,             &
     &      sph_filter%f_width, sph_filter%weight,                      &
     &      sph_moments%num_momentum, sph_moments%filter_mom)
      else
        if(iflag_debug.gt.0) write(*,*)' set_sph_gaussian_filter'
        call set_sph_gaussian_filter(sph_filter%l_truncation,           &
     &      sph_filter%f_width, sph_filter%weight,                      &
     &      sph_moments%num_momentum, sph_moments%filter_mom)
      end if
!
      end subroutine const_filter_on_sphere
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_radial_filter(sph_rj, sph_grps, r_moments,       &
     &          kmin_rtp_OC, kmax_rtp_OC, r_filter)
!
      use cal_radial_filtering_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_group_data), intent(in) :: sph_grps
      type(sph_filter_moment), intent(in) :: r_moments
!
      integer(kind = kint), intent(inout) :: kmin_rtp_OC, kmax_rtp_OC
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: num_rj_OC, kmin_rj_OC, kmax_rj_OC
      integer(kind = kint) :: num_rtp_OC
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
     &    sph_grps%radial_rj_grp, sph_grps%radial_rtp_grp, r_filter,    &
     &    num_rj_OC, kmin_rj_OC, kmax_rj_OC,                            &
     &    num_rtp_OC, kmin_rtp_OC, kmax_rtp_OC)
!
      call alloc_3d_filter_comb(r_filter)
      call alloc_3d_filter_func(r_filter)
!
      call set_filtering_points(num_rj_OC, kmin_rj_OC, kmax_rj_OC,      &
     &    r_moments%num_momentum, r_moments%nfilter_sides,              &
     &    sph_rj, r_filter)

      call cal_radial_fileters(kmin_rj_OC, kmax_rj_OC,                  &
     &    r_moments%num_momentum, r_moments%nfilter_sides,              &
     &    r_moments%filter_mom, sph_rj, r_filter)
!
      end subroutine const_radial_filter
!
! ----------------------------------------------------------------------
!
      end module const_filter_on_sph
