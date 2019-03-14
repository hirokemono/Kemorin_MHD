!>@file   t_sph_filtering.f90
!!@brief  module t_sph_filtering
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine alloc_sph_filter_type(num_filter, dynamic_SPH)
!!      subroutine dealloc_sph_filter_type(dynamic_SPH)
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!
!!      subroutine init_filter_4_SPH_MHD                                &
!!     &         (sph, sph_grps, leg, num_sph_filteres, sph_filters)
!!        type(sph_grids), intent(in) ::  sph
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(sph_filters_type), intent(inout) :: sph_filters(1)
!!      subroutine init_work_4_SGS_sph_mhd(SGS_par, sph_d_grp,          &
!!     &          MHD_prop, ifld_sgs, icomp_sgs, sgs_coefs, wk_sgs)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(SGS_terms_address), intent(inout) :: ifld_sgs, icomp_sgs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!@endverbatim
!!
!
      module t_sph_filtering
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_parameter
      use t_spheric_group
      use t_sph_filtering_data
      use t_schmidt_poly_on_rtm
      use t_filter_coefficients
      use t_SGS_control_parameter
      use t_SGS_model_coefs
      use t_SGS_buoyancy_sph
      use t_ele_info_4_dynamic
      use t_field_data_IO
      use t_groups_sph_dynamic
!
      implicit none
!
!>      Structure of work area for dyanmic SGS model for spectrum dynamo
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
!>         Work area for dynamic SGS model
        type(work_4_sph_SGS_buoyancy) :: wk_sgs_buo
!
!>         Number of filter functions
        integer(kind = kint) :: num_sph_filteres
!>         Filter functions
        type(sph_filters_type), allocatable :: sph_filters(:)
!
        type(field_IO) :: Csim_S_IO
!        type(field_IO) :: Cdiff_S_IO
!
        type(sph_dynamic_model_group) :: sph_d_grp
      end type dynamic_SGS_data_4_sph
!
      private :: const_sph_radial_filter, const_radial_filter
      private :: const_filter_on_sphere
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sph_filter_type(num_filter, dynamic_SPH)
!
      integer(kind = kint), intent(in) :: num_filter
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      dynamic_SPH%num_sph_filteres = num_filter
      allocate(dynamic_SPH%sph_filters(dynamic_SPH%num_sph_filteres))
!
      end subroutine alloc_sph_filter_type
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_filter_type(dynamic_SPH)
!
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      deallocate(dynamic_SPH%sph_filters)
!
      end subroutine dealloc_sph_filter_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_filter_4_SPH_MHD                                  &
     &         (sph, sph_grps, leg, num_sph_filteres, sph_filters)
!
      use calypso_mpi
      use wider_radial_filter_data
!
      integer(kind = kint), intent(in) :: num_sph_filteres
      type(sph_grids), intent(in) ::  sph
      type(sph_group_data), intent(in) :: sph_grps
      type(legendre_4_sph_trans), intent(in) :: leg
      type(sph_filters_type), intent(inout)                             &
     &                        :: sph_filters(num_sph_filteres)
!
      integer(kind = kint)  :: i, i1, i2
      integer(kind = kint)  :: id_file
!
!
      do i = 1, num_sph_filteres
        i1 = sph_filters(i)%id_1st_ref_filter
        i2 = sph_filters(i)%id_2nd_ref_filter
        if(sph_filters(i)%itype_radial_filter                           &
     &                  .eq. iflag_recursive_filter) then
          call radial_wider_fileters                                    &
     &       (sph%sph_rj, sph_filters(i1)%r_filter,                     &
     &        sph_filters(i2)%r_filter, sph_filters(i)%r_filter)
        else
          if(iflag_debug.gt.0) write(*,*)' const_sph_radial_filter'
          call const_sph_radial_filter                                  &
     &       (sph%sph_rj, sph_grps, sph_filters(i))
        end if
!
        if(iflag_debug.gt.0) write(*,*)' const_filter_on_sphere'
        call const_filter_on_sphere(sph_filters(i)%itype_sph_filter,    &
     &      sph%sph_params%l_truncation,                                &
     &      sph_filters(i1)%sph_moments, sph_filters(i2)%sph_moments,   &
     &      sph_filters(i1)%sph_filter, sph_filters(i2)%sph_filter,     &
     &      sph_filters(i)%sph_moments, sph_filters(i)%sph_filter)
!
        if(iflag_debug.gt.0) write(*,*)' const_filter_on_sphere'
        call init_sph_2nd_order_moments_rtp                             &
     &     (sph%sph_rtp, sph%sph_rj, leg, sph_filters(i))
!
        if(iflag_debug .gt. 0) then
          id_file = 50+my_rank
          write(id_file,*) 'check_radial_filter for no. ', i
          call check_radial_filter                                      &
     &      (id_file, sph%sph_rj, sph_filters(i)%r_filter)
          write(id_file,*) 'check_horiz_filter_weight for no. ', i
          call check_horiz_filter_weight                                &
     &       (id_file, sph_filters(i)%sph_filter)
        end if
        if(i_debug .gt. 0) then
          write(id_file,*) 'check_sph_2nd_moments for no. ',            &
     &         i, my_rank
          call check_sph_2nd_moments                                    &
     &       (id_file, sph%sph_rtp, leg, sph_filters(i))
        end if
        call calypso_mpi_barrier
      end do
!
      end subroutine init_filter_4_SPH_MHD
!
! ----------------------------------------------------------------------
!
      subroutine init_work_4_SGS_sph_mhd(SGS_par, sph_d_grp,            &
     &          MHD_prop, ifld_sgs, icomp_sgs, sgs_coefs, wk_sgs)
!
      use t_physical_property
      use count_sgs_components
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(SGS_terms_address), intent(inout) :: ifld_sgs, icomp_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
!
      call s_count_sgs_components(SGS_par%model_p,                      &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, sgs_coefs)
      call alloc_sgs_coefs_layer(sph_d_grp%ngrp_dynamic,                &
     &    sgs_coefs%num_field, sgs_coefs%ntot_comp, wk_sgs)
!
      call alloc_SGS_num_coefs(sgs_coefs)
!
      call set_sgs_addresses                                            &
     &   (SGS_par%model_p, MHD_prop%fl_prop, MHD_prop%cd_prop,          &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    ifld_sgs, icomp_sgs, wk_sgs, sgs_coefs)
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
      type(sph_gaussian_filter), intent(in) :: ref1_filter, ref2_filter
      type(sph_filter_moment), intent(in) :: ref1_moments, ref2_moments
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
      end module t_sph_filtering
