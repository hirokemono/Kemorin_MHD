!>@file   sph_filtering.f90
!!@brief  module sph_filtering
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine init_SGS_model_sph_mhd(sph_params, sph_rj, sph_grps, &
!!     &          r_filters, sph_filters)
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
!
      use t_spheric_rj_data
      use t_phys_data
      use t_radial_filtering_data
      use t_sph_filtering_data
!
      type sph_filter_type
        type(radial_filters_type) :: radial
        type(sph_gaussian_filter) :: sph
      end type sph_filter_type
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_SGS_model_sph_mhd(sph_params, sph_rj, sph_grps,   &
     &          r_filters, sph_filters)
!
      use calypso_mpi
      use wider_radial_filter_data
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_group_data), intent(in) :: sph_grps
      type(radial_filters_type), intent(inout) :: r_filters
      type(sph_gaussian_filters), intent(inout) :: sph_filters
!
!
      call alloc_radial_filter_moms(r_filters)
      call cal_radial_moments(r_filters%num_filter_moments,             &
     &    r_filters%nfilter_sides, r_filters%filter_mom)
      call const_radial_filter(sph_rj, sph_grps, r_filters)
!
      call cal_wider_fileters(sph_rj, r_filters%r_filter,               &
     &    r_filters%wide_filter, r_filters%wide2_filter)
!
!      if(iflag_debug .gt. 0) then
!        write(*,*) 'check_radial_filter r_filter'
!        call check_radial_filter(sph_rj, r_filters%r_filter)
!        write(*,*) 'check_radial_filter wide_filter'
!        call check_radial_filter(sph_rj, r_filters%wide_filter)
!        write(*,*) 'check_radial_filter wide2_filter'
!        call check_radial_filter(sph_rj, r_filters%wide2_filter)
!      end if
!
      call const_sph_gaussian_filter                                    &
     &   (sph_params%l_truncation, sph_filters%sph_filter)
      call const_sph_gaussian_filter                                    &
     &   (sph_params%l_truncation, sph_filters%sph_wide_filter)
      call const_sph_gaussian_filter                                    &
     &   (sph_params%l_truncation, sph_filters%sph_wider_filter)
      call calypso_mpi_barrier
!
      end subroutine init_SGS_model_sph_mhd
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
      call vector_sph_horiz_filter                                      &
     &   (sph_filter%l_truncation, sph_filter%weight,                   &
     &    sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j, i_field, i_filter,     &
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
      call scalar_sph_horiz_filter                                      &
     &   (sph_filter%l_truncation, sph_filter%weight,                   &
     &    sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j, i_field, i_filter,     &
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
      call sym_tensor_sph_horiz_filter                                  &
     &   (sph_filter%l_truncation, sph_filter%weight,                   &
     &    sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j, i_field, i_filter,     &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine sym_tensor_sph_filter
!
! ----------------------------------------------------------------------
!
      end module sph_filtering
