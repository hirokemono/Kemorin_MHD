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
!!      subroutine init_work_4_SGS_sph_mhd(SGS_par, sph, MHD_prop,      &
!!     &                                   dynamic_SPH)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
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
      use t_SGS_buoyancy_sph
      use t_ele_info_4_dynamic
      use t_field_data_IO
      use t_groups_sph_dynamic
      use t_SPH_dynamic_model_coefs
!
      implicit none
!
!>      Structure of work area for dyanmic SGS model for spectrum dynamo
      type dynamic_SGS_data_4_sph
!>         Field adddress for dynamic SGS model
        type(SGS_term_address) :: iak_sgs_term
!>         Work area for dynamic SGS model
        type(SPH_dynamic_model_coefs) :: wk_sph_sgs
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
      character(len=kchara), parameter :: filter_head = 'radial_filter'
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
      use set_parallel_file_name
      use const_filter_on_sph
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
      character(len=kchara) :: fname_tmp, file_name
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
          fname_tmp = add_int_suffix(i, filter_head)
          file_name = add_dat_extension(fname_tmp)
          call check_radial_filter                                      &
     &       (file_name, sph%sph_rj, sph_filters(i)%r_filter)
!
          id_file = 50+my_rank
          write(id_file,*) 'check_horiz_filter_weight for no. ', i
          call check_horiz_filter_weight                                &
     &       (id_file, sph_filters(i)%sph_filter)
        end if
        if(i_debug .gt. 0) then
          id_file = 50+my_rank
          write(id_file,*) 'check_sph_2nd_moments for no. ',            &
     &         i, my_rank
          call check_sph_2nd_moments                                    &
     &       (id_file, sph%sph_rtp, leg, sph_filters(i))
        end if
      end do
!
      end subroutine init_filter_4_SPH_MHD
!
! ----------------------------------------------------------------------
!
      subroutine init_work_4_SGS_sph_mhd(SGS_par, sph, MHD_prop,        &
     &                                   dynamic_SPH)
!
      use t_physical_property
      use count_sgs_components
      use set_groups_sph_dynamic
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) ::  sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
      integer(kind = kint) :: num_SGS_terms, ntot_SGS_comps
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
        if(iflag_debug.gt.0) write(*,*) 'find_grouping_4_dynamic_model'
        call find_grouping_4_dynamic_model(SGS_par%model_p,             &
     &      sph%sph_params, sph%sph_rtp, dynamic_SPH%sph_d_grp)
      end if
!
      call s_count_sgs_components(SGS_par%model_p,                      &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    num_SGS_terms, ntot_SGS_comps)
      call alloc_sph_sgs_coefs_layer                                    &
     &   (dynamic_SPH%sph_d_grp%ngrp_dynamic,                           &
     &    num_SGS_terms, dynamic_SPH%wk_sph_sgs)
!
      call set_sph_sgs_addresses                                        &
     &   (SGS_par%model_p, MHD_prop%fl_prop, MHD_prop%cd_prop,          &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    dynamic_SPH%iak_sgs_term, dynamic_SPH%wk_sph_sgs)
      call check_sph_sgs_addresses(dynamic_SPH%iak_sgs_term,            &
     &                             dynamic_SPH%wk_sph_sgs)
!
      end subroutine init_work_4_SGS_sph_mhd
!
! ----------------------------------------------------------------------
!
      end module t_sph_filtering
