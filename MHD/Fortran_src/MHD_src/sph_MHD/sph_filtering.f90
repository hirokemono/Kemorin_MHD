!>@file   sph_filtering.f90
!!@brief  module sph_filtering
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine init_SGS_model_sph_mhd(SGS_par, sph, sph_grps,       &
!!     &          MHD_prop, dynamic_SPH)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_grids), intent(in) ::  sph
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
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
      use t_SGS_control_parameter
      use t_control_parameter
      use t_physical_property
      use t_spheric_parameter
      use t_spheric_group
      use t_phys_data
      use t_sph_filtering_data
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_sph_filtering
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_SGS_model_sph_mhd(SGS_par, sph, sph_grps,         &
     &          MHD_prop, dynamic_SPH)
!
      use calypso_mpi
      use t_physical_property
      use t_SGS_buoyancy_sph
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) ::  sph
      type(sph_group_data), intent(in) :: sph_grps
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      if(SGS_par%model_p%iflag_SGS .eq. 0) return
!
      call init_filter_4_SPH_MHD(sph%sph_params, sph%sph_rj, sph_grps,  &
     &    dynamic_SPH%num_sph_filteres, dynamic_SPH%sph_filters)
!
      call init_work_4_SGS_sph_mhd(SGS_par, sph%sph_rtp, MHD_prop,      &
     &    dynamic_SPH%ifld_sgs, dynamic_SPH%icomp_sgs,                  &
     &    dynamic_SPH%sgs_coefs, dynamic_SPH%wk_sgs)
!
      call alloc_sph_ave_Csim_SGS_buo                                   &
     &   (sph%sph_rj, sph%sph_rtp, dynamic_SPH%wk_SGS_buo)
      call cal_volume_4__SGS_buoyancy                                   &
     &   (sph%sph_params, sph%sph_rj, dynamic_SPH%wk_sgs_buo)
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
