!>@file   bcast_ctl_SGS_MHD_model.f90
!!@brief  module bcast_ctl_SGS_MHD_model
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine bcast_sgs_ctl(sgs_ctl)
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!@endverbatim
!
      module bcast_ctl_SGS_MHD_model
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
      private :: bcast_3d_filtering_ctl, bcast_control_4_SGS_filter
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine bcast_sgs_ctl(sgs_ctl)
!
      use t_ctl_data_SGS_model
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_4_filter_files_ctl
      use bcast_control_arrays
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
      integer(kind = kint) :: i
!
!
      call bcast_3d_filtering_ctl(sgs_ctl%s3df_ctl)
      call bcast_filter_fnames_control(sgs_ctl%ffile_ctl)
      call bcast_ele_layers_control(sgs_ctl%elayer_ctl)
!
!
      call calypso_mpi_bcast_one_int(sgs_ctl%num_sph_filter_ctl, 0)
      if(my_rank .gt. 0 .and. sgs_ctl%num_sph_filter_ctl .gt. 0) then
        call alloc_sph_filter_ctl(sgs_ctl)
      end if
      call calypso_mpi_barrier
!
      do i = 1, sgs_ctl%num_sph_filter_ctl
        call bcast_control_4_SGS_filter(sgs_ctl%sph_filter_ctl(i))
      end do
!
      call bcast_ctl_array_c1(sgs_ctl%SGS_terms_ctl)
      call bcast_ctl_array_c1(sgs_ctl%commutate_fld_ctl)
!
      call bcast_ctl_type_c1(sgs_ctl%SGS_model_name_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_filter_name_ctl)
      call bcast_ctl_type_c1(sgs_ctl%DIFF_model_coef_ctl)
!
      call bcast_ctl_type_c1(sgs_ctl%SGS_negative_clip_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_marging_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_perturbation_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_model_coef_type_ctl)
!
      call bcast_ctl_type_c1(sgs_ctl%heat_flux_csim_type_ctl)
      call bcast_ctl_type_c1(sgs_ctl%comp_flux_csim_type_ctl)
      call bcast_ctl_type_c1(sgs_ctl%mom_flux_csim_type_ctl)
      call bcast_ctl_type_c1(sgs_ctl%maxwell_csim_type_ctl)
      call bcast_ctl_type_c1(sgs_ctl%uxb_csim_type_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_model_coef_coord_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_buo_Csim_usage_ctl)
!
!
      call bcast_ctl_type_r1(sgs_ctl%delta_to_shrink_dynamic_ctl)
      call bcast_ctl_type_r1(sgs_ctl%clipping_limit_ctl)
!
      call bcast_ctl_type_r1(sgs_ctl%SGS_hf_factor_ctl)
      call bcast_ctl_type_r1(sgs_ctl%SGS_cf_factor_ctl)
      call bcast_ctl_type_r1(sgs_ctl%SGS_mf_factor_ctl)
      call bcast_ctl_type_r1(sgs_ctl%SGS_mxwl_factor_ctl)
      call bcast_ctl_type_r1(sgs_ctl%SGS_uxb_factor_ctl)
!
      call bcast_ctl_type_r1(sgs_ctl%delta_to_extend_dynamic_ctl)
      call bcast_ctl_type_r1(sgs_ctl%stabilize_weight_ctl)
!
      call bcast_ctl_type_i1(sgs_ctl%istep_dynamic_ctl)
      call bcast_ctl_type_i1(sgs_ctl%min_step_dynamic_ctl)
      call bcast_ctl_type_i1(sgs_ctl%max_step_dynamic_ctl)
!
      call bcast_ctl_type_i1(sgs_ctl%ngrp_radial_ave_ctl)
      call bcast_ctl_type_i1(sgs_ctl%ngrp_med_ave_ctl)
!
      call calypso_mpi_bcast_character(sgs_ctl%block_name,              &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(sgs_ctl%i_sgs_ctl, 0)
!
      end subroutine bcast_sgs_ctl
!
! -----------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_3d_filtering_ctl(s3df_ctl)
!
      use t_ctl_SGS_3d_filter
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!
!
      call bcast_ctl_array_c1(s3df_ctl%whole_filter_grp_ctl)
      call bcast_ctl_array_c1(s3df_ctl%fluid_filter_grp_ctl)
!
      call bcast_ctl_type_c1(s3df_ctl%momentum_filter_ctl)
      call bcast_ctl_type_c1(s3df_ctl%heat_filter_ctl)
      call bcast_ctl_type_c1(s3df_ctl%induction_filter_ctl)
      call bcast_ctl_type_c1(s3df_ctl%compostion_filter_ctl)
!
      call calypso_mpi_bcast_character(s3df_ctl%block_name,             &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(s3df_ctl%i_SGS_3d_filter_ctl, 0)
!
      end subroutine bcast_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_control_4_SGS_filter(sphf_ctl)
!
      use t_ctl_data_SGS_filter
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
!
!
      call bcast_ctl_type_c1(sphf_ctl%sph_filter_type_ctl)
      call bcast_ctl_type_c1(sphf_ctl%radial_filter_type_ctl)
      call bcast_ctl_type_i1(sphf_ctl%maximum_moments_ctl)
!
      call bcast_ctl_type_r1(sphf_ctl%radial_filter_width_ctl)
      call bcast_ctl_type_r1(sphf_ctl%sphere_filter_width_ctl)
!
      call bcast_ctl_type_i1(sphf_ctl%first_reference_ctl)
      call bcast_ctl_type_i1(sphf_ctl%second_reference_ctl)
!
      call calypso_mpi_bcast_character(sphf_ctl%block_name,             &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(sphf_ctl%i_sph_filter_ctl, 0)
!
      end subroutine bcast_control_4_SGS_filter
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_SGS_MHD_model
