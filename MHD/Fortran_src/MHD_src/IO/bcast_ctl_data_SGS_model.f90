!>@file   bcast_ctl_data_SGS_model.f90
!!@brief  module bcast_ctl_data_SGS_model
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for SGS model controls
!!
!!@verbatim
!!      subroutine bcast_sgs_ctl(sgs_ctl)
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!
!!      subroutine bcast_3d_filtering_ctl(s3df_ctl)
!!        type(SGS_3d_filter_control), intent(inout) :: s3df_ctl
!!      subroutine bcast_control_4_SGS_filter(sphf_ctl)
!!        type(sph_filter_ctl_type), intent(inout) :: sphf_ctl
!!@endverbatim
!
      module bcast_ctl_data_SGS_model
!
      use m_precision
!
      use calypso_mpi
      use t_ctl_data_SGS_model
      use t_ctl_data_SGS_filter
!
      implicit  none
!
      private :: bcast_3d_filtering_ctl, bcast_control_4_SGS_filter
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sgs_ctl(sgs_ctl)
!
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
        call bcast_ctl_type_c1                                          &
     &     (sgs_ctl%sph_filter_ctl(i)%sph_filter_type_ctl)
        call bcast_ctl_type_c1                                          &
     &     (sgs_ctl%sph_filter_ctl(i)%radial_filter_type_ctl)
        call bcast_ctl_type_i1                                          &
     &     (sgs_ctl%sph_filter_ctl(i)%maximum_moments_ctl)
!
        call bcast_ctl_type_r1                                          &
     &     (sgs_ctl%sph_filter_ctl(i)%radial_filter_width_ctl)
        call bcast_ctl_type_r1                                          &
     &     (sgs_ctl%sph_filter_ctl(i)%sphere_filter_width_ctl)
!
        call bcast_ctl_type_i1                                          &
     &     (sgs_ctl%sph_filter_ctl(i)%first_reference_ctl)
        call bcast_ctl_type_i1                                          &
     &     (sgs_ctl%sph_filter_ctl(i)%second_reference_ctl)
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
      call calypso_mpi_bcast_one_int(sgs_ctl%i_sgs_ctl, 0)
!
!      write(*,*) my_rank, 'num_sph_filter_ctl',                        &
!     &     sgs_ctl%num_sph_filter_ctl
!      do i = 1, sgs_ctl%num_sph_filter_ctl
!        write(*,*) my_rank, 'sph_filter_type_ctl',  i,                 &
!     &     sgs_ctl%sph_filter_ctl(i)%sph_filter_type_ctl%charavalue
!        write(*,*) my_rank, 'radial_filter_type_ctl', i,               &
!     &     sgs_ctl%sph_filter_ctl(i)%radial_filter_type_ctl%charavalue
!        write(*,*) my_rank, 'maximum_moments_ctl', i,                  &
!     &     sgs_ctl%sph_filter_ctl(i)%maximum_moments_ctl%intvalue
!        write(*,*) my_rank, 'sphere_filter_width_ctl', i,              &
!     &     sgs_ctl%sph_filter_ctl(i)%sphere_filter_width_ctl%realvalue
!        write(*,*) my_rank, 'radial_filter_width_ctl', i,              &
!     &     sgs_ctl%sph_filter_ctl(i)%radial_filter_width_ctl%realvalue
!        write(*,*) my_rank, 'first_reference_ctl', i,                  &
!     &     sgs_ctl%sph_filter_ctl(i)%first_reference_ctl%intvalue
!        write(*,*) my_rank, 'second_reference_ctl', i,                 &
!     &     sgs_ctl%sph_filter_ctl(i)%second_reference_ctl%intvalue
!      end do
!
      end subroutine bcast_sgs_ctl
!
! -----------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_3d_filtering_ctl(s3df_ctl)
!
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
      end subroutine bcast_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_control_4_SGS_filter(sphf_ctl)
!
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
        write(*,*) my_rank, 'sphf_ctl%sph_filter_type_ctl',  &
     &     sphf_ctl%sph_filter_type_ctl%charavalue
        write(*,*) my_rank, 'sphf_ctl%radial_filter_type_ctl',   &
     &     sphf_ctl%radial_filter_type_ctl%charavalue
        write(*,*) my_rank, 'sphf_ctl%maximum_moments_ctl',   &
     &     sphf_ctl%maximum_moments_ctl%intvalue
        write(*,*) my_rank, 'sphf_ctl%sphere_filter_width_ctl',   &
     &     sphf_ctl%sphere_filter_width_ctl%realvalue
        write(*,*) my_rank, 'sphf_ctl%radial_filter_width_ctl',   &
     &     sphf_ctl%radial_filter_width_ctl%realvalue
        write(*,*) my_rank, 'sphf_ctl%first_reference_ctl',   &
     &     sphf_ctl%first_reference_ctl%intvalue
        write(*,*) my_rank, 'sphf_ctl%second_reference_ctl',   &
     &     sphf_ctl%second_reference_ctl%intvalue
!
      end subroutine bcast_control_4_SGS_filter
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_SGS_model
