!>@file   bcast_ctl_data_4_sph_trans.f90
!!@brief  module bcast_ctl_data_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine bcast_sph_trans_control_data(spt_ctl)
!!        type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!!@endverbatim
      module bcast_ctl_data_4_sph_trans
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_4_sph_trans
!
      implicit  none
!
      character (len = kchara), parameter, private                      &
     &                :: fname_sph_trns_ctl = 'ctl_sph_transform'
!
      private :: bcast_sph_trans_model_ctl, bcast_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_trans_control_data(spt_ctl)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
      use bcast_4_platform_ctl
      use bcast_control_data_vizs
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      call bcast_sph_trans_model_ctl(spt_ctl)
      call bcast_sph_trans_params_ctl(spt_ctl)
!
      call bcast_ctl_data_4_platform(spt_ctl%plt)
      call bcast_ctl_data_4_platform(spt_ctl%org_plt)
      call bcast_FEM_mesh_control(spt_ctl%Fmesh_ctl)
      call bcast_viz_controls(spt_ctl%viz_ctls)
!
      call calypso_mpi_bcast_one_int(spt_ctl%i_sph_trans_ctl, 0)
      call calypso_mpi_bcast_character                                  &
     &   (spt_ctl%fname_psph, cast_long(kchara), 0)
!
      end subroutine bcast_sph_trans_control_data
!
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_trans_model_ctl(spt_ctl)
!
      use calypso_mpi_int
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      call bcast_phys_data_ctl(spt_ctl%fld_ctl)
      call bcast_ctl_data_4_time_step(spt_ctl%t_ctl)
!
      call calypso_mpi_bcast_one_int(spt_ctl%i_sph_trans_model, 0)
!
      end subroutine bcast_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_sph_trans_params_ctl(spt_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      call bcast_ctl_type_i1(spt_ctl%legendre_vector_len_ctl)
!
      call bcast_ctl_type_c1(spt_ctl%zm_spec_file_head_ctl)
      call bcast_ctl_type_c1(spt_ctl%zonal_udt_head_ctl)
      call bcast_ctl_type_c1(spt_ctl%cmb_radial_grp_ctl)
      call bcast_ctl_type_c1(spt_ctl%icb_radial_grp_ctl)
!
      call bcast_ctl_type_c1(spt_ctl%Legendre_trans_loop_ctl)
      call bcast_ctl_type_c1(spt_ctl%FFT_lib_ctl)
      call bcast_ctl_type_c1(spt_ctl%import_mode_ctl)
!
      call bcast_ctl_type_c1(spt_ctl%gauss_sph_fhead_ctl)
!
      call calypso_mpi_bcast_one_int(spt_ctl%i_sph_trans_params, 0)
!
      end subroutine bcast_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      end module bcast_ctl_data_4_sph_trans
