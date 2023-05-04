!>@file   bcast_ctl_sph_mhd_control.f90
!!@brief  module bcast_ctl_sph_mhd_control
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine bcast_sph_mhd_control(smctl_ctl)
!!        type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!!      subroutine bcast_dynamo_viz_control(zm_ctls)
!!        type(sph_dynamo_viz_controls), intent(in) :: zm_ctls
!!      subroutine bcast_crustal_filtering_ctl(crust_filter_c)
!!        type(clust_filtering_ctl), intent(inout) :: crust_filter_c
!!!@endverbatim
!
      module bcast_ctl_sph_mhd_control
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_crustal_filtering_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_control(smctl_ctl)
!
      use t_ctl_data_SPH_MHD_control
      use calypso_mpi_int
      use bcast_4_time_step_ctl
      use bcast_ctl_data_mhd_time_rst
!
      type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!
!
      call bcast_restart_ctl(smctl_ctl%mrst_ctl)
      call bcast_time_loop_ctl(smctl_ctl%mevo_ctl)
      call bcast_ctl_data_4_time_step(smctl_ctl%tctl)
!
      call calypso_mpi_bcast_one_int(smctl_ctl%i_control, 0)
!
      end subroutine bcast_sph_mhd_control
!
!   --------------------------------------------------------------------
!
      subroutine bcast_dynamo_viz_control(zm_ctls)
!
      use t_control_data_dynamo_vizs
      use bcast_section_control_data
!
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!
!
      call bcast_files_4_psf_ctl(zm_ctls%zm_psf_ctls)
      call bcast_files_4_psf_ctl(zm_ctls%zRMS_psf_ctls)
      call bcast_crustal_filtering_ctl(zm_ctls%crust_filter_ctl)
!
      end subroutine bcast_dynamo_viz_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_crustal_filtering_ctl(crust_filter_c)
!
      use t_ctl_data_crust_filter
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(clust_filtering_ctl), intent(inout) :: crust_filter_c
!
!
      call bcast_ctl_type_i1(crust_filter_c%crust_truncation_ctl)
      call calypso_mpi_bcast_one_int                                    &
     &   (crust_filter_c%i_crustal_filtering, 0)
!
      end subroutine bcast_crustal_filtering_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_sph_mhd_control
