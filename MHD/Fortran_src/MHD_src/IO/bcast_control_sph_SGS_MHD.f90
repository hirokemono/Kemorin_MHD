!>@file   bcast_control_sph_SGS_MHD.f90
!!@brief  module bcast_control_sph_SGS_MHD
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!@n        Modified by H. Matsui on Apr., 2023
!!
!!@verbatim
!!      subroutine s_load_control_sph_SGS_MHD(file_name, MHD_ctl,       &
!!     &                                      add_SSMHD_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(add_sgs_sph_mhd_ctl), intent(inout) :: add_SSMHD_ctl
!!@endverbatim
!
      module bcast_control_sph_SGS_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_control_data_vizs
      use t_control_data_dynamo_vizs
!
      implicit none
!
      private :: bcast_sph_mhd_control_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_load_control_sph_SGS_MHD(file_name, MHD_ctl,         &
     &                                      add_SSMHD_ctl)
!
      use ctl_file_SGS_MHD_IO
      use bcast_ctl_SGS_MHD_model
      use bcast_ctl_sph_mhd_control
      use bcast_control_data_vizs
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(add_sgs_sph_mhd_ctl), intent(inout) :: add_SSMHD_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_4_sph_SGS_MHD                                 &
     &     (file_name, MHD_ctl, add_SSMHD_ctl)
      end if
!
      call bcast_sph_mhd_control_data(MHD_ctl)
      call bcast_sgs_ctl(add_SSMHD_ctl%sgs_ctl)
      call bcast_viz_controls(add_SSMHD_ctl%viz_ctls)
      call bcast_dynamo_viz_control(add_SSMHD_ctl%zm_ctls)
!
      end subroutine s_load_control_sph_SGS_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_control_data(MHD_ctl)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
      use bcast_ctl_MHD_model
      use bcast_monitor_data_ctl
      use bcast_ctl_sph_mhd_control
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
!
!
      call bcast_ctl_data_4_platform(MHD_ctl%plt)
      call bcast_ctl_data_4_platform(MHD_ctl%org_plt)
      call bcast_ctl_data_4_platform(MHD_ctl%new_plt)
!
      call bcast_parallel_shell_ctl(MHD_ctl%psph_ctl)
!
      call bcast_ctl_data_MHD_model(MHD_ctl%model_ctl)
      call bcast_sph_mhd_control(MHD_ctl%smctl_ctl)
!
      call bcast_node_monitor_data_ctl(MHD_ctl%nmtr_ctl)
      call bcast_sph_monitoring_ctl(MHD_ctl%smonitor_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (MHD_ctl%fname_psph_ctl, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(MHD_ctl%i_mhd_ctl, 0)
!
      end subroutine bcast_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!
      end module bcast_control_sph_SGS_MHD
