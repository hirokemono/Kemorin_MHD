!>@file   bcast_control_sph_MHD.f90
!!@brief  module bcast_control_sph_MHD
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
!!      subroutine load_control_4_sph_MHD_w_psf(file_name, MHD_ctl,     &
!!     &                                        surfacing_ctls, zm_ctls)
!!      subroutine load_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(DNS_mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(surfacing_controls), intent(inout) :: surfacing_ctls
!!        type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!!@endverbatim
!
      module bcast_control_sph_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_MHD
!
      implicit none
!
      private :: bcast_sph_mhd_ctl_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_control_4_sph_MHD_w_psf(file_name, MHD_ctl,       &
     &                                        surfacing_ctls, zm_ctls)
!
      use t_control_data_surfacings
      use t_control_data_dynamo_vizs
      use ctl_file_sph_MHD_IO
      use bcast_ctl_sph_mhd_control
      use bcast_ctl_data_surfacings
!
      character(len=kchara), intent(in) :: file_name
      type(DNS_mhd_simulation_control), intent(inout) :: MHD_ctl
      type(surfacing_controls), intent(inout) :: surfacing_ctls
      type(sph_dynamo_viz_controls), intent(inout) :: zm_ctls
!
!
      if(my_rank .eq. 0) then
        call read_control_4_sph_MHD_w_psf(file_name, MHD_ctl,          &
     &                                    surfacing_ctls, zm_ctls)
      end if
!
      call bcast_sph_mhd_ctl_data(MHD_ctl)
      call bcast_surfacing_controls(surfacing_ctls)
      call bcast_dynamo_viz_control(zm_ctls)
!
      end subroutine load_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine load_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!
      use ctl_file_sph_MHD_IO
!
      character(len=kchara), intent(in) :: file_name
      type(DNS_mhd_simulation_control), intent(inout) :: MHD_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_4_sph_MHD_noviz(file_name, MHD_ctl)
      end if
!
      call bcast_sph_mhd_ctl_data(MHD_ctl)
!
      end subroutine load_control_4_sph_MHD_noviz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_ctl_data(MHD_ctl)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
      use bcast_ctl_MHD_model
      use bcast_ctl_sph_mhd_control
      use bcast_monitor_data_ctl
!
      type(DNS_mhd_simulation_control), intent(inout) :: MHD_ctl
!
!
      call bcast_ctl_data_4_platform(MHD_ctl%plt)
      call bcast_ctl_data_4_platform(MHD_ctl%org_plt)
!
      call bcast_parallel_shell_ctl(MHD_ctl%psph_ctl)
!
      call bcast_ctl_data_mhd_model(MHD_ctl%model_ctl)
      call bcast_sph_mhd_control(MHD_ctl%smctl_ctl)
!
      call bcast_node_monitor_data_ctl(MHD_ctl%nmtr_ctl)
      call bcast_sph_monitoring_ctl(MHD_ctl%smonitor_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (MHD_ctl%fname_psph_ctl, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(MHD_ctl%i_mhd_ctl, 0)
!
      end subroutine bcast_sph_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      end module bcast_control_sph_MHD
