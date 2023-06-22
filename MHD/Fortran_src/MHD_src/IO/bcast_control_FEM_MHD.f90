!>@file   bcast_control_FEM_MHD.f90
!!@brief  module bcast_control_FEM_MHD
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
!!      subroutine bcast_fem_mhd_ctl_data(FEM_MHD_ctl)
!!        type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
!!@endverbatim
!
      module bcast_control_FEM_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_FEM_MHD
!
      implicit none
!
      private :: bcast_fem_mhd_control_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine bcast_fem_mhd_ctl_data(FEM_MHD_ctl)
!
      use transfer_to_long_integers
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
      use bcast_ctl_MHD_model
      use bcast_monitor_data_ctl
!
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
!
!
      call bcast_ctl_data_4_platform(FEM_MHD_ctl%plt)
      call bcast_ctl_data_4_platform(FEM_MHD_ctl%org_plt)
!
      call bcast_ctl_data_mhd_model(FEM_MHD_ctl%model_ctl)
      call bcast_fem_mhd_control_ctl(FEM_MHD_ctl%fmctl_ctl)
!
      call bcast_node_monitor_data_ctl(FEM_MHD_ctl%nmtr_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (FEM_MHD_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(FEM_MHD_ctl%i_mhd_ctl, 0)
!
      end subroutine bcast_fem_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_fem_mhd_control_ctl(fmctl_ctl)
!
      use calypso_mpi_int
      use bcast_4_time_step_ctl
      use bcast_4_solver_ctl
      use bcast_4_fem_int_pts_ctl
      use bcast_ctl_data_mhd_time_rst
!
      type(fem_mhd_control_control), intent(inout) :: fmctl_ctl
!
!
      call bcast_restart_ctl(fmctl_ctl%mrst_ctl)
      call bcast_time_loop_ctl(fmctl_ctl%mevo_ctl)
      call bcast_ctl_data_4_time_step(fmctl_ctl%tctl)
!
      call bcast_CG_solver_param_ctl(fmctl_ctl%CG_ctl)
      call bcast_control_fem_int_points(fmctl_ctl%fint_ctl)
!
      call calypso_mpi_bcast_one_int(fmctl_ctl%i_control, 0)
!
      end subroutine bcast_fem_mhd_control_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_control_FEM_MHD
