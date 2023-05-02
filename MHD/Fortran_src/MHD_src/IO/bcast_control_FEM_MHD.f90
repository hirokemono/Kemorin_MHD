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
!!      subroutine load_control_4_fem_MHD                               &
!!     &         (file_name, FEM_MHD_ctl, viz_ctls)
!!        type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
!!        type(visualization_controls), intent(inout) :: viz_ctls
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
      private :: bcast_fem_mhd_ctl_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_control_4_fem_MHD                                 &
     &         (file_name, FEM_MHD_ctl, viz_ctls)
!
      use t_ctl_data_FEM_MHD
!
      character(len=kchara), intent(in) :: file_name
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      if(my_rank .eq. 0) then
        call read_control_4_fem_MHD(file_name, FEM_MHD_ctl, viz_ctls)
      end if
!
      call bcast_fem_mhd_ctl_data(FEM_MHD_ctl, viz_ctls)
!
      end subroutine load_control_4_fem_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine bcast_fem_mhd_ctl_data(FEM_MHD_ctl, viz_ctls)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
!
      type(fem_mhd_control), intent(inout) :: FEM_MHD_ctl
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      call bcast_ctl_data_4_platform(FEM_MHD_ctl%plt)
      call bcast_ctl_data_4_platform(FEM_MHD_ctl%org_plt)
!
      call bcast_sph_sgs_mhd_model(FEM_MHD_ctl%model_ctl)
      call bcast_fem_mhd_control(FEM_MHD_ctl%fmctl_ctl)
!
      call bcast_monitor_data_ctl(FEM_MHD_ctl%nmtr_ctl)
!
      call bcast_viz_controls(viz_ctls)
!
      call calypso_mpi_bcast_one_int(FEM_MHD_ctl%i_mhd_ctl, 0)
!
      end subroutine bcast_fem_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      end module bcast_control_FEM_MHD
