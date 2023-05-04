!>@file   bcast_control_rayleigh_vizs.f90
!!@brief  module bcast_control_rayleigh_vizs
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2019
!
!>@brief  Main loop of visualization of Rayleigh data
!!
!!@verbatim
!!      subroutine load_ctl_file_rayleigh_viz(rayleigh_vizs_ctl)
!!        type(control_data_rayleigh_vizs), intent(inout)               &
!!     &                     :: vizs_ctlrayleigh_vizs_ctl
!!@endverbatim
!
!
      module bcast_control_rayleigh_vizs
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_rayleigh_vizs
      use calypso_mpi
!
      implicit  none
!
      character(len = kchara), parameter, private                       &
     &               :: fname_viz_ctl = "control_viz_rayleigh"
!
      private bcast_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_ctl_file_rayleigh_viz(rayleigh_vizs_ctl)
!
      type(control_data_rayleigh_vizs), intent(inout)                   &
     &                         :: rayleigh_vizs_ctl
!
!
      if(my_rank .eq. 0) then
        call read_ctl_file_rayleigh_viz(fname_viz_ctl,                  &
     &      rayleigh_vizs_ctl)
      end if
!
      call bcast_rayleigh_vizs_ctl_data(rayleigh_vizs_ctl)
!
      end subroutine load_ctl_file_rayleigh_viz
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_rayleigh_vizs_ctl_data(rayleigh_vizs_ctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_control_data_vizs
      use bcast_4_field_ctl
      use bcast_4_sphere_ctl
!
      type(control_data_rayleigh_vizs), intent(inout)                   &
     &                                 :: rayleigh_vizs_ctl
!
!
      call bcast_ctl_data_4_platform(rayleigh_vizs_ctl%viz_plt)
      call bcast_ctl_data_4_time_step(rayleigh_vizs_ctl%t_viz_ctl)
      call bcast_viz_controls(rayleigh_vizs_ctl%viz_ctl_v)
!
      call bcast_phys_data_ctl(rayleigh_vizs_ctl%fld_ctl)
      call bcast_ctl_ndomain_4_shell(rayleigh_vizs_ctl%sdctl)
!
      call calypso_mpi_bcast_one_int                                    &
     &   (rayleigh_vizs_ctl%i_viz_only_file, 0)
!
      end subroutine bcast_rayleigh_vizs_ctl_data
!
!   --------------------------------------------------------------------
!
      end module bcast_control_rayleigh_vizs
