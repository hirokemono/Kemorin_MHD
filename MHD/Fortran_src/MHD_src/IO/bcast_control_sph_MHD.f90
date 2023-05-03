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
!!      subroutine load_control_4_sph_MHD_w_psf(file_name, DMHD_ctl)
!!      subroutine load_control_4_sph_MHD_noviz(file_name, DMHD_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
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
      private :: bcast_sph_mhd_ctl_w_psf, bcast_sph_mhd_ctl_data
      private :: bcast_sph_mhd_model
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_control_4_sph_MHD_w_psf(file_name, DMHD_ctl)
!
      use ctl_file_sph_MHD_IO
!
      character(len=kchara), intent(in) :: file_name
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_4_sph_MHD_w_psf(file_name, DMHD_ctl)
      end if
!
      call bcast_sph_mhd_ctl_w_psf(DMHD_ctl)
!
      end subroutine load_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine load_control_4_sph_MHD_noviz(file_name, DMHD_ctl)
!
      use ctl_file_sph_MHD_IO
!
      character(len=kchara), intent(in) :: file_name
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_4_sph_MHD_noviz(file_name, DMHD_ctl)
      end if
!
      call bcast_sph_mhd_ctl_data(DMHD_ctl)
!
      end subroutine load_control_4_sph_MHD_noviz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_ctl_w_psf(DMHD_ctl)
!
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
!
      call bcast_sph_mhd_ctl_data(DMHD_ctl)
      call bcast_surfacing_controls(DMHD_ctl%surfacing_ctls)
      call bcast_dynamo_viz_control(DMHD_ctl%zm_ctls)
!
      end subroutine bcast_sph_mhd_ctl_w_psf
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_ctl_data(DMHD_ctl)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
      use bcast_monitor_data_ctl
!
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
!
      call bcast_ctl_data_4_platform(DMHD_ctl%plt)
      call bcast_ctl_data_4_platform(DMHD_ctl%org_plt)
!
      call bcast_parallel_shell_ctl(DMHD_ctl%psph_ctl)
!
      call bcast_sph_mhd_model(DMHD_ctl%model_ctl)
      call bcast_sph_mhd_control(DMHD_ctl%smctl_ctl)
!
      call bcast_node_monitor_data_ctl(DMHD_ctl%nmtr_ctl)
      call bcast_sph_monitoring_ctl(DMHD_ctl%smonitor_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (DMHD_ctl%fname_psph_ctl, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(DMHD_ctl%i_mhd_ctl, 0)
!
      end subroutine bcast_sph_mhd_ctl_data
!
!   --------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_model(Dmodel_ctl)
!
      use t_ctl_data_MHD_model
      use calypso_mpi_int
      use bcast_ctl_data_mhd_forces
      use bcast_4_field_ctl
!
      type(mhd_DNS_model_control), intent(inout) :: Dmodel_ctl
!
!
      call bcast_phys_data_ctl(Dmodel_ctl%fld_ctl)
      call bcast_mhd_time_evo_ctl(Dmodel_ctl%evo_ctl)
      call bcast_mhd_layer_ctl(Dmodel_ctl%earea_ctl)
!
      call bcast_bc_4_node_ctl(Dmodel_ctl%nbc_ctl)
      call bcast_bc_4_surf_ctl(Dmodel_ctl%sbc_ctl)
!
      call bcast_dimless_ctl(Dmodel_ctl%dless_ctl)
      call bcast_coef_term_ctl(Dmodel_ctl%eqs_ctl)
      call bcast_forces_ctl(Dmodel_ctl%frc_ctl)
      call bcast_gravity_ctl(Dmodel_ctl%g_ctl)
      call bcast_coriolis_ctl(Dmodel_ctl%cor_ctl)
      call bcast_magneto_ctl(Dmodel_ctl%mcv_ctl)
      call bcast_magnetic_scale_ctl(Dmodel_ctl%bscale_ctl)
      call bcast_ref_scalar_ctl(Dmodel_ctl%reft_ctl)
      call bcast_ref_scalar_ctl(Dmodel_ctl%refc_ctl)
!
      call calypso_mpi_bcast_one_int(Dmodel_ctl%i_model, 0)
!
      end subroutine bcast_sph_mhd_model
!
!   --------------------------------------------------------------------
!
      end module bcast_control_sph_MHD
