!>@file   bcast_control_data_vizs.f90
!!@brief  module bcast_control_data_vizs
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine bcast_viz_controls(viz_ctls)
!!       type(visualization_controls), intent(inout) :: viz_ctls
!!@endverbatim
      module bcast_control_data_vizs
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_vizs
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_files_4_lic_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_viz_controls(viz_ctls)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use bcast_section_control_data
      use bcast_maps_control_data
      use bcast_ctl_data_field_line
      use bcast_ctl_data_vol_repart
      use bcast_control_data_pvrs
      use transfer_to_long_integers
!
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      call bcast_control_vol_repart(viz_ctls%repart_ctl)
!
      call bcast_files_4_psf_ctl(viz_ctls%psf_ctls)
      call bcast_files_4_iso_ctl(viz_ctls%iso_ctls)
      call bcast_files_4_map_ctl(viz_ctls%map_ctls)
      call bcast_files_4_pvr_ctl(viz_ctls%pvr_ctls)
      call bcast_files_4_fline_ctl(viz_ctls%fline_ctls)
      call bcast_files_4_lic_ctl(viz_ctls%lic_ctls)
!
      call bcast_ctl_type_r1(viz_ctls%delta_t_psf_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_iso_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_map_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_pvr_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_fline_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_lic_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_ucd_v_ctl)
!
      call bcast_ctl_type_i1(viz_ctls%i_step_psf_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_iso_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_map_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_pvr_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_lic_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_fline_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_ucd_v_ctl)
!
      call bcast_ctl_type_c1(viz_ctls%output_field_file_fmt_ctl)
!
      call calypso_mpi_bcast_character(viz_ctls%fname_vol_repart_ctl,   &
     &                                 cast_long(kchara), 0)
!
      call calypso_mpi_bcast_character                                  &
     &   (viz_ctls%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(viz_ctls%i_viz_control, 0)
!
      end subroutine bcast_viz_controls
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_files_4_lic_ctl(lic_ctls)
!
      use t_control_data_LIC_pvrs
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_data_4_pvr
!
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
!
      call calypso_mpi_bcast_character                                  &
     &   (lic_ctls%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(lic_ctls%num_lic_ctl, 0)
      if(lic_ctls%num_lic_ctl .le. 0) return
!
      if(my_rank .gt. 0)  call alloc_lic_ctl_struct(lic_ctls)
!
      call calypso_mpi_bcast_character(lic_ctls%fname_lic_ctl,          &
     &    cast_long(kchara*lic_ctls%num_lic_ctl), 0)
!
      end subroutine bcast_files_4_lic_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_control_data_vizs
