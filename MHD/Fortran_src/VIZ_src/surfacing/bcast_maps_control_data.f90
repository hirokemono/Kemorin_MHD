!>@file   bcast_maps_control_data.f90
!!@brief  module bcast_maps_control_data
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  control ID data for surfacing module
!!
!!@verbatim
!!      subroutine bcast_files_4_map_ctl(map_ctls)
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!@endverbatim
!
      module bcast_maps_control_data
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_map_control_data
      private :: bcast_fld_on_map_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_files_4_map_ctl(map_ctls)
!
      use t_control_data_maps
      use t_control_data_4_psf
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(map_rendering_controls), intent(inout) :: map_ctls
      integer (kind=kint) :: i_psf
!
!
      call calypso_mpi_bcast_one_int(map_ctls%num_psf_ctl, 0)
      if(map_ctls%num_psf_ctl .le. 0) return
!
      if(my_rank .gt. 0) call alloc_map_ctl_stract(map_ctls)
!
      do i_psf = 1, map_ctls%num_psf_ctl
        call bcast_map_control_data(map_ctls%psf_ctl_struct(i_psf))
      end do
      call calypso_mpi_bcast_character(map_ctls%fname_psf_ctl,          &
     &    cast_long(map_ctls%num_psf_ctl*kchara), 0)
!
      end subroutine bcast_files_4_map_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_map_control_data(psf_c)
!
      use t_control_data_4_psf
      use calypso_mpi_int
      use bcast_section_control_data
      use bcast_control_arrays
!
      type(psf_ctl), intent(inout) :: psf_c
!
!
      call calypso_mpi_bcast_one_int(psf_c%i_psf_ctl, 0)
      call calypso_mpi_bcast_one_int(psf_c%i_output_field, 0)
!
      call bcast_ctl_type_c1(psf_c%psf_file_head_ctl)
      call bcast_ctl_type_c1(psf_c%psf_output_type_ctl)
!
      call bcast_section_def_control(psf_c%psf_def_c)
      call bcast_fld_on_map_control(psf_c%fld_on_psf_c)
!
      end subroutine bcast_map_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_fld_on_map_control(fld_on_psf_c)
!
      use t_control_data_4_fld_on_psf
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(field_on_psf_ctl), intent(inout) :: fld_on_psf_c
!
!
      call bcast_ctl_type_r1(fld_on_psf_c%output_value_ctl)
      call bcast_ctl_type_c1(fld_on_psf_c%output_type_ctl)
      call bcast_ctl_array_c2(fld_on_psf_c%field_output_ctl)
!
      call calypso_mpi_bcast_one_int(fld_on_psf_c%i_iso_result, 0)
!
      end subroutine bcast_fld_on_map_control
!
!   --------------------------------------------------------------------
!
      end module bcast_maps_control_data
