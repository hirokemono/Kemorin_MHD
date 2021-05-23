!>@file   bcast_masking_control_data.f90
!!@brief  module bcast_masking_control_data
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine bcast_masking_ctl_data(mask_ctl)
!!        type(masking_by_field_ctl), intent(inout) :: mask_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      begin masking_control
!!        masking_type         field or geometry
!!        masking_field        magnetic_field
!!        masking_component    magnitude
!!        array masking_range      1
!!          masking_range       0.5    0.8
!!          ...
!!        end array masking_range
!!      end masking_control
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module bcast_masking_control_data
!
      use m_precision
      use calypso_mpi
!
      use t_control_data_masking
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_masking_ctl_data(mask_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(masking_by_field_ctl), intent(inout) :: mask_ctl
!
!
      call bcast_ctl_type_c1(mask_ctl%mask_type_ctl)
      call bcast_ctl_type_c1(mask_ctl%field_name_ctl)
      call bcast_ctl_type_c1(mask_ctl%component_ctl)
      call bcast_ctl_array_r2(mask_ctl%mask_range_ctl)
!
      call calypso_mpi_bcast_one_int(mask_ctl%i_mask_control, 0)
!
      end subroutine bcast_masking_ctl_data
!
!  ---------------------------------------------------------------------
!
      end module bcast_masking_control_data
