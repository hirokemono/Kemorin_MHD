!>@file   t_control_data_LIC_masking.f90
!!@brief  module t_control_data_LIC_masking
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_lic_masking_ctl_data(hd_masking_ctl, mask_ctl)
!!      subroutine dealloc_lic_masking_ctl_flags(mask_ctl)
!!      subroutine bcast_lic_masking_ctl_data(mask_ctl)
!!        type(lic_masking_ctl), intent(inout) :: mask_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      begin masking_control
!!        masking_type         field or geometry
!!        masking_field        magnetic_field
!!        masking_component    magnetic_field
!!        array masking_range      1
!!          masking_range       0.5    0.8
!!          ...
!!        end array masking_range
!!      end masking_control
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_LIC_masking
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_read_control_elements
      use t_control_elements
      use t_control_array_real2
      use skip_comment_f
!
      implicit  none
!
!
      type lic_masking_ctl
        type(read_character_item) :: mask_type_ctl
        type(read_character_item) :: field_name_ctl
        type(read_character_item) :: component_ctl
        type(ctl_array_r2) ::       mask_range_ctl
      end type lic_masking_ctl
!
!     4th level for LIC masking
!
      character(len=kchara) :: hd_masking_type = 'masking_type'
      character(len=kchara) :: hd_masking_field = 'masking_field'
      character(len=kchara) :: hd_masking_comp = 'masking_component'
!
      character(len=kchara) :: hd_masking_range = 'masking_range'
!
      private :: hd_masking_field, hd_masking_comp, hd_masking_range
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_lic_masking_ctl_data(hd_masking_ctl, mask_ctl)
!
      character(len = kchara), intent(in) :: hd_masking_ctl
      type(lic_masking_ctl), intent(inout) :: mask_ctl
!
      integer(kind = kint) :: i_masking
!
!
      if(right_begin_flag(hd_masking_ctl) .eq. 0) return
      i_masking = 0
      do
        call load_ctl_label_and_line
!
        i_masking = find_control_end_flag(hd_masking_ctl)
        if(i_masking .gt. 0) exit
!
!
        call read_chara_ctl_type                                        &
     &     (hd_masking_type, mask_ctl%mask_type_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_masking_field, mask_ctl%field_name_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_masking_comp, mask_ctl%component_ctl)
        call read_control_array_r2(ctl_file_code,                       &
     &      hd_masking_range, mask_ctl%mask_range_ctl, c_buf1)
      end do
!
      end subroutine read_lic_masking_ctl_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_masking_ctl_flags(mask_ctl)
!
      type(lic_masking_ctl), intent(inout) :: mask_ctl
!
!
      mask_ctl%mask_type_ctl%iflag = 0
      mask_ctl%field_name_ctl%iflag = 0
      mask_ctl%component_ctl%iflag =  0
!
      call dealloc_control_array_r2(mask_ctl%mask_range_ctl)
      mask_ctl%mask_range_ctl%num =  0
      mask_ctl%mask_range_ctl%icou = 0
!
      end subroutine dealloc_lic_masking_ctl_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_lic_masking_ctl_data(mask_ctl)
!
      use bcast_control_arrays
!
      type(lic_masking_ctl), intent(inout) :: mask_ctl
!
!
      call bcast_ctl_type_c1(mask_ctl%mask_type_ctl)
      call bcast_ctl_type_c1(mask_ctl%field_name_ctl)
      call bcast_ctl_type_c1(mask_ctl%component_ctl)
      call bcast_ctl_array_r2(mask_ctl%mask_range_ctl)
!
      end subroutine bcast_lic_masking_ctl_data
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_LIC_masking
