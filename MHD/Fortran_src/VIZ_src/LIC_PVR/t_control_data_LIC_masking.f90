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
!!      subroutine read_lic_masking_ctl_data                            &
!!     &         (id_control, hd_block, mask_ctl, c_buf)
!!  `      type(lic_masking_ctl), intent(inout) :: mask_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dealloc_lic_masking_ctls(num_ctl, mask_ctl)
!!        type(lic_masking_ctl), intent(inout) :: mask_ctl(num_ctl)
!!      subroutine dealloc_lic_masking_ctl_flags(mask_ctl)
!!      subroutine bcast_lic_masking_ctl_data(mask_ctl)
!!        type(lic_masking_ctl), intent(inout) :: mask_ctl
!!
!!      subroutine dup_lic_masking_ctls(num_ctl, org_mask_c, new_mask_c)
!!        type(lic_masking_ctl), intent(in) :: org_mask_c(num_ctl)
!!        type(lic_masking_ctl), intent(inout) :: new_mask_c(num_ctl)
!!      subroutine dup_lic_masking_ctl_data(org_mask_c, new_mask_c)
!!        type(lic_masking_ctl), intent(in) :: org_mask_c
!!        type(lic_masking_ctl), intent(inout) :: new_mask_c
!!
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
      module t_control_data_LIC_masking
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
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
      subroutine read_lic_masking_ctl_data                              &
     &         (id_control, hd_block, mask_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
!
      type(lic_masking_ctl), intent(inout) :: mask_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_masking_type, mask_ctl%mask_type_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_masking_field, mask_ctl%field_name_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_masking_comp, mask_ctl%component_ctl)
        call read_control_array_r2(id_control,                          &
     &      hd_masking_range, mask_ctl%mask_range_ctl, c_buf)
      end do
!
      end subroutine read_lic_masking_ctl_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_masking_ctls(num_ctl, mask_ctl)
!
      integer(kind = kint) :: num_ctl
      type(lic_masking_ctl), intent(inout) :: mask_ctl(num_ctl)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_ctl
        call dealloc_lic_masking_ctl_flags(mask_ctl(i))
      end do
!
      end subroutine dealloc_lic_masking_ctls
!
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
!  ---------------------------------------------------------------------
!
      subroutine dup_lic_masking_ctls(num_ctl, org_mask_c, new_mask_c)
!
      integer(kind = kint) :: num_ctl
      type(lic_masking_ctl), intent(in) :: org_mask_c(num_ctl)
      type(lic_masking_ctl), intent(inout) :: new_mask_c(num_ctl)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_ctl
        call dup_lic_masking_ctl_data(org_mask_c(i), new_mask_c(i))
      end do
!
      end subroutine dup_lic_masking_ctls
!
!  ---------------------------------------------------------------------
!
!
      subroutine dup_lic_masking_ctl_data(org_mask_c, new_mask_c)
!
      use copy_control_elements
!
      type(lic_masking_ctl), intent(in) :: org_mask_c
      type(lic_masking_ctl), intent(inout) :: new_mask_c
!
!
      call copy_chara_ctl(org_mask_c%mask_type_ctl,                     &
     &                    new_mask_c%mask_type_ctl)
      call copy_chara_ctl(org_mask_c%field_name_ctl,                    &
     &                    new_mask_c%field_name_ctl)
      call copy_chara_ctl(org_mask_c%component_ctl,                     &
     &                    new_mask_c%component_ctl)
      call dup_control_array_r2(org_mask_c%mask_range_ctl,              &
     &                    new_mask_c%mask_range_ctl)
!
      end subroutine dup_lic_masking_ctl_data
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_LIC_masking
