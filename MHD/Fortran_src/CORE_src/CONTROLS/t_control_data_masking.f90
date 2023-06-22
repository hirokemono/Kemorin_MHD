!>@file   t_control_data_masking.f90
!!@brief  module t_control_data_masking
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for data masking
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_masking_ctl_data                                &
!!     &         (id_control, hd_block, mask_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_block
!!  `     type(masking_by_field_ctl), intent(inout) :: mask_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_masking_ctl_data                               &
!!     &         (id_control, hd_block, mask_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_block
!!        type(masking_by_field_ctl), intent(in) :: mask_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_masking_ctls(num_ctl, mask_ctl)
!!        type(masking_by_field_ctl), intent(inout) :: mask_ctl(num_ctl)
!!      subroutine dealloc_masking_ctl_flags(mask_ctl)
!!
!!      subroutine dup_masking_ctls(num_ctl, org_mask_c, new_mask_c)
!!        type(masking_by_field_ctl), intent(in) :: org_mask_c(num_ctl)
!!        type(masking_by_field_ctl), intent(inout)                     &
!!     &                                         :: new_mask_c(num_ctl)
!!      subroutine dup_masking_ctl_data(org_mask_c, new_mask_c)
!!        type(masking_by_field_ctl), intent(in) :: org_mask_c
!!        type(masking_by_field_ctl), intent(inout) :: new_mask_c
!!
!!      subroutine add_mask_field_to_fld_ctl(mask_ctl, field_ctl)
!!        type(masking_by_field_ctl), intent(in) :: mask_ctl
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!      integer(kind = kint) function num_ctl_label_masking()
!!      subroutine set_ctl_label_masking(names)
!!        character(len = kchara), intent(inout)                        &
!!                                :: names(n_label_masking)
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
      module t_control_data_masking
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real2
      use skip_comment_f
!
      implicit  none
!
!
      type masking_by_field_ctl
        type(read_character_item) :: mask_type_ctl
        type(read_character_item) :: field_name_ctl
        type(read_character_item) :: component_ctl
        type(ctl_array_r2) ::       mask_range_ctl
!
        integer (kind=kint) :: i_mask_control = 0
      end type masking_by_field_ctl
!
!     4th level for masking
!
      character(len=kchara), parameter, private                         &
     &                      :: hd_masking_type = 'masking_type'
      character(len=kchara), parameter, private                         &
     &                      :: hd_masking_field = 'masking_field'
      character(len=kchara), parameter, private                         &
     &                      :: hd_masking_comp = 'masking_component'
!
      character(len=kchara), parameter, private                         &
     &                      :: hd_masking_range = 'masking_range'
!
      integer(kind = kint), parameter, private :: n_label_masking = 4
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_masking_ctl_data                                  &
     &         (id_control, hd_block, mask_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
!
      type(masking_by_field_ctl), intent(inout) :: mask_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(mask_ctl%i_mask_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
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
      mask_ctl%i_mask_control = 1
!
      end subroutine read_masking_ctl_data
!
!  ---------------------------------------------------------------------
!
      subroutine write_masking_ctl_data                                 &
     &         (id_control, hd_block, mask_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
      type(masking_by_field_ctl), intent(in) :: mask_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(mask_ctl%i_mask_control .le. 0) return
!
      maxlen = len_trim(hd_masking_type)
      maxlen = max(maxlen, len_trim(hd_masking_field))
      maxlen = max(maxlen, len_trim(hd_masking_comp))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    mask_ctl%mask_type_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    mask_ctl%field_name_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    mask_ctl%component_ctl)
!
      call write_control_array_r2(id_control, level,                    &
     &    mask_ctl%mask_range_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_masking_ctl_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_masking_ctls(num_ctl, mask_ctl)
!
      integer(kind = kint) :: num_ctl
      type(masking_by_field_ctl), intent(inout) :: mask_ctl(num_ctl)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_ctl
        call dealloc_masking_ctl_flags(mask_ctl(i))
      end do
!
      end subroutine dealloc_masking_ctls
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_masking_ctl_flags(mask_ctl)
!
      type(masking_by_field_ctl), intent(inout) :: mask_ctl
!
!
      mask_ctl%mask_type_ctl%iflag =  0
      mask_ctl%field_name_ctl%iflag = 0
      mask_ctl%component_ctl%iflag =  0
!
      call dealloc_control_array_r2(mask_ctl%mask_range_ctl)
      mask_ctl%mask_range_ctl%num =  0
      mask_ctl%mask_range_ctl%icou = 0
!
      mask_ctl%i_mask_control =      0
!
      end subroutine dealloc_masking_ctl_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_masking_ctls(num_ctl, org_mask_c, new_mask_c)
!
      integer(kind = kint) :: num_ctl
      type(masking_by_field_ctl), intent(in) :: org_mask_c(num_ctl)
      type(masking_by_field_ctl), intent(inout) :: new_mask_c(num_ctl)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_ctl
        call dup_masking_ctl_data(org_mask_c(i), new_mask_c(i))
      end do
!
      end subroutine dup_masking_ctls
!
!  ---------------------------------------------------------------------
!
      subroutine dup_masking_ctl_data(org_mask_c, new_mask_c)
!
      type(masking_by_field_ctl), intent(in) :: org_mask_c
      type(masking_by_field_ctl), intent(inout) :: new_mask_c
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
      end subroutine dup_masking_ctl_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_mask_field_to_fld_ctl(mask_ctl, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(masking_by_field_ctl), intent(in) :: mask_ctl
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(mask_ctl%field_name_ctl%iflag .eq. 0) return
      call add_viz_name_ctl                                             &
     &   (mask_ctl%field_name_ctl%charavalue, field_ctl)
!
      end subroutine add_mask_field_to_fld_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_ctl_label_masking()
      num_ctl_label_masking = n_label_masking
      return
      end function num_ctl_label_masking
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_masking(names)
!
      character(len = kchara), intent(inout) :: names(n_label_masking)
!
!
      call set_control_labels(hd_masking_type,  names( 1))
      call set_control_labels(hd_masking_field, names( 2))
      call set_control_labels(hd_masking_comp,  names( 3))
      call set_control_labels(hd_masking_range, names( 4))
!
      end subroutine set_ctl_label_masking
!
! ----------------------------------------------------------------------
!
      end module t_control_data_masking
