!>@file   t_control_data_maskings.f90
!!@brief  module t_control_data_maskings
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_multi_masking_ctl                               &
!!     &         (id_control, hd_block, mul_mask_c, c_buf)
!!         integer(kind = kint), intent(in) :: id_control
!!         character(len=kchara), intent(in) :: hd_block
!!         type(multi_masking_ctl), intent(inout) :: mul_mask_c
!!         type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_multi_masking_ctl                              &
!!     &         (id_control, mul_mask_c, level)
!!         integer(kind = kint), intent(in) :: id_control
!!         type(multi_masking_ctl), intent(in) :: mul_mask_c
!!         integer(kind = kint), intent(inout) :: level
!!      subroutine alloc_mul_masking_ctl(mul_mask_c)
!!      subroutine dealloc_mul_masking_ctl(mul_mask_c)
!!         type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine dup_mul_masking_ctl(org_mul_mask_c, new_mul_mask_c)
!!        type(multi_masking_ctl), intent(in) :: org_mul_mask_c
!!        type(multi_masking_ctl), intent(inout) :: new_mul_mask_c
!!
!!      subroutine append_mul_masking_ctl(idx_in, hd_block, mul_mask_c)
!!      subroutine delete_mul_masking_ctl(idx_in, mul_mask_c)
!!        integer(kind = kint), intent(in) :: idx_in
!!        character(len=kchara), intent(in) :: hd_block
!!        type(multi_masking_ctl), intent(inout) :: mul_mask_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array masking_control
!!        begin masking_control
!!          masking_field        magnetic_field
!!          masking_component    amplitude
!!          array masking_range      1
!!            masking_range       0.5    0.8
!!            ...
!!          end array masking_range
!!        end masking_control
!!        ...
!!      end array masking_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_maskings
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_masking
      use skip_comment_f
!
      implicit  none
!
!
      type multi_masking_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'masking_control'
!
        integer(kind = kint) :: num_masking_ctl = 0
        type(masking_by_field_ctl), allocatable :: mask_ctl(:)
      end type multi_masking_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_multi_masking_ctl                                 &
     &         (id_control, hd_block, mul_mask_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(multi_masking_ctl), intent(inout) :: mul_mask_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: n_append
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(mul_mask_c%mask_ctl)) return
      call alloc_mul_masking_ctl(mul_mask_c)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          n_append = mul_mask_c%num_masking_ctl
          call append_mul_masking_ctl(n_append, hd_block, mul_mask_c)
          call read_masking_ctl_data(id_control, hd_block,              &
     &        mul_mask_c%mask_ctl(mul_mask_c%num_masking_ctl), c_buf)
        end if
      end do
!
      end subroutine read_multi_masking_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_multi_masking_ctl                                &
     &         (id_control, mul_mask_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(multi_masking_ctl), intent(in) :: mul_mask_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(mul_mask_c%num_masking_ctl .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 mul_mask_c%block_name)
      do i = 1, mul_mask_c%num_masking_ctl
        call write_masking_ctl_data(id_control,                         &
     &                              mul_mask_c%mask_ctl(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     mul_mask_c%block_name)
!
      end subroutine write_multi_masking_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_mul_masking_ctl(mul_mask_c)
!
      type(multi_masking_ctl), intent(inout) :: mul_mask_c
!
      integer(kind = kint) :: i
!
      do i = 1, mul_mask_c%num_masking_ctl
        call dealloc_masking_ctl_flags(mul_mask_c%mask_ctl(i))
      end do
!
      if(allocated(mul_mask_c%mask_ctl)) deallocate(mul_mask_c%mask_ctl)
      mul_mask_c%num_masking_ctl = 0
!
      end subroutine dealloc_mul_masking_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_mul_masking_ctl(mul_mask_c)
!
      type(multi_masking_ctl), intent(inout) :: mul_mask_c
!
!
      allocate(mul_mask_c%mask_ctl(mul_mask_c%num_masking_ctl))
!
      end subroutine alloc_mul_masking_ctl
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_mul_masking_ctl(idx_in, hd_block, mul_mask_c)
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(multi_masking_ctl), intent(inout) :: mul_mask_c
!
      type(multi_masking_ctl) :: tmp_pvr_isos
      integer(kind = kint) :: i
!
!
      if(idx_in.lt.0 .or. idx_in.gt.mul_mask_c%num_masking_ctl) return
!
      tmp_pvr_isos%num_masking_ctl = mul_mask_c%num_masking_ctl
      call alloc_mul_masking_ctl(tmp_pvr_isos)
      do i = 1, tmp_pvr_isos%num_masking_ctl
        call dup_masking_ctl_data(mul_mask_c%mask_ctl(i),               &
     &                            tmp_pvr_isos%mask_ctl(i))
      end do
!
      call dealloc_mul_masking_ctl(mul_mask_c)
      mul_mask_c%num_masking_ctl = tmp_pvr_isos%num_masking_ctl + 1
      call alloc_mul_masking_ctl(mul_mask_c)
!
      do i = 1, idx_in
        call dup_masking_ctl_data(tmp_pvr_isos%mask_ctl(i),             &
     &                            mul_mask_c%mask_ctl(i))
      end do
      call init_masking_ctl_label(hd_block,                             &
     &    mul_mask_c%mask_ctl(idx_in+1))
      do i = idx_in+1, tmp_pvr_isos%num_masking_ctl
        call dup_masking_ctl_data(tmp_pvr_isos%mask_ctl(i),             &
     &                            mul_mask_c%mask_ctl(i+1))
      end do
!
      call dealloc_mul_masking_ctl(tmp_pvr_isos)
!
      end subroutine append_mul_masking_ctl
!
! -----------------------------------------------------------------------
!
      subroutine delete_mul_masking_ctl(idx_in, mul_mask_c)
!
      integer(kind = kint), intent(in) :: idx_in
      type(multi_masking_ctl), intent(inout) :: mul_mask_c
!
      type(multi_masking_ctl) :: tmp_pvr_isos
      integer(kind = kint) :: i
!
!
      if(idx_in.le.0 .or. idx_in.gt.mul_mask_c%num_masking_ctl) return
!
      tmp_pvr_isos%num_masking_ctl = mul_mask_c%num_masking_ctl
      call alloc_mul_masking_ctl(tmp_pvr_isos)
      do i = 1, tmp_pvr_isos%num_masking_ctl
        call dup_masking_ctl_data(mul_mask_c%mask_ctl(i),               &
     &                            tmp_pvr_isos%mask_ctl(i))
      end do
!
      call dealloc_mul_masking_ctl(mul_mask_c)
      mul_mask_c%num_masking_ctl = tmp_pvr_isos%num_masking_ctl - 1
      call alloc_mul_masking_ctl(mul_mask_c)
!
      do i = 1, idx_in-1
        call dup_masking_ctl_data(tmp_pvr_isos%mask_ctl(i),             &
     &                            mul_mask_c%mask_ctl(i))
      end do
      do i = idx_in, tmp_pvr_isos%num_masking_ctl
        call dup_masking_ctl_data(tmp_pvr_isos%mask_ctl(i+1),           &
     &                            mul_mask_c%mask_ctl(i))
      end do
!
      call dealloc_mul_masking_ctl(tmp_pvr_isos)
!
      end subroutine delete_mul_masking_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dup_mul_masking_ctl(org_mul_mask_c, new_mul_mask_c)
!
      type(multi_masking_ctl), intent(in) :: org_mul_mask_c
      type(multi_masking_ctl), intent(inout) :: new_mul_mask_c
!
      integer(kind = kint) :: i
!
!
      new_mul_mask_c%block_name = org_mul_mask_c%block_name
      new_mul_mask_c%num_masking_ctl = org_mul_mask_c%num_masking_ctl
      call alloc_mul_masking_ctl(new_mul_mask_c)
      do i = 1, org_mul_mask_c%num_masking_ctl
        call dup_masking_ctl_data(org_mul_mask_c%mask_ctl(i),           &
     &                            new_mul_mask_c%mask_ctl(i))
      end do
!
      end subroutine dup_mul_masking_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_maskings
