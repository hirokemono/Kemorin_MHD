!>@file   t_control_data_LIC_pvrs.f90
!!@brief  module t_control_data_LIC_pvrs
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine alloc_lic_ctl_struct(lic_ctls)
!!      subroutine dealloc_lic_ctl_struct(lic_ctls)
!!      subroutine init_lic_ctls_labels(hd_block, lic_ctls)
!!      subroutine read_files_4_lic_ctl                                 &
!!     &         (id_control, hd_lic_ctl, lic_ctls, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_lic_ctl
!!        type(lic_rendering_controls), intent(inout) :: lic_ctls
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_files_4_lic_ctl(id_control, hd_lic_ctl,        &
!!     &          lic_ctls, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_lic_ctl
!!        type(lic_rendering_controls), intent(in) :: lic_ctls
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine add_fields_4_lics_to_fld_ctl(lic_ctls, field_ctl)
!!        type(lic_rendering_controls), intent(in) :: lic_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!      subroutine append_lic_ctl_struct(idx_in, hd_block, lic_ctls)
!!      subroutine delete_lic_ctl_struct(idx_in, lic_ctls)
!!        integer(kind = kint), intent(in) :: idx_in
!!        character(len=kchara), intent(in) :: hd_block
!!        type(lic_rendering_controls), intent(inout) :: lic_ctls
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  LIC_rendering
!!      file  LIC_rendering  'ctl_pvr_temp'
!!    end array LIC_rendering
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module t_control_data_LIC_pvrs
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_pvr
      use t_control_data_LIC
!
      implicit  none
!
      type lic_rendering_controls
!>        Control block name
        character(len = kchara) :: block_name = 'cross_section_ctl'
!
        integer(kind = kint) :: num_lic_ctl = 0
        character(len = kchara), allocatable :: fname_lic_ctl(:)
        type(pvr_parameter_ctl), allocatable :: pvr_ctl_type(:)
        type(lic_parameter_ctl), allocatable :: lic_ctl_type(:)
      end type lic_rendering_controls
!
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_lic_ctl_struct(lic_ctls)
!
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
!
      allocate(lic_ctls%fname_lic_ctl(lic_ctls%num_lic_ctl))
      allocate(lic_ctls%pvr_ctl_type(lic_ctls%num_lic_ctl))
      allocate(lic_ctls%lic_ctl_type(lic_ctls%num_lic_ctl))
!
      end subroutine alloc_lic_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_ctl_struct(lic_ctls)
!
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
      integer(kind = kint) :: i
!
!
      if(allocated(lic_ctls%fname_lic_ctl)) then
        do i = 1, lic_ctls%num_lic_ctl
          call dealloc_lic_control_flags(lic_ctls%lic_ctl_type(i))
          call deallocate_cont_dat_pvr(lic_ctls%pvr_ctl_type(i))
        end do
!
        deallocate(lic_ctls%lic_ctl_type)
        deallocate(lic_ctls%pvr_ctl_type)
        deallocate(lic_ctls%fname_lic_ctl)
      end if
      lic_ctls%num_lic_ctl = 0
!
      end subroutine dealloc_lic_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine init_lic_ctls_labels(hd_block, lic_ctls)
!
      character(len=kchara), intent(in) :: hd_block
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
      lic_ctls%num_lic_ctl = 0
      lic_ctls%block_name = hd_block
!
      end subroutine init_lic_ctls_labels
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_lic_ctl                                   &
     &         (id_control, hd_lic_ctl, lic_ctls, c_buf)
!
      use t_read_control_elements
      use ctl_file_lic_pvr_IO
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_lic_ctl
!
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: n_append
!
      if(check_array_flag(c_buf, hd_lic_ctl) .eqv. .FALSE.) return
      if(allocated(lic_ctls%fname_lic_ctl)) return
      call alloc_lic_ctl_struct(lic_ctls)
!
      do
        call load_one_line_from_control(id_control, hd_lic_ctl, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_lic_ctl)) exit
!
        if(check_file_flag(c_buf, hd_lic_ctl)                           &
     &        .or. check_begin_flag(c_buf, hd_lic_ctl)) then
          n_append = lic_ctls%num_lic_ctl
          call append_lic_ctl_struct(n_append, hd_lic_ctl, lic_ctls)
!
          call write_multi_ctl_file_message                             &
     &       (hd_lic_ctl, lic_ctls%num_lic_ctl, c_buf%level)
          call sel_read_control_lic_pvr(id_control, hd_lic_ctl,         &
     &        lic_ctls%fname_lic_ctl(lic_ctls%num_lic_ctl),             &
     &        lic_ctls%pvr_ctl_type(lic_ctls%num_lic_ctl),              &
     &        lic_ctls%lic_ctl_type(lic_ctls%num_lic_ctl), c_buf)
        end if
      end do
!
      end subroutine read_files_4_lic_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_files_4_lic_ctl(id_control, hd_lic_ctl,          &
     &          lic_ctls, level)
!
      use t_read_control_elements
      use ctl_file_lic_pvr_IO
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_lic_ctl
      type(lic_rendering_controls), intent(in) :: lic_ctls
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(lic_ctls%num_lic_ctl .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, hd_lic_ctl)
      do i = 1, lic_ctls%num_lic_ctl
        write(*,'(3a,i4,a)', ADVANCE='NO') '!  ', trim(hd_lic_ctl),     &
     &                                     ' No. ', i
        call sel_write_control_lic_pvr                                  &
     &     (id_control, hd_lic_ctl, lic_ctls%fname_lic_ctl(i),          &
     &      lic_ctls%pvr_ctl_type(i), lic_ctls%lic_ctl_type(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     hd_lic_ctl)
!
      end subroutine write_files_4_lic_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_lic_ctl_struct(idx_in, hd_block, lic_ctls)
!
      use ctl_data_lic_pvr_IO
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
      type(lic_rendering_controls) :: tmp_lics_c
      integer(kind = kint) :: i
!
!
      if(idx_in.lt.0 .or. idx_in.gt.lic_ctls%num_lic_ctl) return
!
      tmp_lics_c%num_lic_ctl = lic_ctls%num_lic_ctl
      call alloc_lic_ctl_struct(tmp_lics_c)
      do i = 1, lic_ctls%num_lic_ctl
        tmp_lics_c%fname_lic_ctl(i) = lic_ctls%fname_lic_ctl(i)
        call dup_pvr_ctl(lic_ctls%pvr_ctl_type(i),                      &
     &                   tmp_lics_c%pvr_ctl_type(i))
        call dup_lic_control_data(lic_ctls%lic_ctl_type(i),             &
     &                            tmp_lics_c%lic_ctl_type(i))
      end do
!
      call dealloc_lic_ctl_struct(lic_ctls)
      lic_ctls%num_lic_ctl = tmp_lics_c%num_lic_ctl + 1
      call alloc_lic_ctl_struct(lic_ctls)
!
      do i = 1, idx_in
        lic_ctls%fname_lic_ctl(i) = tmp_lics_c%fname_lic_ctl(i)
        call dup_pvr_ctl(tmp_lics_c%pvr_ctl_type(i),                    &
     &                   lic_ctls%pvr_ctl_type(i))
        call dup_lic_control_data(tmp_lics_c%lic_ctl_type(i),           &
     &                            lic_ctls%lic_ctl_type(i))
      end do
!
      lic_ctls%fname_lic_ctl(idx_in+1) = 'NO_FILE'
      call init_lic_pvr_ctl_label(hd_block,                             &
     &                            lic_ctls%pvr_ctl_type(idx_in+1),      &
     &                            lic_ctls%lic_ctl_type(idx_in+1))
!
      do i = idx_in+1, tmp_lics_c%num_lic_ctl
        lic_ctls%fname_lic_ctl(i+1) = tmp_lics_c%fname_lic_ctl(i)
        call dup_pvr_ctl(tmp_lics_c%pvr_ctl_type(i),                    &
     &                   lic_ctls%pvr_ctl_type(i+1))
        call dup_lic_control_data(tmp_lics_c%lic_ctl_type(i),           &
     &                            lic_ctls%lic_ctl_type(i+1))
      end do
!
      call dealloc_lic_ctl_struct(tmp_lics_c)
!
      end subroutine append_lic_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine delete_lic_ctl_struct(idx_in, lic_ctls)
!
      integer(kind = kint), intent(in) :: idx_in
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
      type(lic_rendering_controls) :: tmp_lics_c
      integer(kind = kint) :: i
!
!
      if(idx_in.le.0 .or. idx_in.gt.lic_ctls%num_lic_ctl) return
!
      tmp_lics_c%num_lic_ctl = lic_ctls%num_lic_ctl
      call alloc_lic_ctl_struct(tmp_lics_c)
      do i = 1, lic_ctls%num_lic_ctl
        tmp_lics_c%fname_lic_ctl(i) = lic_ctls%fname_lic_ctl(i)
        call dup_pvr_ctl(lic_ctls%pvr_ctl_type(i),                      &
     &                   tmp_lics_c%pvr_ctl_type(i))
        call dup_lic_control_data(lic_ctls%lic_ctl_type(i),             &
     &                            tmp_lics_c%lic_ctl_type(i))
      end do
!
      call dealloc_lic_ctl_struct(lic_ctls)
      lic_ctls%num_lic_ctl = tmp_lics_c%num_lic_ctl - 1
      call alloc_lic_ctl_struct(lic_ctls)
!
      do i = 1, idx_in-1
        lic_ctls%fname_lic_ctl(i) = tmp_lics_c%fname_lic_ctl(i)
        call dup_pvr_ctl(tmp_lics_c%pvr_ctl_type(i),                    &
     &                   lic_ctls%pvr_ctl_type(i))
        call dup_lic_control_data(tmp_lics_c%lic_ctl_type(i),           &
     &                            lic_ctls%lic_ctl_type(i))
      end do
      do i = idx_in, lic_ctls%num_lic_ctl
        lic_ctls%fname_lic_ctl(1) = tmp_lics_c%fname_lic_ctl(i+1)
        call dup_pvr_ctl(tmp_lics_c%pvr_ctl_type(i+1),                  &
     &                   lic_ctls%pvr_ctl_type(i))
        call dup_lic_control_data(tmp_lics_c%lic_ctl_type(i+1),         &
     &                            lic_ctls%lic_ctl_type(i))
      end do
!
      call dealloc_lic_ctl_struct(tmp_lics_c)
!
      end subroutine delete_lic_ctl_struct
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_fields_4_lics_to_fld_ctl(lic_ctls, field_ctl)
!
      use t_control_array_character3
!
      type(lic_rendering_controls), intent(in) :: lic_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i_lic
!
!
      do i_lic = 1, lic_ctls%num_lic_ctl
        call add_fields_4_lic_to_fld_ctl                                &
     &     (lic_ctls%lic_ctl_type(i_lic), field_ctl)
      end do
!
      end subroutine add_fields_4_lics_to_fld_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_LIC_pvrs
