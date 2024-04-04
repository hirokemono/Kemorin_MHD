!>@file   ctl_data_SGS_filters_IO.f90
!!@brief  module ctl_data_SGS_filters_IO
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for SGS model controls
!!
!!@verbatim
!!      subroutine read_control_4_SGS_filters                           &
!!     &         (id_control, hd_block, sgs_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_4_SGS_filters                          &
!!     &         (id_control, sgs_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(SGS_model_control), intent(in) :: sgs_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine append_SGS_filter_ctls(idx_in, hd_block, sgs_ctl)
!!      subroutine delete_SGS_filter_ctls(idx_in, hd_block, sgs_ctl)
!!        integer(kind = kint), intent(in) :: idx_in
!!        character(len=kchara), intent(in) :: hd_block
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!@endverbatim
!
      module ctl_data_SGS_filters_IO
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_ctl_data_SGS_filter
      use t_ctl_data_SGS_model
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_SGS_filters                             &
     &         (id_control, hd_block, sgs_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: n_append
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(sgs_ctl%num_sph_filter_ctl .gt. 0) return
      sgs_ctl%num_sph_filter_ctl = 0
      call alloc_sph_filter_ctl(sgs_ctl)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          n_append = sgs_ctl%num_sph_filter_ctl
          call append_SGS_filter_ctls(n_append, hd_block, sgs_ctl)
          call read_control_4_SGS_filter(id_control, hd_block,          &
     &        sgs_ctl%sph_filter_ctl(sgs_ctl%num_sph_filter_ctl),       &
     &        c_buf)
        end if
      end do
!
      end subroutine read_control_4_SGS_filters
!
!   --------------------------------------------------------------------
!
      subroutine write_control_4_SGS_filters                            &
     &          (id_control, sgs_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(SGS_model_control), intent(in) :: sgs_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
      if(sgs_ctl%num_sph_filter_ctl .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                            sgs_ctl%sph_filter_ctl(1)%block_name)
      do i = 1, sgs_ctl%num_sph_filter_ctl
        call write_control_4_SGS_filter(id_control,                     &
     &      sgs_ctl%sph_filter_ctl(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                            sgs_ctl%sph_filter_ctl(1)%block_name)
!
      end subroutine write_control_4_SGS_filters
!
!   --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_SGS_filter_ctls(idx_in, hd_block, sgs_ctl)
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
      integer(kind = kint) :: num_tmp = 0
      type(sph_filter_ctl_type), allocatable :: tmp_sfil_c(:)
!
      integer(kind = kint) :: i
!
      if(idx_in.lt.0 .or. idx_in.gt.sgs_ctl%num_sph_filter_ctl) return
!
      num_tmp = sgs_ctl%num_sph_filter_ctl
      allocate(tmp_sfil_c(num_tmp))
      do i = 1, num_tmp
        call copy_control_4_SGS_filter(sgs_ctl%sph_filter_ctl(i),       &
     &                                 tmp_sfil_c(i))
      end do
!
      call dealloc_sph_filter_ctl(sgs_ctl)
      sgs_ctl%num_sph_filter_ctl = num_tmp + 1
      call alloc_sph_filter_ctl(sgs_ctl)
!
      do i = 1, idx_in
        call copy_control_4_SGS_filter(tmp_sfil_c(i),                   &
     &                                 sgs_ctl%sph_filter_ctl(i))
      end do
      call init_control_SGS_filter_label(hd_block,                      &
          sgs_ctl%sph_filter_ctl(idx_in+1))
      do i = idx_in+1, num_tmp
        call copy_control_4_SGS_filter(tmp_sfil_c(i),                   &
     &                                 sgs_ctl%sph_filter_ctl(i+1))
      end do
!
      deallocate(tmp_sfil_c)
!
      end subroutine append_SGS_filter_ctls
!
! -----------------------------------------------------------------------
!
      subroutine delete_SGS_filter_ctls(idx_in, sgs_ctl)
!
      integer(kind = kint), intent(in) :: idx_in
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
      integer(kind = kint) :: num_tmp = 0
      type(sph_filter_ctl_type), allocatable :: tmp_sfil_c(:)
!
      integer(kind = kint) :: i
!
      if(idx_in.lt.0 .or. idx_in.gt.sgs_ctl%num_sph_filter_ctl) return
!
      num_tmp = sgs_ctl%num_sph_filter_ctl
      allocate(tmp_sfil_c(num_tmp))
      do i = 1, num_tmp
        call copy_control_4_SGS_filter(sgs_ctl%sph_filter_ctl(i),       &
     &                                 tmp_sfil_c(i))
      end do
!
      call dealloc_sph_filter_ctl(sgs_ctl)
      sgs_ctl%num_sph_filter_ctl = num_tmp - 1
      call alloc_sph_filter_ctl(sgs_ctl)
!
      do i = 1, idx_in-1
        call copy_control_4_SGS_filter(tmp_sfil_c(i),                   &
     &                                 sgs_ctl%sph_filter_ctl(i))
      end do
      do i = idx_in, sgs_ctl%num_sph_filter_ctl
        call copy_control_4_SGS_filter(tmp_sfil_c(i+1),                 &
     &                                 sgs_ctl%sph_filter_ctl(i))
      end do
!
      deallocate(tmp_sfil_c)
!
      end subroutine delete_SGS_filter_ctls
!
! -----------------------------------------------------------------------
!
      end module ctl_data_SGS_filters_IO
