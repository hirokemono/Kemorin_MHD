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
!!      subroutine append_SGS_filter_ctls(sgs_ctl)
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
      private :: append_SGS_filter_ctls, copy_SGS_filter_ctls
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
          call append_SGS_filter_ctls(sgs_ctl)
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
      subroutine append_SGS_filter_ctls(sgs_ctl)
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
      integer(kind = kint) :: num_tmp = 0
      type(sph_filter_ctl_type), allocatable :: tmp_sfil_c(:)
!
!
      num_tmp = sgs_ctl%num_sph_filter_ctl
      allocate(tmp_sfil_c(num_tmp))
      call copy_SGS_filter_ctls                                         &
     &   (num_tmp, sgs_ctl%sph_filter_ctl, tmp_sfil_c)
      call dealloc_sph_filter_ctl(sgs_ctl)
!
      sgs_ctl%num_sph_filter_ctl = num_tmp + 1
      call alloc_sph_filter_ctl(sgs_ctl)
!
      call copy_SGS_filter_ctls                                         &
     &   (num_tmp, tmp_sfil_c, sgs_ctl%sph_filter_ctl(1))
      deallocate(tmp_sfil_c)
!
      end subroutine append_SGS_filter_ctls
!
! -----------------------------------------------------------------------
!
      subroutine copy_SGS_filter_ctls(num_ctl, org_sfil_c, new_sfil_c)
!
      integer(kind = kint), intent(in) :: num_ctl
      type(sph_filter_ctl_type), intent(in) :: org_sfil_c(num_ctl)
      type(sph_filter_ctl_type), intent(inout) :: new_sfil_c(num_ctl)
!
      integer(kind = kint) :: i
!
      do i = 1, num_ctl
        call copy_control_4_SGS_filter(org_sfil_c(i), new_sfil_c(i))
      end do
!
      end subroutine copy_SGS_filter_ctls
!
! -----------------------------------------------------------------------
!
      end module ctl_data_SGS_filters_IO
