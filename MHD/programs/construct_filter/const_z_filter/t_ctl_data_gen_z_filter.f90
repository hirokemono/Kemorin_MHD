!>@file   t_ctl_data_gen_z_filter.f90
!!@brief  module t_ctl_data_gen_z_filter
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief  Structure for reading parameters for z-filtering files
!!
!!
!!      subroutine read_control_4_z_filter(z_filter_ctl)
!!      subroutine dealloc_ctl_data_gen_z_filter(z_filter_ctl)
!!        type(ctl_data_gen_z_filter), intent(inout) :: z_filter_ctl
!!
      module t_ctl_data_gen_z_filter
!
      use m_precision
      use t_ctl_data_gen_filter
      use t_control_elements
!
      implicit  none
!
!
      integer(kind = kint), parameter :: filter_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_zfilter_ctl = "ctl_z_filter"
!
      type ctl_data_gen_z_filter
!>        File  prefix for filter file
        type(read_character_item) :: z_filter_head_ctl
!
!>        Number of SMP
        type(read_integer_item) :: ip_smp_z_ctl
!
        type(ctl_data_gen_filter) :: gen_f_ctl
      end type ctl_data_gen_z_filter
!
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_control = 'const_z_filter'
      integer (kind=kint) :: i_filter_control = 0
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_head_ctl =  'filter_file_header'
      character(len=kchara), parameter                                  &
     &         :: hd_ip_smp_z_ctl =     'num_smp_ctl'
!
!
      private :: hd_ip_smp_z_ctl, hd_filter_head_ctl
      private :: read_ctl_z_filter_ctl_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_z_filter(z_filter_ctl)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(ctl_data_gen_z_filter), intent(inout) :: z_filter_ctl
!
!
      ctl_file_code = filter_ctl_file_code
!
      open(ctl_file_code, file=fname_zfilter_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_ctl_z_filter_ctl_data(z_filter_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_z_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_z_filter_ctl_data(z_filter_ctl)
!
      use m_read_control_elements
!
      use m_machine_parameter
      use m_ctl_data_4_plane_model
      use skip_comment_f
!
      type(ctl_data_gen_z_filter), intent(inout) :: z_filter_ctl
!
!
      if(right_begin_flag(hd_filter_control) .eq. 0) return
      if (i_filter_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_filter_control = find_control_end_flag(hd_filter_control)
        if(i_filter_control .gt. 0) exit
!
!
        call read_plane_model_param_ctl
        call read_filter_param_ctl(z_filter_ctl%gen_f_ctl)
!
!
        call read_chara_ctl_type                                        &
     &     (hd_filter_head_ctl, z_filter_ctl%z_filter_head_ctl)
!
        call read_integer_ctl_type                                      &
     &      (hd_ip_smp_z_ctl, z_filter_ctl%ip_smp_z_ctl)
      end do
!
      end subroutine read_ctl_z_filter_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_gen_z_filter(z_filter_ctl)
!
      type(ctl_data_gen_z_filter), intent(inout) :: z_filter_ctl
!
!
      call dealloc_filter_param_ctl(z_filter_ctl%gen_f_ctl)
      z_filter_ctl%z_filter_head_ctl%iflag = 0
      z_filter_ctl%ip_smp_z_ctl%iflag = 0
!
      end subroutine dealloc_ctl_data_gen_z_filter
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_gen_z_filter
