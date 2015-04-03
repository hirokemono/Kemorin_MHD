!>@file   m_ctl_data_gen_z_filter.f90
!!@brief  module m_ctl_data_gen_z_filter
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief  Structure for reading parameters for z-filtering files
!!
      module m_ctl_data_gen_z_filter
!
      use m_precision
      use m_ctl_data_4_solvers
      use m_ctl_data_gen_filter
      use t_control_elements
!
      implicit  none
!
!
      integer(kind = kint), parameter :: filter_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_zfilter_ctl = "ctl_z_filter"
!
!>      File  prefix for filter file
      type(read_character_item), save :: z_filter_head_ctl
!
!>      Number of SMP
      type(read_integer_item), save :: ip_smp_z_ctl
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
      subroutine read_control_4_z_filter
!
      use m_read_control_elements
      use skip_comment_f
!
!
      ctl_file_code = filter_ctl_file_code
!
      open(ctl_file_code, file=fname_zfilter_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_ctl_z_filter_ctl_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_z_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_z_filter_ctl_data
!
      use m_read_control_elements
!
      use m_machine_parameter
      use m_ctl_data_4_plane_model
      use m_ctl_data_gen_filter
      use skip_comment_f
!
!
      if(right_begin_flag(hd_filter_control) .eq. 0) return
      if (i_filter_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_filter_control, i_filter_control)
        if(i_filter_control .gt. 0) exit
!
!
        call read_plane_model_param_ctl
        call read_filter_param_ctl
!
!
        call read_chara_ctl_type(hd_filter_head_ctl, z_filter_head_ctl)
!
        call read_integer_ctl_type(hd_ip_smp_z_ctl, ip_smp_z_ctl)
      end do
!
      end subroutine read_ctl_z_filter_ctl_data
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_gen_z_filter
