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
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_gen_filter
      use t_control_array_character
      use t_control_array_integer
      use t_ctl_data_4_plane_model
!
      implicit  none
!
!
      integer(kind = kint), parameter :: filter_ctl_file_code = 11
!
      type ctl_data_gen_z_filter
!>        File  prefix for filter file
        type(read_character_item) :: z_filter_head_ctl
!>        Number of SMP
        type(read_integer_item) :: ip_smp_z_ctl
!
        type(ctl_data_gen_filter) :: gen_f_ctl
        type(ctl_data_4_plane_model) :: cube_c
!
        integer (kind=kint) :: i_filter_control = 0
      end type ctl_data_gen_z_filter
!
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_control = 'const_z_filter'
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_head_ctl =  'filter_file_header'
      character(len=kchara), parameter                                  &
     &         :: hd_ip_smp_z_ctl =     'num_smp_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_param_ctl = 'filter_control'
      character(len=kchara), parameter                                  &
     &         :: hd_plane_def = 'plane_mesh_ctl'
!
!
      private :: hd_ip_smp_z_ctl, hd_filter_head_ctl, hd_plane_def
      private :: hd_filter_param_ctl
      private :: read_ctl_z_filter_ctl_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_z_filter(file_name, z_filter_ctl)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_name
      type(ctl_data_gen_z_filter), intent(inout) :: z_filter_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
        c_buf1%level = 0
        open(filter_ctl_file_code, file=file_name, status='old')
!
        do
          call load_one_line_from_control(filter_ctl_file_code, c_buf1)
          call read_ctl_z_filter_ctl_data(filter_ctl_file_code,         &
     &        hd_filter_control, z_filter_ctl, c_buf1)
          if(z_filter_ctl%i_filter_control .gt. 0) exit
        end do
        close(filter_ctl_file_code)
!
      end subroutine read_control_4_z_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_z_filter_ctl_data                             &
     &         (id_control, hd_block, z_filter_ctl, c_buf)
!
      use skip_comment_f
      use ctl_data_gen_filter_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_gen_z_filter), intent(inout) :: z_filter_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(z_filter_ctl%i_filter_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_plane_model_param_ctl                                 &
     &     (id_control, hd_plane_def, z_filter_ctl%cube_c, c_buf)
        call read_filter_param_ctl(id_control, hd_filter_param_ctl,     &
     &      z_filter_ctl%gen_f_ctl, c_buf)
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_filter_head_ctl, z_filter_ctl%z_filter_head_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_ip_smp_z_ctl, z_filter_ctl%ip_smp_z_ctl)
      end do
      z_filter_ctl%i_filter_control = 1
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
      call reset_plane_model_param_ctl(z_filter_ctl%cube_c)
      call dealloc_filter_param_ctl(z_filter_ctl%gen_f_ctl)
!
      z_filter_ctl%z_filter_head_ctl%iflag = 0
      z_filter_ctl%ip_smp_z_ctl%iflag = 0
!
      z_filter_ctl%i_filter_control = 0
!
      end subroutine dealloc_ctl_data_gen_z_filter
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_gen_z_filter
