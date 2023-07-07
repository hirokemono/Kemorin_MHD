!>@file   t_control_data_LIC_kernel.f90
!!@brief  module t_control_data_LIC_kernel
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for noise configration for LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine init_kernel_control_label(hd_block, kernel_ctl)
!!      subroutine read_kernel_control_data                             &
!!     &         (id_control, hd_block, kernel_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_block
!!        type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_kernel_control_data                            &
!!     &         (id_control, hd_block, kernel_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_block
!!        type(lic_kernel_ctl), intent(in) :: kernel_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine reset_kernel_control_data(kernel_ctl)
!!      subroutine copy_kernel_control_data(org_kernel_c, new_kernel_c)
!!        type(lic_kernel_ctl), intent(in) :: org_kernel_c
!!        type(lic_kernel_ctl), intent(inout) :: new_kernel_c
!!
!!      integer(kind = kint) function num_ctl_label_LIC_kernel()
!!      subroutine set_ctl_label_LIC_kernel(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags  (Not used currently)
!!    kernel_type:             'gaussian' or 'triangle'
!!    trace_length_mode:       'length'  or  'element_count'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin kernel_ctl
!!    kernel_type            'Gaussian'
!!
!!    kernel_resolution          256
!!    peak_position_ctl          0.4
!!    gaussian_width_ctl         0.25
!!
!!    trace_length_mode   'length'
!!    half_length_ctl           0.3
!!    max_trace_count             8
!!  end kernel_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_LIC_kernel
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use skip_comment_f
!
      implicit  none
!
!
      type lic_kernel_ctl
!>        Block name
        character(len=kchara) :: block_name = 'kernel_ctl'
!>         Kernel type name
        type(read_character_item) :: kernel_type_ctl
!
!>         number of grid of half kernel
        type(read_integer_item) :: kernel_resolution_ctl
!
!>         peak position of kernel
        type(read_real_item) :: kernel_peak_ctl
!>         width of Gaussian
        type(read_real_item) :: kernel_sigma_ctl
!
!>         Kernel type name
        type(read_character_item) :: trace_length_mode_ctl
!>         half length of line integration
        type(read_real_item) :: half_length_ctl
!>         number of grid of half kernel
        type(read_integer_item) :: max_trace_count_ctl
!
!>          loaded flag for kernel_ctl
        integer (kind=kint) :: i_kernel_control = 0
      end type lic_kernel_ctl
!
!     3rd level for noise control
      character(len=kchara) :: hd_kernel_type =      'kernel_type'
!
      character(len=kchara) :: hd_kernel_grid_size                      &
     &                        = 'kernel_resolution'
!
      character(len=kchara) :: hd_kernel_peak =  'peak_position_ctl'
      character(len=kchara) :: hd_kernel_sigma = 'gaussian_width_ctl'
!
      character(len=kchara) :: hd_trace_type =  'trace_length_mode'
      character(len=kchara) :: hd_half_length = 'half_length_ctl'
      character(len=kchara) :: hd_trace_count = 'max_trace_count'
!
      integer(kind = kint), parameter :: n_label_LIC_kernel = 7
!
      private :: hd_kernel_type, n_label_LIC_kernel
      private :: hd_kernel_grid_size, hd_trace_type, hd_trace_count
      private :: hd_kernel_sigma, hd_kernel_peak, hd_half_length
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_kernel_control_data                               &
     &         (id_control, hd_block, kernel_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
!
      type(lic_kernel_ctl), intent(inout) :: kernel_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(kernel_ctl%i_kernel_control .gt. 0) return
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_kernel_type, kernel_ctl%kernel_type_ctl)
        call read_chara_ctl_type(c_buf, hd_trace_type,                  &
     &      kernel_ctl%trace_length_mode_ctl)
!
        call read_integer_ctl_type(c_buf, hd_kernel_grid_size,          &
     &      kernel_ctl%kernel_resolution_ctl)
        call read_integer_ctl_type(c_buf, hd_trace_count,               &
     &      kernel_ctl%max_trace_count_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_kernel_sigma, kernel_ctl%kernel_sigma_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_kernel_peak, kernel_ctl%kernel_peak_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_half_length, kernel_ctl%half_length_ctl)
      end do
      kernel_ctl%i_kernel_control = 1
!
      end subroutine read_kernel_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine write_kernel_control_data                              &
     &         (id_control, hd_block, kernel_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
      type(lic_kernel_ctl), intent(in) :: kernel_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(kernel_ctl%i_kernel_control .le. 0) return
!
      maxlen = len_trim(hd_kernel_type)
      maxlen = max(maxlen, len_trim(hd_kernel_grid_size))
      maxlen = max(maxlen, len_trim(hd_kernel_peak))
      maxlen = max(maxlen, len_trim(hd_kernel_sigma))
      maxlen = max(maxlen, len_trim(hd_trace_type))
      maxlen = max(maxlen, len_trim(hd_half_length))
      maxlen = max(maxlen, len_trim(hd_trace_count))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    kernel_ctl%kernel_resolution_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    kernel_ctl%kernel_peak_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    kernel_ctl%kernel_sigma_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    kernel_ctl%trace_length_mode_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    kernel_ctl%half_length_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    kernel_ctl%max_trace_count_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_kernel_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine init_kernel_control_label(hd_block, kernel_ctl)
!
      character(len = kchara), intent(in) :: hd_block
      type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!
!
      kernel_ctl%block_name = hd_block
!
        call init_chara_ctl_item_label                                  &
     &     (hd_kernel_type, kernel_ctl%kernel_type_ctl)
        call init_chara_ctl_item_label(hd_trace_type,                   &
     &      kernel_ctl%trace_length_mode_ctl)
!
        call init_int_ctl_item_label(hd_kernel_grid_size,               &
     &      kernel_ctl%kernel_resolution_ctl)
        call init_int_ctl_item_label(hd_trace_count,                    &
     &      kernel_ctl%max_trace_count_ctl)
!
        call init_real_ctl_item_label                                   &
     &     (hd_kernel_sigma, kernel_ctl%kernel_sigma_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_kernel_peak, kernel_ctl%kernel_peak_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_half_length, kernel_ctl%half_length_ctl)
!
      end subroutine init_kernel_control_label
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_kernel_control_data(kernel_ctl)
!
      type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!
!
      kernel_ctl%kernel_type_ctl%iflag =        0
      kernel_ctl%kernel_resolution_ctl%iflag =  0
      kernel_ctl%kernel_sigma_ctl%iflag =  0
      kernel_ctl%kernel_peak_ctl%iflag =   0
      kernel_ctl%half_length_ctl%iflag =   0
      kernel_ctl%trace_length_mode_ctl%iflag = 0
      kernel_ctl%max_trace_count_ctl%iflag =   0
!
      kernel_ctl%i_kernel_control = 0
!
      end subroutine reset_kernel_control_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_kernel_control_data(org_kernel_c, new_kernel_c)
!
      type(lic_kernel_ctl), intent(in) :: org_kernel_c
      type(lic_kernel_ctl), intent(inout) :: new_kernel_c
!
!
      new_kernel_c%block_name =       org_kernel_c%block_name
      new_kernel_c%i_kernel_control = org_kernel_c%i_kernel_control
!
      call copy_chara_ctl(org_kernel_c%kernel_type_ctl,                 &
     &                    new_kernel_c%kernel_type_ctl)
      call copy_chara_ctl(org_kernel_c%trace_length_mode_ctl,           &
     &                    new_kernel_c%trace_length_mode_ctl)
!
      call copy_integer_ctl(org_kernel_c%kernel_resolution_ctl,         &
     &                      new_kernel_c%kernel_resolution_ctl)
      call copy_integer_ctl(org_kernel_c%max_trace_count_ctl,           &
     &                      new_kernel_c%max_trace_count_ctl)
!
      call copy_real_ctl(org_kernel_c%kernel_sigma_ctl,                 &
     &                   new_kernel_c%kernel_sigma_ctl)
      call copy_real_ctl(org_kernel_c%kernel_peak_ctl,                  &
     &                   new_kernel_c%kernel_peak_ctl)
      call copy_real_ctl(org_kernel_c%half_length_ctl,                  &
     &                   new_kernel_c%half_length_ctl)
!
      end subroutine copy_kernel_control_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_ctl_label_LIC_kernel()
      num_ctl_label_LIC_kernel = n_label_LIC_kernel
      return
      end function num_ctl_label_LIC_kernel
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_LIC_kernel(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_LIC_kernel)
!
      call set_control_labels(hd_kernel_type,      names( 1))
      call set_control_labels(hd_kernel_grid_size, names( 2))
      call set_control_labels(hd_kernel_sigma,     names( 3))
      call set_control_labels(hd_kernel_peak,      names( 4))
      call set_control_labels(hd_trace_type,       names( 5))
      call set_control_labels(hd_half_length,      names( 6))
      call set_control_labels(hd_trace_count,      names( 7))
!
      end subroutine set_ctl_label_LIC_kernel
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_LIC_kernel
