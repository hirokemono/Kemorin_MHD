!>@file   ctl_file_LIC_noise_IO.f90
!!@brief  module ctl_file_LIC_noise_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for noise configration for LIC
!!
!!@verbatim
!!      subroutine sel_read_cube_noise_ctl_file                         &
!!     &         (id_control, hd_block, file_name, noise_ctl, c_buf)
!!      subroutine read_cube_noise_control_file(id_control, file_name,  &
!!     &          hd_block, noise_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(inout) :: file_name
!!        type(cube_noise_ctl), intent(inout) :: noise_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine sel_write_cube_noise_ctl_file                        &
!!     &         (id_control, hd_block, file_name, noise_ctl, level)
!!      subroutine write_cube_noise_control_file(id_control, file_name, &
!!     &                                         noise_ctl)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        type(cube_noise_ctl), intent(in) :: noise_ctl
!!        integer(kind = kint), intent(inout) :: level
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags  (Not used currently)
!!    noise_type:             'external_file' or 'random'
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin cube_noise_ctl
!!    noise_type             'external_file'
!!    noise_file_prefix      'noise/noise_64'
!!    noise_file_format      'gzip'
!!
!!    noise_resolution          256
!!    noise_step_size            20
!!
!!    noise_cube_size          0.4
!!    noise_delta_x            0.01
!!  end cube_noise_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_file_LIC_noise_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_data_LIC_noise
      use skip_comment_f
!
      implicit  none
!
!      character(len=kchara) :: hd_cube_noise =      'cube_noise_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_cube_noise_ctl_file                           &
     &         (id_control, hd_block, file_name, noise_ctl, c_buf)
!
      use t_read_control_elements
!
      use write_control_elements
      use ctl_data_isosurface_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(inout) :: file_name
      type(cube_noise_ctl), intent(inout) :: noise_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
!
        call write_one_ctl_file_message                                 &
     &     (hd_block, c_buf%level, file_name)
        call read_cube_noise_control_file((id_control+2), file_name,    &
     &                                    hd_block, noise_ctl, c_buf)
      else if(check_begin_flag(c_buf, hd_block)) then
        file_name = 'NO_FILE'
!
        call write_included_message(hd_block, c_buf%level)
        call read_cube_noise_control_data(id_control, hd_block,         &
     &                                    noise_ctl, c_buf)
      end if
!
      end subroutine sel_read_cube_noise_ctl_file
!
!   --------------------------------------------------------------------
!
      subroutine read_cube_noise_control_file(id_control, file_name,    &
     &          hd_block, noise_ctl, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len = kchara), intent(in) :: hd_block
      type(cube_noise_ctl), intent(inout) :: noise_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(no_file_flag(file_name)) return
!
      c_buf%level = c_buf%level + 1
      open(id_control, file=file_name, status='old')
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_cube_noise_control_data                               &
     &     (id_control, hd_block, noise_ctl, c_buf)
        if(noise_ctl%i_cube_noise_control .gt. 0) exit
      end do
      close(id_control)
!
      c_buf%level = c_buf%level - 1
!
      end subroutine read_cube_noise_control_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_cube_noise_ctl_file                          &
     &         (id_control, hd_block, file_name, noise_ctl, level)
!
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len = kchara), intent(in) :: hd_block
      type(cube_noise_ctl), intent(in) :: noise_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(no_file_flag(file_name)) then
        call write_cube_noise_control_data(id_control,                  &
     &                                     noise_ctl, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(4a)') '!  ', trim(hd_block),                          &
     &                 ' should be written to ... ', trim(file_name)
        call write_cube_noise_control_data(id_control,                  &
     &                                     noise_ctl, level)
      else
        write(*,'(3a)') trim(hd_block),                                 &
     &                 ' is written file to ... ', trim(file_name)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
        call write_cube_noise_control_file((id_control+2), file_name,   &
     &                                     noise_ctl)
      end if
!
      end subroutine sel_write_cube_noise_ctl_file
!
!   --------------------------------------------------------------------
!
      subroutine write_cube_noise_control_file(id_control, file_name,   &
     &                                         noise_ctl)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      type(cube_noise_ctl), intent(in) :: noise_ctl
!
      integer(kind = kint) :: level
!
      level = 0
      open(id_control, file=file_name)
      call write_cube_noise_control_data                                &
     &     (id_control, noise_ctl, level)
      close(id_control)
!
      end subroutine write_cube_noise_control_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_LIC_noise_IO
