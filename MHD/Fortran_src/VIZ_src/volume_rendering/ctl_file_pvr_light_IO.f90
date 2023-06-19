!>@file   ctl_file_pvr_light_IO.f90
!!@brief  module ctl_file_pvr_light_IO
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR lightin parameter file
!!
!!@verbatim
!!      subroutine sel_read_ctl_pvr_light_file                          &
!!     &         (id_control, hd_block, file_name, light, c_buf)
!!      subroutine sel_write_ctl_pvr_light_file                         &
!!     &         (id_control, hd_block, file_name, light, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_light_ctl), intent(in) :: light
!!        character(len = kchara), intent(inout) :: file_name
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine read_control_pvr_light_file(id_control, file_name,   &
!!     &                                       hd_block, light)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_light_ctl), intent(inout) :: light
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_pvr_light_file(id_control, file_name,  &
!!     &                                        hd_block, light)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_light_ctl), intent(in) :: light
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Input example
!
!!  begin lighting_ctl
!!    array position_of_lights    4
!!      position_of_lights    0.0   0.0    0.0   end
!!      position_of_lights  -10.0   0.0  -10.0   end
!!      position_of_lights  -10.0   0.0    0.0   end
!!      position_of_lights    0.0  10.0    0.0   end
!!    end array position_of_lights
!!!
!!    ambient_coef_ctl              0.5
!!    diffuse_coef_ctl              5.6
!!    specular_coef_ctl             0.8
!!  end lighting_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
!
      module ctl_file_pvr_light_IO
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_ctl_data_pvr_light
      use t_read_control_elements
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_ctl_pvr_light_file                            &
     &         (id_control, hd_block, file_name, light, c_buf)
!
      use write_control_elements
      use write_control_items
      use ctl_data_view_transfer_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(inout) :: file_name
      type(pvr_light_ctl), intent(inout) :: light
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
!
        call write_space_4_parse(id_monitor, c_buf%level)
        write(id_monitor,'(3a)', ADVANCE='NO')                          &
     &          'Read file for ', trim(hd_block), '... '
        file_name = third_word(c_buf)
        call read_control_pvr_light_file(id_control+1, file_name,       &
     &                                   hd_block, light)
        if(light%i_pvr_lighting .ne. 1)                                 &
     &                         c_buf%iend = light%i_pvr_lighting
      else if(check_begin_flag(c_buf, hd_block)) then
!
        call write_space_4_parse(id_monitor, c_buf%level)
        write(id_monitor,'(a)')  'Lighting control is included'
        file_name = 'NO_FILE'
        call read_lighting_ctl(id_control, hd_block, light, c_buf)
      end if
!
      end subroutine sel_read_ctl_pvr_light_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_write_ctl_pvr_light_file                           &
     &         (id_control, hd_block, file_name, light, level)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(pvr_light_ctl), intent(in) :: light
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(file_name, 'NO_FILE')) then
        call write_lighting_ctl(id_control, hd_block, light, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(4a)') '!  ', trim(hd_block),                          &
     &        ' should be written to file ... ', trim(file_name)
        call write_lighting_ctl(id_control, hd_block, light, level)
      else
        write(*,'(4a)') 'Write file for ', trim(hd_block),              &
     &                  ' ... ', trim(file_name)
        call write_control_pvr_light_file(id_control+1, file_name,      &
     &                                    hd_block, light)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
      end if
!
      end subroutine sel_write_ctl_pvr_light_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_light_file(id_control, file_name,     &
     &                                       hd_block, light)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(pvr_light_ctl), intent(inout) :: light
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      write(*,*) 'Lighting control: ', trim(file_name)
      open(id_control, file = file_name, status='old')
!
      do 
        call load_one_line_from_control(id_control, hd_block, c_buf1)
        if(c_buf1%iend .gt. 0) exit
!
        call read_lighting_ctl(id_control, hd_block, light, c_buf1)
        if(light%i_pvr_lighting .gt. 0) exit
      end do
      close(id_control)
      if(c_buf1%iend .gt. 0) light%i_pvr_lighting = c_buf1%iend
!
      end subroutine read_control_pvr_light_file
!
!  ---------------------------------------------------------------------
!
      subroutine write_control_pvr_light_file(id_control, file_name,    &
     &                                        hd_block, light)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(pvr_light_ctl), intent(in) :: light
!
      integer(kind = kint) :: level
!
!
      open(id_control, file = file_name)
!
      level = 0
      call write_lighting_ctl(id_control, hd_block, light, level)
      close(id_control)
!
      end subroutine write_control_pvr_light_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_pvr_light_IO
