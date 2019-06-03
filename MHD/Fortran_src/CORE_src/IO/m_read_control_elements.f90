!>@file   m_read_control_elements.f90
!!@brief  module m_read_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2005
!
!>@brief  Subroutines to read control data
!!
!!@verbatim
!!      subroutine load_ctl_label_and_line
!!      subroutine check_read_control_header
!!      subroutine check_read_control_buffer
!!
!!      integer(kind = kint) function right_begin_flag(label)
!!      integer(kind = kint) function right_file_flag(label)
!!
!!      integer(kind = kint) function find_control_end_flag(label)
!!      subroutine find_control_array_flag(label, num_array)
!!      subroutine find_control_end_array_flag(label, num, iflag_end)
!!
!!      subroutine read_file_name_from_ctl_line(icou, fname)
!!      subroutine read_file_names_from_ctl_line(num, icou, fname)
!!@endverbatim
!!
!!@n @param  label      label for control items
!!@n @param  iflag_end  integer flag for reading block
!!@n @param  iflag_dat  integer flag for reading block
!!@n @param  num_array  size of array block
!!@n @param  num        number of blocks already read
!!@n @param  icou       counter for reading array
!!
!!@n @param real_data     read real data
!!@n @param int_data      read integre data
!!@n @param chara_data    read character data
!!
!!@n @param ivect         integer array data
!!@n @param int1          integer array data
!!@n @param int2          integer array data
!!@n @param vect          real array data
!!@n @param vec1          real array data
!!@n @param vec2          real array data
!!@n @param vec3          real array data
!!@n @param c_tbl         character array data
!!@n @param c1_tbl         character array data
!!@n @param c2_tbl         character array data
!!@n @param c3_tbl         character array data
!
      module m_read_control_elements
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
!
      implicit none
!
!>   control file id
      integer (kind=kint) :: ctl_file_code = 11
!
      type(buffer_for_control), save  :: c_buf1
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine load_ctl_label_and_line
!
!
      call load_one_line_from_control(ctl_file_code, c_buf1)
!
      end subroutine load_ctl_label_and_line
!
!   --------------------------------------------------------------------
!
      subroutine check_read_control_header
!
      call monitor_read_control_label(c_buf1)
!
      end subroutine check_read_control_header
!
!   --------------------------------------------------------------------
!
      subroutine check_read_control_buffer
!
      call monitor_read_control_buffer(c_buf1)
!
      end subroutine check_read_control_buffer
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function right_begin_flag(label)
!
      character(len=kchara), intent(in) :: label
!
!
      right_begin_flag = 0
      if(check_begin_flag(c_buf1, label)) right_begin_flag = 1
!
      end function right_begin_flag
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function right_file_flag(label)
!
      character(len=kchara), intent(in) :: label
!
!
      right_file_flag = 0
      if(check_file_flag(c_buf1, label)) right_file_flag = 1
!
      end function right_file_flag
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function find_control_end_flag(label)
!
      character(len=kchara), intent(in) :: label
!
!
      find_control_end_flag = 0
      if(check_end_flag(c_buf1, label)) find_control_end_flag = 1
!
      end function find_control_end_flag
!
!   --------------------------------------------------------------------
!
      subroutine find_control_array_flag(label, num_array)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: num_array
!
      character(len=kchara)  :: tmpchara, item_name
!
!
!      write(*,*) 'c_buf1%header_chara: ', num_array, trim(c_buf1%header_chara)
      if(num_array .gt. 0) return
!
      if(check_array_flag(c_buf1, label)) then
        read(c_buf1%ctl_buffer,*) tmpchara, item_name, num_array
      end if
!
      end subroutine find_control_array_flag
!
!   --------------------------------------------------------------------
!
      subroutine find_control_end_array_flag(label, num, iflag_end)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(inout) :: iflag_end
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if (check_end_array_flag(c_buf1, label)) iflag = 1
!
      if(iflag .gt. 0) then
        if(iflag_end .lt. num) then
           write(*,*) 'number of array is not enough!'
           stop
        end if
      else
        if(iflag_end .ge. num) then
          write(*,*) ' array should be finished!!'
          iflag_end = num-1
        end if
      end if
!
      end subroutine find_control_end_array_flag
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
       subroutine read_file_name_from_ctl_line(icou, fname)
!
       integer (kind=kint), intent(inout) :: icou
       character(len=kchara), intent(inout) :: fname
!
       character(len=kchara) :: tmpchara
!
!
       if(icou .ge. 1) return
       icou = icou + 1
       read(c_buf1%ctl_buffer,*) c_buf1%header_chara, tmpchara, fname
!
       end subroutine read_file_name_from_ctl_line
!
!   --------------------------------------------------------------------
!
       subroutine read_file_names_from_ctl_line(num, icou, fname)
!
       integer (kind=kint), intent(in) :: num
       integer (kind=kint), intent(inout) :: icou
       character(len=kchara), intent(inout) :: fname(num)
!
       character(len=kchara) :: tmpchara
!
!
       if(icou .ge. num) return
       icou = icou + 1
       read(c_buf1%ctl_buffer,*) c_buf1%header_chara,                   &
     &                          tmpchara, fname(icou)
!
       end subroutine read_file_names_from_ctl_line
!
!   --------------------------------------------------------------------
!
      end module m_read_control_elements
