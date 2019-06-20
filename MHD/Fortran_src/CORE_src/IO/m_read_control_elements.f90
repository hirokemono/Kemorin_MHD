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
!!      integer(kind = kint) function find_control_end_flag(label)
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
      end module m_read_control_elements
