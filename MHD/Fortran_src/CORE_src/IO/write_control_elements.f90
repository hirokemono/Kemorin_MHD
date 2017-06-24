!>@file   write_control_elements.f90
!!@brief  module write_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2005
!
!>@brief  Subroutines to read control data
!!
!!@verbatim
!!      subroutine write_control_begin_flag(idepth, ctl_name)
!!      subroutine write_control_end_flag(idepth, ctl_name)
!!      subroutine write_control_array_flag(idepth, ctl_name, num_array)
!!      subroutine write_control_end_array_flag(idepth, ctl_name)
!!
!!      subroutine write_real_ctl_item(idepth, label, real_data)
!!      subroutine write_integer_ctl_item(idepth, label, int_data)
!!      subroutine write_character_ctl_item(idepth, label, chara_data)
!!
!!      subroutine write_real2_ctl_item(idepth, label, real_data)
!!      subroutine write_real3_ctl_item(idepth, label, real_data)
!!      subroutine write_integer3_ctl_item(idepth, label, int_data)
!!      subroutine write_character3_ctl_item(idepth, label,             &
!!     &           chara_data)
!!      subroutine write_control_array_i2_r2_item(idepth, label,        &
!!     &          int1, int2, vec1, vec2)
!!
!!      subroutine write_file_name_from_ctl_line(idepth, label, fname)
!!      subroutine write_file_names_from_ctl_line(idepth, label, num,   &
!!     &          icou, fname)
!!
!!      subroutine write_control_array_real_list(idepth, label, num,    &
!!     &          vec1)
!!      subroutine write_control_array_real2_list(idepth, label, num,   &
!!     &          vec1, vec2)
!!      subroutine write_control_array_real3_list(idepth, label, num,   &
!!     &          vec1, vec2, vec3)
!!      subroutine write_control_array_int_list                         &
!!     &         (idepth, label, num, ivect)
!!      subroutine write_control_array_int2_list(idepth, label, num,    &
!!     &          int1, int2)
!!      subroutine write_control_array_chara_list                       &
!!     &         (idepth, label, num, c_tbl)
!!      subroutine write_control_array_chara2_list(idepth, label, num,  &
!!     &          c1_tbl, c2_tbl)
!!      subroutine write_control_array_chara3_list(idepth, label, num,  &
!!     &          c1_tbl, c2_tbl, c3_tbl)
!!      subroutine write_control_array_vect_list(idepth, label, num,    &
!!     &          c_tbl, vect)
!!      subroutine write_control_array_int_v_list(idepth, label, num,   &
!!     &          c_tbl, ivect)
!!      subroutine write_control_array_c_r2_list(idepth, label, num,    &
!!     &          c_tbl, vec1, vec2)
!!      subroutine write_control_array_c2_r_list(idepth, label, num,    &
!!     &          c1_tbl, c2_tbl, vect)
!!      subroutine write_control_array_i_c_r_list(idepth, label, num,   &
!!     &          ivect, c_tbl, vect)
!!      subroutine write_control_array_int_r_list(idepth, label, num,   &
!!     &          ivect, vect)
!!      subroutine write_control_array_i2_r_list(idepth, label, num,    &
!!     &          int1, int2, vect)
!!      subroutine write_control_array_i2_r2_list(idepth, label, num,   &
!!     &          int1, int2, vec1, vec2)
!!@endverbatim
!!
!!@n @param  ctl_name   label for control block
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
      module write_control_elements
!
      use m_precision
      use m_machine_parameter
      use m_read_control_elements
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_control_begin_flag(idepth, ctl_name)
!
      integer (kind=kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: ctl_name
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(hd_begin), ' ',    &
     &                      trim(ctl_name)
!
      end subroutine write_control_begin_flag
!
!   --------------------------------------------------------------------
!
      subroutine write_control_end_flag(idepth, ctl_name)
!
      integer (kind=kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: ctl_name
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(hd_end), ' ',      &
     &                      trim(ctl_name)
!
      end subroutine write_control_end_flag
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_flag(idepth, ctl_name, num_array)
!
      integer (kind=kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: ctl_name
      integer(kind = kint), intent(in) :: num_array
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(hd_begin), ' ',    &
     &           trim(hd_array), ' ', trim(ctl_name), '  ', num_array
!
      end subroutine write_control_array_flag
!
!   --------------------------------------------------------------------
!
      subroutine write_control_end_array_flag(idepth, ctl_name)
!
      integer (kind=kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: ctl_name
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(hd_end), ' ',      &
     &                       trim(hd_array), ' ', trim(ctl_name)
!
      end subroutine write_control_end_array_flag
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_real_ctl_item(idepth, label, real_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: idepth
      real(kind = kreal), intent(inout) :: real_data
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(label), '  ',      &
     &                       real_data
!
      end subroutine write_real_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_integer_ctl_item(idepth, label, int_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: idepth
      integer (kind=kint), intent(inout) :: int_data
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(label), '  ',      &
     &                       int_data
!
      end subroutine write_integer_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_character_ctl_item(idepth, label, chara_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: idepth
      character(len=kchara), intent(inout) :: chara_data
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(label), '  ',      &
     &                       chara_data
!
       end subroutine write_character_ctl_item
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_real2_ctl_item(idepth, label, real_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: idepth
      real(kind = kreal), intent(inout) :: real_data(2)
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(label), '  ',      &
     &                       real_data(1:2)
!
       end subroutine write_real2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_real3_ctl_item(idepth, label, real_data)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: idepth
      real(kind = kreal), intent(inout) :: real_data(3)
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(label), '  ',      &
     &                       real_data(1:3)
!
      end subroutine write_real3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_integer3_ctl_item(idepth, label, int_data)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: idepth
      integer (kind=kint), intent(inout) :: int_data(3)
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(label), '  ',      &
     &                       int_data(1:3)
!
      end subroutine write_integer3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_character3_ctl_item(idepth, label,               &
     &           chara_data)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: idepth
      character(len=kchara), intent(inout) :: chara_data(3)
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(label), '  ',      &
     &                       trim(chara_data(1)), '  ',                 &
     &                       trim(chara_data(2)), '  ',                 &
     &                       trim(chara_data(3))
!
       end subroutine write_character3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2_r2_item(idepth, label,          &
     &          int1, int2, vec1, vec2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: idepth
      integer(kind = kint), intent(inout) :: int1, int2
      real (kind=kreal), intent(inout) :: vec1, vec2
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(label), '  ',      &
     &                       int1, int2, vec1, vec2
!
      end subroutine write_control_array_i2_r2_item
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_file_name_from_ctl_line(idepth, label, fname)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: idepth
      character(len=kchara), intent(inout) :: fname
!
      integer(kind = kint) :: j
!
!
      write(ctl_file_code,*) ('  ',j=1,idepth), trim(hd_file), ' ',     &
     &                       trim(label), '  ', fname
!
       end subroutine write_file_name_from_ctl_line
!
!   --------------------------------------------------------------------
!
      subroutine write_file_names_from_ctl_line(idepth, label, num,     &
     &          icou, fname)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: idepth
      integer (kind=kint), intent(in) :: num
      integer (kind=kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: fname(num)
!
!
      icou = icou + 1
      call write_file_name_from_ctl_line(idepth, label, fname(icou))
!
       end subroutine write_file_names_from_ctl_line
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_control_array_int_list                           &
     &         (idepth, label, num, ivect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      integer (kind=kint), intent(in) :: ivect(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       ivect(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_int_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_int_r_list(idepth, label, num,     &
     &          ivect, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      integer (kind=kint), intent(in) :: ivect(num)
      real (kind=kreal), intent(in) :: vect(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       ivect(i), vect(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_int_r_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_vect_list(idepth, label, num,      &
     &          c_tbl, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: c_tbl(num)
      real (kind=kreal), intent(in) :: vect(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       c_tbl(i), vect(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_vect_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_int_v_list(idepth, label, num,     &
     &          c_tbl, ivect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: c_tbl(num)
      integer (kind=kint), intent(in) :: ivect(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       c_tbl(i), ivect(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_int_v_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_real_list(idepth, label, num,      &
     &          vec1)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      real (kind=kreal), intent(in) :: vec1(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       vec1(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_real_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_real2_list(idepth, label, num,     &
     &          vec1, vec2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      real (kind=kreal), intent(in) :: vec1(num), vec2(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       vec1(i), vec2(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_real2_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_real3_list(idepth, label, num,     &
     &          vec1, vec2, vec3)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      real (kind=kreal), intent(in) :: vec1(num), vec2(num)
      real (kind=kreal), intent(in) :: vec3(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       vec1(i), vec2(i), vec3(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_real3_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_chara_list                         &
     &         (idepth, label, num, c_tbl)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: c_tbl(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       c_tbl(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_chara_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_chara2_list(idepth, label, num,    &
     &          c1_tbl, c2_tbl)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: c1_tbl(num), c2_tbl(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       c1_tbl(i), c2_tbl(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_chara2_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_chara3_list(idepth, label, num,    &
     &          c1_tbl, c2_tbl, c3_tbl)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: c1_tbl(num), c2_tbl(num)
      character(len=kchara), intent(in) :: c3_tbl(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       c1_tbl(i), c2_tbl(i), c3_tbl(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_chara3_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c_r2_list(idepth, label, num,      &
     &          c_tbl, vec1, vec2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: c_tbl(num)
      real (kind=kreal), intent(in) :: vec1(num), vec2(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       c_tbl(i), vec1(i), vec2(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_c_r2_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c2_r_list(idepth, label, num,      &
     &          c1_tbl, c2_tbl, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: c1_tbl(num), c2_tbl(num)
      real (kind=kreal), intent(in) :: vect(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       c1_tbl(i), c2_tbl(i), vect(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_c2_r_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i_c_r_list(idepth, label, num,     &
     &          ivect, c_tbl, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      character(len=kchara), intent(in) :: c_tbl(num)
      integer(kind = kint), intent(in) :: ivect(num)
      real (kind=kreal), intent(in) :: vect(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       ivect(i), c_tbl(i), vect(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_i_c_r_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_int2_list(idepth, label, num,      &
     &          int1, int2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      integer(kind = kint), intent(in) :: int1(num), int2(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       int1(i), int2(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_int2_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2_r_list(idepth, label, num,      &
     &          int1, int2, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      integer(kind = kint), intent(in) :: int1(num), int2(num)
      real (kind=kreal), intent(in) :: vect(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       int1(i), int2(i), vect(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_i2_r_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2_r2_list(idepth, label, num,     &
     &          int1, int2, vec1, vec2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: idepth
      integer(kind = kint), intent(in) :: int1(num), int2(num)
      real (kind=kreal), intent(in) :: vec1(num), vec2(num)
!
      integer(kind = kint) :: i, j
!
!
      call write_control_array_flag(idepth, label, num)
      do i = 1, num
        write(ctl_file_code,*) ('  ',j=1,idepth+1), trim(label), '  ',  &
     &                       int1(i), int2(i), vec1(i), vec2(i)
      end do
      call write_control_end_array_flag(idepth, label)
!
      end subroutine write_control_array_i2_r2_list
!
!   --------------------------------------------------------------------
!
      end module write_control_elements
