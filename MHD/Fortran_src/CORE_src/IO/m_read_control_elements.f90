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
!!      subroutine read_real_ctl_item(label, iflag_dat, real_data)
!!      subroutine read_integer_ctl_item(label, iflag_dat, int_data)
!!      subroutine read_character_ctl_item(label, iflag_dat,            &
!!     &           chara_data)
!!
!!      subroutine read_real2_ctl_item(label, iflag_dat, real1, real2)
!!      subroutine read_real3_ctl_item                                  &
!!     &         (label, iflag_dat, real1, real2, real3)
!!      subroutine read_integer3_ctl_item                               &
!!     &         (label, iflag_dat, int1, int2, int3)
!!      subroutine read_character2_ctl_item(label, iflag_dat,           &
!!     &          chara1, chara2)
!!      subroutine read_character3_ctl_item(label, iflag_dat,           &
!!     &          chara1, chara2, chara3)
!!
!!      subroutine read_file_name_from_ctl_line(icou, fname)
!!      subroutine read_file_names_from_ctl_line(num, icou, fname)
!!
!!      subroutine read_control_array_real_list(label, num, icou,       &
!!     &          vec1)
!!      subroutine read_control_array_real2_list(label, num, icou,      &
!!     &          vec1, vec2)
!!      subroutine read_control_array_real3_list(label, num, icou,      &
!!     &          vec1, vec2, vec3)
!!      subroutine read_control_array_int_list(label, num, icou, ivect)
!!      subroutine read_control_array_int2_list(label, num, icou, int1, &
!!     &          int2)
!!      subroutine read_control_array_chara_list(label, num, icou, c_tbl)
!!      subroutine read_control_array_chara2_list(label, num, icou,     &
!!     &          c1_tbl, c2_tbl)
!!      subroutine read_control_array_vect_list(label, num, icou,       &
!!     &          c_tbl, vect)
!!      subroutine read_control_array_int_v_list(label, num, icou,      &
!!     &          c_tbl, ivect)
!!      subroutine read_control_array_c_r2_list(label, num, icou, c_tbl,&
!!     &          vec1, vec2)
!!      subroutine read_control_array_c2_r_list(label, num, icou,       &
!!     &          c1_tbl, c2_tbl, vect)
!!      subroutine read_control_array_i_c_r_list(label, num, icou,      &
!!     &          ivect, c_tbl, vect)
!!      subroutine read_control_array_int_r_list(label, num, icou,      &
!!     &          ivect, vect)
!!      subroutine read_control_array_i2_r_list(label, num, icou, int1, &
!!     &          int2, vect)
!!      subroutine read_control_array_i2_r2_list(label, num, icou, int1,&
!!     &          int2, vec1, vec2)
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
      subroutine read_real_ctl_item(label, iflag_dat, real_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      real(kind = kreal), intent(inout) :: real_data
!
!
      if(iflag_dat.gt.0 .or. c_buf1%header_chara.ne.label) return
!
      read(c_buf1%ctl_buffer,*) c_buf1%header_chara, real_data
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf1%header_chara), real_data
      iflag_dat = 1
!
      end subroutine read_real_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine read_integer_ctl_item(label, iflag_dat, int_data)
!
       character(len=kchara), intent(in) :: label
       integer (kind=kint), intent(inout) :: iflag_dat
       integer (kind=kint), intent(inout) :: int_data
!
!
      if(iflag_dat.gt.0 .or. c_buf1%header_chara.ne.label) return
!
      read(c_buf1%ctl_buffer,*) c_buf1%header_chara, int_data
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf1%header_chara), int_data
      iflag_dat = 1
!
       end subroutine read_integer_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_character_ctl_item(label, iflag_dat,              &
     &           chara_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara_data
!
!
      if(iflag_dat.gt.0 .or. c_buf1%header_chara.ne.label) return
!
      read(c_buf1%ctl_buffer,*) c_buf1%header_chara, chara_data
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf1%header_chara), ': ',     &
     &                                    trim(chara_data)
      iflag_dat = 1
!
      end subroutine read_character_ctl_item
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_real2_ctl_item(label, iflag_dat, real1, real2)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      real(kind = kreal), intent(inout) :: real1, real2
!
!
      if(iflag_dat.gt.0 .or. c_buf1%header_chara.ne.label) return
!
      read(c_buf1%ctl_buffer,*) c_buf1%header_chara, real1, real2
      if (iflag_debug .gt. 0)  write(*,'(a,a2,1p3e16.7)')               &
     &            trim(c_buf1%header_chara), ': ', real1, real2
      iflag_dat = 1
!
       end subroutine read_real2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_real3_ctl_item                                    &
     &         (label, iflag_dat, real1, real2, real3)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      real(kind = kreal), intent(inout) :: real1, real2, real3
!
!
      if(iflag_dat.gt.0 .or. c_buf1%header_chara.ne.label) return
!
      read(c_buf1%ctl_buffer,*) c_buf1%header_chara, real1, real2, real3
      if (iflag_debug .gt. 0)  write(*,'(a,a2,1p3e16.7)')               &
     &            trim(c_buf1%header_chara), ': ', real1, real2, real3
      iflag_dat = 1
!
      end subroutine read_real3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_integer3_ctl_item                                 &
     &         (label, iflag_dat, int1, int2, int3)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      integer (kind=kint), intent(inout) :: int1, int2, int3
!
!
      if(iflag_dat.gt.0 .or. c_buf1%header_chara.ne.label) return
!
      read(c_buf1%ctl_buffer,*) c_buf1%header_chara, int1, int2, int3
      if (iflag_debug .gt. 0)  write(*,'(a,a2,3i6)')                    &
     &            trim(c_buf1%header_chara), ': ', int1, int2, int3
      iflag_dat = 1
!
      end subroutine read_integer3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_character2_ctl_item(label, iflag_dat,             &
     &          chara1, chara2)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara1, chara2
!
!
      if(iflag_dat.gt.0 .or. c_buf1%header_chara.ne.label) return
!
      read(c_buf1%ctl_buffer,*) c_buf1%header_chara, chara1, chara2
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf1%header_chara), ' 1: ', chara1
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf1%header_chara), ' 2: ', chara2
      iflag_dat = 1
!
       end subroutine read_character2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_character3_ctl_item(label, iflag_dat,             &
     &          chara1, chara2, chara3)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara1, chara2, chara3
!
!
      if(iflag_dat.gt.0 .or. c_buf1%header_chara.ne.label) return
!
      read(c_buf1%ctl_buffer,*) c_buf1%header_chara,                    &
     &                         chara1, chara2, chara3
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf1%header_chara), ' 1: ', chara1
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf1%header_chara), ' 2: ', chara2
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf1%header_chara), ' 3: ', chara3
      iflag_dat = 1
!
       end subroutine read_character3_ctl_item
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
       read(c_buf1%ctl_buffer,*) c_buf1%header_chara, tmpchara, fname(icou)
!
       end subroutine read_file_names_from_ctl_line
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int_list(label, num, icou, ivect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      integer (kind=kint), intent(inout) :: ivect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .ge. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara, ivect(icou)
       end if
      end do
!
      end subroutine read_control_array_int_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int_r_list(label, num, icou,        &
     &          ivect, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      integer (kind=kint), intent(inout) :: ivect(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .ge. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara,                        &
     &                            ivect(icou), vect(icou)
        end if
      end do
!
      end subroutine read_control_array_int_r_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_vect_list(label, num, icou,         &
     &          c_tbl, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .ge. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara,                        &
     &           c_tbl(icou), vect(icou)
        end if
      end do
!
      end subroutine read_control_array_vect_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int_v_list(label, num, icou,        &
     &          c_tbl, ivect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      integer (kind=kint), intent(inout) :: ivect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .ge. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara,                        &
     &                            c_tbl(icou), ivect(icou)
        end if
      end do
!
      end subroutine read_control_array_int_v_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_real_list(label, num, icou,        &
     &          vec1)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      real (kind=kreal), intent(inout) :: vec1(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .ge. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara, vec1(icou)
        end if
      end do
!
      end subroutine read_control_array_real_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_real2_list(label, num, icou,        &
     &          vec1, vec2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .ge. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara, vec1(icou), vec2(icou)
        end if
      end do
!
      end subroutine read_control_array_real2_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_real3_list(label, num, icou,        &
     &          vec1, vec2, vec3)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
      real (kind=kreal), intent(inout) :: vec3(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .ge. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara,                &
     &                          vec1(icou), vec2(icou), vec3(icou)
        end if
      end do
!
      end subroutine read_control_array_real3_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara_list(label, num, icou, c_tbl)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara, c_tbl(icou)
        end if
      end do
!
      end subroutine read_control_array_chara_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara2_list(label, num, icou,       &
     &          c1_tbl, c2_tbl)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c1_tbl(num), c2_tbl(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .ge. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara,                        &
     &                           c1_tbl(icou), c2_tbl(icou)
        end if
      end do
!
      end subroutine read_control_array_chara2_list
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c_r2_list(label, num, icou, c_tbl,  &
     &          vec1, vec2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .ge. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara, c_tbl(icou),           &
     &                           vec1(icou), vec2(icou)
        end if
      end do
!
      end subroutine read_control_array_c_r2_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c2_r_list(label, num, icou,         &
     &          c1_tbl, c2_tbl, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c1_tbl(num), c2_tbl(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara,                        &
     &                         c1_tbl(icou), c2_tbl(icou), vect(icou)
        end if
      end do
!
      end subroutine read_control_array_c2_r_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i_c_r_list(label, num, icou,        &
     &          ivect, c_tbl, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      integer(kind = kint), intent(inout) :: ivect(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .ge. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara,                        &
     &                         ivect(icou), c_tbl(icou), vect(icou)
        end if
      end do
!
      end subroutine read_control_array_i_c_r_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int2_list(label, num, icou, int1,   &
     &          int2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: int1(num), int2(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara, int1(icou), int2(icou)
        end if
      end do
!
      end subroutine read_control_array_int2_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i2_r_list(label, num, icou, int1,   &
     &          int2, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: int1(num), int2(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara, int1(icou),            &
     &                            int2(icou), vect(icou)
        end if
      end do
!
      end subroutine read_control_array_i2_r_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i2_r2_list(label, num, icou, int1,  &
     &          int2, vec1, vec2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: int1(num), int2(num)
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(c_buf1%header_chara.eq.label) then
          icou = icou + 1
          read(c_buf1%ctl_buffer,*) c_buf1%header_chara, int1(icou),            &
     &                          int2(icou), vec1(icou), vec2(icou)
        end if
      end do
!
      end subroutine read_control_array_i2_r2_list
!
!   --------------------------------------------------------------------
!
      end module m_read_control_elements
