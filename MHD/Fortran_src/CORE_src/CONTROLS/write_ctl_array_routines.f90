!>@file   write_ctl_array_routines.f90
!!        module write_ctl_array_routines
!!
!!@author H. Matsui
!!@date Programmed in June, 2021
!!
!>@brief  Module to generate routines for control arrays
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!      subroutine write_ctl_array_struct(id_file, type_list, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(array_table_names), intent(in) :: a_WK
!!
!!       Example
!!  !>  Structure for character control ctl_array
!!        type ctl_array_crii
!!  !>        Number of ctl_array items
!!          integer(kind=kint) :: num = 0
!!  !>        ctl_array counter
!!          integer(kind=kint) :: icou = 0
!
!!  !>        1-th character ctl_array
!!          character(len=kchara), allocatable :: c_tbl_1(:)
!!  !>        2-th real ctl_array
!!          real(kind = kreal), allocatable :: r_tbl_2(:)
!!  !>        3-th integer ctl_array
!!          integer(kind = kint), allocatable :: i_tbl_3(:)
!!  !>        4-th integer ctl_array
!!          integer(kind = kint), allocatable :: i_tbl_4(:)
!!        end type ctl_array_crii
!! ----------------------------------------------------------------------
!!      subroutine write_ctl_array_comments                             &
!!     &         (id_file, type_list, c_WK, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!        type(array_table_names), intent(in) :: a_WK
!!
!!       Example
!!  !!      subroutine alloc_control_array_crii(ctl_array)
!!  !!      subroutine dealloc_control_array_crii(ctl_array)
!!  !!      subroutine read_control_array_crii    &
!!  !!     &         (id_file, label, ctl_array, c_buf)
!!  !!        type(ctl_array_crii), intent(in) :: ctl_array
!!  !!        type(buffer_for_control), intent(in) :: c_buf
!!  !!      subroutine write_control_array_crii    &
!!  !!     &         (id_file, level, label, ctl_array)
!!  !!      subroutine bcast_control_array_crii(ctl_array)
!!  !!        type(ctl_array_crii), intent(in) :: ctl_array
!!  !!
!!  !!      subroutine append_control_array_crii(add_item, tgt_array)
!!  !!        type(ctl_item_crii), intent(inout) :: add_item
!!  !!        type(ctl_array_crii), intent(inout) :: tgt_array
!!  !!      subroutine dup_control_array_crii(org_array, tgt_array)
!!  !!      subroutine copy_control_array_crii    &
!!  !!     &         (num_copy, org_array, tgt_array)
!!  !!        type(ctl_array_crii), intent(in) :: org_array
!!  !!        type(ctl_array_crii), intent(inout) :: tgt_array
!!  !!      subroutine append_control_item_crii(add_item, tgt_array)
!!  !!        type(ctl_item_crii), intent(in) :: add_item
!!  !!        type(ctl_array_crii), intent(inout) :: tgt_array
!! ----------------------------------------------------------------------
!!      subroutine write_alloc_array_routine(id_file, type_list, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(array_table_names), intent(in) :: a_WK
!!       Example
!!        subroutine alloc_control_array_crii(ctl_array)
!!  !
!!        type(ctl_array_crii), intent(inout) :: ctl_array
!!  !
!!  !
!!        allocate(ctl_array%c_tbl_1(ctl_array%num))
!!        allocate(ctl_array%r_tbl_2(ctl_array%num))
!!        allocate(ctl_array%i_tbl_3(ctl_array%num))
!!        allocate(ctl_array%i_tbl_4(ctl_array%num))
!!  !
!!        if(ctl_array%num .le. 0) return
!!        ctl_array%r_tbl_2(1:ctl_array%num) = 0.0d0
!!        ctl_array%i_tbl_3(1:ctl_array%num) = 0
!!        ctl_array%i_tbl_4(1:ctl_array%num) = 0
!!  !
!!        end subroutine alloc_control_array_crii
!! ----------------------------------------------------------------------
!!      subroutine write_dealloc_array_routine(id_file, type_list, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(array_table_names), intent(in) :: a_WK
!!       Example
!!        subroutine dealloc_control_array_crii(ctl_array)
!!  !
!!        type(ctl_array_crii), intent(inout) :: ctl_array
!!  !
!!  !
!!        if(allocated(ctl_array%c_tbl_1) .eqv. .FALSE.) return
!!        deallocate(ctl_array%c_tbl_1)
!!        deallocate(ctl_array%r_tbl_2)
!!        deallocate(ctl_array%i_tbl_3)
!!        deallocate(ctl_array%i_tbl_4)
!!        ctl_array%num = 0
!!  !
!!        end subroutine dealloc_control_array_crii
!! ----------------------------------------------------------------------
!! ----------------------------------------------------------------------
!!      subroutine write_read_array_routine(id_file, type_list,         &
!!     &                                    c_WK, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!        type(array_table_names), intent(in) :: a_WK
!!
!!       Example
!!        subroutine read_control_array_crii    &
!!       &         (id_file, label, ctl_array, c_buf)
!!  !
!!        use t_read_control_elements
!!  !
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: label
!!        type(ctl_array_crii), intent(inout) :: ctl_array
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!  !
!!        type(ctl_item_crii) :: add_item
!!  !
!!  !
!!        if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!!        if(ctl_array%icou .gt. 0) return
!!  !
!!        add_item%iflag = 0
!!        ctl_array%num =  0
!!        call alloc_control_array_crii(ctl_array)
!!  !
!!        do
!!          call load_one_line_from_control(id_file, label, c_buf)
!!          if(c_buf%iend .gt. 0) exit
!!          if(check_end_array_flag(c_buf, label)) exit
!!  !
!!          if(c_buf%header_chara.eq.label) then
!!            call read_ctl_item_crii(c_buf, label, add_item)
!!            call append_control_array_crii(add_item, ctl_array)
!!          end if
!!        end do
!!  !
!!        end subroutine read_control_array_crii
!! ----------------------------------------------------------------------
!!      subroutine write_write_array_routine(id_file, type_list, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(array_table_names), intent(in) :: a_WK
!!
!!       Example
!!        subroutine write_control_array_crii    &
!!       &         (id_file, level, label, ctl_array)
!!  !
!!        use skip_comment_f
!!        use write_control_elements
!!  !
!!        integer(kind = kint), intent(in) :: id_file, level
!!        character(len=kchara), intent(in) :: label
!!        type(ctl_array_crii), intent(in) :: ctl_array
!!  !
!!        integer(kind = kint) :: maxlen_1 = 0
!!        integer(kind = kint) :: i, length
!!  !
!!  !
!!        if(ctl_array%num .le. 0) return
!!        write(id_file,'(a1)') '!'
!!  !
!!        maxlen_1 = max_len_of_charaarray(ctl_array%num,    &
!!       &                                ctl_array%c_tbl_1)
!!  !
!!        level = write_array_flag_for_ctl(id_file, level, label)
!!        do i = 1, ctl_array%num
!!          length = len_trim(label)
!!          call write_space_4_parse(id_file, level)
!!          call write_ctl_chara_cont(id_file, label)
!!          call write_ctl_fixlen_chara(id_file, maxlen_1,     &
!!       &      ctl_array%c_tbl_1(i))
!!          call write_ctl_real_cont(id_file, ctl_array%r_tbl_2(i))
!!          call write_ctl_integer_cont(id_file, ctl_array%i_tbl_3(i))
!!          call write_ctl_integer_cont(id_file, ctl_array%i_tbl_4(i))
!!          write(id_file,'(a1)', ADVANCE='NO') char(10)
!!        end do
!!        level = write_end_array_flag_for_ctl(id_file, level, label)
!!  !
!!        end subroutine write_control_array_crii
!! ----------------------------------------------------------------------
!!      subroutine write_bcast_array_routine(id_file, type_list, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(array_table_names), intent(in) :: a_WK
!!
!!       Example
!!        subroutine bcast_control_array_crii(ctl_array)
!!  !
!!        use calypso_mpi
!!        use calypso_mpi_int
!!        use calypso_mpi_real
!!        use calypso_mpi_char
!!        use transfer_to_long_integers
!!  !
!!        type(ctl_array_crii), intent(inout) :: ctl_array
!!  !
!!  !
!!        if(nprocs .eq. 1) return
!!  !
!!        call calypso_mpi_bcast_one_int(ctl_array%num, 0)
!!        call calypso_mpi_bcast_one_int(ctl_array%icou, 0)
!!  !
!!        if(my_rank .ne. 0) then
!!          call alloc_control_array_crii(ctl_array)
!!        end if
!!  !
!!        call calypso_mpi_bcast_character                              &
!!       &   (ctl_array%c_tbl_1, cast_long(ctl_array%num*kchara), 0)
!!        call calypso_mpi_bcast_real                                   &
!!       &   (ctl_array%r_tbl_2, cast_long(ctl_array%num), 0)
!!        call calypso_mpi_bcast_int                                    &
!!       &   (ctl_array%i_tbl_3, cast_long(ctl_array%num), 0)
!!        call calypso_mpi_bcast_int                                    &
!!       &   (ctl_array%i_tbl_4, cast_long(ctl_array%num), 0)
!!  !
!!        end subroutine bcast_control_array_crii
!! ----------------------------------------------------------------------
!! ----------------------------------------------------------------------
!!      subroutine write_append_array_routine(id_file, type_list,       &
!!     &                                      c_WK, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!        type(array_table_names), intent(in) :: a_WK
!!
!!       Example
!!        subroutine append_control_array_crii(add_item, tgt_array)
!!  !
!!        type(ctl_item_crii), intent(inout) :: add_item
!!        type(ctl_array_crii), intent(inout) :: tgt_array
!!  !
!!        type(ctl_array_crii) ::    org_array
!!  !
!!  !
!!        org_array%num = tgt_array%num
!!        call alloc_control_array_crii(org_array)
!!        call copy_control_array_crii    &
!!       &   (org_array%num, tgt_array, org_array)
!!        call dealloc_control_array_crii(tgt_array)
!!  !
!!        tgt_array%num = org_array%num + 1
!!        call alloc_control_array_crii(tgt_array)
!!        call copy_control_array_crii    &
!!       &   (org_array%num, org_array, tgt_array)
!!        call append_control_item_crii(add_item, tgt_array)
!!        add_item%iflag = 0
!!  !
!!        call dealloc_control_array_crii(org_array)
!!  !
!!        end subroutine append_control_array_crii
!! ----------------------------------------------------------------------
!!      subroutine write_dup_array_routine(id_file, type_list, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(array_table_names), intent(in) :: a_WK
!!
!!       Example
!!        subroutine dup_control_array_crii(org_array, tgt_array)
!!  !
!!        type(ctl_array_crii), intent(in) :: org_array
!!        type(ctl_array_crii), intent(inout) :: tgt_array
!!  !
!!  !
!!        tgt_array%num = org_array%num
!!        call alloc_control_array_crii(tgt_array)
!!        call copy_control_array_crii    &
!!       &   (org_array%num, org_array, tgt_array)
!!  !
!!        end subroutine dup_control_array_crii
!!
!! ----------------------------------------------------------------------
!!      subroutine write_copy_array_routine(id_file, type_list, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(array_table_names), intent(in) :: a_WK
!!
!!       Example
!!        subroutine copy_control_array_crii    &
!!       &         (num_copy, org_array, tgt_array)
!!  !
!!        integer(kind = kint), intent(in) ::  num_copy
!!        type(ctl_array_crii), intent(in) :: org_array
!!        type(ctl_array_crii), intent(inout) :: tgt_array
!!  !
!!  !
!!        if(num_copy .le. 0) return
!!        tgt_array%icou = org_array%icou
!!        tgt_array%c_tbl_1(1:num_copy) = org_array%c_tbl_1(1:num_copy)
!!        tgt_array%r_tbl_2(1:num_copy) = org_array%r_tbl_2(1:num_copy)
!!        tgt_array%i_tbl_3(1:num_copy) = org_array%i_tbl_3(1:num_copy)
!!        tgt_array%i_tbl_4(1:num_copy) = org_array%i_tbl_4(1:num_copy)
!!  !
!!        end subroutine copy_control_array_crii
!!
!! ----------------------------------------------------------------------
!!      subroutine write_append_item_routine                            &
!!     &         (id_file, type_list, c_WK, a_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!        type(array_table_names), intent(in) :: a_WK
!!
!!       Example
!!        subroutine append_control_item_crii(add_item, tgt_array)
!!  !
!!        type(ctl_item_crii), intent(in) :: add_item
!!        type(ctl_array_crii), intent(inout) :: tgt_array
!!  !
!!  !
!!        tgt_array%icou  = tgt_array%icou + add_item%iflag
!!        tgt_array%c_tbl_1(tgt_array%num) = add_item%c_item_1
!!        tgt_array%r_tbl_2(tgt_array%num) = add_item%r_item_2
!!        tgt_array%i_tbl_3(tgt_array%num) = add_item%i_item_3
!!        tgt_array%i_tbl_4(tgt_array%num) = add_item%i_item_4
!!  !
!!        end subroutine append_control_item_crii
!! ----------------------------------------------------------------------
!!@endverbatim
      module write_ctl_array_routines
!
      use m_precision
      use m_ctl_item_routine_name
      use m_ctl_array_routine_name
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_ctl_array_struct(id_file, type_list, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(array_table_names), intent(in) :: a_WK
!
      integer(kind = kint) :: j
!
!
      write(id_file,'(2a)') '!>  Structure for character ',             &
     &                     'control ctl_array'
      write(id_file,'(2a)') '      type ', trim(a_WK%array_type)
      write(id_file,'(a)')  '!>        Number of ctl_array items'
      write(id_file,'(a)')  '        integer(kind=kint) :: num = 0'
      write(id_file,'(a)')  '!>        ctl_array counter'
      write(id_file,'(a)')  '        integer(kind=kint) :: icou = 0'
      write(id_file,'(a)')  '!'
      do j = 1, a_WK%num_item
        if     (type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') then
          write(id_file,'(a,i3,a)')                                     &
     &     '!>      ', j, '-th real ctl_array'
          write(id_file,'(3a)')                                         &
     &     '        real(kind = kreal), allocatable :: ',               &
     &     trim(a_WK%table_name(j)), '(:)'
        else if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') then
          write(id_file,'(a,i3,a)')                                     &
     &     '!>      ', j, '-th integer ctl_array'
          write(id_file,'(3a)')                                         &
     &     '        integer(kind = kint), allocatable :: ',             &
     &     trim(a_WK%table_name(j)), '(:)'
        else
          write(id_file,'(a,i3,a)')                                     &
     &     '!>      ', j, '-th character ctl_array'
          write(id_file,'(3a)')                                         &
     &     '        character(len=kchara), allocatable :: ',            &
     &     trim(a_WK%table_name(j)), '(:)'
        end if
      end do
      write(id_file,'(2a)') '      end type ', trim(a_WK%array_type)
!
      end subroutine write_ctl_array_struct
!
! ----------------------------------------------------------------------
!
      subroutine write_ctl_array_comments                               &
     &         (id_file, type_list, c_WK, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
      type(array_table_names), intent(in) :: a_WK
!
      character(len=kchara) :: item_tmp
!
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                     trim(alloc_array_routine(type_list)),        &
     &                     '(ctl_array)'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                     trim(dealloc_array_routine(type_list)),      &
     &                    '(ctl_array)'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                     trim(read_array_routine(type_list)),         &
     &                     '    &'
      write(id_file,'(a)')                                              &
     &     '!!     &         (id_file, label, ctl_array, c_buf)'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_in(a_WK%array_type), 'ctl_array'
      item_tmp = 'buffer_for_control'
      write(id_file,'(3a)') '!!  ', def_in(item_tmp), 'c_buf'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                     trim(write_array_routine(type_list)),        &
     &                     '    &'
      write(id_file,'(a)')                                              &
     &     '!!     &         (id_file, level, label, ctl_array)'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                     trim(bcast_array_routine(type_list)),        &
     &                     '(ctl_array)'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_in(a_WK%array_type), 'ctl_array'
      write(id_file,'(a)')  '!!'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                     trim(append_array_routine(type_list)),       &
     &                     '(add_item, tgt_array)'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_inout(c_WK%item_type), 'add_item'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_inout(a_WK%array_type), 'tgt_array'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                     trim(dup_array_routine(type_list)),          &
     &                     '(org_array, tgt_array)'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                      trim(copy_array_routine(type_list)),        &
     &                      '    &'
      write(id_file,'(a)')                                              &
     &     '!!     &         (num_copy, org_array, tgt_array)'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_in(a_WK%array_type), 'org_array'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_inout(a_WK%array_type), 'tgt_array'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                     trim(append_item_routine(type_list)),        &
     &                     '(add_item, tgt_array)'
      write(id_file,'(3a)') '!!  ', def_in(c_WK%item_type), 'add_item'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_inout(a_WK%array_type), 'tgt_array'
!
      end subroutine write_ctl_array_comments
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_alloc_array_routine(id_file, type_list, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(array_table_names), intent(in) :: a_WK
!
      integer(kind = kint) :: j
!
      write(id_file,'(3a)') start_routine,                              &
     &              trim(alloc_array_routine(type_list)), '(ctl_array)'
      write(id_file,'(a)')    '!'
      write(id_file,'(3a)') '      type(', trim(a_WK%array_type),       &
     &                     '), intent(inout) :: ctl_array'
      write(id_file,'(a)')    '!'
      write(id_file,'(a)')    '!'
      do j = 1, a_WK%num_item
        write(id_file,'(3a)') '      allocate(ctl_array%',              &
     &                     trim(a_WK%table_name(j)), '(ctl_array%num))'
      end do
      write(id_file,'(a)')    '!'
      write(id_file,'(a)')    '      if(ctl_array%num .le. 0) return'
      do j = 1, a_WK%num_item
        if     (type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') then
          write(id_file,'(3a)') '      ctl_array%',                     &
     &     trim(a_WK%table_name(j)), '(1:ctl_array%num) = 0.0d0'
        else if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') then
          write(id_file,'(3a)') '      ctl_array%',                     &
     &     trim(a_WK%table_name(j)), '(1:ctl_array%num) = 0'
        end if
      end do
      write(id_file,'(a)')    '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(alloc_array_routine(type_list))
!
      end subroutine write_alloc_array_routine
!
! ----------------------------------------------------------------------
!
      subroutine write_dealloc_array_routine(id_file, type_list, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(array_table_names), intent(in) :: a_WK
!
      integer(kind = kint) :: j
!
      write(id_file,'(3a)') start_routine,                              &
     &                     trim(dealloc_array_routine(type_list)),      &
     &                     '(ctl_array)'
      write(id_file,'(a)')  '!'
      write(id_file,'(3a)') '      type(', trim(a_WK%array_type),       &
     &                     '), intent(inout) :: ctl_array'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(5a)') '      if',                                 &
     &         '(allocated(ctl_array%', trim(a_WK%table_name(1)), ') ', &
     &         '.eqv. .FALSE.) return'
      do j = 1, a_WK%num_item
        write(id_file,'(3a)')                                           &
     &     '      deallocate(ctl_array%', trim(a_WK%table_name(j)), ')'
      end do
      write(id_file,'(a)')                                              &
     &     '      ctl_array%num = 0'
      write(id_file,'(a)') '!'
      write(id_file,'(2a)') end_routine,                                &
     &              trim(dealloc_array_routine(type_list))
!
      end subroutine write_dealloc_array_routine
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_read_array_routine(id_file, type_list,           &
     &                                    c_WK, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
      type(array_table_names), intent(in) :: a_WK
!
!
      write(id_file,'(3a)') start_routine,                              &
     &                     trim(read_array_routine(type_list)), '    &'
      write(id_file,'(a)')                                              &
     &     '     &         (id_file, label, ctl_array, c_buf)'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      use t_read_control_elements'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') '      integer(kind = kint), ',             &
     &                     'intent(in) :: id_file'
      write(id_file,'(2a)') '      character(len=kchara), ',            &
     &                     'intent(in) :: label'
      write(id_file,'(3a)') '      type(', trim(a_WK%array_type),       &
     &                     '), intent(inout) :: ctl_array'
      write(id_file,'(a)')                                              &
     &     '      type(buffer_for_control), intent(inout)  :: c_buf'
      write(id_file,'(a)')  '!'
      write(id_file,'(4a)') '      type(', trim(c_WK%item_type), ') ',  &
     &                     ':: add_item'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') '      if(check_array_flag(c_buf, label) ', &
     &                     '.eqv. .FALSE.) return'
      write(id_file,'(a)')  '      if(ctl_array%icou .gt. 0) return'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      add_item%iflag = 0'
      write(id_file,'(a)')  '      ctl_array%num =  0'
      write(id_file,'(3a)') '      call ',                              &
     &              trim(alloc_array_routine(type_list)), '(ctl_array)'
      write(id_file,'(a)') '!'
      write(id_file,'(a)')                                              &
     &     '      do'
      write(id_file,'(2a)') '        call ',                            &
     &            'load_one_line_from_control(id_file, label, c_buf)'
      write(id_file,'(2a)') '        if',                               &
     &            '(c_buf%iend .gt. 0) exit'
      write(id_file,'(2a)') '        if',                               &
     &            '(check_end_array_flag(c_buf, label)) exit'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') '        if',                               &
     &                     '(c_buf%header_chara.eq.label) then'
      write(id_file,'(3a)') '          call ',                          &
     &               trim(read_item_routine(type_list)),                &
     &               '(c_buf, label, add_item)'
      write(id_file,'(3a)') '          call ',                          &
     &               trim(append_array_routine(type_list)),             &
     &               '(add_item, ctl_array)'
      write(id_file,'(a)')  '        end if'
      write(id_file,'(a)')  '      end do'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(read_array_routine(type_list))
!
      end subroutine write_read_array_routine
!
! ----------------------------------------------------------------------
!
      subroutine write_write_array_routine(id_file, type_list, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(array_table_names), intent(in) :: a_WK
!
      integer(kind = kint) :: j
!
      write(id_file,'(3a)') start_routine,                              &
     &                    trim(write_array_routine(type_list)), '    &'
      write(id_file,'(2a)')  '     &         ',                         &
     &           '(id_file, level, label, ctl_array)'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      use skip_comment_f'
      write(id_file,'(a)')  '      use write_control_elements'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') '      integer(kind = kint), ',             &
     &                     'intent(in) :: id_file, level'
      write(id_file,'(2a)') '      character(len=kchara), ',            &
     &                     'intent(in) :: label'
      write(id_file,'(2a)') def_in(a_WK%array_type), 'ctl_array'
      write(id_file,'(a)')  '!'
      do j = 1, a_WK%num_item
        if(type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') cycle
        if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') cycle
        write(id_file,'(3a)') '      integer(kind = kint) :: ',         &
     &                       trim(a_WK%maxlen_name(j)), ' = 0'
      end do
      write(id_file,'(a)')                                              &
     &     '      integer(kind = kint) :: i, length'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '      if(ctl_array%num .le. 0) return'
      write(id_file,'(a,a1,a,a1,a,a1,a,a1)')                            &
     &     '      write(id_file,', char(39), '(a1)', char(39), ') ',    &
     &     char(39), '!', char(39)
!
      do j = 1, a_WK%num_item
        if(type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') cycle
        if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') cycle
          write(id_file,'(a)') '!'
          write(id_file,'(4a)') '      ', trim(a_WK%maxlen_name(j)),    &
     &                         ' = max_len_of_charaarray',              &
     &                         '(ctl_array%num,                 &'
          write(id_file,'(4a)') '     &                              ', &
     &                    '  ctl_array%', trim(a_WK%table_name(j)), ')'
      end do
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)')                                              &
     &  '      level = write_array_flag_for_ctl(id_file, level, label)'
      write(id_file,'(a)')                                              &
     &  '      do i = 1, ctl_array%num'
      write(id_file,'(a)')                                              &
     &  '        length = len_trim(label)'
      write(id_file,'(a)')                                              &
     &  '        call write_space_4_parse(id_file, level)'
      write(id_file,'(a)')                                              &
     &  '        call write_ctl_chara_cont(id_file, label)'
      do j = 1, a_WK%num_item
        if     (type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') then
          write(id_file,'(3a)')                                         &
     &     '        call write_ctl_real_cont(id_file, ctl_array%',      &
     &     trim(a_WK%table_name(j)), '(i))'
        else if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') then
          write(id_file,'(3a)')                                         &
     &     '        call write_ctl_integer_cont(id_file, ctl_array%',   &
     &     trim(a_WK%table_name(j)), '(i))'
        else
          write(id_file,'(3a)')                                         &
     &     '        call write_ctl_fixlen_chara(id_file, ',             &
     &     trim(a_WK%maxlen_name(j)), ',     &'
          write(id_file,'(3a)')                                         &
     &     '     &      ctl_array%', trim(a_WK%table_name(j)), '(i))'
        end if
      end do
      write(id_file,'(a,a1,a,a1,a,a1,a,a1,a)')                          &
     &        '        write(id_file,', char(39), '(a1)', char(39),     &
     &        ', ADVANCE=', char(39), 'NO', char(39), ') char(10)'
      write(id_file,'(a)')  '      end do'
      write(id_file,'(2a)') '      level = ',                           &
     &     'write_end_array_flag_for_ctl(id_file, level, label)'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(write_array_routine(type_list))
!
      end subroutine write_write_array_routine
!
! ----------------------------------------------------------------------
!
      subroutine write_bcast_array_routine(id_file, type_list, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(array_table_names), intent(in) :: a_WK
!
      integer(kind = kint) :: j
!
      write(id_file,'(3a)') start_routine,                              &
     &              trim(bcast_array_routine(type_list)), '(ctl_array)'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      use calypso_mpi'
      write(id_file,'(a)')  '      use calypso_mpi_int'
      do j = 1, a_WK%num_item
        if(type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') then
          write(id_file,'(a)')  '      use calypso_mpi_real'
          exit
        end if
      end do
      do j = 1, a_WK%num_item
        if(type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') cycle
        if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') cycle
        write(id_file,'(a)')  '      use calypso_mpi_char'
        exit
      end do
      write(id_file,'(a)')  '      use transfer_to_long_integers'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') def_inout(a_WK%array_type), 'ctl_array'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      if(nprocs .eq. 1) return'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') '      call calypso_mpi_bcast_one_int',     &
     &                     '(ctl_array%num, 0)'
      write(id_file,'(2a)') '      call calypso_mpi_bcast_one_int',     &
     &                     '(ctl_array%icou, 0)'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      if(my_rank .ne. 0) then'
      write(id_file,'(3a)') '        call ',                            &
     &                      trim(alloc_array_routine(type_list)),       &
     &                      '(ctl_array)'
      write(id_file,'(a)')  '      end if'
      write(id_file,'(a)')  '!'
      do j = 1, a_WK%num_item
        if     (type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') then
          write(id_file,'(3a)') '      call ',                          &
     &                      'calypso_mpi_bcast_real',                   &
     &                      '                                       &'
          write(id_file,'(4a)') '     &   (',                           &
     &                         'ctl_array%', trim(a_WK%table_name(j)),  &
     &                         ', cast_long(ctl_array%num), 0)'
        else if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') then
          write(id_file,'(3a)') '      call ',                          &
     &                      'calypso_mpi_bcast_int',                    &
     &                      '                                        &'
          write(id_file,'(4a)') '     &   (',                           &
     &                        'ctl_array%', trim(a_WK%table_name(j)),   &
     &                        ', cast_long(ctl_array%num), 0)'
        else
          write(id_file,'(3a)') '      call ',                          &
     &                        'calypso_mpi_bcast_character',            &
     &                        '                                  &'
          write(id_file,'(4a)') '     &   (',                           &
     &                        'ctl_array%', trim(a_WK%table_name(j)),   &
     &                        ', cast_long(ctl_array%num*kchara), 0)'
        end if
      end do
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(bcast_array_routine(type_list))
!
      end subroutine write_bcast_array_routine
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_append_array_routine(id_file, type_list,         &
     &                                      c_WK, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
      type(array_table_names), intent(in) :: a_WK
!
      write(id_file,'(3a)') start_routine,                              &
     &                     trim(append_array_routine(type_list)),       &
     &                     '(add_item, tgt_array)'
      write(id_file,'(a)')  '!'
      write(id_file,'(4a)') '      type(', trim(c_WK%item_type), '), ', &
     &                     'intent(inout) :: add_item'
      write(id_file,'(3a)') '      type(', trim(a_WK%array_type),       &
     &                     '), intent(inout) :: tgt_array'
      write(id_file,'(a)')  '!'
      write(id_file,'(3a)') '      type(', trim(a_WK%array_type),       &
     &                     ') ::    org_array'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      org_array%num = tgt_array%num'
      write(id_file,'(3a)') '      call ',                              &
     &             trim(alloc_array_routine(type_list)), '(org_array)'
      write(id_file,'(3a)') '      call ',                              &
     &             trim(copy_array_routine(type_list)), '    &'
      write(id_file,'(2a)') '     &   ',                                &
     &                     '(org_array%num, tgt_array, org_array)'
      write(id_file,'(3a)') '      call ',                              &
     &            trim(dealloc_array_routine(type_list)), '(tgt_array)'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      tgt_array%num = org_array%num + 1'
      write(id_file,'(3a)') '      call ',                              &
     &              trim(alloc_array_routine(type_list)), '(tgt_array)'
      write(id_file,'(3a)') '      call ',                              &
     &                     trim(copy_array_routine(type_list)), '    &'
      write(id_file,'(3a)') '     &   ',                                &
     &                     '(org_array%num, org_array, tgt_array)'
      write(id_file,'(3a)') '      call ',                              &
     &                     trim(append_item_routine(type_list)),        &
     &                     '(add_item, tgt_array)'
      write(id_file,'(a)')                                              &
     &     '      add_item%iflag = 0'
      write(id_file,'(a)')  '!'
      write(id_file,'(3a)') '      call ',                              &
     &            trim(dealloc_array_routine(type_list)), '(org_array)'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(append_array_routine(type_list))
!
      end subroutine write_append_array_routine
!
! ----------------------------------------------------------------------
!
      subroutine write_dup_array_routine(id_file, type_list, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(array_table_names), intent(in) :: a_WK
!
!
      write(id_file,'(3a)') start_routine,                              &
     &                     trim(dup_array_routine(type_list)),          &
     &                     '(org_array, tgt_array)'
      write(id_file,'(a)') '!'
      write(id_file,'(2a)') def_in(a_WK%array_type),    'org_array'
      write(id_file,'(3a)') def_inout(a_WK%array_type), 'tgt_array'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      tgt_array%num = org_array%num'
      write(id_file,'(3a)') '      call ',                              &
     &              trim(alloc_array_routine(type_list)), '(tgt_array)'
      write(id_file,'(3a)') '      call ',                              &
     &                     trim(copy_array_routine(type_list)), '    &'
      write(id_file,'(2a)') '     &  ',                                 &
     &                     '(org_array%num, org_array, tgt_array)'
      write(id_file,'(a)') '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(dup_array_routine(type_list))
!
      end subroutine write_dup_array_routine
!
! ----------------------------------------------------------------------
!
      subroutine write_copy_array_routine(id_file, type_list, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(array_table_names), intent(in) :: a_WK
!
      integer(kind = kint) :: j
!
      write(id_file,'(3a)') start_routine,                              &
     &                     trim(copy_array_routine(type_list)), '    &'
      write(id_file,'(2a)') '     &         ',                          &
     &                    '(num_copy, org_array, tgt_array)'
      write(id_file,'(2a)') '!'
      write(id_file,'(2a)') '      integer(kind = kint), ',             &
     &                     'intent(in) ::  num_copy'
      write(id_file,'(2a)')  def_in(a_WK%array_type), 'org_array'
      write(id_file,'(3a)') '      type(', trim(a_WK%array_type),       &
     &                     '), intent(inout) :: tgt_array'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '      if(num_copy .le. 0) return'
      write(id_file,'(a)') '      tgt_array%icou = org_array%icou'
      do j = 1, a_WK%num_item
        write(id_file,'(6a)')                                           &
     &   '      tgt_array%', trim(a_WK%table_name(j)), '(1:num_copy) ', &
     &       '= org_array%', trim(a_WK%table_name(j)), '(1:num_copy)'
      end do
      write(id_file,'(a)') '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(copy_array_routine(type_list))
!
      end subroutine write_copy_array_routine
!
! ----------------------------------------------------------------------
!
      subroutine write_append_item_routine                              &
     &         (id_file, type_list, c_WK, a_WK)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
      type(array_table_names), intent(in) :: a_WK
!
      integer(kind = kint) :: j
!
      write(id_file,'(3a)') start_routine,                              &
     &                     trim(append_item_routine(type_list)),        &
     &                     '(add_item, tgt_array)'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)')  def_in(c_WK%item_type), 'add_item'
      write(id_file,'(2a)')  def_inout(a_WK%array_type), 'tgt_array'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') '      tgt_array%icou ',                    &
     &                      ' = tgt_array%icou + add_item%iflag'
      do j = 1, c_WK%num_item
        write(id_file,'(6a)') '      ',                                 &
     &      'tgt_array%', trim(a_WK%table_name(j)), '(tgt_array%num) ', &
     &      '= add_item%', trim(c_WK%item_name(j))
      end do
      write(id_file,'(a)') '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(append_item_routine(type_list))
!
      end subroutine write_append_item_routine
!
! ----------------------------------------------------------------------
!
      end module write_ctl_array_routines
!
