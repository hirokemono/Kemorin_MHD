!>@file   write_ctl_item_routines.f90
!!        module write_ctl_item_routines
!!
!!@author H. Matsui
!!@date Programmed in June, 2021
!!
!>@brief  Module to generate routines for control arrays
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!      subroutine write_ctl_item_struct(id_file, type_list, c_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!
!!       Example
!!  !>       Structure of control item
!!        type ctl_item_crii
!!  !>          Read flag
!!  !!           (If item is read iflag = 1)
!!          integer(kind = kint) :: iflag = 0
!!  !
!!  !>        1-th character item
!!          character(len=kchara) :: c_item_1
!!  !>        2-th real item
!!          real(kind = kreal) :: r_item_2
!!  !>        3-th integer item
!!          integer(kind = kint) :: i_item_3
!!  !>        4-th integer item
!!          integer(kind = kint) :: i_item_4
!!        end type ctl_item_crii
!! ----------------------------------------------------------------------
!!      subroutine write_ctl_item_comments(id_file, type_list, c_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!
!!       Example
!!  !!      subroutine read_ctl_item_crii(c_buf, label, ctl_item)
!!  !!        type(buffer_for_control), intent(in) :: c_buf
!!  !!        type(ctl_item_crii), intent(inout) :: ctl_item
!!  !!      subroutine write_ctl_item_crii    &
!!  !!     &         (id_file, level, maxlen, label, ctl_item)
!!  !!        type(ctl_item_crii), intent(in) :: ctl_item
!!  !!      subroutine reset_ctl_item_crii(ctl_item)
!!  !!      subroutine bcast_ctl_item_crii(ctl_item)
!!  !!        type(ctl_item_crii), intent(inout) :: ctl_item
!!  !!      subroutine copy_ctl_item_crii(org_item, new_item)
!!  !!        type(ctl_item_crii), intent(in) :: org_item
!!  !!        type(ctl_item_crii), intent(inout) :: new_item
!! ----------------------------------------------------------------------
!! ----------------------------------------------------------------------
!!      subroutine write_read_item_routine(id_file, type_list, c_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!
!!       Example
!!        subroutine read_ctl_item_crii(c_buf, label, ctl_item)
!!  !
!!        use t_read_control_elements
!!  !
!!        type(buffer_for_control), intent(in) :: c_buf
!!        character(len=kchara), intent(in) :: label
!!        type(ctl_item_crii), intent(inout) :: ctl_item
!!  !
!!        character(len=kchara) :: tmpchara
!!  !
!!  !
!!        if(ctl_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!!  !
!!        read(c_buf%ctl_buffer,*) tmpchara, &
!!       &     ctl_item%c_item_1,  &
!!       &     ctl_item%r_item_2,  &
!!       &     ctl_item%i_item_3,  &
!!       &     ctl_item%i_item_4
!!        ctl_item%iflag = 1
!!  !
!!        if(iflag_debug .gt. 0) write(*,*) trim(c_buf%header_chara), ': ', &
!!       &     ctl_item%c_item_1,  &
!!       &     ctl_item%r_item_2,  &
!!       &     ctl_item%i_item_3,  &
!!       &     ctl_item%i_item_4
!!  !
!!        end subroutine read_ctl_item_crii
!! ----------------------------------------------------------------------
!!      subroutine write_write_item_routine(id_file, type_list, c_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!
!!       Example
!!        subroutine write_ctl_item_crii    &
!!       &         (id_file, level, maxlen, label, ctl_item)
!!  !
!!        use skip_comment_f
!!  !
!!        integer(kind = kint), intent(in) :: id_file, level
!!        integer(kind = kint), intent(in) :: maxlen
!!        character(len=kchara), intent(in) :: label
!!        type(ctl_item_crii), intent(in) :: ctl_item
!!  !
!!  !
!!        if(ctl_item%iflag .eq. 0) return
!!  !
!!        call write_space_4_parse(id_file, level)
!!        call write_ctl_fixlen_chara(id_file, maxlen, label)
!!        call write_ctl_chara_cont(id_file, ctl_item%c_item_1)
!!        call write_ctl_real_cont(id_file, ctl_item%r_item_2)
!!        call write_ctl_integer_cont(id_file, ctl_item%i_item_3)
!!        call write_ctl_integer_cont(id_file, ctl_item%i_item_4)
!!        write(id_file,'(a1)', ADVANCE='NO') char(10)
!!  !
!!        end subroutine write_ctl_item_crii
!! ----------------------------------------------------------------------
!!      subroutine write_reset_item_routine(id_file, type_list, c_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!
!!       Example
!!        subroutine reset_ctl_item_crii(ctl_item)
!!  !
!!        type(ctl_item_crii), intent(inout) :: ctl_item
!!  !
!!  !
!!        ctl_item%iflag = 0
!!  !
!!        end subroutine reset_ctl_item_crii
!! ----------------------------------------------------------------------
!!      subroutine write_bcast_item_routine(id_file, type_list, c_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!
!!       Example
!!        subroutine bcast_ctl_item_crii(ctl_item)
!!  !
!!        use calypso_mpi
!!        use calypso_mpi_int
!!        use calypso_mpi_real
!!        use calypso_mpi_char
!!        use transfer_to_long_integers
!!  !
!!        type(ctl_item_crii), intent(inout) :: ctl_item
!!  !
!!  !
!!        if(nprocs .eq. 1) return
!!  !
!!        call calypso_mpi_bcast_one_int(ctl_item%iflag, 0)
!!  !
!!        call calypso_mpi_bcast_character                              &
!!       &   (ctl_item%c_item_1, cast_long(kchara), 0)
!!        call calypso_mpi_bcast_one_real(ctl_item%r_item_2, 0)
!!        call calypso_mpi_bcast_one_int(ctl_item%i_item_3, 0)
!!        call calypso_mpi_bcast_one_int(ctl_item%i_item_4, 0)
!!  !
!!        end subroutine bcast_ctl_item_crii
!! ----------------------------------------------------------------------
!!      subroutine write_copy_item_routine(id_file, type_list, c_WK)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!        type(item_list_names), intent(in) :: c_WK
!!
!!       Example
!!        subroutine copy_ctl_item_crii(org_item, new_item)
!!  !
!!        type(ctl_item_crii), intent(in) :: org_item
!!        type(ctl_item_crii), intent(inout) :: new_item
!!  !
!!  !
!!        new_item%iflag = org_item%iflag
!!  !
!!        new_item%c_item_1 = org_item%c_item_1
!!        new_item%r_item_2 = org_item%r_item_2
!!        new_item%i_item_3 = org_item%i_item_3
!!        new_item%i_item_4 = org_item%i_item_4
!!  !
!!        end subroutine copy_ctl_item_crii
!! ----------------------------------------------------------------------
!!@endverbatim
      module write_ctl_item_routines
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
      subroutine write_ctl_item_struct(id_file, type_list, c_WK)
!
      use m_ctl_item_routine_name
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
!
      integer(kind = kint) :: j
!
!
      write(id_file,'(a)')  '!>       Structure of control item'
      write(id_file,'(2a)') '      type ', trim(c_WK%item_type)
      write(id_file,'(a)')  '!>          Read flag'
      write(id_file,'(a)')  '!!           (If item is read iflag = 1)'
      write(id_file,'(a)')  '        integer(kind = kint) :: iflag = 0'
      write(id_file,'(a)')  '!'
      do j = 1, c_WK%num_item
        if     (type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') then
          write(id_file,'(a,i3,a)')                                     &
     &     '!>      ', j, '-th real item'
          write(id_file,'(2a)')                                         &
     &     '        real(kind = kreal) :: ', trim(c_WK%item_name(j))
        else if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') then
          write(id_file,'(a,i3,a)')                                     &
     &     '!>      ', j, '-th integer item'
          write(id_file,'(2a)')                                         &
     &     '        integer(kind = kint) :: ', trim(c_WK%item_name(j))
        else
          write(id_file,'(a,i3,a)')                                     &
     &     '!>      ', j, '-th character item'
          write(id_file,'(2a)')                                         &
     &     '        character(len=kchara) :: ', trim(c_WK%item_name(j))
        end if
      end do
      write(id_file,'(2a)') '      end type ', trim(c_WK%item_type)
!
      end subroutine write_ctl_item_struct
!
! ----------------------------------------------------------------------
!
      subroutine write_ctl_item_comments(id_file, type_list, c_WK)
!
      use m_ctl_item_routine_name
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
!
      character(len=kchara) :: item_tmp
!
!
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                      trim(read_item_routine(type_list)),         &
     &                      '(c_buf, label, ctl_item)'
      item_tmp = 'buffer_for_control'
      write(id_file,'(3a)') '!!  ', def_in(item_tmp), 'c_buf'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_inout(c_WK%item_type), 'ctl_item'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                      trim(write_item_routine(type_list)),        &
     &                      '    &'
      write(id_file,'(a)')                                              &
     &     '!!     &         (id_file, level, maxlen, label, ctl_item)'
      write(id_file,'(3a)') '!!  ',                                     &
     &                      def_in(c_WK%item_type), 'ctl_item'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                      trim(reset_item_routine(type_list)),        &
     &                      '(ctl_item)'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                      trim(bcast_item_routine(type_list)),        &
     &                     '(ctl_item)'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_inout(c_WK%item_type), 'ctl_item'
      write(id_file,'(4a)') '!!', start_routine,                        &
     &                     trim(copy_item_routine(type_list)),          &
     &                     '(org_item, new_item)'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_in(c_WK%item_type), 'org_item'
      write(id_file,'(3a)') '!!  ',                                     &
     &                     def_inout(c_WK%item_type), 'new_item'
!
      end subroutine write_ctl_item_comments
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_read_item_routine(id_file, type_list, c_WK)
!
      use m_ctl_item_routine_name
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
!
      integer(kind = kint) :: j
      character(len=kchara) :: item_tmp
!
!
      write(id_file,'(3a)') start_routine,                              &
     &                     trim(read_item_routine(type_list)),          &
     &                     '(c_buf, label, ctl_item)'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      use t_read_control_elements'
      write(id_file,'(a)')  '!'
      item_tmp = 'buffer_for_control'
      write(id_file,'(2a)') def_in(item_tmp), 'c_buf'
      write(id_file,'(2a)') '      character(len=kchara), ',            &
     &                    'intent(in) :: label'
      write(id_file,'(4a)') '      type(', trim(c_WK%item_type), '), ', &
     &                     'intent(inout) :: ctl_item'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      character(len=kchara) :: tmpchara'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(3a)') '      if',                                 &
     &       '(ctl_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) ', &
     &       'return'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '      read(c_buf%ctl_buffer,*) tmpchara, &'
      do j = 1, c_WK%num_item-1
        write(id_file,'(3a)')                                           &
     &      '     &     ctl_item%', trim(c_WK%item_name(j)), ',  &'
      end do
      write(id_file, '(2a)')                                            &
     &      '     &     ctl_item%', trim(c_WK%item_name(c_WK%num_item))
      write(id_file,'(a)') '      ctl_item%iflag = 1'
      write(id_file,'(a)') '!'
!
      write(id_file,'(6a)') '      if(iflag_debug .gt. 0) ',            &
     &                      'write(*,*) trim(c_buf%header_chara), ',    &
     &                      char(39), ': ', char(39), ', &'
      do j = 1, c_WK%num_item-1
        write(id_file,'(3a)')                                           &
     &      '     &     ctl_item%', trim(c_WK%item_name(j)), ',  &'
      end do
      write(id_file, '(2a)')                                            &
     &      '     &     ctl_item%', trim(c_WK%item_name(c_WK%num_item))
!
      write(id_file,'(a)') '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(read_item_routine(type_list))
!
      end subroutine write_read_item_routine
!
! ----------------------------------------------------------------------
!
      subroutine write_write_item_routine(id_file, type_list, c_WK)
!
      use m_ctl_item_routine_name
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
!
      integer(kind = kint) :: j
!
      write(id_file,'(3a)') start_routine,                              &
     &                     trim(write_item_routine(type_list)), '    &'
      write(id_file,'(a)')                                              &
     &     '     &         (id_file, level, maxlen, label, ctl_item)'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '      use skip_comment_f'
      write(id_file,'(a)') '!'
      write(id_file,'(a)')                                              &
     &     '      integer(kind = kint), intent(in) :: id_file, level'
      write(id_file,'(a)')                                              &
     &     '      integer(kind = kint), intent(in) :: maxlen'
      write(id_file,'(a)')                                              &
     &     '      character(len=kchara), intent(in) :: label'
      write(id_file,'(2a)') def_in(c_WK%item_type), 'ctl_item'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      if(ctl_item%iflag .eq. 0) return'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')                                              &
     &     '      call write_space_4_parse(id_file, level)'
      write(id_file,'(a)')                                              &
     &     '      call write_ctl_fixlen_chara(id_file, maxlen, label)'
      do j = 1, c_WK%num_item
        if     (type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') then
          write(id_file,'(3a)')                                         &
     &     '      call write_ctl_real_cont(id_file, ctl_item%',         &
     &     trim(c_WK%item_name(j)), ')'
        else if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') then
          write(id_file,'(3a)')                                         &
     &     '      call write_ctl_integer_cont(id_file, ctl_item%',      &
     &     trim(c_WK%item_name(j)), ')'
        else
          write(id_file,'(3a)')                                         &
     &     '      call write_ctl_chara_cont(id_file, ctl_item%',        &
     &     trim(c_WK%item_name(j)), ')'
        end if
      end do
      write(id_file,'(a,a1,a,a1,a,a1,a,a1,a)')                          &
     &        '      write(id_file,', char(39), '(a1)', char(39),       &
     &        ', ADVANCE=', char(39), 'NO', char(39), ') char(10)'
      write(id_file,'(a)') '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(write_item_routine(type_list))
!
      end subroutine write_write_item_routine
!
! ----------------------------------------------------------------------
!
      subroutine write_reset_item_routine(id_file, type_list, c_WK)
!
      use m_ctl_item_routine_name
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
!
!
      write(id_file,'(3a)') start_routine,                              &
     &                trim(reset_item_routine(type_list)), '(ctl_item)'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') def_inout(c_WK%item_type), 'ctl_item'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      ctl_item%iflag = 0'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(reset_item_routine(type_list))
!
      end subroutine write_reset_item_routine
!
! ----------------------------------------------------------------------
!
      subroutine write_bcast_item_routine(id_file, type_list, c_WK)
!
      use m_ctl_item_routine_name
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
!
      integer(kind = kint) :: j
!
!
      write(id_file,'(3a)') start_routine,                              &
     &                trim(bcast_item_routine(type_list)), '(ctl_item)'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      use calypso_mpi'
      write(id_file,'(a)')  '      use calypso_mpi_int'
      do j = 1, c_WK%num_item
        if(type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') then
          write(id_file,'(a)')  '      use calypso_mpi_real'
          exit
        end if
      end do
      do j = 1, c_WK%num_item
        if(type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') cycle
        if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') cycle
        write(id_file,'(a)')  '      use calypso_mpi_char'
        exit
      end do
      write(id_file,'(a)')  '      use transfer_to_long_integers'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') def_inout(c_WK%item_type), 'ctl_item'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      if(nprocs .eq. 1) return'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') '      call calypso_mpi_bcast_one_int',     &
     &                     '(ctl_item%iflag, 0)'
      write(id_file,'(a)')  '!'
      do j = 1, c_WK%num_item
        if     (type_list(j:j).eq.'R' .or. type_list(j:j).eq.'r') then
          write(id_file,'(5a)') '      call ',                          &
     &                        'calypso_mpi_bcast_one_real',             &
     &                     '(ctl_item%', trim(c_WK%item_name(j)),', 0)'
        else if(type_list(j:j).eq.'I' .or. type_list(j:j).eq.'i') then
          write(id_file,'(5a)') '      call ',                          &
     &                        'calypso_mpi_bcast_one_int',              &
     &                     '(ctl_item%', trim(c_WK%item_name(j)),', 0)'
        else
          write(id_file,'(3a)') '      call ',                          &
     &                        'calypso_mpi_bcast_character',            &
     &                        '                                  &'
          write(id_file,'(4a)') '     &   (',                           &
     &                        'ctl_item%', trim(c_WK%item_name(j)),     &
     &                        ', cast_long(kchara), 0)'
        end if
      end do
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(bcast_item_routine(type_list))
!
      end subroutine write_bcast_item_routine
!
! ----------------------------------------------------------------------
!
      subroutine write_copy_item_routine(id_file, type_list, c_WK)
!
      use m_ctl_item_routine_name
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(in) :: c_WK
!
      integer(kind = kint) :: j
!
!
      write(id_file,'(3a)') start_routine,                              &
     &                     trim(copy_item_routine(type_list)),          &
     &                     '(org_item, new_item)'
      write(id_file,'(a)')  '!'
      write(id_file,'(2a)') def_in(c_WK%item_type), 'org_item'
      write(id_file,'(3a)') '      type(', trim(c_WK%item_type),        &
     &                     '), intent(inout) :: new_item'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      new_item%iflag = org_item%iflag'
      write(id_file,'(a)')  '!'
      do j = 1, c_WK%num_item
        write(id_file,'(4a)')                                           &
     &                   '      new_item%', trim(c_WK%item_name(j)),    &
     &                      ' = org_item%', trim(c_WK%item_name(j))
      end do
      write(id_file,'(a)') '!'
      write(id_file,'(2a)') end_routine,                                &
     &                     trim(copy_item_routine(type_list))
!
      end subroutine write_copy_item_routine
!
! ----------------------------------------------------------------------
!
      end module write_ctl_item_routines
