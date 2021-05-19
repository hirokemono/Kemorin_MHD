!>@file   write_control_IO_source.f90
!!        module write_control_IO_source
!!
!!@author H. Matsui
!!@date Programmed in June, 2021
!!
!>@brief  Module to generate control item and array routines
!!
!!@verbatim
!!      subroutine s_write_control_IO_source(id_file, type_list)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: type_list
!!@endverbatim
      module write_control_IO_source
!
      use m_precision
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine s_write_control_IO_source(id_file, type_list)
!
      use m_ctl_item_routine_name
      use m_ctl_array_routine_name
      use set_parallel_file_name
      use write_ctl_item_routines
      use write_ctl_array_routines
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: type_list
!
      integer(kind = kint) :: num_item = 0
!
      type(item_list_names) :: c_WK1
      type(array_table_names) :: a_WK1
!
!
      num_item = len_trim(type_list)
      call alloc_item_list_names(num_item, c_WK1)
      call set_item_list_names(type_list, c_WK1)
!
      call alloc_array_table_names(c_WK1%num_item, a_WK1)
      call set_array_table_names(type_list, a_WK1)
!
!
      write(id_file,'(2a)') '!>@file   ',                               &
     &                      trim(ctl_array_file_name(type_list))
      write(id_file,'(2a)') '!!        module ',                        &
     &                     trim(ctl_array_module_name(type_list))
      write(id_file,'(a)')  '!!'
      write(id_file,'(a)')  '!!@author H. Matsui'
      write(id_file,'(a)')  '!!@date Programmed in June, 2014'
      write(id_file,'(a)')  '!!'
      write(id_file,'(2a)') '!>@brief  Subroutines to read ',           &
     &                     'character control arrays'
      write(id_file,'(a)')  '!!'
!
      write(id_file,'(a)')  '!!@verbatim'
      call write_ctl_item_comments(id_file, type_list, c_WK1)
      write(id_file,'(a)')  '!!'
      call write_ctl_array_comments(id_file, type_list, c_WK1, a_WK1)
      write(id_file,'(a)')  '!!@endverbatim'
!
      write(id_file,'(2a)') '      module ',                            &
     &                     trim(ctl_array_module_name(type_list))
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      use m_precision'
      write(id_file,'(a)')  '      use m_machine_parameter'
      write(id_file,'(a)')  '!'
      write(id_file,'(a)')  '      implicit none'
!
      write(id_file,'(a)')  '!'
      call write_ctl_item_struct(id_file, type_list, c_WK1)
      write(id_file,'(a)')  '!'
      call write_ctl_array_struct(id_file, type_list, a_WK1)
!
      write(id_file,'(a)') separator_text()
      write(id_file,'(a)') '      contains'
      write(id_file,'(a)') separator_text()
!
      call write_read_item_routine(id_file, type_list, c_WK1)
      write(id_file,'(a)') separator_text()
      call write_write_item_routine(id_file, type_list, c_WK1)
      write(id_file,'(a)') separator_text()
      call write_reset_item_routine(id_file, type_list, c_WK1)
      write(id_file,'(a)') separator_text()
      call write_bcast_item_routine(id_file, type_list, c_WK1)
      write(id_file,'(a)') separator_text()
      call write_copy_item_routine(id_file, type_list, c_WK1)
!
      write(id_file,'(a)') double_separator_text()
!
      call write_alloc_array_routine(id_file, type_list, a_WK1)
      write(id_file,'(a)') separator_text()
      call write_dealloc_array_routine(id_file, type_list, a_WK1)
!
      write(id_file,'(a)') double_separator_text()
!
      call write_read_array_routine(id_file, type_list,  c_WK1, a_WK1)
      write(id_file,'(a)') separator_text()
      call write_write_array_routine(id_file, type_list, a_WK1)
      write(id_file,'(a)') separator_text()
      call write_bcast_array_routine(id_file, type_list, a_WK1)
!
      write(id_file,'(a)') separator_text()
      call write_append_array_routine(id_file, type_list, c_WK1, a_WK1)
      write(id_file,'(a)') separator_text()
      call write_dup_array_routine(id_file, type_list, a_WK1)
      write(id_file,'(a)') separator_text()
      call write_copy_array_routine(id_file, type_list, a_WK1)
      write(id_file,'(a)') separator_text()
      call write_append_item_routine(id_file, type_list, c_WK1, a_WK1)
!
      write(id_file,'(a)') separator_text()
      write(id_file,'(2a)') '      end module ',                        &
     &                     trim(ctl_array_module_name(type_list))
!
      call dealloc_array_table_names(a_WK1)
      call dealloc_item_list_names(c_WK1)
!
      end subroutine s_write_control_IO_source
!
! ----------------------------------------------------------------------
!
      end module write_control_IO_source
