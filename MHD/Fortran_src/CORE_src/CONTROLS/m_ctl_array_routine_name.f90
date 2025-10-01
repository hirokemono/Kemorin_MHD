!>@file   m_ctl_array_routine_name.f90
!!        module m_ctl_array_routine_name
!!
!!@author H. Matsui
!!@date Programmed in June, 2021
!!
!>@brief  Module to construct subroutine names of control array access
!!
!!@verbatim
!!      subroutine alloc_array_table_names(num, a_WK)
!!      subroutine dealloc_array_table_names(a_WK)
!!        integer(kind = kint), intent(in) :: num
!!        type(array_table_names), intent(inout) :: a_WK
!!      subroutine set_array_table_names(type_list, a_WK)
!!        character(len=kchara), intent(in) :: type_list
!!        type(array_table_names), intent(inout) :: a_WK
!!
!!      character(len=kchara) function ctl_array_struct_name(type_list)
!!      character(len=kchara) function ctl_array_module_name(type_list)
!!      character(len=kchara) function ctl_array_file_name(type_list)
!!        character(len=kchara), intent(in) :: type_list
!!
!!      character(len=kchara) function read_item_routine(type_list)
!!      character(len=kchara) function write_item_routine(type_list)
!!      character(len=kchara) function reset_item_routine(type_list)
!!      character(len=kchara) function bcast_item_routine(type_list)
!!      character(len=kchara) function copy_item_routine(type_list)
!!        character(len=kchara), intent(in) :: type_list
!!
!!      character(len=kchara) function alloc_array_routine(type_list)
!!      character(len=kchara) function dealloc_array_routine(type_list)
!!      character(len=kchara) function read_array_routine(type_list)
!!      character(len=kchara) function write_array_routine(type_list)
!!      character(len=kchara) function bcast_array_routine(type_list)
!!      character(len=kchara) function append_array_routine(type_list)
!!      character(len=kchara) function dup_array_routine(type_list)
!!      character(len=kchara) function copy_array_routine(type_list)
!!      character(len=kchara) function append_item_routine(type_list)
!!        character(len=kchara), intent(in) :: type_list
!!@endverbatim
!
      module m_ctl_array_routine_name
!
      use m_precision
      implicit none
!
!>      Structure of control array item names
      type array_table_names
!>        Structure name
        character(len=kchara) :: array_type
!
!>        Array items
        integer(kind = kint) :: num_item = 0
!>        Array item names
        character(len=kchara), allocatable :: table_name(:)
!>        Array item for names of maximum character length
        character(len=kchara), allocatable :: maxlen_name(:)
      end type array_table_names
!
      character(len=kchara), parameter, private                         &
     &            :: array_name_prefix =    'ctl_array_'
!
      character(len=kchara), parameter, private                         &
     &            :: ctl_array_mod_prefix = 't_control_array_'
!
      character(len=kchara), parameter, private                         &
     &            :: alloc_array_prefix =   'alloc_control_array_'
      character(len=kchara), parameter, private                         &
     &            :: dealloc_array_prefix = 'dealloc_control_array_'
      character(len=kchara), parameter, private                         &
     &            :: read_array_prefix =    'read_control_array_'
      character(len=kchara), parameter, private                         &
     &            :: write_array_prefix =   'write_control_array_'
      character(len=kchara), parameter, private                         &
     &            :: bcast_array_prefix =   'bcast_control_array_'
      character(len=kchara), parameter, private                         &
     &            :: append_array_prefix =  'append_control_array_'
      character(len=kchara), parameter, private                         &
     &            :: duplicate_prefix =     'dup_control_array_'
      character(len=kchara), parameter, private                         &
     &            :: copy_array_prefix =    'copy_control_array_'
      character(len=kchara), parameter, private                         &
     &            :: append_item_prefix =   'append_control_item_'
!
! ------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_array_table_names(num, a_WK)
!
      integer(kind = kint), intent(in) :: num
      type(array_table_names), intent(inout) :: a_WK
!
      a_WK%num_item = num
      allocate(a_WK%table_name(a_WK%num_item))
      allocate(a_WK%maxlen_name(a_WK%num_item))
!
      end subroutine alloc_array_table_names
!
!------------------------------------------------------------------
!
      subroutine dealloc_array_table_names(a_WK)
!
      type(array_table_names), intent(inout) :: a_WK
!
      deallocate(a_WK%table_name, a_WK%maxlen_name)
      a_WK%num_item = 0
!
      end subroutine dealloc_array_table_names
!
!------------------------------------------------------------------
!
      subroutine set_array_table_names(type_list, a_WK)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: type_list
      type(array_table_names), intent(inout) :: a_WK
!
      integer(kind = kint) :: j
      character(len=kchara) :: item_tmp
!
!
      a_WK%array_type = ctl_array_struct_name(type_list)
      do j = 1, a_WK%num_item
        write(item_tmp,'(a1,a)') type_list(j:j), '_tbl_'
        a_WK%table_name(j) = append_index(j, item_tmp)
!
        write(item_tmp,'(a)') 'maxlen_'
        a_WK%maxlen_name(j) = append_index(j, item_tmp)
      end do
!
      end subroutine set_array_table_names
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function ctl_array_struct_name(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(ctl_array_struct_name,'(2a)')                               &
     &      trim(array_name_prefix), trim(type_list)
!
      end function ctl_array_struct_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function ctl_array_module_name(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(ctl_array_module_name,'(2a)')                               &
     &      trim(ctl_array_mod_prefix), trim(type_list)
!
      end function ctl_array_module_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function ctl_array_file_name(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(ctl_array_file_name,'(3a)')                                 &
     &      trim(ctl_array_mod_prefix), trim(type_list), '.f90'
!
      end function ctl_array_file_name
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function alloc_array_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(alloc_array_routine,'(2a)')                                 &
     &       trim(alloc_array_prefix), trim(type_list)
!
      end function alloc_array_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function dealloc_array_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(dealloc_array_routine,'(2a)')                               &
     &       trim(dealloc_array_prefix), trim(type_list)
!
      end function dealloc_array_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function read_array_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(read_array_routine,'(2a)')                                  &
     &       trim(read_array_prefix), trim(type_list)
!
      end function read_array_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function write_array_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(write_array_routine,'(2a)')                                 &
     &       trim(write_array_prefix), trim(type_list)
!
      end function write_array_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function bcast_array_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(bcast_array_routine,'(2a)')                                 &
     &       trim(bcast_array_prefix), trim(type_list)
!
      end function bcast_array_routine
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function append_array_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(append_array_routine,'(2a)')                                &
     &       trim(append_array_prefix), trim(type_list)
!
      end function append_array_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function dup_array_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(dup_array_routine,'(2a)')                                   &
     &       trim(duplicate_prefix), trim(type_list)
!
      end function dup_array_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function copy_array_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(copy_array_routine,'(2a)')                                  &
     &       trim(copy_array_prefix), trim(type_list)
!
      end function copy_array_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function append_item_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(append_item_routine,'(2a)')                                 &
     &       trim(append_item_prefix), trim(type_list)
!
      end function append_item_routine
!
!------------------------------------------------------------------
!
      end module m_ctl_array_routine_name
