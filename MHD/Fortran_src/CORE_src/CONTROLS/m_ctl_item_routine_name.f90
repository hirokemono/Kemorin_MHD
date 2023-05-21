!>@file   m_ctl_item_routine_name.f90
!!        module m_ctl_item_routine_name
!!
!!@author H. Matsui
!!@date Programmed in June, 2021
!!
!>@brief  Module to construct subroutine names of control item access
!!
!!@verbatim
!!      character(len=2) function skip_text()
!!      character(len=ilen_separator_text) function separator_text()
!!      character(len=ilen_dbl_separator_text)                          &
!!     &             function double_separator_text()
!!
!!      function def_in(type_name)
!!        character(len=len_trim(type_name)+11+3+14) :: def_in
!!        character(len=kchara), intent(in) :: type_name
!!      function def_inout(type_name)
!!        character(len=kchara), intent(in) :: type_name
!!        character(len=len_trim(type_name)+11+3+17) :: def_inout
!!
!!      character(len=kchara) function ctl_item_struct_name(type_list)
!!      character(len=kchara) function ctl_item_module_name(type_list)
!!      character(len=kchara) function ctl_item_file_name(type_list)
!!        character(len=kchara), intent(in) :: type_list
!!
!!      character(len=kchara) function read_item_routine(type_list)
!!      character(len=kchara) function write_item_routine(type_list)
!!      character(len=kchara) function reset_item_routine(type_list)
!!      character(len=kchara) function bcast_item_routine(type_list)
!!      character(len=kchara) function copy_item_routine(type_list)
!!        character(len=kchara), intent(in) :: type_list
!!@endverbatim
!
      module m_ctl_item_routine_name
!
      use m_precision
      implicit none
!
!>      Structure of control item names
      type item_list_names
!>        Structure name
        character(len=kchara) :: item_type
!
!>        Number of items
        integer(kind = kint) :: num_item = 0
!>        Item names
        character(len=kchara), allocatable :: item_name(:)
      end type item_list_names
!
      integer(kind = kint), parameter                                   &
     &            :: ilen_separator_text = 1+1+72+1+1
      integer(kind = kint), parameter                                   &
     &            :: ilen_dbl_separator_text = 1+1+72+1+72+1+1
!
      character(len=17), parameter :: start_routine                     &
     &                               = '      subroutine '
      character(len=21), parameter :: end_routine                       &
     &                               = '      end subroutine '
!
      character(len=kchara), parameter, private                         &
     &            :: ctl_item_mod_prefix =  't_control_item_'
      character(len=kchara), parameter, private                         &
     &            :: ctl_array_mod_prefix = 't_control_array_'
!
      character(len=kchara), parameter, private                         &
     &            :: read_item_prefix =     'read_ctl_item_'
      character(len=kchara), parameter, private                         &
     &            :: write_item_prefix =    'write_ctl_item_'
      character(len=kchara), parameter, private                         &
     &            :: reset_item_prefix =    'reset_ctl_item_'
      character(len=kchara), parameter, private                         &
     &            :: bcast_item_prefix =    'bcast_ctl_item_'
      character(len=kchara), parameter, private                         &
     &            :: copy_item_prefix =     'copy_ctl_item_'
!
      character(len=kchara), parameter, private                         &
     &            :: item_type_prefix =     'ctl_item_'
!
! ------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_item_list_names(num, c_WK)
!
      integer(kind = kint), intent(in) :: num
      type(item_list_names), intent(inout) :: c_WK
!
      c_WK%num_item = num
      allocate(c_WK%item_name(c_WK%num_item))
!
      end subroutine alloc_item_list_names
!
!------------------------------------------------------------------
!
      subroutine dealloc_item_list_names(c_WK)
!
      type(item_list_names), intent(inout) :: c_WK
!
      deallocate(c_WK%item_name)
      c_WK%num_item = 0
!
      end subroutine dealloc_item_list_names
!
!------------------------------------------------------------------
!
      subroutine set_item_list_names(type_list, c_WK)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: type_list
      type(item_list_names), intent(inout) :: c_WK
!
      integer(kind = kint) :: j
      character(len=kchara) :: item_tmp
!
!
      c_WK%item_type = ctl_item_struct_name(type_list)
      do j = 1, c_WK%num_item
        write(item_tmp,'(a1,a)') type_list(j:j), '_item_'
        c_WK%item_name(j) = append_index(j, item_tmp)
      end do
!
      end subroutine set_item_list_names
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=2) function skip_text()
!
      skip_text = '!' // char(10)
!
      end function skip_text
!
! ------------------------------------------------------------------
!
      character(len=ilen_separator_text) function separator_text()
!
      separator_text                                                    &
     &      = '!' // char(10)                                           &
     &     // '! -------------------------------------'                 &
     &     // '---------------------------------' // char(10)           &
     &     // '!'
!
      end function separator_text
!
! ------------------------------------------------------------------
!
      character(len=ilen_dbl_separator_text)                            &
     &             function double_separator_text()
!
      double_separator_text                                             &
     &      = '!' // char(10)                                           &
     &     // '! -------------------------------------'                 &
     &     // '---------------------------------' // char(10)           &
     &     // '! -------------------------------------'                 &
     &     // '---------------------------------' // char(10)           &
     &     // '!'
!
      end function double_separator_text
!
!------------------------------------------------------------------
!
      function def_in(type_name)
!
      character(len=kchara), intent(in) :: type_name
      character(len=len_trim(type_name)+11+3+14) :: def_in
!
      def_in = '      type(' // trim(type_name) // '), '                &
     &               // 'intent(in) :: '
!
      end function def_in
!
!------------------------------------------------------------------
!
      function def_inout(type_name)
!
      character(len=kchara), intent(in) :: type_name
!
      character(len=len_trim(type_name)+11+3+17) :: def_inout
!
      def_inout = '      type(' // trim(type_name) // '), '             &
     &               // 'intent(inout) :: '
!
      end function def_inout
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function ctl_item_struct_name(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(ctl_item_struct_name,'(2a)')                                &
     &      trim(item_type_prefix), trim(type_list)
!
      end function ctl_item_struct_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function ctl_item_module_name(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(ctl_item_module_name,'(2a)')                                &
     &      trim(ctl_item_mod_prefix), trim(type_list)
!
      end function ctl_item_module_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function ctl_item_file_name(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(ctl_item_file_name,'(3a)')                                  &
     &      trim(ctl_item_mod_prefix), trim(type_list), '.f90'
!
      end function ctl_item_file_name
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function read_item_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(read_item_routine,'(2a)')                                   &
     &       trim(read_item_prefix), trim(type_list)
!
      end function read_item_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function write_item_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(write_item_routine,'(2a)')                                  &
     &       trim(write_item_prefix), trim(type_list)
!
      end function write_item_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function reset_item_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(reset_item_routine,'(2a)')                                  &
     &       trim(reset_item_prefix), trim(type_list)
!
      end function reset_item_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function bcast_item_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(bcast_item_routine,'(2a)')                                  &
     &       trim(bcast_item_prefix), trim(type_list)
!
      end function bcast_item_routine
!
!------------------------------------------------------------------
!
      character(len=kchara) function copy_item_routine(type_list)
      character(len=kchara), intent(in) :: type_list
!
      write(copy_item_routine,'(2a)')                                   &
     &       trim(copy_item_prefix), trim(type_list)
!
      end function copy_item_routine
!
!------------------------------------------------------------------
!
      end module m_ctl_item_routine_name
