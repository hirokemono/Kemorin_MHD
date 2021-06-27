!
      program control_generation
!
      use m_precision
      use m_ctl_array_routine_name
      use write_control_IO_source
!
      implicit none
!
!
      character(len=kchara), parameter :: type_list1 = 'crii'
      integer(kind = kint), parameter :: id_file1 = 12
!
!
      open(id_file1,file = ctl_array_file_name(type_list1))
      call s_write_control_IO_source(id_file1, type_list1)
      close(id_file1)
!
      end program control_generation
