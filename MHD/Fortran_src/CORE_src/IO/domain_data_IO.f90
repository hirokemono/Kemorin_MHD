!>@file   domain_data_IO.f90
!!@brief  module domain_data_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Routine for doimain data IO
!!
!!@verbatim
!!      subroutine read_domain_info(id_file)
!!      subroutine read_import_data(id_file)
!!      subroutine read_export_data(id_file)
!!
!!      subroutine write_domain_info(id_file)
!!      subroutine write_import_data(id_file)
!!      subroutine write_export_data(id_file)
!!@endverbatim
!!
!@param id_file file ID
!
      module domain_data_IO
!
      use m_precision
      use m_constants
!
      use m_comm_data_IO
      use field_data_IO
      use comm_table_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
       subroutine read_domain_info(id_file)
!
       use skip_comment_f
!
       integer(kind = kint), intent(in) :: id_file
!
      character(len=255) :: character_4_read = ''
!
!
       call skip_comment(character_4_read,id_file)
       read(character_4_read,*) my_rank_IO
!
       read(id_file,*) num_neib_domain_IO
!
       call allocate_neib_domain_IO
!
       if (num_neib_domain_IO .gt. 0) then
         read(id_file,*) id_neib_domain_IO(1:num_neib_domain_IO)
       end if
!
       end subroutine read_domain_info
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_import_data(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call allocate_import_stack_IO
!
      if (num_neib_domain_IO .gt. 0) then
!
        call read_arrays_for_stacks(id_file, num_neib_domain_IO,        &
     &      izero, ntot_import_IO, istack_import_IO)
!
        call allocate_import_item_IO
        call read_send_recv_item(id_file, ntot_import_IO,               &
     &      item_import_IO)
      else
        ntot_import_IO = 0
        call allocate_import_item_IO
      end if
!
      end subroutine read_import_data
!
! -----------------------------------------------------------------------
!
      subroutine read_export_data(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call allocate_export_stack_IO
!
      if (num_neib_domain_IO .gt. 0) then
!
        call read_arrays_for_stacks(id_file, num_neib_domain_IO,        &
     &      izero, ntot_export_IO, istack_export_IO)
        call allocate_export_item_IO
        call read_send_recv_item(id_file, ntot_export_IO,               &
     &      item_export_IO)
      else
        ntot_export_IO = 0
        call allocate_export_item_IO
      end if
!
      end subroutine read_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_domain_info(id_file)
!
       integer(kind = kint), intent(in) :: id_file
!
!
!      write(id_file,'(a)') '! '
!      write(id_file,'(a)') '! 1.parallel information'
!      write(id_file,'(a)') '!    domain ID'
!      write(id_file,'(a)') '!    number of domain for transfer'
!      write(id_file,'(a)') '!    domain ID for transfer'
!      write(id_file,'(a)') '! '
!
      write(id_file,'(i16)') my_rank_IO
      write(id_file,'(i16)') num_neib_domain_IO
!
      if (num_neib_domain_IO .gt. 0) then
        write(id_file,'(8i16)') id_neib_domain_IO(1:num_neib_domain_IO)
      else
        write(id_file,'(a)') ''
      end if
!
      call deallocate_neib_domain_IO
!
      end subroutine write_domain_info
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_import_data(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call write_send_recv_data(id_file, num_neib_domain_IO,            &
     &    ntot_import_IO, istack_import_IO, item_import_IO)
!
      call deallocate_import_item_IO
!
      end subroutine write_import_data
!
! -----------------------------------------------------------------------
!
      subroutine write_export_data(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call write_send_recv_data(id_file, num_neib_domain_IO,            &
     &    ntot_export_IO, istack_export_IO, item_export_IO)
!
      call deallocate_export_item_IO
!
      end subroutine write_export_data
!
! -----------------------------------------------------------------------!
      end module domain_data_IO
