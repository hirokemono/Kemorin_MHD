!work_nod_comm_table_IO.f90
!     module work_nod_comm_table_IO
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine write_node_import_to_work(id_file)
!      subroutine write_node_export_to_work(id_file)
!
!      subroutine read_node_import_from_work(id_file)
!      subroutine read_node_export_from_work(id_file)
!
!      subroutine read_node_import_num_tmp(id_file)
!      subroutine read_node_import_item_tmp(id_file)
!
      module work_nod_comm_table_IO
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_node_import_to_work(id_file)
!
      use m_2nd_geometry_data
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
      rewind (id_file)
      write (id_file) comm_2nd%num_neib
      write (id_file) comm_2nd%id_neib(1:comm_2nd%num_neib)
      write (id_file) comm_2nd%istack_import(1:comm_2nd%num_neib)
      write (id_file) comm_2nd%item_import(1:comm_2nd%ntot_import)
!
      end subroutine write_node_import_to_work
!
!   --------------------------------------------------------------------
!
      subroutine write_node_export_to_work(id_file)
!
      use m_2nd_geometry_data
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
      write (id_file) comm_2nd%istack_export(1:comm_2nd%num_neib)
      write (id_file) comm_2nd%item_export(1:comm_2nd%ntot_export)
!
      end subroutine write_node_export_to_work
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_node_import_from_work(id_file)
!
      use m_2nd_geometry_data
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      rewind (id_file)
      read (id_file) comm_2nd%num_neib
!
      call allocate_type_neib_id(comm_2nd)
      call allocate_type_import_num(comm_2nd)
!
      read (id_file) comm_2nd%id_neib(1:comm_2nd%num_neib)
      read (id_file) comm_2nd%istack_import(1:comm_2nd%num_neib)
!
      comm_2nd%ntot_import =      comm_2nd%istack_import(comm_2nd%num_neib)
      call allocate_type_import_item(comm_2nd)
!
      read (id_file) comm_2nd%item_import(1:comm_2nd%ntot_import)
!
      do i = 1, comm_2nd%num_neib
        comm_2nd%num_import(i) = comm_2nd%istack_import(i)    &
     &                          - comm_2nd%istack_import(i-1)
      end do
!
      end subroutine read_node_import_from_work
!
!   --------------------------------------------------------------------
!
      subroutine read_node_export_from_work(id_file)
!
      use m_2nd_geometry_data
      use m_partitioner_comm_table
!
      integer(kind = kint) :: i
!
      integer(kind = kint), intent(in) :: id_file
!
      call allocate_type_export_num(comm_2nd)
      read (id_file) comm_2nd%istack_export(1:comm_2nd%num_neib)
!
      comm_2nd%ntot_export =      comm_2nd%istack_export(comm_2nd%num_neib)
      call allocate_type_export_item(comm_2nd)
!
      read (id_file) comm_2nd%item_export(1:comm_2nd%ntot_export)
!
        do i = 1, comm_2nd%num_neib
          comm_2nd%num_export(i) = comm_2nd%istack_export(i) - comm_2nd%istack_export(i-1)
        end do
!
      end subroutine read_node_export_from_work
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_node_import_num_tmp(id_file)
!
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
      rewind (id_file)
      read (id_file) NP_TMP
!
      call allocate_nod_import_num_tmp
!
      read (id_file) NEIB_TMP(1:NP_TMP)
      read (id_file) ISTACK_NOD_TMP(1:NP_TMP)
!
      end subroutine read_node_import_num_tmp
!
!   --------------------------------------------------------------------
!
      subroutine read_node_import_item_tmp(id_file)
!
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call allocate_nod_import_item_tmp
!
      read (id_file) IMPORT_NOD_TMP(1:NTOT_NOD_TMP)
!
      end subroutine read_node_import_item_tmp
!
!   --------------------------------------------------------------------
!
      end module work_nod_comm_table_IO
