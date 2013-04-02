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
      use m_2nd_nod_comm_table
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
      rewind (id_file)
      write (id_file) num_neib_2
      write (id_file) id_neib_2(1:num_neib_2)
      write (id_file) istack_import_2(1:num_neib_2)
      write (id_file) item_import_2(1:ntot_import_2)
!
      end subroutine write_node_import_to_work
!
!   --------------------------------------------------------------------
!
      subroutine write_node_export_to_work(id_file)
!
      use m_2nd_nod_comm_table
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
      write (id_file) istack_export_2(1:num_neib_2)
      write (id_file) item_export_2(1:ntot_export_2)
!
      end subroutine write_node_export_to_work
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_node_import_from_work(id_file)
!
      use m_2nd_nod_comm_table
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      rewind (id_file)
      read (id_file) num_neib_2
!
      call allocate_2nd_neib_id
      call allocate_2nd_nod_import_num
!
      read (id_file) id_neib_2(1:num_neib_2)
      read (id_file) istack_import_2(1:num_neib_2)
!
      ntot_import_2 =      istack_import_2(num_neib_2)
      call allocate_2nd_nod_import_item
!
      read (id_file) item_import_2(1:ntot_import_2)
!
      do i = 1, num_neib_2
        num_import_2(i) = istack_import_2(i) - istack_import_2(i-1)
      end do
!
      end subroutine read_node_import_from_work
!
!   --------------------------------------------------------------------
!
      subroutine read_node_export_from_work(id_file)
!
      use m_2nd_nod_comm_table
      use m_partitioner_comm_table
!
      integer(kind = kint) :: i
!
      integer(kind = kint), intent(in) :: id_file
!
      call allocate_2nd_nod_export_num
      read (id_file) istack_export_2(1:num_neib_2)
!
      ntot_export_2 =      istack_export_2(num_neib_2)
      call allocate_2nd_nod_export_item
!
      read (id_file) item_export_2(1:ntot_export_2)
!
        do i = 1, num_neib_2
          num_export_2(i) = istack_export_2(i) - istack_export_2(i-1)
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
