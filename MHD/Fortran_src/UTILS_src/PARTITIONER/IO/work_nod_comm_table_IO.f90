!work_nod_comm_table_IO.f90
!     module work_nod_comm_table_IO
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine write_node_import_to_work(id_file, new_comm)
!      subroutine write_node_export_to_work(id_file, new_comm)
!
!      subroutine read_node_import_from_work(id_file, new_comm)
!      subroutine read_node_export_from_work(id_file, new_comm)
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
      subroutine write_node_import_to_work(id_file, new_comm)
!
      use t_comm_table
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(in) :: new_comm
!
      rewind (id_file)
      write (id_file) new_comm%num_neib
      write (id_file) new_comm%id_neib(1:new_comm%num_neib)
      write (id_file) new_comm%istack_import(1:new_comm%num_neib)
      write (id_file) new_comm%item_import(1:new_comm%ntot_import)
!
      end subroutine write_node_import_to_work
!
!   --------------------------------------------------------------------
!
      subroutine write_node_export_to_work(id_file, new_comm)
!
      use t_comm_table
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(in) :: new_comm
!
      write (id_file) new_comm%istack_export(1:new_comm%num_neib)
      write (id_file) new_comm%item_export(1:new_comm%ntot_export)
!
      end subroutine write_node_export_to_work
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_node_import_from_work(id_file, new_comm)
!
      use t_comm_table
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(inout) :: new_comm
      integer(kind = kint) :: i
!
      rewind (id_file)
      read (id_file) new_comm%num_neib
!
      call allocate_type_neib_id(new_comm)
      call allocate_type_import_num(new_comm)
!
      read (id_file) new_comm%id_neib(1:new_comm%num_neib)
      read (id_file) new_comm%istack_import(1:new_comm%num_neib)
!
      new_comm%ntot_import = new_comm%istack_import(new_comm%num_neib)
      call allocate_type_import_item(new_comm)
!
      read (id_file) new_comm%item_import(1:new_comm%ntot_import)
!
      do i = 1, new_comm%num_neib
        new_comm%num_import(i)                                          &
     &       = new_comm%istack_import(i) - new_comm%istack_import(i-1)
      end do
!
      end subroutine read_node_import_from_work
!
!   --------------------------------------------------------------------
!
      subroutine read_node_export_from_work(id_file, new_comm)
!
      use t_comm_table
!
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: i
!
!
      call allocate_type_export_num(new_comm)
      read (id_file) new_comm%istack_export(1:new_comm%num_neib)
!
      new_comm%ntot_export = new_comm%istack_export(new_comm%num_neib)
      call allocate_type_export_item(new_comm)
!
      read (id_file) new_comm%item_export(1:new_comm%ntot_export)
!
        do i = 1, new_comm%num_neib
          new_comm%num_export(i)                                        &
     &       = new_comm%istack_export(i) - new_comm%istack_export(i-1)
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
