!work_comm_table_IO.f90
!     module work_comm_table_IO
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine write_all_import_to_work(id_file)
!      subroutine write_all_export_to_work(id_file)
!
!      subroutine read_all_import_from_work(id_file)
!      subroutine read_all_export_from_work(id_file)
!
!      subroutine read_all_import_num_tmp(id_file)
!      subroutine read_all_import_item_tmp(id_file)
!
      module work_comm_table_IO
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
      subroutine write_all_import_to_work(id_file)
!
      use m_2nd_geometry_data
!
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
!
      rewind (id_file)
      write (id_file) comm_2nd%num_neib
      write (id_file) comm_2nd%id_neib(1:comm_2nd%num_neib)
      write (id_file) comm_2nd%istack_import(1:comm_2nd%num_neib)
      write (id_file) ele_comm_2nd%num_import(1:ele_comm_2nd%num_neib)
      write (id_file) surf_comm_2nd%istack_import(1:surf_comm_2nd%num_neib)
      write (id_file) edge_comm_2nd%istack_import(1:edge_comm_2nd%num_neib)
      write (id_file) comm_2nd%item_import(1:comm_2nd%ntot_import)
      write (id_file) ele_comm_2nd%item_import(1:ele_comm_2nd%ntot_import)
      write (id_file) surf_comm_2nd%item_import(1:surf_comm_2nd%ntot_import)
      write (id_file) edge_comm_2nd%item_import(1:edge_comm_2nd%ntot_import)
!
      end subroutine write_all_import_to_work
!
!   --------------------------------------------------------------------
!
      subroutine write_all_export_to_work(id_file)
!
      use m_2nd_geometry_data
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
      write (id_file) comm_2nd%istack_export(1:comm_2nd%num_neib)
      write (id_file) ele_comm_2nd%istack_export(1:ele_comm_2nd%num_neib)
      write (id_file) surf_comm_2nd%istack_export(1:surf_comm_2nd%num_neib)
      write (id_file) edge_comm_2nd%istack_export(1:edge_comm_2nd%num_neib)
      write (id_file) comm_2nd%item_export(1:comm_2nd%ntot_export)
      write (id_file) ele_comm_2nd%item_export(1:ele_comm_2nd%ntot_export)
      write (id_file) surf_comm_2nd%item_export(1:surf_comm_2nd%ntot_export)
      write (id_file) edge_comm_2nd%item_export(1:edge_comm_2nd%ntot_export)
!
      end subroutine write_all_export_to_work
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_all_import_from_work(id_file)
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
      ele_comm_2nd%num_neib =  comm_2nd%num_neib
      surf_comm_2nd%num_neib = comm_2nd%num_neib
      edge_comm_2nd%num_neib = comm_2nd%num_neib
!
      call allocate_type_neib_id(comm_2nd)
      call allocate_type_import_num(comm_2nd)
!
      call allocate_type_neib_id(ele_comm_2nd)
      call allocate_type_import_num(ele_comm_2nd)
!
      call allocate_type_neib_id(surf_comm_2nd)
      call allocate_type_import_num(surf_comm_2nd)
!
      call allocate_type_neib_id(edge_comm_2nd)
      call allocate_type_import_num(edge_comm_2nd)
!
      read (id_file) comm_2nd%id_neib(1:comm_2nd%num_neib)
      read (id_file) comm_2nd%istack_import(1:comm_2nd%num_neib)
      read (id_file) ele_comm_2nd%num_import(1:ele_comm_2nd%num_neib)
      read (id_file) surf_comm_2nd%istack_import(1:surf_comm_2nd%num_neib)
      read (id_file) edge_comm_2nd%istack_import(1:edge_comm_2nd%num_neib)
!
      ele_comm_2nd%id_neib(1:comm_2nd%num_neib) =  comm_2nd%id_neib(1:comm_2nd%num_neib)
      surf_comm_2nd%id_neib(1:comm_2nd%num_neib) = comm_2nd%id_neib(1:comm_2nd%num_neib)
      edge_comm_2nd%id_neib(1:comm_2nd%num_neib) = comm_2nd%id_neib(1:comm_2nd%num_neib)
!
      comm_2nd%ntot_import =      comm_2nd%istack_import(comm_2nd%num_neib)
      ele_comm_2nd%ntot_import =  ele_comm_2nd%num_import(ele_comm_2nd%num_neib)
      surf_comm_2nd%ntot_import = surf_comm_2nd%istack_import(surf_comm_2nd%num_neib)
      edge_comm_2nd%ntot_import = edge_comm_2nd%istack_import(edge_comm_2nd%num_neib)
!
      call allocate_type_import_item(comm_2nd)
      call allocate_type_import_item(ele_comm_2nd)
      call allocate_type_import_item(surf_comm_2nd)
      call allocate_type_import_item(edge_comm_2nd)
!
      read (id_file) comm_2nd%item_import(1:comm_2nd%ntot_import)
      read (id_file) ele_comm_2nd%item_import(1:ele_comm_2nd%ntot_import)
      read (id_file) surf_comm_2nd%item_import(1:surf_comm_2nd%ntot_import)
      read (id_file) edge_comm_2nd%item_import(1:edge_comm_2nd%ntot_import)
!
        do i = 1, comm_2nd%num_neib
          comm_2nd%num_import(i) = comm_2nd%istack_import(i)   &
     &                            - comm_2nd%istack_import(i-1)
        end do
!
        do i = 1, ele_comm_2nd%num_neib
          ele_comm_2nd%num_import(i) = ele_comm_2nd%istack_import(i)      &
     &                          - ele_comm_2nd%istack_import(i-1)
        end do
!
        do i = 1, surf_comm_2nd%num_neib
          surf_comm_2nd%num_import(i) = surf_comm_2nd%istack_import(i)  &
     &                          - surf_comm_2nd%istack_import(i-1)
        end do
!
        do i = 1, edge_comm_2nd%num_neib
          edge_comm_2nd%num_import(i) = edge_comm_2nd%istack_import(i)  &
     &                          - edge_comm_2nd%istack_import(i-1)
        end do
!
      end subroutine read_all_import_from_work
!
!   --------------------------------------------------------------------
!
      subroutine read_all_export_from_work(id_file)
!
      use m_2nd_geometry_data
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      call allocate_type_export_num(comm_2nd)
      call allocate_type_export_num(ele_comm_2nd)
      call allocate_type_export_num(surf_comm_2nd)
      call allocate_type_export_num(edge_comm_2nd)
!
      read (id_file) comm_2nd%istack_export(1:comm_2nd%num_neib)
      read (id_file) ele_comm_2nd%istack_export(1:ele_comm_2nd%num_neib)
      read (id_file) surf_comm_2nd%istack_export(1:surf_comm_2nd%num_neib)
      read (id_file) edge_comm_2nd%istack_export(1:edge_comm_2nd%num_neib)
!
      comm_2nd%ntot_export =      comm_2nd%istack_export(comm_2nd%num_neib)
      ele_comm_2nd%ntot_export =  ele_comm_2nd%istack_export(ele_comm_2nd%num_neib)
      surf_comm_2nd%ntot_export = surf_comm_2nd%istack_export(surf_comm_2nd%num_neib)
      edge_comm_2nd%ntot_export = edge_comm_2nd%istack_export(edge_comm_2nd%num_neib)
!
      call allocate_type_export_item(comm_2nd)
      call allocate_type_export_item(ele_comm_2nd)
      call allocate_type_export_item(surf_comm_2nd)
      call allocate_type_export_item(edge_comm_2nd)
!
      read (id_file) comm_2nd%item_export(1:comm_2nd%ntot_export)
      read (id_file) ele_comm_2nd%item_export(1:ele_comm_2nd%ntot_export)
      read (id_file) surf_comm_2nd%item_export(1:surf_comm_2nd%ntot_export)
      read (id_file) edge_comm_2nd%item_export(1:edge_comm_2nd%ntot_export)
!
        do i = 1, comm_2nd%num_neib
          comm_2nd%num_export(i) = comm_2nd%istack_export(i) - comm_2nd%istack_export(i-1)
        end do
!
        do i = 1, ele_comm_2nd%num_neib
          ele_comm_2nd%num_export(i) = ele_comm_2nd%istack_export(i)    &
     &                          - ele_comm_2nd%istack_export(i-1)
        end do
!
        do i = 1, surf_comm_2nd%num_neib
          surf_comm_2nd%num_export(i) = surf_comm_2nd%istack_export(i)  &
     &                          - surf_comm_2nd%istack_export(i-1)
        end do
!
        do i = 1, edge_comm_2nd%num_neib
          edge_comm_2nd%num_export(i) = edge_comm_2nd%istack_export(i)  &
     &                          - edge_comm_2nd%istack_export(i-1)
        end do
!
      end subroutine read_all_export_from_work
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_all_import_num_tmp(id_file)
!
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
      rewind (id_file)
      read (id_file) NP_TMP
!
      call allocate_all_import_num_tmp
!
      read (id_file) NEIB_TMP(1:NP_TMP)
      read (id_file) ISTACK_NOD_TMP(1:NP_TMP)
      read (id_file) ISTACK_ELE_TMP(1:NP_TMP)
      read (id_file) ISTACK_SURF_TMP(1:NP_TMP)
      read (id_file) ISTACK_EDGE_TMP(1:NP_TMP)
!
      end subroutine read_all_import_num_tmp
!
!   --------------------------------------------------------------------
!
      subroutine read_all_import_item_tmp(id_file)
!
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call allocate_all_import_item_tmp
!
      read (id_file) IMPORT_NOD_TMP(1:NTOT_NOD_TMP)
      read (id_file) IMPORT_ELE_TMP(1:NTOT_ELE_TMP)
      read (id_file) IMPORT_SURF_TMP(1:NTOT_SURF_TMP)
      read (id_file) IMPORT_EDGE_TMP(1:NTOT_EDGE_TMP)
!
      end subroutine read_all_import_item_tmp
!
!   --------------------------------------------------------------------
!
      end module work_comm_table_IO
