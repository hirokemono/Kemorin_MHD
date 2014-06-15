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
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_geometry_data
!
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
!
      rewind (id_file)
      write (id_file) num_neib_2
      write (id_file) id_neib_2(1:num_neib_2)
      write (id_file) istack_import_2(1:num_neib_2)
      write (id_file) istack_import_ele_2(1:num_neib_ele_2)
      write (id_file) istack_import_surf_2(1:num_neib_surf_2)
      write (id_file) edge_comm_2nd%istack_import(1:edge_comm_2nd%num_neib)
      write (id_file) item_import_2(1:ntot_import_2)
      write (id_file) item_import_ele_2(1:ntot_import_ele_2)
      write (id_file) item_import_surf_2(1:ntot_import_surf_2)
      write (id_file) edge_comm_2nd%item_import(1:edge_comm_2nd%ntot_import)
!
      end subroutine write_all_import_to_work
!
!   --------------------------------------------------------------------
!
      subroutine write_all_export_to_work(id_file)
!
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_geometry_data
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
!
      write (id_file) istack_export_2(1:num_neib_2)
      write (id_file) istack_export_ele_2(1:num_neib_ele_2)
      write (id_file) istack_export_surf_2(1:num_neib_surf_2)
      write (id_file) edge_comm_2nd%istack_export(1:edge_comm_2nd%num_neib)
      write (id_file) item_export_2(1:ntot_export_2)
      write (id_file) item_export_ele_2(1:ntot_export_ele_2)
      write (id_file) item_export_surf_2(1:ntot_export_surf_2)
      write (id_file) edge_comm_2nd%item_export(1:edge_comm_2nd%ntot_export)
!
      end subroutine write_all_export_to_work
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_all_import_from_work(id_file)
!
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_geometry_data
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      rewind (id_file)
      read (id_file) num_neib_2
!
      num_neib_ele_2 =  num_neib_2
      num_neib_surf_2 = num_neib_2
      edge_comm_2nd%num_neib = num_neib_2
!
      call allocate_2nd_neib_id
      call allocate_2nd_ele_neib_id
      call allocate_2nd_surf_neib_id
      call allocate_type_neib_id(edge_comm_2nd)
!
      call allocate_2nd_nod_import_num
      call allocate_2nd_ele_import_num
      call allocate_2nd_surf_import_num
      call allocate_type_import_num(edge_comm_2nd)
!
      read (id_file) id_neib_2(1:num_neib_2)
      read (id_file) istack_import_2(1:num_neib_2)
      read (id_file) istack_import_ele_2(1:num_neib_ele_2)
      read (id_file) istack_import_surf_2(1:num_neib_surf_2)
      read (id_file) edge_comm_2nd%istack_import(1:edge_comm_2nd%num_neib)
!
      id_neib_ele_2(1:num_neib_2) =  id_neib_2(1:num_neib_2)
      id_neib_surf_2(1:num_neib_2) = id_neib_2(1:num_neib_2)
      edge_comm_2nd%id_neib(1:num_neib_2) = id_neib_2(1:num_neib_2)
!
      ntot_import_2 =      istack_import_2(num_neib_2)
      ntot_import_ele_2 =  istack_import_ele_2(num_neib_ele_2)
      ntot_import_surf_2 = istack_import_surf_2(num_neib_surf_2)
      edge_comm_2nd%ntot_import = edge_comm_2nd%istack_import(edge_comm_2nd%num_neib)
!
      call allocate_2nd_nod_import_item
      call allocate_2nd_ele_import_item
      call allocate_2nd_surf_import_item
      call allocate_type_import_item(edge_comm_2nd)
!
      read (id_file) item_import_2(1:ntot_import_2)
      read (id_file) item_import_ele_2(1:ntot_import_ele_2)
      read (id_file) item_import_surf_2(1:ntot_import_surf_2)
      read (id_file) edge_comm_2nd%item_import(1:edge_comm_2nd%ntot_import)
!
        do i = 1, num_neib_2
          num_import_2(i) = istack_import_2(i) - istack_import_2(i-1)
        end do
!
        do i = 1, num_neib_ele_2
          num_import_ele_2(i) = istack_import_ele_2(i)                  &
     &                          - istack_import_ele_2(i-1)
        end do
!
        do i = 1, num_neib_surf_2
          num_import_surf_2(i) = istack_import_surf_2(i)                &
     &                          - istack_import_surf_2(i-1)
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
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_geometry_data
      use m_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      call allocate_2nd_nod_export_num
      call allocate_2nd_ele_export_num
      call allocate_2nd_surf_export_num
      call allocate_type_export_num(edge_comm_2nd)
!
      read (id_file) istack_export_2(1:num_neib_2)
      read (id_file) istack_export_ele_2(1:num_neib_ele_2)
      read (id_file) istack_export_surf_2(1:num_neib_surf_2)
      read (id_file) edge_comm_2nd%istack_export(1:edge_comm_2nd%num_neib)
!
      ntot_export_2 =      istack_export_2(num_neib_2)
      ntot_export_ele_2 =  istack_export_ele_2(num_neib_ele_2)
      ntot_export_surf_2 = istack_export_surf_2(num_neib_surf_2)
      edge_comm_2nd%ntot_export = edge_comm_2nd%istack_export(edge_comm_2nd%num_neib)
!
      call allocate_2nd_nod_export_item
      call allocate_2nd_ele_export_item
      call allocate_2nd_surf_export_item
      call allocate_type_export_item(edge_comm_2nd)
!
      read (id_file) item_export_2(1:ntot_export_2)
      read (id_file) item_export_ele_2(1:ntot_export_ele_2)
      read (id_file) item_export_surf_2(1:ntot_export_surf_2)
      read (id_file) edge_comm_2nd%item_export(1:edge_comm_2nd%ntot_export)
!
        do i = 1, num_neib_2
          num_export_2(i) = istack_export_2(i) - istack_export_2(i-1)
        end do
!
        do i = 1, num_neib_ele_2
          num_export_ele_2(i) = istack_export_ele_2(i)                  &
     &                          - istack_export_ele_2(i-1)
        end do
!
        do i = 1, num_neib_surf_2
          num_export_surf_2(i) = istack_export_surf_2(i)                &
     &                          - istack_export_surf_2(i-1)
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
