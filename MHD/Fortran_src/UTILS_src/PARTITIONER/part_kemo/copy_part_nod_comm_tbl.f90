!copy_part_nod_comm_tbl.f90
!     module copy_part_nod_comm_tbl
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine copy_node_import_to_mem(ip)
!      subroutine copy_node_import_from_mem(ip)
!
!      subroutine copy_node_export_to_mem(ip)
!      subroutine copy_node_export_from_mem(ip)
!
!      subroutine copy_node_import_num_tmp(ip)
!      subroutine copy_node_import_item_tmp(ip)
!
      module copy_part_nod_comm_tbl
!
      use m_precision
!
      use t_comm_table
      use m_partitioner_comm_table
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine copy_node_import_to_mem(ip)
!
      use m_2nd_geometry_data
!
      integer(kind = kint),  intent(in) :: ip 
!
!
      nod_comm_tbl_part(ip)%num_neib = comm_2nd%num_neib
      nod_comm_tbl_part(ip)%ntot_import = comm_2nd%ntot_import
!
      call allocate_type_neib_id( nod_comm_tbl_part(ip) )
      call allocate_type_import_num( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%id_neib(1:comm_2nd%num_neib)  &
     &       = comm_2nd%id_neib(1:comm_2nd%num_neib)
      nod_comm_tbl_part(ip)%istack_import(0:comm_2nd%num_neib)   &
     &       = comm_2nd%istack_import(0:comm_2nd%num_neib)
!
      call allocate_type_import_item( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%item_import(1:comm_2nd%ntot_import)   &
     &       = comm_2nd%item_import(1:comm_2nd%ntot_import)
!
      end subroutine copy_node_import_to_mem
!
!------------------------------------------------------------------
!
      subroutine copy_node_import_from_mem(ip)
!
      use m_2nd_geometry_data
!
      integer(kind = kint),  intent(in) :: ip
      integer(kind = kint) :: i
!
!
      comm_2nd%num_neib = nod_comm_tbl_part(ip)%num_neib
      comm_2nd%ntot_import = nod_comm_tbl_part(ip)%ntot_import
!
      call allocate_type_neib_id(comm_2nd)
      call allocate_type_import_num(comm_2nd)
!
      comm_2nd%id_neib(1:comm_2nd%num_neib)                     &
     &       = nod_comm_tbl_part(ip)%id_neib(1:comm_2nd%num_neib)
      comm_2nd%istack_import(0:comm_2nd%num_neib)                              &
     &       = nod_comm_tbl_part(ip)%istack_import(0:comm_2nd%num_neib)
!
      do i = 1, comm_2nd%num_neib
        comm_2nd%num_import(i) = comm_2nd%istack_import(i)  &
     &                          - comm_2nd%istack_import(i-1)
      end do
!
      call allocate_type_import_item(comm_2nd)
!
      comm_2nd%item_import(1:comm_2nd%ntot_import)                             &
     &       = nod_comm_tbl_part(ip)%item_import(1:comm_2nd%ntot_import)
!
      end subroutine copy_node_import_from_mem
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_node_export_to_mem(ip)
!
      use m_2nd_geometry_data
!
      integer(kind = kint),  intent(in) :: ip 
!
!
      nod_comm_tbl_part(ip)%ntot_export = comm_2nd%ntot_export
!
      call allocate_type_export_num( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%istack_export(0:comm_2nd%num_neib)          &
     &       = comm_2nd%istack_export(0:comm_2nd%num_neib)
!
      call allocate_type_export_item( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%item_export(1:comm_2nd%ntot_export)         &
     &       = comm_2nd%item_export(1:comm_2nd%ntot_export)
!
      end subroutine copy_node_export_to_mem
!
!------------------------------------------------------------------
!
      subroutine copy_node_export_from_mem(ip)
!
      use m_2nd_geometry_data
!
      integer(kind = kint),  intent(in) :: ip
      integer(kind = kint) :: i
!
!
      comm_2nd%ntot_export = nod_comm_tbl_part(ip)%ntot_export
!
      call allocate_type_export_num(comm_2nd)
!
      comm_2nd%istack_export(0:comm_2nd%num_neib)                       &
     &       = nod_comm_tbl_part(ip)%istack_export(0:comm_2nd%num_neib)
!
      do i = 1, comm_2nd%num_neib
        comm_2nd%num_export(i) = comm_2nd%istack_export(i)   &
     &                          - comm_2nd%istack_export(i-1)
      end do
!
      call allocate_type_export_item(comm_2nd)
!
      comm_2nd%item_export(1:comm_2nd%ntot_export)                             &
     &       = nod_comm_tbl_part(ip)%item_export(1:comm_2nd%ntot_export)
!
      end subroutine copy_node_export_from_mem
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_node_import_num_tmp(ip)
!
      integer(kind = kint), intent(in) :: ip
!
!
      NP_TMP = nod_comm_tbl_part(ip)%num_neib
!
      call allocate_nod_import_num_tmp
!
      NEIB_TMP(1:NP_TMP) = nod_comm_tbl_part(ip)%id_neib(1:NP_TMP)
      ISTACK_NOD_TMP(0:NP_TMP)                                          &
     &        =  nod_comm_tbl_part(ip)%istack_import(0:NP_TMP)
!
      end subroutine copy_node_import_num_tmp
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_import_item_tmp(ip)
!
      integer(kind = kint), intent(in) :: ip
!
!
      call allocate_nod_import_item_tmp
!
      IMPORT_NOD_TMP(1:NTOT_NOD_TMP)                                    &
     &        =  nod_comm_tbl_part(ip)%item_import(1:NTOT_NOD_TMP)
!
      end subroutine copy_node_import_item_tmp
!
!   --------------------------------------------------------------------
!
      end module copy_part_nod_comm_tbl
