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
      use m_2nd_nod_comm_table
!
      integer(kind = kint),  intent(in) :: ip 
!
!
      nod_comm_tbl_part(ip)%num_neib = num_neib_2
      nod_comm_tbl_part(ip)%ntot_import = ntot_import_2
!
      call allocate_type_neib_id( nod_comm_tbl_part(ip) )
      call allocate_type_import_num( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%id_neib(1:num_neib_2)                       &
     &       = id_neib_2(1:num_neib_2)
      nod_comm_tbl_part(ip)%istack_import(0:num_neib_2)                 &
     &       = istack_import_2(0:num_neib_2)
!
      call allocate_type_import_item( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%item_import(1:ntot_import_2)                &
     &       = item_import_2(1:ntot_import_2)
!
      end subroutine copy_node_import_to_mem
!
!------------------------------------------------------------------
!
      subroutine copy_node_import_from_mem(ip)
!
      use m_2nd_nod_comm_table
!
      integer(kind = kint),  intent(in) :: ip
      integer(kind = kint) :: i
!
!
      num_neib_2 = nod_comm_tbl_part(ip)%num_neib
      ntot_import_2 = nod_comm_tbl_part(ip)%ntot_import
!
      call allocate_2nd_neib_id
      call allocate_2nd_nod_import_num
!
      id_neib_2(1:num_neib_2)                                           &
     &       = nod_comm_tbl_part(ip)%id_neib(1:num_neib_2)
      istack_import_2(0:num_neib_2)                                     &
     &       = nod_comm_tbl_part(ip)%istack_import(0:num_neib_2)
!
      do i = 1, num_neib_2
        num_import_2(i) = istack_import_2(i) - istack_import_2(i-1)
      end do
!
      call allocate_2nd_nod_import_item
!
      item_import_2(1:ntot_import_2)                                    &
     &       = nod_comm_tbl_part(ip)%item_import(1:ntot_import_2)
!
      end subroutine copy_node_import_from_mem
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_node_export_to_mem(ip)
!
      use m_2nd_nod_comm_table
!
      integer(kind = kint),  intent(in) :: ip 
!
!
      nod_comm_tbl_part(ip)%ntot_export = ntot_export_2
!
      call allocate_type_export_num( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%istack_export(0:num_neib_2)                 &
     &       = istack_export_2(0:num_neib_2)
!
      call allocate_type_export_item( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%item_export(1:ntot_export_2)                &
     &       = item_export_2(1:ntot_export_2)
!
      end subroutine copy_node_export_to_mem
!
!------------------------------------------------------------------
!
      subroutine copy_node_export_from_mem(ip)
!
      use m_2nd_nod_comm_table
!
      integer(kind = kint),  intent(in) :: ip
      integer(kind = kint) :: i
!
!
      ntot_export_2 = nod_comm_tbl_part(ip)%ntot_export
!
      call allocate_2nd_nod_export_num
!
      istack_export_2(0:num_neib_2)                                     &
     &       = nod_comm_tbl_part(ip)%istack_export(0:num_neib_2)
!
      do i = 1, num_neib_2
        num_export_2(i) = istack_export_2(i) - istack_export_2(i-1)
      end do
!
      call allocate_2nd_nod_export_item
!
      item_export_2(1:ntot_export_2)                                    &
     &       = nod_comm_tbl_part(ip)%item_export(1:ntot_export_2)
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
