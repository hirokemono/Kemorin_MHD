!const_node_comm_table.f90
!     module const_node_comm_table
!
!      Written by H. Matsui on Aug., 2007
!
!      subroutine const_nod_import_table_4_part(ip)
!
!      subroutine count_nod_export_item_4_part(ip, work_f_head)
!      subroutine set_nod_export_item_4_part(ip, work_f_head)
!
      module const_node_comm_table
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
      subroutine const_nod_import_table_4_part(ip)
!
      use m_2nd_nod_comm_table
      use set_import_items
!
      integer(kind = kint), intent(in) :: ip
!
      call allocate_2nd_nod_import_num
!
      call count_node_import_item(ip, num_neib_2, id_neib_2,            &
     &      ntot_import_2, istack_import_2)
!
      call allocate_2nd_nod_import_item
!
      call set_node_import_item(ip, num_neib_2, id_neib_2,              &
     &      ntot_import_2, istack_import_2, item_import_2)
!
!
      end subroutine const_nod_import_table_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_nod_export_item_4_part(ip, work_f_head)
!
      use m_2nd_nod_comm_table
      use m_partitioner_comm_table
      use m_domain_group_4_partition
      use m_internal_4_partitioner
      use sel_part_nod_comm_input
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
!
      integer(kind = kint) :: j, jp, jg
!
      do j = 1, num_neib_2
        jp = id_neib_2(j)
!
        call load_node_import_num_tmp(jp, work_f_head)
!
        ISTACK_NOD_TMP(0) = 0
        do jg = 1, NP_TMP
          if (NEIB_TMP(jg) .eq. ip) then
            num_export_2(j) = ISTACK_NOD_TMP(jg) - ISTACK_NOD_TMP(jg-1)
            exit
          end if
        end do
!
        call deallocate_nod_import_num_tmp
      end do
!
      end subroutine count_nod_export_item_4_part
!
!   --------------------------------------------------------------------
!
      subroutine set_nod_export_item_4_part(ip, work_f_head)
!
      use m_2nd_nod_comm_table
      use m_partitioner_comm_table
      use m_domain_group_4_partition
      use m_internal_4_partitioner
      use sel_part_nod_comm_input
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
!
      integer(kind = kint) :: j, jp, jg, jst, jnum, icou
      integer(kind = kint) :: jst_im, jed_im, jnod
      integer(kind = kint) :: inod, inod_org
!
      do j = 1, num_neib_2
        jp = id_neib_2(j)
!
        call load_node_import_item_tmp(jp, work_f_head)
!
        ISTACK_NOD_TMP(0) = 0
        do jg = 1, NP_TMP
          if (NEIB_TMP(jg) .eq. ip) then
            jst_im = ISTACK_NOD_TMP(jg-1)
            jed_im = ISTACK_NOD_TMP(jg)
            exit
          end if
        end do
!
        jst = istack_numnod_sub(jp-1)
        icou = istack_export_2(j-1)
        do jnum = jst_im+1, jed_im
          icou = icou + 1
          jnod = IMPORT_NOD_TMP(jnum)
          inod = inod_4_subdomain(jst+jnod)
          inod_org = id_glnode_org(inod)
          item_export_2(icou) = inod_local_part(inod_org)
        end do
!
        call deallocate_nod_import_tmp
!
      end do
!
      end subroutine set_nod_export_item_4_part
!
!   --------------------------------------------------------------------
!
      end module const_node_comm_table
