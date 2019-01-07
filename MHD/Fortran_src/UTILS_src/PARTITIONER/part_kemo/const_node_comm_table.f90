!const_node_comm_table.f90
!     module const_node_comm_table
!
!      Written by H. Matsui on Aug., 2007
!
!!      subroutine const_nod_import_table_4_part(ip, nod_d_grp, new_comm)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(communication_table), intent(inout) :: new_comm
!!
!!      subroutine count_nod_export_item_4_part                         &
!!     &         (ip, work_f_head, new_comm)
!!      subroutine set_nod_export_item_4_part                           &
!!     &         (ip, work_f_head, nod_d_grp, new_comm)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(communication_table), intent(inout) :: new_comm
!
      module const_node_comm_table
!
      use m_precision
      use t_comm_table
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine const_nod_import_table_4_part(ip, nod_d_grp, new_comm)
!
      use t_domain_group_4_partition
      use set_import_items
!
      integer(kind = kint), intent(in) :: ip
      type(domain_group_4_partition), intent(in) :: nod_d_grp
!
      type(communication_table), intent(inout) :: new_comm
!
!
      call allocate_type_import_num(new_comm)
!
      call count_node_import_item(nod_d_grp, ip, new_comm%num_neib,     &
     &    new_comm%id_neib,  new_comm%ntot_import,                      &
     &    new_comm%istack_import)
!
      call allocate_type_import_item(new_comm)
!
      call set_node_import_item(nod_d_grp, ip, new_comm%num_neib,       &
     &    new_comm%id_neib, new_comm%ntot_import,                       &
     &    new_comm%istack_import, new_comm%item_import)
!
      end subroutine const_nod_import_table_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_nod_export_item_4_part                           &
     &         (ip, work_f_head, new_comm)
!
      use t_domain_group_4_partition
      use m_partitioner_comm_table
      use m_internal_4_partitioner
      use sel_part_nod_comm_input
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: j, jp, jg
!
      do j = 1, new_comm%num_neib
        jp = new_comm%id_neib(j)
!
        call load_node_import_num_tmp(jp, work_f_head)
!
        ISTACK_NOD_TMP(0) = 0
        do jg = 1, NP_TMP
          if (NEIB_TMP(jg) .eq. ip) then
            new_comm%num_export(j) = ISTACK_NOD_TMP(jg)                 &
     &                              - ISTACK_NOD_TMP(jg-1)
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
      subroutine set_nod_export_item_4_part                             &
     &         (ip, work_f_head, nod_d_grp, new_comm)
!
      use m_partitioner_comm_table
      use m_internal_4_partitioner
      use t_domain_group_4_partition
      use sel_part_nod_comm_input
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
      type(domain_group_4_partition), intent(in) :: nod_d_grp
!
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: j, jp, jg, jst, jnum, icou
      integer(kind = kint) :: inod, jst_im, jed_im, jnod
      integer(kind = kint_gl) :: inod_org
!
      do j = 1, new_comm%num_neib
        jp = new_comm%id_neib(j)
!
        call load_node_import_item_tmp(jp, work_f_head)
!
        ISTACK_NOD_TMP(0) = 0
        jst_im = ISTACK_NOD_TMP(0)
        jed_im = ISTACK_NOD_TMP(0)
        do jg = 1, NP_TMP
          if (NEIB_TMP(jg) .eq. ip) then
            jst_im = ISTACK_NOD_TMP(jg-1)
            jed_im = ISTACK_NOD_TMP(jg)
            exit
          end if
        end do
!
        jst = istack_numnod_sub(jp-1)
        icou = new_comm%istack_export(j-1)
        do jnum = jst_im+1, jed_im
          icou = icou + 1
          jnod = IMPORT_NOD_TMP(jnum)
          inod = inod_4_subdomain(jst+jnod)
          inod_org = nod_d_grp%id_global_org(inod)
          new_comm%item_export(icou)                                    &
     &         = nod_d_grp%id_local_part(inod_org)
        end do
!
        call deallocate_nod_import_tmp
      end do
!
      end subroutine set_nod_export_item_4_part
!
!   --------------------------------------------------------------------
!
      end module const_node_comm_table
