!const_node_comm_table.f90
!     module const_node_comm_table
!
!      Written by H. Matsui on Aug., 2007
!
!!      subroutine const_nod_import_table_4_part                        &
!!     &         (ip, nod_d_grp, itl_nod_part, new_comm)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(internal_4_partitioner), intent(in) :: itl_nod_part
!!        type(communication_table), intent(inout) :: new_comm
!!
!!      subroutine count_nod_export_item_4_part(ip, new_comm, comm_part)
!!      subroutine set_nod_export_item_4_part                           &
!!     &         (ip, nod_d_grp, itl_nod_part, new_comm, comm_part)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(internal_4_partitioner), intent(in) :: itl_nod_part
!!        type(communication_table), intent(inout) :: new_comm
!!        type(partitioner_comm_tables), intent(inout) :: comm_part
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
      subroutine const_nod_import_table_4_part                          &
     &         (ip, nod_d_grp, itl_nod_part, new_comm)
!
      use t_domain_group_4_partition
      use t_internal_4_partitioner
      use set_import_items
!
      integer(kind = kint), intent(in) :: ip
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      type(communication_table), intent(inout) :: new_comm
!
!
      call alloc_import_num(new_comm)
!
      call count_node_import_item(nod_d_grp, itl_nod_part, ip,          &
     &    new_comm%num_neib, new_comm%id_neib, new_comm%ntot_import,    &
     &    new_comm%istack_import)
!
      call alloc_import_item(new_comm)
!
      call set_node_import_item(nod_d_grp, itl_nod_part, ip,            &
     &    new_comm%num_neib, new_comm%id_neib, new_comm%ntot_import,    &
     &    new_comm%istack_import, new_comm%item_import)
!
      end subroutine const_nod_import_table_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_nod_export_item_4_part(ip, new_comm, comm_part)
!
      use t_domain_group_4_partition
      use t_partitioner_comm_table
      use sel_part_nod_comm_input
!
      integer(kind = kint), intent(in) :: ip
!
      type(communication_table), intent(inout) :: new_comm
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      integer(kind = kint) :: j
      integer :: jp
!
      do j = 1, new_comm%num_neib
        jp = int(new_comm%id_neib(j))
!
        call load_node_import_num_tmp(jp, comm_part)
        call count_each_nod_export_num_part                             &
     &     (ip, j, comm_part%ipt_tmp, new_comm)
        call deallocate_nod_import_num_tmp(comm_part%ipt_tmp)
      end do
!
      end subroutine count_nod_export_item_4_part
!
!   --------------------------------------------------------------------
!
      subroutine set_nod_export_item_4_part                             &
     &         (ip, nod_d_grp, itl_nod_part, new_comm, comm_part)
!
      use t_partitioner_comm_table
      use t_internal_4_partitioner
      use t_domain_group_4_partition
      use sel_part_nod_comm_input
!
      integer(kind = kint), intent(in) :: ip
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      type(communication_table), intent(inout) :: new_comm
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      integer(kind = kint) :: j
      integer :: jp
!
!
      do j = 1, new_comm%num_neib
        jp = int(new_comm%id_neib(j))
!
        call load_node_import_item_tmp(jp, comm_part)
        call each_nod_export_item_4_part(ip, j, nod_d_grp,              &
     &      itl_nod_part, comm_part%ipt_tmp, new_comm)
!
        call deallocate_nod_import_tmp(comm_part%ipt_tmp)
      end do
!
      end subroutine set_nod_export_item_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_each_nod_export_num_part                         &
     &         (ip, j, ipt_tmp, new_comm)
!
      use t_domain_group_4_partition
      use t_partitioner_comm_table
!
      integer(kind = kint), intent(in) :: ip, j
      type(temporary_import_4_part), intent(in) :: ipt_tmp
!
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: jg
!
      do jg = 1, ipt_tmp%NP_TMP
        if (ipt_tmp%NEIB_TMP(jg) .eq. ip) then
          new_comm%num_export(j) = ipt_tmp%ISTACK_NOD_TMP(jg)           &
     &                              - ipt_tmp%ISTACK_NOD_TMP(jg-1)
          exit
        end if
      end do
!
      end subroutine count_each_nod_export_num_part
!
!   --------------------------------------------------------------------
!
      subroutine each_nod_export_item_4_part                            &
     &         (ip, j, nod_d_grp, itl_nod_part, ipt_tmp, new_comm)
!
      use t_partitioner_comm_table
      use t_internal_4_partitioner
      use t_domain_group_4_partition
      use sel_part_nod_comm_input
!
      integer(kind = kint), intent(in) :: ip, j
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      type(temporary_import_4_part), intent(in) :: ipt_tmp
!
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: jp, jg, jst, jnum, icou
      integer(kind = kint) :: inod, jst_im, jed_im, jnod
      integer(kind = kint_gl) :: inod_org
!
!
      jp = new_comm%id_neib(j)
      jst_im = ipt_tmp%ISTACK_NOD_TMP(0)
      jed_im = ipt_tmp%ISTACK_NOD_TMP(0)
      do jg = 1, ipt_tmp%NP_TMP
        if (ipt_tmp%NEIB_TMP(jg) .eq. ip) then
          jst_im = ipt_tmp%ISTACK_NOD_TMP(jg-1)
          jed_im = ipt_tmp%ISTACK_NOD_TMP(jg)
          exit
        end if
      end do
!
      jst = itl_nod_part%istack_4_subdomain(jp-1)
      icou = new_comm%istack_export(j-1)
      do jnum = jst_im+1, jed_im
        icou = icou + 1
        jnod = ipt_tmp%IMPORT_NOD_TMP(jnum)
        inod = itl_nod_part%id_4_subdomain(jst+jnod)
        inod_org = nod_d_grp%id_global_org(inod)
        new_comm%item_export(icou)                                      &
     &         = nod_d_grp%id_local_part(inod_org)
      end do
!
      end subroutine each_nod_export_item_4_part
!
!   --------------------------------------------------------------------
!
      end module const_node_comm_table
