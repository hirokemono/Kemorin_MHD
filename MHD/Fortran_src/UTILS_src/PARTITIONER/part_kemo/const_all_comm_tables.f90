!const_all_comm_tables.f90
!     module const_all_comm_tables
!
!      Written by H. Matsui on Aug., 2007
!
!      subroutine const_all_import_tbl_part(ip, nproc,                  &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
!      subroutine count_all_export_item_4_part(ip, work_f_head,         &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!      subroutine set_all_export_item_4_part(ip, work_f_head,           &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      module const_all_comm_tables
!
      use m_precision
!
      use t_comm_table
!
      implicit none
!
      private :: const_ele_import_tbl_4_part
      private :: const_surf_import_tbl_4_part
      private :: const_edge_import_tbl_4_part
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine const_all_import_tbl_part(ip, nproc,                   &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use const_node_comm_table
!
      integer(kind = kint), intent(in) :: ip, nproc
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
!
      call const_nod_import_table_4_part(ip, new_comm)
      call const_ele_import_tbl_4_part(ip, nproc, new_ele_comm)
      call const_surf_import_tbl_4_part(ip, nproc, new_surf_comm)
      call const_edge_import_tbl_4_part(ip, nproc, new_edge_comm)
!
      end subroutine const_all_import_tbl_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_ele_import_tbl_4_part(ip, nproc, new_ele_comm)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
      use set_import_items
!
      integer(kind = kint), intent(in) :: ip, nproc
      type(communication_table), intent(inout) :: new_ele_comm
!
!
      call allocate_type_import_num(new_ele_comm)
!
      call count_ele_import_item(ip, nproc, ntot_numele_sub,            &
     &    istack_numele_sub, iele_4_subdomain, nele_s_domin,            &
     &    id_glelem_org, IGROUP_ele, new_ele_comm%num_neib,             &
     &    new_ele_comm%id_neib, new_ele_comm%ntot_import,               &
     &    new_ele_comm%num_import)
!
      call allocate_type_import_item(new_ele_comm)
!
      call set_ele_import_item(ip, nproc, ntot_numele_sub,              &
     &    istack_numele_sub, iele_4_subdomain, nele_s_domin,            &
     &    id_glelem_org, IGROUP_ele, new_ele_comm%num_neib,             &
     &    new_ele_comm%id_neib, new_ele_comm%ntot_import,               &
     &    new_ele_comm%num_import, new_ele_comm%item_import)
!
      end subroutine const_ele_import_tbl_4_part
!
!   --------------------------------------------------------------------
!
      subroutine const_surf_import_tbl_4_part(ip, nproc, new_surf_comm)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
      use set_import_items
!
      integer(kind = kint), intent(in) :: ip, nproc
      type(communication_table), intent(inout) :: new_surf_comm
!
!
      call allocate_type_import_num(new_surf_comm)
!
      call count_ele_import_item(ip, nproc, ntot_numsurf_sub,           &
     &    istack_numsurf_sub, isurf_4_subdomain, nsurf_s_domin,         &
     &    id_glsurf_org, IGROUP_surf, new_surf_comm%num_neib,           &
     &    new_surf_comm%id_neib, new_surf_comm%ntot_import,             &
     &    new_surf_comm%istack_import)
!
      call allocate_type_import_item(new_surf_comm)
!
      call set_ele_import_item(ip, nproc, ntot_numsurf_sub,             &
     &    istack_numsurf_sub, isurf_4_subdomain, nsurf_s_domin,         &
     &    id_glsurf_org, IGROUP_surf, new_surf_comm%num_neib,           &
     &    new_surf_comm%id_neib, new_surf_comm%ntot_import,             &
     &    new_surf_comm%istack_import, new_surf_comm%item_import)
!
      end subroutine const_surf_import_tbl_4_part
!
!   --------------------------------------------------------------------
!
      subroutine const_edge_import_tbl_4_part(ip, nproc, new_edge_comm)
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
      use set_import_items
!
      integer(kind = kint), intent(in) :: ip, nproc
      type(communication_table), intent(inout) :: new_edge_comm
!
!
      call allocate_type_import_num(new_edge_comm)
!
      call count_ele_import_item(ip, nproc, ntot_numedge_sub,           &
     &    istack_numedge_sub, iedge_4_subdomain, nedge_s_domin,         &
     &    id_gledge_org, IGROUP_edge, new_edge_comm%num_neib,           &
     &    new_edge_comm%id_neib, new_edge_comm%ntot_import,             &
     &    new_edge_comm%istack_import)
!
      call allocate_type_import_item(new_edge_comm)
!
      call set_ele_import_item(ip, nproc, ntot_numedge_sub,             &
     &    istack_numedge_sub, iedge_4_subdomain, nedge_s_domin,         &
     &    id_gledge_org, IGROUP_edge, new_edge_comm%num_neib,           &
     &    new_edge_comm%id_neib, new_edge_comm%ntot_import,             &
     &    new_edge_comm%istack_import, new_edge_comm%item_import)
!
!
      end subroutine const_edge_import_tbl_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_all_export_item_4_part(ip, work_f_head,          &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use m_partitioner_comm_table
      use m_domain_group_4_partition
      use m_internal_4_partitioner
      use sel_part_comm_tbl_input
!
      integer(kind = kint), intent(in) :: ip
      character(len=kchara), intent(in) :: work_f_head
!
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
      integer(kind = kint) :: j, jp, jg
!
      do j = 1, new_comm%num_neib
        jp = new_comm%id_neib(j)
!
        call load_all_import_num_tmp(jp, work_f_head)
!
        ISTACK_NOD_TMP(0) = 0
        do jg = 1, NP_TMP
          if (NEIB_TMP(jg) .eq. ip) then
            new_comm%num_export(j) = ISTACK_NOD_TMP(jg)                 &
     &                            - ISTACK_NOD_TMP(jg-1)
            new_ele_comm%num_export(j) =  ISTACK_ELE_TMP(jg)            &
     &                            - ISTACK_ELE_TMP(jg-1)
            new_surf_comm%num_export(j) = ISTACK_SURF_TMP(jg)           &
     &                            - ISTACK_SURF_TMP(jg-1)
            new_edge_comm%num_export(j) = ISTACK_EDGE_TMP(jg)           &
     &                            - ISTACK_EDGE_TMP(jg-1)
            exit
          end if
        end do
!
        call deallocate_all_import_num_tmp
      end do
!
      end subroutine count_all_export_item_4_part
!
!   --------------------------------------------------------------------
!
      subroutine set_all_export_item_4_part(ip, work_f_head,            &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use m_partitioner_comm_table
      use m_domain_group_4_partition
      use m_internal_4_partitioner
      use sel_part_comm_tbl_input
!
      character(len=kchara), intent(in) :: work_f_head
      integer(kind = kint), intent(in) :: ip
!
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
      integer(kind = kint) :: j, jp, jg, jst, jnum, icou
      integer(kind = kint) :: jst_nod_im,  jed_nod_im,  jnod
      integer(kind = kint) :: jst_ele_im,  jed_ele_im,  jele
      integer(kind = kint) :: jst_surf_im, jed_surf_im, jsurf
      integer(kind = kint) :: jst_edge_im, jed_edge_im, jedge
      integer(kind = kint) :: inod,  inod_org
      integer(kind = kint) :: iele,  iele_org
      integer(kind = kint) :: isurf, isurf_org
      integer(kind = kint) :: iedge, iedge_org
!
      do j = 1, new_comm%num_neib
        jp = new_comm%id_neib(j)
        call load_all_import_item_tmp(jp, work_f_head)
!
        ISTACK_NOD_TMP(0) =  0
        ISTACK_ELE_TMP(0) =  0
        ISTACK_SURF_TMP(0) = 0
        ISTACK_EDGE_TMP(0) = 0
        do jg = 1, NP_TMP
          if (NEIB_TMP(jg) .eq. ip) then
            jst_nod_im =  ISTACK_NOD_TMP(jg-1)
            jed_nod_im =  ISTACK_NOD_TMP(jg)
            jst_ele_im =  ISTACK_ELE_TMP(jg-1)
            jed_ele_im =  ISTACK_ELE_TMP(jg)
            jst_surf_im = ISTACK_SURF_TMP(jg-1)
            jed_surf_im = ISTACK_SURF_TMP(jg)
            jst_edge_im = ISTACK_EDGE_TMP(jg-1)
            jed_edge_im = ISTACK_EDGE_TMP(jg)
            exit
          end if
        end do
!
        jst = istack_numnod_sub(jp-1)
        icou = new_comm%istack_export(j-1)
        do jnum = jst_nod_im+1, jed_nod_im
          icou = icou + 1
          jnod = IMPORT_NOD_TMP(jnum)
          inod = inod_4_subdomain(jst+jnod)
          inod_org = id_glnode_org(inod)
          new_comm%item_export(icou) = inod_local_part(inod_org)
        end do
!
        jst = istack_numele_sub(jp-1)
        icou = new_ele_comm%istack_export(j-1)
        do jnum = jst_ele_im+1, jed_ele_im
          icou = icou + 1
          jele = IMPORT_ELE_TMP(jnum)
          iele = iele_4_subdomain(jst+jele)
          iele_org = id_glelem_org(iele)
          new_ele_comm%item_export(icou) = iele_local_part(iele_org)
        end do
!
        jst = istack_numsurf_sub(jp-1)
        icou = new_surf_comm%istack_export(j-1)
        do jnum = jst_surf_im+1, jed_surf_im
          icou = icou + 1
          jsurf = IMPORT_SURF_TMP(jnum)
          isurf = isurf_4_subdomain(jst+jsurf)
          isurf_org = id_glsurf_org(isurf)
          new_surf_comm%item_export(icou) = isurf_local_part(isurf_org)
        end do
!
        jst = istack_numedge_sub(jp-1)
        icou = new_edge_comm%istack_export(j-1)
        do jnum = jst_edge_im+1, jed_edge_im
          icou = icou + 1
          jedge = IMPORT_EDGE_TMP(jnum)
          iedge = iedge_4_subdomain(jst+jedge)
          iedge_org = id_gledge_org(iedge)
          new_edge_comm%item_export(icou) = iedge_local_part(iedge_org)
        end do
!
        call deallocate_all_import_tmp
!
      end do
!
      end subroutine set_all_export_item_4_part
!
!   --------------------------------------------------------------------
!
      end module const_all_comm_tables
