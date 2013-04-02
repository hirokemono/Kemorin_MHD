!
!      module set_domain_and_org_id
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine set_ele_domain_groups
!      subroutine set_all_domain_groups
!      subroutine set_origin_global_node
!      subroutine set_origin_global_ids
!
      module set_domain_and_org_id
!
      use m_precision
!
      implicit  none
!
      private :: set_domain_group_4_ele
      private :: copy_node_id_4_peri_sleeve
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_ele_domain_groups
!
      use m_geometry_parameter
      use m_geometry_data
      use m_domain_group_4_partition
!
!
      call set_domain_group_4_ele(nnod_s_domin, nele_s_domin,           &
     &    nnod_4_ele, ie, IGROUP_nod, IGROUP_ele)
!
      end subroutine set_ele_domain_groups
!
!   --------------------------------------------------------------------
!
      subroutine set_all_domain_groups
!
      use m_geometry_parameter
      use m_geometry_data
      use m_domain_group_4_partition
!
!
      call set_domain_group_4_ele(nnod_s_domin, nsurf_s_domin,          &
     &    nnod_4_surf, ie_surf, IGROUP_nod, IGROUP_surf)
!
      call set_domain_group_4_ele(nnod_s_domin, nedge_s_domin,          &
     &    nnod_4_edge, ie_edge, IGROUP_nod, IGROUP_edge)
!
      end subroutine set_all_domain_groups
!
!   --------------------------------------------------------------------
!
      subroutine set_origin_global_node
!
      use m_nod_comm_table
      use m_domain_group_4_partition
!
!
      call copy_node_id_4_peri_sleeve(nnod_s_domin, ntot_export,        &
     &    item_export, item_import, id_glnode_org)
!
      end subroutine set_origin_global_node
!
!------------------------------------------------------------------
!
      subroutine set_origin_global_ids
!
      use m_ele_comm_table
      use m_surf_comm_table
      use m_edge_comm_table
      use m_domain_group_4_partition
!
!
      call copy_node_id_4_peri_sleeve(nele_s_domin, ntot_export_ele,   &
     &    item_export_ele, item_import_ele, id_glelem_org)
!
      call copy_node_id_4_peri_sleeve(nsurf_s_domin, ntot_export_surf, &
     &    item_export_surf, item_import_surf, id_glsurf_org)
!
      call copy_node_id_4_peri_sleeve(nedge_s_domin, ntot_export_edge, &
     &    item_export_edge, item_import_edge, id_gledge_org)
!
      end subroutine set_origin_global_ids
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_domain_group_4_ele(nnod, nele, nnod_4_ele, ie,     &
     &          IGROUP, IGROUP_ele)
!
      integer(kind = kint), intent(in) :: nnod, nele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_4_ele)
      integer(kind = kint), intent(in) :: IGROUP(nnod)
!
      integer(kind = kint), intent(inout) :: IGROUP_ele(nele)
!
      integer(kind = kint) :: iele, inod
!
!
      do iele = 1, nele
        inod = ie(iele,1)
        IGROUP_ele(iele) = IGROUP(inod)
      end do
!
      end subroutine set_domain_group_4_ele
!
!   --------------------------------------------------------------------
!
      subroutine copy_node_id_4_peri_sleeve(num, ntot_export,           &
     &          item_export, item_import, id_gl_org)
!
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: ntot_export
      integer(kind = kint), intent(in) :: item_export(ntot_export)
      integer(kind = kint), intent(in) :: item_import(ntot_export)
!
      integer(kind = kint), intent(inout) :: id_gl_org(num)
!
      integer(kind = kint) :: i, inum, iex, iim
!
      do i = 1, num
        id_gl_org(i) = i
      end do
      do inum = 1, ntot_export
        iex = item_export(inum)
        iim = item_import(inum)
        id_gl_org(iim) = iex
      end do
!
      end subroutine copy_node_id_4_peri_sleeve
!
!------------------------------------------------------------------
!
      end module set_domain_and_org_id
