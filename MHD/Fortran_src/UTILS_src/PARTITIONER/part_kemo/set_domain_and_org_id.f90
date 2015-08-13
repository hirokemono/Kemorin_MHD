!
!      module set_domain_and_org_id
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine set_ele_domain_groups
!      subroutine set_origin_global_node
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
      use m_geometry_data
      use m_domain_group_4_partition
!
!
      call set_domain_group_4_ele(nnod_s_domin, nele_s_domin,           &
     &    ele1%nnod_4_ele, ie, IGROUP_nod, IGROUP_ele)
!
      end subroutine set_ele_domain_groups
!
!   --------------------------------------------------------------------
!
      subroutine set_origin_global_node
!
      use m_nod_comm_table
      use m_domain_group_4_partition
!
!
      call copy_node_id_4_peri_sleeve                                   &
     &   (nnod_s_domin, nod_comm%ntot_export, nod_comm%item_export,     &
     &    nod_comm%item_import, id_glnode_org)
!
      end subroutine set_origin_global_node
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
      integer(kind = kint_gl), intent(inout) :: id_gl_org(num)
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
