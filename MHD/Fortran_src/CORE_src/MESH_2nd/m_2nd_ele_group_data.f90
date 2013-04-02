!
!     module m_2nd_ele_group_data
!
!     Writteg by H.Matsui on Aug., 2006
!
!      subroutine allocate_sf_stack_4_ele_g_2nd
!      subroutine allocate_sf_id_4_ele_g_2nd
!
!      subroutine allocate_eg_stack_4_ele_g_2nd
!      subroutine allocate_eg_id_4_ele_g_2nd
!
!      subroutine allocate_nd_stack_4_ele_g_2nd
!      subroutine allocate_nd_id_4_ele_g_2nd
!
!      subroutine deallocate_sf_id_4_ele_g_2nd
!      subroutine deallocate_eg_id_4_ele_g_2nd
!      subroutine deallocate_nd_id_4_ele_g_2nd
!
!      subroutine disconnect_sf_id_4_ele_g_2nd
!      subroutine disconnect_eg_id_4_ele_g_2nd
!      subroutine disconnect_nd_id_4_ele_g_2nd
!
      module m_2nd_ele_group_data
!
      use m_precision
!
      implicit  none
!
      integer(kind=kint) :: ntot_surf_ele_grp_2nd
      integer(kind=kint), pointer :: nsurf_ele_grp_2nd(:)
      integer(kind=kint), pointer :: isurf_stack_ele_grp_2nd(:)
!
      integer(kind=kint), pointer :: isurf_ele_grp_2nd(:)
!
!
      integer(kind=kint) :: ntot_edge_ele_grp_2nd
      integer(kind=kint), pointer :: nedge_ele_grp_2nd(:)
      integer(kind=kint), pointer :: iedge_stack_ele_grp_2nd(:)
!
      integer(kind=kint), pointer :: iedge_ele_grp_2nd(:)
!
!
      integer(kind=kint) :: ntot_node_ele_grp_2nd
      integer(kind=kint), pointer :: nnod_ele_grp_2nd(:)
      integer(kind=kint), pointer :: inod_stack_ele_grp_2nd(:)
!
      integer(kind=kint), pointer :: inod_ele_grp_2nd(:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_sf_stack_4_ele_g_2nd
!
      use m_2nd_group_data
!
      allocate(nsurf_ele_grp_2nd(num_mat_2nd))
      allocate(isurf_stack_ele_grp_2nd(0:num_mat_2nd))
!
      nsurf_ele_grp_2nd = 0
      isurf_stack_ele_grp_2nd = 0
!
      end subroutine allocate_sf_stack_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine allocate_sf_id_4_ele_g_2nd
!
      allocate(isurf_ele_grp_2nd(ntot_surf_ele_grp_2nd))
      isurf_ele_grp_2nd = 0
!
      end subroutine allocate_sf_id_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine allocate_eg_stack_4_ele_g_2nd
!
      use m_2nd_group_data
!
      allocate(nedge_ele_grp_2nd(num_mat_2nd))
      allocate(iedge_stack_ele_grp_2nd(0:num_mat_2nd))
!
      nedge_ele_grp_2nd = 0
      iedge_stack_ele_grp_2nd = 0
!
      end subroutine allocate_eg_stack_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine allocate_eg_id_4_ele_g_2nd
!
      allocate(iedge_ele_grp_2nd(ntot_edge_ele_grp_2nd))
      iedge_ele_grp_2nd = 0
!
      end subroutine allocate_eg_id_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine allocate_nd_stack_4_ele_g_2nd
!
      use m_2nd_group_data
!
      allocate(nnod_ele_grp_2nd(num_mat_2nd))
      allocate(inod_stack_ele_grp_2nd(0:num_mat_2nd))
!
      nnod_ele_grp_2nd = 0
      inod_stack_ele_grp_2nd = 0
!
      end subroutine allocate_nd_stack_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine allocate_nd_id_4_ele_g_2nd
!
      allocate(inod_ele_grp_2nd(ntot_node_ele_grp_2nd))
      inod_ele_grp_2nd = 0
!
      end subroutine allocate_nd_id_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_sf_id_4_ele_g_2nd
!
      deallocate(nsurf_ele_grp_2nd)
      deallocate(isurf_stack_ele_grp_2nd)
      deallocate(isurf_ele_grp_2nd)
!
      end subroutine deallocate_sf_id_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_eg_id_4_ele_g_2nd
!
      deallocate(nedge_ele_grp_2nd)
      deallocate(iedge_stack_ele_grp_2nd)
      deallocate(iedge_ele_grp_2nd)
!
      end subroutine deallocate_eg_id_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_nd_id_4_ele_g_2nd
!
      deallocate(nnod_ele_grp_2nd)
      deallocate(inod_stack_ele_grp_2nd)
      deallocate(inod_ele_grp_2nd)
!
      end subroutine deallocate_nd_id_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine disconnect_sf_id_4_ele_g_2nd
!
      nullify(nsurf_ele_grp_2nd)
      nullify(isurf_stack_ele_grp_2nd)
      nullify(isurf_ele_grp_2nd)
!
      end subroutine disconnect_sf_id_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine disconnect_eg_id_4_ele_g_2nd
!
      nullify(nedge_ele_grp_2nd)
      nullify(iedge_stack_ele_grp_2nd)
      nullify(iedge_ele_grp_2nd)
!
      end subroutine disconnect_eg_id_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine disconnect_nd_id_4_ele_g_2nd
!
      nullify(nnod_ele_grp_2nd)
      nullify(inod_stack_ele_grp_2nd)
      nullify(inod_ele_grp_2nd)
!
      end subroutine disconnect_nd_id_4_ele_g_2nd
!
!-----------------------------------------------------------------------
!
      end module m_2nd_ele_group_data
