!
!     module m_2nd_surf_group_data
!
!     Writteg by H.Matsui on Sep., 2006
!
!      subroutine allocate_sf_id_4_sf_g_2nd
!
!      subroutine allocate_eg_stack_4_sf_g_2nd
!      subroutine allocate_eg_id_4_sf_g_2nd
!
!      subroutine allocate_nd_stack_4_sf_g_2nd
!      subroutine allocate_nd_id_4_sf_g_2nd
!
!      subroutine deallocate_sf_id_4_sf_g_2nd
!      subroutine deallocate_eg_id_4_sf_g_2nd
!      subroutine deallocate_nd_id_4_sf_g_2nd
!
!      subroutine disconnect_sf_id_4_sf_g_2nd
!      subroutine disconnect_eg_id_4_sf_g_2nd
!      subroutine disconnect_nd_id_4_sf_g_2nd
!
!       subroutine deallocate_2nd_sf_nod_grp_smp(num_surf_2nd_smp)
!       subroutine deallocate_2nd_sf_nod_grp_smp
!
      module m_2nd_surf_group_data
!
      use m_precision
!
      implicit  none
!
      integer(kind=kint), pointer :: isurf_grp_2nd(:)
      integer(kind=kint), pointer :: isurf_grp_n_2nd(:)
!
      integer(kind=kint) :: ntot_edge_sf_grp_2nd
      integer(kind=kint), pointer :: nedge_sf_grp_2nd(:)
      integer(kind=kint), pointer :: iedge_stack_sf_grp_2nd(:)
!
      integer(kind=kint), pointer :: iedge_surf_grp_2nd(:)
!
!
      integer(kind=kint) :: ntot_node_sf_grp_2nd
      integer(kind=kint), pointer :: nnod_sf_grp_2nd(:)
      integer(kind=kint), pointer :: inod_stack_sf_grp_2nd(:)
!
      integer(kind=kint), pointer :: inod_surf_grp_2nd(:)
!
      integer( kind=kint ), pointer :: isurf_nod_2nd_smp_stack(:)
!     number of node on this PE
      integer( kind=kint )  ::  max_sf_nod_2nd_4_smp
!     number of node on whole processes

!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_sf_id_4_sf_g_2nd
!
      use m_2nd_group_data
!
      allocate(isurf_grp_2nd(num_surf_bc_2nd))
      allocate(isurf_grp_n_2nd(num_surf_bc_2nd))
      if(num_surf_bc_2nd .gt. 0) then
        isurf_grp_2nd =   0
        isurf_grp_n_2nd = 0
      end if
!
      end subroutine allocate_sf_id_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine allocate_eg_stack_4_sf_g_2nd
!
      use m_2nd_group_data
!
      allocate(nedge_sf_grp_2nd(num_surf_2nd))
      allocate(iedge_stack_sf_grp_2nd(0:num_surf_2nd))
!
      nedge_sf_grp_2nd = 0
      iedge_stack_sf_grp_2nd = 0
!
      end subroutine allocate_eg_stack_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine allocate_eg_id_4_sf_g_2nd
!
      allocate(iedge_surf_grp_2nd(ntot_edge_sf_grp_2nd))
      iedge_surf_grp_2nd = 0
!
      end subroutine allocate_eg_id_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine allocate_nd_stack_4_sf_g_2nd
!
      use m_2nd_group_data
!
      allocate(nnod_sf_grp_2nd(num_surf_2nd))
      allocate(inod_stack_sf_grp_2nd(0:num_surf_2nd))
!
      nnod_sf_grp_2nd = 0
      inod_stack_sf_grp_2nd = 0
!
      end subroutine allocate_nd_stack_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine allocate_nd_id_4_sf_g_2nd
!
      allocate(inod_surf_grp_2nd(ntot_node_sf_grp_2nd))
      inod_surf_grp_2nd = 0
!
      end subroutine allocate_nd_id_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_sf_id_4_sf_g_2nd
!
      deallocate(isurf_grp_2nd, isurf_grp_n_2nd)
!
      end subroutine deallocate_sf_id_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_eg_id_4_sf_g_2nd
!
      deallocate(nedge_sf_grp_2nd)
      deallocate(iedge_stack_sf_grp_2nd)
      deallocate(iedge_surf_grp_2nd)
!
      end subroutine deallocate_eg_id_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_nd_id_4_sf_g_2nd
!
      deallocate(nnod_sf_grp_2nd)
      deallocate(inod_stack_sf_grp_2nd)
      deallocate(inod_surf_grp_2nd)
!
      end subroutine deallocate_nd_id_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine disconnect_sf_id_4_sf_g_2nd
!
      nullify(isurf_grp_2nd, isurf_grp_n_2nd)
!
      end subroutine disconnect_sf_id_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine disconnect_eg_id_4_sf_g_2nd
!
      nullify(nedge_sf_grp_2nd)
      nullify(iedge_stack_sf_grp_2nd)
      nullify(iedge_surf_grp_2nd)
!
      end subroutine disconnect_eg_id_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!
      subroutine disconnect_nd_id_4_sf_g_2nd
!
      nullify(nnod_sf_grp_2nd)
      nullify(inod_stack_sf_grp_2nd)
      nullify(inod_surf_grp_2nd)
!
      end subroutine disconnect_nd_id_4_sf_g_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine allocate_2nd_sf_nod_grp_smp(num_surf_2nd_smp)
!
       integer(kind = kint), intent(in) :: num_surf_2nd_smp
!
       allocate( isurf_nod_2nd_smp_stack(0:num_surf_2nd_smp))
       isurf_nod_2nd_smp_stack = 0
!
       end subroutine allocate_2nd_sf_nod_grp_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_2nd_sf_nod_grp_smp
!
       deallocate(isurf_nod_2nd_smp_stack)
!
       end subroutine deallocate_2nd_sf_nod_grp_smp
!
!-----------------------------------------------------------------------
!
      end module m_2nd_surf_group_data
