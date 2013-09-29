!
!      module mesh_send_recv_test
!
      module mesh_send_recv_test
!
!     Written by H. Matsui on Sep., 2007
!     Written by H. Matsui on Apr., 2008
!
      use m_precision
!
      use calypso_mpi
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_4_comm_test
      use solver_SR_3
      use solver_SR_int
!
      implicit  none
!
      private :: ele_send_recv_test, surf_send_recv_test
      private :: edge_send_recv_test
!
!      subroutine s_mesh_send_recv_test
!
!      subroutine node_send_recv_test
!      subroutine ele_send_recv_test
!      subroutine surf_send_recv_test
!      subroutine edge_send_recv_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_mesh_send_recv_test
!
      call ele_send_recv_test
      call surf_send_recv_test
      call edge_send_recv_test
!
      end subroutine s_mesh_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine node_send_recv_test
!
      use m_nod_comm_table
      use m_array_for_send_recv
!
      integer(kind = kint) :: inod
!
!
      do inod = 1, internal_node
        ix_vec(inod) = globalnodid(inod)
        x_vec(3*inod-2) = xx(inod,1)
        x_vec(3*inod-1) = xx(inod,2)
        x_vec(3*inod  ) = xx(inod,3)
      end do
!
      call solver_send_recv_i(numnod, num_neib, id_neib,                &
     &                        istack_import, item_import,               &
     &                        istack_export, item_export, ix_vec)
!
      call solver_send_recv_3(numnod, num_neib, id_neib,                &
     &                        istack_import, item_import,               &
     &                        istack_export, item_export, x_vec)
!
      end subroutine node_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine ele_send_recv_test
!
      use m_ele_comm_table
!
      integer(kind = kint) :: iele, inum
!
!
      do iele = 1, numele
        iele_gl_comm(iele) = globalelmid(iele)
        x_ele_comm(3*iele-2) = x_ele(iele,1)
        x_ele_comm(3*iele-1) = x_ele(iele,2)
        x_ele_comm(3*iele  ) = x_ele(iele,3)
      end do
      do inum = 1, ntot_import_ele
        iele = item_import_ele(inum)
        iele_gl_comm(iele) = 0
        x_ele_comm(3*iele-2) = 0.0d0
        x_ele_comm(3*iele-1) = 0.0d0
        x_ele_comm(3*iele  ) = 0.0d0
      end do
!
      call solver_send_recv_i(numele, num_neib_ele, id_neib_ele,        &
     &                        istack_import_ele, item_import_ele,       &
     &                        istack_export_ele, item_export_ele,       &
     &                        iele_gl_comm)
!
      call solver_send_recv_3(numele, num_neib_ele, id_neib_ele,        &
     &                        istack_import_ele, item_import_ele,       &
     &                        istack_export_ele, item_export_ele,       &
     &                        x_ele_comm)
!
      end subroutine ele_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine surf_send_recv_test
!
      use m_surface_geometry_data
      use m_surf_comm_table
!
      integer(kind = kint) :: isurf, inum
!
!
      do isurf = 1, numsurf
        isurf_gl_comm(isurf) = globalsurfid(isurf)
        x_surf_comm(3*isurf-2) = x_surf(isurf,1)
        x_surf_comm(3*isurf-1) = x_surf(isurf,2)
        x_surf_comm(3*isurf  ) = x_surf(isurf,3)
      end do
      do inum = 1, ntot_import_surf
        isurf = item_import_surf(inum)
        isurf_gl_comm(isurf) = 0
        x_surf_comm(3*isurf-2) = 0.0d0
        x_surf_comm(3*isurf-1) = 0.0d0
        x_surf_comm(3*isurf  ) = 0.0d0
      end do
!
      call solver_send_recv_i(numsurf, num_neib_surf, id_neib_surf,     &
     &                        istack_import_surf, item_import_surf,     &
     &                        istack_export_surf, item_export_surf,     &
     &                        isurf_gl_comm)
!
      call solver_send_recv_3(numsurf, num_neib_surf, id_neib_surf,     &
     &                        istack_import_surf, item_import_surf,     &
     &                        istack_export_surf, item_export_surf,     &
     &                        x_surf_comm)
!
      end subroutine surf_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine edge_send_recv_test
!
      use m_edge_geometry_data
      use m_edge_comm_table
!
      integer(kind = kint) :: iedge, inum
!
!
      do iedge = 1, numedge
        iedge_gl_comm(iedge) = globaledgeid(iedge)
        x_edge_comm(3*iedge-2) = x_edge(iedge,1)
        x_edge_comm(3*iedge-1) = x_edge(iedge,2)
        x_edge_comm(3*iedge  ) = x_edge(iedge,3)
      end do
      do inum = 1, ntot_import_edge
        iedge = item_import_edge(inum)
        iedge_gl_comm(iedge) = 0
        x_edge_comm(3*iedge-2) = 0.0d0
        x_edge_comm(3*iedge-1) = 0.0d0
        x_edge_comm(3*iedge  ) = 0.0d0
      end do
!
      call solver_send_recv_i(numedge, num_neib_edge, id_neib_edge,     &
     &                        istack_import_edge, item_import_edge,     &
     &                        istack_export_edge, item_export_edge,     &
     &                        iedge_gl_comm)
!
      call solver_send_recv_3(numedge, num_neib_edge, id_neib_edge,     &
     &                        istack_import_edge, item_import_edge,     &
     &                        istack_export_edge, item_export_edge,     &
     &                        x_edge_comm)
!
      end subroutine edge_send_recv_test
!
! ----------------------------------------------------------------------
!
      end module mesh_send_recv_test
