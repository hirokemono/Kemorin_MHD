!
!      module set_diff_geom_comm_test
!
!     Written by H. Matsui on Sep., 2007
!
!!      subroutine s_count_diff_geom_comm_test                          &
!!     &         (ele, surf, edge, ele_comm, surf_comm, edge_comm)
!!      subroutine s_set_diff_geom_comm_test                            &
!!     &         (ele, surf, edge, ele_comm, surf_comm, edge_comm)
!!
!!      subroutine count_diff_node_comm_test(node, v_sol)
!!      subroutine set_diff_node_comm_test(node, v_sol)
!!        type(vectors_4_solver), intent(in) :: v_sol
!!
!!      subroutine count_node_comm_test(num_d, inter_d, x_org,          &
!!     &          x_comm, num_diff_l)
!!      subroutine compare_nod_comm_test(num_d, inter_d,                &
!!     &          x_org, x_comm, num_diff_l, id_diff, x_diff)
!
      module set_diff_geom_comm_test
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_comm_table
      use t_vector_for_solver
      use m_geometry_4_comm_test
!
      implicit  none
!
!
      private :: compare_ele_comm_test, count_ele_comm_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_count_diff_geom_comm_test                            &
     &         (ele, surf, edge, ele_comm, surf_comm, edge_comm)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: ele_comm
      type(communication_table), intent(in) :: surf_comm
      type(communication_table), intent(in) :: edge_comm
!
!
      nele_diff_local = count_ele_comm_test                             &
     &   (ele%numele, ele%x_ele, ele_comm%ntot_import,                  &
     &    ele_comm%item_import, ele_check%xx_test)
      nsurf_diff_local =  count_ele_comm_test                           &
     &   (surf%numsurf, surf%x_surf,               &
     &    surf_comm%ntot_import, surf_comm%item_import,                 &
     &    surf_check%xx_test)
      nedge_diff_local =  count_ele_comm_test                       &
     &   (edge%numedge, edge%x_edge,               &
     &    edge_comm%ntot_import,  edge_comm%item_import,                &
     &    edge_check%xx_test)
!
      end subroutine s_count_diff_geom_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine s_set_diff_geom_comm_test                              &
     &         (ele, surf, edge, ele_comm, surf_comm, edge_comm)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: ele_comm
      type(communication_table), intent(in) :: surf_comm
      type(communication_table), intent(in) :: edge_comm
!
!
      call compare_ele_comm_test(ele%numele, ele%x_ele,                 &
     &    ele_comm%ntot_import, ele_comm%item_import,                   &
     &    ele_check%xx_test, nele_diff_local, iele_diff, xele_diff)
      call compare_ele_comm_test(surf%numsurf, surf%x_surf,             &
     &    surf_comm%ntot_import, surf_comm%item_import,                 &
     &    surf_check%xx_test, nsurf_diff_local, isurf_diff, xsurf_diff)
      call compare_ele_comm_test(edge%numedge, edge%x_edge,             &
     &    edge_comm%ntot_import, edge_comm%item_import,                 &
     &    edge_check%xx_test, nedge_diff_local, iedge_diff, xedge_diff)
!
      end subroutine s_set_diff_geom_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_diff_node_comm_test(node, v_sol)
!
      use calypso_mpi
!
      type(node_data), intent(in) :: node
      type(vectors_4_solver), intent(in) :: v_sol
!
!
      call count_node_comm_test(node%numnod, node%internal_node,        &
     &    node%xx, v_sol%x_vec(1), nnod_diff_local)
!
      end subroutine count_diff_node_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_diff_node_comm_test(node, v_sol)
!
      use calypso_mpi
!
      type(node_data), intent(in) :: node
      type(vectors_4_solver), intent(in) :: v_sol
!
!
      call compare_nod_comm_test(node%numnod, node%internal_node,       &
     &    node%xx, v_sol%x_vec(1), nnod_diff_local, inod_diff, xx_diff)
!
      end subroutine set_diff_node_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_node_comm_test(num_d, inter_d, x_org,            &
     &          x_comm, num_diff_l)
!
      integer(kind = kint), intent(in) :: num_d, inter_d
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint), intent(inout) :: num_diff_l
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: diff
!
      num_diff_l = 0
      do inod = inter_d+1, num_d
        diff =  abs(x_comm(3*inod-2) - x_org(inod,1))                   &
     &        + abs(x_comm(3*inod-1) - x_org(inod,2))                   &
     &        + abs(x_comm(3*inod  ) - x_org(inod,3))
        if (diff .gt. TINY) num_diff_l = num_diff_l + 1
      end do
!
      end subroutine count_node_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine compare_nod_comm_test(num_d, inter_d,                  &
     &          x_org, x_comm, num_diff_l, id_diff, x_diff)
!
      integer(kind = kint), intent(in) :: inter_d, num_d
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint), intent(in) :: num_diff_l
      integer(kind = kint), intent(inout) :: id_diff(num_diff_l)
      real(kind = kreal), intent(inout) :: x_diff(6*num_diff_l)
!
      integer(kind = kint) :: inod, icou
      real(kind = kreal) :: diff
!
      icou = 0
      do inod = inter_d+1, num_d
        diff =  abs(x_comm(3*inod-2) - x_org(inod,1))                   &
     &        + abs(x_comm(3*inod-1) - x_org(inod,2))                   &
     &        + abs(x_comm(3*inod  ) - x_org(inod,3))
        if(diff .gt. TINY) then
          icou = icou + 1
          id_diff(icou) =        inod
          x_diff(6*icou-5) =      x_org(inod,1)
          x_diff(6*icou-4) =      x_org(inod,2)
          x_diff(6*icou-3) =      x_org(inod,3)
          x_diff(6*icou-2) =      x_comm(3*inod-2)
          x_diff(6*icou-1) =      x_comm(3*inod-1)
          x_diff(6*icou  ) =      x_comm(3*inod  )
        end if
      end do
!
      end subroutine compare_nod_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function count_ele_comm_test                 &
     &         (num_d, x_org, ntot_import_e, item_import_e, x_comm)
!
      integer(kind = kint), intent(in) :: num_d
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      integer(kind = kint), intent(in) :: ntot_import_e
      integer(kind = kint), intent(in) :: item_import_e(ntot_import_e)
!
      real(kind = kreal), intent(in) :: x_comm(num_d,3)
!
      integer(kind = kint) :: num_diff_l
      integer(kind = kint) :: inum, iele
      real(kind = kreal) :: diff
!
      num_diff_l = 0
      do inum = 1, ntot_import_e
        iele = item_import_e(inum)
        diff =  abs(x_comm(iele,1) - x_org(iele,1))                     &
     &        + abs(x_comm(iele,2) - x_org(iele,2))                     &
     &        + abs(x_comm(iele,3) - x_org(iele,3))
        if (diff .gt. TINY) num_diff_l = num_diff_l + 1
      end do
      count_ele_comm_test = num_diff_l
!
      end function count_ele_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine compare_ele_comm_test(num_d, x_org,                    &
     &          ntot_import_e, item_import_e, x_comm,                   &
     &          num_diff_l, id_diff, x_diff)
!
      integer(kind = kint), intent(in) :: num_d
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      integer(kind = kint), intent(in) :: ntot_import_e
      integer(kind = kint), intent(in) :: item_import_e(ntot_import_e)
!
      real(kind = kreal), intent(in) :: x_comm(num_d,3)
!
      integer(kind = kint), intent(in) :: num_diff_l
      integer(kind = kint), intent(inout) :: id_diff(num_diff_l)
      real(kind = kreal), intent(inout) :: x_diff(6*num_diff_l)
!
      integer(kind = kint) :: inum, iele, icou
      real(kind = kreal) :: diff
!
      icou = 0
      do inum = 1, ntot_import_e
        iele = item_import_e(inum)
        diff =  abs(x_comm(iele,1) - x_org(iele,1))                   &
     &        + abs(x_comm(iele,2) - x_org(iele,2))                   &
     &        + abs(x_comm(iele,3) - x_org(iele,3))
        if (diff .gt. TINY) then
          icou = icou + 1
          id_diff(icou) =        iele
          x_diff(6*icou-5) =      x_org(iele,1)
          x_diff(6*icou-4) =      x_org(iele,2)
          x_diff(6*icou-3) =      x_org(iele,3)
          x_diff(6*icou-2) =      x_comm(iele,1)
          x_diff(6*icou-1) =      x_comm(iele,2)
          x_diff(6*icou  ) =      x_comm(iele,3)
        end if
      end do
!
      end subroutine compare_ele_comm_test
!
! ----------------------------------------------------------------------
!
      end module set_diff_geom_comm_test
