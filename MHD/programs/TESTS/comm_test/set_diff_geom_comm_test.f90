!
!      module set_diff_geom_comm_test
!
!     Written by H. Matsui on Sep., 2007
!
!      subroutine s_count_diff_geom_comm_test
!      subroutine s_set_diff_geom_comm_test
!
!      subroutine count_diff_node_comm_test
!      subroutine set_diff_node_comm_test
!
!      subroutine count_node_comm_test(num_d, inter_d, x_org,           &
!     &          x_comm, num_diff_l)
!      subroutine compare_nod_comm_test(num_d, inter_d,                 &
!     &          x_org, x_comm, num_diff_l, id_diff, x_diff)
!
      module set_diff_geom_comm_test
!
      use m_precision
!
      use m_geometry_data
      use m_geometry_4_comm_test
!
      implicit  none
!
      private :: compare_ele_comm_test, count_ele_comm_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_count_diff_geom_comm_test
!
      use m_ele_sf_eg_comm_tables
!
!
      call count_ele_comm_test                                          &
     &   (ele1%numele, ele1%x_ele, ele_comm%ntot_import,                &
     &    ele_comm%item_import, x_ele_comm, nele_diff_local)
      call count_ele_comm_test(surf1%numsurf, surf1%x_surf,             &
     &    surf_comm%ntot_import, surf_comm%item_import,                 &
     &    x_surf_comm, nsurf_diff_local)
      call count_ele_comm_test(edge1%numedge, edge1%x_edge,             &
     &    edge_comm%ntot_import,  edge_comm%item_import,                &
     &    x_edge_comm, nedge_diff_local)
!
      end subroutine s_count_diff_geom_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine s_set_diff_geom_comm_test
!
      use m_ele_sf_eg_comm_tables
!
!
      call compare_ele_comm_test(ele1%numele, ele1%x_ele,               &
     &    ele_comm%ntot_import, ele_comm%item_import,                   &
     &    x_ele_comm, nele_diff_local, iele_diff, xele_diff)
      call compare_ele_comm_test(surf1%numsurf, surf1%x_surf,           &
     &    surf_comm%ntot_import, surf_comm%item_import,                 &
     &    x_surf_comm, nsurf_diff_local, isurf_diff, xsurf_diff)
      call compare_ele_comm_test(edge1%numedge, edge1%x_edge,           &
     &    edge_comm%ntot_import, edge_comm%item_import,                 &
     &    x_edge_comm, nedge_diff_local, iedge_diff, xedge_diff)
!
      end subroutine s_set_diff_geom_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_diff_node_comm_test
!
      use calypso_mpi
      use m_array_for_send_recv
!
!
      call count_node_comm_test(node1%numnod, node1%internal_node,      &
     &    node1%xx, x_vec, nnod_diff_local)
!
      end subroutine count_diff_node_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_diff_node_comm_test
!
      use calypso_mpi
      use m_array_for_send_recv
!
!
      call compare_nod_comm_test(node1%numnod, node1%internal_node,     &
     &    node1%xx, x_vec, nnod_diff_local, inod_diff, xx_diff)
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
!
      num_diff_l = 0
      do inod = inter_d+1, num_d
        if (   x_comm(3*inod-2)  .ne. x_org(inod,1)                     &
     &    .or. x_comm(3*inod-1)  .ne. x_org(inod,2)                     &
     &    .or. x_comm(3*inod  )  .ne. x_org(inod,3) ) then
          num_diff_l = num_diff_l + 1
        end if
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
!
      icou = 0
      do inod = inter_d+1, num_d
        if (   x_comm(3*inod-2) .ne. x_org(inod,1)                      &
     &    .or. x_comm(3*inod-1) .ne. x_org(inod,2)                      &
     &    .or. x_comm(3*inod  ) .ne. x_org(inod,3) ) then
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
      subroutine count_ele_comm_test(num_d, x_org,                      &
     &          ntot_import_e, item_import_e, x_comm, num_diff_l)
!
      integer(kind = kint), intent(in) :: num_d
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      integer(kind = kint), intent(in) :: ntot_import_e
      integer(kind = kint), intent(in) :: item_import_e(ntot_import_e)
!
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint), intent(inout) :: num_diff_l
!
      integer(kind = kint) :: inum, iele
!
      num_diff_l = 0
      do inum = 1, ntot_import_e
        iele = item_import_e(inum)
        if (   x_comm(3*iele-2) .ne. x_org(iele,1)                      &
     &    .or. x_comm(3*iele-1) .ne. x_org(iele,2)                      &
     &    .or. x_comm(3*iele  ) .ne. x_org(iele,3) ) then
          num_diff_l = num_diff_l + 1
        end if
      end do
!
      end subroutine count_ele_comm_test
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
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint), intent(in) :: num_diff_l
      integer(kind = kint), intent(inout) :: id_diff(num_diff_l)
      real(kind = kreal), intent(inout) :: x_diff(6*num_diff_l)
!
      integer(kind = kint) :: inum, iele, icou
!
      icou = 0
      do inum = 1, ntot_import_e
        iele = item_import_e(inum)
        if (   x_comm(3*iele-2) .ne. x_org(iele,1)                      &
     &    .or. x_comm(3*iele-1) .ne. x_org(iele,2)                      &
     &    .or. x_comm(3*iele  ) .ne. x_org(iele,3) ) then
          icou = icou + 1
          id_diff(icou) =        iele
          x_diff(6*icou-5) =      x_org(iele,1)
          x_diff(6*icou-4) =      x_org(iele,2)
          x_diff(6*icou-3) =      x_org(iele,3)
          x_diff(6*icou-2) =      x_comm(3*iele-2)
          x_diff(6*icou-1) =      x_comm(3*iele-1)
          x_diff(6*icou  ) =      x_comm(3*iele  )
        end if
      end do
!
      end subroutine compare_ele_comm_test
!
! ----------------------------------------------------------------------
!
      end module set_diff_geom_comm_test
