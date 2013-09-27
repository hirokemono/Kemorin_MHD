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
!      subroutine count_node_comm_test(num_d, inter_d, id_global, x_org,&
!     &          id_gl_comm, x_comm, num_diff_l)
!      subroutine compare_nod_comm_test(num_d, inter_d,                 &
!     &          id_global, x_org, id_gl_comm, x_comm, num_diff_l,      &
!     &          id_diff, id_gl_diff, x_diff)
!
      module set_diff_geom_comm_test
!
      use m_precision
!
      use m_geometry_parameter
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
      use m_ele_comm_table
      use m_surface_geometry_data
      use m_surf_comm_table
      use m_edge_geometry_data
      use m_edge_comm_table
!
!
      call count_ele_comm_test(numele, globalelmid, x_ele,              &
     &    ntot_import_ele, item_import_ele, iele_gl_comm,               &
     &    x_ele_comm, nele_diff_local)
      call count_ele_comm_test(numsurf, globalsurfid, x_surf,           &
     &    ntot_import_surf, item_import_surf, isurf_gl_comm,            &
     &    x_surf_comm, nsurf_diff_local)
      call count_ele_comm_test(numedge, globaledgeid, x_edge,           &
     &    ntot_import_edge, item_import_edge, iedge_gl_comm,            &
     &    x_edge_comm, nedge_diff_local)
!
      end subroutine s_count_diff_geom_comm_test
!
! ----------------------------------------------------------------------
!
      subroutine s_set_diff_geom_comm_test
!
      use m_ele_comm_table
      use m_surface_geometry_data
      use m_surf_comm_table
      use m_edge_geometry_data
      use m_edge_comm_table
!
!
      call compare_ele_comm_test(numele, globalelmid, x_ele,            &
     &    ntot_import_ele, item_import_ele, iele_gl_comm,               &
     &    x_ele_comm, nele_diff_local, iele_diff,                       &
     &    iele_gl_diff, xele_diff)
      call compare_ele_comm_test(numsurf, globalsurfid, x_surf,         &
     &    ntot_import_surf, item_import_surf, isurf_gl_comm,            &
     &    x_surf_comm, nsurf_diff_local, isurf_diff,                    &
     &    isurf_gl_diff, xsurf_diff)
      call compare_ele_comm_test(numedge, globaledgeid, x_edge,         &
     &    ntot_import_edge, item_import_edge, iedge_gl_comm,            &
     &    x_edge_comm, nedge_diff_local, iedge_diff,                    &
     &    iedge_gl_diff, xedge_diff)
!
      end subroutine s_set_diff_geom_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_diff_node_comm_test
!
      use m_parallel_var_dof
      use m_array_for_send_recv
!
!
      call count_node_comm_test(numnod, internal_node,                  &
     &    globalnodid, xx, ix_vec, x_vec, nnod_diff_local)
!
      end subroutine count_diff_node_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_diff_node_comm_test
!
      use m_parallel_var_dof
      use m_array_for_send_recv
!
!
      call compare_nod_comm_test(numnod, internal_node,                 &
     &    globalnodid, xx, ix_vec, x_vec, nnod_diff_local,              &
     &    inod_diff, inod_gl_diff, xx_diff)
!
      end subroutine set_diff_node_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_node_comm_test(num_d, inter_d, id_global, x_org, &
     &          id_gl_comm, x_comm, num_diff_l)
!
      integer(kind = kint), intent(in) :: num_d, inter_d
      integer(kind = kint), intent(in) :: id_global(num_d)
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      integer(kind = kint), intent(in) :: id_gl_comm(num_d)
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint), intent(inout) :: num_diff_l
!
      integer(kind = kint) :: inod
!
      num_diff_l = 0
      do inod = inter_d+1, num_d
        if (   id_gl_comm(inod) .ne. id_global(inod)                    &
     &    .or. x_comm(3*inod-2)  .ne. x_org(inod,1)                     &
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
     &          id_global, x_org, id_gl_comm, x_comm, num_diff_l,       &
     &          id_diff, id_gl_diff, x_diff)
!
      integer(kind = kint), intent(in) :: inter_d, num_d
      integer(kind = kint), intent(in) :: id_global(num_d)
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      integer(kind = kint), intent(in) :: id_gl_comm(num_d)
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint), intent(in) :: num_diff_l
      integer(kind = kint), intent(inout) :: id_diff(num_diff_l)
      integer(kind = kint), intent(inout) :: id_gl_diff(2*num_diff_l)
      real(kind = kreal), intent(inout) :: x_diff(6*num_diff_l)
!
      integer(kind = kint) :: inod, icou
!
      icou = 0
      do inod = inter_d+1, num_d
        if (id_gl_comm(inod) .ne. id_global(inod)                       &
     &    .or. x_comm(3*inod-2) .ne. x_org(inod,1)                      &
     &    .or. x_comm(3*inod-1) .ne. x_org(inod,2)                      &
     &    .or. x_comm(3*inod  ) .ne. x_org(inod,3) ) then
          icou = icou + 1
          id_diff(icou) =        inod
          id_gl_diff(2*icou-1) = id_global(inod)
          id_gl_diff(2*icou  ) = id_gl_comm(inod)
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
      subroutine count_ele_comm_test(num_d, id_global, x_org,           &
     &          ntot_import_e, item_import_e, id_gl_comm, x_comm,       &
     &          num_diff_l)
!
      integer(kind = kint), intent(in) :: num_d
      integer(kind = kint), intent(in) :: id_global(num_d)
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      integer(kind = kint), intent(in) :: ntot_import_e
      integer(kind = kint), intent(in) :: item_import_e(ntot_import_e)
!
      integer(kind = kint), intent(in) :: id_gl_comm(num_d)
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint), intent(inout) :: num_diff_l
!
      integer(kind = kint) :: inum, iele
!
      num_diff_l = 0
      do inum = 1, ntot_import_e
        iele = item_import_e(inum)
        if (   id_gl_comm(iele)   .ne. id_global(iele)                  &
     &    .or. x_comm(3*iele-2) .ne. x_org(iele,1)                      &
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
      subroutine compare_ele_comm_test(num_d, id_global, x_org,         &
     &          ntot_import_e, item_import_e, id_gl_comm, x_comm,       &
     &          num_diff_l, id_diff, id_gl_diff, x_diff)
!
      integer(kind = kint), intent(in) :: num_d
      integer(kind = kint), intent(in) :: id_global(num_d)
      real(kind = kreal), intent(in) :: x_org(num_d,3)
!
      integer(kind = kint), intent(in) :: ntot_import_e
      integer(kind = kint), intent(in) :: item_import_e(ntot_import_e)
!
      integer(kind = kint), intent(in) :: id_gl_comm(num_d)
      real(kind = kreal), intent(in) :: x_comm(3*num_d)
!
      integer(kind = kint), intent(in) :: num_diff_l
      integer(kind = kint), intent(inout) :: id_diff(num_diff_l)
      integer(kind = kint), intent(inout) :: id_gl_diff(2*num_diff_l)
      real(kind = kreal), intent(inout) :: x_diff(6*num_diff_l)
!
      integer(kind = kint) :: inum, iele, icou
!
      icou = 0
      do inum = 1, ntot_import_e
        iele = item_import_e(inum)
        if (id_gl_comm(iele) .ne. id_global(iele)                       &
     &    .or. x_comm(3*iele-2) .ne. x_org(iele,1)                      &
     &    .or. x_comm(3*iele-1) .ne. x_org(iele,2)                      &
     &    .or. x_comm(3*iele  ) .ne. x_org(iele,3) ) then
          icou = icou + 1
          id_diff(icou) =        iele
          id_gl_diff(2*icou-1) = id_global(iele)
          id_gl_diff(2*icou  ) = id_gl_comm(iele)
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
