!
!      module const_geometry_z_commute
!
!     Written by Hiroaki Matsui
!     Modified by Hiroaki Matsui on Apr., 2008
!
!!      subroutine set_geometry_z_commute                               &
!!     &         (nod_comm, node, ele, surf, edge)
!
      module const_geometry_z_commute
!
      use m_precision
      use m_constants
!
      use m_commute_filter_z
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit none
!
      private :: set_numnod_z_commute, set_global_id_z_commute
      private :: set_element_z_commute, set_liner_grids
      private :: set_chebyshev_grids, set_half_chebyshev_grids
      private :: set_test_grids, set_test_grids_2
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_geometry_z_commute                                 &
     &         (nod_comm, node, ele, surf, edge)
!
      use m_spheric_constants
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      ncomp_mat = ncomp_norm + 2
      if (numfilter .eq. 1) then
        ncomp_mat = ncomp_norm
      end if
!
      call set_numnod_z_commute(node, ele, surf, edge)
!
      call alloc_node_geometry_w_sph(node)
      call alloc_edge_connect(edge, surf%numsurf)
!
      call set_element_z_commute(node, edge)
      call set_global_id_z_commute(node, ele, edge)
!
      if (iflag_grid .eq. igrid_Chebyshev) then
        call set_chebyshev_grids                                        &
     &     (node%numnod, node%internal_node, node%xx)
      else if (iflag_grid .eq. igrid_half_Chebyshev) then
        call set_half_chebyshev_grids                                   &
     &     (node%numnod, node%internal_node, node%xx)
      else if (iflag_grid.eq.-1) then
        call set_test_grids(node%numnod, node%internal_node, node%xx)
      else if (iflag_grid.eq.-2) then
        call set_test_grids_2                                           &
     &     (node%numnod, node%internal_node, node%xx)
      else
        call set_liner_grids(node%numnod, node%internal_node, node%xx)
      end if
!
      nod_comm%num_neib =    0
      nod_comm%ntot_import = 0
      nod_comm%ntot_export = 0
      call allocate_type_comm_tbl_num(nod_comm)
      call allocate_type_comm_tbl_item(nod_comm)
!
      end subroutine set_geometry_z_commute
!
! ----------------------------------------------------------------------
!
      subroutine  set_numnod_z_commute(node, ele, surf, edge)
!
!      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      node%internal_node = totalnod
      node%numnod =  node%internal_node
      totalele = node%internal_node - 1
!      nod_comm%num_neib = 2
!
      nmat_ele = totalele*nfilter2_1
      nmat_nod = node%internal_node * nfilter2_3
!
      ele%numele = node%numnod - 1
!
      ele%first_ele_type = 331
      surf%numsurf = ele%numele
      edge%numedge = ele%numele
      edge%nnod_4_edge = 2
!
      nfilter2_1 = 2*numfilter+1
      nfilter2_2 = 2*numfilter+2
      nfilter2_3 = 2*numfilter+3
      nfilter2_4 = 2*numfilter+4
      nfilter6_1 = 6*numfilter+1
!
      end subroutine  set_numnod_z_commute
!
! ----------------------------------------------------------------------
!
      subroutine set_global_id_z_commute(node, ele, edge)
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(edge_data), intent(inout) :: edge
!
      integer (kind = kint) :: i
!
      do i = 1, node%internal_node
        node%inod_global(i) = i
      end do
!
      do i = 1, ele%numele
        edge%iedge_global(i) = node%inod_global(edge%ie_edge(i,1))
      end do
!
      end subroutine set_global_id_z_commute
!
! ----------------------------------------------------------------------
!
      subroutine set_element_z_commute(node, edge)
!
      type(node_data), intent(inout) :: node
      type(edge_data), intent(inout) :: edge
!
      integer (kind = kint) :: i
!
      do i = 1, node%internal_node - 1
        edge%ie_edge(i,1) = i
        edge%ie_edge(i,2) = i+1
      end do
!
      end subroutine set_element_z_commute
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_liner_grids(numnod, internal_node, xx)
!
      integer (kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
      integer (kind = kint) :: i
!
      do i = 1, numnod
        xx(i,3) = zsize * (-0.5d0 + dble(i-1)                           &
     &                   / dble(internal_node-1) )
      end do
!
      end subroutine set_liner_grids
!
!  ---------------------------------------------------------------------
!
      subroutine set_chebyshev_grids(numnod, internal_node, xx)
!
      integer (kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i
!
      pi = four * atan(one)
!
        do i = 1, numnod
          xx(i,3) = -0.5d0 * zsize                                      &
     &         * cos (pi* dble(i - 1) / dble(internal_node-1) )
        end do
!
      end subroutine set_chebyshev_grids
!
!  ---------------------------------------------------------------------
!
      subroutine set_half_chebyshev_grids(numnod, internal_node, xx)
!
      integer (kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i
!
      pi = four * atan(one)
!
        do i = 1, numnod
          xx(i,3) = -0.5d0*zsize - zsize                                &
     &         * cos (pi* dble(i-1) / dble(2*(internal_node-1)) )
        end do
!
      end subroutine set_half_chebyshev_grids
!
!  ---------------------------------------------------------------------
!
      subroutine set_test_grids(numnod, internal_node, xx)
!
      integer (kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
      integer (kind = kint) :: i
!
!
        do i = 1, numnod
          xx(i,3) = 2.0d0 * dble(i-1) - dble(internal_node-1)
        end do
!
      end subroutine set_test_grids
!
!  ---------------------------------------------------------------------
!
      subroutine set_test_grids_2(numnod, internal_node, xx)
!
      integer (kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i
!
      pi = four * atan(one)
!
        do i = 1, numnod
          xx(i,3) = - dble(internal_node-1)                             &
     &             * cos (pi* dble(i - 1) / dble(internal_node-1) ) 
        end do
!
      end subroutine set_test_grids_2
!
!  ---------------------------------------------------------------------
!
      end module const_geometry_z_commute
