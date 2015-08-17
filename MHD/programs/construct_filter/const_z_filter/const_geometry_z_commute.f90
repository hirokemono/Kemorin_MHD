!
!      module const_geometry_z_commute
!
!     Written by Hiroaki Matsui
!     Modified by Hiroaki Matsui on Apr., 2008
!
!      subroutine set_geometry_z_commute
!
      module const_geometry_z_commute
!
      use m_precision
      use m_constants
!
      use m_nod_comm_table
      use m_geometry_data
      use m_commute_filter_z
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
      subroutine set_geometry_z_commute
!
      use m_spheric_constants
!
!
      ncomp_mat = ncomp_norm + 2
      if (numfilter .eq. 1) then
        ncomp_mat = ncomp_norm
      end if
!
      call set_numnod_z_commute
!
      call allocate_node_geometry_type(node1)
      call allocate_sph_node_geometry(node1)
      call allocate_edge_connect
!
      call set_element_z_commute
      call set_global_id_z_commute
!
      if (iflag_grid .eq. igrid_Chebyshev) then
        call set_chebyshev_grids(node1%numnod, node1%xx)
      else if (iflag_grid .eq. igrid_half_Chebyshev) then
        call set_half_chebyshev_grids(node1%numnod, node1%xx)
      else if (iflag_grid.eq.-1) then
        call set_test_grids(node1%numnod, node1%xx)
      else if (iflag_grid.eq.-2) then
        call set_test_grids_2(node1%numnod, node1%xx)
      else
        call set_liner_grids(node1%numnod, node1%xx)
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
      subroutine  set_numnod_z_commute
!
!
      node1%internal_node = totalnod
      node1%numnod =  node1%internal_node
      totalele = node1%internal_node - 1
!      nod_comm%num_neib = 2
!
      nmat_ele = totalele*nfilter2_1
      nmat_nod = node1%internal_node * nfilter2_3
!
      ele1%numele = node1%numnod - 1
!
      ele1%first_ele_type = 331
      surf1%numsurf = ele1%numele
      edge1%numedge = ele1%numele
      edge1%nnod_4_edge = 2
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
      subroutine set_global_id_z_commute
!
      use m_geometry_data
!
      integer (kind = kint) :: i
!
      do i = 1, node1%internal_node
        node1%inod_global(i) = i
      end do
!
      do i = 1, ele1%numele
        iedge_global(i) = node1%inod_global(edge1%ie_edge(i,1))
      end do
!
      end subroutine set_global_id_z_commute
!
! ----------------------------------------------------------------------
!
      subroutine set_element_z_commute
!
      integer (kind = kint) :: i
!
      do i = 1, node1%internal_node - 1
        edge1%ie_edge(i,1) = i
        edge1%ie_edge(i,2) = i+1
      end do
!
      end subroutine set_element_z_commute
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_liner_grids(numnod, xx)
!
      integer (kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
      integer (kind = kint) :: i
!
      do i = 1, numnod
        xx(i,3) = zsize * (-0.5d0 + dble(i-1)                           &
     &                   / dble(node1%internal_node-1) )
      end do
!
      end subroutine set_liner_grids
!
!  ---------------------------------------------------------------------
!
      subroutine set_chebyshev_grids(numnod, xx)
!
      integer (kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i
!
      pi = four * atan(one)
!
        do i = 1, numnod
          xx(i,3) = -0.5d0 * zsize                                      &
     &         * cos (pi* dble(i - 1) / dble(node1%internal_node-1) )
        end do
!
      end subroutine set_chebyshev_grids
!
!  ---------------------------------------------------------------------
!
      subroutine set_half_chebyshev_grids(numnod, xx)
!
      integer (kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i
!
      pi = four * atan(one)
!
        do i = 1, numnod
          xx(i,3) = -0.5d0*zsize - zsize                                &
     &         * cos (pi* dble(i-1) / dble(2*(node1%internal_node-1)) )
        end do
!
      end subroutine set_half_chebyshev_grids
!
!  ---------------------------------------------------------------------
!
      subroutine set_test_grids(numnod, xx)
!
      integer (kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
      integer (kind = kint) :: i
!
!
        do i = 1, numnod
          xx(i,3) = 2.0d0 * dble(i-1) - dble(node1%internal_node-1)
        end do
!
      end subroutine set_test_grids
!
!  ---------------------------------------------------------------------
!
      subroutine set_test_grids_2(numnod, xx)
!
      integer (kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(inout) :: xx(numnod,3)
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i
!
      pi = four * atan(one)
!
        do i = 1, numnod
          xx(i,3) = - dble(node1%internal_node-1)                       &
     &         * cos (pi* dble(i - 1) / dble(node1%internal_node-1) ) 
        end do
!
      end subroutine set_test_grids_2
!
!  ---------------------------------------------------------------------
!
      end module const_geometry_z_commute
