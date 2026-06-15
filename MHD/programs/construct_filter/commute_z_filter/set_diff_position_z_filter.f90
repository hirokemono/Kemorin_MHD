!set_diff_position_z_filter.f90
!      module set_diff_position_z_filter
!
!      Written by H. Matsui
!
!      subroutine set_difference_of_position(node, edge)
!
      module set_diff_position_z_filter
!
      use m_precision
!
      use t_geometry_data
      use t_edge_data
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_difference_of_position(node, edge,                 &
     &          nneib_ele, ineib_ele, alpha)
!
      use m_commute_filter_z
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
!      integer(kind = kint), intent(in) :: totalele, nfilter2_1
      integer(kind = kint), intent(in) :: nneib_ele(totalele,2)
      integer(kind = kint), intent(in)                                  &
     &                     :: ineib_ele(totalele,nfilter2_1,2)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: alpha(totalele,0:nfilter2_1,2)
!
      real(kind = kreal) :: dz0, dzeta
      integer(kind = kint) :: i, j, inod1, inod2, jnod1, jnod2, jele
!
!
      write(*,*) size(alpha,1), size(alpha,2), size(alpha,3)
!
      do i = 1, totalele
        inod1 = edge%ie_edge(i,1)
        inod2 = edge%ie_edge(i,2)
        jnod1 = edge%ie_edge(i,1)
        jnod2 = edge%ie_edge(i,2)
        dz0 =   node%xx(inod2,3) - node%xx(inod1,3)
        dzeta = node%xx(jnod2,3) - node%xx(jnod1,3)
        alpha(i,0,1) = dz0/dzeta - 1
        alpha(i,0,2) = alpha(i,1,1)
!
        do j = 1, nneib_ele(i,1)
          jele = ineib_ele(i,j,1)
          jnod1 = edge%ie_edge(jele,1)
          jnod2 = edge%ie_edge(jele,2)
          dzeta = node%xx(jnod2,3) - node%xx(jnod1,3)
          alpha(i,j,1) = dz0/dzeta - 1
        end do
!
        do j = 1, nneib_ele(i,2)
          jele = ineib_ele(i,j,2)
          jnod1 = edge%ie_edge(jele,1)
          jnod2 = edge%ie_edge(jele,2)
          dzeta = node%xx(jnod2,3) - node%xx(jnod1,3)
          alpha(i,j,2) = dz0/dzeta - 1
        end do
      end do
!
      end subroutine set_difference_of_position
!
!   --------------------------------------------------------------------
!
      end module set_diff_position_z_filter
