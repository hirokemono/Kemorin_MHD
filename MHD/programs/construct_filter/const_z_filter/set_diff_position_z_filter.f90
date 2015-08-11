!set_diff_position_z_filter.f90
!      module set_diff_position_z_filter
!
      module set_diff_position_z_filter
!
!      Written by H. Matsui
!
      use m_precision
!
      implicit none
!
!      subroutine set_difference_of_position
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_difference_of_position
!
      use m_geometry_data
      use m_commute_filter_z
      use m_neibor_data_z
!
      real(kind = kreal) :: dz0, dzeta
      integer (kind = kint) :: i, j, inod1, inod2, jnod1, jnod2, jele
!
!
        write(*,*) size(alpha,1), size(alpha,2), size(alpha,3)
!
        do i = 1, totalele
          inod1 = ie_edge(i,1)
          inod2 = ie_edge(i,2)
          jnod1 = ie_edge(i,1)
          jnod2 = ie_edge(i,2)
          dz0 =   xx(inod2,3) - xx(inod1,3)
          dzeta = xx(jnod2,3) - xx(jnod1,3)
          alpha(i,0,1) = dz0/dzeta - 1
          alpha(i,0,2) = alpha(i,1,1)
!
          do j = 1, nneib_ele(i,1)
            jele = ineib_ele(i,j,1)
            jnod1 = ie_edge(jele,1)
            jnod2 = ie_edge(jele,2)
            dzeta = xx(jnod2,3) - xx(jnod1,3)
            alpha(i,j,1) = dz0/dzeta - 1
          end do
!
          do j = 1, nneib_ele(i,2)
            jele = ineib_ele(i,j,2)
            jnod1 = ie_edge(jele,1)
            jnod2 = ie_edge(jele,2)
            dzeta = xx(jnod2,3) - xx(jnod1,3)
            alpha(i,j,2) = dz0/dzeta - 1
          end do
        end do
!
      end subroutine set_difference_of_position
!
!   --------------------------------------------------------------------
!
      end module set_diff_position_z_filter
