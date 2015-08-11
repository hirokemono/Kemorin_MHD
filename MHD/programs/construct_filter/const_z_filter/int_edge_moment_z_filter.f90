!int_edge_moment_z_filter.f90
!      module int_edge_moment_z_filter
!
!      Written by H. Matsui
!
!      subroutine int_edge_moment(n_int)
!
      module int_edge_moment_z_filter
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine int_edge_moment(n_int)
!
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_shape_functions
      use m_jacobians_4_edge
      use m_int_edge_data
      use m_int_commtative_filter
      use m_commute_filter_z
!
      integer (kind= kint), intent(in) :: n_int
      integer (kind= kint) :: inod, inod1, inod2, iele, j1, j2
      integer (kind= kint) :: i, kf, ix
      real(kind = kreal), parameter :: half = 0.5d0
!
!
      do iele = 1, numele
       do j1 = 1, 2
        do j2 = 1, 2
         inod1 = ie_edge(iele,j1)
         inod2 = ie_edge(iele,j2)
!
         do kf = 0, 2
          do i = 1, n_int
           ix = i + int_start1(n_int)
           xmom_dt(inod2,kf) = xmom_dt(inod2,kf)                        &
     &                        + xmom_int_t(inod1,kf) * owe(ix)          &
     &                         * dnxi_ed1(j1,ix)                        &
     &                         * jac1_1d_l%an_edge(j2,ix)
           xmom_dot(inod2,kf) = xmom_dot(inod2,kf)                      &
     &                        + xmom_int_to(inod1,kf) * owe(ix)         &
     &                         * dnxi_ed1(j1,ix)                        &
     &                         * jac1_1d_l%an_edge(j2,ix)
          end do
         end do
        end do
       end do
      end do
!
      do inod = 1, node1%numnod
       do kf = 0, 2
           xmom_dt(inod,kf)  = xmom_dt(inod,kf)  * mk(inod)
           xmom_dot(inod,kf) = xmom_dot(inod,kf) * mk(inod)
       end do
      end do
!
      end subroutine int_edge_moment
!
!   --------------------------------------------------------------------
!
      end module int_edge_moment_z_filter
