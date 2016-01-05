!int_edge_mass_mat_z_filter.f90
!      module int_edge_mass_mat_z_filter
!
!      Written by H. Matsui
!
!!      subroutine int_edge_mass_matrix                                 &
!!     &         (numnod, numele, edge, n_int, jac_1d)
!
      module int_edge_mass_mat_z_filter
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine int_edge_mass_matrix                                   &
     &         (numnod, numele, edge, n_int, jac_1d)
!
      use t_edge_data
      use t_jacobian_1d
!
      use m_fem_gauss_int_coefs
      use m_int_edge_data
      use m_commute_filter_z
!
      type(edge_data), intent(in) :: edge
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint), intent(in) :: n_int
      integer (kind= kint), intent(in) :: numnod, numele
!
      real(kind = kreal) :: wk
      integer (kind = kint) :: inod1, inod2, iele, k1, k2
      integer (kind = kint) :: i, ix
!
!
      mk_c = 0.0d0
      mk = 0.0d0
!
      do iele = 1, numele
        do i = 1, n_int
         ix = i + int_start1(n_int)
         do k1 = 1, 2
          do k2 = 1, 2
           inod1 = edge%ie_edge(iele,k1)
           inod2 = edge%ie_edge(iele,k2)
           wk = jac_1d%an_edge(k1,ix) * jac_1d%an_edge(k2,ix)           &
     &         * jac_1d%xeg_edge(iele,ix,3) * owe(ix)
           mk_c(inod1,inod2) = mk_c(inod1,inod2) + wk
           mk(inod2) = mk(inod2) + wk
         end do
        end do
       end do
      end do
!
      do inod2 = 1, numnod
        mk(inod2) = one / mk(inod2)
      end do
!
      end subroutine int_edge_mass_matrix
!
!   --------------------------------------------------------------------
!
      end module int_edge_mass_mat_z_filter
