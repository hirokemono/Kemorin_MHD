!int_edge_mass_mat_z_filter.f90
!      module int_edge_mass_mat_z_filter
!
!      Written by H. Matsui
!
!      subroutine int_edge_mass_matrix(n_int)
!
      module int_edge_mass_mat_z_filter
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
      subroutine int_edge_mass_matrix(n_int)
!
      use m_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians_4_edge
      use m_int_edge_data
      use m_commute_filter_z
!
      integer (kind = kint), intent(in) :: n_int
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
           inod1 = ie_edge(iele,k1)
           inod2 = ie_edge(iele,k2)
           wk = jac1_1d_l%an_edge(k1,ix) * jac1_1d_l%an_edge(k2,ix)     &
     &         * jac1_1d_l%xeg_edge(iele,ix,3) * owe(ix)
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
