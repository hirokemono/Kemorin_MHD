!>@file   int_edge_mass_mat_z_filter.f90
!!        module int_edge_mass_mat_z_filter
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief Mass matrices for vertical grid spacing
!!
!!@verbatim
!!      subroutine set_spatial_difference(numele, n_int, g_FEM,         &
!!     &                                  jac_1d, dz_ele)
!!        integer (kind = kint), intent(in) :: numele, n_int
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_1d), intent(in) :: jac_1d
!!        real(kind = kreal), intent(inout) :: dz_ele(numele)
!!      subroutine int_edge_mass_matrix(numnod, numele, edge, n_int,    &
!!     &                                g_FEM, jac_1d, mk, mk_mat)
!!        type(edge_data), intent(in) :: edge
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_1d), intent(in) :: jac_1d
!!        integer (kind = kint), intent(in) :: n_int
!!        integer (kind= kint), intent(in) :: numnod, numele
!!        real(kind = kreal), intent(inout) :: mk(numnod)
!!        real(kind = kreal), intent(inout) :: mk_mat(numnod,numnod)
!!@endverbatim
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
      subroutine set_spatial_difference(numele, n_int, g_FEM,           &
     &                                  jac_1d, dz_ele)
!
      use m_commute_filter_z
!
      use t_fem_gauss_int_coefs
      use t_jacobian_1d
!
      integer (kind = kint), intent(in) :: numele, n_int
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
!
      real(kind = kreal), intent(inout) :: dz_ele(numele)
!
      integer (kind = kint) :: iele, k, ix
!
!
       do k = 1, n_int
         ix = k + g_FEM%int_start1(n_int)
         do iele = 1, numele
           dz_ele(iele) = dz_ele(iele)                                  &
     &                 + jac_1d%xeg_edge(iele,ix,3) * g_FEM%owe(ix)
         end do
       end do
       do iele = 1, numele
         dz_ele(iele) = half * dz_ele(iele)
       end do
!
      end subroutine set_spatial_difference
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine int_edge_mass_matrix(numnod, numele, edge, n_int,      &
     &                                g_FEM, jac_1d, mk, mk_mat)
!
      use t_edge_data
      use t_fem_gauss_int_coefs
      use t_jacobian_1d
!
      use m_commute_filter_z
!
      type(edge_data), intent(in) :: edge
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint), intent(in) :: n_int
      integer (kind= kint), intent(in) :: numnod, numele
!
      real(kind = kreal), intent(inout) :: mk(numnod)
      real(kind = kreal), intent(inout) :: mk_mat(numnod,numnod)
!
      real(kind = kreal) :: wk
      integer (kind = kint) :: inod1, inod2, iele, k1, k2
      integer (kind = kint) :: i, ix
!
!
      mk_mat = 0.0d0
      mk =     0.0d0
!
      do iele = 1, numele
        do i = 1, n_int
         ix = i + g_FEM%int_start1(n_int)
         do k1 = 1, 2
          do k2 = 1, 2
           inod1 = edge%ie_edge(iele,k1)
           inod2 = edge%ie_edge(iele,k2)
           wk = jac_1d%an_edge(k1,ix) * jac_1d%an_edge(k2,ix)           &
     &         * jac_1d%xeg_edge(iele,ix,3) * g_FEM%owe(ix)
           mk_mat(inod1,inod2) = mk_mat(inod1,inod2) + wk
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
