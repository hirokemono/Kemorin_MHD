!>@file   t_z_int_edge_data.f90
!!        module t_z_int_edge_data
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief mass matrix for vertical filter construction
!!
!!@verbatim
!!      subroutine init_int_z_edge_data(node, ele, edge, n_int,         &
!!     &                                g_FEM, jac_1d, z_int_edge)
!!      subroutine dealloc_z_int_edge_data(z_int_edge)
!!        integer(kind = kint), intent(in) :: n_int
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_1d), intent(in) :: jac_1d
!!        type(z_int_edge_data), intent(inout) :: z_int_edge
!!@endverbatim
!!
      module t_z_int_edge_data
!
      use m_precision
!
      implicit none
!
!
      type z_int_edge_data
        real(kind = kreal), allocatable :: dz_ele(:)
        real(kind = kreal), allocatable :: mk_z(:)
        real(kind = kreal), allocatable :: mk_ele(:,:)
      end type z_int_edge_data
!
      private :: alloc_z_int_edge_data
      private :: set_spatial_difference, int_edge_mass_matrix
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_int_z_edge_data(node, ele, edge, n_int,           &
     &                              g_FEM, jac_1d, z_int_edge)
!
      use t_geometry_data
      use t_edge_data
      use t_fem_gauss_int_coefs
      use t_jacobian_1d
!
      integer(kind = kint), intent(in) :: n_int
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
!
      type(z_int_edge_data), intent(inout) :: z_int_edge
!
!
      call alloc_z_int_edge_data(node%numnod, ele%numele, z_int_edge)
      call set_spatial_difference(ele%numele, n_int, g_FEM, jac_1d,     &
     &                            z_int_edge%dz_ele)
      call int_edge_mass_matrix(node%numnod, ele%numele, edge,          &
     &                          n_int, g_FEM, jac_1d,                   &
     &                          z_int_edge%mk_z, z_int_edge%mk_ele)
!
      end subroutine init_int_z_edge_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_z_int_edge_data(z_int_edge)
!
      type(z_int_edge_data), intent(inout) :: z_int_edge
!
!
      deallocate(z_int_edge%dz_ele)
      deallocate(z_int_edge%mk_z, z_int_edge%mk_ele)
!
      end subroutine dealloc_z_int_edge_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_z_int_edge_data(numnod, numele, z_int_edge)
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(z_int_edge_data), intent(inout) :: z_int_edge
!
!
      allocate(z_int_edge%dz_ele(numele))
      allocate(z_int_edge%mk_z(numnod))
      allocate(z_int_edge%mk_ele(numnod,numnod))
!
      if(numele .gt. 0) z_int_edge%dz_ele(1:numele) = 0.0d0
      if(numnod .gt. 0) z_int_edge%mk_z(1:numnod) =   0.0d0
      if(numnod .gt. 0) z_int_edge%mk_ele(1:numnod,1:numnod) = 0.0d0
!
      end subroutine alloc_z_int_edge_data
!
! -----------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_spatial_difference(numele, n_int, g_FEM, jac_1d,   &
     &                                  dz_ele)
!
      use m_commute_filter_z
!
      use t_fem_gauss_int_coefs
      use t_jacobian_1d
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: n_int
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
       dz_ele(1:numele) = half * dz_ele(1:numele)
!
      end subroutine set_spatial_difference
!
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
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: n_int
      type(edge_data), intent(in) :: edge
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
!
      real(kind = kreal), intent(inout) :: mk(numnod)
      real(kind = kreal), intent(inout) :: mk_mat(numnod,numnod)
!
      real(kind = kreal) :: wk
      integer (kind = kint) :: inod1, inod2, iele, k1, k2
      integer (kind = kint) :: i, ix
!
!
      do iele = 1, numele
        do i = 1, n_int
          ix = i + g_FEM%int_start1(n_int)
          do k1 = 1, 2
            do k2 = 1, 2
              inod1 = edge%ie_edge(iele,k1)
              inod2 = edge%ie_edge(iele,k2)
              wk = jac_1d%an_edge(k1,ix) * jac_1d%an_edge(k2,ix)        &
     &            * jac_1d%xeg_edge(iele,ix,3) * g_FEM%owe(ix)
              mk_mat(inod1,inod2) = mk_mat(inod1,inod2) + wk
              mk(inod2) = mk(inod2) + wk
            end do
          end do
        end do
      end do
!
!$omp parallel workshare
      mk(1:numnod) = one / mk(1:numnod)
!$omp end parallel workshare
!
      end subroutine int_edge_mass_matrix
!
!   --------------------------------------------------------------------
!
      end module t_z_int_edge_data
