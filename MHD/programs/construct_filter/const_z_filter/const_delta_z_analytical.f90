!
!      module  const_delta_z_analytical
!
!     Written by H. Matsui
!
!!      subroutine cal_delta_z_analytical                               &
!!     &         (node, ele, edge, g_FEM, jac_1d)
!
      module  const_delta_z_analytical
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_commute_filter_z
      use m_int_edge_vart_width
!
      use t_geometry_data
      use t_edge_data
!
      implicit none
!
      private :: cal_dz_chebyshev_grids, cal_dz_half_chebyshev_grids
      private :: cal_dz_test_grids, cal_dz_test_grids_2
      private :: cal_dz_liner_grids
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_delta_z_analytical                                 &
     &         (node, ele, edge, g_FEM, jac_1d)
!
      use int_edge_mass_mat_z_filter
      use m_spheric_constants
      use t_fem_gauss_int_coefs
      use t_jacobian_1d
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(edge_data), intent(inout) :: edge
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
!
!
      call int_edge_mass_matrix(node%numnod, ele%numele, edge,          &
     &                          i_int_z_filter, g_FEM, jac_1d)
      call allocate_delta_z(node%numnod, ele%numele)
!
      if (iflag_grid .eq. igrid_Chebyshev) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_chebyshev_grids'
        call cal_dz_chebyshev_grids(node, ele, edge)
      else if (iflag_grid .eq. igrid_half_Chebyshev) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_half_chebyshev_grids'
        call cal_dz_half_chebyshev_grids(node, ele, edge)
      else if (iflag_grid.eq.-1) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_test_grids'
        call cal_dz_test_grids(node, ele)
      else if (iflag_grid.eq.-2) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_test_grids_2'
        call cal_dz_test_grids_2(node, ele, edge)
      else
        if (my_rank.eq.0) write(*,*) 'cal_dz_liner_grids'
        call cal_dz_liner_grids(node, ele)
      end if
!
      end subroutine cal_delta_z_analytical
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_liner_grids(node, ele)
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
!
      integer (kind = kint) :: i, nz
!
!
      nz = node%internal_node
      do i = 1, node%numnod
        delta_z(i) =  zsize / dble(2*nz-1)
        delta_dz(i) = zero
        d2_dz(i) =    zero
      end do
!
      do i = 1, ele%numele
        delta_z_e(i) =  zsize / dble(2*nz-1)
        delta_dz_e(i) = zero
        d2_dz_e(i) =    zero
      end do
!
      end subroutine cal_dz_liner_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_chebyshev_grids(node, ele, edge)
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(edge_data), intent(inout) :: edge
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i, inod1, inod2, nz
!
!
      pi = four * atan(one)
!
      nz = node%internal_node
        do i = 1, node%numnod
          delta_z(i)                                                    &
     &          = ( 0.5d0 * zsize * pi / (two*dble(nz-1)) )             &
     &           * sin (pi* dble(i - 1) / dble(nz-1) )
          if ( i.eq.1 ) then
           delta_dz(i) = 1.0d20
           d2_dz(i) = -1.0d20
          else if ( i .eq. nz ) then
           delta_dz(i) = -1.0d20
           d2_dz(i) = -1.0d20
          else
           delta_dz(i) =   pi / ( two * dble(nz-1)                      &
     &         * tan(pi* dble(i - 1) / dble(nz-1)) )
           d2_dz(i) = - pi / ( zsize * dble(nz-1)                       &
     &         * sin(pi* dble(i - 1) / dble(nz-1))**3 )
          end if
        end do
!
        do i = 1, ele%numele
          inod1 = edge%ie_edge(i,1)
          inod2 = edge%ie_edge(i,2)
          delta_z_e(i)                                                  &
     &          = (0.5d0 * zsize * pi / (two*dble(nz-1)))               &
     &           * sin (pi*(dble(i)-0.5d0) / dble(nz-1) )

          delta_dz_e(i)                                                 &
     &          =  pi / ( two * dble(nz-1)                              &
     &           * tan(pi*(dble(i)-0.5d0) / dble(nz-1)) )
          d2_dz_e(i) =  - pi / ( zsize * dble(nz-1)                     &
     &         * sin(pi*(dble(i)-0.5d0) / dble(nz-1))**3 )
        end do
!
      end subroutine cal_dz_chebyshev_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_half_chebyshev_grids(node, ele, edge)
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(edge_data), intent(inout) :: edge
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i, inod1, inod2, nz
!
      pi = four * atan(one)
!
      nz = node%internal_node
        do i = 1, node%numnod
          node%xx(i,3) = -0.5d0*zsize - zsize                           &
     &         * cos (pi* dble(i - 1) / dble(2*(nz-1)) )
        end do
        do i = 1, node%numnod
          delta_z(i) = ( zsize * pi / two*dble(2*(nz-1)) )              &
     &           * sin (pi* dble(i - 1) / dble(2*(nz-1)) )
          if ( i.eq.1 ) then
           delta_dz(i) = 1.0d20
           d2_dz(i) = -1.0d20
          else
           delta_dz(i) =   pi / ( dble(4*(nz-1))                        &
     &         * tan(pi* dble(i - 1) / dble(2*(nz-1))) )
           d2_dz(i) = - pi / ( zsize * dble(4*(nz-1))                   &
     &         * sin(pi* dble(i - 1) / dble(2*(nz-1)) )**3 )
          end if
        end do
!
        do i = 1, ele%numele
          inod1 = edge%ie_edge(i,1)
          inod2 = edge%ie_edge(i,2)
          delta_z_e(i) =  node%xx(inod2,3) - node%xx(inod1,3)
          delta_dz_e(i) = ( delta_z(inod2) - delta_z(inod1) )           &
     &                   / ( two*delta_z_e(i) )
          d2_dz_e(i) =   ( delta_dz(inod2) - delta_dz(inod1) )          &
     &                   / ( delta_z_e(i) )
        end do
!
      end subroutine cal_dz_half_chebyshev_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_test_grids(node, ele)
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
!
      integer (kind = kint) :: i
!
        do i = 1, node%numnod
          delta_z(i) =  one
          delta_dz(i) = zero
          d2_dz(i) =    zero
        end do
!
        do i = 1, ele%numele
          delta_z_e(i) =  one
          delta_dz_e(i) = zero
          d2_dz_e(i) =    zero
        end do
!
      end subroutine cal_dz_test_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_test_grids_2(node, ele, edge)
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(edge_data), intent(inout) :: edge
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i, inod1, inod2, nz
!
      pi = four * atan(one)
!
      nz = node%internal_node
        do i = 1, node%numnod
          node%xx(i,3) = - dble(nz-1)                                   &
     &         * cos (pi* dble(i - 1) / dble(nz-1) ) 
        end do
        do i = 1, node%numnod
          delta_z(i)                                                    &
     &          = 0.5*pi * sin(pi* dble(i - 1) / dble(nz-1))
          delta_dz(i) =   pi / ( two * dble(nz-1)                       &
     &         * tan(pi* dble(i - 1) / dble(nz-1)) )
          d2_dz(i) = - pi / ( dble(nz-1)**2                             &
     &         * sin(pi* dble(i - 1) / dble(nz-1))**3 )
        end do
!
        do i = 1, ele%numele
          inod1 = edge%ie_edge(i,1)
          inod2 = edge%ie_edge(i,2)
          delta_z_e(i) =  node%xx(inod2,3) - node%xx(inod1,3)
          delta_dz_e(i) = ( delta_z(inod2) - delta_z(inod1) )           &
     &                   / ( two*delta_z_e(i) )
          d2_dz_e(i) =   ( delta_dz(inod2) - delta_dz(inod1) )          &
     &                   / ( delta_z_e(i) )
        end do
!
      end subroutine cal_dz_test_grids_2
!
!  ---------------------------------------------------------------------
!
      end module const_delta_z_analytical