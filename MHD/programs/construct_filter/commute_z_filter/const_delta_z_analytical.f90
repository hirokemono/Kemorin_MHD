!
!      module  const_delta_z_analytical
!
!     Written by H. Matsui
!
!>@file   const_delta_z_analytical.f90
!!        module const_delta_z_analytical
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief Construct grid spacing data for plane layer model
!!
!!@verbatim
!!      subroutine cal_delta_z_analytical(ele, edge, node, dz_plane)
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(node_data), intent(inout) :: node
!!        type(edge_z_width), intent(inout) :: dz_plane
!!@endverbatim
!
      module  const_delta_z_analytical
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_commute_filter_z
!
      use t_geometry_data
      use t_edge_data
      use t_vart_edge_width
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
      subroutine cal_delta_z_analytical(ele, edge, node, dz_plane)
!
      use m_spheric_constants
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
!
      type(node_data), intent(inout) :: node
      type(edge_z_width), intent(inout) :: dz_plane
!
!
      call alloc_edge_vart_width(node%numnod, ele%numele, dz_plane)
!
      if (iflag_grid .eq. igrid_Chebyshev) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_chebyshev_grids'
        call cal_dz_chebyshev_grids(ele, edge, node, dz_plane)
      else if (iflag_grid .eq. igrid_half_Chebyshev) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_half_chebyshev_grids'
        call cal_dz_half_chebyshev_grids(ele, edge, node, dz_plane)
      else if (iflag_grid.eq.-1) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_test_grids'
        call cal_dz_test_grids(ele, node, dz_plane)
      else if (iflag_grid.eq.-2) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_test_grids_2'
        call cal_dz_test_grids_2(ele, edge, node, dz_plane)
      else
        if (my_rank.eq.0) write(*,*) 'cal_dz_liner_grids'
        call cal_dz_liner_grids(ele, node, dz_plane)
      end if
!
      end subroutine cal_delta_z_analytical
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_liner_grids(ele, node, dz_plane)
!
      type(element_data), intent(in) :: ele
      type(node_data), intent(inout) :: node
      type(edge_z_width), intent(inout) :: dz_plane
!
      integer (kind = kint) :: i, nz
!
!
      nz = node%internal_node
      do i = 1, node%numnod
        dz_plane%delta_z_n(i) =  zsize / dble(2*nz-1)
        dz_plane%delta_dz_n(i) = zero
        dz_plane%d2_dz_n(i) =    zero
      end do
!
      do i = 1, ele%numele
        dz_plane%delta_z_e(i) =  zsize / dble(2*nz-1)
        dz_plane%delta_dz_e(i) = zero
        dz_plane%d2_dz_e(i) =    zero
      end do
!
      end subroutine cal_dz_liner_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_chebyshev_grids(ele, edge, node, dz_plane)
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
!
      type(node_data), intent(inout) :: node
      type(edge_z_width), intent(inout) :: dz_plane
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i, inod1, inod2, nz
!
!
      pi = four * atan(one)
!
      nz = node%internal_node
        do i = 1, node%numnod
          dz_plane%delta_z_n(i)                                         &
     &          = ( 0.5d0 * zsize * pi / (two*dble(nz-1)) )             &
     &           * sin (pi* dble(i - 1) / dble(nz-1) )
          if ( i.eq.1 ) then
           dz_plane%delta_dz_n(i) = 1.0d20
           dz_plane%d2_dz_n(i) = -1.0d20
          else if ( i .eq. nz ) then
           dz_plane%delta_dz_n(i) = -1.0d20
           dz_plane%d2_dz_n(i) = -1.0d20
          else
           dz_plane%delta_dz_n(i) =   pi / ( two * dble(nz-1)           &
     &                         * tan(pi* dble(i - 1) / dble(nz-1)) )
           dz_plane%d2_dz_n(i) = - pi / ( zsize * dble(nz-1)            &
     &                        * sin(pi* dble(i - 1) / dble(nz-1))**3 )
          end if
        end do
!
        do i = 1, ele%numele
          inod1 = edge%ie_edge(i,1)
          inod2 = edge%ie_edge(i,2)
          dz_plane%delta_z_e(i)                                         &
     &          = (0.5d0 * zsize * pi / (two*dble(nz-1)))               &
     &           * sin (pi*(dble(i)-0.5d0) / dble(nz-1) )

          dz_plane%delta_dz_e(i)                                        &
     &          =  pi / ( two * dble(nz-1)                              &
     &           * tan(pi*(dble(i)-0.5d0) / dble(nz-1)) )
          dz_plane%d2_dz_e(i) =  - pi / ( zsize * dble(nz-1)            &
     &         * sin(pi*(dble(i)-0.5d0) / dble(nz-1))**3 )
        end do
!
      end subroutine cal_dz_chebyshev_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_half_chebyshev_grids(ele, edge, node, dz_plane)
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
!
      type(node_data), intent(inout) :: node
      type(edge_z_width), intent(inout) :: dz_plane
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
          dz_plane%delta_z_n(i) = ( zsize * pi / two*dble(2*(nz-1)) )   &
     &           * sin (pi* dble(i - 1) / dble(2*(nz-1)) )
          if ( i.eq.1 ) then
            dz_plane%delta_dz_n(i) = 1.0d20
            dz_plane%d2_dz_n(i) = -1.0d20
          else
            dz_plane%delta_dz_n(i) =   pi / ( dble(4*(nz-1))            &
     &         * tan(pi* dble(i - 1) / dble(2*(nz-1))) )
            dz_plane%d2_dz_n(i) = - pi / ( zsize * dble(4*(nz-1))       &
     &         * sin(pi* dble(i - 1) / dble(2*(nz-1)) )**3 )
          end if
        end do
!
        do i = 1, ele%numele
          inod1 = edge%ie_edge(i,1)
          inod2 = edge%ie_edge(i,2)
          dz_plane%delta_z_e(i) =  node%xx(inod2,3) - node%xx(inod1,3)
          dz_plane%delta_dz_e(i)                                        &
     &        = (dz_plane%delta_z_n(inod2) - dz_plane%delta_z_n(inod1)) &
     &         / (two * dz_plane%delta_z_e(i))
          dz_plane%d2_dz_e(i)                                           &
     &      = (dz_plane%delta_dz_n(inod2) - dz_plane%delta_dz_n(inod1)) &
     &         / dz_plane%delta_z_e(i)
        end do
!
      end subroutine cal_dz_half_chebyshev_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_test_grids(ele, node, dz_plane)
!
      type(element_data), intent(in) :: ele
!
      type(node_data), intent(inout) :: node
      type(edge_z_width), intent(inout) :: dz_plane
!
      integer (kind = kint) :: i
!
        do i = 1, node%numnod
          dz_plane%delta_z_n(i) =  one
          dz_plane%delta_dz_n(i) = zero
          dz_plane%d2_dz_n(i) =    zero
        end do
!
        do i = 1, ele%numele
          dz_plane%delta_z_e(i) =  one
          dz_plane%delta_dz_e(i) = zero
          dz_plane%d2_dz_e(i) =    zero
        end do
!
      end subroutine cal_dz_test_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_test_grids_2(ele, edge, node, dz_plane)
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
!
      type(node_data), intent(inout) :: node
      type(edge_z_width), intent(inout) :: dz_plane
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
          dz_plane%delta_z_n(i)                                         &
     &          = 0.5*pi * sin(pi* dble(i - 1) / dble(nz-1))
          dz_plane%delta_dz_n(i) =   pi / ( two * dble(nz-1)            &
     &         * tan(pi* dble(i - 1) / dble(nz-1)) )
          dz_plane%d2_dz_n(i) = - pi / ( dble(nz-1)**2                  &
     &         * sin(pi* dble(i - 1) / dble(nz-1))**3 )
        end do
!
        do i = 1, ele%numele
          inod1 = edge%ie_edge(i,1)
          inod2 = edge%ie_edge(i,2)
          dz_plane%delta_z_e(i) =  node%xx(inod2,3) - node%xx(inod1,3)
          dz_plane%delta_dz_e(i)                                        &
     &        = (dz_plane%delta_z_n(inod2) - dz_plane%delta_z_n(inod1)) &
     &         / (two * dz_plane%delta_z_e(i))
          dz_plane%d2_dz_e(i)                                           &
     &      = (dz_plane%delta_dz_n(inod2) - dz_plane%delta_dz_n(inod1)) &
     &       / (dz_plane%delta_z_e(i))
        end do
!
      end subroutine cal_dz_test_grids_2
!
!  ---------------------------------------------------------------------
!
      end module const_delta_z_analytical
