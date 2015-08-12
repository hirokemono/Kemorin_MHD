!
!      module  const_delta_z_analytical
!
!     Written by H. Matsui
!
!      subroutine cal_delta_z_analytical
!
      module  const_delta_z_analytical
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use m_commute_filter_z
      use m_geometry_data
      use m_int_edge_vart_width
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
      subroutine cal_delta_z_analytical
!
      use int_edge_mass_mat_z_filter
      use m_spheric_constants
!
!
      call int_edge_mass_matrix(i_int_z_filter)
      call allocate_delta_z
!
      if (iflag_grid .eq. igrid_Chebyshev) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_chebyshev_grids'
        call cal_dz_chebyshev_grids
      else if (iflag_grid .eq. igrid_half_Chebyshev) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_half_chebyshev_grids'
        call cal_dz_half_chebyshev_grids
      else if (iflag_grid.eq.-1) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_test_grids'
        call cal_dz_test_grids
      else if (iflag_grid.eq.-2) then
        if (my_rank.eq.0) write(*,*) 'cal_dz_test_grids_2'
        call cal_dz_test_grids_2
      else
        if (my_rank.eq.0) write(*,*) 'cal_dz_liner_grids'
        call cal_dz_liner_grids
      end if
!
      end subroutine cal_delta_z_analytical
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_liner_grids
!
      integer (kind = kint) :: i
!
!
        do i = 1, node1%numnod
          delta_z(i) =  zsize / dble(2*internal_node-1)
          delta_dz(i) = zero
          d2_dz(i) =    zero
        end do
!
        do i = 1, ele1%numele
          delta_z_e(i) =  zsize / dble(2*internal_node-1)
          delta_dz_e(i) = zero
          d2_dz_e(i) =    zero
        end do
!
      end subroutine cal_dz_liner_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_chebyshev_grids
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i, inod1, inod2
!
!
      pi = four * atan(one)
!
        do i = 1, node1%numnod
          delta_z(i)                                                    &
     &          = ( 0.5d0 * zsize * pi / (two*dble(internal_node-1)) )  &
     &           * sin (pi* dble(i - 1) / dble(internal_node-1) )
          if ( i.eq.1 ) then
           delta_dz(i) = 1.0d20
           d2_dz(i) = -1.0d20
          else if ( i .eq. internal_node ) then
           delta_dz(i) = -1.0d20
           d2_dz(i) = -1.0d20
          else
           delta_dz(i) =   pi / ( two * dble(internal_node-1)           &
     &         * tan(pi* dble(i - 1) / dble(internal_node-1)) )
           d2_dz(i) = - pi / ( zsize * dble(internal_node-1)            &
     &         * sin(pi* dble(i - 1) / dble(internal_node-1))**3 )
          end if
        end do
!
        do i = 1, ele1%numele
          inod1 = ie_edge(i,1)
          inod2 = ie_edge(i,2)
          delta_z_e(i)                                                  &
     &          = (0.5d0 * zsize * pi / (two*dble(internal_node-1)))    &
     &           * sin (pi*(dble(i)-0.5d0) / dble(internal_node-1) )

          delta_dz_e(i)                                                 &
     &          =  pi / ( two * dble(internal_node-1)                   &
     &           * tan(pi*(dble(i)-0.5d0) / dble(internal_node-1)) )
          d2_dz_e(i) =  - pi / ( zsize * dble(internal_node-1)          &
     &         * sin(pi*(dble(i)-0.5d0) / dble(internal_node-1))**3 )
        end do
!
      end subroutine cal_dz_chebyshev_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_half_chebyshev_grids
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i, inod1, inod2
!
      pi = four * atan(one)
!
        do i = 1, node1%numnod
          xx(i,3) = -0.5d0*zsize - zsize                                &
     &         * cos (pi* dble(i - 1) / dble(2*(internal_node-1)) )
        end do
        do i = 1, node1%numnod
          delta_z(i) = ( zsize * pi / two*dble(2*(internal_node-1)) )   &
     &           * sin (pi* dble(i - 1) / dble(2*(internal_node-1)) )
          if ( i.eq.1 ) then
           delta_dz(i) = 1.0d20
           d2_dz(i) = -1.0d20
          else
           delta_dz(i) =   pi / ( dble(4*(internal_node-1))             &
     &         * tan(pi* dble(i - 1) / dble(2*(internal_node-1))) )
           d2_dz(i) = - pi / ( zsize * dble(4*(internal_node-1))        &
     &         * sin(pi* dble(i - 1) / dble(2*(internal_node-1)) )**3 )
          end if
        end do
!
        do i = 1, ele1%numele
          inod1 = ie_edge(i,1)
          inod2 = ie_edge(i,2)
          delta_z_e(i) =  xx(inod2,3) - xx(inod1,3)
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
      subroutine cal_dz_test_grids
!
      integer (kind = kint) :: i
!
        do i = 1, node1%numnod
          delta_z(i) =  one
          delta_dz(i) = zero
          d2_dz(i) =    zero
        end do
!
        do i = 1, ele1%numele
          delta_z_e(i) =  one
          delta_dz_e(i) = zero
          d2_dz_e(i) =    zero
        end do
!
      end subroutine cal_dz_test_grids
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dz_test_grids_2
!
!
      real (kind = kreal) :: pi
      integer (kind = kint) :: i, inod1, inod2
!
      pi = four * atan(one)
!
        do i = 1, node1%numnod
          xx(i,3) = - dble(internal_node-1)                             &
     &         * cos (pi* dble(i - 1) / dble(internal_node-1) ) 
        end do
        do i = 1, node1%numnod
          delta_z(i)                                                    &
     &          = 0.5*pi * sin(pi* dble(i - 1) / dble(internal_node-1))
          delta_dz(i) =   pi / ( two * dble(internal_node-1)            &
     &         * tan(pi* dble(i - 1) / dble(internal_node-1)) )
          d2_dz(i) = - pi / ( dble(internal_node-1)**2                  &
     &         * sin(pi* dble(i - 1) / dble(internal_node-1))**3 )
        end do
!
        do i = 1, ele1%numele
          inod1 = ie_edge(i,1)
          inod2 = ie_edge(i,2)
          delta_z_e(i) =  xx(inod2,3) - xx(inod1,3)
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