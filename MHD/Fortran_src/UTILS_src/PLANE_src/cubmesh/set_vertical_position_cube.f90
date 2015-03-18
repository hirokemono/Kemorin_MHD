!set_vertical_position_cube.f90
!     module set_vertical_position_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
! ***** allocate position of node at z-component
!
!      subroutine  set_position_4_vartical(elm_type)
!
      module set_vertical_position_cube
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine  set_position_4_vartical(elm_type)
!
      use m_size_of_cube
      use m_size_4_plane
      use m_cube_position
      use m_spheric_constants
!
      integer(kind=kint )  ::  elm_type
!
      integer(kind=kint )  ::  k
!
      allocate ( zz(nz_all) )
      zz = 0.0d0
!
      if (iradi .eq. igrid_euqidistance) then
       do k = 1, nz_all
         zz(k) = zmin + zsize * dble(k-1) / dble(nz_all-1)
       end do
      else if (iradi .eq. igrid_half_Chebyshev) then
       do k = 1, nz_all
         zz(k) = zmax - zsize * cos ( pi*dble(k-1)/dble(2*nz_all-2) )
       end do
      else if (iradi .eq. igrid_Chebyshev) then
       do k = 1, nz_all
         zz(k) = 0.5d0 * ( zmax + zmin                                  &
     &          - zsize * cos ( pi * dble(k-1) / dble(nz_all-1) ) )
       end do
      end if
!
      if (elm_type.eq.332) then
!
       allocate ( zz_edge(nz_all-1) )
       zz_edge = 0.0d0
!
       if (iradi .eq. igrid_euqidistance) then
        do k = 1, nz_all-1
         zz_edge(k) = ( zz(k) + zz(k+1) ) / 2.0d0
        end do
       else if (iradi .eq. igrid_half_Chebyshev) then
        do k = 1, nz_all-1
         zz_edge(k) = zmax - zsize                                      &
     &               * cos ( pi*dble(2*k-1)/dble(4*(nz_all-1)) )
        end do
       else if (iradi .eq. igrid_Chebyshev) then
        do k = 1, nz_all-1
         zz_edge(k) = 0.5d0 * ( zmax + zmin                             &
     &      - zsize * cos ( pi * dble(2*k-1) / dble(2*nz_all-2) ) )
        end do
       end if
!
      end if
!
      end subroutine  set_position_4_vartical
!
! ----------------------------------------------------------------------
!
      end module set_vertical_position_cube
