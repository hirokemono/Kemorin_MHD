!set_vertical_position_cube.f90
!     module set_vertical_position_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
! ***** allocate position of node at z-component
!
!!      subroutine  set_position_4_vartical(elm_type, c_size)
!!        type(size_of_cube), intent(in) :: c_size
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
      subroutine  set_position_4_vartical(elm_type, c_size)
!
      use t_size_of_cube
      use m_size_4_plane
      use m_cube_position
      use m_spheric_constants
!
      integer(kind = kint), intent(in)  ::  elm_type
      type(size_of_cube), intent(in) :: c_size
!
      integer(kind=kint )  ::  k
!
      allocate ( zz(c_size%nz_all) )
      zz = 0.0d0
!
      if (iradi .eq. igrid_equidistance) then
       do k = 1, c_size%nz_all
         zz(k) = c_size%zmin                                            &
     &          + c_size%zsize * dble(k-1) / dble(c_size%nz_all - 1)
       end do
      else if (iradi .eq. igrid_half_Chebyshev) then
       do k = 1, c_size%nz_all
         zz(k) = c_size%zmax - c_size%zsize                             &
     &          * cos(pi*dble(k-1) / dble(2*c_size%nz_all-2))
       end do
      else if (iradi .eq. igrid_Chebyshev) then
       do k = 1, c_size%nz_all
         zz(k) = 0.5d0 * (c_size%zmax + c_size%zmin- c_size%zsize       &
     &          * cos(pi * dble(k-1) / dble(c_size%nz_all - 1)))
       end do
      end if
!
      if (elm_type.eq.332) then
!
       allocate ( zz_edge(c_size%nz_all - 1) )
       zz_edge = 0.0d0
!
       if (iradi .eq. igrid_equidistance) then
        do k = 1, c_size%nz_all-1
         zz_edge(k) = ( zz(k) + zz(k+1) ) / 2.0d0
        end do
       else if (iradi .eq. igrid_half_Chebyshev) then
        do k = 1, c_size%nz_all-1
         zz_edge(k) = c_size%zmax - c_size%zsize                        &
     &               * cos ( pi*dble(2*k-1)/dble(4*(c_size%nz_all-1)) )
        end do
       else if (iradi .eq. igrid_Chebyshev) then
        do k = 1, c_size%nz_all-1
         zz_edge(k) = 0.5d0 * ( c_size%zmax + c_size%zmin               &
     &      - c_size%zsize                                              &
     &       * cos(pi * dble(2*k-1) / dble(2*c_size%nz_all-2)))
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
