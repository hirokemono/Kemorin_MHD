!t_cube_position.f90
!     module t_cube_position
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
! ***** allocate position of node at z-component
!
!!      subroutine set_position_4_vartical(elm_type, c_size, c_vert)
!!      subroutine dealloc_vertical_4_cube(c_vert)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(vertical_position_cube), intent(inout) :: c_vert
!
      module t_cube_position
!
      use m_precision
      use m_constants
!
      implicit none
!
      type vertical_position_cube
        integer(kind = kint) :: nnod
        real(kind = kreal), allocatable :: zz(:)
!
        integer(kind = kint) :: nedge
        real(kind = kreal), allocatable :: zz_edge(:)
      end type vertical_position_cube
!
      private :: alloc_vertical_node, alloc_vertical_edge
      private :: dealloc_vertical_node, dealloc_vertical_edge
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_position_4_vartical(elm_type, c_size, c_vert)
!
      use t_size_of_cube
      use m_spheric_constants
!
      integer(kind = kint), intent(in)  ::  elm_type
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(inout) :: c_vert
!
      real(kind = kreal) :: pi
      integer(kind=kint )  ::  k
!
!
      pi = four * atan(one)
!
      call alloc_vertical_node(c_size%nz_all, c_vert)
!
      if (iradi .eq. igrid_equidistance) then
       do k = 1, c_size%nz_all
         c_vert%zz(k) = c_size%zmin                                     &
     &          + c_size%zsize * dble(k-1) / dble(c_size%nz_all - 1)
       end do
      else if (iradi .eq. igrid_half_Chebyshev) then
       do k = 1, c_size%nz_all
         c_vert%zz(k) = c_size%zmax - c_size%zsize                      &
     &          * cos(pi*dble(k-1) / dble(2*c_size%nz_all-2))
       end do
      else if (iradi .eq. igrid_Chebyshev) then
       do k = 1, c_size%nz_all
         c_vert%zz(k)                                                   &
     &         = half * (c_size%zmax + c_size%zmin- c_size%zsize        &
     &          * cos(pi * dble(k-1) / dble(c_size%nz_all - 1)))
       end do
      end if
!
!
      if(elm_type.eq.332) then
        call alloc_vertical_edge(c_vert)
!
        if (iradi .eq. igrid_equidistance) then
          do k = 1, c_size%nz_all-1
            c_vert%zz_edge(k) = (c_vert%zz(k) + c_vert%zz(k+1)) / 2.0d0
          end do
        else if (iradi .eq. igrid_half_Chebyshev) then
          do k = 1, c_size%nz_all-1
            c_vert%zz_edge(k) = c_size%zmax - c_size%zsize              &
     &               * cos ( pi*dble(2*k-1)/dble(4*(c_size%nz_all-1)) )
          end do
        else if (iradi .eq. igrid_Chebyshev) then
          do k = 1, c_size%nz_all-1
            c_vert%zz_edge(k) = 0.5d0 * ( c_size%zmax + c_size%zmin     &
     &          - c_size%zsize                                          &
     &            * cos(pi * dble(2*k-1) / dble(2*c_size%nz_all-2)))
        end do
       end if
!
      end if
!
      end subroutine  set_position_4_vartical
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_vertical_4_cube(c_vert)
!
      call dealloc_vertical_node(c_vert)
      if(elm_type.eq.332) call dealloc_vertical_edge(c_vert)
!
      end subroutine dealloc_vertical_4_cube
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_vertical_node(nz_all, c_vert)
!
      integer(kind = kint), intent(in) :: nz_all
      type(vertical_position_cube), intent(inout) :: c_vert
!
      c_vert%nnod = nz_all
      allocate(c_vert%zz(c_vert%nnod))
      if(c_vert%nnod .gt. 0) c_vert%zz = 0.0d0
!
      end subroutine alloc_vertical_node
!
! ----------------------------------------------------------------------
!
      subroutine alloc_vertical_edge(c_vert)
!
      type(vertical_position_cube), intent(inout) :: c_vert
!
      c_vert%nedge = c_vert%nnod - 1
      allocate(c_vert%zz_edge(c_vert%nedge))
      if(c_vert%nedge .gt. 0) c_vert%zz_edge = 0.0d0
!
      end subroutine alloc_vertical_edge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_vertical_node(c_vert)
!
      type(vertical_position_cube), intent(inout) :: c_vert
!
      deallocate(c_vert%zz)
!
      end subroutine dealloc_vertical_node
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_vertical_edge(c_vert)
!
      type(vertical_position_cube), intent(inout) :: c_vert
!
      deallocate(c_vert%zz_edge)
!
      end subroutine dealloc_vertical_edge
!
! ----------------------------------------------------------------------
!
      end module t_cube_position
