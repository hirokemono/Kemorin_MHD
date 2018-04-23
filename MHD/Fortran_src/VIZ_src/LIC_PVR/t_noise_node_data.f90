!>@file   t_geometry_data.f90
!!@brief  module t_geometry_data
!!
!!@author  Yangguang Liao
!!@date Programmed in 2018
!
!>@brief structure of geometry data for FEM mesh
!!  including node and element position, connectivities
!!
!!@verbatim
!!      recursive subroutine alloc_noise_node(n_node, dim, level)
!!      recursive subroutine dealloc_noise_node(n_node)

!!@endverbatim
!
      module t_noise_node_data
!
      use m_precision
!
      implicit  none
!
!
!>  structure for noise node data (position)
      type noise_node
!>        node value (current level noise value)
        real( kind=kreal )  ::  n_value
!>        dimesion size of sub node, eg(2, total sub node 2*2*2 = 8)
        integer( kind=kint )  ::  node_dim
!>        node hierarchical level
        integer( kind=kint )  ::  node_level
!>        subnode list for next level noise node
        type( noise_node ), dimension(:), pointer ::  sub_node
!
      end type noise_node

!>  structure for noise mask
      type noise_mask
!>        lower boundary of reference data
        real( kind=kreal )  ::  range_min
!>        upper boundary of reference data
        real( kind=kreal )  ::  range_max
!>        Reference data to define mask
        real(kind = kreal), allocatable :: ref_data(:)
!
      end type noise_mask
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      recursive subroutine alloc_noise_node(n_node, dim, level)
      !
        type(noise_node), intent(inout) :: n_node
        integer(kind=kint), intent(in) :: level, dim
        integer(kind=kint) :: i, size
        real(kind=kreal) :: rand_v

        n_node%n_value = 0.0

        n_node%node_dim = dim
        n_node%node_level = level
        if(level .gt. 0) then
          size = dim*dim*dim
          allocate(n_node%sub_node(size))
          do i = 1, size
            call alloc_noise_node(n_node%sub_node(i), dim, level-1)
          end do
        end if

      end subroutine alloc_noise_node
!
!  ---------------------------------------------------------------------
!
      recursive subroutine dealloc_noise_node(n_node)
!
        type(noise_node), intent(inout) :: n_node
        integer(kind=kint) :: i, size, dim

        if(n_node%node_level .gt. 0) then
          dim = n_node%node_dim
          size = dim*dim*dim
          do i = 1, size
            call dealloc_noise_node(n_node%sub_node(i))
          end do
          deallocate(n_node%sub_node)
        end if

      end subroutine dealloc_noise_node
!
!  ---------------------------------------------------------------------
!
      subroutine init_noise_mask(n_mask, min, max, ref_data, num_ref)
      !
        use t_geometries_in_pvr_screen
      !
        type(noise_mask), intent(inout) :: n_mask
        integer(kind=kint) :: num_ref
        real(kind=kreal), intent(in) :: min, max
        real(kind=kreal), intent(in) :: ref_data(num_ref)
        !type(pvr_projected_field), intent(in) :: field_pvr

        n_mask%range_min = min
        n_mask%range_max = max
        allocate(n_mask%ref_data(num_ref))
        n_mask%ref_data(:) = ref_data(:)

      end subroutine init_noise_mask
!
!-----------------------------------------------------------------------
!
      logical function mask_flag(n_mask, value)
      !
        type(noise_mask), intent(inout) :: n_mask
        real(kind=kreal), intent(in) :: value
        if((value .gt. n_mask%range_min) .and.        &
        &    (value .lt. n_mask%range_max)) then
          mask_flag = .true.
        else
          mask_flag = .false.
        end if
      end function mask_flag
!
!-----------------------------------------------------------------------
!
end module t_noise_node_data
