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
!-----------------------------------------------------------------------
!
      end module t_noise_node_data
