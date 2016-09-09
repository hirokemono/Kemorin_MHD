!set_node_data_4_IO.f90
!      module set_node_data_4_IO
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine copy_node_geometry_to_IO(node)
!      subroutine copy_node_sph_to_IO(node)
!      subroutine copy_node_cyl_to_IO(node)
!
!      subroutine copy_node_geometry_from_IO(node)
!        type(node_data), intent(inout) :: node
!
      module set_node_data_4_IO
!
      use m_precision
!
      use t_geometry_data
      use m_read_mesh_data
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine copy_node_geometry_to_IO(node)
!
      integer(kind = kint) :: inod
      type(node_data), intent(in) :: node
!
!
      nod_IO%numnod =        node%numnod
      nod_IO%internal_node = node%internal_node
!
      call alloc_node_geometry_base(nod_IO)
!
!$omp parallel do
      do inod = 1, node%numnod
        nod_IO%inod_global(inod) = node%inod_global(inod)
        nod_IO%xx(inod,1) = node%xx(inod,1)
        nod_IO%xx(inod,2) = node%xx(inod,2)
        nod_IO%xx(inod,3) = node%xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine copy_node_geometry_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_node_sph_to_IO(node)
!
      integer(kind = kint) :: inod
      type(node_data), intent(inout) :: node
!
!
      nod_IO%numnod =        node%numnod
      nod_IO%internal_node = node%internal_node
!
      call alloc_node_geometry_base(nod_IO)
!
!$omp parallel do
      do inod = 1, node%numnod
        nod_IO%inod_global(inod) = node%inod_global(inod)
        nod_IO%xx(inod,1) = node%rr(inod)
        nod_IO%xx(inod,2) = node%theta(inod)
        nod_IO%xx(inod,3) = node%phi(inod)
      end do
!$omp end parallel do
!
!
      end subroutine copy_node_sph_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_node_cyl_to_IO(node)
!
      integer(kind = kint) :: inod
      type(node_data), intent(inout) :: node
!
!
      nod_IO%numnod =        node%numnod
      nod_IO%internal_node = node%internal_node
!
      call alloc_node_geometry_base(nod_IO)
!
!$omp parallel do
      do inod = 1, node%numnod
        nod_IO%inod_global(inod) = node%inod_global(inod)
        nod_IO%xx(inod,1) = node%ss(inod)
        nod_IO%xx(inod,2) = node%phi(inod)
        nod_IO%xx(inod,3) = node%xx(inod,3)
      end do
!$omp end parallel do
!
!
      end subroutine copy_node_cyl_to_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_node_geometry_from_IO(node)
!
      integer(kind = kint) :: inod
      type(node_data), intent(inout) :: node
!
!
      node%numnod =        nod_IO%numnod
      node%internal_node = nod_IO%internal_node
!
      call alloc_node_geometry_base(node)
!
!$omp parallel do
      do inod = 1, node%numnod
        node%inod_global(inod) = nod_IO%inod_global(inod)
        node%xx(inod,1) = nod_IO%xx(inod,1)
        node%xx(inod,2) = nod_IO%xx(inod,2)
        node%xx(inod,3) = nod_IO%xx(inod,3)
      end do
!$omp end parallel do
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine copy_node_geometry_from_IO
!
!------------------------------------------------------------------
!
      end module set_node_data_4_IO
