!>@file   t_read_mesh_data.f90
!!@brief  module t_read_mesh_data
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui
!
!>@brief Data arry for mesh_data_IO
!!
!!@verbatim
!!      subroutine alloc_surface_connect_IO(sfed_IO)
!!      subroutine alloc_edge_connect_IO(sfed_IO)
!!
!!      subroutine dealloc_surface_connect_IO(sfed_IO)
!!      subroutine dealloc_edge_connect_IO(sfed_IO)
!!
!!      subroutine alloc_ele_vector_IO(nod_IO, sfed_IO)
!!      subroutine dealloc_ele_vector_IO(sfed_IO)
!!
!!      subroutine alloc_ele_scalar_IO(nod_IO, sfed_IO)
!!      subroutine dealloc_ele_scalar_IO(sfed_IO)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module t_read_mesh_data
!
      use m_precision
      use t_geometry_data
!
      implicit  none
!
!>
      type surf_edge_IO_data
        real(kind=kreal),   pointer :: ele_vector(:,:)
        real(kind=kreal),   pointer :: ele_scalar(:)
!
        integer(kind = kint) :: nsf_4_ele
        integer(kind = kint) :: nsurf_in_ele = 6
        integer(kind = kint), pointer  :: isf_for_ele(:,:)
!
        integer(kind = kint) :: ned_4_ele
        integer(kind = kint) :: nedge_in_ele
        integer(kind = kint), pointer  :: iedge_for_ele(:,:)
      end type surf_edge_IO_data
!
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine alloc_surface_connect_IO(sfed_IO)
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: num1, num2
!
!
      num1 = sfed_IO%nsf_4_ele
      num2 = sfed_IO%nsurf_in_ele
      allocate( sfed_IO%isf_for_ele(num1,num2) )
      sfed_IO%isf_for_ele = 0
!
      end subroutine alloc_surface_connect_IO
!
!------------------------------------------------------------------
!
      subroutine alloc_edge_connect_IO(sfed_IO)
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint) :: num1, num2
!
!
      num1 = sfed_IO%ned_4_ele
      num2 = sfed_IO%nedge_in_ele
      allocate(sfed_IO%iedge_for_ele(num1,num2) )
      sfed_IO%iedge_for_ele = 0
!
      end subroutine alloc_edge_connect_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_surface_connect_IO(sfed_IO)
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      deallocate( sfed_IO%isf_for_ele )
!
      end subroutine dealloc_surface_connect_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_edge_connect_IO(sfed_IO)
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      deallocate( sfed_IO%iedge_for_ele )
!
      end subroutine dealloc_edge_connect_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      allocate( sfed_IO%ele_vector(nod_IO%numnod,3) )
      sfed_IO%ele_vector = 0.0d0
!
      end subroutine alloc_ele_vector_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_ele_vector_IO(sfed_IO)
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      deallocate( sfed_IO%ele_vector )
!
      end subroutine dealloc_ele_vector_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      allocate( sfed_IO%ele_scalar(nod_IO%numnod) )
      sfed_IO%ele_scalar = 0.0d0
!
      end subroutine alloc_ele_scalar_IO
!
!------------------------------------------------------------------
!
      subroutine dealloc_ele_scalar_IO(sfed_IO)
!
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      deallocate( sfed_IO%ele_scalar )
!
      end subroutine dealloc_ele_scalar_IO
!
!------------------------------------------------------------------
!
      end module t_read_mesh_data
