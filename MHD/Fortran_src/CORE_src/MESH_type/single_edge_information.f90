!>@file   single_edge_information.f90
!!@brief  module single_edge_information
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Construct mesh strucuture informations
!!
!!@verbatim
!!      subroutine const_single_edge_infos                              &
!!     &         (id_rank, node, ele, surf, edge)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(inout) :: edge
!!@endverbatim
!
      module single_edge_information
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_single_edge_infos                                &
     &         (id_rank, node, ele, surf, edge)
!
      use set_surf_edge_mesh
      use cal_mesh_position
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(edge_data), intent(inout) :: edge
!
      integer(kind = kint), allocatable :: irank_local(:)
      integer(kind = kint), allocatable :: inod_local(:)
!
      integer(kind = kint) :: inod
!
!
      allocate(irank_local(node%numnod))
      allocate(inod_local(node%numnod))
!
!$omp parallel do
      do inod = 1, node%numnod
        irank_local(inod) = id_rank
        inod_local(inod) =  inod
      end do
!$omp end parallel do
!
      if(iflag_debug .gt. 0) write(*,*) 'const_edge_connectivity'
      call const_edge_connectivity(node, ele, surf,                     &
     &    irank_local, inod_local, edge)
      deallocate(irank_local, inod_local)
!
      if (iflag_debug.gt.0) write(*,*) 'set_center_of_edge'
      call alloc_edge_geometory(edge)
      call set_center_of_edge(node, edge)
!
      end subroutine const_single_edge_infos
!
! ----------------------------------------------------------------------
!
      end module single_edge_information
