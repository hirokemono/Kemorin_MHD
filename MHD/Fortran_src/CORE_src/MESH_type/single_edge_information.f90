!>@file   single_edge_information.f90
!!@brief  module single_edge_information
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Construct mesh strucuture informations
!!
!!@verbatim
!!      subroutine const_single_edge_infos(id_rank, mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!@endverbatim
!
      module single_edge_information
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
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
      subroutine const_single_edge_infos(id_rank, mesh)
!
      use set_surf_edge_mesh
      use cal_mesh_position
!
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(inout) :: mesh
!
      integer(kind = kint), allocatable :: irank_local(:)
      integer(kind = kint), allocatable :: inod_local(:)
!
      integer(kind = kint) :: inod
!
!
      allocate(irank_local(mesh%node%numnod))
      allocate(inod_local(mesh%node%numnod))
!
!$omp parallel do
      do inod = 1, mesh%node%numnod
        irank_local(inod) = id_rank
        inod_local(inod) =  inod
      end do
!$omp end parallel do
!
      if(iflag_debug .gt. 0) write(*,*) 'const_edge_connectivity'
      call const_edge_connectivity(mesh%node, mesh%ele, mesh%surf,      &
     &    irank_local, inod_local, mesh%edge)
      deallocate(irank_local, inod_local)
!
      if (iflag_debug.gt.0) write(*,*) 'set_center_of_edge'
      call alloc_edge_geometory(mesh%edge)
      call set_center_of_edge(mesh%node, mesh%edge)
!
      end subroutine const_single_edge_infos
!
! ----------------------------------------------------------------------
!
      end module single_edge_information
