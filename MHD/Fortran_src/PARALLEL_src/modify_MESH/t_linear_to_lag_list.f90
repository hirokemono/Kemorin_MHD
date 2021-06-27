!>@file   t_linear_to_lag_list.f90
!!@brief  module t_linear_to_lag_list
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief node list to construct lagrange mesh from tri-linear mesh
!!
!!@verbatim
!!      subroutine init_linear_to_lag_list                              &
!!     &         (mesh_l, ele_comm_l, surf_comm_l, edge_comm_l,         &
!!     &          l_to_lag)
!!      subroutine dealloc_linear_to_lag_list(l_to_lag)
!!        type(mesh_geometry), intent(in) :: mesh_l
!!        type(communication_table), intent(in) :: ele_comm_l
!!        type(communication_table), intent(in) :: surf_comm_l
!!        type(communication_table), intent(in) :: edge_comm_l
!!        type(linear_to_lag_list), intent(inout) :: l_to_lag
!!        integer(kind = kint), intent(inout) :: numnod_l
!!        integer(kind = kint), intent(inout) :: internal_node_l
!!@endverbatim
!
      module t_linear_to_lag_list
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_comm_table
      use t_surface_data
      use t_edge_data
!
      type linear_to_lag_list
        integer(kind = kint) :: internal_edge_l2lag
        integer(kind = kint) :: internal_surf_l2lag
        integer(kind = kint) :: internal_ele_l2lag
!
        integer(kind = kint) :: numnod_gl_l2lag
        integer(kind = kint) :: numedge_gl_l2lag
        integer(kind = kint) :: numsurf_gl_l2lag
        integer(kind = kint) :: numele_gl_l2lag
!
        integer(kind = kint), allocatable :: inod_linear_to_lag(:)
        integer(kind = kint), allocatable :: iedge_linear_to_lag(:)
        integer(kind = kint), allocatable :: isurf_linear_to_lag(:)
        integer(kind = kint), allocatable :: iele_linear_to_lag(:)
      end type linear_to_lag_list
!
      private :: alloc_linear_to_lag_list, set_linear_to_lag_list
      private :: check_linear_to_lag_list
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_linear_to_lag_list                                &
     &         (mesh_l, ele_comm_l, surf_comm_l, edge_comm_l,           &
     &          l_to_lag)
!
      type(mesh_geometry), intent(in) :: mesh_l
      type(communication_table), intent(in) :: ele_comm_l
      type(communication_table), intent(in) :: surf_comm_l
      type(communication_table), intent(in) :: edge_comm_l
!
      type(linear_to_lag_list), intent(inout) :: l_to_lag
!
!
      call alloc_linear_to_lag_list                                     &
     &   (mesh_l%node, mesh_l%ele, mesh_l%surf, mesh_l%edge, l_to_lag)
      call set_linear_to_lag_list                                       &
     &   (mesh_l%node, mesh_l%ele, mesh_l%surf, mesh_l%edge,            &
     &    mesh_l%nod_comm, ele_comm_l, surf_comm_l, edge_comm_l,        &
     &    l_to_lag)
!
      if(i_debug .eq. 0) return
      call check_linear_to_lag_list                                     &
     &   (mesh_l%node, mesh_l%ele, mesh_l%surf, mesh_l%edge, l_to_lag)
!
      end subroutine init_linear_to_lag_list
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_linear_to_lag_list(l_to_lag)
!
      type(linear_to_lag_list), intent(inout) :: l_to_lag
!
!
      if(allocated(l_to_lag%inod_linear_to_lag) .eqv. .FALSE.) return
!
      deallocate(l_to_lag%inod_linear_to_lag)
      deallocate(l_to_lag%iedge_linear_to_lag)
      deallocate(l_to_lag%isurf_linear_to_lag)
      deallocate(l_to_lag%iele_linear_to_lag)
!
      end subroutine dealloc_linear_to_lag_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_linear_to_lag_list                               &
     &         (node, ele, surf, edge, l_to_lag)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(linear_to_lag_list), intent(inout) :: l_to_lag
!
!
      if(allocated(l_to_lag%inod_linear_to_lag)) return
!
      allocate(l_to_lag%inod_linear_to_lag(1:node%numnod))
      allocate(l_to_lag%iedge_linear_to_lag(1:edge%numedge))
      allocate(l_to_lag%isurf_linear_to_lag(1:surf%numsurf))
      allocate(l_to_lag%iele_linear_to_lag(1:ele%numele))
!
      if(node%numnod .gt. 0) then
!$omp parallel workshare
        l_to_lag%inod_linear_to_lag(1:node%numnod) =   0
!$omp end parallel workshare
      end if
!
      if(surf%numsurf .gt. 0) then
!$omp parallel workshare
        l_to_lag%isurf_linear_to_lag(1:surf%numsurf) = 0
!$omp end parallel workshare
      end if
!
      if(edge%numedge .gt. 0) then
!$omp parallel workshare
        l_to_lag%iedge_linear_to_lag(1:edge%numedge) = 0
!$omp end parallel workshare
      end if
!
      if(ele%numele .gt. 0) then
!$omp parallel workshare
        l_to_lag%iele_linear_to_lag(1:ele%numele) =    0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_linear_to_lag_list
!
!-----------------------------------------------------------------------
!
      subroutine set_linear_to_lag_list(node, ele, surf, edge,          &
     &          nod_comm, ele_comm, surf_comm, edge_comm, l_to_lag)
!
      use calypso_mpi_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(communication_table), intent(in) :: nod_comm, ele_comm
      type(communication_table), intent(in) :: surf_comm, edge_comm
!
      type(linear_to_lag_list), intent(inout) :: l_to_lag
!
      integer(kind = kint) :: i, icou, inum, internal_node_l, numnod_l
!
!
!$omp parallel do
      do i = 1, node%internal_node
        l_to_lag%inod_linear_to_lag(i) = i
      end do
!$omp end parallel do
!
      icou = node%internal_node
      do i = 1, edge%numedge
        if(edge%ie_edge(i,1) .le. node%internal_node) then
          icou = icou + 1
          l_to_lag%iedge_linear_to_lag(i) = icou
        end if
      end do
      l_to_lag%internal_edge_l2lag = icou - node%internal_node
!
      do i = 1, surf%numsurf
        if(surf%ie_surf(i,1) .le. node%internal_node) then
          icou = icou + 1
          l_to_lag%isurf_linear_to_lag(i) = icou
        end if
      end do
      l_to_lag%internal_surf_l2lag = icou - node%internal_node          &
     &                              - l_to_lag%internal_edge_l2lag
!
      do i = 1, ele%numele
        if(ele%ie(i,1) .le. node%internal_node) then
          icou = icou + 1
          l_to_lag%iele_linear_to_lag(i) = icou
        end if
      end do
      l_to_lag%internal_ele_l2lag = icou - node%internal_node           &
     &                             - l_to_lag%internal_edge_l2lag       &
     &                             - l_to_lag%internal_surf_l2lag
      internal_node_l =  icou
!
      call calypso_mpi_allreduce_one_int(node%internal_node,            &
     &    l_to_lag%numnod_gl_l2lag, MPI_SUM)
      call calypso_mpi_allreduce_one_int(l_to_lag%internal_edge_l2lag,  &
     &    l_to_lag%numedge_gl_l2lag, MPI_SUM)
      call calypso_mpi_allreduce_one_int(l_to_lag%internal_surf_l2lag,  &
     &    l_to_lag%numsurf_gl_l2lag, MPI_SUM)
      call calypso_mpi_allreduce_one_int(l_to_lag%internal_ele_l2lag,   &
     &    l_to_lag%numele_gl_l2lag, MPI_SUM)
!
      icou = internal_node_l
!$omp parallel do private(inum,i)
      do inum = 1, nod_comm%ntot_import
        i = nod_comm%item_import(inum)
        l_to_lag%inod_linear_to_lag(i) = icou + inum
      end do
!$omp end parallel do
!
      icou = icou + nod_comm%ntot_import
!$omp parallel do private(inum,i)
      do inum = 1, edge_comm%ntot_import
        i = edge_comm%item_import(inum)
        l_to_lag%iedge_linear_to_lag(i) = icou + inum
      end do
!$omp end parallel do
!
      icou = icou + edge_comm%ntot_import
!$omp parallel do private(inum,i)
      do inum = 1, surf_comm%ntot_import
        i = surf_comm%item_import(inum)
        l_to_lag%isurf_linear_to_lag(i) = icou + inum
      end do
!$omp end parallel do
!
      icou = icou + surf_comm%ntot_import
!$omp parallel do private(inum,i)
      do inum = 1, ele_comm%ntot_import
        i = ele_comm%item_import(inum)
        l_to_lag%iele_linear_to_lag(i) = icou + inum
      end do
!$omp end parallel do
      numnod_l = icou + ele_comm%ntot_import
!
      end subroutine set_linear_to_lag_list
!
!-----------------------------------------------------------------------
!
      subroutine check_linear_to_lag_list(node, ele, surf, edge,        &
     &                                    l_to_lag)
!
      use calypso_mpi_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(linear_to_lag_list), intent(in) :: l_to_lag
!
      integer(kind = kint) :: i, icou, ntot_gl
!
!
      icou = 0
!$omp parallel do reduction(+:icou)
      do i = 1, node%numnod
        if(l_to_lag%inod_linear_to_lag(i) .eq. 0) icou = icou + 1
      end do
!$omp end parallel do
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Missing node in table inod_linear_to_lag', ntot_gl
!
      icou = 0
!$omp parallel do reduction(+:icou)
      do i = 1, edge%numedge
        if(l_to_lag%iedge_linear_to_lag(i) .eq. 0) icou = icou + 1
      end do
!$omp end parallel do
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Missing edge in table iedge_linear_to_lag', ntot_gl
!
      icou = 0
!$omp parallel do reduction(+:icou)
      do i = 1, surf%numsurf
        if(l_to_lag%isurf_linear_to_lag(i) .eq. 0) icou = icou + 1
      end do
!$omp end parallel do
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Missing surface in table isurf_linear_to_lag', ntot_gl
!
      icou = 0
!$omp parallel do reduction(+:icou)
      do i = 1, ele%numele
        if(l_to_lag%iele_linear_to_lag(i) .eq. 0) icou = icou + 1
      end do
!$omp end parallel do
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Missing element in table iele_linear_to_lag', ntot_gl
!
      end subroutine check_linear_to_lag_list
!
!-----------------------------------------------------------------------
!
      end module t_linear_to_lag_list
