!>@file   t_linear_to_quad_list.f90
!!@brief  module t_linear_to_quad_list
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief node list to construct quad mesh from tri-linear mesh
!!
!!@verbatim
!!      subroutine init_linear_to_quad_list(mesh_q, edge_comm_q,        &
!!     &          numnod_l, internal_node_l, l_to_q)
!!      subroutine dealloc_linear_to_quad_list(l_to_q)
!!        type(mesh_geometry), intent(in) :: mesh_q
!!        type(communication_table), intent(in) :: edge_comm_q
!!        type(linear_to_quad_list), intent(inout) :: l_to_q
!!        integer(kind = kint), intent(inout) :: numnod_l
!!        integer(kind = kint), intent(inout) :: internal_node_l
!!@endverbatim
!
      module t_linear_to_quad_list
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
      type linear_to_quad_list
        integer(kind = kint) :: internal_edge_l2q
!
        integer(kind = kint) :: numnod_gl_l2q
        integer(kind = kint) :: numedge_gl_l2q
!
        integer(kind = kint), allocatable :: inod_linear_to_quad(:)
        integer(kind = kint), allocatable :: iedge_linear_to_quad(:)
      end type linear_to_quad_list
!
      private :: alloc_linear_to_quad_list, set_linear_to_quad_list
      private :: check_linear_to_quad_list
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_linear_to_quad_list(mesh_q, edge_comm_q,          &
     &          numnod_l, internal_node_l, l_to_q)
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(communication_table), intent(in) :: edge_comm_q
!
      type(linear_to_quad_list), intent(inout) :: l_to_q
      integer(kind = kint), intent(inout) :: numnod_l
      integer(kind = kint), intent(inout) :: internal_node_l
!
!
      call alloc_linear_to_quad_list                                    &
     &   (mesh_q%node, mesh_q%edge, l_to_q)
      call set_linear_to_quad_list                                      &
     &   (mesh_q%node, mesh_q%edge, mesh_q%nod_comm, edge_comm_q,       &
     &    numnod_l, internal_node_l, l_to_q)
!
      if(i_debug .eq. 0) return
      call check_linear_to_quad_list                                    &
     &   (mesh_q%node, mesh_q%edge, numnod_l, l_to_q)
!
      end subroutine init_linear_to_quad_list
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_linear_to_quad_list(l_to_q)
!
      type(linear_to_quad_list), intent(inout) :: l_to_q
!
!
      if(allocated(l_to_q%inod_linear_to_quad) .eqv. .FALSE.) return
!
      deallocate(l_to_q%inod_linear_to_quad)
      deallocate(l_to_q%iedge_linear_to_quad)
!
      end subroutine dealloc_linear_to_quad_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_linear_to_quad_list(node, edge, l_to_q)
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
!
      type(linear_to_quad_list), intent(inout) :: l_to_q
!
!
      if(allocated(l_to_q%inod_linear_to_quad)) return
!
      allocate(l_to_q%inod_linear_to_quad(1:node%numnod))
      allocate(l_to_q%iedge_linear_to_quad(1:edge%numedge))
!
      if(node%numnod .gt. 0) then
!$omp parallel workshare
        l_to_q%inod_linear_to_quad(1:node%numnod) =   0
!$omp end parallel workshare
      end if
!
      if(edge%numedge .gt. 0) then
!$omp parallel workshare
        l_to_q%iedge_linear_to_quad(1:edge%numedge) = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_linear_to_quad_list
!
!-----------------------------------------------------------------------
!
      subroutine set_linear_to_quad_list                                &
     &         (node, edge, nod_comm, edge_comm,                        &
     &          numnod_l, internal_node_l, l_to_q)
!
      use calypso_mpi_int
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
!
      type(communication_table), intent(in) :: nod_comm, edge_comm
!
      type(linear_to_quad_list), intent(inout) :: l_to_q
      integer(kind = kint), intent(inout) :: numnod_l
      integer(kind = kint), intent(inout) :: internal_node_l
!
      integer(kind = kint) :: i, icou, inum
!
!
!$omp parallel do
      do i = 1, node%internal_node
        l_to_q%inod_linear_to_quad(i) = i
      end do
!$omp end parallel do
!
      icou = node%internal_node
      do i = 1, edge%numedge
        if(edge%ie_edge(i,1) .le. node%internal_node) then
          icou = icou + 1
          l_to_q%iedge_linear_to_quad(i) = icou
        end if
      end do
      l_to_q%internal_edge_l2q = icou - node%internal_node
      internal_node_l =  icou
!
      call calypso_mpi_allreduce_one_int(node%internal_node,            &
     &    l_to_q%numnod_gl_l2q, MPI_SUM)
      call calypso_mpi_allreduce_one_int(l_to_q%internal_edge_l2q,      &
     &    l_to_q%numedge_gl_l2q, MPI_SUM)
!
      icou = internal_node_l
!$omp parallel do private(inum,i)
      do inum = 1, nod_comm%ntot_import
        i = nod_comm%item_import(inum)
        l_to_q%inod_linear_to_quad(i) = icou + inum
      end do
!$omp end parallel do
!
      icou = icou + nod_comm%ntot_import
!$omp parallel do private(inum,i)
      do inum = 1, edge_comm%ntot_import
        i = edge_comm%item_import(inum)
        l_to_q%iedge_linear_to_quad(i) = icou + inum
      end do
!$omp end parallel do
!
      numnod_l = icou + edge_comm%ntot_import
!
      end subroutine set_linear_to_quad_list
!
!-----------------------------------------------------------------------
!
      subroutine check_linear_to_quad_list(node, edge,                  &
     &                                     numnod_l, l_to_q)
!
      use calypso_mpi_int
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(linear_to_quad_list), intent(in) :: l_to_q
      integer(kind = kint), intent(in) :: numnod_l
!
      integer(kind = kint) :: i, icou, ntot_gl
!
!
      icou = node%numnod + edge%numedge
      if(numnod_l .ne. icou) then
        write(*,*) 'Wrong number of node for linear mesh at ', my_rank, &
     &            ':  ', numnod_l, icou
      end if
!
      icou = 0
!$omp parallel do reduction(+:icou)
      do i = 1, node%numnod
        if(l_to_q%inod_linear_to_quad(i) .eq. 0) icou = icou + 1
      end do
!$omp end parallel do
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Missing node in table inod_linear_to_quad', ntot_gl
!
      icou = 0
!$omp parallel do reduction(+:icou)
      do i = 1, edge%numedge
        if(l_to_q%iedge_linear_to_quad(i) .eq. 0) icou = icou + 1
      end do
!$omp end parallel do
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Missing edge in table iedge_linear_to_quad', ntot_gl
!
      end subroutine check_linear_to_quad_list
!
!-----------------------------------------------------------------------
!
      end module t_linear_to_quad_list
