!>@file   t_quad_to_linear_list.f90
!!@brief  module t_quad_to_linear_list
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief node list to construct tri-linear mesh from quad mesh
!!
!!@verbatim
!!      subroutine init_quad_to_linear_list                             &
!!     &         (mesh_q, ele_comm_q, surf_comm_q,                      &
!!     &          numnod_l, internal_node_l, q_to_l)
!!      subroutine dealloc_quad_to_linear_list(q_to_l)
!!        type(mesh_geometry), intent(in) :: mesh_q
!!        type(communication_table), intent(in) :: ele_comm_q
!!        type(communication_table), intent(in) :: surf_comm_q
!!        type(quad_to_linear_list), intent(inout) :: q_to_l
!!        integer(kind = kint), intent(inout) :: numnod_l
!!        integer(kind = kint), intent(inout) :: internal_node_l
!!@endverbatim
!
      module t_quad_to_linear_list
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_comm_table
      use t_surface_data
!
      type quad_to_linear_list
        integer(kind = kint) :: internal_surf_q2l
        integer(kind = kint) :: internal_ele_q2l
!
        integer(kind = kint) :: numnod_gl_q2l
        integer(kind = kint) :: numsurf_gl_q2l
        integer(kind = kint) :: numele_gl_q2l
!
        integer(kind = kint), allocatable :: inod_quad_to_linear(:)
        integer(kind = kint), allocatable :: isurf_quad_to_linear(:)
        integer(kind = kint), allocatable :: iele_quad_to_linear(:)
      end type quad_to_linear_list
!
      private :: alloc_quad_to_linear_list, set_quad_to_linear_list
      private :: check_quad_to_linear_list
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_quad_to_linear_list                               &
     &         (mesh_q, ele_comm_q, surf_comm_q,                        &
     &          numnod_l, internal_node_l, q_to_l)
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(communication_table), intent(in) :: ele_comm_q
      type(communication_table), intent(in) :: surf_comm_q
!
      type(quad_to_linear_list), intent(inout) :: q_to_l
      integer(kind = kint), intent(inout) :: numnod_l
      integer(kind = kint), intent(inout) :: internal_node_l
!
!
      call alloc_quad_to_linear_list                                    &
     &   (mesh_q%node, mesh_q%ele, mesh_q%surf, q_to_l)
      call set_quad_to_linear_list                                      &
     &   (mesh_q%node, mesh_q%ele, mesh_q%surf, mesh_q%nod_comm,        &
     &    ele_comm_q, surf_comm_q, numnod_l, internal_node_l, q_to_l)
!
      if(i_debug .eq. 0) return
      call check_quad_to_linear_list                                    &
     &   (mesh_q%node, mesh_q%ele, mesh_q%surf, numnod_l, q_to_l)
!
      end subroutine init_quad_to_linear_list
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_quad_to_linear_list(q_to_l)
!
      type(quad_to_linear_list), intent(inout) :: q_to_l
!
!
      if(allocated(q_to_l%inod_quad_to_linear) .eqv. .FALSE.) return
!
      deallocate(q_to_l%inod_quad_to_linear)
      deallocate(q_to_l%isurf_quad_to_linear)
      deallocate(q_to_l%iele_quad_to_linear)
!
      end subroutine dealloc_quad_to_linear_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_quad_to_linear_list(node, ele, surf, q_to_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(quad_to_linear_list), intent(inout) :: q_to_l
!
!
      if(allocated(q_to_l%inod_quad_to_linear)) return
!
      allocate(q_to_l%inod_quad_to_linear(1:node%numnod))
      allocate(q_to_l%isurf_quad_to_linear(1:surf%numsurf))
      allocate(q_to_l%iele_quad_to_linear(1:ele%numele))
!
      if(node%numnod .gt. 0) then
!$omp parallel workshare
        q_to_l%inod_quad_to_linear(1:node%numnod) =   0
!$omp end parallel workshare
      end if
!
      if(surf%numsurf .gt. 0) then
!$omp parallel workshare
        q_to_l%isurf_quad_to_linear(1:surf%numsurf) = 0
!$omp end parallel workshare
      end if
!
      if(ele%numele .gt. 0) then
!$omp parallel workshare
        q_to_l%iele_quad_to_linear(1:ele%numele) =    0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_quad_to_linear_list
!
!-----------------------------------------------------------------------
!
      subroutine set_quad_to_linear_list                                &
     &         (node, ele, surf, nod_comm, ele_comm, surf_comm,         &
     &          numnod_l, internal_node_l, q_to_l)
!
      use calypso_mpi_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
      type(communication_table), intent(in) :: surf_comm
!
      type(quad_to_linear_list), intent(inout) :: q_to_l
      integer(kind = kint), intent(inout) :: numnod_l
      integer(kind = kint), intent(inout) :: internal_node_l
!
      integer(kind = kint) :: i, icou, inum
!
!
!$omp parallel do
      do i = 1, node%internal_node
        q_to_l%inod_quad_to_linear(i) = i
      end do
!$omp end parallel do
!
      icou = node%internal_node
      do i = 1, surf%numsurf
        if(surf%ie_surf(i,1) .le. node%internal_node) then
          icou = icou + 1
          q_to_l%isurf_quad_to_linear(i) = icou
        end if
      end do
      q_to_l%internal_surf_q2l = icou - node%internal_node
!
      do i = 1, ele%numele
        if(ele%ie(i,1) .le. node%internal_node) then
          icou = icou + 1
          q_to_l%iele_quad_to_linear(i) = icou
        end if
      end do
      q_to_l%internal_ele_q2l = icou - node%internal_node               &
     &                               - q_to_l%internal_surf_q2l
      internal_node_l =  icou
!
      call calypso_mpi_allreduce_one_int(node%internal_node,            &
     &    q_to_l%numnod_gl_q2l, MPI_SUM)
      call calypso_mpi_allreduce_one_int(q_to_l%internal_surf_q2l,      &
     &    q_to_l%numsurf_gl_q2l, MPI_SUM)
      call calypso_mpi_allreduce_one_int(q_to_l%internal_ele_q2l,       &
     &    q_to_l%numele_gl_q2l, MPI_SUM)
!
      icou = internal_node_l
!$omp parallel do private(inum,i)
      do inum = 1, nod_comm%ntot_import
        i = nod_comm%item_import(inum)
        q_to_l%inod_quad_to_linear(i) = icou + inum
      end do
!$omp end parallel do
!
      icou = icou + nod_comm%ntot_import
!$omp parallel do private(inum,i)
      do inum = 1, surf_comm%ntot_import
        i = surf_comm%item_import(inum)
        q_to_l%isurf_quad_to_linear(i) = icou + inum
      end do
!$omp end parallel do
!
      icou = icou + surf_comm%ntot_import
!$omp parallel do private(inum,i)
      do inum = 1, ele_comm%ntot_import
        i = ele_comm%item_import(inum)
        q_to_l%iele_quad_to_linear(i) = icou + inum
      end do
!$omp end parallel do
      numnod_l = icou + ele_comm%ntot_import
!
      end subroutine set_quad_to_linear_list
!
!-----------------------------------------------------------------------
!
      subroutine check_quad_to_linear_list(node, ele, surf,             &
     &                                     numnod_l, q_to_l)
!
      use calypso_mpi_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(quad_to_linear_list), intent(in) :: q_to_l
      integer(kind = kint), intent(in) :: numnod_l
!
      integer(kind = kint) :: i, icou, ntot_gl
!
!
      icou = node%numnod + surf%numsurf + ele%numele
      if(numnod_l .ne. icou) then
        write(*,*) 'Wrong number of node for linear mesh at ', my_rank, &
     &            ':  ', numnod_l, icou
      end if
!
      icou = 0
!$omp parallel do reduction(+:icou)
      do i = 1, node%numnod
        if(q_to_l%inod_quad_to_linear(i) .eq. 0) icou = icou + 1
      end do
!$omp end parallel do
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Missing node in table inod_quad_to_linear', ntot_gl
!
      icou = 0
!$omp parallel do reduction(+:icou)
      do i = 1, surf%numsurf
        if(q_to_l%isurf_quad_to_linear(i) .eq. 0) icou = icou + 1
      end do
!$omp end parallel do
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Missing surface in table isurf_quad_to_linear', ntot_gl
!
      icou = 0
!$omp parallel do reduction(+:icou)
      do i = 1, ele%numele
        if(q_to_l%iele_quad_to_linear(i) .eq. 0) icou = icou + 1
      end do
!$omp end parallel do
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Missing element in table iele_quad_to_linear', ntot_gl
!
      end subroutine check_quad_to_linear_list
!
!-----------------------------------------------------------------------
!
      end module t_quad_to_linear_list
