!>@file   t_comm_table_4_assemble.f90
!!@brief  module t_comm_table_4_assemble
!!
!!@author H. Okuda and H. Matsui
!!@date  Programmed by H. MAtsui in June, 2018
!
!>@brief  comminucation table for data assemble
!!
!!@verbatim
!!      subroutine dealloc_comm_table_4_assemble(asbl_comm)
!!      subroutine s_search_original_domain_node(nprocs_org, org_mesh,  &
!!     &          new_node, irank_from_org, inod_from_org)
!!        type(node_data), intent(in) :: new_node
!!        type(mesh_geometry), intent(inout) :: org_mesh(nprocs_org)
!!        type(comm_table_4_assemble), intent(inout) :: asbl_comm
!!@endverbatim
!!
      module t_comm_table_4_assemble
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_mesh_data
      use t_geometry_data
      use t_file_IO_parameter
!
      implicit none
!
      type comm_table_4_assemble
        integer(kind = kint) :: nprocs_org
        integer(kind = kint), allocatable :: istack_recv(:)
!
        integer(kind = kint), allocatable :: item_send(:)
        integer(kind = kint), allocatable :: item_recv(:)
      end type comm_table_4_assemble
!
      private :: alloc_comm_table_4_assemble
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_comm_table_4_assemble                            &
     &         (nprocs_org, new_node, asbl_comm)
!
      integer, intent(in) :: nprocs_org
      type(node_data), intent(in) :: new_node
      type(comm_table_4_assemble), intent(inout) :: asbl_comm
!
!
      asbl_comm%nprocs_org = nprocs_org
      allocate(asbl_comm%istack_recv(0:asbl_comm%nprocs_org))
      allocate(asbl_comm%item_send(new_node%internal_node))
      allocate(asbl_comm%item_recv(new_node%internal_node))
!
      asbl_comm%istack_recv = 0
!$omp parallel workshare
      asbl_comm%item_send = 0
      asbl_comm%item_recv = 0
!$omp end parallel workshare
!
      end subroutine alloc_comm_table_4_assemble
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_comm_table_4_assemble(asbl_comm)
!
      type(comm_table_4_assemble), intent(inout) :: asbl_comm
!
      deallocate(asbl_comm%istack_recv)
      deallocate(asbl_comm%item_send, asbl_comm%item_recv)
!
      end subroutine dealloc_comm_table_4_assemble
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_search_original_domain_node                          &
     &         (nprocs_org, org_mesh, new_node, asbl_comm)
!
      use share_mesh_data
      use search_original_domain_node
!
      integer, intent(in) :: nprocs_org
      type(node_data), intent(in) :: new_node
!
      type(mesh_geometry), intent(inout) :: org_mesh(nprocs_org)
      type(comm_table_4_assemble), intent(inout) :: asbl_comm
!
      integer(kind = kint), allocatable :: irank_from_org(:)
      integer(kind = kint), allocatable :: inod_from_org(:)
!
      integer(kind = kint_gl), allocatable :: inew_gl_sorted(:)
      integer(kind = kint), allocatable :: inew_lc_sorted(:)
!
      integer(kind = kint) :: inod, ip
      integer :: id_rank
!
!
      call alloc_comm_table_4_assemble(nprocs_org, new_node, asbl_comm)
!
      allocate(inod_from_org(new_node%internal_node))
      allocate(irank_from_org(new_node%internal_node))
!
      allocate(inew_gl_sorted(new_node%internal_node))
      allocate(inew_lc_sorted(new_node%internal_node))
!
!$omp parallel do
      do inod = 1, new_node%internal_node
        irank_from_org(inod) = -1
        inod_from_org(inod) =   0
        inew_gl_sorted(inod) = new_node%inod_global(inod)
        inew_lc_sorted(inod) = inod
      end do
!$omp end parallel do
!
      do ip = 1, nprocs_org
        id_rank = int(ip - 1)
!        write(*,*) 'share_each_node_data', ip
        call share_each_node_data(ip, org_mesh(ip)%node)
!
        call search_node_by_global_id(id_rank, org_mesh(ip)%node,       &
     &      new_node, inew_gl_sorted, inew_lc_sorted,                   &
     &      irank_from_org, inod_from_org)
      end do
!
      call set_comm_table_4_assemble                                    &
     &   (nprocs_org, org_mesh, new_node,                               &
     &    irank_from_org, inod_from_org, asbl_comm%istack_recv,         &
     &    asbl_comm%item_send, asbl_comm%item_recv)
!
      deallocate(inew_gl_sorted, inew_lc_sorted)
      deallocate(irank_from_org, inod_from_org)
!
      end subroutine s_search_original_domain_node
!
! ----------------------------------------------------------------------
!
      end module t_comm_table_4_assemble
