!> @file  const_nod_ele_comm_table.f90
!!      module const_nod_ele_comm_table
!!
!! @author  H. Matsui
!! @date Programmed in Nov., 2008
!
!> @brief Routines to make import table
!!
!!@verbatim
!!      subroutine const_nod_import_table(id_rank, nprocs,              &
!!     &          numnod, id_org_domain, nod_comm)
!!        integer(kind = kint), intent(in) :: id_rank, nprocs
!!        integer(kind = kint), intent(in) :: numnod
!!        integer(kind = kint), intent(in) :: id_org_domain(numnod)
!!        type(communication_table), intent(inout) :: nod_comm
!!
!!      subroutine const_ele_import_table(id_rank, nprocs,              &
!!     &          numnod, numele, nnod_4_ele, inod_global, ie,          &
!!     &          id_org_domain, ele_comm, ele_comm_gl)
!!        integer(kind = kint), intent(in) :: id_rank, nprocs
!!        integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
!!        integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!!        integer(kind = kint), intent(in) :: inod_global(numnod)
!!        integer(kind = kint), intent(in) :: id_org_domain(numele)
!!        type(communication_table), intent(inout) :: ele_comm
!!        type(work_4_const_export), intent(inout) :: ele_comm_gl
!!
!!      subroutine const_ele_export_table(id_rank, nprocs,              &
!!     &          numnod, internal_node, numele, nnod_4_ele,            &
!!     &          id_global, ie, ele_comm, ele_comm_gl, comm_tbl)
!!        integer(kind = kint), intent(in) :: numnod, internal_node
!!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!!        integer(kind = kint), intent(in) :: id_global(numnod)
!!        integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!!        type(communication_table), intent(in) :: ele_comm(nprocs)
!!        type(work_4_const_export), intent(in) :: ele_comm_gl(nprocs)
!!        type(communication_table), intent(inout) :: comm_tbl
!!@endverbatim
!
      module const_nod_ele_comm_table
!
      use m_precision
!
      use t_comm_table
      use t_const_export_table
!
      implicit  none
!
      private :: set_global_nod_4_import_ele
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_nod_import_table(id_rank, nprocs,                &
     &          numnod, id_org_domain, nod_comm)
!
      use const_import_table
!
      integer(kind = kint), intent(in) :: id_rank, nprocs
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: id_org_domain(numnod)
!
      type(communication_table), intent(inout) :: nod_comm
!
!
      call count_import_domain(nprocs, numnod, id_org_domain,           &
     &    nod_comm%num_neib)
!
      call alloc_neighbouring_id( nod_comm )
      call alloc_import_num( nod_comm )
!
      call set_import_domain(nprocs, numnod, id_org_domain,             &
     &    nod_comm%num_neib, nod_comm%id_neib)
      call count_num_import(numnod, id_org_domain,                      &
     &    nod_comm%num_neib, nod_comm%id_neib, nod_comm%num_import,     &
     &    nod_comm%istack_import, nod_comm%ntot_import)
!
      call alloc_import_item(nod_comm)
!
      call set_import_item(numnod, id_org_domain,                       &
     &    nod_comm%num_neib, nod_comm%id_neib, nod_comm%istack_import,  &
     &    nod_comm%ntot_import, nod_comm%item_import)
!
      end subroutine const_nod_import_table
!
! ----------------------------------------------------------------------
!
      subroutine const_ele_import_table(id_rank, nprocs,                &
     &          numnod, numele, nnod_4_ele, inod_global, ie,            &
     &          id_org_domain, ele_comm, ele_comm_gl)
!
      use const_import_table
!
      integer(kind = kint), intent(in) :: id_rank, nprocs
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: id_org_domain(numele)
!
      type(communication_table), intent(inout) :: ele_comm
      type(work_4_const_export), intent(inout) :: ele_comm_gl
!
      integer(kind = kint) :: ip
!
!
      ip = id_rank + 1
!
      call count_import_domain(nprocs, numele, id_org_domain,           &
     &    ele_comm%num_neib)
!
      call alloc_neighbouring_id( ele_comm )
      call alloc_import_num( ele_comm )
!
      call set_import_domain(nprocs, numele, id_org_domain,             &
     &    ele_comm%num_neib, ele_comm%id_neib)
      call count_num_import(numele, id_org_domain,                      &
     &    ele_comm%num_neib, ele_comm%id_neib, ele_comm%num_import,     &
     &    ele_comm%istack_import, ele_comm%ntot_import)
!
      call alloc_import_item(ele_comm)
!
      call set_import_item(numele, id_org_domain,                       &
     &    ele_comm%num_neib, ele_comm%id_neib, ele_comm%istack_import,  &
     &    ele_comm%ntot_import, ele_comm%item_import)
!
!
      call alloc_ie_gl_import                                           &
     &   (nnod_4_ele, ele_comm%ntot_import, ele_comm_gl)
!
      call set_global_nod_4_import_ele(numnod, numele, nnod_4_ele,      &
     &    inod_global, ie, ele_comm%ntot_import, ele_comm%item_import,  &
     &    ele_comm_gl%ie_gl_import)
!
      end subroutine const_ele_import_table
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_ele_export_table(id_rank, nprocs,                &
     &          numnod, internal_node, numele, nnod_4_ele,              &
     &          id_global, ie, ele_comm, ele_comm_gl, comm_tbl)
!
      integer(kind = kint), intent(in) :: id_rank, nprocs
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: id_global(numnod)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      type(communication_table), intent(in) :: ele_comm(nprocs)
      type(work_4_const_export), intent(in) :: ele_comm_gl(nprocs)
!
      type(communication_table), intent(inout) :: comm_tbl
!
!
      call count_ele_comm_neib                                          &
     &   (id_rank, nprocs, ele_comm, comm_tbl%num_neib)
!
      call alloc_comm_table_num(comm_tbl)
!
      call set_ele_comm_neib(id_rank, nprocs, ele_comm,                 &
     &    comm_tbl%num_neib, comm_tbl%id_neib)
      call set_ele_comm_tbl_num(id_rank, nprocs, ele_comm,              &
     &    comm_tbl%num_neib, comm_tbl%id_neib,                          &
     &    comm_tbl%ntot_import, comm_tbl%ntot_export,                   &
     &    comm_tbl%num_import, comm_tbl%num_export,                     &
     &    comm_tbl%istack_import, comm_tbl%istack_export)
!
      call alloc_comm_table_item(comm_tbl)
!
      call set_ele_import_item(id_rank, nprocs, ele_comm,               &
     &    comm_tbl%num_neib, comm_tbl%ntot_import, comm_tbl%id_neib,    &
     &    comm_tbl%num_import, comm_tbl%istack_import,                  &
     &    comm_tbl%item_import)
!
      call set_ele_export_item(numnod, internal_node, numele,           &
     &    nnod_4_ele, id_global, ie, nprocs, ele_comm, ele_comm_gl,     &
     &    comm_tbl%num_neib, comm_tbl%id_neib, comm_tbl%ntot_export,    &
     &    comm_tbl%num_export, comm_tbl%istack_export,                  &
     &    comm_tbl%item_export)
!
      end subroutine const_ele_export_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_global_nod_4_import_ele(numnod, numele,            &
     &          nnod_4_ele, inod_global, ie, ntot_import, item_import,  &
     &          ie_gl_import)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: inod_global(numnod)
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: item_import(ntot_import)
      integer(kind = kint), intent(inout)                               &
     &      :: ie_gl_import(nnod_4_ele,ntot_import)
!
      integer(kind = kint) :: inum, iele, k1, inod
!
      do inum = 1, ntot_import
        iele = item_import(inum)
        do k1 = 1, nnod_4_ele
          inod = ie(iele,k1)
          ie_gl_import(k1,inum) = inod_global(inod)
        end do
      end do
!
      end subroutine set_global_nod_4_import_ele
!
!------------------------------------------------------------------
!
      end module const_nod_ele_comm_table
