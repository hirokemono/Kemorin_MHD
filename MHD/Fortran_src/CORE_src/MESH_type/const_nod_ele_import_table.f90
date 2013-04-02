!const_nod_ele_import_table.f90
!      module const_nod_ele_import_table
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine const_nod_import_table(my_rank, nprocs,               &
!     &          numnod, id_org_domain, nod_comm)
!        integer(kind = kint), intent(in) :: my_rank, nprocs
!        integer(kind = kint), intent(in) :: numnod
!        integer(kind = kint), intent(in) :: id_org_domain(numnod)
!        type(communication_table), intent(inout) :: nod_comm
!
!      subroutine const_ele_import_table(my_rank, nprocs,               &
!     &          numnod, numele, nnod_4_ele, inod_global, ie,           &
!     &          id_org_domain, ele_comm, ele_comm_gl)
!        integer(kind = kint), intent(in) :: my_rank, nprocs
!        integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
!        integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!        integer(kind = kint), intent(in) :: inod_global(numnod)
!        integer(kind = kint), intent(in) :: id_org_domain(numele)
!        type(communication_table), intent(inout) :: ele_comm
!        type(work_4_const_export), intent(inout) :: ele_comm_gl
!
      module const_nod_ele_import_table
!
      use m_precision
!
      use t_comm_table
      use const_import_table
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_nod_import_table(my_rank, nprocs,                &
     &          numnod, id_org_domain, nod_comm)
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: id_org_domain(numnod)
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: ip
!
!
      ip = my_rank + 1
!
      call count_import_domain(nprocs, numnod, id_org_domain,           &
     &    nod_comm%num_neib)
!
      call allocate_type_neib_id( nod_comm )
      call allocate_type_import_num( nod_comm )
!
      call set_import_domain(nprocs, numnod, id_org_domain,             &
     &    nod_comm%num_neib, nod_comm%id_neib)
      call count_num_import(numnod, id_org_domain,                      &
     &    nod_comm%num_neib, nod_comm%id_neib, nod_comm%num_import,     &
     &    nod_comm%istack_import, nod_comm%ntot_import)
!
      call allocate_type_import_item(nod_comm)
!
      call set_import_item(numnod, id_org_domain,                       &
     &    nod_comm%num_neib, nod_comm%id_neib, nod_comm%istack_import,  &
     &    nod_comm%ntot_import, nod_comm%item_import)
!
      end subroutine const_nod_import_table
!
! ----------------------------------------------------------------------
!
      subroutine const_ele_import_table(my_rank, nprocs,                &
     &          numnod, numele, nnod_4_ele, inod_global, ie,            &
     &          id_org_domain, ele_comm, ele_comm_gl)
!
      use m_const_ele_comm_tbl
!
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
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
      ip = my_rank + 1
!
      call count_import_domain(nprocs, numele, id_org_domain,           &
     &    ele_comm%num_neib)
!
      call allocate_type_neib_id( ele_comm )
      call allocate_type_import_num( ele_comm )
!
      call set_import_domain(nprocs, numele, id_org_domain,             &
     &    ele_comm%num_neib, ele_comm%id_neib)
      call count_num_import(numele, id_org_domain,                      &
     &    ele_comm%num_neib, ele_comm%id_neib, ele_comm%num_import,     &
     &    ele_comm%istack_import, ele_comm%ntot_import)
!
      call allocate_type_import_item(ele_comm)
!
      call set_import_item(numele, id_org_domain,                       &
     &    ele_comm%num_neib, ele_comm%id_neib, ele_comm%istack_import,  &
     &    ele_comm%ntot_import, ele_comm%item_import)
!
!
      call alloc_ie_gl_import(ele_comm_gl, nnod_4_ele,                  &
     &    ele_comm%ntot_import)
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
      end module const_nod_ele_import_table
