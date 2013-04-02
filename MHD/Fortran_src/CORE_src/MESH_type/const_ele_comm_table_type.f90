!const_ele_comm_table_type.f90
!      module const_ele_comm_table_type
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine s_const_ele_comm_table_type(my_rank, nprocs,          &
!     &          numnod, internal_node, numele, nnod_4_ele,             &
!     &          id_global, ie, comm_tbl)
!        integer(kind = kint), intent(in) :: numnod, internal_node
!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!        integer(kind = kint), intent(in) :: id_global(numnod)
!        integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!        type(communication_table), intent(inout) :: comm_tbl
!
      module const_ele_comm_table_type
!
      use m_precision
!
      use const_export_table
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_const_ele_comm_table_type(my_rank, nprocs,           &
     &          numnod, internal_node, numele, nnod_4_ele,              &
     &          id_global, ie, comm_tbl)
!
      use t_comm_table
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: id_global(numnod)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      type(communication_table), intent(inout) :: comm_tbl
!
!
      call count_ele_comm_neib(my_rank, nprocs, comm_tbl%num_neib)
!
      call allocate_type_comm_tbl_num(comm_tbl)
!
      call set_ele_comm_neib(my_rank, nprocs,                           &
     &    comm_tbl%num_neib, comm_tbl%id_neib)
      call set_ele_comm_tbl_num(my_rank,                                &
     &    comm_tbl%num_neib, comm_tbl%id_neib,                          &
     &    comm_tbl%ntot_import, comm_tbl%ntot_export,                   &
     &    comm_tbl%num_import, comm_tbl%num_export,                     &
     &    comm_tbl%istack_import, comm_tbl%istack_export)
!
      call allocate_type_comm_tbl_item(comm_tbl)
!
      call set_ele_import_item(my_rank,                                 &
     &    comm_tbl%num_neib, comm_tbl%ntot_import, comm_tbl%id_neib,    &
     &    comm_tbl%num_import, comm_tbl%istack_import,                  &
     &    comm_tbl%item_import)
!
      call set_ele_export_item(numnod, internal_node,                   &
     &    numele, nnod_4_ele, id_global, ie, comm_tbl%num_neib,         &
     &    comm_tbl%id_neib, comm_tbl%ntot_export, comm_tbl%num_export,  &
     &    comm_tbl%istack_export, comm_tbl%item_export)
!
      end subroutine s_const_ele_comm_table_type
!
!------------------------------------------------------------------
!
      end module const_ele_comm_table_type
