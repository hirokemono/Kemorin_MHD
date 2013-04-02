!const_ele_comm_table.f90
!      module const_ele_comm_table
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine s_const_ele_comm_table(my_rank, nprocs)
!        integer(kind = kint), intent(in) :: numnod, internal_node
!      subroutine const_surf_comm_table(my_rank, nprocs)
!        integer(kind = kint), intent(in) :: numnod, internal_node
!      subroutine const_edge_comm_table(my_rank, nprocs)
!        integer(kind = kint), intent(in) :: numnod, internal_node
!
      module const_ele_comm_table
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
      subroutine s_const_ele_comm_table(my_rank, nprocs)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_ele_comm_table
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
!
!
      call count_ele_comm_neib(my_rank, nprocs, num_neib_ele)
!
      call allocate_ele_neib_id
      call allocate_ele_import_num
      call allocate_ele_export_num
!
      call set_ele_comm_neib(my_rank, nprocs,                           &
     &    num_neib_ele, id_neib_ele)
      call set_ele_comm_tbl_num(my_rank, num_neib_ele, id_neib_ele,     &
     &    ntot_import_ele, ntot_export_ele, num_import_ele,             &
     &    num_export_ele,  istack_import_ele, istack_export_ele)
!
      call allocate_ele_import_item
      call allocate_ele_export_item
!
      call set_ele_import_item(my_rank, num_neib_ele, ntot_import_ele,  &
     &    id_neib_ele, num_import_ele, istack_import_ele,               &
     &    item_import_ele)
!
      call set_ele_export_item(numnod, internal_node, numele,           &
     &    nnod_4_ele, globalnodid, ie, num_neib_ele, id_neib_ele,       &
     &    ntot_export_ele, num_export_ele, istack_export_ele,           &
     &    item_export_ele)
!
      end subroutine s_const_ele_comm_table
!
!------------------------------------------------------------------
!
      subroutine const_surf_comm_table(my_rank, nprocs)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_surf_comm_table
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
!
!
      call count_ele_comm_neib(my_rank, nprocs, num_neib_surf)
!
      call allocate_surf_neib_id
      call allocate_surf_import_num
      call allocate_surf_export_num
!
      call set_ele_comm_neib(my_rank, nprocs,                           &
     &    num_neib_surf, id_neib_surf)
      call set_ele_comm_tbl_num(my_rank, num_neib_surf, id_neib_surf,   &
     &    ntot_import_surf, ntot_export_surf, num_import_surf,          &
     &    num_export_surf, istack_import_surf, istack_export_surf)
!
      call allocate_surf_import_item
      call allocate_surf_export_item
!
      call set_ele_import_item(my_rank, num_neib_surf,                  &
     &    ntot_import_surf, id_neib_surf, num_import_surf,              &
     &    istack_import_surf, item_import_surf)
!
      call set_ele_export_item(numnod, internal_node, numsurf,          &
     &    nnod_4_surf, globalnodid, ie_surf, num_neib_surf,             &
     &    id_neib_surf, ntot_export_surf, num_export_surf,              &
     &    istack_export_surf, item_export_surf)
!
      end subroutine const_surf_comm_table
!
!------------------------------------------------------------------
!
      subroutine const_edge_comm_table(my_rank, nprocs)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_edge_comm_table
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
!
!
      call count_ele_comm_neib(my_rank, nprocs, num_neib_edge)
!
      call allocate_edge_neib_id
      call allocate_edge_import_num
      call allocate_edge_export_num
!
      call set_ele_comm_neib(my_rank, nprocs,                           &
     &    num_neib_edge, id_neib_edge)
      call set_ele_comm_tbl_num(my_rank, num_neib_edge, id_neib_edge,   &
     &    ntot_import_edge, ntot_export_edge, num_import_edge,          &
     &    num_export_edge, istack_import_edge, istack_export_edge)
!
      call allocate_edge_import_item
      call allocate_edge_export_item
!
      call set_ele_import_item(my_rank, num_neib_edge,                  &
     &    ntot_import_edge, id_neib_edge, num_import_edge,              &
     &    istack_import_edge, item_import_edge)
!
      call set_ele_export_item(numnod, internal_node, numedge,          &
     &    nnod_4_edge, globalnodid, ie_edge, num_neib_edge,             &
     &    id_neib_edge, ntot_export_edge, num_export_edge,              &
     &    istack_export_edge, item_export_edge)
!
      end subroutine const_edge_comm_table
!
!------------------------------------------------------------------
!
      end module const_ele_comm_table
