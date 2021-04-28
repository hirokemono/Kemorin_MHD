!>@file   const_node_comm_table_q2l.f90
!!@brief  module const_node_comm_table_q2l
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief Construct node communication table on linear mesh from quad mesh
!!
!!@verbatim
!!      subroutine s_const_node_comm_table_q2l                          &
!!     &         (mesh_q, ele_comm_q, surf_comm_q, q_to_l, nod_comm_l)
!!        type(mesh_geometry), intent(in) :: mesh_q
!!        type(communication_table), intent(in) :: ele_comm_q
!!        type(communication_table), intent(in) :: surf_comm_q
!!        type(quad_to_linear_list), intent(in) :: q_to_l
!!        type(communication_table), intent(inout) :: nod_comm_l
!!      subroutine const_node_comm_table_l2lag                          &
!!     &         (mesh_l, ele_comm_l, surf_comm_l, edge_comm_l,         &
!!     &          l_to_lag, nod_comm_lag)
!!        type(mesh_geometry), intent(in) :: mesh_l
!!        type(communication_table), intent(in) :: ele_comm_l
!!        type(communication_table), intent(in) :: surf_comm_l
!!        type(communication_table), intent(in) :: edge_comm_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        type(communication_table), intent(inout) :: nod_comm_lag
!!@endverbatim
!
      module const_node_comm_table_q2l
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_comm_table
      use t_calypso_comm_table
!
      implicit none
!
      private :: convert_comm_item_q2l
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_const_node_comm_table_q2l                            &
     &         (mesh_q, ele_comm_q, surf_comm_q, q_to_l, nod_comm_l)
!
      use t_quad_to_linear_list
      use cvt_calypso_geofem_comm_tbl
      use append_communication_table
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(communication_table), intent(in) :: ele_comm_q
      type(communication_table), intent(in) :: surf_comm_q
      type(quad_to_linear_list), intent(in) :: q_to_l
!
      type(communication_table), intent(inout) :: nod_comm_l
!
      type(communication_table) :: comm_tmp1, comm_tmp2
      type(calypso_comm_table) :: cps_nod_comm
      type(calypso_comm_table) :: cps_surf_comm, cps_ele_comm
!
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    mesh_q%node%numnod, mesh_q%nod_comm, cps_nod_comm)
      call convert_comm_item_q2l                                        &
     &   (mesh_q%node%numnod, q_to_l%inod_quad_to_linear,               &
     &    cps_nod_comm%ntot_import, cps_nod_comm%item_import,           &
     &    cps_nod_comm%ntot_export, cps_nod_comm%item_export)
      call dup_calypso_comm_to_comm_tbl                                 &
     &   (my_rank, nprocs, cps_nod_comm, comm_tmp1)
      call dealloc_calypso_comm_table(cps_nod_comm)
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    mesh_q%surf%numsurf, surf_comm_q, cps_surf_comm)
      call convert_comm_item_q2l                                        &
     &   (mesh_q%surf%numsurf, q_to_l%isurf_quad_to_linear,             &
     &    cps_surf_comm%ntot_import, cps_surf_comm%item_import,         &
     &    cps_surf_comm%ntot_export, cps_surf_comm%item_export)
      call append_ele_communication_table                               &
     &   (comm_tmp1, cps_surf_comm, comm_tmp2)
      call dealloc_calypso_comm_table(cps_surf_comm)
      call dealloc_comm_table(comm_tmp1)
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    mesh_q%ele%numele, ele_comm_q, cps_ele_comm)
      call convert_comm_item_q2l                                        &
     &   (mesh_q%ele%numele, q_to_l%iele_quad_to_linear,                &
     &    cps_ele_comm%ntot_import, cps_ele_comm%item_import,           &
     &    cps_ele_comm%ntot_export, cps_ele_comm%item_export)
      call append_ele_communication_table                               &
     &   (comm_tmp2, cps_ele_comm, nod_comm_l)
      call dealloc_calypso_comm_table(cps_ele_comm)
      call dealloc_comm_table(comm_tmp2)
!
      end subroutine s_const_node_comm_table_q2l
!
!-----------------------------------------------------------------------
!
      subroutine const_node_comm_table_l2lag                            &
     &         (mesh_l, ele_comm_l, surf_comm_l, edge_comm_l,           &
     &          l_to_lag, nod_comm_lag)
!
      use t_linear_to_lag_list
      use cvt_calypso_geofem_comm_tbl
      use append_communication_table
!
      type(mesh_geometry), intent(in) :: mesh_l
      type(communication_table), intent(in) :: ele_comm_l
      type(communication_table), intent(in) :: surf_comm_l
      type(communication_table), intent(in) :: edge_comm_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
!
      type(communication_table), intent(inout) :: nod_comm_lag
!
      type(communication_table) :: comm_tmp1, comm_tmp2, comm_tmp3
      type(calypso_comm_table) :: cps_nod_comm, cps_edge_comm
      type(calypso_comm_table) :: cps_surf_comm, cps_ele_comm
!
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    mesh_l%node%numnod, mesh_l%nod_comm, cps_nod_comm)
      call convert_comm_item_q2l                                        &
     &   (mesh_l%node%numnod, l_to_lag%inod_linear_to_lag,              &
     &    cps_nod_comm%ntot_import, cps_nod_comm%item_import,           &
     &    cps_nod_comm%ntot_export, cps_nod_comm%item_export)
      call dup_calypso_comm_to_comm_tbl                                 &
     &   (my_rank, nprocs, cps_nod_comm, comm_tmp1)
      call dealloc_calypso_comm_table(cps_nod_comm)
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    mesh_l%edge%numedge, edge_comm_l, cps_edge_comm)
      call convert_comm_item_q2l                                        &
     &   (mesh_l%edge%numedge, l_to_lag%iedge_linear_to_lag,            &
     &    cps_edge_comm%ntot_import, cps_edge_comm%item_import,         &
     &    cps_edge_comm%ntot_export, cps_edge_comm%item_export)
      call append_ele_communication_table                               &
     &   (comm_tmp1, cps_edge_comm, comm_tmp2)
      call dealloc_calypso_comm_table(cps_edge_comm)
      call dealloc_comm_table(comm_tmp1)
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    mesh_l%surf%numsurf, surf_comm_l, cps_surf_comm)
      call convert_comm_item_q2l                                        &
     &   (mesh_l%surf%numsurf, l_to_lag%isurf_linear_to_lag,            &
     &    cps_surf_comm%ntot_import, cps_surf_comm%item_import,         &
     &    cps_surf_comm%ntot_export, cps_surf_comm%item_export)
      call append_ele_communication_table                               &
     &   (comm_tmp2, cps_surf_comm, comm_tmp3)
      call dealloc_calypso_comm_table(cps_surf_comm)
      call dealloc_comm_table(comm_tmp2)
!
      call dup_comm_tbl_to_calypso_comm(my_rank, nprocs,                &
     &    mesh_l%ele%numele, ele_comm_l, cps_ele_comm)
      call convert_comm_item_q2l                                        &
     &   (mesh_l%ele%numele, l_to_lag%iele_linear_to_lag,               &
     &    cps_ele_comm%ntot_import, cps_ele_comm%item_import,           &
     &    cps_ele_comm%ntot_export, cps_ele_comm%item_export)
      call append_ele_communication_table                               &
     &   (comm_tmp3, cps_ele_comm, nod_comm_lag)
      call dealloc_calypso_comm_table(cps_ele_comm)
      call dealloc_comm_table(comm_tmp3)
!
      end subroutine const_node_comm_table_l2lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine convert_comm_item_q2l(num_list, idx_old_to_newnod,     &
     &          ntot_import, item_import, ntot_export, item_export)
!
      integer(kind = kint), intent(in) :: num_list
      integer(kind = kint), intent(in) :: idx_old_to_newnod(num_list)
      integer(kind = kint), intent(in) :: ntot_import, ntot_export
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
      integer(kind = kint), intent(inout) :: item_export(ntot_export)
!
      integer(kind = kint) :: i, inum
!
!$omp parallel do private(i,inum)
      do inum = 1, ntot_import
        i = item_import(inum)
        item_import(inum) = idx_old_to_newnod(i)
      end do
!$omp end parallel do
!$omp parallel do private(i,inum)
      do inum = 1, ntot_export
        i = item_export(inum)
        item_export(inum) = idx_old_to_newnod(i)
      end do
!$omp end parallel do
!
      end subroutine convert_comm_item_q2l
!
!-----------------------------------------------------------------------
!
      end module const_node_comm_table_q2l
