!analyzer_comm_test.f90
!      module analyzer_comm_test
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_communication_test
!      subroutine analyze_communication_test
!
      module analyzer_comm_test
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
!
      use t_solver_SR
      use t_ctl_data_comm_test
      use t_mesh_data
      use t_belonged_element_4_node
      use t_file_IO_parameter
      use t_control_param_comm_test
      use t_vector_for_solver
      use t_failed_export_list
      use const_element_comm_table
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &      :: comm_test_name = 'comm_test.dat'
!
      type(comm_test_control), save ::  comm_tctl1
      type(comm_test_files_param), save ::  T_files
      type(mesh_data), save :: test_fem
!
      type(communication_table), save :: T_ele_comm
      type(communication_table), save :: T_surf_comm
      type(communication_table), save :: T_edge_comm
!
!>      Structure for communicatiors for solver
      type(vectors_4_solver), save :: v_sol_T
!
      character(len=kchara), parameter :: txt_edge = 'edge'
      character(len=kchara), parameter :: txt_surf = 'surface'
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_communication_test
!
      use parallel_FEM_mesh_init
      use mesh_send_recv_test
      use nod_phys_send_recv
      use mpi_load_mesh_data
      use const_element_comm_tables
      use const_element_comm_table
!
      type(failed_table), save :: fail_tbl_s, fail_tbl_d
      integer :: i, num_in, num_ex
!
      if(my_rank .eq. 0)  write(*,*) 'check commnication tables start'
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
      call elapsed_label_4_ele_comm_tbl
      call elpsed_label_4_comm_test
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 's_input_control_comm_test'
      call s_input_control_comm_test(comm_tctl1, T_files)
!
!  --  read geometry
!
      if(my_rank .eq. 0) iflag_debug = 1
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(T_files%mesh_file_IO, nprocs, test_fem)
!
      do i = 1, test_fem%mesh%nod_comm%num_neib
        num_in = test_fem%mesh%nod_comm%istack_import(i) &
     &          - test_fem%mesh%nod_comm%istack_import(i-1)
        num_ex = test_fem%mesh%nod_comm%istack_export(i) &
     &          - test_fem%mesh%nod_comm%istack_export(i-1)
!        write(50+my_rank,*) 'id_neib:',                                &
!     &      test_fem%mesh%nod_comm%id_neib(i), num_in, num_ex
      end do
!      close(50+my_rank)
!
!  -------------------------------------------
!
      call FEM_comm_initialization(test_fem%mesh, v_sol_T)
      call FEM_mesh_initialization(test_fem%mesh, test_fem%group)
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_tbl'
      call const_ele_comm_tbl                                           &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm,                   &
     &    T_ele_comm, test_fem%mesh%ele)
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table2'
      call alloc_failed_export(0, fail_tbl_s)
      call const_surf_comm_table2                                      &
     &   (test_fem%mesh%node, test_fem%mesh%ele,                       &
     &    test_fem%mesh%nod_comm, T_ele_comm, T_surf_comm,             &
     &    test_fem%mesh%surf, fail_tbl_s)
!
      write(*,*) 'fail_tbl_s', fail_tbl_s%num_fail
      call dealloc_failed_export(fail_tbl_s)
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table'
      call alloc_failed_export(0, fail_tbl_d)
      call const_edge_comm_table2                                      &
     &   (test_fem%mesh%node, test_fem%mesh%ele,                       &
     &    test_fem%mesh%nod_comm, T_ele_comm, T_edge_comm,             &
     &    test_fem%mesh%edge, fail_tbl_d)
      call dealloc_failed_export(fail_tbl_d)
!
      call calypso_mpi_barrier
      call calypso_mpi_abort(0, 'manuke')
!
      end subroutine initialize_communication_test
!
! ----------------------------------------------------------------------
!
      subroutine analyze_communication_test
!
      use t_vector_for_solver
      use t_work_for_comm_check
!
      use calypso_mpi
      use mesh_send_recv_test
      use diff_geometory_comm_test
      use write_diff_4_comm_test
      use mesh_send_recv_check
!
      use set_ele_id_4_node_type
      use const_element_comm_tables
!
      integer(kind = kint), parameter :: N12 = 12
!
      type(work_for_comm_check), save :: nod_check
      type(work_for_comm_check), save :: ele_check
      type(work_for_comm_check), save :: surf_check
      type(work_for_comm_check), save :: edge_check
!
!
      call calypso_mpi_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'node_send_recv_test'
      call node_send_recv_test                                          &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm, nod_check)
      call ele_send_recv_test(test_fem%mesh%node, test_fem%mesh%ele,    &
     &    T_ele_comm, ele_check)
      call surf_send_recv_test(test_fem%mesh%node, test_fem%mesh%surf,  &
     &    T_surf_comm, surf_check)
      call edge_send_recv_test(test_fem%mesh%node, test_fem%mesh%edge,  &
     &    T_edge_comm, edge_check)
!
      call output_diff_mesh_comm_test(comm_test_name,                   &
     &    nod_check, ele_check, surf_check, edge_check)
!
      call dealloc_ele_comm_test_IO(nod_check)
      call dealloc_ele_comm_test_IO(ele_check)
      call dealloc_ele_comm_test_IO(surf_check)
      call dealloc_ele_comm_test_IO(edge_check)
!
!
      if (iflag_debug.gt.0) write(*,*) 'node_send_recv4_test'
      call alloc_iccg_int8_vector(test_fem%mesh%node%numnod, v_sol_T)
      call verify_iccgN_vec_type                                        &
     &   (N12, test_fem%mesh%node%numnod, v_sol_T)
      call node_send_recv4_test                                         &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm, N12, v_sol_T)
!
      call dealloc_iccgN_vector(v_sol_T)
      call dealloc_iccg_int8_vector(v_sol_T)
!
      call output_elapsed_times
!
      end subroutine analyze_communication_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_surf_comm_table2                                 &
     &         (node, ele, nod_comm, ele_comm,                          &
     &          surf_comm, surf, fail_tbl)
!
      use m_geometry_constants
      use set_ele_id_4_node_type
      use const_element_comm_table
      use const_element_comm_tables
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
      type(communication_table), intent(inout) :: surf_comm
      type(surface_data), intent(inout) :: surf
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
!
      integer(kind = kint), allocatable :: inod_dbl(:,:)
      integer(kind = kint), allocatable :: iele_dbl(:,:)
!
      integer(kind = kint), allocatable :: nnod_same(:)
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: k_ref(:)
!
      integer(kind = kint) :: isurf, i1, i2, i, inod, ip
!
!
      allocate(inod_dbl(node%numnod,2))
      allocate(iele_dbl(0:ele%numele,2))
      call set_node_ele_double_address(node, ele, nod_comm, ele_comm,   &
     &                                 inod_dbl, iele_dbl)
!
      allocate(nnod_same(surf%numsurf))
      allocate(ip_ref(surf%numsurf))
      allocate(k_ref(surf%numsurf))
!$omp parallel workshare
      nnod_same(1:surf%numsurf) = 0
      ip_ref(1:surf%numsurf) =   -1
      k_ref(1:surf%numsurf) =     0
!$omp end parallel workshare
!
!%omp parallel do private(isurf)
      do isurf = 1, surf%numsurf
       call find_belonged_pe_4_surf(surf, node%numnod, inod_dbl(1,2),   &
     &     isurf, nnod_same(isurf), ip_ref(isurf), k_ref(isurf))
      end do
!%omp end parallel do
!
      i1 = 0
      i2 = 0
      do isurf = 1, surf%numsurf
        if(k_ref(isurf) .ne. 1) then
          i1 = i1 + 1
          if(surf%iele_4_surf(isurf,2,1) .eq. 0) i2 = i2 + 1
        end if
      end do
      write(*,*) my_rank, 'Need to flip', i1, i2
!
!
      call set_surf_id_4_node(node, surf, belongs%blng_surf)
      call alloc_x_ref_surf(node, belongs)
      call sort_inod_4_ele_by_position(ione, surf%numsurf, surf%x_surf, &
     &    node, belongs%blng_surf, belongs%x_ref_surf)
!
      call belonged_surf_id_4_node2                                     &
     &   (ione, node, surf, belongs%host_surf)
      deallocate(nnod_same, ip_ref, k_ref)
!
      call const_comm_table_by_connenct2                                &
     &   (txt_surf, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &    surf%interior_surf, surf%x_surf, node, nod_comm,              &
     &    belongs%blng_surf, belongs%x_ref_surf, belongs%host_surf,     &
     &    surf_comm, inod_dbl, fail_tbl)
      deallocate(inod_dbl, iele_dbl)
!
      call dealloc_iele_belonged(belongs%host_surf)
      call dealloc_x_ref_surf(belongs)
      call dealloc_iele_belonged(belongs%blng_surf)
!
      call const_global_surface_id(surf_comm, surf)
!
      end subroutine const_surf_comm_table2
!
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table2                                 &
     &         (node, ele, nod_comm, ele_comm,                          &
     &         edge_comm, edge, fail_tbl)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
      use const_element_comm_tables
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
!
      type(communication_table), intent(inout) :: edge_comm
      type(edge_data), intent(inout) :: edge
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
!
      integer(kind = kint), allocatable :: inod_dbl(:,:)
      integer(kind = kint), allocatable :: iele_dbl(:,:)
!
      integer(kind = kint), allocatable :: nnod_same(:)
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: k_ref(:)
!
      integer(kind = kint) :: iedge
!
!
      allocate(inod_dbl(node%numnod,2))
      allocate(iele_dbl(0:ele%numele,2))
      call set_node_ele_double_address(node, ele, nod_comm, ele_comm,   &
     &                                 inod_dbl, iele_dbl)
!
      allocate(nnod_same(edge%numedge))
      allocate(ip_ref(edge%numedge))
      allocate(k_ref(edge%numedge))
!$omp parallel workshare
      nnod_same(1:edge%numedge) = 0
      ip_ref(1:edge%numedge) =   -1
      k_ref(1:edge%numedge) =     0
!$omp end parallel workshare
!
!%omp parallel do private(isurf)
      do iedge = 1, edge%numedge
       call find_belonged_pe_4_edge(edge, node%numnod, inod_dbl(1,2),   &
     &     iedge, nnod_same(iedge), ip_ref(iedge), k_ref(iedge))
      end do
!%omp end parallel do
      deallocate(nnod_same, ip_ref, k_ref)
      deallocate(inod_dbl, iele_dbl)
!
!
!
      if(iflag_debug.gt.0) write(*,*) ' set_edge_id_4_node in edge'
      call set_edge_id_4_node(node, edge, belongs%blng_edge)
      call alloc_x_ref_edge(node, belongs)
      call sort_inod_4_ele_by_position(ione, edge%numedge, edge%x_edge, &
     &    node, belongs%blng_edge, belongs%x_ref_edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' belonged_edge_id_4_node2 in edge'
      call belonged_edge_id_4_node2                                     &
     &   (1, node, edge, belongs%host_edge)
      deallocate(nnod_same, ip_ref, k_ref)
      deallocate(inod_dbl, iele_dbl)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' const_comm_table_by_connenct in edge'
      call const_comm_table_by_connenct                                 &
     &    (txt_edge, edge%numedge, edge%nnod_4_edge, edge%ie_edge,      &
     &    edge%interior_edge, edge%x_edge, node, nod_comm,              &
     &    belongs%blng_edge, belongs%x_ref_edge, belongs%host_edge,     &
     &    edge_comm, fail_tbl)
!
      call dealloc_iele_belonged(belongs%host_edge)
      call dealloc_x_ref_edge(belongs)
      call dealloc_iele_belonged(belongs%blng_edge)
!
      call const_global_edge_id(edge_comm, edge)
!
      end subroutine const_edge_comm_table2
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------!
      subroutine set_node_ele_double_address                            &
     &         (node, ele, nod_comm, ele_comm, inod_dbl, iele_dbl)
!
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
!
      integer(kind = kint), intent(inout) :: inod_dbl(node%numnod,2)
      integer(kind = kint), intent(inout) :: iele_dbl(0:ele%numele,2)
!
      integer(kind = kint) :: i
!
!
!$onp parallel do
      do i = 1, node%numnod
        inod_dbl(i,1) = i
        inod_dbl(i,2) = my_rank
      end do
!$onp end parallel do
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, inod_dbl(1,1))
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, inod_dbl(1,2))
!
      iele_dbl(0,1) = 0
      iele_dbl(0,2) = -1
!$onp parallel do
      do i = 1, ele%numele
        iele_dbl(i,1) = i
        iele_dbl(i,2) = inod_dbl(ele%ie(i,1),2)
      end do
!$onp end parallel do
      call SOLVER_SEND_RECV_int_type                                    &
     &   (ele%numele, ele_comm, iele_dbl(1,1))
!
      end subroutine set_node_ele_double_address
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------!
       subroutine find_belonged_pe_4_surf(surf, numnod, ip_node,        &
      &          isurf, nnod_same, ip_ref, k_ref)
!
       type(surface_data), intent(in) :: surf
       integer(kind = kint), intent(in) :: numnod
       integer(kind = kint), intent(in) :: ip_node(numnod)
       integer(kind = kint), intent(in) :: isurf
!
       integer(kind = kint), intent(inout) :: nnod_same
       integer(kind = kint), intent(inout) :: ip_ref
       integer(kind = kint), intent(inout) :: k_ref
!
       integer(kind = kint) :: ip1, ip2, ip3, ip4
!
!
       ip1 = ip_node(surf%ie_surf(isurf,1))
       ip2 = ip_node(surf%ie_surf(isurf,2))
       ip3 = ip_node(surf%ie_surf(isurf,3))
       ip4 = ip_node(surf%ie_surf(isurf,4))
       if(ip1.eq.ip2 .and. ip1.eq.ip3 .and. ip1.eq.ip4) then
         nnod_same = 4
         ip_ref = ip1
         k_ref = 1
       else if(ip2.eq.ip3 .and. ip2.eq.ip4) then
         nnod_same = 3
         ip_ref = ip2
         k_ref = 2
       else if(ip1.eq.ip3 .and. ip1.eq.ip4) then
         nnod_same = 3
         ip_ref = ip1
         k_ref = 1
       else if(ip1.eq.ip2 .and. ip1.eq.ip4) then
         nnod_same = 3
         ip_ref = ip1
         k_ref = 1
       else if(ip1.eq.ip2 .and. ip1.eq.ip3) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
!
       else if(ip1.eq.ip2 .and. ip3.eq.ip4) then
         nnod_same = 2
         ip_ref = min(ip1, ip3)
         k_ref = 1
         if(ip_ref .eq. ip4) k_ref = 3
       else if(ip2.eq.ip3 .and. ip4.eq.ip1) then
         nnod_same = 2
         ip_ref = min(ip1, ip2)
         k_ref = 1
         if(ip_ref .eq. ip2) k_ref = 2
       else if(ip1.eq.ip2) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else if(ip2.eq.ip3) then
         nnod_same = 2
         ip_ref = ip2
         k_ref = 2
       else if(ip3.eq.ip4) then
         nnod_same = 2
         ip_ref = ip3
         k_ref = 3
       else if(ip4.eq.ip1) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else if(ip1.eq.ip3) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else if(ip2.eq.ip4) then
         nnod_same = 2
         ip_ref = ip2
         k_ref = 2
       else
         nnod_same = 1
         ip_ref = min(ip1, ip2)
         ip_ref = min(ip3, ip_ref)
         ip_ref = min(ip4, ip_ref)
         k_ref = 1
         if(ip_ref .eq. ip2) k_ref = 2
         if(ip_ref .eq. ip3) k_ref = 3
         if(ip_ref .eq. ip4) k_ref = 4
       end if
!
       end subroutine find_belonged_pe_4_surf
!
! ----------------------------------------------------------------------
!
       subroutine find_belonged_pe_4_edge(edge, numnod, ip_node,        &
      &          iedge, nnod_same, ip_ref, k_ref)
!
       type(edge_data), intent(in) :: edge
       integer(kind = kint), intent(in) :: numnod
       integer(kind = kint), intent(in) :: ip_node(numnod)
       integer(kind = kint), intent(in) :: iedge
!
       integer(kind = kint), intent(inout) :: nnod_same
       integer(kind = kint), intent(inout) :: ip_ref
       integer(kind = kint), intent(inout) :: k_ref
!
       integer(kind = kint) ::ip1, ip2
!
       ip1 = ip_node(edge%ie_edge(iedge,1))
       ip2 = ip_node(edge%ie_edge(iedge,2))
!
       if(ip1.eq.ip2) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else
         nnod_same = 1
         ip_ref = min(ip1, ip2)
         k_ref = 1
         if(ip_ref .eq. ip2) k_ref = 2
       end if
!
       end subroutine find_belonged_pe_4_edge
!
! ----------------------------------------------------------------------
!
      subroutine belonged_surf_id_4_node2                               &
     &         (k_ref, node, surf, host_surf)
!
      use t_geometry_data
      use t_surface_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      integer (kind=kint), intent(in) :: k_ref
      type(node_data), intent(in) ::    node
      type(surface_data), intent(in) :: surf
      type(element_around_node), intent(inout) :: host_surf
!
!
      call alloc_numele_belonged(node%numnod, host_surf)
!
      call count_belonged_ele_4_node2                                   &
     &   (k_ref, node%numnod, surf%numsurf, surf%ie_surf(1,1),          &
     &    ione, surf%numsurf, host_surf%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    host_surf%nele_4_node, izero, host_surf%istack_4_node,        &
     &    host_surf%ntot, host_surf%nmax, host_surf%nmin)
!
!
      call alloc_iele_belonged(host_surf)
!
      call set_belonged_ele_4_node2                                     &
     &   (k_ref, node%numnod, surf%numsurf, surf%ie_surf(1,1),          &
     &    ione, surf%numsurf, host_surf%ntot, host_surf%istack_4_node,  &
     &    host_surf%nele_4_node, host_surf%iele_4_node,                 &
     &    host_surf%iconn_4_node)
!
      end subroutine belonged_surf_id_4_node2
!
!-----------------------------------------------------------------------
!
      subroutine belonged_edge_id_4_node2                               &
     &         (k_ref, node, edge, host_edge)
!
      use t_geometry_data
      use t_edge_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      integer (kind=kint), intent(in) :: k_ref
      type(node_data), intent(in) ::    node
      type(edge_data), intent(in) ::    edge
      type(element_around_node), intent(inout) :: host_edge
!
!
      call alloc_numele_belonged(node%numnod, host_edge)
!
      call count_belonged_ele_4_node2                                   &
     &   (k_ref, node%numnod, edge%numedge, edge%ie_edge(1,1),          &
     &    ione, edge%numedge, host_edge%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    host_edge%nele_4_node, izero, host_edge%istack_4_node,        &
     &    host_edge%ntot, host_edge%nmax, host_edge%nmin)
!
      call alloc_iele_belonged(host_edge)
!
      call set_belonged_ele_4_node2                                     &
     &   (k_ref, node%numnod, edge%numedge, edge%ie_edge(1,1),          &
     &    ione, edge%numedge, host_edge%ntot, host_edge%istack_4_node,  &
     &    host_edge%nele_4_node, host_edge%iele_4_node,                 &
     &    host_edge%iconn_4_node)
!
      end subroutine belonged_edge_id_4_node2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_belonged_ele_4_node2(k_ref, numnod, numele, ie,  &
     &          iele_st, iele_ed, nele_4_node)
!
      integer (kind=kint), intent(in) :: k_ref
      integer (kind=kint), intent(in) :: numnod, numele
      integer (kind=kint), intent(in) :: ie(numele,1)
      integer (kind=kint), intent(in) :: iele_st, iele_ed
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
!
      integer (kind = kint) :: inod, iele
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        inod = ie(iele,k_ref)
        nele_4_node(inod) = nele_4_node(inod) + 1
      end do
!
      end  subroutine count_belonged_ele_4_node2
!
! -----------------------------------------------------------------------
!
      subroutine set_belonged_ele_4_node2(k_ref, numnod, numele, ie,    &
     &          iele_st, iele_ed, ntot_ele_4_node, iele_stack_4_node,   &
     &          nele_4_node, iele_4_node, iconn_4_node)
!
      use quicksort
!
      integer (kind=kint), intent(in) :: k_ref
      integer (kind=kint), intent(in) :: numnod, numele
      integer (kind=kint), intent(in) :: ie(numele,1)
      integer (kind=kint), intent(in) :: iele_st, iele_ed
      integer (kind=kint), intent(in) :: ntot_ele_4_node
      integer (kind=kint), intent(in) :: iele_stack_4_node(0:numnod)
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
      integer (kind=kint), intent(inout)                                &
     &                    :: iele_4_node(ntot_ele_4_node)
      integer (kind=kint), intent(inout)                                &
     &                    :: iconn_4_node(ntot_ele_4_node)
!
      integer (kind = kint) :: inod, iele, icou, ist
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        inod = ie(iele,k_ref)
        nele_4_node(inod) = nele_4_node(inod) + 1
        icou = iele_stack_4_node(inod-1) + nele_4_node(inod)
        iele_4_node(icou) = iele
        iconn_4_node(icou) =  1
      end do
!
!$omp parallel do private(inod,ist)
      do inod = 1, numnod
        ist = iele_stack_4_node(inod-1) + 1
        if(nele_4_node(inod) .gt. 0) then
          call quicksort_w_index(nele_4_node(inod), iele_4_node(ist),   &
     &        ione, nele_4_node(inod), iconn_4_node(ist))
        end if
      end do
!$omp end parallel do
!
      end  subroutine set_belonged_ele_4_node2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_by_connenct2                          &
     &         (txt, numele, nnod_4_ele, ie, internal_flag, x_ele,      &
     &          node, nod_comm, neib_e, x_ref_ele, host,                &
     &          e_comm, inod_dbl, fail_tbl)
!
      use m_solver_SR
      use reverse_SR_int
      use find_element_comm_table
      use const_global_element_ids
      use set_element_export_item
      use make_element_comm_table_SR
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      type(node_data), intent(in) :: node
      type(element_around_node), intent(in) :: host
      type(element_around_node), intent(in) :: neib_e
      type(communication_table), intent(in) :: nod_comm
      real(kind = kreal), intent(in)                                    &
     &           :: x_ref_ele(neib_e%istack_4_node(node%numnod))
      integer(kind = kint), intent(in) :: inod_dbl(node%numnod,2)
!
      type(communication_table), intent(inout) :: e_comm
      type(failed_table), intent(inout) :: fail_tbl
!
      type(work_4_ele_comm_table) :: wk_comm
      integer(kind = kint), allocatable :: inod_lc_import(:,:)
      integer(kind = kint), allocatable :: ipe_lc_import(:,:)
      integer(kind = kint), allocatable :: inod_lc_export(:,:)
      integer(kind = kint), allocatable :: ipe_lc_export(:,:)
!
      integer :: i, j, ip_org, ii, inod, ip, i1, isurf
!
      do inod = 1, node%numnod
        if(inod_dbl(inod,2) .eq. 7   &
     &      .and. inod_dbl(inod,1) .eq. 77318) then
          write(*,*) 'Hit', my_rank, inod
!
          ip = -1
          do i = 1, nod_comm%num_neib
            do i1 = nod_comm%istack_import(i-1)+1, nod_comm%istack_import(i)
              if(nod_comm%item_import(i1) .eq. inod) ip = i
            end do
        end do
!
        do i = host%istack_4_node(inod-1)+1, &
     &         host%istack_4_node(inod)
          isurf = host%iele_4_node(i)
          write(*,*) 'Detail/w/pe', my_rank, inod, i,  &
     &           ip, nod_comm%id_neib(ip), &
     &           inod_dbl(ie(isurf,1:4),1),  &
     &           inod_dbl(ie(isurf,1:4),2)
        end do
        end if
      end do
!
!
      e_comm%num_neib = nod_comm%num_neib
      call alloc_neighbouring_id(e_comm)
      call alloc_import_num(e_comm)
!
!      write(*,*) 'count_element_import_num', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+1)
      call count_element_import_num(node%numnod, host%istack_4_node,    &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+1)
!
      call alloc_element_rev_imports(node%numnod,                       &
     &    nod_comm%ntot_export, e_comm%ntot_import, wk_comm)
      call alloc_import_item(e_comm)
!
!      write(*,*) 'local_node_id_reverse_SR', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+2)
      call local_node_id_reverse_SR                                     &
     &   (node%numnod, nod_comm%num_neib, nod_comm%id_neib,             &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    SR_sig1, wk_comm%item_local, wk_comm%inod_local)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+2)
!
      allocate(inod_lc_import(e_comm%ntot_import,nnod_4_ele))
      allocate(ipe_lc_import(e_comm%ntot_import,nnod_4_ele))
!      write(*,*) 'set_element_import_item2', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+3)
      call set_element_import_item2(node%numnod, node%internal_node,    &
     &    numele, nnod_4_ele, ie, node%inod_global, x_ele,              &
     &    host%istack_4_node, host%iele_4_node, wk_comm%inod_local, inod_dbl, &
     &    nod_comm%num_neib, nod_comm%istack_import,                    &
     &    nod_comm%item_import, e_comm%num_neib,                        &
     &    e_comm%istack_import, e_comm%item_import,                     &
     &    wk_comm%inod_import_e, wk_comm%inod_import_l,                 &
     &    inod_lc_import, ipe_lc_import, wk_comm%xe_import)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+3)
!
      call alloc_export_num(e_comm)
!
!      write(*,*) 'element_num_reverse_SR', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+4)
      call element_num_reverse_SR                                       &
     &   (e_comm%num_neib, e_comm%id_neib, e_comm%num_import, SR_sig1,  &
     &    e_comm%num_export, e_comm%istack_export, e_comm%ntot_export)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+4)
!
      call alloc_element_rev_exports(e_comm%ntot_export, wk_comm)
      call alloc_export_item(e_comm)
!
      allocate(inod_lc_export(e_comm%ntot_export,nnod_4_ele))
      allocate(ipe_lc_export(e_comm%ntot_export,nnod_4_ele))
!
!      write(*,*) 'element_data_reverse_SR2', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+5)
      call element_data_reverse_SR2(nnod_4_ele, e_comm%num_neib, e_comm%id_neib,    &
     &    e_comm%istack_import, e_comm%istack_export,                   &
     &    wk_comm%inod_import_e, wk_comm%inod_import_l,                 &
     &    inod_lc_import, ipe_lc_import,                   &
     &    wk_comm%xe_import, wk_comm%inod_export_e,                     &
     &    wk_comm%inod_export_l, inod_lc_export, ipe_lc_export,   &
     &    wk_comm%xe_export)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+5)
      deallocate(inod_lc_import, ipe_lc_import)
!
!      write(*,*) 'set_element_export_item', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+6)
      call s_set_element_export_item(txt, node%numnod, numele,          &
     &    internal_flag, x_ele, neib_e%istack_4_node,                   &
     &    neib_e%iele_4_node, x_ref_ele, nod_comm%num_neib,             &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%istack_export,                        &
     &    wk_comm%inod_export_l, wk_comm%xe_export, e_comm%item_export)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+6)
!
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+7)
      call element_export_item_in_ext                                   &
     &   (txt, node%numnod, numele, node%inod_global,                   &
     &    internal_flag, x_ele, neib_e%istack_4_node,                   &
     &    neib_e%iele_4_node, x_ref_ele, nod_comm%num_neib,             &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    e_comm%num_neib, e_comm%istack_export,                        &
     &    wk_comm%inod_export_e, wk_comm%xe_export, e_comm%item_export, &
     &    fail_tbl)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+7)
!
      if(fail_tbl%num_fail .gt. 0) then
        write(80+my_rank,*) 'Conunt, inum, item_export_e, dist'
        do i = 1, fail_tbl%num_fail
          j  = fail_tbl%fail_comm(i)%id_failed
!  
          do ii = 1, e_comm%num_neib
            if(j.gt.e_comm%istack_export(ii-1)    &
     &          .and. j.le.e_comm%istack_export(ii))  &
     &             ip_org = e_comm%id_neib(ii)
          end do
!
          write(80+my_rank,*) i, ip_org, fail_tbl%fail_comm(i)%id_failed,   &
     &     fail_tbl%fail_comm(i)%item_fail,    &
     &     fail_tbl%fail_comm(i)%dist_fail,    &
     &     'inod_lc_export', inod_lc_export(j,1:4), &
     &     'ipe_lc_export', ipe_lc_export(j,1:4)
!
          if(my_rank .eq. ipe_lc_export(j,1)) then
            write(80+my_rank,*) 'Hit self surface'
          end if
!
        end do
        close(80+my_rank)
      end if  
!
      deallocate(inod_lc_export, ipe_lc_export)
!
      call calypso_mpi_barrier
      call calypso_mpi_abort(0, 'tako')
!
      call dealloc_element_rev_exports(wk_comm)
      call dealloc_element_rev_imports(wk_comm)
!
!      write(*,*) 'check_element_position', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+8)
      call check_element_position(txt, numele, x_ele, e_comm)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+8)
!
      end subroutine const_comm_table_by_connenct2
!
!-----------------------------------------------------------------------
!
      subroutine flip_surface_connenct(isurf, node, ele, surf)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: isurf
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(surface_data), intent(inout) :: surf
!
      integer :: k
      integer(kind = kint) :: iele1, iele2, k_lc1, k_lc2
      integer(kind = kint) :: ie_surf_new(surf%nnod_4_surf)
!
!
!
      iele1 = surf%iele_4_surf(isurf,1,1)
      iele2 = surf%iele_4_surf(isurf,2,1)
      k_lc1 = surf%iele_4_surf(isurf,1,2)
      k_lc2 = surf%iele_4_surf(isurf,2,2)
      if(iele2 .le. 0) return
!
      if(surf%nnod_4_surf .eq. num_quad_sf) then
        do k = 1, num_quad_sf
          ie_surf_new(k) = ele%ie(iele2,node_on_sf_8(k,k_lc2))
        end do
      else if(surf%nnod_4_surf .eq. num_lag_sf) then
        do k = 1, num_lag_sf
          ie_surf_new(k) = ele%ie(iele2,node_on_sf_9(k,k_lc2))
        end do
      else
        do k = 1, num_linear_sf
          ie_surf_new(k) = ele%ie(iele2,node_on_sf_4(k,k_lc2))
        end do
        end if
!
      surf%ie_surf(isurf,1:surf%nnod_4_surf)                            &
     &      = ie_surf_new(1:surf%nnod_4_surf)
      surf%isf_4_ele(iele1,k_lc1) = -isurf
      surf%isf_4_ele(iele2,k_lc2) =  isurf
!
      surf%iele_4_surf(isurf,1,1) = iele2
      surf%iele_4_surf(isurf,2,1) = iele1
      surf%iele_4_surf(isurf,1,2) = k_lc2
      surf%iele_4_surf(isurf,2,2) = k_lc1
!
      if(surf%ie_surf(isurf,1) .le. node%internal_node) then
        surf%interior_surf(isurf) = 1
      else
        surf%interior_surf(isurf) = 0
      end if
!
      end subroutine flip_surface_connenct
!
!-----------------------------------------------------------------------
!
      subroutine tmp_surface_connent(numsurf, nnod_4_surf, ie_surf,     &
     &                               k_ref, ie_sf_tmp)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: k_ref(numsurf)
!
      integer(kind = kint), intent(inout)                               &
     &       :: ie_sf_tmp(numsurf,num_linear_sf)
!
      integer(kind = kint) :: isurf
!
!
!$omp parallel do
      do isurf = 1, numsurf
        if(k_ref(isurf) .eq. 2) then
          ie_sf_tmp(isurf,1) = ie_surf(isurf,2)
          ie_sf_tmp(isurf,2) = ie_surf(isurf,3)
          ie_sf_tmp(isurf,3) = ie_surf(isurf,4)
          ie_sf_tmp(isurf,4) = ie_surf(isurf,1)
        else if(k_ref(isurf) .eq. 2) then
          ie_sf_tmp(isurf,1) = ie_surf(isurf,3)
          ie_sf_tmp(isurf,2) = ie_surf(isurf,4)
          ie_sf_tmp(isurf,3) = ie_surf(isurf,1)
          ie_sf_tmp(isurf,4) = ie_surf(isurf,2)
        else
          ie_sf_tmp(isurf,1) = ie_surf(isurf,1)
          ie_sf_tmp(isurf,2) = ie_surf(isurf,2)
          ie_sf_tmp(isurf,3) = ie_surf(isurf,3)
          ie_sf_tmp(isurf,4) = ie_surf(isurf,4)
        end if
      end do
!$omp end parallel do
!
      end subroutine tmp_surface_connent
!
!-----------------------------------------------------------------------
!
      subroutine set_element_import_item2                               &
     &         (numnod, internal_node, numele, nnod_4_ele, ie,          &
     &          inod_global, x_ele, iele_stack_ht_node, iele_ht_node,   &
     &          inod_local, inod_dbl, num_neib, istack_import, item_import,       &
     &          num_neib_e, istack_import_e, item_import_e,             &
     &          inod_import_e, inod_import_l,               &
     &          inod_lc_import, ipe_lc_import, xe_import)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
      integer(kind = kint), intent(in) :: iele_stack_ht_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &        :: iele_ht_node(iele_stack_ht_node(numnod))
      integer(kind = kint), intent(in) :: inod_local(numnod)
      integer(kind = kint), intent(in) :: inod_dbl(numnod,2)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &              :: item_import(istack_import(num_neib))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_import_e(istack_import_e(num_neib_e))
      integer(kind = kint_gl), intent(inout)                            &
     &        :: inod_import_e(istack_import_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_import_l(istack_import_e(num_neib_e))
      real(kind = kreal), intent(inout)                                 &
     &        :: xe_import(3*istack_import_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &        :: ipe_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint) :: ip, icou
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, num, jnum, jele
      integer(kind = kint) :: k1, jnod, minimum, nele
!
!
      do ip = 1, num_neib
        ist = istack_import(ip-1) + 1
        ied = istack_import(ip)
        icou = istack_import_e(ip-1)
        do inum = ist, ied
          inod = item_import(inum)
          jst = iele_stack_ht_node(inod-1)
          num = iele_stack_ht_node(inod  ) - jst
          do jnum = 1, num
            icou = icou + 1
            jele = iele_ht_node(jst+jnum)
            item_import_e(icou) = jele
!
            inod_import_e(icou) = inod_global(inod)
            inod_import_l(icou) = 0
            xe_import(3*icou-2) = x_ele(jele,1)
            xe_import(3*icou-1) = x_ele(jele,2)
            xe_import(3*icou  ) = x_ele(jele,3)
!
            minimum = num
            do k1 = 1, nnod_4_ele
              jnod = ie(jele,k1)
!
              inod_lc_import(icou,k1) = inod_dbl(jnod,1)
              ipe_lc_import(icou,k1) =  inod_dbl(jnod,2)
!
              if(jnod .gt. internal_node) cycle
              nele = iele_stack_ht_node(jnod)                           &
     &              - iele_stack_ht_node(jnod-1)
              if(nele .lt. minimum) then
                minimum = nele
                inod_import_l(icou) = inod_local(jnod)
              end if
            end do
!
          end do
        end do
      end do
!
      end subroutine  set_element_import_item2
!
!-----------------------------------------------------------------------
!
      subroutine element_data_reverse_SR2(nnod_4_ele, num_neib_e, id_neib_e,        &
     &          istack_import_e, istack_export_e,                       &
     &          inod_import_e, inod_import_l, &
     &          inod_lc_import, ipe_lc_import, xe_import,               &
     &          inod_export_e, inod_export_l,    &
     &          inod_lc_export, ipe_lc_export,xe_export)
!
      use m_solver_SR
      use reverse_SR_real
      use reverse_SR_int
      use reverse_SR_int8
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
!
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
      integer(kind = kint), intent(in) :: nnod_4_ele
!
      integer(kind = kint_gl), intent(in)                               &
     &         :: inod_import_e(istack_import_e(num_neib_e))
      integer(kind = kint), intent(in)                                  &
     &         :: inod_import_l(istack_import_e(num_neib_e))
      real(kind = kreal), intent(in)                                    &
     &         :: xe_import(3*istack_import_e(num_neib_e))
      integer(kind = kint), intent(in)                                  &
     &        :: inod_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &        :: ipe_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint_gl), intent(inout)                            &
     &         :: inod_export_e(istack_export_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &         :: inod_export_l(istack_export_e(num_neib_e))
      real(kind = kreal), intent(inout)                                 &
     &         :: xe_export(3*istack_export_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &        :: ipe_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint) :: ip, k1
!
!      do ip = 1, istack_import_e(num_neib_e)
!        write(*,*) ip, inod_import_e(ip), xe_import(3*ip-2:3*ip)
!      end do
!
!
      call reverse_send_recv_int8(num_neib_e, id_neib_e,                &
     &    istack_import_e, istack_export_e, inod_import_e,              &
     &    SR_sig1, inod_export_e)
!
      call reverse_send_recv_int(num_neib_e, id_neib_e,                 &
     &    istack_import_e, istack_export_e, inod_import_l,              &
     &    SR_sig1, inod_export_l)
!
      call reverse_send_recv_3(num_neib_e, id_neib_e,                   &
     &    istack_import_e, istack_export_e, xe_import,                  &
     &    SR_sig1, xe_export)
!
      do k1 = 1, nnod_4_ele
        call reverse_send_recv_int(num_neib_e, id_neib_e,               &
     &      istack_import_e, istack_export_e, inod_lc_import(1,k1),     &
     &      SR_sig1, inod_lc_export(1,k1))
        call reverse_send_recv_int(num_neib_e, id_neib_e,               &
     &      istack_import_e, istack_export_e, ipe_lc_import(1,k1),      &
     &      SR_sig1, ipe_lc_export(1,k1))
      end do
!
      end subroutine element_data_reverse_SR2
!
!-----------------------------------------------------------------------
!
      end module analyzer_comm_test
