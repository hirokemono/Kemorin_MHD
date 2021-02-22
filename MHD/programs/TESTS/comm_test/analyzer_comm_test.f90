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
      integer(kind = kint), allocatable :: inod_lc(:)
      integer(kind = kint), allocatable :: ip_node(:)
      integer(kind = kint), allocatable :: iele_lc(:)
      integer(kind = kint), allocatable :: ip_ele(:)
!
      integer(kind = kint), allocatable :: nnod_same(:)
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: k_ref(:)
!
      integer(kind = kint) :: isurf, i1, i2
!
!
      allocate(inod_lc(node%numnod))
      allocate(ip_node(node%numnod))
      allocate(iele_lc(0:ele%numele))
      allocate(ip_ele(0:ele%numele))
      call set_node_ele_double_address(node, ele, nod_comm, ele_comm,   &
     &    inod_lc, ip_node, iele_lc, ip_ele)
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
       call find_belonged_pe_4_surf(surf, node%numnod, ip_node,         &
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
      call calypso_mpi_barrier
      call calypso_mpi_abort(0, 'tako')
!
      deallocate(inod_lc, ip_node)
      deallocate(iele_lc, ip_ele)
!
      call set_surf_id_4_node(node, surf, belongs%blng_surf)
      call alloc_x_ref_surf(node, belongs)
      call sort_inod_4_ele_by_position(ione, surf%numsurf, surf%x_surf, &
     &    node, belongs%blng_surf, belongs%x_ref_surf)
!
      call belonged_surf_id_4_node                                      &
     &   (node, surf, belongs%host_surf)
      deallocate(nnod_same, ip_ref, k_ref)
!
      call const_comm_table_by_connenct                                 &
     &   (txt_surf, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &    surf%interior_surf, surf%x_surf, node, nod_comm,              &
     &    belongs%blng_surf, belongs%x_ref_surf, belongs%host_surf,     &
     &    surf_comm, fail_tbl)
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
      integer(kind = kint), allocatable :: inod_lc(:)
      integer(kind = kint), allocatable :: ip_node(:)
      integer(kind = kint), allocatable :: iele_lc(:)
      integer(kind = kint), allocatable :: ip_ele(:)
!
      integer(kind = kint), allocatable :: nnod_same(:)
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: k_ref(:)
!
      integer(kind = kint) :: iedge
!
!
      allocate(inod_lc(node%numnod))
      allocate(ip_node(node%numnod))
      allocate(iele_lc(0:ele%numele))
      allocate(ip_ele(0:ele%numele))
      call set_node_ele_double_address(node, ele, nod_comm, ele_comm,   &
     &    inod_lc, ip_node, iele_lc, ip_ele)
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
       call find_belonged_pe_4_edge(edge, node%numnod, ip_node,         &
     &     iedge, nnod_same(iedge), ip_ref(iedge), k_ref(iedge))
      end do
!%omp end parallel do
      deallocate(nnod_same, ip_ref, k_ref)
      deallocate(inod_lc, ip_node)
      deallocate(iele_lc, ip_ele)
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
      call belonged_edge_id_4_node                                      &
     &   (node, edge, belongs%host_edge)
      deallocate(nnod_same, ip_ref, k_ref)
      deallocate(inod_lc, ip_node)
      deallocate(iele_lc, ip_ele)
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
!
      subroutine belonged_surf_id_4_node2                               &
     &         (node, surf, k_ref, host_surf)
!
      use t_geometry_data
      use t_surface_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) ::    node
      type(surface_data), intent(in) :: surf
      integer (kind=kint), intent(in) :: k_ref(surf%numsurf)
      type(element_around_node), intent(inout) :: host_surf
!
!
      call alloc_numele_belonged(node%numnod, host_surf)
!
      call count_belonged_ele_4_node2                                   &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,    &
     &    k_ref, ione, surf%numsurf, host_surf%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    host_surf%nele_4_node, izero, host_surf%istack_4_node,        &
     &    host_surf%ntot, host_surf%nmax, host_surf%nmin)
!
!
      call alloc_iele_belonged(host_surf)
!
      call set_belonged_ele_4_node2                                     &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,    &
     &    k_ref, ione, surf%numsurf, host_surf%ntot,                    &
     &    host_surf%istack_4_node, host_surf%nele_4_node,               &
     &    host_surf%iele_4_node, host_surf%iconn_4_node)
!
      end subroutine belonged_surf_id_4_node2
!
!-----------------------------------------------------------------------
!
      subroutine belonged_edge_id_4_node2                               &
     &         (node, edge, k_ref, host_edge)
!
      use t_geometry_data
      use t_edge_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) ::    node
      type(edge_data), intent(in) ::    edge
      integer (kind=kint), intent(in) :: k_ref(edge%numedge)
      type(element_around_node), intent(inout) :: host_edge
!
!
      call alloc_numele_belonged(node%numnod, host_edge)
!
      call count_belonged_ele_4_node2                                   &
     &   (node%numnod, edge%numedge, edge%nnod_4_edge, edge%ie_edge,    &
     &    k_ref, ione, edge%numedge, host_edge%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    host_edge%nele_4_node, izero, host_edge%istack_4_node,        &
     &    host_edge%ntot, host_edge%nmax, host_edge%nmin)
!
!
      call alloc_iele_belonged(host_edge)
!
      call set_belonged_ele_4_node2                                     &
     &   (node%numnod, edge%numedge, edge%nnod_4_edge, edge%ie_edge,    &
     &    k_ref, ione, edge%numedge, host_edge%ntot,                    &
     &    host_edge%istack_4_node, host_edge%nele_4_node,               &
     &    host_edge%iele_4_node, host_edge%iconn_4_node)
!
      end subroutine belonged_edge_id_4_node2
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_belonged_ele_4_node2                             &
     &         (numnod, numele, nnod_4_ele, ie, k_ref,                  &
     &          iele_st, iele_ed, nele_4_node)
!
      integer (kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind=kint), intent(in) :: k_ref(numele)
!
      integer (kind=kint), intent(in) :: iele_st, iele_ed
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
!
      integer (kind = kint) :: inod, iele, k
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        k = k_ref(iele)
        inod = ie(iele,k)
        nele_4_node(inod) = nele_4_node(inod) + 1
      end do
!
      end  subroutine count_belonged_ele_4_node2
!
! -----------------------------------------------------------------------
!
      subroutine set_belonged_ele_4_node2                               &
     &         (numnod, numele, nnod_4_ele, ie, k_ref,                  &
     &          iele_st, iele_ed, ntot_ele_4_node, iele_stack_4_node,   &
     &          nele_4_node, iele_4_node, iconn_4_node)
!
      use quicksort
!
      integer (kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind=kint), intent(in) :: k_ref(numele)
!
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
      integer (kind = kint) :: inod, iele, icou, ist, k
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        k = k_ref(iele)
        inod = ie(iele,k)
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
! -----------------------------------------------------------------------! -----------------------------------------------------------------------!
      subroutine set_node_ele_double_address                            &
     &         (node, ele, nod_comm, ele_comm,                          &
     &          inod_lc, ip_node, iele_lc, ip_ele)
!
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
!
      integer(kind = kint), intent(inout) :: inod_lc(node%numnod)
      integer(kind = kint), intent(inout) :: ip_node(node%numnod)
      integer(kind = kint), intent(inout) :: iele_lc(0:ele%numele)
      integer(kind = kint), intent(inout) :: ip_ele(0:ele%numele)
!
      integer(kind = kint) :: i
!
!
!$onp parallel do
      do i = 1, node%numnod
        inod_lc(i) = i
        ip_node(i) = my_rank
      end do
!$onp end parallel do
      call SOLVER_SEND_RECV_int_type(node%numnod, nod_comm, inod_lc(1))
      call SOLVER_SEND_RECV_int_type(node%numnod, nod_comm, ip_node(1))
!
!
      iele_lc(0) = 0
      ip_ele(0) = -1
!$onp parallel do
      do i = 1, ele%numele
        iele_lc(i) = i
        ip_ele(i) = ip_node(ele%ie(i,1))
      end do
!$onp end parallel do
      call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm, iele_lc(1))
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
      end module analyzer_comm_test
