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
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table2'
      call alloc_failed_export(0, fail_tbl_s)
      call const_ele_comm_table2                                        &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm,                   &
     &    T_ele_comm, test_fem%mesh%ele, fail_tbl_s)
      call dealloc_failed_export(fail_tbl_s)
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table2'
      call alloc_failed_export(0, fail_tbl_s)
      call const_surf_comm_table2                                      &
     &   (test_fem%mesh%node, test_fem%mesh%ele,                       &
     &    test_fem%mesh%nod_comm, T_surf_comm,                         &
     &    test_fem%mesh%surf, fail_tbl_s)
!
      call dealloc_failed_export(fail_tbl_s)
!
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table2'
      call alloc_failed_export(0, fail_tbl_d)
      call const_edge_comm_table2                                      &
     &   (test_fem%mesh%node, test_fem%mesh%ele,                       &
     &    test_fem%mesh%nod_comm, T_edge_comm,                         &
     &    test_fem%mesh%edge, fail_tbl_d)
      call dealloc_failed_export(fail_tbl_d)
!
!      call calypso_mpi_barrier
!      call calypso_mpi_abort(0, 'manuke')
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
      subroutine const_ele_comm_table2                                 &
     &         (node, nod_comm, ele_comm, ele, fail_tbl)
!
      use m_geometry_constants
      use t_para_double_numbering
      use t_const_comm_table
      use set_ele_id_4_node_type
      use const_element_comm_table
      use const_element_comm_tables
      use find_belonged_process
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(element_data), intent(inout) :: ele
      type(communication_table), intent(inout) :: ele_comm
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
!
      type(parallel_double_numbering) :: inod_dbl
!
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: k_ref(:)
!
      integer(kind = kint) :: iele, i1, i2, i, inod, ip
!
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl)
!
      allocate(ip_ref(ele%numele))
      allocate(k_ref(ele%numele))
      call find_belonged_pe_4_ele(my_rank, node, inod_dbl%ip_home(1),   &
     &                            ele, ip_ref, k_ref)
!
      call set_ele_id_4_node(node, ele, belongs%blng_ele)
      call alloc_x_ref_ele(node, belongs)
      call sort_inod_4_ele_by_position(ione, ele%numele, ele%x_ele,     &
     &    node, belongs%blng_ele, belongs%x_ref_ele)
!
      call const_comm_table_by_connenct                                 &
     &   (txt_surf, ele%numele, ele%nnod_4_ele, ele%ie,                 &
     &    ele%x_ele, node, nod_comm, inod_dbl, ip_ref, k_ref,           &
     &    belongs%blng_ele, ele_comm, fail_tbl)
      call dealloc_double_numbering(inod_dbl)
      deallocate(ip_ref, k_ref)
!
      call dealloc_x_ref_ele(belongs)
      call dealloc_iele_belonged(belongs%blng_ele)
!
      call const_global_element_id(ele_comm, ele)
      call calypso_mpi_barrier
!
      end subroutine const_ele_comm_table2
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table2                                 &
     &         (node, ele, nod_comm, surf_comm, surf, fail_tbl)
!
      use m_geometry_constants
      use t_para_double_numbering
      use t_const_comm_table
      use set_ele_id_4_node_type
      use const_element_comm_table
      use const_element_comm_tables
      use find_belonged_process
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(inout) :: surf_comm
      type(surface_data), intent(inout) :: surf
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
!
      type(parallel_double_numbering) :: inod_dbl
!
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: k_ref(:)
!
      integer(kind = kint) :: isurf, i1, i2, i, inod, ip
!
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl)
!
      allocate(ip_ref(surf%numsurf))
      allocate(k_ref(surf%numsurf))
!
      call find_belonged_pe_4_surf(my_rank, node, inod_dbl%ip_home(1),  &
     &                             surf, ip_ref, k_ref)
!
      call set_surf_id_4_node(node, surf, belongs%blng_surf)
      call alloc_x_ref_surf(node, belongs)
      call sort_inod_4_ele_by_position(ione, surf%numsurf, surf%x_surf, &
     &    node, belongs%blng_surf, belongs%x_ref_surf)
!
      call const_comm_table_by_connenct                                 &
     &   (txt_surf, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &    surf%x_surf, node, nod_comm, inod_dbl, ip_ref, k_ref,         &
     &    belongs%blng_surf, surf_comm, fail_tbl)
      call dealloc_double_numbering(inod_dbl)
      deallocate(ip_ref, k_ref)
!
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
     &         (node, ele, nod_comm, edge_comm, edge, fail_tbl)
!
      use m_geometry_constants
      use t_para_double_numbering
      use t_const_comm_table
      use set_ele_id_4_node_type
      use const_element_comm_table
      use const_element_comm_tables
      use find_belonged_process
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
!
      type(communication_table), intent(inout) :: edge_comm
      type(edge_data), intent(inout) :: edge
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
!
      type(parallel_double_numbering) :: inod_dbl
!
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: k_ref(:)
!
      integer(kind = kint) :: iedge
!
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl)
!
      allocate(ip_ref(edge%numedge))
      allocate(k_ref(edge%numedge))
      call find_belonged_pe_4_edge(my_rank, node, inod_dbl%ip_home(1),  &
     &                             edge, ip_ref, k_ref)
!
!
      if(iflag_debug.gt.0) write(*,*) ' set_edge_id_4_node in edge'
      call set_edge_id_4_node(node, edge, belongs%blng_edge)
      call alloc_x_ref_edge(node, belongs)
      call sort_inod_4_ele_by_position(ione, edge%numedge, edge%x_edge, &
     &    node, belongs%blng_edge, belongs%x_ref_edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' const_comm_table_by_connenct in edge'
      call const_comm_table_by_connenct                                 &
     &   (txt_edge, edge%numedge, edge%nnod_4_edge, edge%ie_edge,       &
     &    edge%x_edge, node, nod_comm, inod_dbl, ip_ref, k_ref,         &
     &    belongs%blng_edge, edge_comm, fail_tbl)
      call dealloc_double_numbering(inod_dbl)
      deallocate(ip_ref, k_ref)
!
      call dealloc_x_ref_edge(belongs)
      call dealloc_iele_belonged(belongs%blng_edge)
!
      call const_global_edge_id(edge_comm, edge)
!
      end subroutine const_edge_comm_table2
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_node_ele_double_address                            &
     &         (node, ele, nod_comm, ele_comm, inod_dbl, iele_dbl)
!
      use t_para_double_numbering
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
!
      type(parallel_double_numbering), intent(inout) :: inod_dbl
      type(parallel_double_numbering), intent(inout) :: iele_dbl
!
!
      call set_node_double_numbering(node, nod_comm, inod_dbl)
      call set_ele_double_numbering                                     &
     &   (ele, ele_comm, inod_dbl, iele_dbl)
!
      end subroutine set_node_ele_double_address
!
! -----------------------------------------------------------------------
!
      end module analyzer_comm_test
