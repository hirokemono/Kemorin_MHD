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
!
      implicit none
!
      type(send_recv_status), save :: SR_sig_t
!
      type(comm_test_control), save ::  comm_tctl1
      type(comm_test_files_param), save ::  T_files
      type(mesh_data), save :: test_fem
!
      type(communication_table), save :: T_ele_comm
      type(communication_table), save :: T_surf_comm
      type(communication_table), save :: T_edge_comm
      type(belonged_table), save :: blng
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_communication_test
!
      use m_array_for_send_recv
      use parallel_FEM_mesh_init
      use mesh_send_recv_test
      use nod_phys_send_recv
      use mpi_load_mesh_data
      use const_element_comm_tables
      use const_element_comm_table
!
      integer :: i, num_in, num_ex
!
      if(my_rank .eq. 0)  write(*,*) 'check commnication tables'
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
        write(50+my_rank,*) 'id_neib:',                                 &
     &      test_fem%mesh%nod_comm%id_neib(i), num_in, num_ex
      end do
      close(50+my_rank)
!
!  -------------------------------------------
!
      call resize_SR_flag(nprocs, 1, SR_sig_t)
!
      call FEM_mesh_initialization(test_fem%mesh, test_fem%group)
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_tbl'
      call const_ele_comm_tbl                                           &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm, blng,             &
     &    T_ele_comm, test_fem%mesh%ele)
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
      call const_surf_comm_table                                        &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm, blng,             &
     &    T_surf_comm, test_fem%mesh%surf)
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table'
      call const_edge_comm_table                                        &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm, blng,             &
     &    T_edge_comm, test_fem%mesh%edge)
!
      end subroutine initialize_communication_test
!
! ----------------------------------------------------------------------
!
      subroutine analyze_communication_test
!
      use t_vector_for_solver
!
      use calypso_mpi
      use collect_SR_N
      use collect_SR_int
      use m_array_for_send_recv
      use m_geometry_4_comm_test
      use mesh_send_recv_test
      use set_diff_geom_comm_test
      use write_diff_4_comm_test
      use nod_phys_send_recv
!
      use set_ele_id_4_node_type
      use const_element_comm_tables
!
      integer(kind = kint), parameter :: N12 = 12
      type(vectors_4_solver) :: vect1
!
!
      call calypso_mpi_barrier
!
      call alloc_iccgN_vec_type(12, test_fem%mesh%node%numnod,      &
     &                          vect1)
      call alloc_iccg_int8_vector(test_fem%mesh%node%numnod, vect1)
!
      if (iflag_debug.gt.0) write(*,*) 'node_send_recv4_test'
      call node_send_recv4_test                                         &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm, N12, vect1)
!
      call dealloc_iccgN_vec_type(vect1)
      call dealloc_iccg_int8_vector(vect1)
!
      call alloc_iccgN_vec_type(ithree, test_fem%mesh%node%numnod,      &
     &                          vect1)
      call alloc_iccg_int8_vector(test_fem%mesh%node%numnod, vect1)
!
      if (iflag_debug.gt.0) write(*,*) 'node_send_recv_test'
      call node_send_recv_test                                          &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm, vect1)
      if (iflag_debug.gt.0) write(*,*) 'count_diff_node_comm_test'
      call count_diff_node_comm_test(test_fem%mesh%node, vect1)
!
      call allocate_diff_nod_comm_test
      if (iflag_debug.gt.0) write(*,*) 'set_diff_node_comm_test'
      call set_diff_node_comm_test(test_fem%mesh%node, vect1)
!
      call dealloc_iccgN_vec_type(vect1)
      call dealloc_iccg_int8_vector(vect1)
!
      call allocate_nod_stack_ctest_IO
      if (iflag_debug.gt.0) write(*,*) 'count_diff_nod_comm_test'
      call count_collect_SR_num                                         &
     &   (nnod_diff_local, istack_nod_diff_pe, SR_sig_t)
      call allocate_nod_comm_test_IO
!
      if (iflag_debug.gt.0) write(*,*) 'collect_diff_nod_comm_test'
      call collect_send_recv_int                                        &
     &   (0, nnod_diff_local, inod_diff,                                &
     &    istack_nod_diff_pe, inod_diff_IO, SR_sig_t)
      call collect_send_recv_N                                          &
     &   (0, isix, nnod_diff_local, xx_diff,                            &
     &    istack_nod_diff_pe, xx_diff_IO, SR_sig_t)
      call deallocate_diff_nod_comm_test
!
!
      call allocate_geom_4_comm_test (test_fem%mesh%ele%numele,         &
     &    test_fem%mesh%surf%numsurf, test_fem%mesh%edge%numedge)
      if (iflag_debug.gt.0) write(*,*) 's_mesh_send_recv_test'
      call s_mesh_send_recv_test(test_fem%mesh%ele,                     &
     &    test_fem%mesh%surf, test_fem%mesh%edge,                       &
     &    T_ele_comm, T_surf_comm, T_edge_comm)
      if (iflag_debug.gt.0) write(*,*) 's_count_diff_geom_comm_test'
      call s_count_diff_geom_comm_test                                  &
     &   (test_fem%mesh%ele, test_fem%mesh%surf, test_fem%mesh%edge,    &
     &    T_ele_comm, T_surf_comm, T_edge_comm)
!
      call allocate_diff_geom_comm_test
      if (iflag_debug.gt.0) write(*,*) 's_set_diff_geom_comm_test'
      call s_set_diff_geom_comm_test(test_fem%mesh%ele,                 &
     &   test_fem%mesh%surf, test_fem%mesh%edge,                        &
     &   T_ele_comm, T_surf_comm, T_edge_comm)
      call deallocate_geom_4_comm_test
!
      call allocate_geom_stack_ctest_IO
!
      write(*,*) 'nnod_diff_local, nele_diff_local, ',                  &
     &                      'nsurf_diff_local, nedge_diff_local '
      write(*,*) my_rank, nnod_diff_local, nele_diff_local,             &
     &                      nsurf_diff_local, nedge_diff_local
      call count_collect_SR_num                                         &
     &   (nele_diff_local, istack_ele_diff_pe, SR_sig_t)
      call count_collect_SR_num                                         &
     &   (nsurf_diff_local, istack_surf_diff_pe, SR_sig_t)
      call count_collect_SR_num                                         &
     &   (nedge_diff_local, istack_edge_diff_pe, SR_sig_t)
!
      call allocate_geom_comm_test_IO
      if (iflag_debug.gt.0) write(*,*) 's_collect_diff_4_comm_test'
!
      call collect_send_recv_int                                        &
     &   (0, nele_diff_local, iele_diff,                                &
     &    istack_ele_diff_pe, iele_diff_IO, SR_sig_t)
      call collect_send_recv_N                                          &
     &   (0, isix, nele_diff_local, xele_diff,                          &
     &    istack_ele_diff_pe, xele_diff_IO, SR_sig_t)
!
      call collect_send_recv_int                                        &
     &   (0, nsurf_diff_local, isurf_diff,                              &
     &    istack_surf_diff_pe, isurf_diff_IO, SR_sig_t)
      call collect_send_recv_N                                          &
     &   (0, isix, nsurf_diff_local, xsurf_diff,                        &
     &    istack_surf_diff_pe, xsurf_diff_IO, SR_sig_t)
!
      call collect_send_recv_int                                        &
     &   (0, nedge_diff_local, iedge_diff,                              &
     &    istack_edge_diff_pe, iedge_diff_IO, SR_sig_t)
      call collect_send_recv_N                                          &
     &   (0, isix, nedge_diff_local, xedge_diff,                        &
     &    istack_edge_diff_pe, xedge_diff_IO, SR_sig_t)
      call deallocate_diff_geom_comm_test
!
      call dealloc_SR_flag(SR_sig_t)
!
      if (my_rank .eq. 0) call output_diff_mesh_comm_test
      call deallocate_nod_comm_test_IO
      call deallocate_geom_comm_test_IO
!
      call output_elapsed_times
!
      end subroutine analyze_communication_test
!
! ----------------------------------------------------------------------
!
      end module analyzer_comm_test
