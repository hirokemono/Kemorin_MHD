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
      use t_ctl_data_comm_test
      use t_mesh_data
      use t_belonged_element_4_node
      use t_file_IO_parameter
      use t_control_param_comm_test
!
      implicit none
!
      type(comm_test_control), save ::  comm_tctl1
      type(comm_test_files_param), save ::  T_files
      type(mesh_data), save :: test_fem
      type(element_geometry), save :: test_ele_mesh
!
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
      use collect_diff_4_comm_test
      use nod_phys_send_recv
      use mpi_load_mesh_data
      use const_element_comm_table
!
      integer :: i, num_in, num_ex
!
      if(my_rank .eq. 0)  write(*,*) 'check commnication tables'
!
!     --------------------- 
!
      num_elapsed = 3
      call allocate_elapsed_times
!
      write(elapse_labels( 1),'(a)') 'const_mesh_informations'
      write(elapse_labels( 2),'(a)') 'const_global_numnod_list'
      write(elapse_labels( 3),'(a)') 'const_ele_comm_tbla'
!
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
      call mpi_input_mesh                                               &
     &   (T_files%mesh_file_IO, nprocs, test_fem, test_ele_mesh)
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
      call calypso_mpi_barrier
      call calypso_mpi_abort(1,'tako')
!
!  -------------------------------------------
!
      call allocate_iccg_int8_matrix(test_fem%mesh%node%numnod)
      call allocate_cflag_collect_diff
!
      call FEM_mesh_init_with_IO                                        &
     &   (T_files%iflag_output_SURF, T_files%mesh_file_IO,              &
     &    test_fem%mesh, test_fem%group, test_ele_mesh)
!
      end subroutine initialize_communication_test
!
! ----------------------------------------------------------------------
!
      subroutine analyze_communication_test
!
      use calypso_mpi
      use m_array_for_send_recv
      use m_geometry_4_comm_test
      use mesh_send_recv_test
      use set_diff_geom_comm_test
      use collect_diff_4_comm_test
      use write_diff_4_comm_test
      use nod_phys_send_recv
!
      use set_ele_id_4_node_type
      use const_element_comm_tables
!
!
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'node_send_recv4_test'
      call node_send_recv4_test                                         &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm)
!
      if (iflag_debug.gt.0) write(*,*) 'node_send_recv_test'
      call node_send_recv_test                                          &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm)
      if (iflag_debug.gt.0) write(*,*) 'count_diff_node_comm_test'
      call count_diff_node_comm_test(test_fem%mesh%node)
!
      call allocate_diff_nod_comm_test
      if (iflag_debug.gt.0) write(*,*) 'set_diff_node_comm_test'
      call set_diff_node_comm_test(test_fem%mesh%node)
!
      call deallocate_iccg_int8_matrix
      call deallocate_vector_for_solver
!
      call allocate_nod_stack_ctest_IO
      if (iflag_debug.gt.0) write(*,*) 'count_diff_nod_comm_test'
      call count_diff_nod_comm_test
      call allocate_nod_comm_test_IO
      if (iflag_debug.gt.0) write(*,*) 'collect_diff_nod_comm_test'
      call collect_diff_nod_comm_test
      call deallocate_diff_nod_comm_test
!
      call allocate_geom_4_comm_test (test_fem%mesh%ele%numele,         &
     &    test_ele_mesh%surf%numsurf, test_ele_mesh%edge%numedge)
      if (iflag_debug.gt.0) write(*,*) 's_mesh_send_recv_test'
      call s_mesh_send_recv_test(test_fem%mesh%ele,                     &
     &    test_ele_mesh%surf, test_ele_mesh%edge,                       &
     &    test_ele_mesh%ele_comm, test_ele_mesh%surf_comm,              &
     &    test_ele_mesh%edge_comm)
      if (iflag_debug.gt.0) write(*,*) 's_count_diff_geom_comm_test'
      call s_count_diff_geom_comm_test                                  &
     &   (test_fem%mesh%ele, test_ele_mesh%surf, test_ele_mesh%edge,    &
     &    test_ele_mesh%ele_comm, test_ele_mesh%surf_comm,              &
     &    test_ele_mesh%edge_comm)
!
      call allocate_diff_geom_comm_test
      if (iflag_debug.gt.0) write(*,*) 's_set_diff_geom_comm_test'
      call s_set_diff_geom_comm_test(test_fem%mesh%ele,                 &
     &   test_ele_mesh%surf, test_ele_mesh%edge,                        &
     &   test_ele_mesh%ele_comm, test_ele_mesh%surf_comm,               &
     &   test_ele_mesh%edge_comm)
      call deallocate_geom_4_comm_test
!
      call allocate_geom_stack_ctest_IO
      if (iflag_debug.gt.0) write(*,*) 's_count_diff_4_comm_test'
      call s_count_diff_4_comm_test
      call allocate_geom_comm_test_IO
      if (iflag_debug.gt.0) write(*,*) 's_collect_diff_4_comm_test'
      call s_collect_diff_4_comm_test
      call deallocate_diff_geom_comm_test
!
      call deallocate_cflag_collect_diff
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
