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
      use t_belonged_element_4_node
!
      implicit none
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
      use input_control_comm_test
!
!
      if(my_rank .eq. 0)  write(*,*) 'check commnication tables'
!
!     --------------------- 
!
      num_elapsed = 29
      call allocate_elapsed_times
!
      write(elapse_labels( 1),'(a)') 'const_mesh_informations'
      write(elapse_labels( 2),'(a)') 'const_global_numnod_list'
      write(elapse_labels( 3),'(a)') 'const_ele_comm_tbla'
      write(elapse_labels( 4),'(a)') 'const_global_element_id'
      write(elapse_labels( 5),'(a)') '          '
      write(elapse_labels( 6),'(a)') '          '
      write(elapse_labels( 7),'(a)') '          '
      write(elapse_labels( 8),'(a)') '          '
      write(elapse_labels( 9),'(a)') '          '
      write(elapse_labels(10),'(a)') '          '
      write(elapse_labels(11),'(a)') '          '
      write(elapse_labels(12),'(a)') '          '
      write(elapse_labels(13),'(a)') '          '
      write(elapse_labels(14),'(a)') '          '
      write(elapse_labels(15),'(a)') '          '
      write(elapse_labels(16),'(a)') '          '
      write(elapse_labels(17),'(a)') '          '
      write(elapse_labels(18),'(a)') 'const_comm_table_by_connenct'
      write(elapse_labels(19),'(a)') '          '
      write(elapse_labels(20),'(a)') '          '
      write(elapse_labels(21),'(a)') 'const_edge_hash_4_ele'
      write(elapse_labels(22),'(a)') 'count_num_edges_by_ele'
      write(elapse_labels(23),'(a)') 'set_edges_connect_by_ele'
      write(elapse_labels(24),'(a)') 'set_edges_connect_4_sf'
      write(elapse_labels(25),'(a)') 'count_element_import_num'
      write(elapse_labels(26),'(a)') 'set_element_import_item'
      write(elapse_labels(27),'(a)') 'element_num_reverse_SR'
      write(elapse_labels(28),'(a)') 'element_position_reverse_SR'
      write(elapse_labels(29),'(a)') 'set_element_export_item'
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 's_input_control_comm_test'
      call s_input_control_comm_test
!
       end subroutine initialize_communication_test
!
! ----------------------------------------------------------------------
!
      subroutine analyze_communication_test
!
      use calypso_mpi
      use m_geometry_data
      use m_group_data
      use m_nod_comm_table
      use m_array_for_send_recv
      use m_array_for_send_recv
      use m_geometry_4_comm_test
      use m_read_mesh_data
      use mesh_send_recv_test
      use set_diff_geom_comm_test
      use collect_diff_4_comm_test
      use write_diff_4_comm_test
      use nod_phys_send_recv
      use const_mesh_information
      use const_element_comm_tables
!
      use set_ele_id_4_node_type
      use const_element_comm_tables
!
!
      call allocate_iccg_int8_matrix(node1%numnod)
      call allocate_cflag_collect_diff
      call allocate_vector_for_solver(ithree, node1%numnod)
!
      call init_send_recv(nod_comm)
!
!  -----    construct geometry informations
!
      call start_eleps_time(1)
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank,                                    &
     &    node1, ele1, surf1, edge1, nod_grp1, ele_grp1, sf_grp1,       &
     &    ele_grp_tbl1, sf_grp_tbl1, sf_grp_nod1)
      call end_eleps_time(1)
      call calypso_mpi_barrier
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call start_eleps_time(2)
      call const_element_comm_tbls(node1, ele1, surf1, edge1,           &
     &    nod_comm, ele_comm, surf_comm, edge_comm)
      call end_eleps_time(2)
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
      call const_surf_comm_table                                       &
     &   (node1, nod_comm, surf1, blng, surf_comm)
      call const_global_surface_id(surf1, surf_comm)
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table'
      call const_edge_comm_table                                        &
     &   (node1, nod_comm, edge1, blng, edge_comm)
      call const_global_edge_id(edge1, edge_comm)
!
      call output_elapsed_times
      return
!
!  -------------------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'node_send_recv4_test'
      call node_send_recv4_test(node1, nod_comm)
!
      if (iflag_debug.gt.0) write(*,*) 'node_send_recv_test'
      call node_send_recv_test(node1, nod_comm)
      if (iflag_debug.gt.0) write(*,*) 'count_diff_node_comm_test'
      call count_diff_node_comm_test
!
      call allocate_diff_nod_comm_test
      if (iflag_debug.gt.0) write(*,*) 'set_diff_node_comm_test'
      call set_diff_node_comm_test
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
      call allocate_geom_4_comm_test                                    &
     &   (ele1%numele, surf1%numsurf, edge1%numedge)
      if (iflag_debug.gt.0) write(*,*) 's_mesh_send_recv_test'
      call s_mesh_send_recv_test(ele1, surf1, edge1,                    &
     &    ele_comm, surf_comm, edge_comm)
      if (iflag_debug.gt.0) write(*,*) 's_count_diff_geom_comm_test'
      call s_count_diff_geom_comm_test
!
      call allocate_diff_geom_comm_test
      if (iflag_debug.gt.0) write(*,*) 's_set_diff_geom_comm_test'
      call s_set_diff_geom_comm_test
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
      end subroutine analyze_communication_test
!
! ----------------------------------------------------------------------
!
      end module analyzer_comm_test
