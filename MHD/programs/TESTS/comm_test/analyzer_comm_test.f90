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
!
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_communication_test
!
      use calypso_mpi
      use input_control_comm_test
!
!
      if(my_rank .eq. 0)  write(*,*) 'check commnication tables'
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 's_input_control_comm_test'
      call s_input_control_comm_test
!
!
       end subroutine initialize_communication_test
!
! ----------------------------------------------------------------------
!
      subroutine analyze_communication_test
!
      use calypso_mpi
      use m_geometry_data
      use m_array_for_send_recv
      use m_array_for_send_recv
      use m_geometry_parameter
      use m_geometry_4_comm_test
      use m_read_mesh_data
      use m_ele_sf_eg_comm_tables
      use mesh_send_recv_test
      use set_diff_geom_comm_test
      use collect_diff_4_comm_test
      use write_diff_4_comm_test
      use nodal_vector_send_recv
      use const_mesh_info
!
!
      call allocate_iccg_int8_matrix(node1%numnod)
      call allocate_cflag_collect_diff
      call allocate_vector_for_solver(ithree, node1%numnod)
!
      call init_send_recv
!
!  -----    construct geometry informations
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tables_1st'
      call const_element_comm_tables_1st
!
!  -------------------------------------------
!
      call node_send_recv4_test
!
      call node_send_recv_test
      call count_diff_node_comm_test
!
      call allocate_diff_nod_comm_test
      call set_diff_node_comm_test
!
      call deallocate_iccg_int8_matrix
      call deallocate_vector_for_solver
!
      call allocate_nod_stack_ctest_IO
      call count_diff_nod_comm_test
      call allocate_nod_comm_test_IO
      call collect_diff_nod_comm_test
      call deallocate_diff_nod_comm_test
!
      call allocate_geom_4_comm_test(numele, numsurf, numedge)
      call s_mesh_send_recv_test
      call s_count_diff_geom_comm_test
!
      call allocate_diff_geom_comm_test
      call s_set_diff_geom_comm_test
      call deallocate_geom_4_comm_test
!
      call allocate_geom_stack_ctest_IO
      call s_count_diff_4_comm_test
      call allocate_geom_comm_test_IO
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
