!analyzer_comm_test.f90
!      module analyzer_comm_test
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer
!      subroutine analyze
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
      subroutine init_analyzer
!
      use m_parallel_var_dof
      use const_mesh_info
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
!     --------------------- 
!
!       if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
!      call set_local_element_info
!
       end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_parallel_var_dof
      use m_geometry_parameter
      use m_geometry_4_comm_test
      use m_read_mesh_data
      use mesh_send_recv_test
      use set_diff_geom_comm_test
      use collect_diff_4_comm_test
      use write_diff_4_comm_test
!
      integer(kind = kint) :: iflag
!
!
      call allocate_iccg_int_matrix(numnod)
      call allocate_iccgN_matrix(ithree, numnod)
      call allocate_cflag_collect_diff
!
      call node_send_recv_test
      call count_diff_node_comm_test
!
      call allocate_diff_nod_comm_test
      call set_diff_node_comm_test
!
      call deallocate_iccg_int_matrix
      call deallocate_iccgN_matrix
!
      call allocate_nod_stack_ctest_IO
      call count_diff_nod_comm_test
      call allocate_nod_comm_test_IO
      call collect_diff_nod_comm_test
      call deallocate_diff_nod_comm_test
!
!
      iflag = iflag_ele_file_name*iflag_surf_file_name                  &
     &       *iflag_edge_file_name
      if (iflag .gt. 0) then
        call allocate_geom_4_comm_test
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
      end if
!
      call deallocate_cflag_collect_diff
!
      if (iflag .eq. 0) then
        if (my_rank .eq. 0) call output_diff_node_comm_test
        call deallocate_nod_comm_test_IO
      else
        if (my_rank .eq. 0) call output_diff_mesh_comm_test
        call deallocate_nod_comm_test_IO
        call deallocate_geom_comm_test_IO
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_comm_test
