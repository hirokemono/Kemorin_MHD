!analyzer_filter_comm_test.f90
!      module analyzer_filter_comm_test
!
!      modified by H. Matsui on May., 2008
!
!      subroutine init_analyzer
!      subroutine analyze
!
!..................................................
!
      module analyzer_filter_comm_test
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
      use input_ctl_filter_comm_test
!
!
      if(my_rank .eq. 0) write(*,*) 'check commnication tables'
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_input_ctl_filter_comm_test'
      call s_input_ctl_filter_comm_test
!
       end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_parallel_var_dof
      use m_geometry_filter_comm_test
      use filter_send_recv_test
      use set_diff_filter_comm_test
      use collect_diff_4_comm_test
      use collect_diff_filter_ctest
      use write_diff_filter_comm_test
!
!
      call allocate_filter_nod_comm_test
      call nod_filter_send_recv_test
!
!
      call count_filter_node_comm_test
!
      call allocate_diff_filter_ctest
      call set_diff_filter_nod_comm_test
!
      call deallocate_filter_nod_comm_test
!
!
      call allocate_filter_stk_ctest_IO
!
      call allocate_cflag_collect_diff
      call count_diff_filter_nod_comm_test
!
      call allocate_filter_comm_test_IO
!
      call collect_diff_filter_nod_ctest
!
      call deallocate_cflag_collect_diff
      call deallocate_diff_filter_ctest
!
      if (my_rank .eq. 0) then
        call output_diff_filter_nod_ctest
      end if
!
      call deallocate_filter_comm_test_IO
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filter_comm_test
