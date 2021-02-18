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
      use t_geometry_data
      use t_solver_SR
      use t_filtering_data
      use t_work_for_comm_check
      use t_vector_for_solver
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &      :: comm_test_name = 'filter_comm_test.dat'
      type(filtering_data_type), save :: filtering_test
      type(filtering_work_type), save :: wk_filter_test
      type(work_for_comm_check), save :: filter_check
      type(node_data), save :: filter_nod1
!>      Structure for communicatiors for solver
      type(vectors_4_solver), save :: v_sol_F
      type(send_recv_status), save :: SR_sig_F
!
      private :: filtering_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use calypso_mpi
      use nod_phys_send_recv
      use input_ctl_filter_comm_test
!
!
      if(my_rank .eq. 0) write(*,*) 'check commnication tables'
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_input_ctl_filter_comm_test'
      call s_input_ctl_filter_comm_test                                 &
     &   (filtering_test, filter_nod1, wk_filter_test)
      call alloc_iccgN_vec_type(6, filter_nod1%numnod, v_sol_F)
      call init_send_recv(filtering_test%comm)
!
       end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use calypso_mpi
      use collect_SR_int
      use collect_SR_N
      use t_work_for_comm_check
      use diff_geometory_comm_test
      use mesh_send_recv_check
      use write_diff_4_comm_test
!
      call resize_SR_flag(nprocs, 1, SR_sig_F)
      call node_send_recv_test(filter_nod1, filtering_test%comm,        &
     &    filter_check, v_sol_F, SR_sig_F)
      call output_diff_node_comm_test(comm_test_name, filter_check)
!
      call dealloc_ele_comm_test_IO(filter_check)
      call dealloc_SR_flag(SR_sig_F)
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filter_comm_test
