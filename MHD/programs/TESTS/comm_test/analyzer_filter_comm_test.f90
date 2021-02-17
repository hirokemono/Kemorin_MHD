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
      use t_solver_SR
      use t_filtering_data
!
      implicit none
!
      type(filtering_data_type), save :: filtering_test
      type(filtering_work_type), save :: wk_filter_test
      type(send_recv_status), save :: SR_sig_t
!
      private :: filtering_test
      private :: nod_filter_send_recv_test
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
      use input_ctl_filter_comm_test
!
!
      if(my_rank .eq. 0) write(*,*) 'check commnication tables'
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_input_ctl_filter_comm_test'
      call s_input_ctl_filter_comm_test(filtering_test, wk_filter_test)
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
      use m_geometry_filter_comm_test
      use m_nod_filter_comm_table
      use set_diff_geom_comm_test
      use write_diff_filter_comm_test
!
!
      call allocate_filter_nod_comm_test
      call nod_filter_send_recv_test(filtering_test)
!
!
      call count_node_comm_test(nnod_filtering, inter_nod_3dfilter,     &
     &    xx_filtering, xx_filter_comm, nnod_filter_diff_local)
!
      call allocate_diff_filter_ctest
      call compare_nod_comm_test(nnod_filtering,                        &
     &    inter_nod_3dfilter, xx_filtering, xx_filter_comm,             &
     &    nnod_filter_diff_local, inod_filter_diff, xx_filter_diff)
!
!
      call deallocate_filter_nod_comm_test
!
!
      call allocate_filter_stk_ctest_IO
!
      call resize_SR_flag(nprocs, 1, SR_sig_t)
      call count_collect_SR_num                                         &
     &   (nnod_filter_diff_local, istack_filter_nod_diff_pe, SR_sig_t)
!
      call allocate_filter_comm_test_IO
!
      call collect_send_recv_int                                        &
     &   (0, nnod_filter_diff_local, inod_filter_diff,                  &
     &    istack_filter_nod_diff_pe, inod_filter_diff_IO, SR_sig_t)
      call collect_send_recv_N                                          &
     &   (0, isix, nnod_filter_diff_local, xx_filter_diff,              &
     &    istack_filter_nod_diff_pe, xx_filter_diff_IO, SR_sig_t)
      call dealloc_SR_flag(SR_sig_t)
!!
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
! ----------------------------------------------------------------------
!
      subroutine nod_filter_send_recv_test(filtering)
!
      use calypso_mpi
      use m_nod_filter_comm_table
      use m_geometry_filter_comm_test
      use solver_SR_type
!
      type(filtering_data_type), intent(in) :: filtering
      integer(kind = kint) :: inod
!
!
      do inod = 1, inter_nod_3dfilter
        xx_filter_comm(3*inod-2) = xx_filtering(inod,1)
        xx_filter_comm(3*inod-1) = xx_filtering(inod,2)
        xx_filter_comm(3*inod  ) = xx_filtering(inod,3)
      end do
!
      call SOLVER_SEND_RECV_3_type                                      &
     &   (nnod_filtering, filtering%comm, xx_filter_comm)
!
      end subroutine nod_filter_send_recv_test
!
! ----------------------------------------------------------------------
!
      end module analyzer_filter_comm_test
