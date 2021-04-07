!>@file   check_sleeve_extend_mesh.f90
!!@brief  module check_sleeve_extend_mesh
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Check extended mesh infoemation
!!
!!@verbatim
!!      subroutine check_extended_element                               &
!!     &         (new_nod_comm, new_node, new_ele, new_ele_comm)
!!        type(communication_table), intent(inout) :: new_nod_comm
!!        type(node_data), intent(inout) :: new_node
!!        type(element_data), intent(inout) :: new_ele
!!        type(communication_table), intent(inout) :: new_ele_comm
!!        type(node_ele_double_number), save :: inod_dbl
!!        type(node_ele_double_number), save :: iele_dbl
!!@endverbatim
!
      module check_sleeve_extend_mesh
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine check_extended_element                                 &
     &         (new_nod_comm, new_node, new_ele, new_ele_comm)
!
      use t_comm_table
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_para_double_numbering
!
      use m_solver_SR
      use solver_SR_type
!
      type(communication_table), intent(inout) :: new_nod_comm
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
      type(communication_table), intent(inout) :: new_ele_comm
!
      type(node_ele_double_number), save :: inod_dbl
!
      integer(kind = kint), allocatable :: inod_lc_test(:,:)
      integer(kind = kint), allocatable :: irank_lc_test(:,:)
!
      integer(kind = kint) inod, iele, k1
      integer(kind = kint) icou, jcou, kcou, lcou
!
!
      call alloc_double_numbering(new_node%numnod, inod_dbl)
      if (iflag_debug.gt.0) write(*,*) 'set_node_double_numbering'
      call set_node_double_numbering(new_node, new_nod_comm, inod_dbl)
!
      allocate(inod_lc_test(new_ele%numele,new_ele%nnod_4_ele))
      allocate(irank_lc_test(new_ele%numele,new_ele%nnod_4_ele))
!
      icou = 0
      jcou = 0
      lcou = 0
      do iele = 1, new_ele%numele
        kcou = 0
        do k1 = 1, new_ele%nnod_4_ele
          if(new_ele%ie(iele,k1) .le. 0                                 &
     &         .or. new_ele%ie(iele,k1) .gt. new_node%numnod)           &
     &          kcou = kcou + 1
        end do
        if(kcou .gt. 0) then
          jcou = jcou + 1
          write(50+my_rank,*) new_node%numnod, iele,    &
     &         'Failed conectivity:', new_ele%ie(iele,:)
        end if
        if(kcou .eq. new_ele%nnod_4_ele) lcou = lcou + 1
        icou = icou + kcou
      end do
      write(*,*) my_rank, 'Failed Node ID:', icou, lcou, jcou
      call calypso_mpi_barrier
!
      do k1 = 1, new_ele%nnod_4_ele
        do iele = 1, new_ele%numele
          if(new_ele%ie(iele,1) .le. new_node%internal_node) then
            inod = new_ele%ie(iele,k1)
            inod_lc_test(iele,k1) =  inod_dbl%index(inod)
            irank_lc_test(iele,k1) = inod_dbl%irank(inod)
          end if
        end do
      end do
!
      do k1 = 1, new_ele%nnod_4_ele
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, inod_lc_test(1,k1))
      end do
      do k1 = 1, new_ele%nnod_4_ele
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, irank_lc_test(1,k1))
      end do
!
      icou = 0
      jcou = 0
      lcou = 0
      do iele = 1, new_ele%numele
        kcou = 0
        do k1 = 1, new_ele%nnod_4_ele
          inod = new_ele%ie(iele,k1)
          if(inod_lc_test(iele,k1) .ne. inod_dbl%index(inod)            &
     &      .or. irank_lc_test(iele,k1) .ne. inod_dbl%irank(inod))      &
     &          kcou = kcou + 1
        end do
        if(kcou .gt. 0) jcou = jcou + 1
        if(kcou .eq. new_ele%nnod_4_ele) lcou = lcou + 1
        icou = icou + kcou
      end do
      write(*,*) my_rank, 'Failed connectivity ID:', icou, lcou, jcou
!
      deallocate(inod_lc_test, irank_lc_test)
      call dealloc_double_numbering(inod_dbl)
!
      end subroutine check_extended_element
!
!  ---------------------------------------------------------------------
!
      end module check_sleeve_extend_mesh
