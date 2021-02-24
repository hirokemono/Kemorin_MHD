!>@file   t_para_double_numbering.f90
!!       module t_para_double_numbering
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2013
!
!> @brief Data for merged UCD file output
!!
!!@verbatim
!!      subroutine alloc_double_numbering(numnod, dbl_id)
!!      subroutine dealloc_double_numbering(dbl_id)
!!        type(node_ele_double_number), intent(inout) :: dbl_id
!!
!!      subroutine set_node_double_numbering(node, nod_comm, inod_dbl)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_ele_double_number), intent(inout) :: inod_dbl
!!      subroutine set_ele_double_numbering                             &
!!     &         (ele, ele_comm, inod_dbl, iele_dbl)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: ele_comm
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(node_ele_double_number), intent(inout) :: iele_dbl
!!
!!      subroutine find_belonged_pe_4_node                              &
!!     &         (my_rank, node, nod_comm, ip_node)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        integer(kind = kint), intent(inout) :: ip_node(node%numnod)
!!@endverbatim
!
      module t_para_double_numbering
!
      use m_precision
      use m_constants
!
      use calypso_mpi
!
      implicit none
!
!>      Structure of double numbering
      type node_ele_double_number
!>        number of node for each subdomain
        integer(kind = kint) :: num_dbl
!>        local node ID
        integer(kind = kint), allocatable :: id_local(:)
!>        belonged subdomains ID for each node
        integer(kind = kint), allocatable :: ip_home(:)
      end type node_ele_double_number
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_double_numbering(numnod, dbl_id)
!
      integer(kind = kint), intent(in) :: numnod
      type(node_ele_double_number), intent(inout) :: dbl_id
!
!
      dbl_id%num_dbl = numnod
      allocate(dbl_id%id_local(dbl_id%num_dbl))
      allocate(dbl_id%ip_home(dbl_id%num_dbl))
      if(dbl_id%num_dbl .gt. 0) then
        dbl_id%id_local = 0
        dbl_id%ip_home =  0
      end if
!
      end subroutine alloc_double_numbering
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_double_numbering(dbl_id)
!
      type(node_ele_double_number), intent(inout) :: dbl_id
!
!
      deallocate(dbl_id%id_local, dbl_id%ip_home)
!
      end subroutine dealloc_double_numbering
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_node_double_numbering(node, nod_comm, inod_dbl)
!
      use t_geometry_data
      use t_comm_table
      use solver_SR_type
      use find_belonged_process
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(node_ele_double_number), intent(inout) :: inod_dbl
!
      integer(kind = kint) :: inod
!
!
      call find_belonged_pe_4_node                                      &
     &   (my_rank, node, nod_comm, inod_dbl%ip_home)
!
!$omp parallel do
      do inod = 1, node%numnod
        inod_dbl%id_local(inod) = inod
      end do
!$omp end parallel do
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, inod_dbl%id_local(1))
!
      end subroutine set_node_double_numbering
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_double_numbering                               &
     &         (ele, ele_comm, inod_dbl, iele_dbl)

!
      use t_geometry_data
      use t_comm_table
      use solver_SR_type
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: ele_comm
      type(node_ele_double_number), intent(in) :: inod_dbl
!
      type(node_ele_double_number), intent(inout) :: iele_dbl
!
      integer(kind = kint) :: iele
!
!$omp parallel do
      do iele = 1, ele%numele
        iele_dbl%id_local(iele) = iele
        iele_dbl%ip_home(iele) =  inod_dbl%ip_home(ele%ie(iele,1))
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (ele%numele, ele_comm, iele_dbl%id_local(1))
!
      end subroutine set_ele_double_numbering
!
! -----------------------------------------------------------------------
!
      end module t_para_double_numbering
