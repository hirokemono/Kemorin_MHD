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
!!        type(parallel_double_numbering), intent(inout) :: dbl_id
!!
!!      subroutine set_para_double_numbering                            &
!!     &         (internal_node, nod_comm, dbl_id)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(parallel_double_numbering), intent(inout) :: dbl_id
!!      subroutine set_para_ele_double_numbering                        &
!!     &         (internal_node, ele_comm, ele, dbl_id)
!!        type(communication_table), intent(in) :: ele_comm
!!        type(element_data), intent(in) :: ele
!!        type(parallel_double_numbering), intent(inout) :: dbl_id
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
      type parallel_double_numbering
!>        number of node for each subdomain
        integer(kind = kint) :: num_dbl
!>        local node ID
        integer(kind = kint), allocatable :: id_local(:)
!>        belonged subdomains ID for each node
        integer(kind = kint), allocatable :: ip_home(:)
      end type parallel_double_numbering
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
      type(parallel_double_numbering), intent(inout) :: dbl_id
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
      type(parallel_double_numbering), intent(inout) :: dbl_id
!
!
      deallocate(dbl_id%id_local, dbl_id%ip_home)
!
      end subroutine dealloc_double_numbering
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_para_double_numbering                              &
     &         (internal_node, nod_comm, dbl_id)
!
      use t_ucd_data
      use t_comm_table
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: internal_node
      type(communication_table), intent(in) :: nod_comm
      type(parallel_double_numbering), intent(inout) :: dbl_id
!
      integer(kind = kint) :: inod
!
!$omp parallel do
      do inod = 1, internal_node
        dbl_id%id_local(inod) = inod
        dbl_id%ip_home(inod) = my_rank
      end do
!$omp end parallel do
!$omp parallel do
      do inod = internal_node+1, dbl_id%num_dbl
        dbl_id%id_local(inod) =  0
        dbl_id%ip_home(inod) = -1
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (dbl_id%num_dbl, nod_comm, dbl_id%id_local)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (dbl_id%num_dbl, nod_comm, dbl_id%ip_home)
!
      end subroutine set_para_double_numbering
!
! -----------------------------------------------------------------------
!
      subroutine set_para_ele_double_numbering                          &
     &         (internal_node, ele_comm, ele, dbl_id)
!
      use t_ucd_data
      use t_comm_table
      use t_geometry_data
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: internal_node
      type(communication_table), intent(in) :: ele_comm
      type(element_data), intent(in) :: ele
      type(parallel_double_numbering), intent(inout) :: dbl_id
!
      integer(kind = kint) :: iele
!
!$omp parallel do
      do iele = 1, dbl_id%num_dbl
        if(ele%ie(iele,1) .le. internal_node) then
          dbl_id%id_local(iele) = iele
          dbl_id%ip_home(iele) = my_rank
        else
          dbl_id%id_local(iele) = 0
          dbl_id%ip_home(iele) = -1
        end if
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (ele%numele, ele_comm, dbl_id%id_local)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (ele%numele, ele_comm, dbl_id%ip_home)
!
      end subroutine set_para_ele_double_numbering
!
! -----------------------------------------------------------------------
!
      end module t_para_double_numbering
