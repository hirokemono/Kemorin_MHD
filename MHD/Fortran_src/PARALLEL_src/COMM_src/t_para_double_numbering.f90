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
!!      subroutine find_belonged_pe_4_ele(my_rank, inod_dbl, numele, ie,&
!!     &                                  interior_ele, iele_dbl)
!!        integer, intent(in) :: my_rank
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        integer(kind = kint), intent(in) :: numele
!!        integer(kind = kint), intent(in) :: ie(numele,1)
!!        type(node_ele_double_number), intent(inout) :: iele_dbl
!!        integer(kind = kint), intent(inout) :: interior_ele(numele)
!!      subroutine find_belonged_pe_4_surf(my_rank, inod_dbl,           &
!!     &          numsurf, nnod_4_surf, ie_surf, interior_surf,         &
!!     &          isurf_dbl)
!!        integer, intent(in) :: my_rank
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
!!        integer(kind = kint), intent(in):: ie_surf(numsurf,nnod_4_surf)
!!        type(node_ele_double_number), intent(inout) :: isurf_dbl
!!        integer(kind = kint), intent(inout) :: interior_surf(numsurf)
!!      subroutine find_belonged_pe_4_edge(my_rank, inod_dbl,           &
!!     &          numedge, nnod_4_edge, ie_edge, interior_edge,         &
!!     &          iedge_dbl)
!!        integer, intent(in) :: my_rank
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        integer(kind = kint), intent(in) :: numedge, nnod_4_edge
!!        integer(kind = kint), intent(in):: ie_edge(numedge,nnod_4_edge)
!!        type(edge_data), intent(inout) :: edge
!!        type(node_ele_double_number), intent(inout) :: iedge_dbl
!!        integer(kind = kint), intent(inout) :: interior_edge(numedge)
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
! -----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_ele(my_rank, inod_dbl, numele, ie,  &
     &                                  interior_ele, iele_dbl)
!
      use find_belonged_process
!
      integer, intent(in) :: my_rank
      type(node_ele_double_number), intent(in) :: inod_dbl
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie(numele,1)
!
      type(node_ele_double_number), intent(inout) :: iele_dbl
      integer(kind = kint), intent(inout) :: interior_ele(numele)
!
      integer(kind = kint) :: iele
      integer(kind = kint) :: ie_one
!
!
!$omp parallel workshare
      iele_dbl%ip_home(1:numele) =   -1
      iele_dbl%id_local(1:numele) =   0
!$omp end parallel workshare
!
!%omp parallel do private(iele,ie_one)
      do iele = 1, numele
        ie_one = ie(iele,1)
        call find_belonged_pe_each_ele                                  &
     &     (inod_dbl%num_dbl, inod_dbl%ip_home, ie_one,                 &
     &      iele_dbl%ip_home(iele), iele_dbl%id_local(iele))
!
        interior_ele(iele)                                              &
     &     = set_each_interior_flag(my_rank, iele_dbl%ip_home(iele))
      end do
!%omp end parallel do
!
      end subroutine find_belonged_pe_4_ele
!
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_surf(my_rank, inod_dbl,             &
     &          numsurf, nnod_4_surf, ie_surf, interior_surf,           &
     &          isurf_dbl)
!
      use t_surface_data
      use find_belonged_process
!
      integer, intent(in) :: my_rank
      type(node_ele_double_number), intent(in) :: inod_dbl
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
!
      type(node_ele_double_number), intent(inout) :: isurf_dbl
      integer(kind = kint), intent(inout) :: interior_surf(numsurf)
!
      integer(kind = kint) :: isurf, nnod_same
      integer(kind = kint) :: ie_surf_one(num_linear_sf)
!
!
!$omp parallel workshare
      isurf_dbl%ip_home(1:numsurf) =   -1
      isurf_dbl%id_local(1:numsurf) =  0
!$omp end parallel workshare
!
!%omp parallel do private(isurf,ie_surf_one,nnod_same)
      do isurf = 1, numsurf
        ie_surf_one(1:num_linear_sf) = ie_surf(isurf,1:num_linear_sf)
        call find_belonged_pe_each_surf                                 &
     &     (inod_dbl%num_dbl, inod_dbl%ip_home, ie_surf_one, nnod_same, &
     &      isurf_dbl%ip_home(isurf), isurf_dbl%id_local(isurf))
!
        interior_surf(isurf)                                            &
     &     = set_each_interior_flag(my_rank, isurf_dbl%ip_home(isurf))
      end do
!%omp end parallel do
!
      end subroutine find_belonged_pe_4_surf
!
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_edge(my_rank, inod_dbl,             &
     &          numedge, nnod_4_edge, ie_edge, interior_edge,           &
     &          iedge_dbl)
!
      use t_edge_data
      use find_belonged_process
!
      integer, intent(in) :: my_rank
      type(node_ele_double_number), intent(in) :: inod_dbl
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      type(node_ele_double_number), intent(inout) :: iedge_dbl
      integer(kind = kint), intent(inout) :: interior_edge(numedge)
!
      integer(kind = kint) :: iedge, nnod_same
      integer(kind = kint) :: ie_edge_one(num_linear_edge)
!
!$omp parallel workshare
      iedge_dbl%ip_home(1:numedge) =   -1
      iedge_dbl%id_local(1:numedge) =   0
!$omp end parallel workshare
!
!%omp parallel do private(iedge,ie_edge_one,nnod_same)
      do iedge = 1, numedge
        ie_edge_one(1) = ie_edge(iedge,1)
        ie_edge_one(2) = ie_edge(iedge,2)
        call find_belonged_pe_each_edge                                 &
     &     (inod_dbl%num_dbl, inod_dbl%ip_home, ie_edge_one, nnod_same, &
     &      iedge_dbl%ip_home(iedge), iedge_dbl%id_local(iedge))
!
        interior_edge(iedge)                                            &
     &     = set_each_interior_flag(my_rank, iedge_dbl%ip_home(iedge))
      end do
!%omp end parallel do
!
      end subroutine find_belonged_pe_4_edge
!
! ----------------------------------------------------------------------
!
      end module t_para_double_numbering
