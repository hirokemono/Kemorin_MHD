!>@file   t_belonged_element_4_node.f90
!!@brief  module t_belonged_element_4_node
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine alloc_x_ref_ele(node, belongs)
!!      subroutine alloc_x_ref_surf(node, belongs)
!!      subroutine alloc_x_ref_edge(node, belongs)
!!      subroutine dealloc_x_ref_ele(belongs)
!!      subroutine dealloc_x_ref_surf(belongs)
!!      subroutine adelloc_x_ref_edge(belongs)
!!        type(belonged_table), intent(inout) :: belongs
!!
!!      subroutine sort_inod_4_ele_by_position                          &
!!     &         (nd, numele, x_ele, node, neib_e, x_ref_ele)
!!        integer(kind = kint), intent(in) :: nd
!!        type(node_data), intent(in) :: node
!!      type(element_data), intent(in) :: ele
!!        type(element_around_node), intent(inout) :: neib_e
!!@endverbatim
!
      module t_belonged_element_4_node
!
      use m_precision
      use m_constants
      use t_next_node_ele_4_node
!
      implicit none
!
!
      type belonged_table
!>   Structure of belonged element list for each node
        type(element_around_node) :: blng_ele
!>   Structure of belonged surface list for each node
        type(element_around_node) :: blng_surf
!>   Structure of belonged edge list for each node
        type(element_around_node) :: blng_edge
!
!>   x-position of element list after sorting
        real(kind = kreal), allocatable :: x_ref_ele(:)
!>   x-position of surface list after sorting
        real(kind = kreal), allocatable :: x_ref_surf(:)
!>   x-position of edge list after sorting
        real(kind = kreal), allocatable :: x_ref_edge(:)
!
!>   Structure of belonged element list for each node
        type(element_around_node) :: host_ele
!>   Structure of belonged surface list for each node
        type(element_around_node) :: host_surf
!>   Structure of belonged edge list for each node
        type(element_around_node) :: host_edge
      end type belonged_table
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_x_ref_ele(node, belongs)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(belonged_table), intent(inout) :: belongs
!
      integer(kind = kint) :: num
!
      num = belongs%blng_ele%istack_4_node(node%numnod)
      allocate(belongs%x_ref_ele(num))
!
      if(num .gt. 0) belongs%x_ref_ele = 0.0d0
!
      end subroutine alloc_x_ref_ele
!
!-----------------------------------------------------------------------
!
      subroutine alloc_x_ref_surf(node, belongs)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(belonged_table), intent(inout) :: belongs
!
      integer(kind = kint) :: num
!
      num = belongs%blng_surf%istack_4_node(node%numnod)
      allocate(belongs%x_ref_surf(num))
!
      if(num .gt. 0) belongs%x_ref_surf = 0.0d0
!
      end subroutine alloc_x_ref_surf
!
!-----------------------------------------------------------------------
!
      subroutine alloc_x_ref_edge(node, belongs)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(belonged_table), intent(inout) :: belongs
!
      integer(kind = kint) :: num
!
      num = belongs%blng_edge%istack_4_node(node%numnod)
      allocate(belongs%x_ref_edge(num))
!
      if(num .gt. 0) belongs%x_ref_edge = 0.0d0
!
      end subroutine alloc_x_ref_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_x_ref_ele(belongs)
!
      type(belonged_table), intent(inout) :: belongs
!
      deallocate(belongs%x_ref_ele)
!
      end subroutine dealloc_x_ref_ele
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_x_ref_surf(belongs)
!
      type(belonged_table), intent(inout) :: belongs
!
      deallocate(belongs%x_ref_surf)
!
      end subroutine dealloc_x_ref_surf
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_x_ref_edge(belongs)
!
      type(belonged_table), intent(inout) :: belongs
!
      deallocate(belongs%x_ref_edge)
!
      end subroutine dealloc_x_ref_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine belonged_ele_id_4_node(node, ele, host_ele)
!
      use t_geometry_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(inout) :: host_ele
!
!
      call alloc_numele_belonged(node%numnod, host_ele)
!
      call count_belonged_ele_4_node                                    &
     &   (node%numnod, ele%numele, ele%ie(1,1), ione, ele%numele,       &
     &    host_ele%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    host_ele%nele_4_node, izero, host_ele%istack_4_node,          &
     &    host_ele%ntot, host_ele%nmax, host_ele%nmin)
!
!
      call alloc_iele_belonged(host_ele)
!
      call set_belonged_ele_4_node                                      &
     &   (node%numnod, ele%numele, ele%ie(1,1), ione, ele%numele,       &
     &    host_ele%ntot, host_ele%istack_4_node, host_ele%nele_4_node,  &
     &    host_ele%iele_4_node, host_ele%iconn_4_node)
!
      end subroutine belonged_ele_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine belonged_surf_id_4_node(node, surf, host_surf)
!
      use t_geometry_data
      use t_surface_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) ::    node
      type(surface_data), intent(in) :: surf
      type(element_around_node), intent(inout) :: host_surf
!
!
      call alloc_numele_belonged(node%numnod, host_surf)
!
      call count_belonged_ele_4_node                                    &
     &   (node%numnod, surf%numsurf, surf%ie_surf(1,1),                 &
     &    ione, surf%numsurf, host_surf%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    host_surf%nele_4_node, izero, host_surf%istack_4_node,        &
     &    host_surf%ntot, host_surf%nmax, host_surf%nmin)
!
!
      call alloc_iele_belonged(host_surf)
!
      call set_belonged_ele_4_node                                      &
     &   (node%numnod, surf%numsurf, surf%ie_surf(1,1),                 &
     &    ione, surf%numsurf, host_surf%ntot, host_surf%istack_4_node,  &
     &    host_surf%nele_4_node, host_surf%iele_4_node,                 &
     &    host_surf%iconn_4_node)
!
      end subroutine belonged_surf_id_4_node
!
!-----------------------------------------------------------------------
!
      subroutine belonged_edge_id_4_node(node, edge, host_edge)
!
      use t_geometry_data
      use t_edge_data
      use find_element_id_4_node
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) ::    node
      type(edge_data), intent(in) ::    edge
      type(element_around_node), intent(inout) :: host_edge
!
!
      call alloc_numele_belonged(node%numnod, host_edge)
!
      call count_belonged_ele_4_node                                    &
     &   (node%numnod, edge%numedge, edge%ie_edge(1,1),                 &
     &    ione, edge%numedge, host_edge%nele_4_node)
      call s_cal_minmax_and_stacks(node%numnod,                         &
     &    host_edge%nele_4_node, izero, host_edge%istack_4_node,        &
     &    host_edge%ntot, host_edge%nmax, host_edge%nmin)
!
!
      call alloc_iele_belonged(host_edge)
!
      call set_belonged_ele_4_node                                      &
     &   (node%numnod, edge%numedge, edge%ie_edge(1,1),                 &
     &    ione, edge%numedge, host_edge%ntot, host_edge%istack_4_node,  &
     &    host_edge%nele_4_node, host_edge%iele_4_node,                 &
     &    host_edge%iconn_4_node)
!
      end subroutine belonged_edge_id_4_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sort_inod_4_ele_by_position                            &
     &         (nd, numele, x_ele, node, neib_e, x_ref_ele)
!
      use t_geometry_data
      use quicksort
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: numele
      real(kind = kreal), intent(in) :: x_ele(numele,3)
!
      type(node_data), intent(in) :: node
!
      type(element_around_node), intent(inout) :: neib_e
      real(kind = kreal), intent(inout)                                 &
     &           :: x_ref_ele(neib_e%istack_4_node(node%numnod))
!
      integer(kind = kint) :: inod, jst, jed, num, jnum, jele
!
!
!$omp parallel do private(inod,jst,jed,num,jnum,jele)
      do inod = 1, node%numnod
        jst = neib_e%istack_4_node(inod-1) + 1
        jed = neib_e%istack_4_node(inod)
        num = neib_e%istack_4_node(inod) - neib_e%istack_4_node(inod-1)
        do jnum = jst, jed
          jele = neib_e%iele_4_node(jnum)
          x_ref_ele(jnum) = x_ele(jele,nd)
        end do
 !
        if(num .gt. 1) then
          call quicksort_real_w_index(num, x_ref_ele(jst), ione, num,   &
     &        neib_e%iele_4_node(jst))
        end if
      end do
!$omp end parallel do
!
      end subroutine sort_inod_4_ele_by_position
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_belonged_ele_4_node                              &
     &         (numnod, numele, ie, iele_st, iele_ed, nele_4_node)
!
      integer (kind=kint), intent(in) :: numnod, numele
      integer (kind=kint), intent(in) :: ie(numele,1)
      integer (kind=kint), intent(in) :: iele_st, iele_ed
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
!
      integer (kind = kint) :: inod, iele
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        inod = ie(iele,1)
        nele_4_node(inod) = nele_4_node(inod) + 1
      end do
!
      end  subroutine count_belonged_ele_4_node
!
! -----------------------------------------------------------------------
!
      subroutine set_belonged_ele_4_node(numnod, numele, ie,            &
     &          iele_st, iele_ed, ntot_ele_4_node, iele_stack_4_node,   &
     &          nele_4_node, iele_4_node, iconn_4_node)
!
      use quicksort
!
      integer (kind=kint), intent(in) :: numnod, numele
      integer (kind=kint), intent(in) :: ie(numele,1)
      integer (kind=kint), intent(in) :: iele_st, iele_ed
      integer (kind=kint), intent(in) :: ntot_ele_4_node
      integer (kind=kint), intent(in) :: iele_stack_4_node(0:numnod)
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
      integer (kind=kint), intent(inout)                                &
     &                    :: iele_4_node(ntot_ele_4_node)
      integer (kind=kint), intent(inout)                                &
     &                    :: iconn_4_node(ntot_ele_4_node)
!
      integer (kind = kint) :: inod, iele, icou, ist
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        inod = ie(iele,1)
        nele_4_node(inod) = nele_4_node(inod) + 1
        icou = iele_stack_4_node(inod-1) + nele_4_node(inod)
        iele_4_node(icou) = iele
        iconn_4_node(icou) =  1
      end do
!
!$omp parallel do private(inod,ist)
      do inod = 1, numnod
        ist = iele_stack_4_node(inod-1) + 1
        if(nele_4_node(inod) .gt. 0) then
          call quicksort_w_index(nele_4_node(inod), iele_4_node(ist),   &
     &        ione, nele_4_node(inod), iconn_4_node(ist))
        end if
      end do
!$omp end parallel do
!
      end  subroutine set_belonged_ele_4_node
!
! -----------------------------------------------------------------------
!
      end module t_belonged_element_4_node
