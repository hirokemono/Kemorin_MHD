!
!      module set_crs_connection
!
!        programmed by H.Matsui on Oct., 2006
!
!      subroutine s_set_crs_connection
!
!      subroutine count_item_crs(numnod, np_smp, inod_fsmp_stack,       &
!     &          ntot_next_nod_4_node, inod_next_stack_4_node,          &
!     &          inod_next_4_node, num_crs_l, num_crs_u)
!        output: num_crs_l, num_crs_u
!      subroutine set_item_crs(numnod, np_smp, inod_fsmp_stack,         &
!     &          ntot_next_nod_4_node, inod_next_stack_4_node,          &
!     &          inod_next_4_node, ntot_crs_l, ntot_crs_u,              &
!     &          istack_crs_l, istack_crs_u, item_crs_l, item_crs_u)
!        output: item_crs_l, item_crs_u
!
      module set_crs_connection
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_crs_connection
!
      use m_geometry_data
      use m_machine_parameter
      use m_next_node_id_4_node
      use m_crs_connect
!
      use cal_minmax_and_stacks
!
!
      call allocate_crs_stack(node1%numnod)
!
      call count_item_crs(node1%numnod, np_smp, inod_smp_stack,         &
     &    neib_nod1%ntot, neib_nod1%istack_next, neib_nod1%inod_next,   &
     &    num_crs_l, num_crs_u)
!
      call s_cal_minmax_and_stacks(node1%numnod, num_crs_l, izero,      &
     &    istack_crs_l, ntot_crs_l, max_crs_l, min_crs_l)
      call s_cal_minmax_and_stacks(node1%numnod, num_crs_u, izero,      &
     &    istack_crs_u, ntot_crs_u, max_crs_u, min_crs_u)
!
      call allocate_crs_connect
!
      call set_item_crs(node1%numnod, np_smp, inod_smp_stack,           &
     &    neib_nod1%ntot, neib_nod1%istack_next, neib_nod1%inod_next,   &
     &    ntot_crs_l, ntot_crs_u, istack_crs_l, istack_crs_u,           &
     &    item_crs_l, item_crs_u)
!
      end subroutine s_set_crs_connection
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_item_crs(numnod, np_smp, inod_fsmp_stack,        &
     &          ntot_next_nod_4_node, inod_next_stack_4_node,           &
     &          inod_next_4_node, num_crs_l, num_crs_u)
!
      integer(kind = kint), intent(in) :: numnod, np_smp
      integer(kind = kint), intent(in) :: inod_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ntot_next_nod_4_node
      integer(kind = kint), intent(in)                                  &
     &      :: inod_next_stack_4_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_next_4_node(ntot_next_nod_4_node)
!
      integer(kind = kint), intent(inout) :: num_crs_l(numnod)
      integer(kind = kint), intent(inout) :: num_crs_u(numnod)
!
      integer(kind = kint) :: ip, ist_smp, ied_smp
      integer(kind = kint) :: inod, ist, ied, inum
      integer(kind = kint) :: icou_l, icou_u
!
!
      num_crs_l(1:numnod) = 0
      num_crs_u(1:numnod) = 0
!
!$omp parallel do                                                       &
!$omp&   private (ist_smp,ied_smp,inod,ist,ied,inum,icou_l,icou_u)
      do ip = 1, np_smp
        ist_smp = inod_fsmp_stack(ip-1) + 1
        ied_smp = inod_fsmp_stack(ip)
        do inod = ist_smp, ied_smp
          ist = inod_next_stack_4_node(inod-1) + 1
          ied = inod_next_stack_4_node(inod)
          do inum = ist, ied
!
            if (inod_next_4_node(inum) .lt. inod) then
              num_crs_l(inod) = num_crs_l(inod) + 1
            else if (inod_next_4_node(inum) .gt. inod) then
              num_crs_u(inod) = num_crs_u(inod) + 1
            end if
!
          end do
        end do
      end do
!$omp end parallel do
!
      end  subroutine count_item_crs
!
!-----------------------------------------------------------------------
!
      subroutine set_item_crs(numnod, np_smp, inod_fsmp_stack,          &
     &          ntot_next_nod_4_node, inod_next_stack_4_node,           &
     &          inod_next_4_node, ntot_crs_l, ntot_crs_u,               &
     &          istack_crs_l, istack_crs_u, item_crs_l, item_crs_u)
!
      integer(kind = kint), intent(in) :: numnod, np_smp
      integer(kind = kint), intent(in) :: inod_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ntot_next_nod_4_node
      integer(kind = kint), intent(in)                                  &
     &      :: inod_next_stack_4_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_next_4_node(ntot_next_nod_4_node)
!
      integer(kind = kint), intent(in) :: ntot_crs_l, ntot_crs_u
      integer(kind = kint), intent(in) :: istack_crs_l(0:numnod)
      integer(kind = kint), intent(in) :: istack_crs_u(0:numnod)
!
      integer(kind = kint), intent(inout) :: item_crs_l(ntot_crs_l)
      integer(kind = kint), intent(inout) :: item_crs_u(ntot_crs_u)
!
      integer(kind = kint) :: ip, ist_smp, ied_smp
      integer(kind = kint) :: inod, ist, ied, inum
      integer(kind = kint) :: icou_l, icou_u
!
!$omp parallel do                                                       &
!$omp&   private (ist_smp,ied_smp,inod,ist,ied,inum,icou_l,icou_u)
      do ip = 1, np_smp
        ist_smp = inod_fsmp_stack(ip-1) + 1
        ied_smp = inod_fsmp_stack(ip)
        do inod = ist_smp, ied_smp
          ist = inod_next_stack_4_node(inod-1) + 1
          ied = inod_next_stack_4_node(inod)
          icou_l = istack_crs_l(inod-1)
          icou_u = istack_crs_u(inod-1)
          do inum = ist, ied
!
            if (inod_next_4_node(inum) .lt. inod) then
              icou_l = icou_l + 1
              item_crs_l(icou_l) = inod_next_4_node(inum)
            else if (inod_next_4_node(inum) .gt. inod) then
              icou_u = icou_u + 1
              item_crs_u(icou_u) = inod_next_4_node(inum)
            end if
!
          end do
        end do
      end do
!$omp end parallel do
!
      end  subroutine set_item_crs
!
!-----------------------------------------------------------------------
!
      end module set_crs_connection
