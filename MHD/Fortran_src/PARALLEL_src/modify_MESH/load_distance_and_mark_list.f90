!> @file  load_distance_and_mark_list.f90
!!      module load_distance_and_mark_list
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Structure of marking and distance for sleeveextension
!!
!!@verbatim
!!      subroutine count_num_marked_by_dist(node, each_exp_flags,       &
!!     &          num_marked, istack_marked_smp)
!!        type(node_data), intent(in) :: node
!!        type(flags_each_comm_extend), intent(in) :: each_exp_flags
!!        integer(kind = kint), intent(inout) :: num_marked
!!        integer(kind = kint), intent(inout) :: num_marked
!!        integer(kind = kint), intent(inout)                           &
!!     &                     :: istack_marked_smp(0:np_smp)
!!      subroutine count_num_marked_list                                &
!!     &         (iflag_ref, numnod, istack_nod_smp, iflag_node,        &
!!     &          num_marked, istack_marked_smp)
!!        integer, intent(in) :: iflag_ref
!!        integer(kind = kint), intent(in) :: numnod
!!        integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
!!        integer(kind = kint), intent(in) :: iflag_node(numnod)
!!        integer(kind = kint), intent(inout) :: num_marked
!!        integer(kind = kint), intent(inout)                           &
!!       &                     :: istack_marked_smp(0:np_smp)
!!      subroutine set_distance_to_mark_list                            &
!!     &         (iflag_ref, node, each_exp_flags, num_marked,          &
!!     &          istack_marked_smp, idx_marked, dist_marked)
!!        integer, intent(in) :: iflag_ref
!!        type(node_data), intent(in) :: node
!!        type(flags_each_comm_extend), intent(in) :: each_exp_flags
!!        integer(kind = kint), intent(in) :: num_marked
!!        integer(kind = kint), intent(in) :: istack_marked_smp(0:np_smp)
!!        integer(kind = kint), intent(inout) :: idx_marked(num_marked)
!!        real(kind = kreal), intent(inout) :: dist_marked(num_marked)
!!      subroutine set_distance_to_mark_by_dist(numnod, istack_nod_smp, &
!!     &                                        distance, mark_nod)
!!        integer(kind = kint), intent(in) :: numnod
!!        integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
!!        real(kind = kreal), intent(in) :: distance(numnod)
!!        type(mark_for_each_comm), intent(inout) :: mark_nod
!!      subroutine set_distance_from_mark_list                          &
!!     &         (iflag_ref, mark_nod, each_exp_flags)
!!        integer, intent(in) :: iflag_ref
!!        type(mark_for_each_comm), intent(in) :: mark_nod
!!        type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!!      subroutine set_distance_from_intenal_mark                       &
!!     &         (internal_node, mark_nod, each_exp_flags)
!!        integer(kind = kint), intent(in) :: internal_node
!!        type(mark_for_each_comm), intent(in) :: mark_nod
!!        type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!!@endverbatim
!
      module load_distance_and_mark_list
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_geometry_data
      use t_comm_table
      use t_comm_table_for_each_pe
      use t_flags_each_comm_extend
      use t_mark_node_ele_to_extend
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_marked_by_dist(node, each_exp_flags,         &
     &          num_marked, istack_marked_smp)
!
      type(node_data), intent(in) :: node
      type(flags_each_comm_extend), intent(in) :: each_exp_flags
!
      integer(kind = kint), intent(inout) :: num_marked
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_marked_smp(0:np_smp)
!
      integer(kind = kint) :: icou, inod, ist, ied, ip
!
!
!$omp parallel do private(ip,icou,ist,ied,inod)
      do ip = 1, np_smp
        icou = 0
        ist = node%istack_nod_smp(ip-1) + 1
        ied = node%istack_nod_smp(ip  )
        do inod = ist, ied
          if(each_exp_flags%distance(inod) .gt. 0.0d0) icou = icou + 1
        end do
        istack_marked_smp(ip) = icou
      end do
!$omp end parallel do
!
      istack_marked_smp(0) = 0
      do ip = 1, np_smp
        istack_marked_smp(ip) = istack_marked_smp(ip-1)                 &
     &                         + istack_marked_smp(ip)
      end do
      num_marked = istack_marked_smp(np_smp)
!
      end subroutine count_num_marked_by_dist
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_marked_list                                  &
     &         (iflag_ref, numnod, istack_nod_smp, iflag_node,          &
     &          num_marked, istack_marked_smp)
!
      integer, intent(in) :: iflag_ref
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
      integer(kind = kint), intent(in) :: iflag_node(numnod)
!
      integer(kind = kint), intent(inout) :: num_marked
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_marked_smp(0:np_smp)
!
      integer(kind = kint) :: icou, inod, ip, ist, ied
!
!
!$omp parallel do private(ip,icou,ist,ied,inod)
      do ip = 1, np_smp
        icou = 0
        ist = istack_nod_smp(ip-1) + 1
        ied = istack_nod_smp(ip  )
        do inod = ist, ied
          if(iflag_node(inod) .eq. iflag_ref) icou = icou + 1
        end do
        istack_marked_smp(ip) = icou
      end do
!$omp end parallel do
!
      istack_marked_smp(0) = 0
      do ip = 1, np_smp
        istack_marked_smp(ip) = istack_marked_smp(ip-1)                 &
     &                         + istack_marked_smp(ip)
      end do
      num_marked = istack_marked_smp(np_smp)
!
      end subroutine count_num_marked_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_distance_to_mark_list                              &
     &         (iflag_ref, node, each_exp_flags, num_marked,            &
     &          istack_marked_smp, idx_marked, dist_marked)
!
      integer, intent(in) :: iflag_ref
      type(node_data), intent(in) :: node
      type(flags_each_comm_extend), intent(in) :: each_exp_flags
      integer(kind = kint), intent(in) :: num_marked
      integer(kind = kint), intent(in) :: istack_marked_smp(0:np_smp)
!
      integer(kind = kint), intent(inout) :: idx_marked(num_marked)
      real(kind = kreal), intent(inout) :: dist_marked(num_marked)
!
      integer(kind = kint) :: icou, inod, ip, ist, ied
!
!
      if(num_marked .le. 0) return
!
!$omp parallel do private(ip,icou,ist,ied,inod)
      do ip = 1, np_smp
        icou = istack_marked_smp(ip-1)
        ist = node%istack_nod_smp(ip-1) + 1
        ied = node%istack_nod_smp(ip  )
        do inod = ist, ied
          if(each_exp_flags%iflag_node(inod) .eq. iflag_ref) then
            icou = icou + 1
            idx_marked(icou) = inod
            dist_marked(icou) = each_exp_flags%distance(inod)
!            write(*,*) my_rank, 'mark_nod', inod,                     &
!     &           idx_marked(icou), dist_marked(icou)
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_distance_to_mark_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_distance_to_mark_by_dist                           &
     &         (node, each_exp_flags, num_marked,                       &
     &          istack_marked_smp, idx_marked, dist_marked)
!
      type(node_data), intent(in) :: node
      type(flags_each_comm_extend), intent(in) :: each_exp_flags
      integer(kind = kint), intent(in) :: num_marked
      integer(kind = kint), intent(in) :: istack_marked_smp(0:np_smp)
!
      integer(kind = kint), intent(inout) :: idx_marked(num_marked)
      real(kind = kreal), intent(inout) :: dist_marked(num_marked)
!
      integer(kind = kint) :: icou, inod, ip, ist, ied
!
!
      if(num_marked .le. 0) return
!
!$omp parallel do private(ip,icou,ist,ied,inod)
      do ip = 1, np_smp
        icou = istack_marked_smp(ip-1)
        ist = node%istack_nod_smp(ip-1) + 1
        ied = node%istack_nod_smp(ip  )
        do inod = ist, ied
          if(each_exp_flags%distance(inod) .gt. 0.0d0) then
            icou = icou + 1
            idx_marked(icou) = inod
            dist_marked(icou) = each_exp_flags%distance(inod)
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_distance_to_mark_by_dist
!
!  ---------------------------------------------------------------------
!
      subroutine set_distance_from_mark_list                            &
     &         (iflag_ref, mark_nod, each_exp_flags)
!
      integer, intent(in) :: iflag_ref
      type(mark_for_each_comm), intent(in) :: mark_nod
!
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
      integer(kind = kint) :: icou, inod
!
!
!$omp parallel do private(icou,inod)
      do icou = 1, mark_nod%num_marked
        inod = mark_nod%idx_marked(icou)
        each_exp_flags%iflag_node(inod) = iflag_ref
        each_exp_flags%distance(inod) = mark_nod%dist_marked(icou)
      end do
!$omp end parallel do
!
      end subroutine set_distance_from_mark_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_distance_from_intenal_mark                         &
     &         (internal_node, mark_nod, each_exp_flags)
!
      integer(kind = kint), intent(in) :: internal_node
      type(mark_for_each_comm), intent(in) :: mark_nod
!
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
      integer(kind = kint) :: icou, inod
!
!
!$omp parallel do private(icou,inod)
      do icou = 1, mark_nod%num_marked
        inod = mark_nod%idx_marked(icou)
        if(inod .gt. internal_node) cycle
!
        each_exp_flags%iflag_node(inod) = -1
        each_exp_flags%distance(inod) = mark_nod%dist_marked(icou)
      end do
!$omp end parallel do
!
      end subroutine set_distance_from_intenal_mark
!
!  ---------------------------------------------------------------------
!
      end module load_distance_and_mark_list
