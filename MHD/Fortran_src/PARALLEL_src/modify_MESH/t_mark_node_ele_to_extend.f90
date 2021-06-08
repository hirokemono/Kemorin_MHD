!> @file  t_mark_node_ele_to_extend.f90
!!      module t_mark_node_ele_to_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Structure of marking and distance for sleeveextension
!!
!!@verbatim
!!      subroutine alloc_mark_for_each_comm(num, mark_comm)
!!      subroutine dealloc_mark_for_each_comm(mark_comm)
!!        integer(kind = kint), intent(in) :: num
!!        type(mark_for_each_comm), intent(inout) :: mark_comm
!!      subroutine copy_mark_for_each_comm(org_mark_comm, new_mark_comm)
!!        type(mark_for_each_comm), intent(in) :: org_mark_comm
!!        type(mark_for_each_comm), intent(inout) :: new_mark_comm
!!
!!      subroutine s_mark_node_ele_to_extend(ineib, sleeve_exp_p,       &
!!     &          nod_comm, ele_comm, node, ele, neib_ele,              &
!!     &          dist_4_comm, sleeve_exp_WK, each_comm,                &
!!     &          mark_saved, mark_nod_checked, mark_ele, each_exp_flags)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(communication_table), intent(in) :: nod_comm, ele_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(element_around_node), intent(in) :: neib_ele
!!        type(dist_from_wall_in_export), intent(in) :: dist_4_comm
!!        type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!!        type(comm_table_for_each_pe), intent(inout) :: each_comm
!!        type(mark_for_each_comm), intent(inout) :: mark_saved
!!        type(mark_for_each_comm), intent(inout) :: mark_nod_checked
!!        type(mark_for_each_comm), intent(inout) :: mark_ele
!!        type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!!
!!      subroutine check_missing_connect_to_extend                      &
!!    &          (node, ele, mark_ele, iflag_node, icou_nod, icou_ele)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(mark_for_each_comm), intent(inout) :: mark_ele
!!        integer(kind = kint), intent(in) :: iflag_node(node%numnod)
!!        integer(kind = kint), intent(inout) :: icou_nod, icou_ele
!!@endverbatim
!
      module t_mark_node_ele_to_extend
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_comm_table
      use t_comm_table_for_each_pe
      use t_flags_each_comm_extend
!
      implicit none
!
      type mark_for_each_comm
        integer(kind = kint) :: num_marked = 0
        integer(kind = kint), allocatable :: idx_marked(:)
        real(kind = kreal), allocatable :: dist_marked(:)
      end type mark_for_each_comm
!
      private :: mark_by_last_import, mark_by_last_export
      private :: mark_surround_ele_of_import
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_mark_for_each_comm(num, mark_comm)
!
      integer(kind = kint), intent(in) :: num
      type(mark_for_each_comm), intent(inout) :: mark_comm
!
!
      mark_comm%num_marked = num
!
      allocate(mark_comm%idx_marked(mark_comm%num_marked))
      allocate(mark_comm%dist_marked(mark_comm%num_marked))
!
      if(mark_comm%num_marked .le. 0) return
!$omp parallel workshare
      mark_comm%idx_marked(1:mark_comm%num_marked) = 0
      mark_comm%dist_marked(1:mark_comm%num_marked) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_mark_for_each_comm
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_mark_for_each_comm(mark_comm)
!
      type(mark_for_each_comm), intent(inout) :: mark_comm
!
      deallocate(mark_comm%idx_marked, mark_comm%dist_marked)
!
      end subroutine dealloc_mark_for_each_comm
!
!  ---------------------------------------------------------------------
!
      subroutine copy_mark_for_each_comm(org_mark_comm, new_mark_comm)
!
      type(mark_for_each_comm), intent(in) :: org_mark_comm
      type(mark_for_each_comm), intent(inout) :: new_mark_comm
!
!
      if(new_mark_comm%num_marked .le. 0) return
!$omp parallel workshare
      new_mark_comm%idx_marked(1:new_mark_comm%num_marked)              &
     &   = org_mark_comm%idx_marked(1:new_mark_comm%num_marked)
      new_mark_comm%dist_marked(1:new_mark_comm%num_marked)             &
     &   = org_mark_comm%dist_marked(1:new_mark_comm%num_marked)
!$omp end parallel workshare
!
      end subroutine copy_mark_for_each_comm
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_mark_node_ele_to_extend(ineib, sleeve_exp_p,         &
     &          nod_comm, ele_comm, node, ele, neib_ele,                &
     &          dist_4_comm, sleeve_exp_WK, each_comm,                  &
     &          mark_saved, mark_nod_checked, mark_ele, each_exp_flags)
!
      use calypso_mpi
      use t_ctl_param_sleeve_extend
      use t_next_node_ele_4_node
!
      integer(kind = kint), intent(in) :: ineib
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(communication_table), intent(in) :: nod_comm, ele_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(dist_from_wall_in_export), intent(in) :: dist_4_comm
      type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
      type(mark_for_each_comm), intent(inout) :: mark_saved
      type(mark_for_each_comm), intent(inout) :: mark_nod_checked
      type(mark_for_each_comm), intent(inout) :: mark_ele
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
      type(mark_for_each_comm) :: mark_nod_done
!
      integer(kind = kint) :: inod, icou, idummy
!
!
!       Set each_exp_flags%iflag_node = -2 (exclude for check)
!          for imported nodes
      call mark_by_last_import                                          &
     &  (ineib, node, nod_comm, each_exp_flags%iflag_node)
      call mark_by_last_export                                          &
     &  (sleeve_exp_p%dist_max, ineib, node, nod_comm, dist_4_comm,     &
     &   each_comm, each_exp_flags%distance, each_exp_flags%iflag_node)
      call mark_surround_ele_of_import(ineib, ele_comm, node, ele,      &
     &    each_exp_flags%iflag_node, each_exp_flags%iflag_ele)
!
      call dealloc_mark_for_each_comm(mark_saved)
!
      do idummy = 2, 100
        if(i_debug .gt. 0) write(*,*) my_rank, 'extend loop for ',      &
     &                    idummy, each_comm%num_each_export
        if(each_comm%num_each_export .le. 0) exit
        call cal_min_dist_from_last_export                              &
     &     (sleeve_exp_p, node, ele, neib_ele,                          &
     &      each_comm%num_each_export, each_comm%item_each_export,      &
     &      sleeve_exp_WK, each_exp_flags)
!
        call set_new_export_to_extend                                   &
     &     (sleeve_exp_p%dist_max, node, each_exp_flags%distance,       &
     &     each_comm%num_each_export, each_comm%item_each_export,       &
     &     each_exp_flags%iflag_node)
      end do
!      write(*,*) my_rank, 'Maximum extend size is ', idummy
!
!
      icou = count_num_marked_list(-2, node%numnod,                     &
     &                             each_exp_flags%iflag_node)
      call alloc_mark_for_each_comm(icou, mark_nod_done)
      call set_distance_to_mark_list                                    &
     &   (-2, node%numnod, each_exp_flags, mark_nod_done)
!
      icou = count_num_marked_list(-1, node%numnod,                     &
     &                             each_exp_flags%iflag_node)
      call alloc_mark_for_each_comm(icou, mark_nod_checked)
      call set_distance_to_mark_list                                    &
     &   (-1, node%numnod, each_exp_flags, mark_nod_checked)
!
      icou = count_num_marked_list( 1, ele%numele,                      &
     &                             each_exp_flags%iflag_ele)
      call alloc_mark_for_each_comm(icou, mark_ele)
      call ele_distance_to_mark_list                                    &
     &   ( 1, ele, each_exp_flags, mark_ele)
!
      call alloc_mark_for_each_comm(mark_nod_checked%num_marked,        &
     &                              mark_saved)
      call copy_mark_for_each_comm(mark_nod_checked, mark_saved)
!
!      call dealloc_mark_for_each_comm(mark_ele)
!      call dealloc_mark_for_each_comm(mark_nod_checked)
      call dealloc_mark_for_each_comm(mark_nod_done)
!
      end subroutine s_mark_node_ele_to_extend
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_missing_connect_to_extend                        &
    &          (node, ele, mark_ele, iflag_node, icou_nod, icou_ele)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(mark_for_each_comm), intent(inout) :: mark_ele
      integer(kind = kint), intent(in) :: iflag_node(node%numnod)
!
      integer(kind = kint), intent(inout) :: icou_nod, icou_ele
!
      integer(kind = kint) :: inum, iele, k1, kcou, inod
!
      do inum = 1, mark_ele%num_marked
        iele = mark_ele%idx_marked(inum)
        kcou = 0
        do k1 = 1, ele%nnod_4_ele
          inod = ele%ie(iele,k1)
          if(iflag_node(inod) .ge. 0) kcou = kcou + 1
        end do
        icou_nod = icou_nod + kcou
        if(kcou .gt. 0) then
          icou_ele = icou_ele + 1
!          write(*,*) iele, ele%ie(iele,1:ele%nnod_4_ele),              &
!     &                iflag_node(ele%ie(iele,1:ele%nnod_4_ele))
        end if
      end do
!
      end subroutine check_missing_connect_to_extend
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mark_by_last_import                                    &
     &         (ineib, node, nod_comm, iflag_node)
!
      integer(kind = kint), intent(in) :: ineib
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
!
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
!
      integer(kind = kint) :: inum, inod, ist, ied
!
!
!$omp parallel workshare
      iflag_node(1:node%numnod) = 0
!$omp end parallel workshare
!
      ist = nod_comm%istack_import(ineib-1) + 1
      ied = nod_comm%istack_import(ineib)
!$omp parallel do private(inum,inod)
      do inum = ist, ied
        inod = nod_comm%item_import(inum)
        iflag_node(inod) = -2
      end do
!$omp end parallel do
!
      end subroutine mark_by_last_import
!
!  ---------------------------------------------------------------------
!
      subroutine mark_by_last_export                                    &
     &         (dist_max, ineib, node, nod_comm, dist_4_comm,           &
     &          each_comm, distance, iflag_node)
!
      real(kind = kreal), intent(in) :: dist_max
      integer(kind = kint), intent(in) :: ineib
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(dist_from_wall_in_export), intent(in) :: dist_4_comm
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
      real(kind = kreal), intent(inout) :: distance(node%numnod)
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
!
      integer(kind = kint) :: inum, inod, ist, ied, jcou
!
!
!$omp parallel workshare
      distance(1:node%numnod) = 0.0d0
!$omp end parallel workshare
!
      ist = nod_comm%istack_export(ineib-1) + 1
      ied = nod_comm%istack_export(ineib)
!$omp parallel do private(inum,inod)
      do inum = ist, ied
        inod = nod_comm%item_export(inum)
        distance(inod) = dist_4_comm%distance_in_export(inum)
        if(distance(inod) .lt. dist_max) then
          iflag_node(inod) = -1
        else
          iflag_node(inod) = -2
        end if
      end do
!$omp end parallel do
!
      jcou = 0
      do inum = ist, ied
        inod = nod_comm%item_export(inum)
        if(iflag_node(inod) .eq. -1) then
          jcou = jcou + 1
          each_comm%item_each_export(jcou) = inod
        end if
      end do
      each_comm%num_each_export = jcou
!
      end subroutine mark_by_last_export
!
!  ---------------------------------------------------------------------
!
      subroutine mark_surround_ele_of_import                            &
     &         (ineib, ele_comm, node, ele, iflag_node, iflag_ele)
!
      integer(kind = kint), intent(in) :: ineib
      type(communication_table), intent(in) :: ele_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: iflag_node(node%numnod)
!
      integer(kind = kint), intent(inout) :: iflag_ele(ele%numele)
!
      integer(kind = kint) :: iele, k1, inod, ist, ied, inum
!
!
!$omp parallel workshare
      iflag_ele(1:ele%numele) = 2
!$omp end parallel workshare
!
!$omp parallel do private(iele,k1,inod)
      do iele = 1, ele%numele
        do k1 = 1, ele%nnod_4_ele
          inod = ele%ie(iele,k1)
          if(iflag_node(inod) .gt. -2) then
            iflag_ele(iele) = 0
            exit
          end if
        end do
      end do
!$omp end parallel do
!
      ist = ele_comm%istack_import(ineib-1) + 1
      ied = ele_comm%istack_import(ineib)
!$omp parallel do private(iele,inum)
      do inum = ist, ied
        iele = ele_comm%item_import(inum)
        iflag_ele(iele) = 2
      end do
!$omp end parallel do
!
      end subroutine mark_surround_ele_of_import
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function count_num_marked_list               &
     &         (iflag_ref, numnod, iflag_node)
!
      integer, intent(in) :: iflag_ref
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: iflag_node(numnod)
!
      integer(kind = kint) :: icou, inod
!
!
      icou = 0
      do inod = 1, numnod
        if(iflag_node(inod) .eq. iflag_ref) icou = icou + 1
      end do
      count_num_marked_list = icou
!
      end function count_num_marked_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_distance_to_mark_list                              &
     &         (iflag_ref, numnod, each_exp_flags, mark_nod)
!
      integer, intent(in) :: iflag_ref
      integer(kind = kint), intent(in) :: numnod
      type(flags_each_comm_extend), intent(in) :: each_exp_flags
!
      type(mark_for_each_comm), intent(inout) :: mark_nod
!
      integer(kind = kint) :: icou, inod
!
!
      icou = 0
      do inod = 1, numnod
        if(each_exp_flags%iflag_node(inod) .eq. iflag_ref) then
          icou = icou + 1
          mark_nod%idx_marked(icou) = inod
          mark_nod%dist_marked(icou) = each_exp_flags%distance(inod)
!          write(*,*) my_rank, 'mark_nod', inod,                       &
!     &           mark_nod%idx_marked(icou), mark_nod%dist_marked(icou)
        end if
      end do
!
      end subroutine set_distance_to_mark_list
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
      subroutine ele_distance_to_mark_list                              &
     &         (iflag_ref, ele, each_exp_flags, mark_ele)
!
      integer, intent(in) :: iflag_ref
      type(element_data), intent(in) :: ele
      type(flags_each_comm_extend), intent(in) :: each_exp_flags
!
      type(mark_for_each_comm), intent(inout) :: mark_ele
!
      integer(kind = kint) :: inod, icou, iele, k1
      real(kind = kreal) :: anum
!
!
      anum = one / real(ele%nnod_4_ele)
      icou = 0
      do iele = 1, ele%numele
        if(each_exp_flags%iflag_ele(iele) .eq. iflag_ref) then
          icou = icou + 1
          mark_ele%idx_marked(icou) = iele
          mark_ele%dist_marked(icou) = 0.0d0
          do k1 = 1, ele%nnod_4_ele
            inod = ele%ie(iele,k1)
            mark_ele%dist_marked(icou) = mark_ele%dist_marked(icou)     &
                                      + each_exp_flags%distance(inod)
          end do
          mark_ele%dist_marked(icou)                                    &
                 = mark_ele%dist_marked(icou) * anum
        end if
      end do
!
      end subroutine ele_distance_to_mark_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_mark_from_mark_list                            &
     &         (iflag_ref, mark_ele, each_exp_flags)
!
      integer, intent(in) :: iflag_ref
      type(mark_for_each_comm), intent(in) :: mark_ele
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
!
      integer(kind = kint) :: inod, icou, iele, k1
      real(kind = kreal) :: anum
!
!
!$omp parallel do private(icou,inod)
      do icou = 1, mark_ele%num_marked
        iele = mark_ele%idx_marked(icou)
        each_exp_flags%iflag_ele(iele) = iflag_ref
      end do
!$omp end parallel do
!
      end subroutine set_ele_mark_from_mark_list
!
!  ---------------------------------------------------------------------
!
      end module t_mark_node_ele_to_extend
