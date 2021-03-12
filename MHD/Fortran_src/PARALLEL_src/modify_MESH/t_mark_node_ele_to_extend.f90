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
!!
!!      subroutine s_mark_node_ele_to_extend                            &
!!     &         (sleeve_exp_p, node, ele, neib_ele, d_vec, each_comm,  &
!!     &          mark_nod, mark_ele, each_exp_flags)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(element_around_node), intent(in) :: neib_ele
!!        real(kind = kreal), intent(in) :: d_vec(node%numnod,3)
!!        type(comm_table_for_each_pe), intent(inout) :: each_comm
!!        type(mark_for_each_comm), intent(inout) :: mark_nod
!!        type(mark_for_each_comm), intent(inout) :: mark_ele
!!        type(flags_each_comm_extend), intent(inout) :: each_exp_flags
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
!
      type flags_each_comm_extend
        integer(kind = kint), allocatable :: iflag_ele(:)
        integer(kind = kint), allocatable :: iflag_node(:)
        real(kind = kreal), allocatable :: distance(:)
      end type flags_each_comm_extend
!
      type mark_for_each_comm
        integer(kind = kint) :: num_marked = 0
        integer(kind = kint), allocatable :: idx_marked(:)
        real(kind = kreal), allocatable :: dist_marked(:)
      end type mark_for_each_comm
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_flags_each_comm_extend                           &
     &                           (numnod, numele, each_exp_flags)
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
!
      allocate(each_exp_flags%iflag_node(numnod))
      allocate(each_exp_flags%distance(numnod))
!$omp parallel workshare
      each_exp_flags%iflag_node(1:numnod) = 0
      each_exp_flags%distance(1:numnod) =   0.0d0
!$omp end parallel workshare
!
      allocate(each_exp_flags%iflag_ele(numele))
!$omp parallel workshare
      each_exp_flags%iflag_ele(1:numele) = 0
!$omp end parallel workshare
!
      end subroutine alloc_flags_each_comm_extend
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_flags_each_comm_extend(each_exp_flags)
!
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
!
      deallocate(each_exp_flags%iflag_node)
      deallocate(each_exp_flags%distance)
      deallocate(each_exp_flags%iflag_ele)
!
      end subroutine dealloc_flags_each_comm_extend
!
!  ---------------------------------------------------------------------
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
!  ---------------------------------------------------------------------
!
      subroutine s_mark_node_ele_to_extend                              &
     &         (sleeve_exp_p, node, ele, neib_ele, d_vec, each_comm,    &
     &          mark_nod, mark_ele, each_exp_flags)
!
      use t_ctl_param_sleeve_extend
      use t_next_node_ele_4_node
!
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      real(kind = kreal), intent(in) :: d_vec(node%numnod,3)
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
      type(mark_for_each_comm), intent(inout) :: mark_nod
      type(mark_for_each_comm), intent(inout) :: mark_ele
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
      integer(kind = kint) :: inum, inod, icou, idummy, jcou, iele
      integer(kind = kint) :: jst, jed, jnum, jnod, jele, k1
      real(kind = kreal) :: dist, anum
!
!
      call mark_by_last_import                                          &
     &   (node, each_comm, each_exp_flags%iflag_node)
      call mark_surround_ele_of_import(node, ele,                       &
     &    each_exp_flags%iflag_node, each_exp_flags%iflag_ele)
!
      call cal_min_dist_from_last_import                                &
     &   (sleeve_exp_p, node, ele, neib_ele, each_comm, d_vec,          &
     &    each_exp_flags)
      call mark_by_last_export                                          &
     &   (node, each_comm, each_exp_flags%iflag_node)
!
      do idummy = 2, 100
        call cal_min_dist_from_last_export                              &
     &     (sleeve_exp_p, node, ele, neib_ele, each_comm, d_vec,        &
     &      each_exp_flags)
!
        call set_new_export_to_extend                                   &
     &     (sleeve_exp_p%dist_max, node, each_exp_flags%distance,       &
     &     each_comm%num_each_export, each_comm%item_each_export,       &
     &     each_exp_flags%iflag_node)
!        write(*,*) my_rank, 'extend again for ', idummy, &
!     &            each_comm%num_each_export
        if(each_comm%num_each_export .le. 0) exit
      end do
!      write(*,*) my_rank, 'Maximum extend size is ', idummy
!
      icou = 0
      do inod = 1, node%numnod
        if(each_exp_flags%iflag_node(inod) .eq. -1) icou = icou + 1
      end do
      call alloc_mark_for_each_comm(icou, mark_nod)
!
      icou = 0
      do inod = 1, node%numnod
        if(each_exp_flags%iflag_node(inod) .eq. -1) then
          icou = icou + 1
          mark_nod%idx_marked(icou) = inod
          mark_nod%dist_marked(icou) = each_exp_flags%distance(inod)
!          write(*,*) my_rank, 'mark_nod', inod,                       &
!     &           mark_nod%idx_marked(icou), mark_nod%dist_marked(icou)
        end if
      end do
!
      icou = count_mark_ele_to_extend(ele, each_exp_flags%iflag_ele)
      call alloc_mark_for_each_comm(icou, mark_ele)
      call set_mark_ele_to_extend(node, ele, each_exp_flags, mark_ele)
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
      integer(kind = kint) :: inum, iele, k1, kcou
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
!
      subroutine cal_min_dist_from_last_import                          &
     &         (sleeve_exp_p, node, ele, neib_ele, each_comm, d_vec,    &
     &          each_exp_flags)
!
      use t_ctl_param_sleeve_extend
      use t_comm_table_for_each_pe
      use t_next_node_ele_4_node
!
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(comm_table_for_each_pe), intent(in) :: each_comm
      real(kind = kreal), intent(in) :: d_vec(node%numnod,3)
!
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
      integer(kind = kint) :: inum, inod
      integer(kind = kint) :: jst, jed, jnum, jnod, jele, k1
      real(kind = kreal) :: dist
!
      do inum = 1, each_comm%num_each_import
        inod = each_comm%item_each_import(inum)
        jst = neib_ele%istack_4_node(inod-1) + 1
        jed = neib_ele%istack_4_node(inod)
        do jnum = jst, jed
          jele = neib_ele%iele_4_node(jnum)
          if(each_exp_flags%iflag_ele(jele) .gt. 0) cycle
!
          each_exp_flags%iflag_ele(jele) = 2
          do k1 = 1, ele%nnod_4_ele
            jnod = ele%ie(jele,k1)
            if(each_exp_flags%iflag_node(jnod) .eq. -2) cycle
!
            each_exp_flags%iflag_node(jnod) = 1
            dist = distance_select(sleeve_exp_p, inod, jnod,           &
     &                             node, d_vec)
            if(each_exp_flags%distance(jnod) .eq. 0.0d0) then
              each_exp_flags%distance(jnod)                            &
     &                = dist + each_exp_flags%distance(inod)
            else
              each_exp_flags%distance(jnod)                            &
     &                = min(dist+each_exp_flags%distance(inod),        &
     &                      each_exp_flags%distance(jnod))
            end if
          end do
        end do
      end do
!
      end subroutine cal_min_dist_from_last_import
!
!  ---------------------------------------------------------------------
!
      subroutine cal_min_dist_from_last_export                          &
     &         (sleeve_exp_p, node, ele, neib_ele, each_comm, d_vec,    &
     &          each_exp_flags)
!
      use t_ctl_param_sleeve_extend
      use t_comm_table_for_each_pe
      use t_next_node_ele_4_node
!
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(comm_table_for_each_pe), intent(in) :: each_comm
      real(kind = kreal), intent(in) :: d_vec(node%numnod,3)
!
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
      integer(kind = kint) :: inum, inod, jst, jed, jnum, jele, jnod
      real(kind = kreal) :: dist
!
      do inum = 1, each_comm%num_each_export
        inod = each_comm%item_each_export(inum)
        jst = neib_ele%istack_4_node(inod-1) + 1
        jed = neib_ele%istack_4_node(inod)
        do jnum = jst, jed
          jele = neib_ele%iele_4_node(jnum)
          if(each_exp_flags%iflag_ele(jele) .gt. 0) cycle
!
          each_exp_flags%iflag_ele(jele) = 1
          do k1 = 1, ele%nnod_4_ele
            jnod = ele%ie(jele,k1)
            if(each_exp_flags%iflag_node(jnod) .lt. 0) cycle
!
            dist = distance_select(sleeve_exp_p, inod, jnod,            &
     &                             node, d_vec)
            if(each_exp_flags%iflag_node(jnod) .eq. 0) then
              each_exp_flags%iflag_node(jnod) = 1
              each_exp_flags%distance(jnod)                             &
     &              = dist + each_exp_flags%distance(inod)
            else
              each_exp_flags%distance(jnod)                             &
     &              = min(dist+each_exp_flags%distance(inod),           &
     &                    each_exp_flags%distance(jnod))
            end if
          end do
        end do
      end do
!
      end subroutine cal_min_dist_from_last_export
!
!  ---------------------------------------------------------------------
!
      subroutine mark_by_last_import(node, each_comm, iflag_node)
!
      type(node_data), intent(in) :: node
      type(comm_table_for_each_pe), intent(in) :: each_comm
!
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
!
      integer(kind = kint) :: inum, inod
!
!
!$omp parallel workshare
      iflag_node(1:node%numnod) = 0
!$omp end parallel workshare
!
!$omp parallel do private(inum,inod)
      do inum = 1, each_comm%num_each_import
        inod = each_comm%item_each_import(inum)
        iflag_node(inod) = -2
      end do
!$omp end parallel do
!
      end subroutine mark_by_last_import
!
!  ---------------------------------------------------------------------
!
      subroutine mark_surround_ele_of_import                            &
     &         (node, ele, iflag_node, iflag_ele)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: iflag_node(node%numnod)
!
      integer(kind = kint), intent(inout) :: iflag_ele(ele%numele)
!
      integer(kind = kint) :: iele, k1, inod
!
!
!$omp parallel do private(iele,k1,inod)
      do iele = 1, ele%numele
        iflag_ele(iele) = 2
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
      end subroutine mark_surround_ele_of_import
!
!  ---------------------------------------------------------------------
!
      subroutine mark_by_last_export(node, each_comm, iflag_node)
!
      type(node_data), intent(in) :: node
      type(comm_table_for_each_pe), intent(in) :: each_comm
!
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
!
      integer(kind = kint) :: inum, inod
!
!
!$omp parallel do private(inum,inod)
      do inum = 1, each_comm%num_each_export
        inod = each_comm%item_each_export(inum)
        iflag_node(inod) = -1
      end do
!$omp end parallel do
!
      end subroutine mark_by_last_export
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_export_to_extend(dist_max, node, distance,     &
     &          num_each_export, item_each_export, iflag_node)
!
      real(kind = kreal), intent(in) :: dist_max
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: distance(node%numnod)
!
      integer(kind = kint), intent(inout) :: num_each_export
      integer(kind = kint), intent(inout)                               &
     &                     :: item_each_export(node%numnod)
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
!
      integer(kind = kint) :: jcou, inod
!
!
      jcou = 0
      do inod = 1, node%numnod
        if(iflag_node(inod) .gt. 0) then
          if(distance(inod) .lt. dist_max) then
            jcou = jcou + 1
            item_each_export(jcou) = inod
          end if
          iflag_node(inod) = -1
        end if
      end do
      num_each_export = jcou
!
      end subroutine set_new_export_to_extend
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function count_mark_ele_to_extend            &
     &                                          (ele, iflag_ele)
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in) :: iflag_ele(ele%numele)
!
      integer(kind = kint) :: icou, iele
!
!
      icou = 0
      do iele = 1, ele%numele
        if(iflag_ele(iele) .eq. 1) icou = icou + 1
      end do
      count_mark_ele_to_extend = icou
!
      end function count_mark_ele_to_extend
!
!  ---------------------------------------------------------------------
!
      subroutine set_mark_ele_to_extend                                 &
     &         (node, ele, each_exp_flags, mark_ele)
!
      type(node_data), intent(in) :: node
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
        if(each_exp_flags%iflag_ele(iele) .eq. 1) then
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
      end subroutine set_mark_ele_to_extend
!
!  ---------------------------------------------------------------------
!
      end module t_mark_node_ele_to_extend
