!> @file  t_mark_node_ele_to_extend.f90
!!      module t_mark_node_ele_to_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Control parameter for sleeve extension
!!
!!@verbatim
!!      subroutine alloc_mark_for_each_comm(num, mark_comm)
!!      subroutine dealloc_mark_for_each_comm(mark_comm)
!!        integer(kind = kint), intent(in) :: num
!!        type(mark_for_each_comm), intent(inout) :: mark_comm
!!
!!      subroutine init_comm_table_for_each                             &
!!     &         (ineib, node, nod_comm, each_comm)
!!      subroutine dealloc_comm_table_for_each(each_comm)
!!
!!      subroutine s_mark_node_ele_to_extend                            &
!!     &         (sleeve_exp_p, node, ele, neib_ele, d_vec, each_comm,  &
!!     &          mark_nod, mark_ele, iflag_ele, iflag_node, distance)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(element_around_node), intent(in) :: neib_ele
!!        real(kind = kreal), intent(in) :: d_vec(node%numnod,3)
!!        type(comm_table_for_each_pe), intent(inout) :: each_comm
!!        type(mark_for_each_comm), intent(inout) :: mark_nod
!!        type(mark_for_each_comm), intent(inout) :: mark_ele
!!        integer(kind = kint), intent(inout) :: iflag_ele(ele%numele)
!!        integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
!!        real(kind = kreal), intent(inout) :: distance(node%numnod)
!!@endverbatim
!
      module t_mark_node_ele_to_extend
!
      use m_precision
      use m_constants
!
      type mark_for_each_comm
        integer(kind = kint) :: nnod_marked = 0
        integer(kind = kint), allocatable :: idx_marked(:)
        real(kind = kreal), allocatable :: dist_marked(:)
      end type mark_for_each_comm
!
      type comm_table_for_each_pe
        integer(kind = kint) :: num_each_export = 0
        integer(kind = kint), allocatable :: item_each_export(:)
!
        integer(kind = kint) :: num_each_import = 0
        integer(kind = kint), allocatable :: item_each_import(:)
!
        integer(kind = kint) :: num_other_import = 0
        integer(kind = kint), allocatable :: item_other_import(:)
      end type comm_table_for_each_pe
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
      mark_comm%nnod_marked = num
!
      allocate(mark_comm%idx_marked(mark_comm%nnod_marked))
      allocate(mark_comm%dist_marked(mark_comm%nnod_marked))
!
!$omp parallel workshare
      mark_comm%idx_marked(1:mark_comm%nnod_marked) = 0
      mark_comm%dist_marked(1:mark_comm%nnod_marked) = 0.0d0
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
      subroutine init_comm_table_for_each                               &
     &         (ineib, node, nod_comm, each_comm)
!
      use t_geometry_data
      use t_comm_table
!
      integer(kind = kint), intent(in) :: ineib
      type(node_data), intent(in) ::                 node
      type(communication_table), intent(in) ::       nod_comm
      type(comm_table_for_each_pe), intent(inout) :: each_comm
!
      integer(kind = kint) :: ist, ied, i, icou, ip
!
!
      allocate(each_comm%item_each_export(node%numnod))
      allocate(each_comm%item_each_import(node%numnod))
      allocate(each_comm%item_other_import(node%numnod))
!
      each_comm%num_each_export = nod_comm%istack_export(ineib)         &
     &                           - nod_comm%istack_export(ineib-1)
!
      ist = nod_comm%istack_export(ineib-1) 
      do i = 1, each_comm%num_each_export
        each_comm%item_each_export(i) = nod_comm%item_export(i+ist)
      end do
!
      each_comm%num_each_import = nod_comm%istack_import(ineib)         &
     &                           - nod_comm%istack_import(ineib-1)
!
      ist = nod_comm%istack_import(ineib-1) 
      do i = 1, each_comm%num_each_import
        each_comm%item_each_import(i) = nod_comm%item_import(i+ist)
      end do
!
      each_comm%num_other_import = nod_comm%ntot_import                 &
     &                            - each_comm%num_each_import
      icou = 0
      do ip = 1, nod_comm%num_neib
        if(ip .eq. ineib) cycle
!
        ist = nod_comm%istack_import(ip-1) + 1
        ied = nod_comm%istack_import(ip)
        do i = ist, ied
          icou = icou + 1
          each_comm%item_other_import(icou) = nod_comm%item_import(i)
        end do
      end do
!
      end subroutine init_comm_table_for_each
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_comm_table_for_each(each_comm)
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
!
      deallocate(each_comm%item_other_import)
      deallocate(each_comm%item_each_import)
      deallocate(each_comm%item_each_export)
!
      end subroutine dealloc_comm_table_for_each
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_mark_node_ele_to_extend                              &
     &         (sleeve_exp_p, node, ele, neib_ele, d_vec, each_comm,    &
     &          mark_nod, mark_ele, iflag_ele, iflag_node, distance)
!
      use t_ctl_param_sleeve_extend
      use t_geometry_data
      use t_comm_table
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
      integer(kind = kint), intent(inout) :: iflag_ele(ele%numele)
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
      real(kind = kreal), intent(inout) :: distance(node%numnod)
!
      integer(kind = kint) :: inum, inod, icou, idummy, jcou, iele
      integer(kind = kint) :: jst, jed, jnum, jnod, jele, k1
      real(kind = kreal) :: dist, anum
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
      do inum = 1, each_comm%num_each_import
        inod = each_comm%item_each_import(inum)
        jst = neib_ele%istack_4_node(inod-1) + 1
        jed = neib_ele%istack_4_node(inod)
        do jnum = jst, jed
          jele = neib_ele%iele_4_node(jnum)
          if(iflag_ele(jele) .gt. 0) cycle
!
          iflag_ele(jele) = 2
          do k1 = 1, ele%nnod_4_ele
            jnod = ele%ie(jele,k1)
            if(iflag_node(jnod) .eq. -2) cycle
!
            iflag_node(jnod) = 1
            dist = distance_select(sleeve_exp_p, inod, jnod,           &
     &                             node, d_vec)
            if(distance(jnod) .eq. 0.0d0) then
              distance(jnod) = dist + distance(inod)
            else
              distance(jnod)                                           &
     &                     = min(dist+distance(inod), distance(jnod))
            end if
          end do
        end do
      end do
!
!$omp parallel do private(inum,inod)
      do inum = 1, each_comm%num_each_export
        inod = each_comm%item_each_export(inum)
        iflag_node(inod) = -1
      end do
!$omp end parallel do
!
      do idummy = 2, 100
        do inum = 1, each_comm%num_each_export
          inod = each_comm%item_each_export(inum)
          jst = neib_ele%istack_4_node(inod-1) + 1
          jed = neib_ele%istack_4_node(inod)
          do jnum = jst, jed
            jele = neib_ele%iele_4_node(jnum)
            if(iflag_ele(jele) .gt. 0) cycle
!
            iflag_ele(jele) = 1
            do k1 = 1, ele%nnod_4_ele
              jnod = ele%ie(jele,k1)
              if(iflag_node(jnod) .ge. 0) then
!
                dist = distance_select(sleeve_exp_p, inod, jnod,        &
     &                                 node, d_vec)
                if(iflag_node(jnod) .eq. 0) then
                  iflag_node(jnod) = 1
                  distance(jnod) = dist + distance(inod)
                else
                  distance(jnod)                                        &
     &                   = min(dist+distance(inod), distance(jnod))
                end if
              end if
            end do
          end do
        end do
!
        jcou = 0
        do inod = 1, node%numnod
          if(iflag_node(inod) .gt. 0) then
            if(distance(inod) .lt. sleeve_exp_p%dist_max) then
              jcou = jcou + 1
              each_comm%item_each_export(jcou) = inod
            end if
            iflag_node(inod) = -1
          end if
        end do
        each_comm%num_each_export = jcou
!        write(*,*) my_rank, 'extend again for ', idummy, &
!     &            each_comm%num_each_export
        if(each_comm%num_each_export .le. 0) exit
      end do
!      write(*,*) my_rank, 'Maximum extend size is ', idummy
!
      mark_nod%nnod_marked = 0
      do inod = 1, node%numnod
        if(iflag_node(inod) .eq. -1) then
          mark_nod%nnod_marked = mark_nod%nnod_marked + 1
        end if
      end do
      allocate(mark_nod%idx_marked(mark_nod%nnod_marked))
      allocate(mark_nod%dist_marked(mark_nod%nnod_marked))
!
      icou = 0
      do inod = 1, node%numnod
        if(iflag_node(inod) .eq. -1) then
          icou = icou + 1
          mark_nod%idx_marked(icou) = inod
          mark_nod%dist_marked(icou) = distance(inod)
!          write(*,*) my_rank, 'mark_nod', inod,                       &
!     &           mark_nod%idx_marked(icou), mark_nod%dist_marked(icou)
        end if
      end do
!
      mark_ele%nnod_marked = 0
      do iele = 1, ele%numele
        if(iflag_ele(iele) .eq. 1) then
          mark_ele%nnod_marked = mark_ele%nnod_marked + 1
        end if
      end do
      allocate(mark_ele%idx_marked(mark_ele%nnod_marked))
      allocate(mark_ele%dist_marked(mark_ele%nnod_marked))
!
      anum = one / real(ele%nnod_4_ele)
      icou = 0
      do iele = 1, ele%numele
        if(iflag_ele(iele) .eq. 1) then
          icou = icou + 1
          mark_ele%idx_marked(icou) = iele
          mark_ele%dist_marked(icou) = 0.0d0
          do k1 = 1, ele%nnod_4_ele
            inod = ele%ie(iele,k1)
            mark_ele%dist_marked(icou)                                  &
                 = mark_ele%dist_marked(icou) + distance(inod)
          end do
          mark_ele%dist_marked(icou)                                    &
                 = mark_ele%dist_marked(icou) * anum
        end if
      end do
!
!
      end subroutine s_mark_node_ele_to_extend
!
!  ---------------------------------------------------------------------
!
      end module t_mark_node_ele_to_extend
