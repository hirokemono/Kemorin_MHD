!> @file  t_mark_node_ele_to_extend.f90
!!      module t_mark_node_ele_to_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Structure of marking and distance for sleeveextension
!!
!!@verbatim
!!      subroutine alloc_istack_mark_ecomm_smp(mark_comm)
!!      subroutine alloc_mark_for_each_comm(mark_comm)
!!      subroutine dealloc_istack_mark_ecomm_smp(mark_comm)
!!      subroutine dealloc_mark_for_each_comm(mark_comm)
!!        integer(kind = kint), intent(in) :: num
!!        type(mark_for_each_comm), intent(inout) :: mark_comm
!!      subroutine copy_mark_for_each_comm(org_mark_comm, new_mark_comm)
!!        type(mark_for_each_comm), intent(in) :: org_mark_comm
!!        type(mark_for_each_comm), intent(inout) :: new_mark_comm
!!
!!      subroutine init_min_dist_from_import(sleeve_exp_p, nod_comm,    &
!!     &          node, ele, neib_ele, sleeve_exp_WK, mark_saved)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(element_around_node), intent(in) :: neib_ele
!!        type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!!        type(mark_for_each_comm), intent(inout) :: mark_saved(nprocs)
!!      subroutine s_mark_node_ele_to_extend                            &
!!     &         (ineib, sleeve_exp_p, nod_comm, ele_comm, node, ele,   &
!!     &          neib_ele, sleeve_exp_WK, each_comm, mark_saved,       &
!!     &          mark_nod, mark_ele, each_exp_flags)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(communication_table), intent(in) :: nod_comm, ele_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(element_around_node), intent(in) :: neib_ele
!!        type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!!        type(comm_table_for_each_pe), intent(inout) :: each_comm
!!        type(mark_for_each_comm), intent(inout) :: mark_saved
!!        type(mark_for_each_comm), intent(inout) :: mark_nod
!!        type(mark_for_each_comm), intent(inout) :: mark_ele
!!        type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!!
!!      subroutine set_distance_to_mark_by_dist(numnod, istack_nod_smp, &
!!     &                                        distance, mark_nod)
!!        integer(kind = kint), intent(in) :: numnod
!!        integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
!!        real(kind = kreal), intent(in) :: distance(numnod)
!!        type(mark_for_each_comm), intent(inout) :: mark_nod
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
      use m_machine_parameter
      use calypso_mpi
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
        integer(kind = kint), allocatable :: istack_marked_smp(:)
        integer(kind = kint), allocatable :: idx_marked(:)
        real(kind = kreal), allocatable :: dist_marked(:)
      end type mark_for_each_comm
!
      private :: mark_by_last_import
      private :: mark_surround_ele_of_import
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_istack_mark_ecomm_smp(mark_comm)
!
      type(mark_for_each_comm), intent(inout) :: mark_comm
!
!
      allocate(mark_comm%istack_marked_smp(0:np_smp))
!
!$omp parallel workshare
      mark_comm%istack_marked_smp(0:np_smp) = 0
!$omp end parallel workshare
!
      end subroutine alloc_istack_mark_ecomm_smp
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_mark_for_each_comm(mark_comm)
!
      type(mark_for_each_comm), intent(inout) :: mark_comm
!
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
      subroutine dealloc_istack_mark_ecomm_smp(mark_comm)
!
      type(mark_for_each_comm), intent(inout) :: mark_comm
!
!
      deallocate(mark_comm%istack_marked_smp)
!
      end subroutine dealloc_istack_mark_ecomm_smp
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
!
!$omp parallel workshare
      new_mark_comm%istack_marked_smp(0:np_smp)                         &
     &   = org_mark_comm%istack_marked_smp(0:np_smp)
!$omp end parallel workshare
!
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
      subroutine init_min_dist_from_import(sleeve_exp_p, nod_comm,      &
     &          node, ele, neib_ele, sleeve_exp_WK, mark_saved)
!
      use t_ctl_param_sleeve_extend
      use t_next_node_ele_4_node
!
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!
      type(mark_for_each_comm), intent(inout) :: mark_saved(nprocs)
!
      integer(kind = kint) :: ineib, ip, max_4_smp
      integer(kind = kint) :: ist, inum, inod
      real(kind= kreal), allocatable :: dist_tmp(:)
!
!
      allocate(dist_tmp(node%numnod))
!
!$omp parallel workshare
      mark_saved(1:nprocs)%num_marked = -1
!$omp end parallel workshare
!
      do ineib = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(ineib) + 1
        ist = nod_comm%istack_export(ineib-1)
        mark_saved(ip)%num_marked = nod_comm%istack_export(ineib) - ist
        call count_number_4_smp                                         &
           (np_smp, ione, mark_saved(ip)%num_marked,                    &
     &      mark_saved(ip)%istack_marked_smp, max_4_smp)
        call alloc_mark_for_each_comm(mark_saved(ip))
!
        call init_min_dist_each_import                                  &
     &     (ineib, sleeve_exp_p, nod_comm, node, ele, neib_ele,         &
     &      sleeve_exp_WK, dist_tmp)
!
!$omp parallel do private(inum,inod)
        do inum = 1, mark_saved(ip)%num_marked
          inod = nod_comm%item_export(ist+inum)
          mark_saved(ip)%idx_marked(inum) =  inod
          mark_saved(ip)%dist_marked(inum) = dist_tmp(inod)
        end do
!$omp end parallel do
      end do
      deallocate(dist_tmp)
!
      do ip = 1, nprocs
        if(mark_saved(ip)%num_marked .eq. -1) then
          mark_saved(ip)%num_marked = 0
          mark_saved(ip)%istack_marked_smp(0:np_smp) = 0
          call alloc_mark_for_each_comm(mark_saved(ip))
        end if
      end do
!
      end subroutine init_min_dist_from_import
!
!  ---------------------------------------------------------------------
!
      subroutine s_mark_node_ele_to_extend                              &
     &         (ineib, sleeve_exp_p, nod_comm, ele_comm, node, ele,     &
     &          neib_ele, sleeve_exp_WK, each_comm, mark_saved,         &
     &          mark_nod, mark_ele, each_exp_flags)
!
      use t_ctl_param_sleeve_extend
      use t_next_node_ele_4_node
!
      integer(kind = kint), intent(in) :: ineib
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(communication_table), intent(in) :: nod_comm, ele_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
      type(mark_for_each_comm), intent(inout) :: mark_saved
      type(mark_for_each_comm), intent(inout) :: mark_nod
      type(mark_for_each_comm), intent(inout) :: mark_ele
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
      integer(kind = kint) :: idummy
!
!
!       Set each_exp_flags%iflag_node = -2 (exclude for check)
!          for imported nodes
      call reset_flags_each_comm_extend                                 &
     &   (node%numnod, ele%numele, each_exp_flags)
      call mark_by_last_import                                          &
     &  (ineib, node, nod_comm, each_exp_flags%iflag_node)
      call set_distance_from_mark_list(-1, mark_saved, each_exp_flags)
      call set_each_export_item(ineib, nod_comm, node,                  &
     &                          each_exp_flags%iflag_node, each_comm)
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
      call count_num_marked_list(-1, node%numnod, node%istack_nod_smp,  &
     &    each_exp_flags%iflag_node, mark_nod%num_marked,               &
     &    mark_nod%istack_marked_smp)
      call alloc_mark_for_each_comm(mark_nod)
      call set_distance_to_mark_list(-1, node, each_exp_flags,          &
     &    mark_nod%num_marked, mark_nod%istack_marked_smp,              &
     &    mark_nod%idx_marked, mark_nod%dist_marked)
!
      call count_num_marked_list( 1, ele%numele, ele%istack_ele_smp,    &
     &    each_exp_flags%iflag_ele, mark_ele%num_marked,                &
     &    mark_ele%istack_marked_smp)
      call alloc_mark_for_each_comm(mark_ele)
      call ele_distance_to_mark_list(1, ele, each_exp_flags,            &
     &    mark_ele%num_marked, mark_ele%istack_marked_smp,              &
     &    mark_ele%idx_marked, mark_ele%dist_marked)
!
      mark_saved%num_marked = mark_nod%num_marked
      call alloc_mark_for_each_comm(mark_saved)
      call copy_mark_for_each_comm(mark_nod, mark_saved)
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
      subroutine set_each_export_item(ineib, nod_comm, node,            &
     &                                iflag_node, each_comm)
!
      integer(kind = kint), intent(in) :: ineib
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: iflag_node(node%numnod)
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
!
      integer(kind = kint) :: inum, inod, ist, ied, jcou
!
!
      jcou = 0
      ist = nod_comm%istack_export(ineib-1) + 1
      ied = nod_comm%istack_export(ineib)
      do inum = ist, ied
        inod = nod_comm%item_export(inum)
        if(iflag_node(inod) .eq. -1) then
          jcou = jcou + 1
          each_comm%item_each_export(jcou) = inod
        end if
      end do
      each_comm%num_each_export = jcou
!
      end subroutine set_each_export_item
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
      subroutine count_num_marked_by_dist(node, distance,               &
     &          num_marked, istack_marked_smp)
!
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: distance(node%numnod)
!
      integer(kind = kint), intent(inout) :: num_marked
      integer(kind = kint), intent(inout) :: istack_marked_smp(0:np_smp)
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
          if(distance(inod) .gt. 0.0d0) icou = icou + 1
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
      subroutine set_distance_to_mark_by_dist(node, distance,           &
     &         num_marked, istack_marked_smp, idx_marked, dist_marked)
!
      type(node_data), intent(in) :: node
      real(kind = kreal), intent(in) :: distance(node%numnod)
!
      integer(kind = kint), intent(in) :: num_marked
      integer(kind = kint), intent(in) :: istack_marked_smp(0:np_smp)
!
      integer(kind = kint), intent(inout) :: idx_marked(num_marked)
      real(kind = kreal), intent(inout) :: dist_marked(num_marked)
!
      integer(kind = kint) :: icou, inod, ist, ied, ip
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
          if(distance(inod) .gt. 0.0d0) then
            icou = icou + 1
            idx_marked(icou) = inod
            dist_marked(icou) = distance(inod)
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_distance_to_mark_by_dist
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
     &         (iflag_ref, ele, each_exp_flags, num_marked,             &
     &          istack_marked_smp, idx_marked, dist_marked)
!
      integer, intent(in) :: iflag_ref
      type(element_data), intent(in) :: ele
      type(flags_each_comm_extend), intent(in) :: each_exp_flags
!
      integer(kind = kint), intent(in) :: num_marked
      integer(kind = kint), intent(in) :: istack_marked_smp(0:np_smp)
!
      integer(kind = kint), intent(inout) :: idx_marked(num_marked)
      real(kind = kreal), intent(inout) :: dist_marked(num_marked)
!
      integer(kind = kint) :: ip, ist, ied, iele
      integer(kind = kint) :: inod, icou, k1
      real(kind = kreal) :: anum
!
!
      if(num_marked .le. 0) return
      anum = one / real(ele%nnod_4_ele)
!
!$omp parallel do private(ip,icou,ist,ied,iele,k1,inod)
      do ip = 1, np_smp
        icou = istack_marked_smp(ip-1)
        ist = ele%istack_ele_smp(ip-1) + 1
        ied = ele%istack_ele_smp(ip  )
        do iele = ist, ied
          if(each_exp_flags%iflag_ele(iele) .eq. iflag_ref) then
            icou = icou + 1
            idx_marked(icou) = iele
            dist_marked(icou) = 0.0d0
            do k1 = 1, ele%nnod_4_ele
              inod = ele%ie(iele,k1)
              dist_marked(icou)                                         &
     &             = dist_marked(icou) + each_exp_flags%distance(inod)
            end do
            dist_marked(icou)                                  &
                 = dist_marked(icou) * anum
          end if
        end do
      end do
!$omp end parallel do
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
      integer(kind = kint) :: icou, iele
!
!
!$omp parallel do private(icou,iele)
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
