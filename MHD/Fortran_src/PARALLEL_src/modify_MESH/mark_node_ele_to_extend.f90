!> @file  mark_node_ele_to_extend.f90
!!      module mark_node_ele_to_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Structure of marking and distance for sleeveextension
!!
!!@verbatim
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
!!     &          mark_nod, mark_ele, each_exp_flags, iflag_exp_ele)
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
!!        integer(kind = kint), intent(inout)                           &
!!     &                                   :: iflag_exp_ele(ele%numele)
!!@endverbatim
!
      module mark_node_ele_to_extend
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
      private :: mark_by_last_import, set_each_export_item
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_min_dist_from_import(sleeve_exp_p, nod_comm,      &
     &          node, ele, neib_ele, sleeve_exp_WK, mark_saved)
!
      use t_ctl_param_sleeve_extend
      use t_next_node_ele_4_node
      use cal_minmax_and_stacks
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
     &          mark_nod, mark_ele, each_exp_flags, iflag_exp_ele)
!
      use t_ctl_param_sleeve_extend
      use t_next_node_ele_4_node
      use element_mark_sleeve_extend
      use load_distance_and_mark_list
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
      integer(kind = kint), intent(inout) :: iflag_exp_ele(ele%numele)
!
      integer(kind = kint) :: idummy
!
!
!       Set each_exp_flags%iflag_node = -2 (exclude for check)
!          for imported nodes
      call reset_flags_each_comm_extend(node%numnod, each_exp_flags)
      call mark_by_last_import                                          &
     &  (ineib, node, nod_comm, each_exp_flags%iflag_node)
      call set_distance_from_mark_list(-1, mark_saved, each_exp_flags)
      call set_each_export_item(ineib, nod_comm, node,                  &
     &                          each_exp_flags%iflag_node, each_comm)
      call mark_surround_ele_of_import(ineib, ele_comm, node, ele,      &
     &    each_exp_flags%iflag_node, iflag_exp_ele)
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
     &      sleeve_exp_WK, each_exp_flags, iflag_exp_ele)
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
      call count_num_marked_list                                        &
     &   ( 1, ele%numele, ele%istack_ele_smp, iflag_exp_ele,            &
     &    mark_ele%num_marked, mark_ele%istack_marked_smp)
      call alloc_mark_for_each_comm(mark_ele)
      call ele_distance_to_mark_list                                    &
     &   (1, ele, each_exp_flags, iflag_exp_ele,                        &
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
      end module mark_node_ele_to_extend
