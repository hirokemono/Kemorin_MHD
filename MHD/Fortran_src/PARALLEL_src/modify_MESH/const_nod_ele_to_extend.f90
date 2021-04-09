!> @file  const_nod_ele_to_extend.f90
!!      module const_nod_ele_to_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine alloc_sleeve_extension_marks(nod_comm, marks_4_extend)
!!      subroutine dealloc_sleeve_extension_marks(marks_4_extend)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(marks_for_sleeve_extension),                             &
!!     &                     intent(inout) :: marks_4_extend
!!
!!      subroutine const_sleeve_expand_list                             &
!!     &         (sleeve_exp_p, nod_comm, node, ele, neib_ele,          &
!!     &          dist_4_comm, vect_ref, marks_4_extend)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(element_around_node), intent(in) :: neib_ele
!!        type(dist_from_wall_in_export), intent(in) :: dist_4_comm
!!        type(marks_for_sleeve_extension),                             &
!!     &                     intent(inout) :: marks_4_extend
!!      subroutine comm_extended_import_nod_ele                         &
!!     &         (nod_comm, node, inod_dbl, ele, iele_dbl,              &
!!     &          marks_4_extend, expand_nod_comm, expand_ele_comm,     &
!!     &          exp_import_xx, exp_import_ie)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(node_ele_double_number), intent(in) :: iele_dbl
!!        type(marks_for_sleeve_extension) intent(in) :: marks_4_extend
!!        type(communication_table), intent(inout) :: expand_nod_comm
!!        type(communication_table), intent(inout) :: expand_ele_comm
!!        type(node_data_for_sleeve_ext), intent(inout) :: exp_import_xx
!!        type(ele_data_for_sleeve_ext), intent(inout) :: exp_import_ie
!!@endverbatim
!
      module const_nod_ele_to_extend
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_para_double_numbering
      use t_next_node_ele_4_node
      use t_ctl_param_sleeve_extend
      use t_mark_node_ele_to_extend
      use t_mesh_for_sleeve_extend
!
      implicit none
!
!
      type marks_for_sleeve_extension
        type(mark_for_each_comm), allocatable :: mark_nod(:)
        type(mark_for_each_comm), allocatable :: mark_ele(:)
      end type marks_for_sleeve_extension
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_sleeve_extension_marks(nod_comm, marks_4_extend)
!
      type(communication_table), intent(in) :: nod_comm
      type(marks_for_sleeve_extension),                                 &
     &                     intent(inout) :: marks_4_extend
!
!
      allocate(marks_4_extend%mark_nod(nod_comm%num_neib))
      allocate(marks_4_extend%mark_ele(nod_comm%num_neib))
!
      end subroutine alloc_sleeve_extension_marks
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_sleeve_extension_marks(marks_4_extend)
!
      type(marks_for_sleeve_extension),                                 &
     &                     intent(inout) :: marks_4_extend
!
!
      deallocate(marks_4_extend%mark_nod)
      deallocate(marks_4_extend%mark_ele)
!
      end subroutine dealloc_sleeve_extension_marks
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_sleeve_expand_list                               &
     &         (sleeve_exp_p, nod_comm, node, ele, neib_ele,            &
     &          dist_4_comm, vect_ref, marks_4_extend)
!
      use t_flags_each_comm_extend
!
      use calypso_mpi_int
!
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(dist_from_wall_in_export), intent(in) :: dist_4_comm
      real(kind = kreal), intent(in) :: vect_ref(node%numnod,3)
!
      type(marks_for_sleeve_extension),                                 &
     &                     intent(inout) :: marks_4_extend

      type(flags_each_comm_extend), save :: each_exp_flags
      integer(kind = kint) :: i, icou, jcou
      integer(kind = kint) :: ntot_failed_gl, nele_failed_gl
!
!
      call alloc_flags_each_comm_extend                                 &
     &   (node%numnod, ele%numele, each_exp_flags)
!
      icou = 0
      jcou = 0
      do i = 1, nod_comm%num_neib
        call s_mark_node_ele_to_extend(i, sleeve_exp_p,                 &
     &      nod_comm, node, ele, neib_ele, dist_4_comm, vect_ref,       &
     &      marks_4_extend%mark_nod(i), marks_4_extend%mark_ele(i),     &
     &      each_exp_flags)
!
        call check_missing_connect_to_extend(node, ele,                 &
    &       marks_4_extend%mark_ele(i), each_exp_flags%iflag_node,      &
    &       icou, jcou)
      end do
      call dealloc_flags_each_comm_extend(each_exp_flags)
!
!
      call calypso_mpi_reduce_one_int(icou, ntot_failed_gl, MPI_SUM, 0)
      call calypso_mpi_reduce_one_int(jcou, nele_failed_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Failed element list:',             &
     &                             ntot_failed_gl, nele_failed_gl
!
      write(*,*) my_rank, 'mark_nod%num_marked',                        &
     &         marks_4_extend%mark_nod(1:nod_comm%num_neib)%num_marked, &
     &        ' of ', node%numnod
      write(*,*) my_rank, 'mark_ele%num_marked',                        &
     &         marks_4_extend%mark_ele(1:nod_comm%num_neib)%num_marked, &
     &        ' of ', ele%numele
!
      end subroutine const_sleeve_expand_list
!
!  ---------------------------------------------------------------------
!
      subroutine comm_extended_import_nod_ele                           &
     &         (nod_comm, node, inod_dbl, ele, iele_dbl,                &
     &          marks_4_extend, expand_nod_comm, expand_ele_comm,       &
     &          exp_import_xx, exp_import_ie)
!
      use calypso_mpi_int
      use reverse_SR_int
      use cal_minmax_and_stacks
      use set_mesh_for_sleeve_extend
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(marks_for_sleeve_extension), intent(in) :: marks_4_extend
!
      type(communication_table), intent(inout) :: expand_nod_comm
      type(communication_table), intent(inout) :: expand_ele_comm
      type(node_data_for_sleeve_ext), intent(inout) :: exp_import_xx
      type(ele_data_for_sleeve_ext), intent(inout) :: exp_import_ie
!
      type(node_data_for_sleeve_ext), save :: exp_export_xx
      type(ele_data_for_sleeve_ext), save :: exp_export_ie
!
!
      expand_nod_comm%num_neib = nod_comm%num_neib
      expand_ele_comm%num_neib = nod_comm%num_neib
!
      call alloc_neighbouring_id(expand_nod_comm)
      call alloc_import_num(expand_nod_comm)
      call alloc_export_num(expand_nod_comm)
!
      call alloc_neighbouring_id(expand_ele_comm)
      call alloc_import_num(expand_ele_comm)
      call alloc_export_num(expand_ele_comm)
!
!$omp parallel workshare
      expand_nod_comm%id_neib(1:nod_comm%num_neib)                      &
     &        = nod_comm%id_neib(1:nod_comm%num_neib)
      expand_ele_comm%id_neib(1:nod_comm%num_neib)                      &
     &        = nod_comm%id_neib(1:nod_comm%num_neib)
!$omp end parallel workshare
!
      call count_export_4_expanded_mesh(nod_comm, node,                 &
     &    marks_4_extend%mark_nod, marks_4_extend%mark_ele,             &
     &    expand_nod_comm%num_export, expand_ele_comm%num_export)
      call s_cal_total_and_stacks                                       &
     &   (nod_comm%num_neib, expand_nod_comm%num_export, izero,         &
     &    expand_nod_comm%istack_export, expand_nod_comm%ntot_export)
      call s_cal_total_and_stacks                                       &
     &   (nod_comm%num_neib, expand_ele_comm%num_export, izero,         &
     &    expand_ele_comm%istack_export, expand_ele_comm%ntot_export)

      call num_items_send_recv                                          &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_nod_comm%num_export,                                   &
     &    nod_comm%num_neib, nod_comm%id_neib, izero,                   &
     &    expand_nod_comm%num_import, expand_nod_comm%istack_import,    &
     &    expand_nod_comm%ntot_import)
      call num_items_send_recv                                          &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_ele_comm%num_export,                                   &
     &    nod_comm%num_neib, nod_comm%id_neib, izero,                   &
     &    expand_ele_comm%num_import, expand_ele_comm%istack_import,    &
     &    expand_ele_comm%ntot_import)
!
!
      call alloc_export_item(expand_nod_comm)
      call alloc_node_data_sleeve_ext                                   &
     &   (expand_nod_comm%ntot_export, exp_export_xx)
      call alloc_export_item(expand_ele_comm)
      call alloc_ele_data_sleeve_ext                                    &
     &   (expand_ele_comm%ntot_export, ele%nnod_4_ele, exp_export_ie)
!
      call set_export_4_expanded_mesh                                   &
     &   (nod_comm, node, ele, inod_dbl, iele_dbl,                      &
     &    marks_4_extend%mark_nod, marks_4_extend%mark_ele,             &
     &    expand_nod_comm%ntot_export, expand_nod_comm%istack_export,   &
     &    expand_ele_comm%ntot_export, expand_ele_comm%istack_export,   &
     &    expand_nod_comm%item_export, exp_export_xx,                   &
     &    expand_ele_comm%item_export, exp_export_ie)
!
!
      call alloc_import_item(expand_nod_comm)
      call comm_items_send_recv                                         &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_nod_comm%istack_export, expand_nod_comm%item_export,   &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_nod_comm%istack_import, izero,                         &
     &    expand_nod_comm%item_import)
!
      call alloc_node_data_sleeve_ext(expand_nod_comm%ntot_import,      &
     &                                exp_import_xx)
      call send_extended_node_position(expand_nod_comm,                 &
     &                                 exp_export_xx, exp_import_xx)
      call dealloc_node_data_sleeve_ext(exp_export_xx)
!
!
      call alloc_import_item(expand_ele_comm)
      call alloc_ele_data_sleeve_ext                                    &
     &   (expand_ele_comm%ntot_import, ele%nnod_4_ele, exp_import_ie)
!
      call comm_items_send_recv                                         &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_ele_comm%istack_export, expand_ele_comm%item_export,   &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_ele_comm%istack_import, izero,                         &
     &    expand_ele_comm%item_import)
      call send_extended_element_connect(ele, expand_ele_comm,          &
     &    exp_export_ie, exp_import_ie)
      call dealloc_ele_data_sleeve_ext(exp_export_ie)
!
      end subroutine comm_extended_import_nod_ele
!
!  ---------------------------------------------------------------------
!
      end module const_nod_ele_to_extend
