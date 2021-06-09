!> @file  sleeve_extend.f90
!!      module sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine sleeve_extension_loop                                &
!!     &         (sleeve_exp_p, mesh, group, ele_comm, sleeve_exp_WK)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!        type(communication_table), intent(inout) :: ele_comm
!!        type(sleeve_extension_work), intent(inout) :: sleeve_exp_WK
!!      subroutine extend_mesh_sleeve(sleeve_exp_p, nod_comm, ele_comm, &
!!     &          org_node, org_ele, neib_ele, sleeve_exp_WK,           &
!!     &          new_nod_comm, new_node, new_ele, new_ele_comm,        &
!!     &          mark_saved, iflag_process_extend)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: ele_comm
!!        type(node_data), intent(in) :: org_node
!!        type(element_data), intent(in) :: org_ele
!!        type(sleeve_extension_work), intent(inout) :: sleeve_exp_WK
!!        type(communication_table), intent(inout) :: new_nod_comm
!!        type(node_data), intent(inout) :: new_node
!!        type(element_data), intent(inout) :: new_ele
!!        type(communication_table), intent(inout) :: new_ele_comm
!!        type(mark_for_each_comm), intent(inout) :: mark_saved(nprocs)
!!        integer(kind = kint), intent(inout) :: iflag_process_extend
!!@endverbatim
!
      module sleeve_extend
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_calypso_comm_table
      use t_ctl_param_sleeve_extend
      use t_para_double_numbering
      use t_comm_table_for_each_pe
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sleeve_extension_loop                                  &
     &         (sleeve_exp_p, mesh, group, ele_comm, sleeve_exp_WK)
!
      use t_next_node_ele_4_node
      use t_flags_each_comm_extend
      use calypso_mpi_int8
      use transfer_to_long_integers
      use set_ele_id_4_node_type
      use extended_groups
      use copy_mesh_structures
      use nod_and_ele_derived_info
      use const_element_comm_tables
      use const_nod_ele_to_extend
!
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(communication_table), intent(inout) :: ele_comm
      type(sleeve_extension_work), intent(inout) :: sleeve_exp_WK
!
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save :: newgroup
      type(communication_table), save :: new_ele_comm
!
      type(element_around_node), save :: neib_ele
      type(mark_for_each_comm), allocatable, save :: mark_saved1(:)
!
      integer(kind = kint_gl) :: ntot_numnod, ntot_internal_nod
      integer(kind = kint_gl) :: ntot_numele, ntot_import_ele
      integer(kind = kint) :: iflag_process_extend = 0
      integer(kind = kint) :: iloop, ip
      integer(kind = kint) :: i, ist, num, inum
!
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
      call init_work_vector_sleeve_ext                                  &
     &   (mesh%nod_comm, mesh%node, sleeve_exp_p, sleeve_exp_WK)
!
      call set_ele_id_4_node(mesh%node, mesh%ele, neib_ele)
!
      allocate(mark_saved1(nprocs))
      call init_min_dist_from_import                                    &
     &   (sleeve_exp_p, mesh%nod_comm, mesh%node, mesh%ele, neib_ele,   &
     &    sleeve_exp_WK, mark_saved1)
!
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+5)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+1)
!
      do iloop = 1, 10
        if(iflag_debug.gt.0) write(*,*) 'extend_mesh_sleeve', iloop
        call extend_mesh_sleeve(sleeve_exp_p, mesh%nod_comm, ele_comm,  &
     &      mesh%node, mesh%ele, neib_ele, sleeve_exp_WK,               &
     &      newmesh%nod_comm, newmesh%node, newmesh%ele,                &
     &      new_ele_comm, mark_saved1, iflag_process_extend)
        call s_extended_groups                                          &
     &     (mesh, group, newmesh, new_ele_comm, newgroup)
!
        call dealloc_work_vector_sleeve_ext(sleeve_exp_WK)
        call dealloc_iele_belonged(neib_ele)
        call dealloc_comm_table(ele_comm)
        call dealloc_nod_and_ele_infos(mesh)
        call dealloc_mesh_data(mesh, group)
!
!        if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
        call alloc_sph_node_geometry(newmesh%node)
        call copy_mesh_and_group(newmesh, newgroup, mesh, group)
        call copy_comm_tbl_types(new_ele_comm, ele_comm)
!
        call dealloc_comm_table(new_ele_comm)
        call dealloc_nod_and_ele_infos(newmesh)
        call dealloc_mesh_data(newmesh, newgroup)
!        if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+5)
!
        call calypso_mpi_barrier
        call calypso_mpi_reduce_one_int8(cast_long(mesh%node%numnod),   &
     &                                 ntot_numnod, MPI_SUM, 0)
        call calypso_mpi_reduce_one_int8                                &
     &   (cast_long(mesh%node%internal_node), ntot_internal_nod,        &
     &    MPI_SUM, 0)
        call calypso_mpi_reduce_one_int8(cast_long(mesh%ele%numele),    &
     &                                 ntot_numele, MPI_SUM, 0)
        call calypso_mpi_reduce_one_int8                                &
     &                                (cast_long(ele_comm%ntot_import), &
     &                                 ntot_import_ele, MPI_SUM, 0)
!
        if(my_rank .eq. 0) then
          write(*,*) 'total, internal, sleve node '
          write(*,*) ntot_numnod, ntot_internal_nod,                    &
     &           (ntot_numnod-ntot_internal_nod)
          write(*,*) 'total, internal, sleve element '
          write(*,*) ntot_numele, (ntot_numele-ntot_import_ele),        &
     &            ntot_import_ele
        end if
!
        if(iflag_process_extend .eq. 0) exit
        if(my_rank .eq. 0) write(*,*) 'sleeve extension again'
!
        call set_nod_and_ele_infos(mesh%node, mesh%ele)
        if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
        call set_ele_id_4_node(mesh%node, mesh%ele, neib_ele)
!
        call init_work_vector_sleeve_ext                                &
     &     (mesh%nod_comm, mesh%node, sleeve_exp_p, sleeve_exp_WK)
      end do
!
      do ip = 1, nprocs
        call dealloc_mark_for_each_comm(mark_saved1(ip))
      end do
      deallocate(mark_saved1)
!
      end subroutine sleeve_extension_loop
!
!  ---------------------------------------------------------------------
!
      subroutine extend_mesh_sleeve(sleeve_exp_p, nod_comm, ele_comm,   &
     &          org_node, org_ele, neib_ele, sleeve_exp_WK,             &
     &          new_nod_comm, new_node, new_ele, new_ele_comm,          &
     &          mark_saved, iflag_process_extend)
!
      use t_next_node_ele_4_node
      use t_repart_double_numberings
      use t_mesh_for_sleeve_extend
      use t_trim_overlapped_import
      use t_flags_each_comm_extend
      use t_mark_node_ele_to_extend
      use t_comm_table_for_each_pe
      use t_sort_data_for_sleeve_trim
      use t_work_nod_import_extend
!
      use const_extended_neib_domain
      use const_nod_ele_to_extend
      use const_extend_nod_comm_table
      use const_extend_ele_comm_table
      use append_communication_table
      use append_extended_node
      use append_extended_element
      use checks_for_sleeve_extend
      use check_sleeve_extend_mesh
      use reverse_SR_real
!
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(element_around_node), intent(in) :: neib_ele
      type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!
      type(communication_table), intent(inout) :: new_nod_comm
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
      type(communication_table), intent(inout) :: new_ele_comm
      type(mark_for_each_comm), intent(inout) :: mark_saved(nprocs)
      integer(kind = kint), intent(inout) :: iflag_process_extend
!
      type(node_ele_double_number), save :: inod_dbl
      type(node_ele_double_number), save :: iele_dbl
      type(node_ele_double_number), save :: dbl_id2
!
!>      Structure of double numbering
      type(communication_table), save :: expand_ele_comm
      type(ele_data_for_sleeve_ext), save :: exp_import_ie
      type(ele_data_for_sleeve_ext), save :: trim_import_ie
!
      type(communication_table), save :: expand_nod_comm
      type(node_data_for_sleeve_ext), save :: exp_import_xx
      type(node_data_for_sleeve_ext), save :: trim_import_xx
!
      type(data_for_trim_import), save :: ext_nod_trim
      type(work_nod_import_extend), save :: trim_nod_to_ext
!
      type(mark_for_each_comm), allocatable, save :: mark_nod(:)
      type(mark_for_each_comm), allocatable, save :: mark_ele(:)
!
      type(calypso_comm_table), save :: add_nod_comm
      type(calypso_comm_table), save :: add_ele_comm
!
      integer(kind = kint) :: i
!
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
      call alloc_double_numbering(org_node%numnod, inod_dbl)
      call set_node_double_numbering(org_node, nod_comm, inod_dbl)
!
      call alloc_double_numbering(org_ele%numele, iele_dbl)
      call double_numbering_4_element(org_ele, ele_comm, iele_dbl)
!
      allocate(mark_nod(nod_comm%num_neib))
      allocate(mark_ele(nod_comm%num_neib))
      call const_sleeve_expand_list                                     &
     &   (sleeve_exp_p, nod_comm, ele_comm, org_node, org_ele,          &
     &    neib_ele, sleeve_exp_WK, mark_saved, mark_nod, mark_ele)
!
!
!
      call s_const_extended_neib_domain(nod_comm, inod_dbl,             &
     &    mark_nod, add_nod_comm, iflag_process_extend)
!
      call comm_extended_import_nod_ele                                 &
     &   (nod_comm, org_node, inod_dbl, org_ele, iele_dbl,              &
     &    mark_nod, mark_ele, expand_nod_comm, expand_ele_comm,         &
     &    exp_import_xx, exp_import_ie)
      deallocate(mark_nod, mark_ele)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+1)
!
!const_extended_node_position_org
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+2)
      call alloc_idx_extend_to_trim(expand_nod_comm%ntot_import,        &
     &                              trim_nod_to_ext)
      call const_trimmed_expand_import                                  &
     &   (inod_dbl, nod_comm, expand_nod_comm, exp_import_xx,           &
     &    add_nod_comm, ext_nod_trim, trim_nod_to_ext)
      call const_extended_nod_comm_table(org_node, nod_comm,            &
     &    expand_nod_comm, ext_nod_trim, exp_import_xx,                 &
     &    trim_import_xx, trim_nod_to_ext, add_nod_comm)
!
      call s_append_extended_node(org_node, inod_dbl, add_nod_comm,     &
     &    trim_import_xx, trim_nod_to_ext%import_lc_trimmed,            &
     &    new_node, dbl_id2)
      call dealloc_double_numbering(inod_dbl)
!
      call check_appended_node_data                                     &
     &   (org_node, expand_nod_comm, add_nod_comm, exp_import_xx,       &
     &    ext_nod_trim, trim_import_xx, dbl_id2, trim_nod_to_ext)
      call dealloc_import_lc_trimmed(trim_nod_to_ext)
      call dealloc_node_data_sleeve_ext(trim_import_xx)
!
!
      call append_communication_tbl                                     &
     &   (nod_comm, add_nod_comm, new_nod_comm)
!
      call check_new_node_and_comm(new_nod_comm, new_node, dbl_id2)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+2)
!
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+3)
      call const_extended_element_connect(nod_comm, org_ele, dbl_id2,   &
     &    expand_nod_comm, exp_import_xx,                               &
     &    trim_nod_to_ext%inod_added_import,                            &
     &    expand_ele_comm, exp_import_ie)
!
      call dealloc_double_numbering(dbl_id2)
      call dealloc_idx_extend_to_trim(trim_nod_to_ext)
      call dealloc_node_data_sleeve_ext(exp_import_xx)
      call dealloc_stack_to_trim_extend(ext_nod_trim)
      call dealloc_idx_trimed_to_sorted(ext_nod_trim)
!
      call const_extended_ele_comm_table(org_ele, iele_dbl,             &
     &    nod_comm, ele_comm, add_nod_comm, expand_ele_comm,            &
     &    exp_import_ie, trim_import_ie, add_ele_comm)
      call dealloc_ele_data_sleeve_ext(exp_import_ie)
      call dealloc_comm_table(expand_ele_comm)
!
!
      call append_communication_tbl                                     &
     &   (ele_comm, add_ele_comm, new_ele_comm)
      call s_append_extended_element(my_rank, org_ele, add_ele_comm,    &
     &    trim_import_ie, new_node, new_ele)
!
      call check_returned_extend_element                                &
     &   (iele_dbl, add_ele_comm, trim_import_ie)
      call dealloc_ele_data_sleeve_ext(trim_import_ie)
      call dealloc_double_numbering(iele_dbl)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+3)
!
      if(i_debug .gt. 0) then
        call check_extended_element                                     &
     &     (new_nod_comm, new_node, new_ele, new_ele_comm)
!
        do i = 1, nprocs
          call calypso_mpi_barrier
          if(i .eq. my_rank+1) then
            write(*,'(2i6,a,200i6)') my_rank, nod_comm%num_neib,        &
     &           ' nod_comm%id_neib          ', nod_comm%id_neib
            write(*,'(2i6,a,200i6)') my_rank, new_nod_comm%num_neib,    &
     &           ' new_nod_comm%id_neib    ', new_nod_comm%id_neib
            write(*,'(2i6,a,200i6)') my_rank, new_ele_comm%num_neib,    &
     &           ' new_ele_comm%id_neib    ', new_ele_comm%id_neib
            write(*,'(2i6,a,200i6)') my_rank, add_nod_comm%nrank_import,&
     &         ' add_nod_comm%irank_import ', add_nod_comm%irank_import
            write(*,'(2i6,a,200i6)') my_rank, add_nod_comm%nrank_export,&
     &         ' add_nod_comm%irank_export ', add_nod_comm%irank_export
            write(*,'(i6,a,i6)') my_rank,                               &
     &           ' add_nod_comm%iflag_self_copy  ',                     &
     &            add_nod_comm%iflag_self_copy
            write(*,'(i6,a,i6)') my_rank,                               &
     &           ' iflag_process_extend  ', iflag_process_extend
          end if
          call calypso_mpi_barrier
        end do
      end if
      call dealloc_calypso_comm_table(add_ele_comm)
      call dealloc_calypso_comm_table(add_nod_comm)
!
      end subroutine extend_mesh_sleeve
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_new_node_and_comm(new_comm, new_node, dbl_id2)
!
      use m_solver_SR
      use calypso_mpi_int
      use solver_SR_type
!
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(node_ele_double_number), intent(in) :: dbl_id2
!
      integer(kind = kint), allocatable :: inod_lc_check(:)
      integer(kind = kint), allocatable :: irank_lc_check(:)
!
      integer(kind = kint) :: inod, icou
      integer(kind = kint) :: nerror
!
!
      allocate(inod_lc_check(new_node%numnod))
      allocate(irank_lc_check(new_node%numnod))
      inod_lc_check =   0
      irank_lc_check = -1
!
!$omp parallel do
      do inod = 1, new_node%internal_node
        inod_lc_check(inod) = inod
        irank_lc_check(inod) = my_rank
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, SR_sig1, SR_i1, inod_lc_check)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, SR_sig1, SR_i1, irank_lc_check)
!
      icou = 0
      do inod = new_node%internal_node+1, new_node%numnod
        if(dbl_id2%irank(inod) .ne. irank_lc_check(inod)                &
     &    .and. dbl_id2%index(inod) .ne. inod_lc_check(inod)) then
          if(icou .eq. 0) write(50+my_rank,*) 'error list'
          write(50+my_rank,*) inod, my_rank,                            &
     &     dbl_id2%irank(inod), irank_lc_check(inod),                   &
     &     dbl_id2%index(inod), inod_lc_check(inod)
           icou = icou + 1
        end if
      end do
!
      call calypso_mpi_allreduce_one_int(icou, nerror, MPI_SUM)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &      'Number of wrong communication items:', nerror
!
      deallocate(inod_lc_check, irank_lc_check)
!
      end subroutine check_new_node_and_comm
!
!  ---------------------------------------------------------------------
!
      end module sleeve_extend
