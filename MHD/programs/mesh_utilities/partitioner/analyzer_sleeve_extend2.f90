!!analyzer_sleeve_extend2.f90
!!
!!      module analyzer_sleeve_extend2
!!
!!      modified by H. Matsui on Aug., 2006 
!!
!!      subroutine initialize_sleeve_extend2
!!      subroutine analyze_sleeve_extend2
!!
!!..................................................
!
      module analyzer_sleeve_extend2
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use t_next_node_ele_4_node
      use t_control_data_4_part
      use t_partitioner_comm_table
      use t_ctl_param_partitioner
      use para_const_kemoview_mesh
      use parallel_sleeve_extension
!
      use mpi_load_mesh_data
      use m_work_time

!
      implicit none
!
      type(ctl_param_partitioner), save, private :: part_p1
      type(control_data_4_partitioner), save, private :: part_ctl1
      type(partitioner_comm_tables), save, private :: comm_part1
!
      type(mesh_data), save, private :: fem_EXT
      type(parallel_make_vierwer_mesh), save, private :: par_viexw_ex
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sleeve_extend2
!
      use m_phys_constants
      use m_default_file_prefix
!
      use nod_phys_send_recv
      use set_parallel_file_name
!
      use parallel_FEM_mesh_init
      use set_control_data_4_part
      use bcast_control_data_4_part
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_sleeve_ext
      call start_elapsed_time(ied_total_elapsed)
!
!     ----- read control data
!
      if(my_rank .eq. 0) call read_control_data_4_part(part_ctl1)
      call bcast_part_control_data(part_ctl1)
!
      call set_control_4_extend_sleeve                                  &
     &   (my_rank, part_ctl1, comm_part1, part_p1)
      call dealloc_ctl_data_4_part(part_ctl1)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%global_mesh_file, nprocs, fem_EXT)
!
!  ------  Initialize data communication for FEM data
!
      if (iflag_debug.gt.0 ) write(*,*) 'init_nod_send_recv'
      call init_nod_send_recv(fem_EXT%mesh)
!
      end subroutine initialize_sleeve_extend2
!
! ----------------------------------------------------------------------
!
      subroutine analyze_sleeve_extend2
!
      integer(kind = kint) :: ilevel
!
!
      call sleeve_extension_loop2(part_p1%n_overlap,                    &
     &                           fem_EXT%mesh, fem_EXT%group)
!
!      call mpi_output_mesh                                             &
!     &   (part_p1%distribute_mesh_file, fem_EXT%mesh, fem_EXT%group)
!      call dealloc_mesh_data(fem_EXT%mesh, fem_EXT%group)
!
!      if (iflag_debug.gt.0) write(*,*) 'pickup_surface_mesh'
!      call pickup_surface_mesh                                         &
!     &   (part_p1%distribute_mesh_file, par_viexw_ex)
!
      call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_sleeve_extend2
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sleeve_extension_loop2(num_level, mesh, group)
!
      use nod_and_ele_derived_info
      use const_element_comm_tables
!
      integer(kind = kint), intent(in) :: num_level
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
      type(communication_table) :: ele_comm
!
!
      if(num_level .le. 1) return
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
      call set_nod_and_ele_infos(mesh%node, mesh%ele)
      call const_ele_comm_table(mesh%node, mesh%nod_comm,               &
     &                          ele_comm, mesh%ele)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+5)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+1)
!
      if(my_rank .eq. 0) write(*,*) 'extend sleeve:'
      call para_sleeve_extension2(mesh, group, ele_comm)
!
      call dealloc_comm_table(ele_comm)
      call dealloc_numele_stack(mesh%ele)
      call dealloc_nod_and_ele_infos(mesh)
!
      end subroutine sleeve_extension_loop2
!
! ----------------------------------------------------------------------
!
      subroutine para_sleeve_extension2(mesh, group, ele_comm)
!
      use t_para_double_numbering
      use t_next_node_ele_4_node
!
      use nod_and_ele_derived_info
      use const_element_comm_tables
      use set_table_4_RHS_assemble
      use extend_comm_table
      use extend_element_connect
      use extend_group_table
      use copy_mesh_structures
      use set_nnod_4_ele_by_type
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(communication_table), intent(inout) :: ele_comm
!
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save :: newgroup
      type(next_nod_ele_table), save :: next_tbl
      type(node_ele_double_number), save :: dbl_id1
!
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
      if (iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (mesh, next_tbl%neib_ele, next_tbl%neib_nod)
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
!
      call alloc_double_numbering(mesh%node%numnod, dbl_id1)
      if (iflag_debug.gt.0) write(*,*) 'set_node_double_numbering'
      call set_node_double_numbering                                    &
     &   (mesh%node, mesh%nod_comm, dbl_id1)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+1)
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+2)
      if (iflag_debug.gt.0) write(*,*) 'extend_node_comm_table2'
      call extend_node_comm_table2                                      &
     &   (mesh%nod_comm, mesh%node, dbl_id1, next_tbl%neib_nod,         &
     &    newmesh%nod_comm, newmesh%node)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+2)
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+3)
!      if (iflag_debug.gt.0) write(*,*) 'extend_ele_connectivity'
!      call extend_ele_connectivity                                     &
!     &   (mesh%nod_comm, ele_comm, mesh%node, mesh%ele,                &
!     &    dbl_id1, next_tbl%neib_ele, newmesh%nod_comm, newmesh%node,  &
!     &    newmesh%ele, iflag_SLEX_time, ist_elapsed_SLEX)
!      newmesh%ele%first_ele_type                                       &
!     &   = set_cube_eletype_from_num(newmesh%ele%nnod_4_ele)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+3)
!
      call dealloc_next_nod_ele_table(next_tbl)
      call dealloc_double_numbering(dbl_id1)
!      call dealloc_comm_table(ele_comm)
      call dealloc_numele_stack(mesh%ele)
      call dealloc_nod_and_ele_infos(mesh)
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
!      call alloc_sph_node_geometry(newmesh%node)
!      call set_nod_and_ele_infos(newmesh%node, newmesh%ele)
!      call const_ele_comm_table(newmesh%node, newmesh%nod_comm,        &
!     &                          ele_comm, newmesh%ele)
!
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+5)
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+4)
!      if (iflag_debug.gt.0) write(*,*) 's_extend_group_table'
!      call s_extend_group_table(nprocs, newmesh%nod_comm, ele_comm,    &
!     &    newmesh%node, newmesh%ele, group, newgroup)
!      call dealloc_mesh_data(mesh, group)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+4)
!
!      if (iflag_debug.gt.0) write(*,*) 'copy_mesh_and_group'
!      call copy_mesh_and_group(newmesh, newgroup, mesh, group)
!      call dup_nod_and_ele_infos(newmesh, mesh)
!
!      call dealloc_numele_stack(newmesh%ele)
!      call dealloc_nod_and_ele_infos(newmesh)
!      call dealloc_mesh_data(newmesh, newgroup)
!
      end subroutine para_sleeve_extension2
!
! ----------------------------------------------------------------------
!
      subroutine extend_node_comm_table2(nod_comm, org_node, dbl_idx,   &
     &          neib_nod, new_comm, new_node)
!
      use t_para_double_numbering
      use t_next_node_ele_4_node
!
      use calypso_mpi_int
      use solver_SR_type
      use extend_comm_table_SR
      use mark_export_nod_ele_extend
      use cal_minmax_and_stacks
      use find_extended_node_and_ele
      use find_extended_comm_table
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: org_node
      type(node_ele_double_number), intent(in) :: dbl_idx
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
!
!>      Structure of double numbering
      type(node_ele_double_number) :: dbl_id2
!
!>      added_comm%item_export :: export table or flag to be added
!>      added_comm%item_import :: import table or flag to be added
      type(communication_table) :: added_comm
      type(node_buffer_2_extend) :: send_nbuf
      type(node_buffer_2_extend) :: recv_nbuf
!
      integer(kind = kint), allocatable :: iflag_recv(:)
      integer(kind = kint), allocatable :: iflag_send(:)
      integer(kind = kint) :: nnod_marked = 0
      integer(kind = kint), allocatable :: inod_marked(:)
      integer(kind = kint), allocatable :: iflag_node(:)
      integer(kind = kint) :: nnod_mark_origin = 0
      integer(kind = kint), allocatable :: inod_mark_origin(:)
      integer(kind = kint) :: nnod_mark_start = 0
      integer(kind = kint), allocatable :: inod_mark_start(:)
!
      integer(kind = kint), allocatable :: inod_import_new(:)
      integer(kind = kint), allocatable :: irank_import_new(:)
      integer(kind = kint), allocatable :: inod_export_new(:)
      integer(kind = kint), allocatable :: irank_export_new(:)
!
      integer(kind = kint) :: inum, inod, i, ip, ist, ied
!
!
!
      allocate(inod_marked(org_node%numnod))
      allocate(iflag_node(org_node%numnod))
!$omp parallel workshare
      inod_marked(1:org_node%numnod) = 0
      iflag_node(1:org_node%numnod) = 0
!$omp end parallel workshare
!
      call alloc_added_comm_table_num(nod_comm, added_comm)
!
!      do i = 1, nod_comm%num_neib
!        nnod_mark_origin = nod_comm%istack_import(i)                   &
!     &                    - nod_comm%istack_import(i-1)
!        ist = nod_comm%istack_import(i-1) 
!        do inum = 1, nnod_mark_origin
!          inod_mark_origin(inum) = nod_comm%item_import(inum+ist)
!        end do
!        nnod_mark_start =  nod_comm%istack_export(i)                   &
!     &                    - nod_comm%istack_export(i-1)
!        ist = nod_comm%istack_export(i-1) 
!        do inum = 1, nnod_mark_start
!          inod_mark_start(inum) = nod_comm%item_export(inum+ist)
!        end do
!
!        call mark_next_node_of_export(neib_nod, org_node%numnod,       &
!     &      nnod_mark_origin, inod_mark_origin,                        &
!     &      nnod_mark_start, inod_mark_start,                          &
!     &      nnod_marked, inod_marked, iflag_node)
!
!        added_comm%num_export(i) = added_comm%num_export(i)            &
!     &                            + nnod_marked
!      end do
!
!      call s_cal_total_and_stacks                                      &
!     &   (added_comm%num_neib, added_comm%num_export, izero,           &
!     &    added_comm%istack_export, added_comm%ntot_export)
!
!      call alloc_export_item(added_comm)
!      call alloc_node_buffer_2_extend                                  &
!     &   (added_comm%ntot_export, send_nbuf)
!
!      do i = 1, nod_comm%num_neib
!        nnod_mark_origin = nod_comm%istack_import(i)                   &
!     &                    - nod_comm%istack_import(i-1)
!        ist = nod_comm%istack_import(i-1) 
!        do inum = 1, nnod_mark_origin
!          inod_mark_origin(inum) = nod_comm%item_import(inum+ist)
!        end do
!        nnod_mark_start =  nod_comm%istack_export(i)                   &
!     &                    - nod_comm%istack_export(i-1)
!        ist = nod_comm%istack_export(i-1) 
!        do inum = 1, nnod_mark_start
!          inod_mark_start(inum) = nod_comm%item_export(inum+ist)
!        end do
!
!        call mark_next_node_of_export(neib_nod, org_node%numnod,       &
!     &      nnod_mark_origin, inod_mark_origin,                        &
!     &      nnod_mark_start, inod_mark_start,                          &
!     &      nnod_marked, inod_marked, iflag_node)
!
!        call copy_node_to_extend_buffer(added_comm%istack_export(i-1), &
!     &     org_node, dbl_idx, nnod_marked, inod_marked, send_nbuf)
!      end do
!      deallocate(inod_marked, iflag_node)
!      deallocate(inod_mark_start, inod_mark_origin)
!
!
!      call SOLVER_SEND_RECV_num_type                                   &
!     &   (added_comm, added_comm%num_export, added_comm%num_import)
!
!      call s_cal_total_and_stacks                                      &
!     &   (added_comm%num_neib, added_comm%num_import, izero,           &
!     &    added_comm%istack_import, added_comm%ntot_import)
!
!      call check_num_of_added_table(my_rank, added_comm)
!
!      call alloc_import_item(added_comm)
!      call alloc_node_buffer_2_extend                                  &
!     &   (added_comm%ntot_import, recv_nbuf)
!
!      call added_geometry_send_recv(added_comm%num_neib,               &
!     &    added_comm%id_neib, added_comm%istack_export,                &
!     &    added_comm%ntot_export, send_nbuf%xx_add,                    &
!     &    added_comm%istack_import, added_comm%ntot_import,            &
!     &    recv_nbuf%xx_add)
!
!      call added_global_id_send_recv(added_comm%num_neib,              &
!     &    added_comm%id_neib, added_comm%istack_export,                &
!     &    added_comm%ntot_export, send_nbuf%inod_gl_add,               &
!     &    added_comm%istack_import, added_comm%ntot_import,            &
!     &    recv_nbuf%inod_gl_add)
!
!      call added_nod_id_send_recv(added_comm%num_neib,                 &
!     &    added_comm%id_neib, added_comm%istack_export,                &
!     &    added_comm%ntot_export, send_nbuf%inod_add,                  &
!     &    added_comm%istack_import, added_comm%ntot_import,            &
!     &    recv_nbuf%inod_add)
!
!      call added_nod_id_send_recv(added_comm%num_neib,                 &
!     &    added_comm%id_neib, added_comm%istack_export,                &
!     &    added_comm%ntot_export, send_nbuf%irank_add,                 &
!     &    added_comm%istack_import, added_comm%ntot_import,            &
!     &    recv_nbuf%irank_add)
!
!      call mark_added_nod_import_to_del                                &
!     &   (org_node%numnod, dbl_idx%index, dbl_idx%irank,               &
!     &    nod_comm%num_neib, nod_comm%id_neib, nod_comm%ntot_import,   &
!     &    nod_comm%istack_import, nod_comm%item_import,                &
!     &    added_comm%num_neib, added_comm%id_neib,                     &
!     &    added_comm%ntot_import, added_comm%istack_import,            &
!     &    recv_nbuf%inod_add, recv_nbuf%irank_add,                     &
!     &    added_comm%item_import)
!
!      call check_added_impoert_items                                   &
!     &   (my_rank, nod_comm, added_comm, dbl_idx, recv_nbuf)
!
!      call added_nod_id_send_recv(added_comm%num_neib,                 &
!     &    added_comm%id_neib, added_comm%istack_import,                &
!     &    added_comm%ntot_import, added_comm%item_import,              &
!     &    added_comm%istack_export, added_comm%ntot_export,            &
!     &    added_comm%item_export)
!
!      call check_delete_from_SR_list                                   &
!     &   (my_rank, added_comm, send_nbuf, recv_nbuf)
!
!      call dealloc_node_buffer_2_extend(send_nbuf)
!
!      call count_nodes_by_extend_sleeve(added_comm, org_node, new_node)
!
!      call alloc_node_geometry_base(new_node)
!      call alloc_double_numbering(new_node%numnod, dbl_id2)
!
!      call set_nodes_by_extend_sleeve(recv_nbuf, org_node, dbl_idx,    &
!     &    added_comm, new_node, dbl_id2)
!
!      call check_nodes_by_extend_sleeve(org_node, new_node, dbl_id2)
!
!      allocate(iflag_recv(0:nprocs-1))
!      allocate(iflag_send(0:nprocs-1))
!      iflag_recv(0:nprocs-1) = 0
!      iflag_send(0:nprocs-1) = 0
!
!      call mark_extended_nod_neib_pe(nprocs, nod_comm, added_comm,     &
!     &    recv_nbuf, iflag_send, iflag_recv)
!
!      do ip = 0, nprocs-1
!        call calypso_mpi_scatter_one_int(iflag_recv(0),                &
!     &                                   iflag_send(ip), ip)
!      end do
!
!      call count_extended_nod_neib_pe                                  &
!     &   (nprocs, iflag_send, iflag_recv, new_comm)
!
!      call alloc_comm_table_num(new_comm)
!
!      call set_extended_nod_neib_pe(nprocs, my_rank,                   &
!     &   iflag_send, iflag_recv, nod_comm, new_comm)
!
!      deallocate(iflag_recv, iflag_send)
!
!      call count_extended_node_import                                  &
!     &   (recv_nbuf, nod_comm, added_comm, new_comm)
!
!      call SOLVER_SEND_RECV_num_type                                   &
!     &   (new_comm, new_comm%num_import, new_comm%num_export)
!
!      call s_cal_total_and_stacks                                      &
!     &   (new_comm%num_neib, new_comm%num_import, izero,               &
!     &    new_comm%istack_import, new_comm%ntot_import)
!      call s_cal_total_and_stacks                                      &
!     &   (new_comm%num_neib, new_comm%num_export, izero,               &
!     &    new_comm%istack_export, new_comm%ntot_export)
!
!      call alloc_comm_table_item(new_comm)
!
!      call set_extended_node_import                                    &
!     &   (recv_nbuf, nod_comm, added_comm, new_comm)
!
!      call dealloc_node_buffer_2_extend(recv_nbuf)
!      call dealloc_comm_table(added_comm)
!
!
!      allocate(inod_import_new(new_comm%ntot_import))
!      allocate(irank_import_new(new_comm%ntot_import))
!      inod_import_new = 0
!      irank_import_new = -1
!
!      allocate(inod_export_new(new_comm%ntot_export))
!      allocate(irank_export_new(new_comm%ntot_export))
!      inod_export_new = 0
!      irank_export_new = -1
!
!      do inum = 1, new_comm%ntot_import
!        inod =  new_comm%item_import(inum)
!        inod_import_new(inum) =  dbl_id2%index(inod)
!        irank_import_new(inum) = dbl_id2%irank(inod)
!      end do
!
!      call added_nod_id_send_recv(new_comm%num_neib, new_comm%id_neib, &
!     &  new_comm%istack_import, new_comm%ntot_import, inod_import_new, &
!     &  new_comm%istack_export, new_comm%ntot_export, inod_export_new)
!
!      call added_nod_id_send_recv(new_comm%num_neib, new_comm%id_neib, &
!     &  new_comm%istack_import, new_comm%ntot_import, irank_import_new,&
!     &  new_comm%istack_export, new_comm%ntot_export, irank_export_new)
!
!      call set_extended_node_export(my_rank, nod_comm, added_comm,     &
!     &          inod_export_new, irank_export_new, new_comm)
!
!      deallocate(inod_export_new, irank_export_new)
!
!      call check_new_node_and_comm2(new_comm, new_node, dbl_id2)
!
      end subroutine extend_node_comm_table2
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_new_node_and_comm2(new_comm, new_node, dbl_id2)
!
      use t_para_double_numbering
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
     &   (new_node%numnod, new_comm, inod_lc_check)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm, irank_lc_check)
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
      if(my_rank .eq. 0) write(*,*)                                     &
     &      'Number of wrong communication items:', nerror
!
      deallocate(inod_lc_check, irank_lc_check)
!
      end subroutine check_new_node_and_comm2
!
!  ---------------------------------------------------------------------
!
      end module analyzer_sleeve_extend2
