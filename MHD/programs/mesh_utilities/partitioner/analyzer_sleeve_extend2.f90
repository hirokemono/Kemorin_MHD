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
      use t_repart_double_numberings
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
      use mark_export_nod_ele_extend
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(communication_table), intent(inout) :: ele_comm
!
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save :: newgroup
      type(next_nod_ele_table), save :: next_tbl
      type(node_ele_double_number), save :: inod_dbl_org
      type(node_ele_double_number), save :: iele_dbl_org
      type(dist_from_wall_in_export) :: dist_4_comm
!
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
      if (iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (mesh, next_tbl%neib_ele, next_tbl%neib_nod)
!
      call alloc_double_numbering(mesh%node%numnod, inod_dbl_org)
      if (iflag_debug.gt.0) write(*,*) 'set_node_double_numbering'
      call set_node_double_numbering                                    &
     &   (mesh%node, mesh%nod_comm, inod_dbl_org)
!
      call alloc_double_numbering(mesh%ele%numele, iele_dbl_org)
      call double_numbering_4_element(mesh%ele, ele_comm, iele_dbl_org)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+1)
!
!
      dist_4_comm%ntot = mesh%nod_comm%ntot_export
      allocate(dist_4_comm%distance_in_export(dist_4_comm%ntot))
!$omp parallel workshare
      dist_4_comm%distance_in_export(1:dist_4_comm%ntot) = 0.0d0
!$omp end parallel workshare
!
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+2)
      if (iflag_debug.gt.0) write(*,*) 'extend_node_comm_table2'
      call extend_node_comm_table2                                     &
     &   (mesh%nod_comm, mesh%node, inod_dbl_org,                      &
     &    mesh%ele, iele_dbl_org, next_tbl%neib_ele,                   &
     &    newmesh%nod_comm, newmesh%node, dist_4_comm)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+2)
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+3)
!      if (iflag_debug.gt.0) write(*,*) 'extend_ele_connectivity'
!      call extend_ele_connectivity                                     &
!     &   (mesh%nod_comm, ele_comm, mesh%node, mesh%ele,                &
!     &    inod_dbl_org, next_tbl%neib_ele, newmesh%nod_comm, newmesh%node,  &
!     &    newmesh%ele, iflag_SLEX_time, ist_elapsed_SLEX)
!      newmesh%ele%first_ele_type                                       &
!     &   = set_cube_eletype_from_num(newmesh%ele%nnod_4_ele)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+3)
!
      call dealloc_next_nod_ele_table(next_tbl)
      call dealloc_double_numbering(inod_dbl_org)
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
      subroutine extend_node_comm_table2(nod_comm, org_node, inod_dbl,  &
     &          org_ele, iele_dbl, neib_ele, new_comm, new_node, dist_4_comm)
!
      use t_next_node_ele_4_node
!
      use calypso_mpi_int
      use solver_SR_type
      use extend_comm_table_SR
      use mark_export_nod_ele_extend
      use cal_minmax_and_stacks
      use find_extended_node_and_ele
      use find_extended_comm_table
      use extend_comm_table
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(element_around_node), intent(in) :: neib_ele
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
      type(dist_from_wall_in_export), intent(inout) :: dist_4_comm
!
      real(kind = kreal) :: dist_max = 0.05d0
!>      Structure of double numbering
      type(node_ele_double_number) :: dbl_id2
!
!>      added_comm%item_export :: export table or flag to be added
!>      added_comm%item_import :: import table or flag to be added
      type(communication_table) :: added_comm
      type(node_buffer_2_extend) :: send_nbuf
      type(node_buffer_2_extend) :: recv_nbuf
!
      integer(kind = kint), allocatable :: iflag_ele(:)
      integer(kind = kint), allocatable :: iflag_recv(:)
      integer(kind = kint), allocatable :: iflag_send(:)
      integer(kind = kint), allocatable :: iflag_node(:)
      real(kind = kreal), allocatable :: distance(:)
!
      integer(kind = kint), allocatable :: inod_import_new(:)
      integer(kind = kint), allocatable :: irank_import_new(:)
      integer(kind = kint), allocatable :: inod_export_new(:)
      integer(kind = kint), allocatable :: irank_export_new(:)
!
      type(mark_for_each_comm), allocatable :: mark_nod(:)
      type(mark_for_each_comm), allocatable :: mark_ele(:)
      type(comm_table_for_each_pe) :: each_comm
!
      integer(kind = kint) :: ntot_failed_gl, nele_failed_gl
!
      integer(kind = kint) :: inum, inod, i, ip, ist
      integer(kind = kint) :: iele, k1, icou, jcou, kcou
!
!
      allocate(iflag_node(org_node%numnod))
      allocate(distance(org_node%numnod))
!$omp parallel workshare
      iflag_node(1:org_node%numnod) = 0
      distance(1:org_node%numnod) =   0.0d0
!$omp end parallel workshare
!
      allocate(iflag_ele(org_ele%numele))
!$omp parallel workshare
      iflag_ele(1:org_ele%numele) = 0
!$omp end parallel workshare
!
      allocate(mark_nod(nod_comm%num_neib))
      allocate(mark_ele(nod_comm%num_neib))
      icou = 0
      jcou = 0
      do i = 1, nod_comm%num_neib
        call init_comm_table_for_each2                                  &
     &     (i, org_node, nod_comm, dist_4_comm, each_comm, distance)
        call mark_next_node_of_export2                                  &
     &     (dist_max, org_node, org_ele, neib_ele, each_comm,           &
     &      mark_nod(i), mark_ele(i), iflag_ele, iflag_node, distance)
        call dealloc_comm_table_for_each(each_comm)
!
!
        do inum = 1, mark_ele(i)%nnod_marked
          iele = mark_ele(i)%inod_marked(inum)
          kcou = 0
          do k1 = 1, org_ele%nnod_4_ele
            inod = org_ele%ie(iele,k1)
            if(iflag_node(inod) .ge. 0) kcou = kcou + 1
          end do
          icou = icou + kcou
          if(kcou .gt. 0) then
            jcou = jcou + 1
!            write(*,*) iele, org_ele%ie(iele,1:org_ele%nnod_4_ele), &
!     &                iflag_node(org_ele%ie(iele,1:org_ele%nnod_4_ele))
          end if
        end do
      end do
      call calypso_mpi_reduce_one_int(icou, ntot_failed_gl, MPI_SUM, 0)
      call calypso_mpi_reduce_one_int(jcou, nele_failed_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Failed element list:',             &
     &                             ntot_failed_gl, nele_failed_gl
!
      write(*,*) my_rank, 'mark_nod%nnod_marked',                       &
     &          mark_nod(1:nod_comm%num_neib)%nnod_marked,              &
     &        ' of ', org_node%numnod
      write(*,*) my_rank, 'mark_ele%nnod_marked',                       &
     &          mark_ele(1:nod_comm%num_neib)%nnod_marked,              &
     &        ' of ', org_ele%numele
!
!      call alloc_added_comm_table_num(nod_comm, added_comm)
!      do i = 1, nod_comm%num_neib
!        added_comm%num_export(i) = added_comm%num_export(i)            &
!     &                            + mark_nod(i)%nnod_marked
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
!        call copy_node_to_extend_buffer                                &
!     &    (added_comm%istack_export(i-1), org_node, inod_dbl,          &
!     &     mark_nod(i)%nnod_marked, mark_nod(i)%inod_marked, send_nbuf)
!        deallocate(mark_nod(i)%inod_marked)
!      end do
!
!      deallocate(mark_nod)
!      deallocate(iflag_node)
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
!     &   (org_node%numnod, inod_dbl%index, inod_dbl%irank,             &
!     &    nod_comm%num_neib, nod_comm%id_neib, nod_comm%ntot_import,   &
!     &    nod_comm%istack_import, nod_comm%item_import,                &
!     &    added_comm%num_neib, added_comm%id_neib,                     &
!     &    added_comm%ntot_import, added_comm%istack_import,            &
!     &    recv_nbuf%inod_add, recv_nbuf%irank_add,                     &
!     &    added_comm%item_import)
!
!      call check_added_impoert_items                                  &
!     &   (my_rank, nod_comm, added_comm, inod_dbl, recv_nbuf)
!
!      call added_nod_id_send_recv(added_comm%num_neib,                 &
!     &    added_comm%id_neib, added_comm%istack_import,                &
!     &    added_comm%ntot_import, added_comm%item_import,              &
!     &    added_comm%istack_export, added_comm%ntot_export,            &
!     &    added_comm%item_export)
!
!      call check_delete_from_SR_list                                  &
!     &   (my_rank, added_comm, send_nbuf, recv_nbuf)
!
!      call dealloc_node_buffer_2_extend(send_nbuf)
!
!      call count_nodes_by_extend_sleeve(added_comm, org_node, new_node)
!
!      call alloc_node_geometry_base(new_node)
!      call alloc_double_numbering(new_node%numnod, dbl_id2)
!
!      call set_nodes_by_extend_sleeve(recv_nbuf, org_node, inod_dbl,   &
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
!      call check_new_node_and_comm(new_comm, new_node, dbl_id2)
!
      end subroutine extend_node_comm_table2
!
!  ---------------------------------------------------------------------
!
      subroutine init_comm_table_for_each2                              &
     &         (ineib, node, nod_comm, dist_4_comm, each_comm, distance)
!
      use t_geometry_data
      use t_comm_table
      use mark_export_nod_ele_extend
!
      integer(kind = kint), intent(in) :: ineib
      type(node_data), intent(in) ::                 node
      type(communication_table), intent(in) ::       nod_comm
      type(dist_from_wall_in_export), intent(in) :: dist_4_comm
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
      real(kind = kreal), intent(inout) :: distance(node%numnod)
!
      integer(kind = kint) :: ist, ied, i, icou, ip, inod
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
!$omp parallel do private(i,inod)
      do i = 1, each_comm%num_each_export
        inod = nod_comm%item_export(i+ist)
        each_comm%item_each_export(i) = inod
        distance(inod) = dist_4_comm%distance_in_export(i+ist)
      end do
!$omp end parallel do
!
      each_comm%num_each_import = nod_comm%istack_import(ineib)         &
     &                           - nod_comm%istack_import(ineib-1)
!
      ist = nod_comm%istack_import(ineib-1) 
!$omp parallel do private(i)
      do i = 1, each_comm%num_each_import
        each_comm%item_each_import(i) = nod_comm%item_import(i+ist)
      end do
!$omp end parallel do
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
      end subroutine init_comm_table_for_each2
!
!  ---------------------------------------------------------------------
!
      subroutine mark_next_node_of_export2                              &
     &         (dist_max, node, ele, neib_ele, each_comm, mark_nod,     &
     &          mark_ele, iflag_ele, iflag_node, distance)
!
      use mark_export_nod_ele_extend
!
      real(kind = kreal), intent(in) :: dist_max
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
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
!             dist = 1.0d0
            dist = sqrt((node%xx(jnod,1) - node%xx(inod,1))**2         &
     &                + (node%xx(jnod,2) - node%xx(inod,2))**2         &
     &                + (node%xx(jnod,3) - node%xx(inod,3))**2)
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
!
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
!                 dist = 1.0d0
                dist = sqrt((node%xx(jnod,1) - node%xx(inod,1))**2     &
     &                    + (node%xx(jnod,2) - node%xx(inod,2))**2     &
     &                    + (node%xx(jnod,3) - node%xx(inod,3))**2)
                if(iflag_node(jnod) .eq. 0) then
                  iflag_node(jnod) = 1
                  distance(jnod) = dist + distance(inod)
                else
                  distance(jnod)                                       &
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
            if(distance(inod) .lt. dist_max) then
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
      allocate(mark_nod%inod_marked(mark_nod%nnod_marked))
      allocate(mark_nod%dist_marked(mark_nod%nnod_marked))
!
      icou = 0
      do inod = 1, node%numnod
        if(iflag_node(inod) .eq. -1) then
          icou = icou + 1
          mark_nod%inod_marked(icou) = inod
          mark_nod%dist_marked(icou) = distance(inod)
!          write(*,*) my_rank, 'mark_nod', inod, mark_nod%inod_marked(icou), mark_nod%dist_marked(icou)
        end if
      end do
!
      mark_ele%nnod_marked = 0
      do iele = 1, ele%numele
        if(iflag_ele(iele) .eq. 1) then
          mark_ele%nnod_marked = mark_ele%nnod_marked + 1
        end if
      end do
      allocate(mark_ele%inod_marked(mark_ele%nnod_marked))
      allocate(mark_ele%dist_marked(mark_ele%nnod_marked))
!
      anum = one / real(ele%nnod_4_ele)
      icou = 0
      do iele = 1, ele%numele
        if(iflag_ele(iele) .eq. 1) then
          icou = icou + 1
          mark_ele%inod_marked(icou) = iele
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
      end subroutine mark_next_node_of_export2
!
!  ---------------------------------------------------------------------
!
      end module analyzer_sleeve_extend2
