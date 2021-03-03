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
     &          org_ele, iele_dbl, neib_ele, new_nod_comm, new_node, dist_4_comm)
!
      use t_next_node_ele_4_node
!
      use m_solver_SR
      use calypso_mpi_int
      use reverse_SR_int
      use reverse_SR_int8
      use reverse_SR_real
      use solver_SR_type
!
      use quicksort
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
      type(communication_table), intent(inout) :: new_nod_comm
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
      integer(kind = kint), allocatable :: iflag_send_pe(:)
      integer(kind = kint), allocatable :: iflag_recv_pe(:)
!
      integer(kind = kint), allocatable :: np_new_export(:)
      integer(kind = kint), allocatable :: istack_pe_new_export(:)
      integer(kind = kint) :: ntot_pe_new_export
      integer(kind = kint), allocatable :: np_new_import(:)
      integer(kind = kint), allocatable :: istack_pe_new_import(:)
      integer(kind = kint) :: ntot_pe_new_import
      integer(kind = kint), allocatable :: ip_new_export(:)
      integer(kind = kint), allocatable :: ip_new_import(:)
      integer(kind = kint) :: iflag_process_extend = 0
!
      integer(kind = kint), allocatable :: istack_new_export(:)
      integer(kind = kint), allocatable :: num_new_export(:)
      integer(kind = kint) :: ntot_new_export
      integer(kind = kint_gl), allocatable :: inod_gl_new_export(:)
      real(kind = kreal), allocatable :: xx_new_export(:)
      integer(kind = kint), allocatable :: item_new_export(:)
      integer(kind = kint), allocatable :: inod_lc_new_export(:)
      integer(kind = kint), allocatable :: irank_nod_new_export(:)
      real(kind = kreal), allocatable :: distance_new_export(:)
!
      integer(kind = kint), allocatable :: istack_new_import(:)
      integer(kind = kint), allocatable :: num_new_import(:)
      integer(kind = kint) :: ntot_new_import
      integer(kind = kint_gl), allocatable :: inod_gl_new_import(:)
      real(kind = kreal), allocatable :: xx_new_import(:)
      integer(kind = kint), allocatable :: item_new_import(:)
      integer(kind = kint), allocatable :: inod_lc_new_import(:)
      integer(kind = kint), allocatable :: irank_nod_new_import(:)
      real(kind = kreal), allocatable :: distance_new_import(:)
!
      integer(kind = kint), allocatable :: num_import_tmp(:)
      integer(kind = kint), allocatable :: istack_import_tmp(:)
      integer(kind = kint), allocatable :: num_import_trim(:)
!
      integer(kind = kint), allocatable :: istack_import_trim(:)
      integer(kind = kint), allocatable :: index_4_import_trim(:)
      integer(kind = kint), allocatable :: index_4_import_tmp(:)
      integer(kind = kint), allocatable :: inod_lc_import_tmp(:)
      integer(kind = kint), allocatable :: irank_import_tmp(:)
      integer(kind = kint), allocatable :: iflag_nod_recv_pe(:)
      integer(kind = kint), allocatable :: irank_origin_new_import(:)
!
      integer(kind = kint_gl), allocatable :: inod_gl_new_import_trim(:)
      real(kind = kreal), allocatable :: xx_new_import_trim(:)
      integer(kind = kint), allocatable :: item_new_import_trim(:)
      integer(kind = kint), allocatable :: inod_lc_new_import_trim(:)
      integer(kind = kint), allocatable :: irank_new_import_trim(:)
      real(kind = kreal), allocatable :: distance_new_import_trim(:)
!
      integer(kind = kint_gl), allocatable :: inod_gl_new_export_back(:)
      real(kind = kreal), allocatable :: xx_new_export_back(:)
      integer(kind = kint), allocatable :: item_new_export_back(:)
      integer(kind = kint), allocatable :: inod_lc_new_export_back(:)
      integer(kind = kint), allocatable :: irank_new_export_back(:)
      real(kind = kreal), allocatable :: distance_new_export_back(:)
!
      integer(kind = kint) :: ntot_sorted_import_tmp
      integer(kind = kint), allocatable :: istack_sorted_import_tmp(:)
      integer(kind = kint), allocatable :: istack_sorted_import_pe(:)
!
      integer(kind = kint), allocatable :: idx_home_sorted_import(:)
      integer(kind = kint), allocatable :: idx_home_for_import(:)
!
!
!
      type(mark_for_each_comm), allocatable :: mark_nod(:)
      type(mark_for_each_comm), allocatable :: mark_ele(:)
      type(comm_table_for_each_pe) :: each_comm
!
      integer(kind = kint) :: ntot_failed_gl, nele_failed_gl
!
      integer(kind = kint) :: inum, inod, i, ip, ist, ied, jp
      integer(kind = kint) :: iele, k1, icou, jcou, kcou
      integer(kind = kint) :: jnum, jst, jed, irank, ntot
      integer(kind = kint) :: kdx, krank
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
      deallocate(dist_4_comm%distance_in_export)
!
      write(*,*) my_rank, 'mark_nod%nnod_marked',                       &
     &          mark_nod(1:nod_comm%num_neib)%nnod_marked,              &
     &        ' of ', org_node%numnod
      write(*,*) my_rank, 'mark_ele%nnod_marked',                       &
     &          mark_ele(1:nod_comm%num_neib)%nnod_marked,              &
     &        ' of ', org_ele%numele
!
!
!
!
!
!
      allocate(iflag_send_pe(nprocs))
      allocate(np_new_export(nod_comm%num_neib))
      allocate(istack_pe_new_export(0:nod_comm%num_neib))
      allocate(np_new_import(nod_comm%num_neib))
      allocate(istack_pe_new_import(0:nod_comm%num_neib))
!
      do i = 1, nod_comm%num_neib
!$omp parallel workshare
        iflag_send_pe(1:nprocs) = 0
!$omp end parallel workshare
!
        np_new_export(i) = 0
        do inum = 1, mark_nod(i)%nnod_marked
          inod = mark_nod(i)%inod_marked(inum)
          ip = inod_dbl%irank(inod)
          if(iflag_send_pe(ip+1) .eq. 0) then
            np_new_export(i) = np_new_export(i) + 1
            iflag_send_pe(ip+1) = 1
          end if
        end do
      end do
!
      istack_pe_new_export(0) = 0
      do i = 1, nod_comm%num_neib
        istack_pe_new_export(i) = istack_pe_new_export(i-1)             &
     &                           + np_new_export(i)
      end do
      ntot_pe_new_export = istack_pe_new_export(nod_comm%num_neib)
!
      call num_items_send_recv                                          &
     &   (nod_comm%num_neib, nod_comm%id_neib, np_new_export, SR_sig1,  &
     &    np_new_import, istack_pe_new_import, ntot_pe_new_import)
!
      allocate(ip_new_export(ntot_pe_new_export))
      allocate(ip_new_import(ntot_pe_new_import))
!
      do i = 1, nod_comm%num_neib
!$omp parallel workshare
        iflag_send_pe(1:nprocs) = 0
!$omp end parallel workshare
        icou = istack_pe_new_export(i-1)
        do inum = 1, mark_nod(i)%nnod_marked
          inod = mark_nod(i)%inod_marked(inum)
          ip = inod_dbl%irank(inod)
          if(iflag_send_pe(ip+1) .eq. 0) then
            icou = icou + 1
            ip_new_export(icou) = ip
            iflag_send_pe(ip+1) = 1
          end if
        end do
      end do
!
      call comm_items_send_recv                                         &
     &   (nod_comm%num_neib, nod_comm%id_neib, istack_pe_new_export,    &
     &    istack_pe_new_import, ip_new_export, SR_sig1, ip_new_import)
!
!      do i = 1, nod_comm%num_neib
!        ist = istack_pe_new_export(i-1)+1
!        ied = istack_pe_new_export(i)
!        write(*,*) my_rank, i, nod_comm%id_neib(i), 'ip_new_export',   &
!     &            ip_new_export(ist:ied)
!      end do
!
!      do i = 1, nod_comm%num_neib
!        ist = istack_pe_new_import(i-1)+1
!        ied = istack_pe_new_import(i)
!        write(*,*) my_rank, nod_comm%id_neib(i), 'ip_new_import',      &
!     &            ip_new_import(ist:ied)
!      end do
!
      allocate(iflag_recv_pe(nprocs))
!$omp parallel workshare
      iflag_recv_pe(1:nprocs) = 0
!$omp end parallel workshare
!
!$omp parallel do private(i,ip)
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i)
        iflag_recv_pe(ip+1) = 1
      end do
!$omp end parallel do
!
      iflag_process_extend = 0
      new_nod_comm%num_neib = nod_comm%num_neib
      do i = 1, nod_comm%num_neib
        ist = istack_pe_new_import(i-1)+1
        ied = istack_pe_new_import(i)
        do inum = ist, ied
          ip = ip_new_import(inum)
          if(iflag_recv_pe(ip+1) .eq. 0) then
            iflag_recv_pe(ip+1) =  2
            new_nod_comm%num_neib = new_nod_comm%num_neib + 1
            iflag_process_extend = 1
          end if
        end do
      end do
      write(*,*) my_rank, iflag_process_extend, 'new_num_neib',   &
     &           nod_comm%num_neib, new_nod_comm%num_neib
!
      call alloc_comm_table_num(new_nod_comm)
      icou = 0
      do i = 1, nprocs
        ip = mod(my_rank+i,nprocs)
        if(iflag_recv_pe(ip+1) .ge. 1) then
          icou = icou + 1 
          new_nod_comm%id_neib(icou) = ip
        end if
      end do
!
      write(*,*) my_rank, 'new_nod_comm%id_neib',                       &
     &          new_nod_comm%id_neib
!
!
      allocate(num_new_export(nod_comm%num_neib))
      allocate(istack_new_export(0:nod_comm%num_neib))
      allocate(num_new_import(nod_comm%num_neib))
      allocate(istack_new_import(0:nod_comm%num_neib))
!
      istack_new_export(0) = 0
      do i = 1, nod_comm%num_neib
        num_new_export(i) = mark_nod(i)%nnod_marked
        istack_new_export(i) = istack_new_export(i-1)                   &
     &                        + num_new_export(i)
      end do
      ntot_new_export = istack_new_export(nod_comm%num_neib)

      call num_items_send_recv                                          &
     &   (nod_comm%num_neib, nod_comm%id_neib, num_new_export, SR_sig1, &
     &    num_new_import, istack_new_import, ntot_new_import)
!
      allocate(item_new_export(ntot_new_export))
      allocate(inod_gl_new_export(ntot_new_export))
      allocate(inod_lc_new_export(ntot_new_export))
      allocate(irank_nod_new_export(ntot_new_export))
      allocate(distance_new_export(ntot_new_export))
      allocate(xx_new_export(3*ntot_new_export))
!
      allocate(item_new_import(ntot_new_import))
      allocate(inod_gl_new_import(ntot_new_import))
      allocate(inod_lc_new_import(ntot_new_import))
      allocate(irank_nod_new_import(ntot_new_import))
      allocate(distance_new_import(ntot_new_import))
      allocate(xx_new_import(3*ntot_new_import))
!

      do i = 1, nod_comm%num_neib
        ist = istack_new_export(i-1)
        do inum = 1, mark_nod(i)%nnod_marked
          icou = ist + inum
          inod = mark_nod(i)%inod_marked(inum)
          item_new_export(icou) =      inod
          inod_gl_new_export(icou) = org_node%inod_global(inod)
          xx_new_export(3*icou-2) =  org_node%xx(inod,1)
          xx_new_export(3*icou-1) =  org_node%xx(inod,2)
          xx_new_export(3*icou  ) =  org_node%xx(inod,3)
          inod_lc_new_export(icou) =   inod_dbl%index(inod)
          irank_nod_new_export(icou) = inod_dbl%irank(inod)
          distance_new_export(icou) =  mark_nod(i)%dist_marked(inum)
        end do
      end do
!
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    istack_new_export, istack_new_import, item_new_export,        &
     &    SR_sig1, item_new_import)
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    istack_new_export, istack_new_import, inod_lc_new_export,     &
     &    SR_sig1, inod_lc_new_import)
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    istack_new_export, istack_new_import, irank_nod_new_export,   &
     &    SR_sig1, irank_nod_new_import)
      call real_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    istack_new_export, istack_new_import, distance_new_export,    &
     &    SR_sig1, distance_new_import)
!
      call real_items_send_recv_3(nod_comm%num_neib, nod_comm%id_neib,  &
     &    istack_new_export, istack_new_import, xx_new_export,          &
     &    SR_sig1, xx_new_import)
      call int8_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    istack_new_export, istack_new_import, inod_gl_new_export,     &
     &    SR_sig1, inod_gl_new_import)
!
!      icou = 0
!      do i = 1, istack_new_import(nod_comm%num_neib)
!        if(irank_nod_new_import(i) .eq. my_rank) icou = icou + 1
!      end do
!      write(*,*) my_rank, 'self domain', icou
!
      allocate(num_import_tmp(nprocs))
      allocate(istack_import_tmp(0:nprocs))
      num_import_tmp(1:nprocs) =    0
      istack_import_tmp(0:nprocs) = 0
!
      allocate(irank_origin_new_import(ntot_new_import))
      allocate(iflag_nod_recv_pe(ntot_new_import))
      allocate(index_4_import_tmp(ntot_new_import))
      allocate(irank_import_tmp(ntot_new_import))
      allocate(inod_lc_import_tmp(ntot_new_import))
!
!$omp parallel do private(i,ip)
      do i = 1, ntot_new_import
        index_4_import_tmp(i) = i
        inod_lc_import_tmp(i) = 0
        irank_import_tmp(i) = irank_nod_new_import(i)
        irank_origin_new_import(i) = -1
      end do
!$omp end parallel do
!
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i)
        ist = istack_new_import(i-1) + 1
        ied = istack_new_import(i)
        do inum = ist, ied
          jp = irank_nod_new_import(inum)
          irank_origin_new_import(inum) = ip
          if(jp .eq. ip) then
            iflag_nod_recv_pe(inum) =  1
          else if(iflag_recv_pe(jp+1) .eq. 2) then
            iflag_nod_recv_pe(inum) =  2
          else
            iflag_nod_recv_pe(inum) =  0
          end if
        end do
      end do
!
      if(ntot_new_import .gt. 1) then
        call quicksort_w_index(ntot_new_import, irank_import_tmp,       &
     &      ione, ntot_new_import, index_4_import_tmp)
      end if
!
!$omp parallel do private(i,icou)
      do i = 1, ntot_new_import
        icou = index_4_import_tmp(i)
        inod_lc_import_tmp(i) = inod_lc_new_import(icou)
      end do
!$omp end parallel do
!
!$omp parallel workshare
      num_import_tmp(1:nprocs) = 0
!$omp end parallel workshare
      do i = 1, ntot_new_import
        irank = irank_import_tmp(i)
        num_import_tmp(irank+1) = num_import_tmp(irank+1) + 1
      end do
      do ip = 1, nprocs
        istack_import_tmp(ip) = istack_import_tmp(ip-1)                 &
     &                         + num_import_tmp(ip)
      end do
!
      do ip = 1, nprocs
        ist = istack_import_tmp(ip-1)
        if(num_import_tmp(ip) .gt. 1) then
          call quicksort_w_index                                        &
     &       (ntot_new_import, inod_lc_import_tmp(ist+1),               &
     &        ione, num_import_tmp(ip), index_4_import_tmp(ist+1))
        end if
      end do
!
      allocate(num_import_trim(nprocs))
      allocate(istack_import_trim(0:nprocs))
      allocate(index_4_import_trim(ntot_new_import))
      num_import_trim(1:nprocs) = 0
      istack_import_trim(0:nprocs) = 0
!
      do ip = 1, nprocs
        ist = istack_import_tmp(ip-1)
        icou = 0
        if(iflag_recv_pe(ip) .eq. 1) then
          do inum = 1, num_import_tmp(ip)
            jnum = index_4_import_tmp(inum+ist)
            if(iflag_nod_recv_pe(jnum) .eq. 1) icou = icou + 1
          end do
        else if(iflag_recv_pe(ip) .eq. 2) then
          if(num_import_tmp(ip) .gt. 1) icou = 1
          do inum = 2, num_import_tmp(ip)
            if(inod_lc_import_tmp(inum)                                 &
     &            .ne. inod_lc_import_tmp(inum-1)) icou = icou + 1
          end do
        end if
        num_import_trim(ip) = icou
        istack_import_trim(ip) = istack_import_trim(ip-1) + icou
      end do
!
      do ip = 1, nprocs
        ist = istack_import_tmp(ip-1)
        icou = istack_import_trim(ip-1)
        if(iflag_recv_pe(ip) .eq. 1) then
          do inum = 1, num_import_tmp(ip)
            jnum = index_4_import_tmp(inum+ist)
            if(iflag_nod_recv_pe(jnum) .eq. 1) then
              icou = icou + 1
              index_4_import_trim(icou) = jnum
            end if
          end do
        else if(iflag_recv_pe(ip) .eq. 2) then
          if(num_import_tmp(ip) .gt. 1) icou = 1
          do inum = 2, num_import_tmp(ip)
            if(inod_lc_import_tmp(inum)                                 &
     &            .ne. inod_lc_import_tmp(inum-1)) then
              icou = icou + 1
              jnum = index_4_import_tmp(inum+ist)
              index_4_import_trim(icou) = jnum
            end if
          end do
        end if
      end do
!
!
!

      ntot = 0
      do ip = 1, nprocs
        ist = istack_import_tmp(ip-1)
        do inum = 1, num_import_tmp(ip) - 1
          if(inod_lc_import_tmp(ist+inum)                               &
     &        .ne. inod_lc_import_tmp(ist+inum+1)) ntot = ntot + 1
        end do
        if(num_import_tmp(ip) .gt. 0) ntot = ntot + 1
      end do
      ntot_sorted_import_tmp = ntot
!
      allocate(istack_sorted_import_pe(0:nprocs))
      allocate(istack_sorted_import_tmp(0:ntot_sorted_import_tmp))
      istack_sorted_import_pe(0) = 0
      istack_sorted_import_tmp(0) = 0
!
      icou = 0
      do ip = 1, nprocs
        ist = istack_import_tmp(ip-1)
        do inum = 1, num_import_tmp(ip)-1
          if(inod_lc_import_tmp(ist+inum)                               &
     &        .ne. inod_lc_import_tmp(ist+inum+1)) then
            icou = icou + 1
            istack_sorted_import_tmp(icou) = ist + inum
          end if
        end do
        if(num_import_tmp(ip) .gt. 0) then
          icou = icou + 1
          istack_sorted_import_tmp(icou) = ist + num_import_tmp(ip)
        end if
        istack_sorted_import_pe(ip) =    icou
      end do
!
      allocate(idx_home_sorted_import(istack_sorted_import_pe(nprocs)))
      idx_home_sorted_import(1:istack_sorted_import_pe(nprocs)) = -1
      allocate(idx_home_for_import(ntot_new_import))
      idx_home_for_import(1:ntot_new_import) = -1
!
      do ip = 1, nprocs
        ist = istack_sorted_import_pe(ip-1) + 1
        ied = istack_sorted_import_pe(ip)
        do inum = ist, ied
          jst = istack_sorted_import_tmp(inum-1) + 1
          jed = istack_sorted_import_tmp(inum)
          do jnum = jst, jed
            kdx = index_4_import_tmp(jnum)
            krank =     irank_origin_new_import(kdx)
            if(     irank_nod_new_import(kdx) .eq. krank                &
     &        .and. irank_nod_new_import(kdx) .eq. ip-1) then
              idx_home_sorted_import(inum) = kdx
              exit
            end if
          end do
        end do
      end do
!
      icou = 0
      do ip = 1, nprocs
        ist = istack_sorted_import_pe(ip-1) + 1
        ied = istack_sorted_import_pe(ip)
        do inum = ist, ied
          if(idx_home_sorted_import(inum) .gt. 0) cycle
!
          icou = icou + 1
          jst = istack_sorted_import_tmp(inum-1) + 1
          jed = istack_sorted_import_tmp(inum)
          do jnum = jst, jed
            kdx =   index_4_import_tmp(jnum)
            if(irank_nod_new_import(kdx) .eq. ip-1) then
              idx_home_sorted_import(inum) = kdx
              exit
            end if
          end do
        end do
      end do
      write(*,*) my_rank, 'Misisng in import list: ', icou
!
      icou = 0
      do ip = 1, nprocs
        ist = istack_sorted_import_pe(ip-1) + 1
        ied = istack_sorted_import_pe(ip)
        do inum = ist, ied
          if(idx_home_sorted_import(inum) .gt. 0) cycle
!
          jst = istack_sorted_import_tmp(inum-1) + 1
          jed = istack_sorted_import_tmp(inum)
          kdx = index_4_import_tmp(jst)
          idx_home_sorted_import(inum) = kdx
          icou = icou + 1
        end do
      end do
      write(*,*) my_rank, 'Required import from new domain : ', icou
!
      do ip = 1, nprocs
        ist = istack_sorted_import_pe(ip-1) + 1
        ied = istack_sorted_import_pe(ip)
        do inum = ist, ied
          jst = istack_sorted_import_tmp(inum-1) + 1
          jed = istack_sorted_import_tmp(inum)
          kdx = idx_home_sorted_import(inum)
!
          idx_home_for_import(jst:jed) = kdx
        end do
      end do
!
      write(70+my_rank,*) 'check neib', nod_comm%id_neib
      write(70+my_rank,*) 'check istack_sorted_import_pe', istack_sorted_import_pe
      do inum = 1, istack_sorted_import_pe(nprocs)
        inod = idx_home_sorted_import(inum)
          jst = istack_sorted_import_tmp(inum-1) + 1
          jed = istack_sorted_import_tmp(inum)
        write(70+my_rank,*) 'item_new_import', inum, inod, &
     &       inod_lc_new_import(inod), irank_nod_new_import(inod), &
     &       inod_lc_new_import(inod_lc_import_tmp(jst:jed)), &
     &       irank_nod_new_import(inod_lc_import_tmp(jst:jed))
      end do
!
      write(*,*) my_rank, 'org_neib', nod_comm%id_neib
      write(*,*) my_rank, 'new_neib', new_nod_comm%id_neib
      write(*,*) my_rank, 'Totals', nod_comm%ntot_import,               &
     &          sum(num_import_tmp), sum(num_import_trim),              &
     &          ntot_sorted_import_tmp
      do ip = 1, nprocs
        write(*,*) my_rank, ' to ', ip-1,  ' num_import_tmp ',          &
     &            num_import_tmp(ip), num_import_trim(ip),              &
     &            istack_import_trim(ip), istack_sorted_import_pe(ip)
      end do
!
      write(*,*) my_rank, 'new_nod_comm%num_neib', new_nod_comm%num_neib
      call alloc_import_num(new_nod_comm)
!
      new_nod_comm%istack_import(0) = 0
      do i = 1, new_nod_comm%num_neib
        irank = new_nod_comm%id_neib(i)
        new_nod_comm%num_import(i) = istack_sorted_import_pe(irank+1)   &
     &                              - istack_sorted_import_pe(irank)
        new_nod_comm%istack_import(i) = new_nod_comm%istack_import(i-1) &
     &                                 + new_nod_comm%num_import(i)
      end do
      new_nod_comm%ntot_import                                          &
     &       = new_nod_comm%istack_import(new_nod_comm%num_neib)
      write(*,*) my_rank, 'new_nod_comm%ntot_import', new_nod_comm%ntot_import
      call alloc_import_item(new_nod_comm)
!
      allocate(inod_gl_new_import_trim(new_nod_comm%ntot_import))
      allocate(xx_new_import_trim(3*new_nod_comm%ntot_import))
      allocate(item_new_import_trim(new_nod_comm%ntot_import))
      allocate(inod_lc_new_import_trim(new_nod_comm%ntot_import))
      allocate(irank_new_import_trim(new_nod_comm%ntot_import))
      allocate(distance_new_import_trim(new_nod_comm%ntot_import))
!
!
      do i = 1, new_nod_comm%num_neib
        irank = new_nod_comm%id_neib(i)
        ist = istack_sorted_import_pe(irank)
        jst = new_nod_comm%istack_import(i-1)
        do inum = 1, new_nod_comm%num_import(i)
          jcou = inum + jst
          new_nod_comm%item_import(jcou)                                &
     &         = jcou + org_node%internal_node
!
          jnum = idx_home_sorted_import(inum+ist)
          item_new_import_trim(jcou) =    item_new_import(jnum)
          inod_lc_new_import_trim(jcou) = inod_lc_new_import(jnum)
          irank_new_import_trim(jcou) =   irank_nod_new_import(jnum)
          distance_new_import_trim(jcou) = distance_new_import(jnum)
!
          xx_new_import_trim(3*jcou-2) = xx_new_import(3*jnum-2)
          xx_new_import_trim(3*jcou-1) = xx_new_import(3*jnum-1)
          xx_new_import_trim(3*jcou  ) = xx_new_import(3*jnum  )
          inod_gl_new_import_trim(jcou) = inod_gl_new_import(jnum)
        end do
      end do
!
!      write(80+my_rank,*) 'check neib', nod_comm%id_neib
!      write(80+my_rank,*) 'check istack_import_trim', istack_import_trim
!      do i = 1, ntot_new_import
!        write(80+my_rank,*) 'item_new_import', i, &
!     &       index_4_import_trim(i)
!      end do
!
!      write(70+my_rank,*) 'check neib', nod_comm%id_neib
!      write(70+my_rank,*) 'check istack_new_import', istack_new_import
!      do i = 1, ntot_new_import
!        inod = item_new_import(i)
!        write(70+my_rank,*) 'item_new_import', i, inod, &
!     &       inod_lc_new_import(i), irank_nod_new_import(i)
!      end do
!
!        write(60+my_rank,*) 'check neib', new_nod_comm%id_neib
!        write(60+my_rank,*) 'check stack', new_nod_comm%istack_import
!      do i = 1, new_nod_comm%ntot_import
!        inod = new_nod_comm%item_import(i)
!        write(60+my_rank,*) 'irank_new_import_trim', i, inod, &
!     &       inod_lc_new_import_trim(i), irank_new_import_trim(i), item_new_import_trim(i)
!      end do
      call calypso_mpi_barrier
!
!
      call alloc_export_num(new_nod_comm)
      call num_items_send_recv                                          &
     &   (new_nod_comm%num_neib, new_nod_comm%id_neib,                  &
     &    new_nod_comm%num_import, SR_sig1, new_nod_comm%num_export,    &
     &    new_nod_comm%istack_export, new_nod_comm%ntot_export)
      call alloc_export_item(new_nod_comm)
!
!
      call calypso_mpi_barrier
!
      allocate(inod_gl_new_export_back(new_nod_comm%ntot_export))
      allocate(xx_new_export_back(3*new_nod_comm%ntot_export))
      allocate(item_new_export_back(new_nod_comm%ntot_export))
      allocate(inod_lc_new_export_back(new_nod_comm%ntot_export))
      allocate(irank_new_export_back(new_nod_comm%ntot_export))
!
      dist_4_comm%ntot = new_nod_comm%ntot_export
      allocate(dist_4_comm%distance_in_export(dist_4_comm%ntot))
!
      call comm_items_send_recv                                         &
     &   (new_nod_comm%num_neib, new_nod_comm%id_neib,                  &
     &    new_nod_comm%istack_import, new_nod_comm%istack_export,       &
     &    inod_lc_new_import_trim, SR_sig1, new_nod_comm%item_export)
!      call comm_items_send_recv                                        &
!     &   (new_nod_comm%num_neib, new_nod_comm%id_neib,                 &
!     &    new_nod_comm%istack_import, new_nod_comm%istack_export,      &
!     &    inod_lc_new_import_trim, SR_sig1, inod_lc_new_export_back)
      call comm_items_send_recv                                         &
     &   (new_nod_comm%num_neib, new_nod_comm%id_neib,                  &
     &    new_nod_comm%istack_import, new_nod_comm%istack_export,       &
     &    irank_new_import_trim, SR_sig1, irank_new_export_back)
      call real_items_send_recv                                         &
     &   (new_nod_comm%num_neib, new_nod_comm%id_neib,                  &
     &    new_nod_comm%istack_import, new_nod_comm%istack_export,       &
     &    distance_new_import_trim, SR_sig1,                            &
     &    dist_4_comm%distance_in_export)
!
      call real_items_send_recv_3                                       &
     &   (new_nod_comm%num_neib, new_nod_comm%id_neib,                  &
     &    new_nod_comm%istack_import, new_nod_comm%istack_export,       &
     &    xx_new_import_trim, SR_sig1, xx_new_export_back)
      call int8_items_send_recv                                         &
     &   (new_nod_comm%num_neib, new_nod_comm%id_neib,                  &
     &    new_nod_comm%istack_import, new_nod_comm%istack_export,       &
     &    inod_gl_new_import_trim, SR_sig1, inod_gl_new_export_back)
!
!
!        write(50+my_rank,*) 'check neib', new_nod_comm%id_neib
!        write(50+my_rank,*) 'check stack', new_nod_comm%istack_export
      icou = 0
      do i = 1, new_nod_comm%ntot_export
        inod = new_nod_comm%item_export(i)
!        write(50+my_rank,*) 'check', i, inod, &
!     &       inod_dbl%irank(inod), irank_new_export_back(i),  &
!     &       inod_dbl%index(inod)
        if(inod_dbl%irank(inod) .ne. irank_new_export_back(i))     &
     &   icou = icou + 1
      end do
      write(*,*) my_rank, 'Failed communication data: ', icou
      write(*,*) my_rank, 'Extend again flag: ', iflag_process_extend
!
      deallocate(mark_nod)
      deallocate(iflag_node)
!
!
      new_node%numnod = org_node%internal_node                          &
     &                 + new_nod_comm%ntot_import
      new_node%internal_node = org_node%internal_node
!
      call alloc_node_geometry_base(new_node)
      call alloc_double_numbering(new_node%numnod, dbl_id2)
!
!$omp parallel do
      do inod = 1, org_node%internal_node
        new_node%inod_global(inod) = org_node%inod_global(inod)
        new_node%xx(inod,1) = org_node%xx(inod,1)
        new_node%xx(inod,2) = org_node%xx(inod,2)
        new_node%xx(inod,3) = org_node%xx(inod,3)
        dbl_id2%index(inod) = inod_dbl%index(inod)
        dbl_id2%irank(inod) = inod_dbl%irank(inod)
      end do
!$omp end parallel do
!
      ist = org_node%internal_node
!$omp parallel do private(inum,icou)
      do inum = 1, new_nod_comm%ntot_import
        icou = inum + ist
        new_node%inod_global(icou) = inod_gl_new_import_trim(inum)
        new_node%xx(icou,1) = xx_new_import_trim(3*inum-2)
        new_node%xx(icou,2) = xx_new_import_trim(3*inum-1)
        new_node%xx(icou,3) = xx_new_import_trim(3*inum  )
        dbl_id2%index(icou) = inod_lc_new_import_trim(inum)
        dbl_id2%irank(icou) = irank_new_import_trim(inum)
      end do
!$omp end parallel do
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
      call check_new_node_and_comm(new_nod_comm, new_node, dbl_id2)
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
