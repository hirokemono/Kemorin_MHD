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
      integer(kind = kint), allocatable :: istack_new_ele_export(:)
      integer(kind = kint), allocatable :: num_new_ele_export(:)
      integer(kind = kint) :: ntot_new_ele_export
      integer(kind = kint), allocatable :: item_new_ele_export(:)
      integer(kind = kint), allocatable :: iele_lc_new_export(:)
      integer(kind = kint), allocatable :: irank_ele_new_export(:)
      integer(kind = kint_gl), allocatable :: iele_gl_new_export(:)
      integer(kind = kint), allocatable :: ie_new_export(:,:)
      integer(kind = kint), allocatable :: ie_lc_new_export(:,:)
      integer(kind = kint), allocatable :: ie_rank_new_export(:,:)
!
      integer(kind = kint), allocatable :: istack_new_ele_import(:)
      integer(kind = kint), allocatable :: num_new_ele_import(:)
      integer(kind = kint) :: ntot_new_ele_import
      integer(kind = kint), allocatable :: item_new_ele_import(:)
      integer(kind = kint), allocatable :: iele_lc_new_import(:)
      integer(kind = kint), allocatable :: irank_ele_new_import(:)
      integer(kind = kint_gl), allocatable :: iele_gl_new_import(:)
      integer(kind = kint), allocatable :: ie_new_import(:,:)
      integer(kind = kint), allocatable :: ie_lc_new_import(:,:)
      integer(kind = kint), allocatable :: ie_rank_new_import(:,:)
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
      integer(kind = kint), allocatable :: nele_import_tmp(:)
      integer(kind = kint), allocatable :: istack_ele_import_tmp(:)
      integer(kind = kint), allocatable :: irank_org_ele_new_import(:)
      integer(kind = kint), allocatable :: index_ele_import_tmp(:)
      integer(kind = kint), allocatable :: irank_ele_import_tmp(:)
      integer(kind = kint), allocatable :: iele_lc_import_tmp(:)
!
!
      integer(kind = kint), allocatable :: num_import_tmp(:)
      integer(kind = kint), allocatable :: istack_import_tmp(:)
!
      integer(kind = kint), allocatable :: index_4_import_tmp(:)
      integer(kind = kint), allocatable :: inod_lc_import_tmp(:)
      integer(kind = kint), allocatable :: irank_import_tmp(:)
      integer(kind = kint), allocatable :: irank_origin_new_import(:)
!
!
      integer(kind = kint_gl), allocatable :: iele_gl_new_import_trim(:)
      integer(kind = kint), allocatable :: ie_new_import_trim(:,:)
      integer(kind = kint), allocatable :: ie_lc_new_import_trim(:,:)
      integer(kind = kint), allocatable :: ie_rank_new_import_trim(:,:)
      integer(kind = kint), allocatable :: item_new_ele_import_trim(:)
      integer(kind = kint), allocatable :: iele_lc_new_import_trim(:)
      integer(kind = kint), allocatable :: irank_new_ele_import_trim(:)
!
      integer(kind = kint_gl), allocatable :: iele_gl_new_export_trim(:)
      integer(kind = kint), allocatable :: ie_new_export_trim(:,:)
      integer(kind = kint), allocatable :: ie_lc_new_export_trim(:,:)
      integer(kind = kint), allocatable :: ie_rank_new_export_trim(:,:)
      integer(kind = kint), allocatable :: item_new_ele_export_trim(:)
      integer(kind = kint), allocatable :: iele_lc_new_export_trim(:)
      integer(kind = kint), allocatable :: irank_new_ele_export_trim(:)
!
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
      integer(kind = kint) :: ntot_sorted_ele_import_tmp
      integer(kind = kint), allocatable :: istack_sorted_ele_import_tmp(:)
      integer(kind = kint), allocatable :: istack_sorted_ele_import_pe(:)
!
      integer(kind = kint) :: ntot_sorted_import_tmp
      integer(kind = kint), allocatable :: istack_sorted_import_tmp(:)
      integer(kind = kint), allocatable :: istack_sorted_import_pe(:)
!
      integer(kind = kint), allocatable :: idx_home_sorted_import(:)
      integer(kind = kint), allocatable :: idx_home_for_import(:)
!
      integer(kind = kint), allocatable :: idx_home_sorted_ele_import(:)
      integer(kind = kint), allocatable :: idx_home_for_ele_import(:)
!
      integer(kind = kint), allocatable :: inod_in_comm(:)
!
!
      type(mark_for_each_comm), allocatable :: mark_nod(:)
      type(mark_for_each_comm), allocatable :: mark_ele(:)
      type(comm_table_for_each_pe) :: each_comm
!
      integer(kind = kint) :: ntot_failed_gl, nele_failed_gl
!
      integer(kind = kint) :: inum, inod, i, ip, ist, ied, jp, num
      integer(kind = kint) :: iele, k1, icou, jcou, kcou
      integer(kind = kint) :: jnum, jst, jed, irank, ntot
      integer(kind = kint) :: kdx, krank, kst
!
      type(communication_table) :: new_ele_comm
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
!      write(*,*) my_rank, 'new_nod_comm%id_neib',                      &
!     &          new_nod_comm%id_neib
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
!$omp parallel private(i,ist)
      do i = 1, nod_comm%num_neib
        ist = istack_new_export(i-1)
!$omp do private(inum,icou,inod)
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
!$omp end do nowait
      end do
!$omp end parallel
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
      allocate(num_import_tmp(nprocs))
      allocate(istack_import_tmp(0:nprocs))
      num_import_tmp(1:nprocs) =    0
      istack_import_tmp(0:nprocs) = 0
!
      allocate(irank_origin_new_import(ntot_new_import))
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
!$omp parallel private(i,ip,ist,ied)
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i)
        ist = istack_new_import(i-1) + 1
        ied = istack_new_import(i)
!$omp do private(inum)
        do inum = ist, ied
          irank_origin_new_import(inum) = ip
        end do
!$omp end do nowait
      end do
!$omp end parallel
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
     &       (num_import_tmp(ip), inod_lc_import_tmp(ist+1),            &
     &        ione, num_import_tmp(ip), index_4_import_tmp(ist+1))
        end if
      end do
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
            krank = irank_origin_new_import(kdx)
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
      write(*,*) my_rank, 'Misisng node in import list: ', icou
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
!      write(70+my_rank,*) 'check neib', nod_comm%id_neib
!      write(70+my_rank,*) 'check istack_sorted_import_pe', istack_sorted_import_pe
!      do inum = 1, istack_sorted_import_pe(nprocs)
!        inod = idx_home_sorted_import(inum)
!          jst = istack_sorted_import_tmp(inum-1) + 1
!          jed = istack_sorted_import_tmp(inum)
!        write(70+my_rank,*) 'item_new_import', inum, inod, &
!     &       inod_lc_new_import(inod), irank_nod_new_import(inod), &
!     &       inod_lc_new_import(inod_lc_import_tmp(jst:jed)), &
!     &       irank_nod_new_import(inod_lc_import_tmp(jst:jed))
!      end do
!
      write(*,*) my_rank, 'org_neib', nod_comm%id_neib
      write(*,*) my_rank, 'new_neib', new_nod_comm%id_neib
      write(*,*) my_rank, 'Totals', nod_comm%ntot_import,               &
     &          sum(num_import_tmp),  ntot_sorted_import_tmp
      do ip = 1, nprocs
        write(*,*) my_rank, ' to ', ip-1,  ' num_import_tmp ',          &
     &            num_import_tmp(ip), istack_sorted_import_pe(ip)
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
      call check_new_node_and_comm(new_nod_comm, new_node, dbl_id2)
!
!
!
!
      new_ele_comm%num_neib = new_nod_comm%num_neib
      call alloc_comm_table_num(new_ele_comm)
!
!$omp parallel do private(i)
      do i = 1, new_ele_comm%num_neib
        new_ele_comm%id_neib(i) = new_nod_comm%id_neib(i)
      end do
!$omp end parallel do
!
      allocate(num_new_ele_export(nod_comm%num_neib))
      allocate(istack_new_ele_export(0:nod_comm%num_neib))
      allocate(num_new_ele_import(nod_comm%num_neib))
      allocate(istack_new_ele_import(0:nod_comm%num_neib))
!
      istack_new_ele_export(0) = 0
      do i = 1, nod_comm%num_neib
        num_new_ele_export(i) = mark_ele(i)%nnod_marked
        istack_new_ele_export(i) = istack_new_ele_export(i-1)           &
     &                        + num_new_ele_export(i)
      end do
      ntot_new_ele_export = istack_new_ele_export(nod_comm%num_neib)

      call num_items_send_recv                                          &
     &   (nod_comm%num_neib, nod_comm%id_neib, num_new_ele_export,      &
     &    SR_sig1, num_new_ele_import, istack_new_ele_import,           &
     &    ntot_new_ele_import)
!
      allocate(item_new_ele_export(ntot_new_ele_export))
      allocate(iele_gl_new_export(ntot_new_ele_export))
      allocate(iele_lc_new_export(ntot_new_ele_export))
      allocate(irank_ele_new_export(ntot_new_ele_export))
      allocate(ie_new_export(ntot_new_ele_export,org_ele%nnod_4_ele))
      allocate(ie_lc_new_export(ntot_new_ele_export,org_ele%nnod_4_ele))
      allocate(ie_rank_new_export(ntot_new_ele_export,org_ele%nnod_4_ele))
!
      allocate(item_new_ele_import(ntot_new_ele_import))
      allocate(iele_gl_new_import(ntot_new_ele_import))
      allocate(iele_lc_new_import(ntot_new_ele_import))
      allocate(irank_ele_new_import(ntot_new_ele_import))
      allocate(ie_new_import(ntot_new_ele_import,org_ele%nnod_4_ele))
      allocate(ie_lc_new_import(ntot_new_ele_import,org_ele%nnod_4_ele))
      allocate(ie_rank_new_import(ntot_new_ele_import,org_ele%nnod_4_ele))
!
      allocate(inod_in_comm(org_node%numnod))
!$omp parallel workshare
        inod_in_comm(1:org_node%numnod) = 0
!$omp end parallel workshare
!
!$omp parallel private(i,ist,num)
      do i = 1, nod_comm%num_neib
!$omp workshare
        inod_in_comm(1:org_node%numnod) = 0
!$omp end workshare
!
        ist = nod_comm%istack_import(i-1)
        num = nod_comm%istack_import(i) - nod_comm%istack_import(i-1)
!$omp do private(inum,inod)
        do inum = 1, num
          inod = nod_comm%item_import(inum+ist)
          inod_in_comm(inod) = -inum
        end do
!$omp end do
!
        ist = istack_new_export(i-1)
        num = istack_new_export(i) - istack_new_export(i-1)
!$omp do private(inum,inod)
        do inum = 1, num
          inod = item_new_export(inum+ist)
          inod_in_comm(inod) = inum
        end do
!$omp end do
!
        ist = istack_new_ele_export(i-1)
!$omp do private(inum,icou,iele,k1,inod)
        do inum = 1, mark_ele(i)%nnod_marked
          icou = ist + inum
          iele = mark_ele(i)%inod_marked(inum)
          item_new_ele_export(icou) =      iele
          iele_gl_new_export(icou) = org_ele%iele_global(iele)
          iele_lc_new_export(icou) =   iele_dbl%index(iele)
          irank_ele_new_export(icou) = iele_dbl%irank(iele)
!
          do k1 = 1, org_ele%nnod_4_ele
            inod = org_ele%ie(iele,k1)
            ie_new_export(icou,k1) =      inod_in_comm(inod)
            ie_lc_new_export(icou,k1) =   inod_dbl%index(inod)
            ie_rank_new_export(icou,k1) = inod_dbl%irank(inod)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    istack_new_ele_export, istack_new_ele_import,                 &
     &    item_new_ele_export, SR_sig1, item_new_ele_import)
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    istack_new_ele_export, istack_new_ele_import,                 &
     &    iele_lc_new_export, SR_sig1, iele_lc_new_import)
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    istack_new_ele_export, istack_new_ele_import,                 &
     &    irank_ele_new_export, SR_sig1, irank_ele_new_import)
!
      call int8_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    istack_new_ele_export, istack_new_ele_import,                 &
     &    iele_gl_new_export, SR_sig1, iele_gl_new_import)
      do k1 = 1, org_ele%nnod_4_ele
        call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,  &
     &      istack_new_ele_export, istack_new_ele_import,               &
     &      ie_new_export(1,k1), SR_sig1, ie_new_import(1,k1))
      end do
      do k1 = 1, org_ele%nnod_4_ele
        call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,  &
     &      istack_new_ele_export, istack_new_ele_import,               &
     &      ie_lc_new_export(1,k1), SR_sig1, ie_lc_new_import(1,k1))
      end do
      do k1 = 1, org_ele%nnod_4_ele
        call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,  &
     &      istack_new_ele_export, istack_new_ele_import,               &
     &      ie_rank_new_export(1,k1), SR_sig1, ie_rank_new_import(1,k1))
      end do
!
!
!$omp parallel private(k1,i,kst,jst,ist,num)
      do k1 = 1, org_ele%nnod_4_ele
        do i = 1, nod_comm%num_neib
          kst = nod_comm%istack_export(i-1)
          jst = istack_new_import(i-1)
          ist = istack_new_ele_import(i-1)
          num = istack_new_ele_import(i) - istack_new_ele_import(i-1)
!$omp do private(inum,jnum)
          do inum = 1, num
            jnum = ie_new_import(inum+ist,k1)
            if(jnum .lt. 0) then
              ie_new_import(inum+ist,k1) = -jnum + kst
            else if(jnum .gt. 0) then
              ie_new_import(inum+ist,k1) =  jnum + jst
            end if
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
        ist = nod_comm%istack_import(i-1)
        num = nod_comm%istack_import(i) - nod_comm%istack_import(i-1)
!
      allocate(nele_import_tmp(nprocs))
      allocate(istack_ele_import_tmp(0:nprocs))
      nele_import_tmp(1:nprocs) =    0
      istack_ele_import_tmp(0:nprocs) = 0
!
      allocate(irank_org_ele_new_import(ntot_new_ele_import))
      allocate(index_ele_import_tmp(ntot_new_ele_import))
      allocate(irank_ele_import_tmp(ntot_new_ele_import))
      allocate(iele_lc_import_tmp(ntot_new_ele_import))
!
!$omp parallel do private(i,ip)
      do i = 1, ntot_new_ele_import
        index_ele_import_tmp(i) = i
        iele_lc_import_tmp(i) =   0
        irank_ele_import_tmp(i) = irank_ele_new_import(i)
        irank_org_ele_new_import(i) = -1
      end do
!$omp end parallel do
!
!$omp parallel private(i,ip,ist,ied)
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i)
        ist = istack_new_ele_import(i-1) + 1
        ied = istack_new_ele_import(i)
!$omp do private(inum)
        do inum = ist, ied
          irank_org_ele_new_import(inum) = ip
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      if(ntot_new_ele_import .gt. 1) then
        call quicksort_w_index                                          &
     &     (ntot_new_ele_import, irank_ele_import_tmp,                  &
     &      ione, ntot_new_ele_import, index_ele_import_tmp)
      end if
!
!$omp parallel do private(i,icou)
      do i = 1, ntot_new_ele_import
        icou = index_ele_import_tmp(i)
        iele_lc_import_tmp(i) = iele_lc_new_import(icou)
      end do
!$omp end parallel do
!
!$omp parallel workshare
      nele_import_tmp(1:nprocs) = 0
!$omp end parallel workshare
      do i = 1, ntot_new_ele_import
        irank = irank_ele_import_tmp(i)
        nele_import_tmp(irank+1) = nele_import_tmp(irank+1) + 1
      end do
      do ip = 1, nprocs
        istack_ele_import_tmp(ip) = istack_ele_import_tmp(ip-1)         &
     &                             + nele_import_tmp(ip)
      end do
!
      do ip = 1, nprocs
        ist = istack_ele_import_tmp(ip-1)
        if(nele_import_tmp(ip) .gt. 1) then
          call quicksort_w_index                                        &
     &       (nele_import_tmp(ip), iele_lc_import_tmp(ist+1),           &
     &        ione, nele_import_tmp(ip), index_ele_import_tmp(ist+1))
        end if
      end do
!
      ntot = 0
      do ip = 1, nprocs
        ist = istack_ele_import_tmp(ip-1)
        do inum = 1, nele_import_tmp(ip) - 1
          if(iele_lc_import_tmp(ist+inum)                               &
     &        .ne. iele_lc_import_tmp(ist+inum+1)) ntot = ntot + 1
        end do
        if(nele_import_tmp(ip) .gt. 0) ntot = ntot + 1
      end do
      ntot_sorted_ele_import_tmp = ntot
!
      allocate(istack_sorted_ele_import_pe(0:nprocs))
      allocate(istack_sorted_ele_import_tmp(0:ntot_sorted_ele_import_tmp))
      istack_sorted_ele_import_pe(0) =  0
      istack_sorted_ele_import_tmp(0) = 0
!
      icou = 0
      do ip = 1, nprocs
        ist = istack_ele_import_tmp(ip-1)
        do inum = 1, nele_import_tmp(ip)-1
          if(iele_lc_import_tmp(ist+inum)                               &
     &        .ne. iele_lc_import_tmp(ist+inum+1)) then
            icou = icou + 1
            istack_sorted_ele_import_tmp(icou) = ist + inum
          end if
        end do
        if(nele_import_tmp(ip) .gt. 0) then
          icou = icou + 1
          istack_sorted_ele_import_tmp(icou) = ist + nele_import_tmp(ip)
        end if
        istack_sorted_ele_import_pe(ip) =    icou
      end do
!
      allocate(idx_home_sorted_ele_import(istack_sorted_ele_import_pe(nprocs)))
      idx_home_sorted_ele_import(1:istack_sorted_ele_import_pe(nprocs)) = -1
      allocate(idx_home_for_ele_import(ntot_new_ele_import))
      idx_home_for_ele_import(1:ntot_new_ele_import) = -1
!
      do ip = 1, nprocs
        ist = istack_sorted_ele_import_pe(ip-1) + 1
        ied = istack_sorted_ele_import_pe(ip)
        do inum = ist, ied
          jst = istack_sorted_ele_import_tmp(inum-1) + 1
          jed = istack_sorted_ele_import_tmp(inum)
          do jnum = jst, jed
            kdx = index_ele_import_tmp(jnum)
            krank = irank_org_ele_new_import(kdx)
            if(     irank_ele_new_import(kdx) .eq. krank                &
     &        .and. irank_ele_new_import(kdx) .eq. ip-1) then
              idx_home_sorted_ele_import(inum) = kdx
              exit
            end if
          end do
        end do
      end do
!
      icou = 0
      do ip = 1, nprocs
        ist = istack_sorted_ele_import_pe(ip-1) + 1
        ied = istack_sorted_ele_import_pe(ip)
        do inum = ist, ied
          if(idx_home_sorted_ele_import(inum) .gt. 0) cycle
!
          icou = icou + 1
          jst = istack_sorted_ele_import_tmp(inum-1) + 1
          jed = istack_sorted_ele_import_tmp(inum)
          do jnum = jst, jed
            kdx =   index_ele_import_tmp(jnum)
            if(irank_ele_new_import(kdx) .eq. ip-1) then
              idx_home_sorted_ele_import(inum) = kdx
              exit
            end if
          end do
        end do
      end do
      write(*,*) my_rank, 'Misisng element in import list: ', icou
!
      icou = 0
      do ip = 1, nprocs
        ist = istack_sorted_ele_import_pe(ip-1) + 1
        ied = istack_sorted_ele_import_pe(ip)
        do inum = ist, ied
          if(idx_home_sorted_ele_import(inum) .gt. 0) cycle
!
          jst = istack_sorted_ele_import_tmp(inum-1) + 1
          jed = istack_sorted_ele_import_tmp(inum)
          kdx = index_ele_import_tmp(jst)
          idx_home_sorted_ele_import(inum) = kdx
          icou = icou + 1
        end do
      end do
      write(*,*) my_rank, 'Required import element from new domain : ', icou
!
      do ip = 1, nprocs
        ist = istack_sorted_ele_import_pe(ip-1) + 1
        ied = istack_sorted_ele_import_pe(ip)
        do inum = ist, ied
          jst = istack_sorted_ele_import_tmp(inum-1) + 1
          jed = istack_sorted_ele_import_tmp(inum)
          kdx = idx_home_sorted_ele_import(inum)
!
          idx_home_for_ele_import(jst:jed) = kdx
        end do
      end do
!
      write(*,*) my_rank, 'org_neib', nod_comm%id_neib
      write(*,*) my_rank, 'new_neib', new_ele_comm%id_neib
      write(*,*) my_rank, 'Totals', nod_comm%ntot_import,               &
     &          sum(nele_import_tmp),  ntot_sorted_ele_import_tmp
      do ip = 1, nprocs
        write(*,*) my_rank, ' to ', ip-1,  ' nele_import_tmp ',         &
     &            nele_import_tmp(ip), istack_sorted_ele_import_pe(ip)
      end do
!
      write(*,*) my_rank, 'new_ele_comm%num_neib', new_ele_comm%num_neib
      call alloc_import_num(new_ele_comm)
!
      new_ele_comm%istack_import(0) = 0
      do i = 1, new_ele_comm%num_neib
        irank = new_ele_comm%id_neib(i)
        new_ele_comm%num_import(i) = istack_sorted_ele_import_pe(irank+1) &
     &                              - istack_sorted_ele_import_pe(irank)
        new_ele_comm%istack_import(i) = new_ele_comm%istack_import(i-1) &
     &                                 + new_ele_comm%num_import(i)
      end do
      new_ele_comm%ntot_import                                          &
     &       = new_ele_comm%istack_import(new_ele_comm%num_neib)
      write(*,*) my_rank, 'new_ele_comm%ntot_import', new_ele_comm%ntot_import
      call alloc_import_item(new_ele_comm)
!
      allocate(iele_gl_new_import_trim(new_ele_comm%ntot_import))
      allocate(ie_new_import_trim(new_ele_comm%ntot_import,org_ele%nnod_4_ele))
      allocate(ie_lc_new_import_trim(new_ele_comm%ntot_import,org_ele%nnod_4_ele))
      allocate(ie_rank_new_import_trim(new_ele_comm%ntot_import,org_ele%nnod_4_ele))
      allocate(item_new_ele_import_trim(new_ele_comm%ntot_import))
      allocate(iele_lc_new_import_trim(new_ele_comm%ntot_import))
      allocate(irank_new_ele_import_trim(new_ele_comm%ntot_import))
!
      do i = 1, new_ele_comm%num_neib
        irank = new_ele_comm%id_neib(i)
        ist = istack_sorted_ele_import_pe(irank)
        jst = new_ele_comm%istack_import(i-1)
        do inum = 1, new_ele_comm%num_import(i)
          jcou = inum + jst
          new_ele_comm%item_import(jcou)                                &
     &         = jcou + org_ele%numele
!
          jnum = idx_home_sorted_ele_import(inum+ist)
          item_new_ele_import_trim(jcou) =    item_new_ele_import(jnum)
          iele_lc_new_import_trim(jcou) = iele_lc_new_import(jnum)
          irank_new_ele_import_trim(jcou) =   irank_ele_new_import(jnum)
!
          iele_gl_new_import_trim(jcou) = iele_gl_new_import(jnum)
          do k1 = 1, org_ele%nnod_4_ele
            ie_new_import_trim(jcou,k1) = ie_new_import(jnum,k1)
            ie_lc_new_import_trim(jcou,k1) = ie_lc_new_import(jnum,k1)
            ie_rank_new_import_trim(jcou,k1) = ie_rank_new_import(jnum,k1)
          end do
        end do
      end do
!
      call alloc_export_num(new_ele_comm)
      call num_items_send_recv                                          &
     &   (new_ele_comm%num_neib, new_ele_comm%id_neib,                  &
     &    new_ele_comm%num_import, SR_sig1, new_ele_comm%num_export,    &
     &    new_ele_comm%istack_export, new_ele_comm%ntot_export)
      call alloc_export_item(new_ele_comm)
!
      allocate(iele_gl_new_export_trim(new_ele_comm%ntot_export))
      allocate(ie_new_export_trim(new_ele_comm%ntot_export,org_ele%nnod_4_ele))
      allocate(ie_lc_new_export_trim(new_ele_comm%ntot_export,org_ele%nnod_4_ele))
      allocate(ie_rank_new_export_trim(new_ele_comm%ntot_export,org_ele%nnod_4_ele))
      allocate(item_new_ele_export_trim(new_ele_comm%ntot_export))
      allocate(iele_lc_new_export_trim(new_ele_comm%ntot_export))
      allocate(irank_new_ele_export_trim(new_ele_comm%ntot_export))
!
      call comm_items_send_recv                                         &
     &   (new_ele_comm%num_neib, new_ele_comm%id_neib,                  &
     &    new_ele_comm%istack_import, new_ele_comm%istack_export,       &
     &    item_new_ele_import_trim, SR_sig1, item_new_ele_export_trim)
      call comm_items_send_recv                                         &
     &   (new_ele_comm%num_neib, new_ele_comm%id_neib,                  &
     &    new_ele_comm%istack_import, new_ele_comm%istack_export,       &
     &    iele_lc_new_import_trim, SR_sig1, iele_lc_new_export_trim)
      call comm_items_send_recv                                         &
     &   (new_ele_comm%num_neib, new_ele_comm%id_neib,                  &
     &    new_ele_comm%istack_import, new_ele_comm%istack_export,       &
     &    irank_new_ele_import_trim, SR_sig1, irank_new_ele_export_trim)
!
      call int8_items_send_recv                                         &
     &   (new_ele_comm%num_neib, new_ele_comm%id_neib,                  &
     &    new_ele_comm%istack_import, new_ele_comm%istack_export,       &
     &    iele_gl_new_import_trim, SR_sig1, iele_gl_new_export_trim)
!
      do k1 = 1, org_ele%nnod_4_ele
        call comm_items_send_recv                                       &
     &     (new_ele_comm%num_neib, new_ele_comm%id_neib,                &
     &      new_ele_comm%istack_import, new_ele_comm%istack_export,     &
     &     ie_new_import_trim(1,k1), SR_sig1, ie_new_export_trim(1,k1))
      end do
      do k1 = 1, org_ele%nnod_4_ele
        call comm_items_send_recv                                       &
     &     (new_ele_comm%num_neib, new_ele_comm%id_neib,                &
     &      new_ele_comm%istack_import, new_ele_comm%istack_export,     &
     &     ie_lc_new_import_trim(1,k1), SR_sig1, ie_lc_new_export_trim(1,k1))
      end do
      do k1 = 1, org_ele%nnod_4_ele
        call comm_items_send_recv                                       &
     &     (new_ele_comm%num_neib, new_ele_comm%id_neib,                &
     &      new_ele_comm%istack_import, new_ele_comm%istack_export,     &
     &     ie_rank_new_import_trim(1,k1), SR_sig1, ie_rank_new_export_trim(1,k1))
      end do
!
!      icou = 0
!      do inum = 1, new_ele_comm%ntot_export
!        iele = iele_lc_new_export_trim(inum)
!        if(org_ele%iele_global(iele) .ne. iele_gl_new_export_trim(inum)) &
!     &     icou = icou + 1
!        write(50+my_rank,*) inum, 'global ele id',          &
!     &      org_ele%iele_global(iele), iele_gl_new_export_trim(inum)
!      end do
!      write(*,*) my_rank, 'failed global element ID', icou
!
      icou = 0
      do inum = 1, new_ele_comm%ntot_export
        iele = iele_lc_new_export_trim(inum)
        if(iele_dbl%index(iele) .ne. iele_lc_new_export_trim(inum)      &
         .or. iele_dbl%irank(iele) .ne. irank_new_ele_export_trim(inum) &
     &       )  icou = icou + 1
!        write(50+my_rank,*) inum, 'global ele id',          &
!     &      org_ele%iele_global(iele), iele_gl_new_export_trim(inum)
      end do
      write(*,*) my_rank, 'failed double element ID', icou
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
