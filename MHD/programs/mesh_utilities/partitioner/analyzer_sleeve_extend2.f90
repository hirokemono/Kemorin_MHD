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
      use m_solver_SR
      use reverse_SR_int
      use reverse_SR_int8
      use reverse_SR_real
!
      use calypso_mpi_int
      use solver_SR_type
      use extend_comm_table_SR
      use mark_export_nod_ele_extend
      use cal_minmax_and_stacks
      use find_extended_node_and_ele
      use find_extended_comm_table
      use quicksort
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
      type(communication_table) :: new_nod_comm
!
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
      integer(kind = kint), allocatable :: num_import_slashed(:)
      integer(kind = kint), allocatable :: index_4_import_tmp(:)
      integer(kind = kint), allocatable :: inod_lc_import_tmp(:)
      integer(kind = kint), allocatable :: iflag_nod_recv_pe(:)
!
      type(mark_for_each_comm), allocatable :: mark_nod(:)
      type(comm_table_for_each_pe) :: each_comm
!
      integer(kind = kint) :: inum, inod, i, ip, ist, ied, jp
      real(kind = kreal) :: dist_max = 0.02d0
!
      integer(kind = kint) :: icou, jnum
!
!
      allocate(iflag_node(org_node%numnod))
      allocate(distance(org_node%numnod))
!$omp parallel workshare
      iflag_node(1:org_node%numnod) = 0
      distance(1:org_node%numnod) =   0
!$omp end parallel workshare
!
!
      allocate(mark_nod(nod_comm%num_neib))
      do i = 1, nod_comm%num_neib
        call init_comm_table_for_each(i, org_node, nod_comm, each_comm)
        call mark_next_node_of_export2(dist_max, org_node, neib_nod,    &
     &      each_comm, mark_nod(i), iflag_node, distance)
        call dealloc_comm_table_for_each(each_comm)
      end do
      write(*,*) my_rank, 'mark_nod%nnod_marked',   &
     &    mark_nod(1:nod_comm%num_neib)%nnod_marked
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
          ip = dbl_idx%irank(inod)
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
          ip = dbl_idx%irank(inod)
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
          inod_lc_new_export(icou) =   dbl_idx%index(inod)
          irank_nod_new_export(icou) = dbl_idx%irank(inod)
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
      num_import_tmp(1:nprocs) = 0
!
      allocate(iflag_nod_recv_pe(ntot_new_import))
      allocate(index_4_import_tmp(ntot_new_import))
      allocate(inod_lc_import_tmp(ntot_new_import))
!
!$omp parallel do private(i,ip)
      do i = 1, ntot_new_import
        index_4_import_tmp(i) = i
        inod_lc_import_tmp(i) = 0
      end do
!$omp end parallel do
!
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i)
        ist = istack_new_import(i-1) + 1
        ied = istack_new_import(i)
        do inum = ist, ied
          jp = irank_nod_new_import(inum)
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
        call quicksort_w_index(ntot_new_import, irank_nod_new_export,   &
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
        ip = irank_nod_new_import(i)
        num_import_tmp(ip+1) = num_import_tmp(ip+1) + 1
      end do
!
      ist = 0
      do ip = 1, nprocs
        if(num_import_tmp(ip) .gt. 1) then
          call quicksort_w_index                                        &
     &       (ntot_new_import, inod_lc_import_tmp(ist+1),               &
     &        ione, num_import_tmp(ip), index_4_import_tmp(ist+1))
        end if
        ist = ist + num_import_tmp(ip)
      end do
!
      allocate(num_import_slashed(nprocs))
      num_import_slashed(1:nprocs) = 0
!
      ist = 0
      do ip = 1, nprocs
        icou = 0
        if(iflag_recv_pe(ip) .eq. 1) then
          do inum = 1, num_import_tmp(ip)
            jnum = index_4_import_tmp(inum+ist)
            icou = icou + mod(iflag_nod_recv_pe(jnum),2)
          end do
        else if(iflag_recv_pe(ip) .eq. 2) then
          if(num_import_tmp(ip) .gt. 1) icou = 1
          do inum = 2, num_import_tmp(ip)
            if(inod_lc_import_tmp(inum)                                 &
     &            .ne. inod_lc_import_tmp(inum-1)) icou = icou + 1
          end do
        end if
        ist = ist + num_import_tmp(ip)
        num_import_slashed(ip) = icou
      end do
!
      write(*,*) my_rank, 'org_neib', nod_comm%id_neib
      write(*,*) my_rank, 'new_neib', new_nod_comm%id_neib
      write(*,*) my_rank, 'Totals', nod_comm%ntot_import,               &
     &                    sum(num_import_tmp), sum(num_import_slashed)
      do ip = 1, nprocs
        write(*,*) my_rank, ' to ', ip-1,  ' num_import_tmp ',         &
     &            num_import_tmp(ip), num_import_slashed(ip)
      end do
!
      call calypso_mpi_barrier

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
!     &    (added_comm%istack_export(i-1), org_node, dbl_idx,           &
!     &     mark_nod(i)%nnod_marked, mark_nod(i)%inod_marked, send_nbuf)
!        deallocate(mark_nod(i)%inod_marked)
!      end do
!
      deallocate(mark_nod)
      deallocate(iflag_node)
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
      subroutine mark_next_node_of_export2                              &
     &         (dist_max, node, neib_nod, each_comm, mark_nod,          &
     &          iflag_node, distance)
!
      use mark_export_nod_ele_extend
!
      real(kind = kreal), intent(in) :: dist_max
      type(node_data), intent(in) :: node
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(comm_table_for_each_pe), intent(inout) :: each_comm
      type(mark_for_each_comm), intent(inout) :: mark_nod
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
      real(kind = kreal), intent(inout) :: distance(node%numnod)
!
      integer(kind = kint) :: inum, inod, icou, idummy, jcou
      integer(kind = kint) :: jst, jed, jnum, jnod
      real(kind = kreal) :: dist
!
!
!$omp parallel workshare
      iflag_node(1:node%numnod) = 0
      distance(1:node%numnod) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel do private(inum,inod)
      do inum = 1, each_comm%num_each_import
        inod = each_comm%item_each_import(inum)
        iflag_node(inod) = -2
      end do
!$omp end parallel do
!
      do inum = 1, each_comm%num_each_import
        inod = each_comm%item_each_import(inum)
        jst = neib_nod%istack_next(inod-1) + 1
        jed = neib_nod%istack_next(inod)
        do jnum = jst, jed
          jnod = neib_nod%inod_next(jnum)
          if(iflag_node(jnod) .eq. -2) cycle
!
          iflag_node(jnod) = 1
!           dist = 1.0d0
          dist = sqrt((node%xx(jnod,1) - node%xx(inod,1))**2           &
     &              + (node%xx(jnod,2) - node%xx(inod,2))**2           &
     &              + (node%xx(jnod,3) - node%xx(inod,3))**2)
          if(distance(jnod) .eq. 0.0d0) then
            distance(jnod) = dist + distance(inod)
          else
            distance(jnod) = min(dist+distance(inod), distance(jnod))
          end if
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
          jst = neib_nod%istack_next(inod-1) + 1
          jed = neib_nod%istack_next(inod)
          do jnum = jst, jed
            jnod = neib_nod%inod_next(jnum)
            if(iflag_node(jnod) .ge. 0) then
!
!               dist = 1.0d0
              dist = sqrt((node%xx(jnod,1) - node%xx(inod,1))**2       &
     &                  + (node%xx(jnod,2) - node%xx(inod,2))**2       &
     &                  + (node%xx(jnod,3) - node%xx(inod,3))**2)
              if(distance(jnod) .eq. 0.0d0) then
                iflag_node(jnod) = 1
                distance(jnod) = dist + distance(inod)
              else
                distance(jnod) = min(dist+distance(inod), distance(jnod))
              end if
            end if
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
      write(*,*) my_rank, 'Maximum extend size is ', idummy
!
      mark_nod%nnod_marked = 0
      do inod = 1, node%numnod
        if(iflag_node(inod) .eq. -1) then
          mark_nod%nnod_marked = mark_nod%nnod_marked + 1
        end if
      end do
      allocate(mark_nod%inod_marked(inod))
      allocate(mark_nod%dist_marked(inod))
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
      end subroutine mark_next_node_of_export2
!
!  ---------------------------------------------------------------------
!
      end module analyzer_sleeve_extend2
