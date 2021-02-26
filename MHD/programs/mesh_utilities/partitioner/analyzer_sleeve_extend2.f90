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
!      if (iflag_debug.gt.0) write(*,*) 'extend_node_comm_table'
!      call extend_node_comm_table                                      &
!     &   (mesh%nod_comm, mesh%node, dbl_id1, next_tbl%neib_nod,        &
!     &    newmesh%nod_comm, newmesh%node)
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
      subroutine const_extended_nod_and_comm                            &
     &         (mesh, neib_nod, part_param, part_grp, new_ids_on_org,   &
     &          new_comm, new_node, part_tbl, ext_tbl)
!
      use t_para_double_numbering
      use t_repart_double_numberings
      use t_control_param_vol_grping
      use t_sorting_for_repartition
      use external_group_4_new_part
      use ext_of_int_grp_4_new_part
      use const_comm_tbl_to_new_mesh
      use const_repart_mesh_data
      use const_repart_comm_tbl
      use check_data_for_repartition
!
      type(mesh_geometry), intent(in) :: mesh
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(volume_partioning_param), intent(in) :: part_param
      type(group_data), intent(in) :: part_grp
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
      type(calypso_comm_table), intent(inout) :: part_tbl
      type(calypso_comm_table), intent(inout) :: ext_tbl
      type(node_ele_double_number), intent(inout) :: new_ids_on_org
!
      type(group_data) :: ext_int_grp
      type(group_data) :: ext_grp
      type(sorting_data_for_repartition) :: sort_nod
      type(node_ele_double_number) :: recieved_new_nod_ids
!
      integer(kind = kint) :: numnod, internal_node
      integer(kind = kint) :: i
!
!
      call set_node_double_numbering                                    &
     &   (mesh%node, mesh%nod_comm, new_ids_on_org)
!
      part_tbl%iflag_self_copy = 1
      part_tbl%nrank_export = 1
      call alloc_calypso_export_num(part_tbl)
!
      part_tbl%ntot_export = mesh%node%internal_node
      part_tbl%irank_export = my_rank
      part_tbl%num_export(1) = mesh%node%internal_node
      part_tbl%istack_export(0) = 0
      part_tbl%istack_export(1) = mesh%node%internal_node
!
      call alloc_calypso_export_item(part_tbl)
      do i = 1, part_tbl%num_export(1)
        part_tbl%item_export(i) = i
      end do
!
      part_tbl%nrank_import = 1
      call alloc_calypso_import_num(part_tbl)
!
      part_tbl%ntot_import = mesh%node%internal_node
      part_tbl%irank_import = my_rank
      part_tbl%num_import(1) = mesh%node%internal_node
      part_tbl%istack_import(0) = 0
      part_tbl%istack_import(1) = mesh%node%internal_node
!
      call alloc_calypso_import_item(mesh%node%internal_node, part_tbl)
      do i = 1, part_tbl%num_export(1)
        part_tbl%item_import(i) = i
        part_tbl%irev_import(i) = i
      end do
!
!
      call const_ext_grp_sleeve_ext(mesh%node, mesh%nod_comm, ext_grp)
!       Re-partitioning for external node
      call const_ext_of_int_grp_slv_ext(mesh%node, mesh%nod_comm, neib_nod,            &
     &    ext_int_grp)
!      call const_ext_comm_tbl_to_new_part                              &
!     &   (ext_int_grp, part_tbl, ext_tbl)
!      call dealloc_group(ext_int_grp)
!      call dealloc_group(ext_grp)
!
!      Set local recieved_new_nod_ids in internal node
!      internal_node =                part_tbl%ntot_import
!      numnod = ext_tbl%ntot_import + part_tbl%ntot_import
!
!      call alloc_double_numbering(numnod, recieved_new_nod_ids)
!      call ext_node_dbl_numbering_by_SR(mesh%node, ext_tbl,            &
!     &    new_ids_on_org, internal_node, recieved_new_nod_ids)
!
!      call alloc_sorting_data(ext_tbl%ntot_import, sort_nod)
!      call sort_node_by_domain_and_index                               &
!     &   (internal_node, recieved_new_nod_ids, ext_tbl, sort_nod)
!      call dealloc_double_numbering(recieved_new_nod_ids)
!
!      call const_repartitioned_comm_tbl                                &
!     &   (internal_node, sort_nod%num_recv, sort_nod%nrecv_trim,       &
!     &    ext_tbl%ntot_import, sort_nod%irank_sorted,                  &
!     &    sort_nod%id_sorted, sort_nod%iflag_dup, new_comm)
!
!      call check_num_of_neighbourings                                  &
!     &   (new_comm, ext_tbl, sort_nod%nrecv_trim)
!      call check_new_node_comm_table(my_rank, new_comm)
!      call dealloc_sorting_data(sort_nod)
!
!      call set_repart_node_position                                    &
!     &   (part_tbl, mesh%node, new_comm, new_node)
!      call check_repart_node_transfer                                  &
!     &   (mesh%nod_comm, mesh%node, new_comm, new_node,                &
!     &    part_tbl, new_ids_on_org)
!
      end subroutine const_extended_nod_and_comm
!
! ----------------------------------------------------------------------
!
      subroutine const_ext_grp_sleeve_ext(node, nod_comm, ext_grp)
!
      use set_parallel_file_name
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      type(group_data), intent(inout) :: ext_grp
!
      character(len = kchara) :: chara_tmp
      integer(kind = kint) :: i, inum, ip, ist, jst, num
!
!
      ext_grp%num_grp = nprocs
      call alloc_group_num(ext_grp)
!
      do i = 1, nprocs
        write(chara_tmp,'(a)') 'ext_Domain_'
        call add_index_after_name                                       &
     &         ((i-1), chara_tmp, ext_grp%grp_name(i))
      end do
!
      ext_grp%istack_grp(0:nod_comm%num_neib) = 0
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i) + 1
        ext_grp%istack_grp(ip) = nod_comm%istack_import(i)              &
     &                        - nod_comm%istack_import(i-1)
      end do
      do ip = 1, ext_grp%num_grp
        ext_grp%istack_grp(ip) = ext_grp%istack_grp(ip-1)               &
     &                          + ext_grp%istack_grp(ip)
      end do
      ext_grp%num_item = ext_grp%istack_grp(ext_grp%num_grp)
      call alloc_group_item(ext_grp)
!
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i)
        jst = ext_grp%istack_grp(ip)
        ist = nod_comm%istack_import(i-1)
        num = nod_comm%istack_import(i) - nod_comm%istack_import(i-1)
        do inum = 1, num
          ext_grp%item_grp(inum+jst) = nod_comm%item_import(inum+ist)
        end do
      end do
!
      end subroutine const_ext_grp_sleeve_ext
!
! ----------------------------------------------------------------------
!
      subroutine const_ext_of_int_grp_slv_ext(node, nod_comm, neib_nod, &
     &          ext_int_grp)
!
      use t_control_param_vol_grping
!
      use quicksort
      use set_repartition_group_name
      use set_parallel_file_name
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(group_data), intent(inout) :: ext_int_grp
!
      integer(kind = kint), allocatable :: iflag_nod(:)
      character(len = kchara) :: chara_tmp
      integer(kind = kint) :: i, ist, num
!
!
      allocate(iflag_nod(node%numnod))
!$omp parallel workshare
      iflag_nod(1:node%numnod) = 1
!$omp end parallel workshare
!
      ext_int_grp%num_grp = nprocs
      call alloc_group_num(ext_int_grp)
!
      do i = 1, nprocs
        write(chara_tmp,'(a)') 'ext_int_Domain_'
        call add_index_after_name                                       &
     &         ((i-1), chara_tmp, ext_int_grp%grp_name(i))
      end do
!
!
      call count_ext_of_int_grp_slv_exp                                &
     &   (node, nod_comm, neib_nod,                            &
     &    ext_int_grp%num_grp, ext_int_grp%num_item,                    &
     &    ext_int_grp%istack_grp, iflag_nod)
      call alloc_group_item(ext_int_grp)
!
      call set_ext_of_int_grp_elv_exp                                &
     &   (node, nod_comm, neib_nod,                            &
     &    ext_int_grp%num_grp, ext_int_grp%num_item,                    &
     &    ext_int_grp%istack_grp, ext_int_grp%item_grp, iflag_nod)
!
!$omp parallel do private(i,ist,num)
      do i = 1, ext_int_grp%num_grp
        ist = ext_int_grp%istack_grp(i-1)
        num = ext_int_grp%istack_grp(i  ) - ist
        if(num .gt. 1) then
          call quicksort_int(num, ext_int_grp%item_grp(ist+1),          &
     &                       ione, num)
        end if
      end do
!$omp end parallel do
!
      deallocate(iflag_nod)
!
      end subroutine const_ext_of_int_grp_slv_ext
!
! ----------------------------------------------------------------------
!
      subroutine count_ext_of_int_grp_slv_exp                           &
     &         (node, nod_comm, neib_nod,                      &
     &          num_ext_grp, ntot_ext_grp, istack_ext_grp, iflag_nod)
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      integer(kind = kint), intent(in) :: num_ext_grp
!
      integer(kind = kint), intent(inout) :: ntot_ext_grp
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_ext_grp(0:num_ext_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node%numnod)
!
      integer(kind = kint) :: inum, inod, ist, ied
      integer(kind = kint) :: jnum, jnod, jst, jed
      integer(kind = kint) :: i, ip, icou
!
!
      istack_ext_grp(0:nprocs) = 0
      istack_ext_grp(my_rank+1) = nod_comm%ntot_import
!
      do i = 1, nprocs
        ip = nod_comm%id_neib(i) + 1
!$omp parallel workshare
        iflag_nod(1:node%internal_node) = 1
!$omp end parallel workshare
!
        ist = nod_comm%istack_import(i-1) + 1
        ied = nod_comm%istack_import(i)
        do inum = ist, ied
          inod = nod_comm%item_import(inum)
          jst = neib_nod%istack_next(inod-1) + 1
          jed = neib_nod%istack_next(inod)
          do jnum = jst, jed
            jnod = neib_nod%inod_next(jnum)
            if(iflag_nod(jnod) .gt. 0) then
              icou = icou + 1
              iflag_nod(jnod) = 0
            end if
          end do
        end do
        istack_ext_grp(ip) = icou
      end do
!
      do ip = 1, nprocs
        istack_ext_grp(ip) = istack_ext_grp(ip-1) + istack_ext_grp(ip)
      end do
      ntot_ext_grp = istack_ext_grp(nprocs)
!
      end subroutine count_ext_of_int_grp_slv_exp
!
! ----------------------------------------------------------------------
!
      subroutine set_ext_of_int_grp_elv_exp                          &
     &         (node, nod_comm, neib_nod,                      &
     &          num_ext_grp, ntot_ext_grp, istack_ext_grp,              &
     &          item_ext_grp, iflag_nod)
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      integer(kind = kint), intent(in) :: num_ext_grp
      integer(kind = kint), intent(in) :: ntot_ext_grp
      integer(kind = kint), intent(in) :: istack_ext_grp(0:num_ext_grp)
!
      integer(kind = kint), intent(inout) :: item_ext_grp(ntot_ext_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node%numnod)
!
!
      integer(kind = kint) :: inum, inod, ist, ied
      integer(kind = kint) :: jnum, jnod, jst, jed
      integer(kind = kint) :: i, ip, icou
!
!
      ist = istack_ext_grp(my_rank)
      do inum = 1, nod_comm%ntot_import
        item_ext_grp(inum+ist) = nod_comm%item_import(inum)
      end do
!
      do i = 1, nprocs
        ip = nod_comm%id_neib(i)
        icou = istack_ext_grp(ip)
!$omp parallel workshare
        iflag_nod(1:node%numnod) = 1
!$omp end parallel workshare
!
        ist = nod_comm%istack_import(i-1) + 1
        ied = nod_comm%istack_import(i)
        do inum = ist, ied
          inod = nod_comm%item_import(inum)
          jst = neib_nod%istack_next(inod-1) + 1
          jed = neib_nod%istack_next(inod)
          do jnum = jst, jed
            jnod = neib_nod%inod_next(jnum)
            if(iflag_nod(jnod) .gt. 0) then
              icou = icou + 1
              item_ext_grp(icou) = jnod
              iflag_nod(jnod) =    0
            end if
          end do
        end do
      end do
!
      end subroutine set_ext_of_int_grp_elv_exp
!
! ----------------------------------------------------------------------
!
      end module analyzer_sleeve_extend2
