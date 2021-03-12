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
      type(mesh_data), save, private :: newfem_EXT
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
     &                           fem_EXT%mesh, fem_EXT%group,           &
     &                           newfem_EXT%mesh, newfem_EXT%group)
      call dealloc_mesh_data(fem_EXT%mesh, fem_EXT%group)
!
      call mpi_output_mesh                                              &
     &   (part_p1%distribute_mesh_file, newfem_EXT%mesh, newfem_EXT%group)
      call dealloc_mesh_data(newfem_EXT%mesh, newfem_EXT%group)
      return
!
      if (iflag_debug.gt.0) write(*,*) 'pickup_surface_mesh'
      call pickup_surface_mesh                                          &
     &   (part_p1%distribute_mesh_file, par_viexw_ex)
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
      subroutine sleeve_extension_loop2(num_level, mesh, group,         &
     &          newmesh, newgroup)
!
      use nod_and_ele_derived_info
      use const_element_comm_tables
!
      integer(kind = kint), intent(in) :: num_level
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: newgroup
!
      type(communication_table) :: ele_comm, new_ele_comm
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
      call para_sleeve_extension2(mesh, group, ele_comm,                &
     &                            newmesh, newgroup, new_ele_comm)
      call dealloc_comm_table(new_ele_comm)
!
      call dealloc_comm_table(ele_comm)
      call dealloc_numele_stack(mesh%ele)
      call dealloc_nod_and_ele_infos(mesh)
!
      end subroutine sleeve_extension_loop2
!
! ----------------------------------------------------------------------
!
      subroutine para_sleeve_extension2(mesh, group, ele_comm,          &
     &          newmesh, newgroup, new_ele_comm)
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
      use check_sleeve_extend_mesh
!
      use extended_groups
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(communication_table), intent(inout) :: ele_comm
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: newgroup
      type(communication_table), intent(inout) :: new_ele_comm
!
      type(next_nod_ele_table), save :: next_tbl
      type(node_ele_double_number), save :: inod_dbl_org
      type(node_ele_double_number), save :: iele_dbl_org
      type(dist_from_wall_in_export) :: dist_4_comm
!
      integer :: i
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
      call extend_node_comm_table2                                      &
     &   (mesh%nod_comm, ele_comm, mesh%node, inod_dbl_org,             &
     &    mesh%ele, iele_dbl_org, next_tbl%neib_ele,                    &
     &    newmesh%nod_comm, newmesh%node, newmesh%ele, new_ele_comm,    &
     &    dist_4_comm)
!
      call check_extended_element                                       &
     &   (newmesh%nod_comm, newmesh%node, newmesh%ele, new_ele_comm)
!
!
      call s_extended_groups                                            &
     &   (mesh, group, newmesh, new_ele_comm, newgroup)
!
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
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
      call alloc_sph_node_geometry(newmesh%node)
      call set_nod_and_ele_infos(newmesh%node, newmesh%ele)
      call const_ele_comm_table(newmesh%node, newmesh%nod_comm,         &
     &                          ele_comm, newmesh%ele)
!
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+5)
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
      subroutine extend_node_comm_table2                                &
     &         (nod_comm, ele_comm, org_node, inod_dbl,                 &
     &          org_ele, iele_dbl, neib_ele, new_nod_comm,              &
     &          new_node, new_ele, new_ele_comm, dist_4_comm)
!
      use t_next_node_ele_4_node
      use t_para_double_numbering
      use t_mesh_for_sleeve_extend
      use t_trim_overlapped_import
      use t_ctl_param_sleeve_extend
      use t_flags_each_comm_extend
      use t_mark_node_ele_to_extend
      use t_comm_table_for_each_pe
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
      use cal_minmax_and_stacks
      use append_communication_table
      use append_extended_node
      use append_extended_element
      use trim_redundant_import_item
      use const_extended_neib_domain
      use set_mesh_for_sleeve_extend
      use trim_mesh_for_sleeve_extend
      use set_expanded_comm_table
      use check_slv_ext_local_node_id
      use checks_for_sleeve_extend
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(element_around_node), intent(in) :: neib_ele
!
      type(communication_table), intent(inout) :: new_nod_comm
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
      type(communication_table), intent(inout) :: new_ele_comm
      type(dist_from_wall_in_export), intent(inout) :: dist_4_comm
!
      type(sleeve_extension_param), save :: sleeve_exp_p
!>      Structure of double numbering
      type(node_ele_double_number) :: dbl_id2
!
!>      added_comm%item_export :: export table or flag to be added
!>      added_comm%item_import :: import table or flag to be added
      type(communication_table) :: added_comm
      type(node_buffer_2_extend) :: send_nbuf
      type(node_buffer_2_extend) :: recv_nbuf
!
      type(flags_each_comm_extend), save :: each_exp_flags
!
      integer(kind = kint) :: iflag_process_extend = 0
!
      type(communication_table) :: expand_ele_comm
      type(ele_data_for_sleeve_ext) :: exp_export_ie
      type(ele_data_for_sleeve_ext) :: exp_import_ie
      type(ele_data_for_sleeve_ext) :: trim_import_ie
      integer(kind = kint), allocatable :: iele_lc_import_trim(:)
!
      type(communication_table) :: expand_nod_comm
      type(node_data_for_sleeve_ext), save :: exp_export_xx
      type(node_data_for_sleeve_ext), save :: exp_import_xx
!
      type(sort_data_for_sleeve_trim), save :: sort_ele_import
      type(sort_data_for_sleeve_trim), save :: sort_nod_import
!
!
      type(node_data_for_sleeve_ext), save :: trim_import_xx
!
      type(data_for_trim_import), save :: ext_ele_trim
      type(data_for_trim_import), save :: ext_nod_trim
!
      integer(kind = kint), allocatable :: idx_nod_extend_to_trimmed(:)
      integer(kind = kint), allocatable :: inod_lc_new_import_trim(:)
      integer(kind = kint), allocatable :: inod_added_import(:)
!
      type(mark_for_each_comm), allocatable :: mark_nod(:)
      type(mark_for_each_comm), allocatable :: mark_ele(:)

      type(comm_table_for_each_pe) :: each_comm
!
      integer(kind = kint) :: ntot_failed_gl, nele_failed_gl
!
      integer(kind = kint) :: i, icou, jcou, ntot
!
      type(communication_table) :: add_nod_comm
      type(communication_table) :: add_ele_comm
!
      real(kind = kreal), allocatable :: vect_tmp(:,:)
!
!
      call alloc_flags_each_comm_extend                                 &
     &   (org_node%numnod, org_ele%numele, each_exp_flags)
!
      sleeve_exp_p%iflag_expand = iflag_distance
      sleeve_exp_p%dist_max =     0.05d0
!
      allocate(vect_tmp(org_node%numnod,3))
      allocate(mark_nod(nod_comm%num_neib))
      allocate(mark_ele(nod_comm%num_neib))
      icou = 0
      jcou = 0
      do i = 1, nod_comm%num_neib
        call alloc_comm_table_for_each(org_node, each_comm)
        call init_comm_table_for_each(i, org_node, nod_comm,            &
     &      dist_4_comm, each_comm, each_exp_flags%distance)
        call s_mark_node_ele_to_extend                                  &
     &     (sleeve_exp_p, org_node, org_ele, neib_ele, vect_tmp,        &
     &      each_comm, mark_nod(i), mark_ele(i), each_exp_flags)
        call dealloc_comm_table_for_each(each_comm)
!
        call check_missing_connect_to_extend                            &
    &      (org_node, org_ele, mark_ele(i), each_exp_flags%iflag_node,  &
    &       icou, jcou)
      end do
      call dealloc_flags_each_comm_extend(each_exp_flags)
      deallocate(vect_tmp)
!
!
      call calypso_mpi_reduce_one_int(icou, ntot_failed_gl, MPI_SUM, 0)
      call calypso_mpi_reduce_one_int(jcou, nele_failed_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Failed element list:',             &
     &                             ntot_failed_gl, nele_failed_gl
      deallocate(dist_4_comm%distance_in_export)
!
      write(*,*) my_rank, 'mark_nod%num_marked',                        &
     &          mark_nod(1:nod_comm%num_neib)%num_marked,               &
     &        ' of ', org_node%numnod
      write(*,*) my_rank, 'mark_ele%num_marked',                        &
     &          mark_ele(1:nod_comm%num_neib)%num_marked,               &
     &        ' of ', org_ele%numele
!
!
      call s_const_extended_neib_domain(nod_comm, inod_dbl, mark_nod,   &
     &    add_nod_comm, iflag_process_extend)
!
!
      expand_nod_comm%num_neib = nod_comm%num_neib
      call alloc_neighbouring_id(expand_nod_comm)
      call alloc_import_num(expand_nod_comm)
      call alloc_export_num(expand_nod_comm)
!
!$omp parallel workshare
      expand_nod_comm%id_neib(1:expand_nod_comm%num_neib)               &
     &        = nod_comm%id_neib(1:expand_nod_comm%num_neib)
!$omp end parallel workshare
!
!
      expand_ele_comm%num_neib = nod_comm%num_neib
      call alloc_neighbouring_id(expand_ele_comm)
      call alloc_import_num(expand_ele_comm)
      call alloc_export_num(expand_ele_comm)
!
!$omp parallel workshare
      expand_ele_comm%id_neib(1:expand_ele_comm%num_neib)               &
     &        = nod_comm%id_neib(1:expand_ele_comm%num_neib)
!$omp end parallel workshare
!
      call count_export_4_expanded_mesh                                 &
     &   (nod_comm, org_node, mark_nod, mark_ele,                       &
     &    expand_nod_comm%num_export, expand_ele_comm%num_export)
      call s_cal_total_and_stacks                                       &
     &   (nod_comm%num_neib, expand_nod_comm%num_export, izero,         &
     &    expand_nod_comm%istack_export, expand_nod_comm%ntot_export)
      call s_cal_total_and_stacks                                       &
     &   (nod_comm%num_neib, expand_ele_comm%num_export, izero,         &
     &    expand_ele_comm%istack_export, expand_ele_comm%ntot_export)

      call num_items_send_recv                                          &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_nod_comm%num_export, SR_sig1,                          &
     &    expand_nod_comm%num_import, expand_nod_comm%istack_import,    &
     &    expand_nod_comm%ntot_import)
      call num_items_send_recv                                          &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_ele_comm%num_export, SR_sig1,                          &
     &    expand_ele_comm%num_import, expand_ele_comm%istack_import,    &
     &    expand_ele_comm%ntot_import)
!
      call alloc_export_item(expand_nod_comm)
      call alloc_node_data_sleeve_ext(expand_nod_comm%ntot_export,      &
     &                                exp_export_xx)
!
      call alloc_export_item(expand_ele_comm)
      call alloc_ele_data_sleeve_ext                                    &
     &   (expand_ele_comm%ntot_export, org_ele%nnod_4_ele,              &
     &    exp_export_ie)
!
      call alloc_import_item(expand_nod_comm)
      call alloc_import_item(expand_ele_comm)
!
      call set_export_4_expanded_mesh(nod_comm, org_node, org_ele,      &
     &    inod_dbl, iele_dbl, mark_nod, mark_ele,                       &
     &    expand_nod_comm%ntot_export, expand_nod_comm%istack_export,   &
     &    expand_ele_comm%ntot_export, expand_ele_comm%istack_export,   &
     &    expand_nod_comm%item_export, exp_export_xx,                   &
     &    expand_ele_comm%item_export, exp_export_ie)
!
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    expand_nod_comm%istack_export, expand_nod_comm%istack_import, &
     &    expand_nod_comm%item_export, SR_sig1,                         &
     &    expand_nod_comm%item_import)
!
      call alloc_node_data_sleeve_ext(expand_nod_comm%ntot_import,      &
     &                                exp_import_xx)
      call send_extended_node_position(expand_nod_comm,                 &
     &                                 exp_export_xx, exp_import_xx)
      call dealloc_node_data_sleeve_ext(exp_export_xx)
!
!
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    expand_ele_comm%istack_export, expand_ele_comm%istack_import, &
     &    expand_ele_comm%item_export, SR_sig1,                         &
     &    expand_ele_comm%item_import)
!
      call alloc_ele_data_sleeve_ext                                    &
     &   (expand_ele_comm%ntot_import, org_ele%nnod_4_ele,              &
     &    exp_import_ie)
      call send_extended_element_connect(org_ele, expand_ele_comm,      &
     &    exp_export_ie, exp_import_ie)
      call dealloc_ele_data_sleeve_ext(exp_export_ie)
!
      call alloc_sort_data_sleeve_ext                                   &
     &   (nprocs, expand_nod_comm%ntot_import, sort_nod_import)
      call sort_import_by_pe_and_local_id(nprocs, nod_comm,             &
     &    expand_nod_comm, exp_import_xx%irank_comm, sort_nod_import)
!
      call trim_overlapped_sleeve_ext                                   &
     &   (expand_nod_comm%ntot_import, exp_import_xx%irank_comm,        &
     &    sort_nod_import, ext_nod_trim)
      if(i_debug .gt. 0) then
        call check_overlapped_sleeve_ext                                &
     &    (nod_comm, add_nod_comm, sort_nod_import, ext_nod_trim)
      end if
!
!
      allocate(idx_nod_extend_to_trimmed(expand_nod_comm%ntot_import))
      call find_home_import_item_by_trim                                &
     &   (nprocs, expand_nod_comm%ntot_import, sort_nod_import,         &
     &    ext_nod_trim, idx_nod_extend_to_trimmed, icou)
      call calypso_mpi_reduce_one_int(icou, ntot_failed_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &      'Missing import item in trimmed:', ntot_failed_gl
      call dealloc_sort_data_sleeve_ext(sort_nod_import)
!
      call alloc_import_num(add_nod_comm)
      call count_import_item_for_extend                                 &
     &   (nprocs, ext_nod_trim%istack_trimmed_pe,                       &
     &    add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%num_import)
      call s_cal_total_and_stacks                                       &
     &   (add_nod_comm%num_neib, add_nod_comm%num_import, izero,        &
     &    add_nod_comm%istack_import, add_nod_comm%ntot_import)
      call alloc_import_item(add_nod_comm)
!
      call alloc_node_data_sleeve_ext(add_nod_comm%ntot_import,         &
     &                                trim_import_xx)
      allocate(inod_lc_new_import_trim(add_nod_comm%ntot_import))
!
      call set_import_item_for_extend                                   &
     &   (org_node, expand_nod_comm, ext_nod_trim,                      &
     &    add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%istack_import, add_nod_comm%ntot_import,         &
     &    inod_lc_new_import_trim, add_nod_comm%item_import)
      call trim_imported_expand_node(add_nod_comm, ext_nod_trim,        &
     &                               exp_import_xx, trim_import_xx)
!
!      call s_check_slv_ext_local_node_id(org_node, nod_comm, mark_nod, &
!     &    expand_nod_comm, add_nod_comm, sort_nod_import,              &
!     &    ext_nod_trim, exp_import_xx, trim_import_xx,                 &
!     &    idx_nod_extend_to_trimmed, inod_lc_new_import_trim)
!
      call alloc_export_num(add_nod_comm)
      call num_items_send_recv                                          &
     &   (add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%num_import, SR_sig1, add_nod_comm%num_export,    &
     &    add_nod_comm%istack_export, add_nod_comm%ntot_export)
      call alloc_export_item(add_nod_comm)
!
!
      call calypso_mpi_barrier
!
      dist_4_comm%ntot = add_nod_comm%ntot_export
      allocate(dist_4_comm%distance_in_export(dist_4_comm%ntot))
!
      call comm_items_send_recv                                         &
     &   (add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%istack_import, add_nod_comm%istack_export,       &
     &    inod_lc_new_import_trim, SR_sig1, add_nod_comm%item_export)
      call real_items_send_recv                                         &
     &   (add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%istack_import, add_nod_comm%istack_export,       &
     &    trim_import_xx%distance, SR_sig1,                             &
     &    dist_4_comm%distance_in_export)
!
      call s_append_extended_node(org_node, inod_dbl, add_nod_comm,     &
     &    trim_import_xx, inod_lc_new_import_trim, new_node, dbl_id2)
!
!
      call check_appended_node_data                                     &
     &   (org_node, expand_nod_comm, add_nod_comm, exp_import_xx,       &
     &    ext_nod_trim, trim_import_xx, dbl_id2,                        &
     &    idx_nod_extend_to_trimmed, inod_lc_new_import_trim)
      deallocate(inod_lc_new_import_trim)
!
!
      allocate(inod_added_import(expand_nod_comm%ntot_import))
      inod_added_import(1:expand_nod_comm%ntot_import) = 0
!
      call find_original_import_address                                 &
     &   (org_node, expand_nod_comm, add_nod_comm, ext_nod_trim,        &
     &    idx_nod_extend_to_trimmed, inod_added_import)
      call dealloc_stack_to_trim_extend(ext_nod_trim)
      call dealloc_idx_trimed_to_sorted(ext_nod_trim)
      deallocate(idx_nod_extend_to_trimmed)
!
      if(i_debug .gt. 0) then
        call check_expanded_import_node                                 &
     &     (dbl_id2, expand_nod_comm, exp_import_xx, inod_added_import)
        call check_expanded_import_ele                                  &
     &    (org_ele, expand_ele_comm, exp_import_ie)
      end if
!
      call renumber_extended_ele_import(my_rank, org_ele, nod_comm,     &
     &    expand_nod_comm, expand_ele_comm, inod_added_import,          &
     &    exp_import_ie%ie_comm)
      deallocate(inod_added_import)
!
      if(i_debug .gt. 0) then
        call check_expanded_import_ele                                  &
     &    (org_ele, expand_ele_comm, exp_import_ie)
      end if
!
      call s_append_communication_table                                 &
     &   (nod_comm, add_nod_comm, new_nod_comm)
      call check_new_node_and_comm(new_nod_comm, new_node, dbl_id2)
!
!
      add_ele_comm%num_neib = add_nod_comm%num_neib
      call alloc_comm_table_num(add_ele_comm)
!
!$omp parallel workshare
      add_ele_comm%id_neib(1:add_ele_comm%num_neib)                     &
     &             = add_nod_comm%id_neib(1:add_ele_comm%num_neib)
!$omp end parallel workshare
!
      call alloc_sort_data_sleeve_ext                                   &
     &   (nprocs, expand_ele_comm%ntot_import, sort_ele_import)
!
      call sort_import_by_pe_and_local_id(nprocs, nod_comm,             &
     &    expand_ele_comm, exp_import_ie%irank_comm, sort_ele_import)
!
!
      call trim_overlapped_sleeve_ext                                   &
     &   (expand_ele_comm%ntot_import, exp_import_ie%irank_comm,        &
     &    sort_ele_import, ext_ele_trim)
      if(i_debug .gt. 0) then
        call check_overlapped_sleeve_ext                                &
     &    (nod_comm, add_ele_comm, sort_ele_import, ext_ele_trim)
      end if
!
      call dealloc_sort_data_sleeve_ext(sort_nod_import)
!
      call alloc_import_num(add_ele_comm)
      call count_import_item_for_extend                                 &
     &   (nprocs, ext_ele_trim%istack_trimmed_pe,                       &
     &    add_ele_comm%num_neib, add_ele_comm%id_neib,                  &
     &    add_ele_comm%num_import)
      call s_cal_total_and_stacks                                       &
     &   (add_ele_comm%num_neib, add_ele_comm%num_import, izero,        &
     &    add_ele_comm%istack_import, add_ele_comm%ntot_import)
      call alloc_import_item(add_ele_comm)
!
      allocate(iele_lc_import_trim(add_ele_comm%ntot_import))
      call alloc_ele_data_sleeve_ext                                    &
     &   (add_ele_comm%ntot_import, org_ele%nnod_4_ele,                 &
     &    trim_import_ie)
!
      call set_trimmed_import_items                                     &
     &   (org_ele, expand_ele_comm, add_ele_comm,                       &
     &    ext_ele_trim, exp_import_ie,                                  &
     &    iele_lc_import_trim, trim_import_ie)
      call dealloc_ele_data_sleeve_ext(exp_import_ie)
      call dealloc_stack_to_trim_extend(ext_ele_trim)
      deallocate(ext_ele_trim%idx_trimmed_to_sorted)
!
      call check_trim_import_ele_connect(org_ele, add_ele_comm,         &
     &                                   trim_import_ie%ie_comm)
!
!
      call alloc_export_num(add_ele_comm)
      call num_items_send_recv                                          &
     &   (add_ele_comm%num_neib, add_ele_comm%id_neib,                  &
     &    add_ele_comm%num_import, SR_sig1, add_ele_comm%num_export,    &
     &    add_ele_comm%istack_export, add_ele_comm%ntot_export)
      call alloc_export_item(add_ele_comm)
!
      call comm_items_send_recv                                         &
     &   (add_ele_comm%num_neib, add_ele_comm%id_neib,                  &
     &    add_ele_comm%istack_import, add_ele_comm%istack_export,       &
     &    iele_lc_import_trim, SR_sig1,                                 &
     &    add_ele_comm%item_export)
      deallocate(iele_lc_import_trim)
!
      call s_append_communication_table                                 &
     &   (ele_comm, add_ele_comm, new_ele_comm)
      call s_append_extended_element(org_ele, add_ele_comm,             &
     &    trim_import_ie, new_ele)
!
      call check_returned_extend_element                                &
     &   (iele_dbl, add_ele_comm, trim_import_ie)
!
      end subroutine extend_node_comm_table2
!
!  ---------------------------------------------------------------------
!
      end module analyzer_sleeve_extend2
