!>@file   analyzer_volume_repart_test.f90
!!@brief  module analyzer_volume_repart_test
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_volume_repartition
!!      subroutine analyze_volume_grouping
!!@endverbatim
!
      module analyzer_volume_repart_test
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use m_work_time
!
      implicit none
!
      type(mesh_data), save :: fem_T
      type(mesh_data), save :: new_fem
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_volume_repartition
!
      use m_array_for_send_recv
      use m_default_file_prefix
      use t_ctl_data_volume_grouping
      use t_control_param_vol_grping
      use t_1d_repartitioning_work
      use t_repartition_by_volume
      use set_istack_4_domain_block
      use repart_in_xyz_by_volume
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_comm_table
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
      use t_fem_gauss_int_coefs
      use t_next_node_ele_4_node
      use t_calypso_comm_table
      use t_repart_double_numberings
!
      use mpi_load_mesh_data
      use mesh_file_IO
      use copy_mesh_structures
      use append_group_data
!
      use calypso_mpi_int
      use calypso_mpi_real
      use const_jacobians_3d
      use const_element_comm_tables
      use parallel_FEM_mesh_init
      use output_test_mesh
      use set_table_4_RHS_assemble
      use nod_phys_send_recv
      use solver_SR_type
      use transfer_to_long_integers
!
      use set_parallel_file_name
      use int_volume_of_single_domain
      use set_nnod_4_ele_by_type
!
      use set_mesh_file_names
      use mesh_file_IO
!
      use calypso_SR_type
      use select_copy_from_recv
      use nod_phys_send_recv
!
      use redistribute_groups
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
      type(mesh_test_files_param) ::  T_meshes
!
      type(communication_table) :: ele_comm
      type(next_nod_ele_table) :: next_tbl_T
!
      type(jacobians_type) :: jacobians_T
      type(shape_finctions_at_points) :: spfs_T
!
      type(group_data) :: part_grp
!
      type(calypso_comm_table) :: part_tbl, ext_tbl
      type(calypso_comm_table) :: ele_tbl
!
      type(double_numbering_data) :: new_ids_on_org
!
!      integer(kind = kint_gl), allocatable :: inod_gl_test(:)
!      integer(kind = kint) :: inod, iele, k1, inum, jnod
!
      character(len=kchara) :: file_name
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
!      call elapsed_label_4_ele_comm_tbl
!
!     --------------------- 
!
      if (my_rank.eq.0) then
        write(*,*) 'Test mesh commnucations'
        write(*,*) 'Input file: mesh data'
      end if
!
!     ----- read control data
!
      call read_control_new_partition(part_tctl1)
!
      call s_set_ctl_params_4_test_mesh(part_tctl1, T_meshes)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(T_meshes%mesh_file_IO, nprocs, fem_T)
!
!  -------------------------------
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group)
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbl_only'
      call const_element_comm_tbl_only(fem_T%mesh, ele_comm)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_single_vol'
      allocate(jacobians_T%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (fem_T%mesh%ele%nnod_4_ele, jacobians_T%g_FEM)
      call const_jacobian_and_single_vol                                &
     &   (fem_T%mesh, fem_T%group, spfs_T, jacobians_T)
!
      call init_send_recv(fem_T%mesh%nod_comm)
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
!
!  -------------------------------
!
      if(iflag_debug .gt. 0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl_T%neib_ele, next_tbl_T%neib_nod)
!
!       Re-partitioning
      call s_repartition_by_volume(fem_T%mesh, T_meshes, part_grp)
!
!
      call alloc_double_numbering_data                                  &
     &   (fem_T%mesh%node%numnod, new_ids_on_org)
      call const_comm_tbls_for_new_part                                 &
     &   (fem_T%mesh, next_tbl_T%neib_nod, T_meshes, part_grp,          &
     &    new_ids_on_org, new_fem%mesh%nod_comm,                        &
     &    new_fem%mesh%node, part_tbl, ext_tbl)
!
!
      call const_new_element_connect(fem_T%mesh, ele_comm, part_tbl,    &
     &    new_ids_on_org, ele_tbl, new_fem%mesh)
      call dealloc_double_numbering_data(new_ids_on_org)
!
      call s_redistribute_groups(fem_T%mesh, fem_T%group, ele_comm,     &
     &    new_fem%mesh, part_tbl, ele_tbl, new_fem%group)
!
!      allocate(inod_gl_test(new_fem%mesh%node%numnod))
!      inod_gl_test(1:new_fem%mesh%node%numnod) = 0
!
!      call calypso_SR_type_int8(iflag_import_item, part_tbl,           &
!     &    fem_T%mesh%node%numnod, new_fem%mesh%node%internal_node,     &
!     &    fem_T%mesh%node%inod_global(1), inod_gl_test(1))
!      call SOLVER_SEND_RECV_int8_type                                  &
!     &  (new_fem%mesh%node%numnod, new_fem%mesh%nod_comm, inod_gl_test)
!
!      write(*,*) my_rank, 'Check node transfer'
!      do inod = new_fem%mesh%node%internal_node+1,                     &
!     &         new_fem%mesh%node%numnod
!        if(inod_gl_test(inod)                                          &
!     &       .ne. new_fem%mesh%node%inod_global(inod)) then
!          write(*,*) my_rank, 'Wrong transfer at', inod,    &
!     &        new_fem%mesh%node%inod_global(inod), inod_gl_test(inod)
!        end if
!      end do
!
!       Output appended mesh
      file_name = set_mesh_file_name                                    &
     &          (T_meshes%new_mesh_file_IO%file_prefix,                 &
     &           T_meshes%new_mesh_file_IO%iflag_format, my_rank)
      call write_mesh_file                                             &
     &   (my_rank, file_name, new_fem%mesh, new_fem%group)
!      call dealloc_node_geometry_base(new_fem%mesh%node)
!
      end subroutine initialize_volume_repartition
!
! ----------------------------------------------------------------------
!
      subroutine analyze_volume_repartition
!
!
!      call output_elapsed_times
      call calypso_MPI_barrier
!
      if(iflag_debug.gt.0) write(*,*) 'exit analyze_volume_repartition'
!
      end subroutine analyze_volume_repartition
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_comm_tbls_for_new_part                           &
     &         (mesh, neib_nod, part_param, part_grp,                   &
     &          new_ids_on_org, new_comm, new_node, part_tbl, ext_tbl)
!
      use t_mesh_data
      use t_calypso_comm_table
      use t_control_param_vol_grping
      use t_sorting_for_repartition
      use t_repart_double_numberings
!
      use set_comm_tbl_to_new_part
      use const_comm_tbl_to_new_mesh
      use calypso_mpi_int
      use calypso_SR_type
      use select_copy_from_recv
      use nod_phys_send_recv
      use reverse_SR_int
      use solver_SR_type
      use external_group_4_new_part
      use ext_of_int_grp_4_new_part
!
      use const_repart_mesh_data
      use const_repart_comm_tbl
!
      type(mesh_geometry), intent(in) :: mesh
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(mesh_test_files_param), intent(in) :: part_param
      type(group_data), intent(in) :: part_grp
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
      type(calypso_comm_table), intent(inout) :: part_tbl
      type(calypso_comm_table), intent(inout) :: ext_tbl
!
      type(double_numbering_data), intent(inout) :: new_ids_on_org
!
      type(group_data) :: ext_int_grp
      type(group_data) :: ext_grp
      type(sorting_data_for_repartition) :: sort_nod
!
      integer(kind = kint) :: numnod, internal_node
      type(double_numbering_data) :: recieved_new_nod_ids
!
!
      call const_int_comm_tbl_to_new_part(part_grp, part_tbl)
!    Set new_ids_on_org in internal node
      call node_dbl_numbering_to_repart                                 &
     &   (mesh%nod_comm, mesh%node, part_tbl, new_ids_on_org)
!
      call const_external_grp_4_new_part(new_ids_on_org%irank,          &
     &    mesh%node, part_param, part_grp, ext_grp)
!       Re-partitioning for external node
      call const_ext_of_int_grp_new_part(mesh%node, neib_nod,           &
     &    part_param, part_grp, ext_grp, ext_int_grp)
      call const_ext_comm_tbl_to_new_part                               &
     &   (ext_int_grp, part_tbl, ext_tbl)
      call dealloc_group(ext_int_grp)
      call dealloc_group(ext_grp)
!
!      Set local recieved_new_nod_ids in internal node
      internal_node =                part_tbl%ntot_import
      numnod = ext_tbl%ntot_import + part_tbl%ntot_import
!
      call alloc_double_numbering_data(numnod, recieved_new_nod_ids)
      call ext_node_dbl_numbering_by_SR(mesh%node, ext_tbl,             &
     &    new_ids_on_org, internal_node, recieved_new_nod_ids)
!
      call alloc_sorting_data(ext_tbl%ntot_import, sort_nod)
      call sort_node_by_domain_and_index(numnod, internal_node,         &
     &    recieved_new_nod_ids%irank, recieved_new_nod_ids%index, ext_tbl, sort_nod)
      call dealloc_double_numbering_data(recieved_new_nod_ids)
!
      call const_repartitioned_comm_tbl                                 &
     &   (internal_node, sort_nod%num_recv, sort_nod%nrecv_trim,        &
     &    ext_tbl%ntot_import, sort_nod%irank_sorted,                   &
     &    sort_nod%id_sorted, sort_nod%iflag_dup, new_comm)
!
!      call check_num_of_neighbourings                                  &
!     &   (new_comm, ext_tbl, sort_nod%nrecv_trim)
!      call check_new_node_comm_table(my_rank, new_comm)
      call dealloc_sorting_data(sort_nod)
!
      call set_repart_node_position                                     &
     &   (mesh%node, new_comm, new_node, part_tbl)
      call check_repart_node_transfer                                   &
     &   (mesh%nod_comm, mesh%node, new_comm, new_node,                 &
     &    part_tbl, new_ids_on_org)
!
      end subroutine const_comm_tbls_for_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_new_element_connect(mesh, ele_comm, part_tbl,    &
     &          new_ids_on_org, ele_tbl, new_mesh)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_calypso_comm_table
      use t_sorting_for_repartition
      use t_repart_double_numberings
!
      use ele_trans_tbl_4_repart
!
      type(mesh_geometry), intent(in) :: mesh
      type(communication_table), intent(in) :: ele_comm
      type(calypso_comm_table), intent(in) :: part_tbl
      type(double_numbering_data), intent(in) :: new_ids_on_org
!
      type(calypso_comm_table), intent(inout) :: ele_tbl
      type(mesh_geometry), intent(inout) :: new_mesh
!
      type(double_numbering_data) :: element_ids
!
      integer(kind = kint) :: new_numele
!
!
      call alloc_double_numbering_data(mesh%ele%numele, element_ids)
      call double_numbering_4_element(mesh%ele, ele_comm, element_ids)
!
      call const_ele_trans_tbl_for_repart                               &
     &   (mesh%node, mesh%ele, part_tbl, new_ids_on_org%irank, ele_tbl)
!      call check_element_transfer_tbl(mesh%ele, ele_tbl)
! 
      call trim_overlapped_ele_by_repart                                &
     &   (mesh, new_ids_on_org%irank, new_ids_on_org%index,             &
     &    element_ids%irank, element_ids%index, ele_tbl, new_numele)
!
      call const_reparition_ele_connect                                 &
     &   (mesh%node, mesh%ele, ele_tbl, new_ids_on_org,                 &
     &    element_ids, new_numele, new_mesh)
      call dealloc_double_numbering_data(element_ids)
!
      end subroutine const_new_element_connect
!
! ----------------------------------------------------------------------
!
      subroutine const_reparition_ele_connect                           &
     &         (node, ele, ele_tbl, new_ids_on_org,                     &
     &          element_ids, new_numele, new_mesh)
!
      use t_geometry_data
      use t_calypso_comm_table
      use t_repart_double_numberings
!
      use search_ext_node_repartition
      use const_repart_mesh_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(double_numbering_data), intent(in) :: new_ids_on_org
      type(double_numbering_data), intent(in) :: element_ids
!
      integer(kind = kint), intent(in) :: new_numele
!
      type(mesh_geometry), intent(inout) :: new_mesh
!
      integer(kind = kint), allocatable :: ie_newnod(:,:)
      integer(kind = kint), allocatable :: ie_newdomain(:,:)
!
!
      allocate(ie_newnod(ele%numele,ele%nnod_4_ele))
      allocate(ie_newdomain(ele%numele,ele%nnod_4_ele))
!$omp parallel workshare
      ie_newnod(1:ele%numele,1:ele%nnod_4_ele) =    0
      ie_newdomain(1:ele%numele,1:ele%nnod_4_ele) = 0
!$omp end parallel workshare
!
      call set_repart_element_connect(new_numele, node, ele,            &
     &    ele_tbl, new_ids_on_org%irank, new_ids_on_org%index,          &
     &    ie_newdomain, ie_newnod, new_mesh%ele)
!
      call s_search_ext_node_repartition(ele, ele_tbl,                  &
     &    element_ids%index, element_ids%irank, ie_newdomain,           &
     &    new_mesh%nod_comm, new_mesh%node, new_mesh%ele)
      deallocate(ie_newnod, ie_newdomain)
!
      end subroutine const_reparition_ele_connect
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_num_of_neighbourings                           &
     &         (new_comm, ext_tbl, num_recv_tmp2)
!
      use t_comm_table
      use t_calypso_comm_table
      use calypso_mpi_int
!
      type(communication_table), intent(in) :: new_comm
      type(calypso_comm_table), intent(in) :: ext_tbl
!
      integer(kind = kint), intent(in) :: num_recv_tmp2(nprocs)
!
      integer(kind = kint), allocatable :: num_send_3(:)
      integer(kind = kint), allocatable :: num_recv_3(:)
!
      integer(kind = kint) :: i, ip
!
!
      write(100+my_rank,*) my_rank, 'num_recv_tmp2(i)'
      do i = 1, new_comm%num_neib
        ip = new_comm%id_neib(i)
        write(100+my_rank,*) i, new_comm%num_import(i),                 &
     &                      num_recv_tmp2(ip+1)
      end do
!
      allocate(num_send_3(nprocs))
      allocate(num_recv_3(nprocs))
      num_recv_3(1:nprocs) = 0
      do i = 1, ext_tbl%nrank_import
        ip = ext_tbl%irank_import(i)
        num_recv_3(ip+1) = ext_tbl%istack_import(i)                     &
     &                    - ext_tbl%istack_import(i-1)
      end do
      call calypso_mpi_alltoall_one_int(num_recv_3, num_send_3)
!
      write(100+my_rank,*) my_rank, 'num_recv_3(i), num_send_3(i)'
      do i = 1, nprocs
        write(100+my_rank,*) i-1, num_recv_3(i), num_send_3(i)
        if(num_recv_3(i) .gt. 0 .and. num_send_3(i) .eq. 0) then
          write(*,*) 'something wrong', my_rank, i-1
        end if
        if(num_recv_3(i) .eq. 0 .and. num_send_3(i) .gt. 0) then
          write(*,*) 'something wrong', my_rank, i-1
        end if
      end do
      deallocate(num_recv_3, num_send_3)
!
      end subroutine check_num_of_neighbourings
!
! ----------------------------------------------------------------------
!
      subroutine check_new_node_comm_table(my_rank, new_comm)
!
      use t_comm_table
!
      integer, intent(in) :: my_rank
      type(communication_table), intent(in) :: new_comm
!
      integer(kind = kint) :: i, icou, ist, ied
!
!
      write(my_rank+100,*) 'i, new_comm',                               &
     &    new_comm%num_neib, new_comm%ntot_import
      do icou = 1, new_comm%num_neib
        ist = new_comm%istack_export(icou-1) + 1
        ied = new_comm%istack_export(icou)
        write(my_rank+100,*) 'i, new_comm%istack_export(icou)', &
     &      new_comm%id_neib(icou), new_comm%istack_export(icou)
        do i = ist, ied
          write(my_rank+100,*) i, new_comm%item_export(i)
        end do
      end do
!
      end subroutine check_new_node_comm_table
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_repart_test
