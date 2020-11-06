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
      use external_group_4_new_part
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
      use t_fem_gauss_int_coefs
      use t_next_node_ele_4_node
!
      use mpi_load_mesh_data
      use mesh_file_IO
      use copy_mesh_structures
      use append_group_data
!
      use calypso_mpi_int
      use calypso_mpi_real
      use const_jacobians_3d
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
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
      type(mesh_test_files_param) ::  T_meshes
!
      type(next_nod_ele_table) :: next_tbl_T
!
      type(jacobians_type) :: jacobians_T
      type(shape_finctions_at_points) :: spfs_T
!
      type(group_data) :: part_grp
      type(group_data) :: ext_grp
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
!       Re-partitioning for external node
      call const_external_grp_4_new_part                                &
     &   (fem_T%mesh%node, next_tbl_T%neib_nod, T_meshes,               &
     &    part_grp, ext_grp)
!
!       Append group data
      call s_append_group_data(part_grp, fem_T%group%nod_grp)
!
      call const_comm_tbls_for_new_part(fem_T%mesh%nod_comm,            &
     &    fem_T%mesh%node, part_grp, ext_grp,                           &
     &    new_fem%mesh%nod_comm, new_fem%mesh%node)
!
      new_fem%mesh%ele%numele =         fem_T%mesh%ele%numele
      new_fem%mesh%ele%first_ele_type = fem_T%mesh%ele%first_ele_type
      new_fem%mesh%ele%nnod_4_ele                                       &
     &     = set_nnod_4_ele_by_eletype(new_fem%mesh%ele%first_ele_type)
!
      new_fem%group%nod_grp%num_grp = 0
      call alloc_group_num(new_fem%group%nod_grp)
      call alloc_group_item(new_fem%group%nod_grp)
      new_fem%group%ele_grp%num_grp = 0
      call alloc_group_num(new_fem%group%ele_grp)
      call alloc_group_item(new_fem%group%ele_grp)
      new_fem%group%surf_grp%num_grp = 0
      call alloc_sf_group_num(new_fem%group%surf_grp)
      call alloc_sf_group_item(new_fem%group%surf_grp)
!
!       Output appended mesh
      file_name = set_mesh_file_name                                    &
     &          (T_meshes%new_mesh_file_IO%file_prefix,                 &
     &           T_meshes%new_mesh_file_IO%iflag_format, my_rank)
!      call write_mesh_file                                             &
!     &   (my_rank, file_name, new_fem%mesh, new_fem%group)
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
     &         (nod_comm, node, part_grp, ext_grp, new_comm, new_node)
!
      use t_comm_table
      use t_geometry_data
      use t_calypso_comm_table
      use set_comm_tbl_to_new_part
      use const_comm_tbl_to_new_mesh
      use calypso_mpi_int
      use calypso_SR_type
      use select_copy_from_recv
      use nod_phys_send_recv
      use reverse_SR_int
      use quicksort
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(group_data), intent(in) :: part_grp, ext_grp
!
      type(calypso_comm_table) :: tmp_tbl
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
!
      type(calypso_comm_table) :: part_tbl
      type(calypso_comm_table) :: ext_tbl
!
      integer(kind = kint), allocatable :: num_send_tmp(:)
      integer(kind = kint), allocatable :: num_recv_tmp(:)
!
      integer(kind = kint), allocatable :: inod_recv(:)
      integer(kind = kint), allocatable :: idomain_recv(:)
      integer(kind = kint), allocatable :: idomain_new(:)
      integer(kind = kint), allocatable :: inod_new(:)
!
      integer(kind = kint) :: numnod, internal_node
      integer(kind = kint), allocatable :: idx_sort(:)
      integer(kind = kint), allocatable :: inod_sort(:)
      integer(kind = kint), allocatable :: irank_sort(:)
!
      integer(kind = kint), allocatable :: inod_import(:)
!
      integer(kind = kint) :: i, ist, inum, j, jst, ip
      integer(kind = kint) :: iflag_self, nrank_export
      integer(kind = kint) :: nrank_import
!
!
      allocate(num_send_tmp(part_grp%num_grp))
      allocate(num_recv_tmp(part_grp%num_grp))
!
      call gather_num_trans_for_repart                                  &
     &   (part_grp, num_send_tmp, num_recv_tmp)
      call const_comm_tbl_to_new_part                                   &
     &   (part_grp, num_send_tmp, num_recv_tmp, part_tbl)
      call send_back_istack_import_repart                               &
     &   (part_grp, part_tbl, num_recv_tmp, num_send_tmp)
!
!
      call gather_num_trans_for_repart                                  &
     &   (ext_grp, num_send_tmp, num_recv_tmp)
      call const_comm_tbl_to_new_part                                   &
     &   (ext_grp, num_send_tmp, num_recv_tmp, ext_tbl)
      call send_back_ext_istack_import                                  &
     &   (part_grp, part_tbl, ext_tbl, num_recv_tmp, num_send_tmp)
!
!
      allocate(idomain_new(node%numnod))
      allocate(inod_new(node%numnod))
!$omp parallel workshare
      idomain_new(1:node%numnod) = -1
      inod_new(1:node%numnod) =     0
!$omp end parallel workshare
!
      call set_new_subdomain_id(nod_comm, node, part_grp,               &
     &    num_send_tmp, inod_new, idomain_new)
      deallocate(num_send_tmp, num_recv_tmp)
!
      internal_node =                part_tbl%ntot_import
      numnod = ext_tbl%ntot_import + part_tbl%ntot_import
      allocate(inod_recv(numnod))
      allocate(idomain_recv(numnod))
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, internal_node, idomain_new(1), idomain_recv(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, internal_node, inod_new(1), inod_recv(1))
!
      call calypso_SR_type_int(iflag_import_item, ext_tbl,              &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    idomain_new(1), idomain_recv(internal_node+1))
      call calypso_SR_type_int(iflag_import_item, ext_tbl,              &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    inod_new(1), inod_recv(internal_node+1))
!
      allocate(num_send_tmp(nprocs))
      allocate(num_recv_tmp(nprocs))
      allocate(idx_sort(ext_tbl%ntot_import))
      allocate(inod_sort(ext_tbl%ntot_import))
      allocate(irank_sort(ext_tbl%ntot_import))
!$omp parallel workshare
      num_send_tmp(1:nprocs) = 0
      num_recv_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
      do i = 1, ext_tbl%ntot_import
        ip = mod(idomain_recv(i+internal_node)+nprocs-my_rank,nprocs)
        irank_sort(i) =    ip
        num_recv_tmp(ip) = num_recv_tmp(ip+1) + 1
      end do
!
!$omp parallel do
      do i = 1, ext_tbl%ntot_import
        inod_sort(i) = inod_recv(i+internal_node)
        idx_sort(i) =  ext_tbl%item_import(i)
      end do
!$omp end parallel do
!
      call quicksort_w_index                                            &
     &   (ext_tbl%ntot_import, irank_sort(1),                           &
     &    ione, ext_tbl%ntot_import, idx_sort(1))
!
!$omp parallel do private(i,j)
      do i = 1, ext_tbl%ntot_import
        j = idx_sort(i)
        inod_recv(i+internal_node) =    inod_sort(j)
      end do
!$omp end parallel do
!
      jst = 0
      do ip = 1, nprocs
        if(num_recv_tmp(ip) .gt. 0) then
          call quicksort_w_index                                        &
     &       (num_recv_tmp(ip), inod_recv(internal_node+jst+1),         &
     &        ione, num_recv_tmp(ip), idx_sort(jst+1))
          jst = jst + num_recv_tmp(ip)
        end if
      end do
!
!$omp parallel do private(i,j)
      do i = 1, ext_tbl%ntot_import
        j = idx_sort(i)
        ext_tbl%item_import(j) = i
        ext_tbl%irev_import(i) = j
      end do
!$omp end parallel do
      deallocate(num_send_tmp, num_recv_tmp)
!
!
      new_node%internal_node =                part_tbl%ntot_import
      new_node%numnod = ext_tbl%ntot_import + part_tbl%ntot_import
!
      write(*,*) my_rank, 'new_nomond', new_node%internal_node,         &
     &           new_node%numnod, ext_tbl%ntot_import
      call alloc_node_geometry_base(new_node)
!
      call calypso_SR_type_int8(iflag_import_item, part_tbl,            &
     &    node%numnod, new_node%internal_node,                          &
     &    node%inod_global(1), new_node%inod_global(1))
      call calypso_SR_type_1(iflag_import_item, part_tbl,               &
     &    node%numnod, new_node%internal_node,                          &
     &    node%xx(1,1), new_node%xx(1,1))
      call calypso_SR_type_1(iflag_import_item, part_tbl,               &
     &    node%numnod, new_node%internal_node,                          &
     &    node%xx(1,2), new_node%xx(1,2))
      call calypso_SR_type_1(iflag_import_item, part_tbl,               &
     &    node%numnod, new_node%internal_node,                          &
     &    node%xx(1,3), new_node%xx(1,3))
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    idomain_new(1), idomain_recv(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    inod_new(1), inod_recv(1))
!
!
      ist = new_node%internal_node
      call calypso_SR_type_int8(iflag_import_item, ext_tbl,             &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    node%inod_global(1), new_node%inod_global(ist+1))
!
      call calypso_SR_type_int8(iflag_import_item, ext_tbl,             &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    node%inod_global(1), new_node%inod_global(ist+1))
      call calypso_SR_type_1(iflag_import_item, ext_tbl,                &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    node%xx(1,1), new_node%xx(ist+1,1))
      call calypso_SR_type_1(iflag_import_item, ext_tbl,                &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    node%xx(1,2), new_node%xx(ist+1,2))
      call calypso_SR_type_1(iflag_import_item, ext_tbl,                &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    node%xx(1,3), new_node%xx(ist+1,3))
!
      call calypso_SR_type_int(iflag_import_item, ext_tbl,              &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    idomain_new(1), idomain_recv(ist+1))
      call calypso_SR_type_int(iflag_import_item, ext_tbl,              &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    inod_new(1), inod_recv(ist+1))
!
!
      allocate(num_send_tmp(nprocs))
      allocate(num_recv_tmp(nprocs))

!$omp parallel workshare
      num_send_tmp(1:nprocs) = 0
      num_recv_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
      ist = new_node%internal_node
      do i = 1, ext_tbl%ntot_import
        ip = idomain_recv(ist+i) + 1
        num_recv_tmp(ip) = num_recv_tmp(ip) + 1
      end do
!
      call calypso_mpi_alltoall_one_int                                 &
     &   (num_recv_tmp(1), num_send_tmp(1))
!
      tmp_tbl%iflag_self_copy = 0
      call count_num_export_for_repart(my_rank, nprocs, num_send_tmp,   &
     &    tmp_tbl%iflag_self_copy, tmp_tbl%nrank_export)
      call count_num_import_for_repart                                  &
     &   (nprocs, num_recv_tmp, tmp_tbl%nrank_import)
!
      if(tmp_tbl%nrank_export .ne. tmp_tbl%nrank_import) then
        write(*,*) my_rank, 'num. of communication processes is wrong', &
     &            tmp_tbl%nrank_export, tmp_tbl%nrank_import
      end if
!
      call alloc_calypso_export_num(tmp_tbl)
      call alloc_calypso_import_num(tmp_tbl)
!
      call set_istack_export_for_repart                                 &
     &   (my_rank, int(part_grp%num_grp), num_send_tmp,                 &
     &    tmp_tbl%nrank_export, tmp_tbl%ntot_export,                    &
     &    tmp_tbl%irank_export, tmp_tbl%num_export,                     &
     &    tmp_tbl%istack_export)
      call set_istack_import_for_repart(my_rank, nprocs, num_recv_tmp,  &
     &    tmp_tbl%nrank_import, tmp_tbl%ntot_import,                    &
     &    tmp_tbl%irank_import, tmp_tbl%num_import,                     &
     &    tmp_tbl%istack_import)
!
      do i = 1, tmp_tbl%nrank_export
        if(tmp_tbl%irank_export(i) .ne. tmp_tbl%irank_import(i)) then
          write(*,*) my_rank, 'processes to communication is wrong at', &
     &            i, tmp_tbl%irank_export(i), tmp_tbl%irank_import(i)
        end if
      end do
!
      new_comm%num_neib = tmp_tbl%nrank_export
      call alloc_comm_table_num(new_comm)
!
      new_comm%istack_export(0) = 0
      new_comm%istack_import(0) = 0
!%omp parallel do
      do i = 1, new_comm%num_neib
        new_comm%id_neib(i) =       tmp_tbl%irank_export(i)
        new_comm%num_export(i) =    tmp_tbl%num_export(i)
        new_comm%istack_export(i) = tmp_tbl%istack_export(i)
        new_comm%num_import(i) =    tmp_tbl%num_import(i)
        new_comm%istack_import(i) = tmp_tbl%istack_import(i)
      end do
!%omp end parallel do
      new_comm%ntot_export = new_comm%istack_export(new_comm%num_neib)
      new_comm%ntot_import = new_comm%istack_import(new_comm%num_neib)
!
      allocate(inod_import(new_comm%ntot_import))
      call alloc_comm_table_item(new_comm)
!
!$omp parallel do
      do i = 1, new_comm%ntot_import
        new_comm%item_import(i) = i + new_node%internal_node
        inod_import(i) = inod_recv(i+new_node%internal_node)
      end do
!$omp end parallel do
!
      call reverse_send_recv_int(new_comm%num_neib, new_comm%id_neib,   &
     &    new_comm%istack_import, new_comm%istack_export,               &
     &    inod_import, SR_sig1, new_comm%item_export)
!
      deallocate(idomain_new,  inod_new)
      deallocate(idomain_recv, inod_recv)
      deallocate(num_send_tmp, num_recv_tmp)
!
      end subroutine const_comm_tbls_for_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_new_subdomain_id(nod_comm, node, part_grp,         &
     &          num_send_tmp, inod_new, idomain_new)
!
      use t_comm_table
      use t_geometry_data
      use t_calypso_comm_table
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: part_grp
      integer(kind = kint), intent(in)                                  &
     &                     :: num_send_tmp(part_grp%num_grp)
!
      integer(kind = kint), intent(inout) :: idomain_new(node%numnod)
      integer(kind = kint), intent(inout) :: inod_new(node%numnod)
!
      integer(kind = kint) :: inum, inod, ist, num, jst
      integer(kind = kint) :: igrp
!
!
!$omp parallel workshare
      idomain_new(1:node%numnod) = -1
      inod_new(1:node%numnod) =     0
!$omp end parallel workshare
!
      do igrp = 1, part_grp%num_grp
        jst = num_send_tmp(igrp)
        ist = part_grp%istack_grp(igrp-1)
        num = part_grp%istack_grp(igrp  ) - ist
!$omp parallel do private(inum,inod)
        do inum = 1, num
          if(jst .lt. 0) write(*,*)                                     &
     &            'Wring in recieved stack', igrp, jst
          inod = part_grp%item_grp(ist+inum)
          idomain_new(inod) = igrp - 1
          inod_new(inod) =    jst + inum
        end do
!$omp end parallel do
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, idomain_new)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, inod_new)
!
      end subroutine set_new_subdomain_id
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_repart_test
