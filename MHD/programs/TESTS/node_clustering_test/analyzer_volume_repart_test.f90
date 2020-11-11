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
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
      use t_fem_gauss_int_coefs
      use t_next_node_ele_4_node
      use t_calypso_comm_table
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
      use calypso_SR_type
      use select_copy_from_recv
      use nod_phys_send_recv
      use quicksort
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
      type(group_data) :: ext_int_grp
      type(group_data) :: ext_ext_grp
!
      type(calypso_comm_table) :: part_tbl, ext_tbl
      type(calypso_comm_table) :: ele_tbl
!
      integer(kind = kint), allocatable :: idomain_new(:)
      integer(kind = kint), allocatable :: inod_new(:)
!
      integer(kind = kint_gl), allocatable :: inod_gl_test(:)
      integer(kind = kint) :: inod, iele, k1, inum, jnod
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
      allocate(idomain_new(fem_T%mesh%node%numnod))
      allocate(inod_new(fem_T%mesh%node%numnod))
!$omp parallel workshare
      idomain_new(1:fem_T%mesh%node%numnod) = -1
      inod_new(1:fem_T%mesh%node%numnod) =     0
!$omp end parallel workshare
!
!
!
      call const_comm_tbls_for_new_part(fem_T%mesh%nod_comm,            &
     &    fem_T%mesh%node, next_tbl_T%neib_nod, T_meshes, part_grp, ext_int_grp, ext_ext_grp,          &
     &    idomain_new, inod_new, new_fem%mesh%nod_comm,                 &
     &    new_fem%mesh%node, part_tbl, ext_tbl)
      return
!
      call const_new_element_connect(fem_T%mesh, next_tbl_T%neib_ele,   &
     &    part_tbl, ext_tbl, idomain_new, inod_new, ele_tbl, new_fem%mesh)
!
      allocate(inod_gl_test(new_fem%mesh%node%numnod))
      inod_gl_test(1:new_fem%mesh%node%numnod) = 0
!
      call calypso_SR_type_int8(iflag_import_item, part_tbl,            &
     &    fem_T%mesh%node%numnod, new_fem%mesh%node%internal_node,      &
     &    fem_T%mesh%node%inod_global(1), inod_gl_test(1))
      call SOLVER_SEND_RECV_int8_type                                   &
     &  (new_fem%mesh%node%numnod, new_fem%mesh%nod_comm, inod_gl_test)
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
     &         (nod_comm, node, neib_nod, part_param, part_grp, ext_int_grp, ext_ext_grp,     &
     &          idomain_new, inod_new, new_comm, new_node,              &
     &          part_tbl, ext_int_tbl)
!
      use t_comm_table
      use t_geometry_data
      use t_calypso_comm_table
      use t_control_param_vol_grping
!
      use set_comm_tbl_to_new_part
      use const_comm_tbl_to_new_mesh
      use calypso_mpi_int
      use calypso_SR_type
      use select_copy_from_recv
      use nod_phys_send_recv
      use reverse_SR_int
      use solver_SR_type
      use quicksort
      use external_group_4_new_part
      use ext_of_int_grp_4_new_part
      use ext_of_ext_grp_4_new_part
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(mesh_test_files_param), intent(in) :: part_param
      type(group_data), intent(in) :: part_grp
      type(group_data), intent(inout) :: ext_int_grp, ext_ext_grp
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
      type(calypso_comm_table), intent(inout) :: part_tbl
      type(calypso_comm_table), intent(inout) :: ext_int_tbl
!
      integer(kind = kint), intent(inout) :: idomain_new(node%numnod)
      integer(kind = kint), intent(inout) :: inod_new(node%numnod)
!
      type(group_data) :: ext_grp
      type(calypso_comm_table) :: tmp_tbl
      type(calypso_comm_table) :: ext_ext_tbl
      type(communication_table) :: new_comm0, new_comm_tmp
!
      integer(kind = kint), allocatable :: num_send_tmp(:)
      integer(kind = kint), allocatable :: num_recv_tmp(:)
!
      integer(kind = kint), allocatable :: idomain_recv(:)
      integer(kind = kint), allocatable :: inod_recv(:)
!
      integer(kind = kint), allocatable :: idomain_new2(:)
      integer(kind = kint), allocatable :: inod_new2(:)
      integer(kind = kint), allocatable :: idomain_recv2(:)
      integer(kind = kint), allocatable :: inod_recv2(:)
!
      integer(kind = kint), allocatable :: iflag_pe(:)
      integer(kind = kint) :: num_neib_int_ext
      integer(kind = kint), allocatable :: id_neib_int_ext(:)
      integer(kind = kint), allocatable :: istack_neib_int_ext(:)
!
      integer(kind = kint) :: num_neib_ext_ext
      integer(kind = kint), allocatable :: id_neib_ext_ext(:)
      integer(kind = kint), allocatable :: istack_neib_ext_ext(:)
      integer(kind = kint), allocatable :: ipe_missing_ext_ext(:)
!
      integer(kind = kint), allocatable :: inod_ext_ext(:)
      integer(kind = kint), allocatable :: irank_ext_ext(:)
      integer(kind = kint), allocatable :: jrank_ext_ext(:)
      integer(kind = kint), allocatable :: isort_ext_ext(:)
      integer(kind = kint), allocatable :: iflag_ext_ext(:)
      integer(kind = kint), allocatable :: inod_ext_ext2(:)
      integer(kind = kint), allocatable :: irank_ext_ext2(:)
!
      integer(kind = kint) :: numnod, internal_node
      integer(kind = kint), allocatable :: idx_sort(:)
      integer(kind = kint), allocatable :: inod_sort(:)
      integer(kind = kint), allocatable :: irank_sort(:)
!
      integer(kind = kint), allocatable :: inod_import(:)
      integer(kind = kint), allocatable :: internal_list_new(:)
      integer(kind = kint), allocatable :: internal_list_org(:)
!
      integer(kind = kint), allocatable :: inod_external(:)
      integer(kind = kint), allocatable :: irank_external(:)
      integer(kind = kint), allocatable :: inod_ext_tmp(:)
      integer(kind = kint), allocatable :: irank_ext_tmp(:)
!
      integer(kind = kint), allocatable :: ip_new_comm(:)
      integer(kind = kint), allocatable :: isort_new_comm(:)
!
      integer(kind = kint) :: i, ist, inum, j, jst, ip, inod, jp, num
      integer(kind = kint) :: icou, iflag, ied, jed, jnum, ntot
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
!      call send_back_istack_import_repart                              &
!     &   (part_grp, part_tbl, num_recv_tmp, num_send_tmp)
!
!    Set local (idomain_recv, inod_recv) in internal node
      allocate(inod_recv(part_tbl%ntot_import))
      allocate(idomain_recv(part_tbl%ntot_import))
!$omp parallel do
      do inod = 1, part_tbl%ntot_import
        inod_recv(inod) =    inod
        idomain_recv(inod) = my_rank
      end do
!$omp end parallel do
!
!
!    Send local (idomain_recv, inod_recv) into original domain
!       (inod_new, idomain_new)
      call calypso_rev_SR_type_int(part_tbl,                            &
     &    part_tbl%ntot_import, node%numnod, inod_recv, inod_new)
      call calypso_rev_SR_type_int(part_tbl,                            &
     &    part_tbl%ntot_import, node%numnod, idomain_recv, idomain_new)
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, idomain_new)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, inod_new)
      deallocate(inod_recv, idomain_recv)
!
!
      call const_external_grp_4_new_part(idomain_new, node,             &
     &    part_param, part_grp, ext_grp)
!
!       Re-partitioning for external node
      call const_ext_of_int_grp_new_part                                &
     &   (node, neib_nod, part_param, part_grp, ext_grp, ext_int_grp)
!
      return
!
      call gather_num_trans_for_repart                                  &
     &   (ext_int_grp, num_send_tmp, num_recv_tmp)
      call const_comm_tbl_to_new_part                                   &
     &   (ext_int_grp, num_send_tmp, num_recv_tmp, ext_int_tbl)
!      call send_back_ext_istack_import                                 &
!     &   (part_grp, part_tbl, ext_int_tbl, num_recv_tmp, num_send_tmp)
      deallocate(num_send_tmp, num_recv_tmp)
!
!
      internal_node =                    part_tbl%ntot_import
      numnod = ext_int_tbl%ntot_import + part_tbl%ntot_import
      allocate(internal_list_org(nprocs))
      allocate(internal_list_new(nprocs))
      call calypso_mpi_allgather_one_int                   &
     &   (node%internal_node, internal_list_org)
      call calypso_mpi_allgather_one_int                   &
     &   (internal_node, internal_list_new)
!
!
!    Set local (idomain_recv, inod_recv) in internal node
      allocate(inod_recv(numnod))
      allocate(idomain_recv(numnod))
!$omp parallel do
      do inod = 1, internal_node
        inod_recv(inod) =    inod
        idomain_recv(inod) = my_rank
      end do
!$omp end parallel do
!
!    Set local (idomain_recv, inod_recv) in external node
      call calypso_SR_type_int(iflag_import_item, ext_int_tbl,          &
     &    node%numnod, ext_int_tbl%ntot_import,                         &
     &    idomain_new(1), idomain_recv(internal_node+1))
      call calypso_SR_type_int(iflag_import_item, ext_int_tbl,          &
     &    node%numnod, ext_int_tbl%ntot_import,                         &
     &    inod_new(1), inod_recv(internal_node+1))
!
!
      allocate(num_recv_tmp(nprocs))
      allocate(idx_sort(ext_int_tbl%ntot_import))
      allocate(inod_sort(ext_int_tbl%ntot_import))
      allocate(irank_sort(ext_int_tbl%ntot_import))
!$omp parallel workshare
      num_recv_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
      do i = 1, ext_int_tbl%ntot_import
        ip = mod(idomain_recv(i+internal_node)+nprocs-my_rank,nprocs)
        irank_sort(i) =    ip
        num_recv_tmp(ip) = num_recv_tmp(ip+1) + 1
      end do
!
!$omp parallel do
      do i = 1, ext_int_tbl%ntot_import
        inod_sort(i) = inod_recv(i+internal_node)
        idx_sort(i) =  ext_int_tbl%item_import(i)
      end do
!$omp end parallel do
!
      call quicksort_w_index                                            &
     &   (ext_int_tbl%ntot_import, irank_sort(1),                       &
     &    ione, ext_int_tbl%ntot_import, idx_sort(1))
!
!$omp parallel do private(i,j)
      do i = 1, ext_int_tbl%ntot_import
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
      do i = 1, ext_int_tbl%ntot_import
        j = idx_sort(i)
        ext_int_tbl%item_import(j) = i
        ext_int_tbl%irev_import(i) = j
      end do
!$omp end parallel do
      deallocate(num_recv_tmp)
!
!    Set local (idomain_recv, inod_recv) in external node again
      call calypso_SR_type_int(iflag_import_item, ext_int_tbl,          &
     &    node%numnod, ext_int_tbl%ntot_import,                         &
     &    idomain_new(1), idomain_recv(internal_node+1))
      call calypso_SR_type_int(iflag_import_item, ext_int_tbl,          &
     &    node%numnod, ext_int_tbl%ntot_import,                         &
     &    inod_new(1), inod_recv(internal_node+1))
!
!
      if(ext_int_tbl%ntot_import .eq. 0) then
        num_neib_int_ext = 0
      else
        num_neib_int_ext = 1
        do i = 2, ext_int_tbl%ntot_import
          if(idomain_recv(i+internal_node)       &
     &      .ne. idomain_recv(i+internal_node-1))                    &
     &          num_neib_int_ext = num_neib_int_ext + 1
        end do
      end if
!
      allocate(id_neib_int_ext(num_neib_int_ext))
      allocate(istack_neib_int_ext(0:num_neib_int_ext))
!
      istack_neib_int_ext(0) = 0
      if(ext_int_tbl%ntot_import .gt. 0) then
        icou = 0
        do i = 2, ext_int_tbl%ntot_import
          if(idomain_recv(i+internal_node)   &
     &        .ne. idomain_recv(i+internal_node-1)) then
            icou = icou + 1
            id_neib_int_ext(icou) =     idomain_recv(i+internal_node-1)
            istack_neib_int_ext(icou) = i-1
          end if
        end do
        id_neib_int_ext(num_neib_int_ext)                               &
     &       = idomain_recv(ext_int_tbl%ntot_import+internal_node)
        istack_neib_int_ext(num_neib_int_ext) = ext_int_tbl%ntot_import
      end if
!
!
      allocate(num_send_tmp(part_grp%num_grp))
      allocate(num_recv_tmp(part_grp%num_grp))
!
      call const_ext_of_ext_grp_new_part(idomain_new, inod_new,         &
     &    node, neib_nod, part_param, part_grp, ext_ext_grp)
!
      if(my_rank .eq. 11) then
          jst = neib_nod%istack_next(41091-1) + 1
          jed = neib_nod%istack_next(41091)
          do j = jst, jed
            write(*,*) 11, 41091, 'neib_nod', neib_nod%inod_next(j)
!     &                idomain_new(j), inod_new(j)
          end do
      end if

      if(my_rank .eq. 11) then
        ip = 1
        ist = ext_ext_grp%istack_grp(ip)
        ied = ext_ext_grp%istack_grp(ip+1)
        write(*,*) 'search 47543 in ext_ext'
        do i = ist, ied
          if(ext_ext_grp%item_grp(i) .eq. 47543) write(*,*) 'Found!!'
        end do
      end if
!
      call gather_num_trans_for_repart                                  &
     &   (ext_ext_grp, num_send_tmp, num_recv_tmp)
      call const_comm_tbl_to_new_part                                   &
     &   (ext_ext_grp, num_send_tmp, num_recv_tmp, ext_ext_tbl)
!      call send_back_ext_istack_import                                 &
!     &   (part_grp, part_tbl, ext_ext_tbl, num_recv_tmp, num_send_tmp)
      deallocate(num_send_tmp, num_recv_tmp)
!
      allocate(inod_ext_ext(ext_ext_tbl%ntot_import))
      allocate(irank_ext_ext(ext_ext_tbl%ntot_import))
      allocate(jrank_ext_ext(ext_ext_tbl%ntot_import))
      allocate(isort_ext_ext(ext_ext_tbl%ntot_import))
      allocate(iflag_ext_ext(ext_ext_tbl%ntot_import))
!
      call calypso_SR_type_int(iflag_import_item, ext_ext_tbl,          &
     &    node%numnod, ext_ext_tbl%ntot_import,                         &
     &    idomain_new, irank_ext_ext)
      call calypso_SR_type_int(iflag_import_item, ext_ext_tbl,          &
     &    node%numnod, ext_ext_tbl%ntot_import,                         &
     &    inod_new, inod_ext_ext)
!
!$omp parallel do private(i)
      do i = 1, ext_ext_tbl%ntot_import
        isort_ext_ext(i) = i
        jrank_ext_ext(i) = mod(irank_ext_ext(i)+nprocs-my_rank,nprocs)
      end do
!$omp end parallel do
!
!
      call quicksort_w_index                                            &
     &   (ext_ext_tbl%ntot_import, jrank_ext_ext,                       &
     &    ione, ext_ext_tbl%ntot_import, isort_ext_ext)
!
!$omp parallel do private(i)
      do i = 1, ext_ext_tbl%ntot_import
        iflag_ext_ext(i) = inod_ext_ext(i)
      end do
!$omp end parallel do
!$omp parallel do private(i,j)
      do i = 1, ext_ext_tbl%ntot_import
        j = isort_ext_ext(i)
        inod_ext_ext(i) = iflag_ext_ext(j)
      end do
!$omp end parallel do
!
!$omp parallel do private(i)
      do i = 1, ext_ext_tbl%ntot_import
        iflag_ext_ext(i) = irank_ext_ext(i)
      end do
!$omp end parallel do
!$omp parallel do private(i,j)
      do i = 1, ext_ext_tbl%ntot_import
        j = isort_ext_ext(i)
        irank_ext_ext(i) = iflag_ext_ext(j)
      end do
!$omp end parallel do
!
      if(ext_ext_tbl%ntot_import .eq. 0) then
        num_neib_ext_ext = 0
      else
        num_neib_ext_ext = 1
        do i = 2, ext_ext_tbl%ntot_import
          if(irank_ext_ext(i) .ne. irank_ext_ext(i-1))                  &
     &          num_neib_ext_ext = num_neib_ext_ext + 1
        end do
      end if
!
      allocate(id_neib_ext_ext(num_neib_ext_ext))
      allocate(istack_neib_ext_ext(0:num_neib_ext_ext))
      allocate(ipe_missing_ext_ext(num_neib_ext_ext))
      ipe_missing_ext_ext(1:num_neib_ext_ext) = 0
!
      istack_neib_ext_ext(0) = 0
      if(ext_ext_tbl%ntot_import .gt. 0) then
        icou = 0
        do i = 2, ext_ext_tbl%ntot_import
          if(irank_ext_ext(i) .ne. irank_ext_ext(i-1)) then
            icou = icou + 1
            id_neib_ext_ext(icou) =     irank_ext_ext(i-1)
            istack_neib_ext_ext(icou) = i-1
          end if
        end do
        id_neib_ext_ext(num_neib_ext_ext)                               &
     &       = irank_ext_ext(ext_ext_tbl%ntot_import)
        istack_neib_ext_ext(num_neib_ext_ext) = ext_ext_tbl%ntot_import
      end if
!
      do i = 1, num_neib_ext_ext
        ist = istack_neib_ext_ext(i-1)
        num = istack_neib_ext_ext(i) - istack_neib_ext_ext(i-1)
        if(num .gt. 1) then
          call quicksort_w_index(num, inod_ext_ext(ist+1),              &
     &                           ione, num, isort_ext_ext(ist+1))
        end if
      end do
!
      do i = 1, ext_ext_tbl%ntot_import
        j = isort_ext_ext(i)
        ext_ext_tbl%item_import(j) = i
        ext_ext_tbl%irev_import(i) = j
      end do
!
!
      iflag_ext_ext(1:ext_ext_tbl%ntot_import) = 1
      do i = 2, ext_ext_tbl%ntot_import
        if(irank_ext_ext(i) .eq. irank_ext_ext(i-1)          &
     &     .and. inod_ext_ext(i) .eq. inod_ext_ext(i-1)) then
          iflag_ext_ext(i) = 0
        end if
      end do
!
      do icou = 1, num_neib_ext_ext
        ipe_missing_ext_ext(icou) = 1
        ip = id_neib_ext_ext(icou)
        do j = 1, num_neib_int_ext
          if(id_neib_int_ext(j) .eq. id_neib_ext_ext(icou)) then
            jp = j
            jst = istack_neib_int_ext(jp-1) + 1
            jed = istack_neib_int_ext(jp)
            ipe_missing_ext_ext(icou) = 0
            exit
          end if
        end do
!
        ist = istack_neib_ext_ext(icou-1)+1
        ied = istack_neib_ext_ext(icou)
        do i = ist, ied
          if(iflag_ext_ext(i) .gt. 0) then
            do j = jst, jed
              jnum = j + part_tbl%ntot_import
              if(inod_recv(jnum) .eq. ext_ext_tbl%item_import(i)) then
                iflag_ext_ext(i) = 1
                exit
              end if
            end do
!
          end if
        end do
      end do
!
!      write(my_rank+100,*) 'i, irank_ext_ext(i), inod_ext_ext(i)', &
!     &      ext_ext_tbl%ntot_import, sum(iflag_ext_ext)
!      do icou = 1, num_neib_ext_ext
!        ist = istack_neib_ext_ext(icou-1)+1
!        ied = istack_neib_ext_ext(icou)
!        write(my_rank+100,*) 'i, istack_neib_ext_ext(icou)', &
!     &      icou, id_neib_ext_ext(icou), istack_neib_ext_ext(icou), &
!     &      ipe_missing_ext_ext(icou)
!        do i = ist, ied
!          if(iflag_ext_ext(i) .gt. 0) then
!            write(my_rank+100,*) iflag_ext_ext(i),                 &
!     &       i, irank_ext_ext(i), inod_ext_ext(i), isort_ext_ext(i)
!          end if
!        end do
!      end do
!
!      allocate(inod_ext_ext2(ext_ext_tbl%ntot_import))
!      allocate(irank_ext_ext2(ext_ext_tbl%ntot_import))
!      call calypso_SR_type_int(iflag_import_item, ext_ext_tbl,         &
!     &    node%numnod, ext_ext_tbl%ntot_import,                        &
!     &    idomain_new, irank_ext_ext2)
!      call calypso_SR_type_int(iflag_import_item, ext_ext_tbl,         &
!     &    node%numnod, ext_ext_tbl%ntot_import,                        &
!     &    inod_new, inod_ext_ext2)
!
!      write(*,*) 'test'
!      do i = 1, ext_ext_tbl%ntot_import
!        if(irank_ext_ext2(i) .ne. irank_ext_ext(i)  &
!     &     .or. inod_ext_ext2(i) .ne. inod_ext_ext(i) ) then
!          write(*,*) 'Failed', my_rank, i
!        end if
!      end do
!
!      write(my_rank+100,*) 'i, ext_int_tbl', internal_node, numnod
!      do icou = 1, num_neib_int_ext
!        ist = istack_neib_int_ext(icou-1)+1
!        ied = istack_neib_int_ext(icou)
!        write(my_rank+100,*) 'i, istack_neib_int_ext(icou)', &
!     &      icou, id_neib_int_ext(icou), istack_neib_int_ext(icou)
!        do i = ist, ied
!            write(my_rank+100,*) i, idomain_recv(i+internal_node), inod_recv(i+internal_node)
!        end do
!      end do
!      write(*,*) my_rank, 'sum ipe_missing_ext_ext', sum(ipe_missing_ext_ext)
!
!      new_comm_tmp%num_neib   &
!     &       = num_neib_int_ext + sum(ipe_missing_ext_ext)
      new_comm_tmp%num_neib = num_neib_int_ext
      call alloc_comm_table_num(new_comm_tmp)
!
!      write(*,*) my_rank, 'new_comm_tmp%num_neib',   &
!     &          new_comm_tmp%num_neib, num_neib_int_ext
!
      do i = 1, num_neib_int_ext
        new_comm_tmp%id_neib(i) = id_neib_int_ext(i)
        new_comm_tmp%num_import(i)                                      &
     &      = istack_neib_int_ext(i) - istack_neib_int_ext(i-1)
      end do
!
!      icou = num_neib_int_ext
!      do i = 1, num_neib_ext_ext
!        if(ipe_missing_ext_ext(i) .gt. 0) then
!          icou = icou + 1
!          new_comm_tmp%id_neib(icou) = id_neib_ext_ext(i)
!        end if
!      end do
!
      do icou = 1, num_neib_ext_ext
        jp = 1
        do j = 1, new_comm_tmp%num_neib
          if(new_comm_tmp%id_neib(j) .eq. id_neib_ext_ext(icou)) then
            jp = j
            exit
          end if
        end do
!
        ist = istack_neib_ext_ext(icou-1)
        num = istack_neib_ext_ext(icou) - istack_neib_ext_ext(icou-1)
        if(ipe_missing_ext_ext(icou) .eq. 0) then
          do i = 1, num
            new_comm_tmp%num_import(jp)                                 &
     &         = new_comm_tmp%num_import(jp) + iflag_ext_ext(i+ist)
          end do
!
!        else if(ipe_missing_ext_ext(icou) .gt. 0) then
!          new_comm_tmp%num_import(jp) = num
        end if
      end do
!
      new_comm_tmp%istack_import(0) = 0
      do icou = 1, new_comm_tmp%num_neib
        new_comm_tmp%istack_import(icou) &
     &     = new_comm_tmp%istack_import(icou-1)  &
     &      + new_comm_tmp%num_import(icou)  
      end do
      new_comm_tmp%ntot_import                                          &
     &      = new_comm_tmp%istack_import(new_comm_tmp%num_neib)
!
      allocate(inod_ext_tmp(new_comm_tmp%ntot_import))
      allocate(irank_ext_tmp(new_comm_tmp%ntot_import))
      do jp = 1, num_neib_int_ext
        jst = new_comm_tmp%istack_import(jp-1)
        ist = istack_neib_int_ext(jp-1)
        num = istack_neib_int_ext(jp) - istack_neib_int_ext(jp-1)
        inod_ext_tmp(jst+1:jst+num)       &
     &      = inod_recv(internal_node+ist+1:internal_node+ist+num)
        irank_ext_tmp(jst+1:jst+num)       &
     &      = idomain_recv(internal_node+ist+1:internal_node+ist+num)
        new_comm_tmp%num_import(jp) = num
      end do
!
      do icou = 1, num_neib_ext_ext
        jp = 1
        do j = 1, new_comm_tmp%num_neib
          if(new_comm_tmp%id_neib(j) .eq. id_neib_ext_ext(icou)) then
            jp = j
            exit
          end if
        end do
!
        ist = istack_neib_ext_ext(icou-1)
        num = istack_neib_ext_ext(icou) - istack_neib_ext_ext(icou-1)
        if(ipe_missing_ext_ext(icou) .eq. 0) then
          jst = new_comm_tmp%istack_import(jp-1)   &
     &         + new_comm_tmp%num_import(jp)
          do i = 1, num
            if(iflag_ext_ext(i+ist) .gt. 0) then
              jst = jst + 1
              inod_ext_tmp(jst) =  inod_ext_ext(i+ist)
              irank_ext_tmp(jst) = irank_ext_ext(i+ist)
              new_comm_tmp%num_import(jp)                     &
     &              = new_comm_tmp%num_import(jp) + 1
            end if
          end do
!        else if(ipe_missing_ext_ext(icou) .gt. 0) then
!          jst = new_comm_tmp%istack_import(jp-1)
!          do i = 1, num
!            jst = jst + 1
!            inod_ext_tmp(jst) =  inod_ext_ext(i+ist)
!            irank_ext_tmp(jst) = irank_ext_ext(i+ist)
!          end do
        end if
      end do
!
!      write(my_rank+100,*) 'i, new_comm_tmp',   &
!     &    new_comm_tmp%num_neib, new_comm_tmp%ntot_import
!      do icou = 1, new_comm_tmp%num_neib
!        ist = new_comm_tmp%istack_import(icou-1) + 1
!        ied = new_comm_tmp%istack_import(icou)
!        write(my_rank+100,*) 'i, new_comm_tmp%istack_import(icou)', &
!     &      new_comm_tmp%id_neib(icou), new_comm_tmp%istack_import(icou)
!        do i = ist, ied
!            write(my_rank+100,*) i, irank_ext_tmp(i), inod_ext_tmp(i)
!        end do
!      end do
!
!
      allocate(ip_new_comm(new_comm_tmp%num_neib))
      allocate(isort_new_comm(new_comm_tmp%num_neib))
      do i = 1, new_comm_tmp%num_neib
        ip_new_comm(i) = mod(new_comm_tmp%id_neib(i)+nprocs-my_rank,nprocs)
        isort_new_comm(i) = i
      end do
!
      call quicksort_w_index(new_comm_tmp%num_neib, ip_new_comm,        &
     &    ione, new_comm_tmp%num_neib, isort_new_comm)
!
      new_comm%num_neib = new_comm_tmp%num_neib
      call alloc_comm_table_num(new_comm)
!
      new_comm_tmp%istack_import(0) = 0
      do icou = 1, new_comm%num_neib
        i = isort_new_comm(icou)
        ist = new_comm_tmp%istack_import(i-1)
        num = new_comm_tmp%istack_import(i)   &
     &       - new_comm_tmp%istack_import(i-1)
        new_comm%id_neib(icou) = new_comm_tmp%id_neib(i)
        new_comm%num_import(icou) = num
        new_comm%istack_import(icou) = num + new_comm%istack_import(icou-1)
      end do
      new_comm%ntot_import = new_comm%istack_import(new_comm%num_neib)
!
      call alloc_import_item(new_comm)
      allocate(inod_external(new_comm%ntot_import))
      allocate(irank_external(new_comm%ntot_import))
!
      do icou = 1, new_comm%num_neib
        i = isort_new_comm(icou)
        jst = new_comm%istack_import(icou-1)
        num = new_comm%istack_import(icou)   &
     &       - new_comm%istack_import(icou-1)
        ist = new_comm_tmp%istack_import(i-1)
        inod_external(jst+1:jst+num) =  inod_ext_tmp(ist+1:ist+num)
        irank_external(jst+1:jst+num) = irank_ext_tmp(ist+1:ist+num)
!
        call quicksort_int(num, inod_external(jst+1), ione, num)
!        call quicksort_int(num, irank_external(jst+1), ione, num)
      end do
!
      do icou = 1, new_comm%ntot_import
        new_comm%item_import(icou) = icou + internal_node
      end do
!
!      write(my_rank+100,*) 'i, new_comm',   &
!     &    new_comm%num_neib, new_comm%ntot_import
!      do icou = 1, new_comm%num_neib
!        ist = new_comm%istack_import(icou-1) + 1
!        ied = new_comm%istack_import(icou)
!        write(my_rank+100,*) 'i, new_comm%istack_import(icou)', &
!     &      new_comm%id_neib(icou), new_comm%istack_import(icou)
!        do i = ist, ied
!            write(my_rank+100,*) i, irank_external(i), inod_external(i)
!        end do
!      end do
!
!      allocate(num_send_tmp(nprocs))
!      allocate(num_recv_tmp(nprocs))
!      num_recv_tmp(1:nprocs) = 0
!
!      do icou = 1, new_comm%num_neib
!        ip = new_comm%id_neib(icou)
!        num_recv_tmp(ip+1) = new_comm%num_import(icou)
!      end do
!      call calypso_mpi_alltoall_one_int(num_recv_tmp, num_send_tmp)
!      do ip = 1, nprocs
!        write(200+my_rank,*) 'num_recv_tmp, num_send_tmp', ip-1, &
!     &            num_recv_tmp(ip), num_send_tmp(ip)
!      end do
!
      call element_num_reverse_SR                                       &
     &   (new_comm%num_neib, new_comm%id_neib, new_comm%num_import,     &
     &    SR_sig1, new_comm%num_export, new_comm%istack_export,         &
     &    new_comm%ntot_export)
!
      call alloc_export_item(new_comm)
      call reverse_send_recv_int(new_comm%num_neib, new_comm%id_neib,   &
     &    new_comm%istack_import, new_comm%istack_export,               &
     &    inod_external(1), SR_sig1, new_comm%item_export)
!
!
      new_node%internal_node =                 part_tbl%ntot_import
      new_node%numnod = new_comm%ntot_import + part_tbl%ntot_import
!
!
      write(*,*) my_rank, 'new_nomond', new_node%internal_node,         &
     &           new_node%numnod, ext_int_tbl%ntot_import
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
      call SOLVER_SEND_RECV_int8_type                                   &
     &   (new_node%numnod, new_comm, new_node%inod_global)
      call SOLVER_SEND_RECV_3_type                                      &
     &   (new_node%numnod, new_comm, new_node%xx(1,1))
!
      return
!
!

!
      allocate(iflag_pe(nprocs))
      iflag_pe(1:nprocs) = 0
      do i = 1, ext_int_tbl%nrank_import
      end do
      do i = 1, ext_ext_tbl%nrank_import
      end do
!
!
!
      write(*,*) my_rank, 'num_import', part_tbl%ntot_import,   &
     &               ext_int_tbl%ntot_import, ext_ext_tbl%ntot_import
      go to 10
!
      icou = 0
      do i = 1, ext_ext_tbl%nrank_import
        ip = ext_ext_tbl%irank_import(i)
        iflag = 1
        do j = 1, ext_int_tbl%nrank_import
          if(ip .eq. ext_int_tbl%irank_import(j)) then
            iflag = 0
            exit
          end if
        end do
        icou = icou + iflag
      end do
      call calypso_mpi_allreduce_one_int(icou, ntot, MPI_SUM)
      if(my_rank .eq. 0) write(*,*) 'Missing neighbour', ntot
!
      icou = 0
      jp = 0
      do i = 1, ext_ext_tbl%nrank_import
        ip = ext_ext_tbl%irank_import(i)
        jp = -1
        do j = 1, ext_int_tbl%nrank_import
          if(ip .eq. ext_int_tbl%irank_import(j)) then
            jp = j
            exit
          end if
        end do
!
        ist = ext_ext_tbl%istack_import(i-1) + 1
        ied = ext_ext_tbl%istack_import(i  )
        jst = ext_int_tbl%istack_import(i-1) + 1
        jed = ext_int_tbl%istack_import(i  )
        iflag = 1
        do inum = ist, ied
          do jnum = jst, jed
            if(ext_ext_tbl%item_import(inum)  &
     &          .eq. ext_int_tbl%item_import(jnum)) then
              iflag = 0
              exit
            end if
          end do
          icou = icou + iflag
        end do
      end do
      call calypso_mpi_allreduce_one_int(icou, ntot, MPI_SUM)
      if(my_rank .eq. 0) write(*,*) 'Missing node', ntot
  10  continue
!
!
!
      ist = new_node%internal_node
      call calypso_SR_type_int8(iflag_import_item, ext_int_tbl,         &
     &    node%numnod, ext_int_tbl%ntot_import,                         &
     &    node%inod_global(1), new_node%inod_global(ist+1))
      call calypso_SR_type_1(iflag_import_item, ext_int_tbl,            &
     &    node%numnod, ext_int_tbl%ntot_import,                         &
     &    node%xx(1,1), new_node%xx(ist+1,1))
      call calypso_SR_type_1(iflag_import_item, ext_int_tbl,            &
     &    node%numnod, ext_int_tbl%ntot_import,                         &
     &    node%xx(1,2), new_node%xx(ist+1,2))
      call calypso_SR_type_1(iflag_import_item, ext_int_tbl,            &
     &    node%numnod, ext_int_tbl%ntot_import,                         &
     &    node%xx(1,3), new_node%xx(ist+1,3))
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
      do i = 1, ext_int_tbl%ntot_import
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
      new_comm0%num_neib = tmp_tbl%nrank_export
      call alloc_comm_table_num(new_comm0)
!
      
!
      new_comm0%istack_export(0) = 0
      new_comm0%istack_import(0) = 0
!%omp parallel do
      do i = 1, new_comm0%num_neib
        new_comm0%id_neib(i) =       tmp_tbl%irank_export(i)
        new_comm0%num_export(i) =    tmp_tbl%num_export(i)
        new_comm0%istack_export(i) = tmp_tbl%istack_export(i)
        new_comm0%num_import(i) =    tmp_tbl%num_import(i)
        new_comm0%istack_import(i) = tmp_tbl%istack_import(i)
      end do
!%omp end parallel do
      new_comm0%ntot_export = new_comm0%istack_export(new_comm0%num_neib)
      new_comm0%ntot_import = new_comm0%istack_import(new_comm0%num_neib)
!
      call alloc_comm_table_item(new_comm0)
!
!$omp parallel do
      do i = 1, new_comm0%ntot_import
        new_comm0%item_import(i) = i + new_node%internal_node
        inod_import(i) = inod_recv(i+new_node%internal_node)
      end do
!$omp end parallel do
!
      call reverse_send_recv_int(new_comm0%num_neib, new_comm0%id_neib, &
     &    new_comm0%istack_import, new_comm0%istack_export,             &
     &    inod_import, SR_sig1, new_comm0%item_export)
!
!
!
      allocate(inod_recv2(new_node%numnod))
      allocate(idomain_recv2(new_node%numnod))
      allocate(inod_new2(node%numnod))
      allocate(idomain_new2(node%numnod))
!
      do inod = 1, node%internal_node
        idomain_new2(inod) = my_rank
        inod_new2(inod) =    inod
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, idomain_new2)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, inod_new2)
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    idomain_new(1), idomain_recv(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    inod_new(1), inod_recv(1))
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm0, idomain_recv)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_node%numnod, new_comm0, inod_recv)
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    idomain_new(1), idomain_recv2(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    node%numnod, new_node%internal_node,                          &
     &    inod_new(1), inod_recv2(1))
!
      call calypso_SR_type_int(iflag_import_item, ext_int_tbl,          &
     &    node%numnod, ext_int_tbl%ntot_import,                         &
     &    idomain_new(1), idomain_recv2(ist+1))
      call calypso_SR_type_int(iflag_import_item, ext_int_tbl,          &
     &    node%numnod, ext_int_tbl%ntot_import,                         &
     &    inod_new(1), inod_recv2(ist+1))
!
      write(100+my_rank,*) 'Final Check', my_rank
      do inod = 1, new_node%internal_node
        if(idomain_recv(inod) .ne. idomain_recv2(inod)  &
     &    .or. inod_recv(inod) .ne. inod_recv2(inod)) then
          write(100+my_rank,*) 'Failed', my_rank, inod, &
     &               idomain_recv(inod), idomain_recv2(inod),  &
     &               inod_recv(inod), inod_recv2(inod)
        end if
      end do
!
      deallocate(idomain_recv, inod_recv)
      deallocate(num_send_tmp, num_recv_tmp)
!
      end subroutine const_comm_tbls_for_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_new_element_connect(mesh, neib_ele, part_tbl, &
     &          ext_tbl, idomain_new, inod_new, ele_tbl, new_mesh)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_calypso_comm_table
!
      use calypso_mpi_int
      use calypso_SR_type
      use solver_SR_type
      use select_copy_from_recv
      use set_comm_tbl_to_new_part
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_around_node), intent(in) :: neib_ele
      type(calypso_comm_table), intent(in) :: part_tbl
      type(calypso_comm_table), intent(in) :: ext_tbl
      integer(kind = kint), intent(in) :: idomain_new(mesh%node%numnod)
      integer(kind = kint), intent(in) :: inod_new(mesh%node%numnod)
!
      type(calypso_comm_table), intent(inout) :: ele_tbl
      type(mesh_geometry), intent(inout) :: new_mesh
!
      integer(kind = kint), allocatable :: inod_recv(:)
      integer(kind = kint), allocatable :: icount_node(:)
!
      integer(kind = kint), allocatable :: num_send_tmp(:)
      integer(kind = kint), allocatable :: num_recv_tmp(:)
!
      integer(kind = kint), allocatable :: iflag_ele(:)
      integer(kind = kint), allocatable :: ie_to_new(:,:)
!
      integer(kind = kint), allocatable :: ie_newnod(:,:)
      integer(kind = kint), allocatable :: ie_newdomain(:,:)
!
      integer(kind = kint), allocatable :: ie_domain_recv(:,:)
!
      integer(kind = kint), allocatable :: iele_org_local(:)
      integer(kind = kint), allocatable :: iele_org_domain(:)
      integer(kind = kint), allocatable :: iele_local(:)
      integer(kind = kint), allocatable :: iele_domain(:)
!
      integer(kind = kint) :: icou, inum, i, ist, ied, ntot
      integer(kind = kint) :: ip, inod, iele, k1, ipart, iflag
      integer(kind = kint) :: jnum, j, jst, jed, jnod, jele
!
      integer(kind = kint), allocatable :: inod_old_lc(:)
      integer(kind = kint), allocatable :: irank_old_lc(:)
!
      integer(kind = kint), allocatable :: inod_new_lc(:)
      integer(kind = kint), allocatable :: irank_new_lc(:)
!
      integer(kind = kint), allocatable :: inod_trns1(:)
      integer(kind = kint), allocatable :: irank_trns1(:)
!
      integer(kind = kint), allocatable :: inod_trns2(:)
      integer(kind = kint), allocatable :: irank_trns2(:)
!
!
      
      allocate(inod_old_lc(mesh%node%numnod))
      allocate(irank_old_lc(mesh%node%numnod))
!
      do inod = 1, mesh%node%internal_node
        inod_old_lc(inod) =  inod
        irank_old_lc(inod) = my_rank
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (mesh%node%numnod, mesh%nod_comm, irank_old_lc)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (mesh%node%numnod, mesh%nod_comm, inod_old_lc)
!

      allocate(inod_new_lc(new_mesh%node%numnod))
      allocate(irank_new_lc(new_mesh%node%numnod))
      allocate(inod_trns1(new_mesh%node%numnod))
      allocate(irank_trns1(new_mesh%node%numnod))
      allocate(inod_trns2(new_mesh%node%numnod))
      allocate(irank_trns2(new_mesh%node%numnod))
!
      do inod = 1, new_mesh%node%internal_node
        inod_new_lc(inod) =  inod
        irank_new_lc(inod) = my_rank
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%node%numnod, new_mesh%nod_comm, irank_new_lc)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%node%numnod, new_mesh%nod_comm, inod_new_lc)
!
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    mesh%node%numnod, new_mesh%node%internal_node,                &
     &    inod_new(1), inod_trns1(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    mesh%node%numnod, new_mesh%node%internal_node,                &
     &    idomain_new(1), irank_trns1(1))
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%node%numnod, new_mesh%nod_comm, inod_trns1)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%node%numnod, new_mesh%nod_comm, irank_trns1)
!
!
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    mesh%node%numnod, new_mesh%node%internal_node,                &
     &    inod_new(1), inod_trns2(1))
      call calypso_SR_type_int(iflag_import_item, part_tbl,             &
     &    mesh%node%numnod, new_mesh%node%internal_node,                &
     &    idomain_new(1), irank_trns2(1))
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%node%numnod, new_mesh%nod_comm, inod_trns2)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%node%numnod, new_mesh%nod_comm, irank_trns2)
!
      write(*,*) 'Check irank_trns2, inod_trns2'
      do inod = 1, new_mesh%node%internal_node
        if(inod_trns1(inod) .ne. inod_new_lc(inod)    &
     &      .or. irank_trns1(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns1!' , inod
        end if
        if(inod_trns2(inod) .ne. inod_new_lc(inod)    &
     &      .or. irank_trns2(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns2!' , inod
        end if
      end do
!
      do inod = new_mesh%node%internal_node+1, new_mesh%node%numnod
        if(inod_trns1(inod) .ne. inod_new_lc(inod)    &
     &      .or. irank_trns1(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns1!' , inod
        end if
        if(inod_trns2(inod) .ne. inod_new_lc(inod)    &
     &      .or. irank_trns2(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns2!' , inod
        end if
      end do
!
!      allocate(ie_to_new(mesh%ele%numele,mesh%ele%nnod_4_ele))
!
      allocate(num_send_tmp(nprocs))
      allocate(num_recv_tmp(nprocs))
!
!$omp parallel workshare
        num_send_tmp(1:nprocs) = 0
        num_recv_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
      allocate(iflag_ele(mesh%ele%numele))
      do i = 1, part_tbl%nrank_export
!$omp parallel workshare
        iflag_ele(1:mesh%ele%numele) = 0
!$omp end parallel workshare
!
        ip =  part_tbl%irank_export(i)
        do iele = 1, mesh%ele%numele
!          if(mesh%ele%ie(iele,1) .gt. mesh%node%internal_node) cycle
          do k1 = 1, mesh%ele%nnod_4_ele
            inod = mesh%ele%ie(iele,k1)
            if(idomain_new(inod) .eq. ip) then
              iflag_ele(iele) = 1
              exit
            end if
          end do
        end do
!
        ntot = 0
!!$omp parallel do private(iele) reduction(+:ntot)
        do iele = 1, mesh%ele%numele
          ntot = ntot + iflag_ele(iele)
        end do
!!$omp end parallel do
        num_send_tmp(ip+1) = ntot
      end do
!
      call calypso_mpi_alltoall_one_int                                 &
     &   (num_send_tmp(1), num_recv_tmp(1))
      write(*,*) my_rank, 'internal_ele',  &
     &                    mesh%ele%internal_ele, sum(num_send_tmp)
      write(*,*) my_rank, 'num_recv_tmp',         &
     &                    new_mesh%node%numnod, sum(num_recv_tmp)
!      write(100+my_rank,*) my_rank, 'num_send_tmp', num_send_tmp
!      write(100+my_rank,*) my_rank, 'num_recv_tmp', num_recv_tmp
!
      ele_tbl%iflag_self_copy = 0
      call count_num_export_for_repart(my_rank, nprocs, num_send_tmp,   &
     &    ele_tbl%iflag_self_copy, ele_tbl%nrank_export)
      call count_num_import_for_repart(nprocs, num_recv_tmp,            &
     &                                 ele_tbl%nrank_import)
!
      call alloc_calypso_export_num(ele_tbl)
      call alloc_calypso_import_num(ele_tbl)
!
      call set_istack_export_for_repart(my_rank, nprocs, num_send_tmp,  &
     &    ele_tbl%nrank_export, ele_tbl%ntot_export,                    &
     &    ele_tbl%irank_export, ele_tbl%num_export,                     &
     &    ele_tbl%istack_export)
      call set_istack_import_for_repart(my_rank, nprocs, num_recv_tmp,  &
     &    ele_tbl%nrank_import, ele_tbl%ntot_import,                    &
     &    ele_tbl%irank_import, ele_tbl%num_import,                     &
     &    ele_tbl%istack_import)
!
      call alloc_calypso_export_item(ele_tbl)
      call alloc_calypso_import_item(ele_tbl%ntot_import, ele_tbl)
      call set_import_item_for_repart                                   &
     &     (ele_tbl%ntot_import, ele_tbl%ntot_import,                   &
     &      ele_tbl%item_import, ele_tbl%irev_import)
!
!
      do i = 1, ele_tbl%nrank_export
        ipart = 0
        do j = i, part_tbl%nrank_export
          if(ele_tbl%irank_export(i)                                    &
     &         .eq. part_tbl%irank_export(i)) then
            ipart = j
            exit
          end if
        end do
!
!$omp parallel workshare
        iflag_ele(1:mesh%ele%numele) = 0
!$omp end parallel workshare
!
        ip =  ele_tbl%irank_export(i)
        do iele = 1, mesh%ele%numele
!          if(mesh%ele%ie(iele,1) .gt. mesh%node%internal_node) cycle
          do k1 = 1, mesh%ele%nnod_4_ele
            inod = mesh%ele%ie(iele,k1)
            if(idomain_new(inod) .eq. ip) then
              iflag_ele(iele) = 1
              exit
            end if
          end do
        end do
!
        icou = ele_tbl%istack_export(i-1)
        do iele = 1, mesh%ele%numele
          if(iflag_ele(iele) .gt. 0) then
            icou = icou + 1
            ele_tbl%item_export(icou) = iele
          end if
        end do
      end do
!
!      write(100+my_rank,*) 'ele_tbl%nrank_export', ele_tbl%nrank_export
!      write(100+my_rank,*) 'ele_tbl%irank_export', ele_tbl%irank_export
!
!      do i = 1, ele_tbl%nrank_export
!        write(100+my_rank,*) i, 'ele_tbl%istack_export', &
!     &            ele_tbl%istack_export(i-1:i)
!        do icou = ele_tbl%istack_export(i-1)+1, ele_tbl%istack_export(i)
!          write(100+my_rank,*) icou, 'ele_tbl%item_export',   &
!     &          ele_tbl%item_export(icou), mesh%ele%numele
!        end do
!      end do
!
!      write(100+my_rank,*) 'ele_tbl%nrank_import', ele_tbl%nrank_import
!      write(100+my_rank,*) 'ele_tbl%irank_import', ele_tbl%irank_import
!
!      do i = 1, ele_tbl%nrank_import
!        write(100+my_rank,*) i, 'ele_tbl%istack_import', &
!     &            ele_tbl%istack_import(i-1:i)
!        do icou = ele_tbl%istack_import(i-1)+1, ele_tbl%istack_import(i)
!          write(100+my_rank,*) icou, 'ele_tbl%item_import',   &
!     &          ele_tbl%item_import(icou), ele_tbl%ntot_import
!        end do
!      end do
!
      allocate(ie_newnod(mesh%ele%numele,mesh%ele%nnod_4_ele))
      allocate(ie_newdomain(mesh%ele%numele,mesh%ele%nnod_4_ele))
!
!$omp parallel
      do k1 = 1, mesh%ele%nnod_4_ele
!$omp do private(iele,inod)
        do iele = 1, mesh%ele%numele
          inod = mesh%ele%ie(iele,k1)
          ie_newnod(iele,k1) =    inod_new(inod)
          ie_newdomain(iele,k1) = idomain_new(inod)
        end do
!$omp end do
      end do
!$omp end parallel
!
!
      new_mesh%ele%numele =     ele_tbl%ntot_import
      new_mesh%ele%nnod_4_ele = mesh%ele%nnod_4_ele
      call allocate_ele_connect_type(new_mesh%ele)
      allocate(ie_domain_recv(new_mesh%ele%numele,                 &
     &                        new_mesh%ele%nnod_4_ele))
!
!$omp parallel workshare
      new_mesh%ele%elmtyp(1:new_mesh%ele%numele) = mesh%ele%elmtyp(1)
      new_mesh%ele%nodelm(1:new_mesh%ele%numele) = mesh%ele%nodelm(1)
!$omp end parallel workshare
!
      call calypso_SR_type_int8(iflag_import_item, ele_tbl,             &
     &    mesh%ele%numele, new_mesh%ele%numele,                         &
     &    mesh%ele%iele_global(1), new_mesh%ele%iele_global(1))
!
      do k1 = 1, mesh%ele%nnod_4_ele
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      mesh%ele%numele, new_mesh%ele%numele,                       &
     &      ie_newnod(1,k1), new_mesh%ele%ie(1,k1))
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      mesh%ele%numele, new_mesh%ele%numele,                       &
     &      ie_newdomain(1,k1), ie_domain_recv(1,k1))
      end do
!
      allocate(iele_org_local(new_mesh%ele%numele))
      allocate(iele_org_domain(new_mesh%ele%numele))
      allocate(iele_local(mesh%ele%numele))
      allocate(iele_domain(mesh%ele%numele))
!
      do iele = 1, mesh%ele%numele
        iele_local(iele) = iele
        iele_domain(iele) = my_rank
      end do
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,             &
     &    mesh%ele%numele, new_mesh%ele%numele,                         &
     &    iele_local(1), iele_org_local(1))
      call calypso_SR_type_int(iflag_import_item, ele_tbl,             &
     &    mesh%ele%numele, new_mesh%ele%numele,                         &
     &    iele_domain(1), iele_org_domain(1))
!
      allocate(inod_recv(new_mesh%node%numnod))
      allocate(icount_node(new_mesh%node%numnod))
!$omp parallel do
      do inod = 1, new_mesh%node%numnod
        inod_recv(inod) =   inod
        icount_node(inod) = 0
      end do
!$omp end parallel do
      call SOLVER_SEND_RECV_int_type                                    &
     &  (new_mesh%node%numnod, new_mesh%nod_comm, inod_recv)
!
      icou = 0
        do iele = 1, new_mesh%ele%numele
      do k1 = 1, new_mesh%ele%nnod_4_ele
          ip =   ie_domain_recv(iele,k1)
          inod = new_mesh%ele%ie(iele,k1)
          iflag = 0
          if(ip .eq. my_rank) then
            icount_node(inod) = icount_node(inod) + 1
          else
            ist = 0
            ied = 0
            do inum = 1, new_mesh%nod_comm%num_neib
              if(ip .eq. new_mesh%nod_comm%id_neib(inum)) then
                ist = new_mesh%nod_comm%istack_import(inum-1) + 1
                ied = new_mesh%nod_comm%istack_import(inum)
                exit
              end if
            end do
!
            if(ist .gt. 0) then
              do jnum = ist, ied
                jnod = new_mesh%nod_comm%item_import(jnum)
                if(inod .eq. inod_recv(jnod)) then
                  iflag = inod_recv(jnod)
                  new_mesh%ele%ie(iele,k1) = jnod
                  icount_node(jnod) = icount_node(jnod) + 1
                  exit
                end if
              end do
            end if
!
            if(iflag .le. 0) then
              write(100+my_rank,*) 'Node cannot be found for ',        &
     &           new_mesh%ele%iele_global(iele), iele, k1, ip, inod, &
     &           iele_org_local(iele), iele_org_domain(iele)
              icou = icou + 1
            end if
          end if
        end do
      end do
!
      if(my_rank .eq. 11) then
        write(*,*) '11, 32784, ie_org', mesh%ele%iele_global(32784), &
     &             mesh%ele%ie(32784,1:8)
        do k1 = 1, 8
          icou = mesh%ele%ie(32784,k1)
          write(*,*) k1, 'node', mesh%node%inod_global(icou), &
     &             mesh%node%xx(icou,1:3),    &
     &             irank_old_lc(icou), inod_old_lc(icou),   &
     &             idomain_new(icou), inod_new(icou)
        end do
      end if
!
      write(*,*) my_rank, 'Missing connectivity: ', icou, &
     &          ' of ', new_mesh%ele%nnod_4_ele*new_mesh%ele%numele
!
      do inod = 1, new_mesh%node%internal_node
        inod_new_lc(inod) =  inod
        irank_new_lc(inod) = my_rank
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%node%numnod, new_mesh%nod_comm, irank_new_lc)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (new_mesh%node%numnod, new_mesh%nod_comm, inod_new_lc)
!
!      write(200+my_rank,*) 'new_mesh%node%numnod', new_mesh%node%numnod, new_mesh%node%internal_node
!      write(200+my_rank,*) 'new_mesh%nod_comm%num_neib', new_mesh%nod_comm%num_neib
!      write(200+my_rank,*) 'new_mesh%nod_comm%num_neib', new_mesh%nod_comm%istack_import
!      do inod = new_mesh%node%internal_node+1, new_mesh%node%numnod
!        write(200+my_rank,*) inod, irank_new_lc(inod), inod_new_lc(inod)
!      end do
!
      icou = 0
      do inod = 1, new_mesh%node%numnod
        if(icount_node(inod) .eq. 0) icou = icou + 1
      end do
!
      write(*,*) my_rank, 'Missing connenction: ', icou, &
     &          ' of ', new_mesh%node%numnod
!
      end subroutine const_new_element_connect
!
! ----------------------------------------------------------------------
!
      subroutine calypso_rev_SR_type_int                                &
     &         (cps_tbl, nnod_new, nnod_org, iX_new, iX_org)
!
      use m_solver_SR
      use t_calypso_comm_table
      use calypso_SR_int
      use select_copy_from_recv
!
      type(calypso_comm_table), intent(in) :: cps_tbl
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      integer(kind = kint), intent(in) :: iX_new(nnod_new)
!
      integer(kind = kint), intent(inout) :: iX_org(nnod_org)
!
      integer(kind = kint), allocatable :: irev_tmp(:)
!
      allocate(irev_tmp(nnod_org))
!$omp parallel workshare
      irev_tmp(1:nnod_org) = 0
!$omp end parallel workshare
!
      call calypso_send_recv_int(iflag_import_item, nnod_new, nnod_org, &
     &    cps_tbl%nrank_import, cps_tbl%iflag_self_copy,                &
     &    cps_tbl%irank_import, cps_tbl%istack_import,                  &
     &    cps_tbl%item_import,                                          &
     &    cps_tbl%nrank_export, cps_tbl%iflag_self_copy,                &
     &    cps_tbl%irank_export, cps_tbl%istack_export,                  &
     &    cps_tbl%item_export, irev_tmp,                                &
     &    SR_sig1, SR_i1, iX_new, iX_org)
!
      deallocate(irev_tmp)
!
      end subroutine calypso_rev_SR_type_int
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_repart_test
