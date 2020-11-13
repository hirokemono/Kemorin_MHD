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
      use quicksort
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
!
      call const_new_element_connect                                    &
     &   (fem_T%mesh, ele_comm, next_tbl_T%neib_ele,   &
     &    part_tbl, ext_tbl, idomain_new, inod_new, ele_tbl, new_fem%mesh)
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
      integer(kind = kint), allocatable :: num_recv_tmp2(:)
!
      integer(kind = kint), allocatable :: num_send_3(:)
      integer(kind = kint), allocatable :: num_recv_3(:)
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
      integer(kind = kint), allocatable :: iflag_dup(:)
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
!
!
      call gather_num_trans_for_repart                                  &
     &   (ext_int_grp, num_send_tmp, num_recv_tmp)
      call const_comm_tbl_to_new_part                                   &
     &   (ext_int_grp, num_send_tmp, num_recv_tmp, ext_int_tbl)
!      call send_back_ext_istack_import                                 &
!     &   (part_grp, part_tbl, ext_int_tbl, num_recv_tmp, num_send_tmp)
      deallocate(num_send_tmp, num_recv_tmp)
!
!    Set local (idomain_recv, inod_recv) in internal node
      internal_node =                    part_tbl%ntot_import
      numnod = ext_int_tbl%ntot_import + part_tbl%ntot_import
!
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
      allocate(num_recv_tmp(nprocs))
      allocate(num_recv_tmp2(nprocs))
      allocate(idx_sort(ext_int_tbl%ntot_import))
      allocate(inod_sort(ext_int_tbl%ntot_import))
      allocate(irank_sort(ext_int_tbl%ntot_import))
!$omp parallel workshare
      num_recv_tmp(1:nprocs) =  0
      num_recv_tmp2(1:nprocs) = 0
!$omp end parallel workshare
!
!
!
      do i = 1, ext_int_tbl%ntot_import
        ip = idomain_recv(i+internal_node)
        irank_sort(i) = mod(ip+nprocs-my_rank-1,nprocs)
        num_recv_tmp(ip+1) = num_recv_tmp(ip+1) + 1
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
        inod_recv(i+internal_node) = inod_sort(j)
        irank_sort(i) = idomain_recv(j+internal_node)
      end do
!$omp end parallel do
!
!$omp parallel do private(i)
      do i = 1, ext_int_tbl%ntot_import
        idomain_recv(i+internal_node)                                   &
     &       = mod(irank_sort(i)+my_rank,nprocs)
      end do
!$omp end parallel do
!
      jst = 0
      do i = 1, nprocs
        ip = mod(i+my_rank,nprocs)
        if(num_recv_tmp(ip+1) .gt. 0) then
          call quicksort_w_index                                        &
     &       (num_recv_tmp(ip+1), inod_recv(internal_node+jst+1),       &
     &        ione, num_recv_tmp(ip+1), idx_sort(jst+1))
          jst = jst + num_recv_tmp(ip+1)
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
!
!
      allocate(iflag_dup(ext_int_tbl%ntot_import))
!$omp parallel workshare
      iflag_dup(1:ext_int_tbl%ntot_import) = 1
!$omp end parallel workshare
!
      ist = 0
      do i = 1, nprocs-1
        ip = mod(i+my_rank,nprocs)
        do icou = 2, num_recv_tmp(ip+1)
          inod = internal_node + ist + icou
          if(inod_recv(inod) .eq. inod_recv(inod-1)) then
            iflag_dup(ist+icou) = 0
          end if
        end do
        do icou = 1, num_recv_tmp(ip+1)
          num_recv_tmp2(ip+1) = num_recv_tmp2(ip+1) + iflag_dup(ist+icou)
        end do
        ist = ist + num_recv_tmp(ip+1)
      end do
!
      new_comm%num_neib = 0
      do i = 1, nprocs-1
        ip = mod(i+my_rank,nprocs)
        if(num_recv_tmp2(ip+1) .gt. 0) then
          new_comm%num_neib = new_comm%num_neib + 1
        end if
      end do
      call alloc_comm_table_num(new_comm)
!
      icou = 0
      new_comm%istack_import(icou) = 0
      do i = 1, nprocs-1
        ip = mod(i+my_rank,nprocs)
        if(num_recv_tmp2(ip+1) .gt. 0) then
          icou = icou + 1
          ip = mod(i+my_rank,nprocs)
          new_comm%id_neib(icou) = mod(i+my_rank,nprocs)
          new_comm%num_import(icou) = num_recv_tmp2(ip+1)
          new_comm%istack_import(icou)                                  &
     &         = new_comm%istack_import(icou-1) + num_recv_tmp2(ip+1)
        end if
      end do
      new_comm%ntot_import = new_comm%istack_import(new_comm%num_neib)
!
!
!      write(100+my_rank,*) my_rank, 'num_recv_tmp(i), num_recv_tmp2(i)'
!      do i = 1, new_comm%num_neib
!        ip = new_comm%id_neib(i)
!        write(100+my_rank,*) i, new_comm%num_import(i), num_recv_tmp2(ip+1)
!      end do
!
!      allocate(num_send_3(nprocs))
!      allocate(num_recv_3(nprocs))
!      num_recv_3(1:nprocs) = 0
!      do i = 1, ext_int_tbl%nrank_import
!        ip = ext_int_tbl%irank_import(i)
!        num_recv_3(ip+1) = ext_int_tbl%istack_import(i) - ext_int_tbl%istack_import(i-1)
!      end do
!      call calypso_mpi_alltoall_one_int(num_recv_tmp2, num_send_3)
!
!      write(100+my_rank,*) my_rank, 'num_recv_tmp2(i), num_send_3(i)'
!      do i = 1, nprocs
!        write(100+my_rank,*) i-1, num_recv_tmp2(i), num_send_3(i)
!        if(num_recv_tmp2(i) .gt. 0 .and. num_send_3(i) .eq. 0) then
!          write(*,*) 'something wrong', my_rank, i-1
!        end if
!        if(num_recv_tmp2(i) .eq. 0 .and. num_send_3(i) .gt. 0) then
!          write(*,*) 'something wrong', my_rank, i-1
!        end if
!      end do
!
!
      call alloc_import_item(new_comm)
      allocate(inod_external(new_comm%ntot_import))
      allocate(irank_external(new_comm%ntot_import))
!
      ist = 0
      j = 0
      do i = 1, nprocs-1
        ip = mod(i+my_rank,nprocs)
        do icou = 1, num_recv_tmp(ip+1)
          if(iflag_dup(ist+icou) .gt. 0) then
            j = j + 1
            irank_external(j) = irank_sort(ist+icou)
            inod_external(j) =  inod_recv(internal_node+ist+icou)
            new_comm%item_import(j) = j + internal_node
          end if
        end do
        ist = ist + num_recv_tmp(ip+1)
      end do
!
      call element_num_reverse_SR                                       &
     &   (new_comm%num_neib, new_comm%id_neib, new_comm%num_import,     &
     &    SR_sig1, new_comm%num_export, new_comm%istack_export,         &
     &    new_comm%ntot_export)
!
      call alloc_export_item(new_comm)
      call reverse_send_recv_int(new_comm%num_neib, new_comm%id_neib,   &
     &    new_comm%istack_import, new_comm%istack_export,               &
     &    inod_external, SR_sig1, new_comm%item_export)
!
      deallocate(num_recv_tmp)
!
!      write(my_rank+100,*) 'i, new_comm',   &
!     &    new_comm%num_neib, new_comm%ntot_import
!      do icou = 1, new_comm%num_neib
!        ist = new_comm%istack_export(icou-1) + 1
!        ied = new_comm%istack_export(icou)
!        write(my_rank+100,*) 'i, new_comm%istack_export(icou)', &
!     &      new_comm%id_neib(icou), new_comm%istack_export(icou)
!        do i = ist, ied
!            write(my_rank+100,*) i, new_comm%item_export(i)
!        end do
!      end do
!
      new_node%internal_node =                 part_tbl%ntot_import
      new_node%numnod = new_comm%ntot_import + part_tbl%ntot_import
!
!
      write(*,*) my_rank, 'new_nomond', new_node%internal_node,         &
     &           new_node%numnod, new_comm%ntot_import
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
      call SOLVER_SEND_RECV_type                                        &
     &   (new_node%numnod, new_comm, new_node%xx(1,1))
      call SOLVER_SEND_RECV_type                                        &
     &   (new_node%numnod, new_comm, new_node%xx(1,2))
      call SOLVER_SEND_RECV_type                                        &
     &   (new_node%numnod, new_comm, new_node%xx(1,3))
!
      end subroutine const_comm_tbls_for_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_new_element_connect                              &
     &         (mesh, ele_comm, neib_ele, part_tbl,                     &
     &          ext_tbl, idomain_new, inod_new,                         &
     &          ele_tbl, new_mesh)
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
      use quicksort
!
      type(mesh_geometry), intent(in) :: mesh
      type(communication_table), intent(in) :: ele_comm
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
      integer(kind = kint) :: ntot_internal, ntot_external
      integer(kind = kint), allocatable :: num_send_ele(:)
      integer(kind = kint), allocatable :: num_recv_ele(:)
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
      integer(kind = kint), allocatable :: iele_recv(:)
      integer(kind = kint), allocatable :: idomain_recv(:)
      integer(kind = kint), allocatable :: iele_recv2(:)
      integer(kind = kint), allocatable :: idomain_recv2(:)
!
      integer(kind = kint), allocatable :: nele_send_tmp(:)
      integer(kind = kint), allocatable :: nele_recv_tmp(:)
      integer(kind = kint), allocatable :: nele_recv_tmp2(:)
      integer(kind = kint), allocatable :: iele_lc_recv(:)
      integer(kind = kint), allocatable :: irank_lc_recv(:)
      integer(kind = kint), allocatable :: idx_sort(:)
      integer(kind = kint), allocatable :: iele_sort(:)
      integer(kind = kint), allocatable :: irank_sort(:)
      integer(kind = kint), allocatable :: iflag_dup(:)
!
      integer(kind = kint) :: numele
      integer(kind = kint), allocatable :: idx_sort2(:)
!
      integer(kind = kint) :: icou, inum, i, ist, ied
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
      integer(kind = kint), allocatable :: i4_recv(:)
      integer(kind = kint_gl), allocatable :: i8_recv(:)
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
!
!
      allocate(iele_local(mesh%ele%numele))
      allocate(iele_domain(mesh%ele%numele))
!
      do iele = 1, mesh%ele%numele
        iele_local(iele) = iele
        iele_domain(iele) = my_rank
      end do
!
      call SOLVER_SEND_RECV_int_type                                   &
     &   (mesh%ele%numele, ele_comm, iele_local)
      call SOLVER_SEND_RECV_int_type                                   &
     &   (mesh%ele%numele, ele_comm, iele_domain)
!
!      allocate(ie_to_new(mesh%ele%numele,mesh%ele%nnod_4_ele))
!
      allocate(num_send_ele(nprocs))
      allocate(num_recv_ele(nprocs))
!
!$omp parallel workshare
        num_send_ele(1:nprocs) = 0
        num_recv_ele(1:nprocs) = 0
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
        ntot_internal = 0
!$omp parallel do private(iele) reduction(+:ntot_internal)
        do iele = 1, mesh%ele%numele
          if(iflag_ele(iele) .gt. 0) then
            ntot_internal = ntot_internal + 1
          end if
        end do
!$omp end parallel do
        num_send_ele(ip+1) = ntot_internal
      end do
!
      call calypso_mpi_alltoall_one_int(num_send_ele, num_recv_ele)
!
      write(*,*) my_rank, 'num_send_ele',  mesh%ele%internal_ele,       &
     &  sum(num_send_ele)
!      write(100+my_rank,*) my_rank, 'num_send_ele', num_send_ele
!      write(100+my_rank,*) my_rank, 'num_recv_ele', num_recv_ele
!
      ele_tbl%iflag_self_copy = 0
      call count_num_export_for_repart(my_rank, nprocs, num_send_ele,   &
     &    ele_tbl%iflag_self_copy, ele_tbl%nrank_export)
      call count_num_import_for_repart                                  &
     &   (nprocs, num_recv_ele, ele_tbl%nrank_import)
!
      call alloc_calypso_export_num(ele_tbl)
      call alloc_calypso_import_num(ele_tbl)
!
      call set_istack_export_for_repart(my_rank, nprocs, num_send_ele,  &
     &    ele_tbl%nrank_export, ele_tbl%ntot_export,                    &
     &    ele_tbl%irank_export, ele_tbl%num_export,                     &
     &    ele_tbl%istack_export)
      call set_istack_import_for_repart(my_rank, nprocs, num_recv_ele,  &
     &    ele_tbl%nrank_import, ele_tbl%ntot_import,                    &
     &    ele_tbl%irank_import, ele_tbl%num_import,                     &
     &    ele_tbl%istack_import)
!
      call alloc_calypso_export_item(ele_tbl)
      call alloc_calypso_import_item                                    &
     &   (ele_tbl%ntot_import, ele_tbl)
      call set_import_item_for_repart                                   &
     &   (ele_tbl%ntot_import, ele_tbl%ntot_import,                     &
     &    ele_tbl%item_import, ele_tbl%irev_import)
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
!      do i = 1, ele_tbl%nrank_export
!        write(100+my_rank,*) i, 'ele_tbl%istack_export', &
!     &   ele_tbl%irank_export(i), ele_tbl%istack_export(i-1:i)
!        do icou = ele_tbl%istack_export(i-1)+1, ele_tbl%istack_export(i)
!          write(100+my_rank,*) icou, 'ele_tbl%item_export',   &
!     &          ele_tbl%item_export(icou), mesh%ele%numele
!        end do
!      end do
!
!      write(100+my_rank,*) 'ele_tbl%nrank_import', ele_tbl%nrank_import
!      do i = 1, ele_tbl%nrank_import
!        write(100+my_rank,*) i, 'ele_tbl%istack_import', &
!     &  ele_tbl%irank_import(i), ele_tbl%istack_import(i-1:i)
!        do icou = ele_tbl%istack_import(i-1)+1, ele_tbl%istack_import(i)
!          write(100+my_rank,*) icou, 'ele_tbl%item_import',   &
!     &          ele_tbl%item_import(icou), ele_tbl%ntot_import
!        end do
!      end do
! 
      allocate(iele_recv(ele_tbl%ntot_import))
      allocate(idomain_recv(ele_tbl%ntot_import))
!
!    Set local (idomain_recv, inod_recv) in external node
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    mesh%ele%numele, ele_tbl%ntot_import,                         &
     &    iele_domain, idomain_recv)
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    mesh%ele%numele, ele_tbl%ntot_import,                         &
     &    iele_local, iele_recv)
!
      allocate(nele_send_tmp(nprocs))
      allocate(nele_recv_tmp(nprocs))
      allocate(nele_recv_tmp2(nprocs))
      allocate(idx_sort(ele_tbl%ntot_import))
      allocate(iele_sort(ele_tbl%ntot_import))
      allocate(irank_sort(ele_tbl%ntot_import))
!$omp parallel workshare
      nele_send_tmp(1:nprocs) =  0
      nele_recv_tmp(1:nprocs) =  0
      nele_recv_tmp2(1:nprocs) = 0
!$omp end parallel workshare
!
!$omp parallel do
      do iele = 1, ele_tbl%ntot_import
        idx_sort(iele) =   iele
        irank_sort(iele) = idomain_recv(iele)
        iele_sort(iele) =  iele_recv(iele)
      end do
!$omp end parallel do
!
      call quicksort_w_index(ele_tbl%ntot_import, irank_sort,           &
     &                       ione, ele_tbl%ntot_import, idx_sort)
!
      do icou = 1, ele_tbl%ntot_import
        ip = irank_sort(icou)
        nele_recv_tmp(ip+1) = nele_recv_tmp(ip+1) + 1
      end do
!$omp parallel do private(iele,icou)
      do icou = 1, ele_tbl%ntot_import
        iele = idx_sort(icou)
        iele_sort(icou) = iele_recv(iele)
      end do
!$omp end parallel do
!
      ist = 0
      do ip = 1, nprocs
        if(nele_recv_tmp(ip) .gt. 1) then
          call quicksort_w_index                                        &
     &       (nele_recv_tmp(ip), iele_sort(ist+1),                      &
     &        ione, nele_recv_tmp(ip), idx_sort(ist+1))
        end if
        ist = ist + nele_recv_tmp(ip)
      end do
!
      allocate(iflag_dup(ele_tbl%ntot_import))
!$omp parallel workshare
      iflag_dup(1:ele_tbl%ntot_import) = 1
!$omp end parallel workshare
!
      ist = 0
      do ip = 1, nprocs
        if(nele_recv_tmp(ip) .gt. 1) then
          do icou = 2, nele_recv_tmp(ip)
            if(iele_sort(ist+icou) .eq. iele_sort(ist+icou-1)) then
              iflag_dup(ist+icou) = 0
            end if
          end do
        end if
        ist = ist + nele_recv_tmp(ip)
      end do
!
!
      allocate(idx_sort2(ele_tbl%ntot_import))
      numele = sum(iflag_dup)
      ist = 0
      iele = 0
      jele = numele
      do ip = 1, nprocs
        if(nele_recv_tmp(ip) .le. 0) cycle
!
        do icou = 1, nele_recv_tmp(ip)
          if(iflag_dup(ist+icou) .eq. 0) then
            jele = jele + 1
            idx_sort2(jele) = idx_sort(ist+icou)
          else if(iflag_dup(ist+icou) .gt. 0) then
            iele = iele + 1
            idx_sort2(iele) = idx_sort(ist+icou)
          end if
        end do
        ist = ist + nele_recv_tmp(ip)
      end do

!      write(100+my_rank,*) 'ele_tbl%nrank_import', numele, ele_tbl%ntot_import
!      do icou = 1, ele_tbl%ntot_import
!          iele = idx_sort2(icou)
!          write(100+my_rank,*) icou,    &
!     &      'ele_tbl%item_import', iele,   &
!     &       idomain_recv(iele), iele_recv(iele)
!        end do
!      do icou = numele+1, ele_tbl%ntot_import
!          iele = idx_sort2(icou)
!          write(100+my_rank,*) icou,    &
!     &      'ele_tbl%item_import', iele,   &
!     &       idomain_recv(iele), iele_recv(iele)
!      end do
!
      do i = 1, ele_tbl%ntot_import
        j = idx_sort2(i)
        ele_tbl%item_import(j) = i
        ele_tbl%irev_import(i) = j
      end do
!
      allocate(iele_recv2(ele_tbl%ntot_import))
      allocate(idomain_recv2(ele_tbl%ntot_import))
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    mesh%ele%numele, ele_tbl%ntot_import,                         &
     &    iele_domain, idomain_recv2)
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    mesh%ele%numele, ele_tbl%ntot_import,                         &
     &    iele_local, iele_recv2)
!
      write(100+my_rank,*) 'check'
      do icou = 1, ele_tbl%ntot_import
        iele = idx_sort2(icou)
        if(idomain_recv2(icou) .eq. idomain_recv(iele))  cycle
        if(iele_recv2(icou) .eq. iele_recv(iele))  cycle
!
        write(100+my_rank,*) my_rank, icou, idx_sort2(icou), &
     &             idomain_recv2(icou)-idomain_recv(iele), &
     &             iele_recv2(icou)-iele_recv(iele)
      end do
!
      deallocate(nele_send_tmp, nele_recv_tmp, nele_recv_tmp2)
      deallocate(idx_sort, iele_sort, irank_sort)
!
!
      allocate(i4_recv(ele_tbl%ntot_import))
      allocate(i8_recv(ele_tbl%ntot_import))
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
      new_mesh%ele%numele =     numele
      new_mesh%ele%nnod_4_ele = mesh%ele%nnod_4_ele
      call allocate_ele_connect_type(new_mesh%ele)
      allocate(ie_domain_recv(numele,new_mesh%ele%nnod_4_ele))
!
!$omp parallel workshare
      new_mesh%ele%elmtyp(1:new_mesh%ele%numele) = mesh%ele%elmtyp(1)
      new_mesh%ele%nodelm(1:new_mesh%ele%numele) = mesh%ele%nodelm(1)
!$omp end parallel workshare
!
      call calypso_SR_type_int8(iflag_import_item, ele_tbl,             &
     &    mesh%ele%numele, ele_tbl%ntot_import,                         &
     &    mesh%ele%iele_global(1), i8_recv)
!$omp parallel workshare
      new_mesh%ele%iele_global(1:new_mesh%ele%numele)                   &
     &                   = i8_recv(1:new_mesh%ele%numele)
!$omp end parallel workshare
!
      do k1 = 1, mesh%ele%nnod_4_ele
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      mesh%ele%numele, ele_tbl%ntot_import,                       &
     &      ie_newnod(1,k1), i4_recv)
!$omp parallel workshare
        new_mesh%ele%ie(1:new_mesh%ele%numele,k1)                       &
     &                   = i4_recv(1:new_mesh%ele%numele)
!$omp end parallel workshare
      end do
!
      call search_repart_external_node(mesh%ele, ele_tbl,               &
     &    iele_local, iele_domain, ie_newdomain,                        &
     &    new_mesh%nod_comm, new_mesh%node, new_mesh%ele)
!
!      call check_orogin_node_and_domain                                &
!     &    (new_mesh%nod_comm, new_mesh%node)
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
      subroutine search_repart_external_node                            &
     &         (ele, ele_tbl, iele_local, iele_domain, ie_newdomain,    &
     &          new_comm, new_node, new_ele)
!
      use t_geometry_data
      use t_comm_table
      use t_calypso_comm_table
!
      use calypso_SR_type
      use solver_SR_type
      use select_copy_from_recv
!
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: iele_local(ele%numele)
      integer(kind = kint), intent(in) :: iele_domain(ele%numele)
!
      integer(kind = kint), intent(in)                                  &
     &              :: ie_newdomain(ele%numele,ele%nnod_4_ele)
!
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint), allocatable :: inod_recv(:)
      integer(kind = kint), allocatable :: icount_node(:)
      integer(kind = kint), allocatable :: item_import_recv(:)
!
      integer(kind = kint), allocatable :: iele_org_local(:)
      integer(kind = kint), allocatable :: iele_org_domain(:)
      integer(kind = kint), allocatable :: ie_domain_recv(:,:)
!
      integer(kind = kint), allocatable :: i4_recv(:)
!
      integer(kind = kint) :: ip, inod, icou, inum, ist, ied
      integer(kind = kint) :: iele, k1, jnum, jnod, iflag
!
!
      allocate(i4_recv(ele_tbl%ntot_import))
      allocate(ie_domain_recv(new_ele%numele,new_ele%nnod_4_ele))
!
      do k1 = 1, ele%nnod_4_ele
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import, ie_newdomain(1,k1),        &
     &      i4_recv(1))
!$omp parallel workshare
        ie_domain_recv(1:new_ele%numele,k1) = i4_recv(1:new_ele%numele)
!$omp end parallel workshare
      end do
!
      allocate(iele_org_local(new_ele%numele))
      allocate(iele_org_domain(new_ele%numele))
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, iele_local(1),               &
     &    i4_recv(1))
!$omp parallel workshare
      iele_org_local(1:new_ele%numele) = i4_recv(1:new_ele%numele)
!$omp end parallel workshare
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, iele_domain(1),              &
     &    i4_recv(1))
!$omp parallel workshare
      iele_org_domain(1:new_ele%numele) = i4_recv(1:new_ele%numele)
!$omp end parallel workshare
!
      allocate(inod_recv(new_node%numnod))
      allocate(icount_node(new_node%numnod))
!$omp parallel do
      do inod = 1, new_node%numnod
        inod_recv(inod) =   inod
        icount_node(inod) = 0
      end do
!$omp end parallel do
      call SOLVER_SEND_RECV_int_type                                    &
     &  (new_node%numnod, new_comm, inod_recv)
!
      allocate(item_import_recv(new_comm%ntot_import))
!$omp parallel do private(jnum,jnod)
      do jnum = 1, new_comm%ntot_import
        jnod = new_comm%item_import(jnum)
        item_import_recv(jnum) = inod_recv(jnod)
      end do
!$omp end parallel do
!
      icou = 0
      do iele = 1, new_ele%numele
        do k1 = 1, new_ele%nnod_4_ele
          ip =   ie_domain_recv(iele,k1)
          inod = new_ele%ie(iele,k1)
          iflag = 0
          if(ip .eq. my_rank) then
            icount_node(inod) = icount_node(inod) + 1
          else
            inum = search_from_list_data(ip, ione, new_comm%num_neib,   &
     &                             new_comm%num_neib, new_comm%id_neib)
            if(inum.ge.ione .and. inum.le.new_comm%num_neib) then
              ist = new_comm%istack_import(inum-1) + 1
              ied = new_comm%istack_import(inum)
            else
              ist = 0
              ied = 0
            end if
!
            if(ist .gt. 0) then
              jnum = search_from_sorted_data(inod, ist, ied,            &
     &                          new_comm%ntot_import, item_import_recv)
              if(jnum.ge.ist .and. jnum.le.ied) then
                jnod = new_comm%item_import(jnum)
                iflag = item_import_recv(jnum)
                new_ele%ie(iele,k1) = jnod
                icount_node(jnod) = icount_node(jnod) + 1
              end if
            end if
!
            if(iflag .le. 0) then
              new_ele%ie(iele,k1) = 0
              write(*,*) my_rank, 'Node cannot be found for ',         &
     &           new_ele%iele_global(iele), iele, k1, ip, inod,        &
     &           iele_org_local(iele), iele_org_domain(iele)
              icou = icou + 1
            end if
          end if
        end do
      end do
      deallocate(i4_recv, ie_domain_recv, item_import_recv)
      deallocate(iele_org_local, iele_org_domain, inod_recv)
!
      if(i_debug .gt. 0) then
        write(*,*) my_rank, 'Missing connectivity: ', icou,             &
     &          ' of ', new_ele%nnod_4_ele*new_ele%numele
!
        icou = 0
        do inod = 1, new_node%numnod
          if(icount_node(inod) .eq. 0) icou = icou + 1
        end do
        write(*,*) my_rank, 'Missing connenction: ', icou,              &
     &          ' of ', new_node%numnod
      end if
!
      deallocate(icount_node)
!
      end subroutine search_repart_external_node
!
! ----------------------------------------------------------------------
!
      subroutine check_orogin_node_and_domain(nod_comm, node)
!
      use calypso_mpi
      use t_geometry_data
      use t_comm_table
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
!
      integer(kind = kint), allocatable :: inod_new_lc(:)
      integer(kind = kint), allocatable :: irank_new_lc(:)
!
      integer(kind = kint) :: inod
!
!
      allocate(inod_new_lc(node%numnod))
      allocate(irank_new_lc(node%numnod))
!
      do inod = 1, node%internal_node
        inod_new_lc(inod) =  inod
        irank_new_lc(inod) = my_rank
      end do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, irank_new_lc)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (node%numnod, nod_comm, inod_new_lc)
!
      write(200+my_rank,*) 'node%numnod',                               &
     &              node%numnod, node%internal_node
      write(200+my_rank,*) 'nod_comm%num_neib', nod_comm%num_neib
      write(200+my_rank,*) 'nod_comm%istack_import',                    &
     &                    nod_comm%istack_import
      do inod = node%internal_node+1, node%numnod
        write(200+my_rank,*) inod,                                      &
     &                      irank_new_lc(inod), inod_new_lc(inod)
      end do
!
      deallocate(irank_new_lc, inod_new_lc)
!
      end subroutine check_orogin_node_and_domain
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function search_from_list_data               &
     &                   (i_target, ist, ied, num, input_list)
!
      integer(kind = kint), intent(in) :: i_target
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: input_list(num)
!
      integer(kind = kint) :: jnum, iflag
!
!
      iflag = ist - 1
      do jnum = ist, ied
        if(i_target .eq. input_list(jnum)) then
          iflag = jnum
          exit
        end if
      end do
      search_from_list_data = iflag
!
      end function search_from_list_data
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function search_from_sorted_data             &
     &                   (i_target, ist, ied, num, input_list)
!
      integer(kind = kint), intent(in) :: i_target
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: input_list(num)
!
      integer(kind = kint) :: jst, jed, jnum, iflag
!
!
      jst = ist
      jed = ied
      jnum = (jst+jed) / 2
      do 
        if(i_target .eq. input_list(jnum)) then
          iflag = jnum
          exit
        else if(jst .eq. jed) then
          iflag = ist - 1
          exit
        else if((jed-jst) .eq. 1) then
          iflag = ist - 1
          if(i_target .eq. input_list(jst)) iflag = jst
          if(i_target .eq. input_list(jed)) iflag = jed
          exit
        else if(i_target .lt. input_list(jnum)) then
          jed = jnum
          jnum = (jst+jed) / 2
        else
          jst = jnum
          jnum = (jst+jed) / 2
        end if
      end do
      search_from_sorted_data = iflag
!
      end function search_from_sorted_data
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_repart_test
