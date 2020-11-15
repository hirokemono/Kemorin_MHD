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
     &    fem_T%mesh%node, next_tbl_T%neib_nod, T_meshes, part_grp,          &
     &    idomain_new, inod_new, new_fem%mesh%nod_comm,                 &
     &    new_fem%mesh%node, part_tbl, ext_tbl)
      call check_repart_node_transfer                                   &
     &   (fem_T%mesh, new_fem%mesh, part_tbl, idomain_new, inod_new)
!
!
      call const_new_element_connect(fem_T%mesh, ele_comm, part_tbl,    &
     &    idomain_new, inod_new, ele_tbl, new_fem%mesh)
!      call check_orogin_node_and_domain                                &
!     &    (new_fem%mesh%nod_comm, new_fem%mesh%node)
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
     &         (nod_comm, node, neib_nod, part_param, part_grp,     &
     &          idomain_new, inod_new, new_comm, new_node,              &
     &          part_tbl, ext_tbl)
!
      use t_comm_table
      use t_geometry_data
      use t_calypso_comm_table
      use t_control_param_vol_grping
      use t_sorting_for_repartition
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
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(mesh_test_files_param), intent(in) :: part_param
      type(group_data), intent(in) :: part_grp
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
      type(calypso_comm_table), intent(inout) :: part_tbl
      type(calypso_comm_table), intent(inout) :: ext_tbl
!
      integer(kind = kint), intent(inout) :: idomain_new(node%numnod)
      integer(kind = kint), intent(inout) :: inod_new(node%numnod)
!
      type(group_data) :: ext_int_grp
      type(group_data) :: ext_grp
      type(sorting_data_for_repartition) :: sort_nod
!
      integer(kind = kint), allocatable :: num_send_nod(:)
      integer(kind = kint), allocatable :: num_recv_nod(:)
!
      integer(kind = kint), allocatable :: idomain_recv(:)
      integer(kind = kint), allocatable :: inod_recv(:)
!
      integer(kind = kint) :: numnod, internal_node
!
      integer(kind = kint) :: inod
!
!
      allocate(num_send_nod(part_grp%num_grp))
      allocate(num_recv_nod(part_grp%num_grp))
!
      call gather_num_trans_for_repart                                  &
     &   (part_grp, num_send_nod, num_recv_nod)
      call const_comm_tbl_to_new_part                                   &
     &   (part_grp, num_send_nod, num_recv_nod, part_tbl)
!      call send_back_istack_import_repart                              &
!     &   (part_grp, part_tbl, num_recv_nod, num_send_nod)
!
!    Set local (idomain_recv, inod_recv) in internal node
      call node_dbl_numbering_to_repart                                 &
     &   (nod_comm, node, part_tbl, idomain_new, inod_new)
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
      call gather_num_trans_for_repart                                  &
     &   (ext_int_grp, num_send_nod, num_recv_nod)
      call const_comm_tbl_to_new_part                                   &
     &   (ext_int_grp, num_send_nod, num_recv_nod, ext_tbl)
!      call send_back_ext_istack_import                                 &
!     &   (part_grp, part_tbl, ext_tbl, num_recv_nod, num_send_nod)
      deallocate(num_send_nod, num_recv_nod)
!
!    Set local (idomain_recv, inod_recv) in internal node
      internal_node =                part_tbl%ntot_import
      numnod = ext_tbl%ntot_import + part_tbl%ntot_import
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
      call calypso_SR_type_int(iflag_import_item, ext_tbl,              &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    idomain_new(1), idomain_recv(internal_node+1))
      call calypso_SR_type_int(iflag_import_item, ext_tbl,              &
     &    node%numnod, ext_tbl%ntot_import,                             &
     &    inod_new(1), inod_recv(internal_node+1))
!
      call alloc_sorting_data(ext_tbl%ntot_import, sort_nod)
      call sort_node_by_domain_and_index(numnod, internal_node,         &
     &    idomain_recv, inod_recv, ext_tbl, sort_nod)
      deallocate(idomain_recv, inod_recv)
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
     &   (node, new_comm, new_node, part_tbl)
!
      end subroutine const_comm_tbls_for_new_part
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine node_dbl_numbering_to_repart                           &
     &         (nod_comm, node, part_tbl, idomain_new, inod_new)
!
      use t_comm_table
      use t_geometry_data
      use t_calypso_comm_table
!
      use nod_phys_send_recv
      use reverse_SR_int
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      type(calypso_comm_table), intent(in) :: part_tbl
!
      integer(kind = kint), intent(inout) :: idomain_new(node%numnod)
      integer(kind = kint), intent(inout) :: inod_new(node%numnod)
!
      integer(kind = kint), allocatable :: idomain_recv(:)
      integer(kind = kint), allocatable :: inod_recv(:)
!
      integer(kind = kint) :: inod
!
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
      end subroutine node_dbl_numbering_to_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_new_element_connect(mesh, ele_comm, part_tbl,    &
     &          idomain_new, inod_new, ele_tbl, new_mesh)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_calypso_comm_table
      use t_sorting_for_repartition
!
      use ele_trans_tbl_4_repart
!
      type(mesh_geometry), intent(in) :: mesh
      type(communication_table), intent(in) :: ele_comm
      type(calypso_comm_table), intent(in) :: part_tbl
      integer(kind = kint), intent(in) :: idomain_new(mesh%node%numnod)
      integer(kind = kint), intent(in) :: inod_new(mesh%node%numnod)
!
      type(calypso_comm_table), intent(inout) :: ele_tbl
      type(mesh_geometry), intent(inout) :: new_mesh
!
      integer(kind = kint), allocatable :: iele_local(:)
      integer(kind = kint), allocatable :: iele_domain(:)
!
      integer(kind = kint) :: new_numele
!
!
      allocate(iele_local(mesh%ele%numele))
      allocate(iele_domain(mesh%ele%numele))
!$omp parallel workshare
      iele_local(1:mesh%ele%numele) =  0
      iele_domain(1:mesh%ele%numele) = 0
!$omp end parallel workshare
!
      call double_numbering_4_eleement(mesh%ele, ele_comm,    &
     &          iele_local, iele_domain)
!
      call const_ele_trans_tbl_for_repart                               &
     &   (mesh%node, mesh%ele, part_tbl, idomain_new, ele_tbl)
!      call check_element_transfer_tbl(mesh%ele, ele_tbl)
! 
      call trim_overlapped_ele_by_repart(mesh, idomain_new, inod_new,   &
     &    iele_domain, iele_local, ele_tbl, new_numele)
!
      call const_reparition_ele_connect                                 &
     &   (mesh%node, mesh%ele, ele_tbl, idomain_new, inod_new,          &
     &    iele_local, iele_domain, new_numele, new_mesh)
      deallocate(iele_domain, iele_local)
!
      end subroutine const_new_element_connect
!
! ----------------------------------------------------------------------
!
      subroutine double_numbering_4_eleement(ele, ele_comm,             &
     &                                       iele_local, iele_domain)
!
      use t_geometry_data
      use t_comm_table
!
      use solver_SR_type
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: ele_comm
!
      integer(kind = kint), intent(inout) :: iele_local(ele%numele)
      integer(kind = kint), intent(inout) :: iele_domain(ele%numele)
!
      integer(kind = kint) :: iele
!
!
!$omp parallel do
      do iele = 1, ele%numele
        iele_local(iele) = iele
        iele_domain(iele) = my_rank
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm, iele_local)
      call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm, iele_domain)
!
      end subroutine double_numbering_4_eleement
!
! ----------------------------------------------------------------------
!
      subroutine const_reparition_ele_connect                           &
     &         (node, ele, ele_tbl, idomain_new, inod_new,              &
     &          iele_local, iele_domain, new_numele, new_mesh)
!
      use t_geometry_data
      use t_calypso_comm_table
!
      use search_ext_node_repartition
      use const_repart_mesh_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      integer(kind = kint), intent(in) :: idomain_new(node%numnod)
      integer(kind = kint), intent(in) :: inod_new(node%numnod)
!
      integer(kind = kint), intent(in) :: iele_local(ele%numele)
      integer(kind = kint), intent(in) :: iele_domain(ele%numele)
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
     &    ele_tbl, idomain_new, inod_new, ie_newdomain, ie_newnod,      &
     &    new_mesh%ele)
!
      call s_search_ext_node_repartition(ele, ele_tbl,                  &
     &    iele_local, iele_domain, ie_newdomain,                        &
     &    new_mesh%nod_comm, new_mesh%node, new_mesh%ele)
      deallocate(ie_newnod, ie_newdomain)
!
      end subroutine const_reparition_ele_connect
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
      subroutine check_repart_node_transfer                             &
     &         (mesh, new_mesh, part_tbl, idomain_new, inod_new)
!
      use t_mesh_data
      use t_calypso_comm_table
!
      use calypso_SR_type
      use solver_SR_type
      use select_copy_from_recv
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_geometry), intent(in) :: new_mesh
      type(calypso_comm_table), intent(in) :: part_tbl
      integer(kind = kint), intent(in) :: idomain_new(mesh%node%numnod)
      integer(kind = kint), intent(in) :: inod_new(mesh%node%numnod)
!
      integer(kind = kint), allocatable :: inod_old_lc(:)
      integer(kind = kint), allocatable :: irank_old_lc(:)
      integer(kind = kint), allocatable :: inod_new_lc(:)
      integer(kind = kint), allocatable :: irank_new_lc(:)
!
      integer(kind = kint), allocatable :: inod_trns1(:)
      integer(kind = kint), allocatable :: irank_trns1(:)
      integer(kind = kint), allocatable :: inod_trns2(:)
      integer(kind = kint), allocatable :: irank_trns2(:)
!
      integer(kind = kint) :: icou, inum, i, ist, ied, num
      integer(kind = kint) :: ip, inod, iele, k1, ipart, iflag
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
        if(inod_trns1(inod) .ne. inod_new_lc(inod)                      &
     &      .or. irank_trns1(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns1!' , inod
        end if
        if(inod_trns2(inod) .ne. inod_new_lc(inod)                      &
     &      .or. irank_trns2(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns2!' , inod
        end if
      end do
!
      do inod = new_mesh%node%internal_node+1, new_mesh%node%numnod
        if(inod_trns1(inod) .ne. inod_new_lc(inod)                      &
     &      .or. irank_trns1(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns1!' , inod
        end if
        if(inod_trns2(inod) .ne. inod_new_lc(inod)                      &
     &      .or. irank_trns2(inod) .ne. irank_new_lc(inod)) then
           write(*,*) my_rank, 'Wrong inod_trns2!' , inod
        end if
      end do
      deallocate(irank_old_lc, inod_old_lc)
      deallocate(irank_new_lc, inod_new_lc)
      deallocate(irank_trns1, inod_trns1)
      deallocate(irank_trns2, inod_trns2)
!
      end subroutine check_repart_node_transfer
!
! ----------------------------------------------------------------------
!
      end module analyzer_volume_repart_test
