!>@file   analyzer_repart_by_volume.f90
!!@brief  module analyzer_repart_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine initialize_reapart_by_vol
!!      subroutine analyze_reapart_by_vol
!!@endverbatim
!
      module analyzer_repart_by_volume
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_REPART
      use m_work_time_4_sleeve_extend
      use calypso_mpi
!
      use t_mesh_data
      use t_calypso_comm_table
      use t_control_param_repartition
      use t_vector_for_solver
      use t_mesh_SR
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &     :: repart_test_name = 'repart_check.dat'
!
      type(vol_partion_prog_param), save ::  part_p1
      type(mesh_data), save :: fem_T
      type(mesh_data), save :: new_fem
      type(mesh_SR), save :: m_SR_T
!
      type(calypso_comm_table), save :: repart_nod_tbl1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_reapart_by_vol
!
      use t_next_node_ele_4_node
      use t_ctl_file_volume_grouping
      use t_const_comm_table
!
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use nod_phys_send_recv
      use repartiton_by_volume
      use const_element_comm_tables
      use set_element_id_4_node
      use int_volume_of_single_domain
      use field_to_new_partition
!
!>     Stracture for Jacobians
!
      type(new_patition_test_control) :: part_tctl1
      type(communication_table), save :: ele_comm1
      type(jacobians_type), save :: jacobians1
      type(shape_finctions_at_points), save :: spfs1
      type(next_nod_ele_table), save :: next_tbl1
      type(sleeve_extension_work), save :: sleeve_exp_WK1
!
      type(masking_parameter), allocatable, target :: masking1(:)
      real(kind = kreal), allocatable :: d_mask_org1(:,:)
      real(kind = kreal), allocatable :: vect_ref1(:,:)
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_repartition
      call elpsed_label_4_sleeve_ext
      call elapsed_label_4_ele_comm_tbl
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!     ----- read control data
!
      call read_control_new_partition(part_tctl1)
      call set_control_param_repartition(part_tctl1, part_p1)
!
!  --  read geometry
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%mesh_file, nprocs, fem_T)
!
!  -------------------------------
!
      call init_nod_send_recv(fem_T%mesh,                               &
     &    m_SR_T%SR_sig, m_SR_T%SR_r, m_SR_T%SR_i, m_SR_T%SR_il)
      if(iflag_debug .gt. 0) write(*,*) 'estimate node volume'
!
!  -------------------------------
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization(fem_T%mesh, fem_T%group,             &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
!
!
!  -----  Const Element communication table
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
      call const_global_numele_list(fem_T%mesh%ele)
      call const_ele_comm_table(fem_T%mesh%node,                        &
     &    fem_T%mesh%nod_comm, fem_T%mesh%ele, ele_comm1, m_SR_T)
!
!  -----  Const volume of each element
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_single_vol'
      call const_jacobian_and_single_vol                                &
     &   (fem_T%mesh, fem_T%group, spfs1, jacobians1)
      call finalize_jac_and_single_vol                                  &
     &   (fem_T%mesh, spfs1, jacobians1)
!
!  -----  Const Neighboring information
      if(iflag_debug .gt. 0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (fem_T%mesh, next_tbl1%neib_ele, next_tbl1%neib_nod)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+5)
!
!  -----  Re-partitioning
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+1)
      if(iflag_debug .gt. 0) write(*,*) 's_repartiton_by_volume'
      allocate(masking1(0))
      allocate(d_mask_org1(fem_T%mesh%node%numnod,1))
      allocate(vect_ref1(fem_T%mesh%node%numnod,3))
      call s_repartiton_by_volume((.TRUE.), part_p1%repart_p,           &
     &    fem_T%mesh, fem_T%group, ele_comm1, next_tbl1,                &
     &    izero, masking1, vect_ref1(1,1), d_mask_org1, vect_ref1,      &
     &    new_fem%mesh, new_fem%group, repart_nod_tbl1,                 &
     &    sleeve_exp_WK1, m_SR_T)
      deallocate(d_mask_org1, vect_ref1, masking1)
!
      call dealloc_comm_table(ele_comm1)
      call dealloc_next_nod_ele_table(next_tbl1)
!      call dealloc_mesh_infomations(fem_T%mesh, fem_T%group)
      if(iflag_RPRT_time) call end_elapsed_time(ist_elapsed_RPRT+1)
!
      call dealloc_node_param_smp(new_fem%mesh%node)
      call dealloc_ele_param_smp(new_fem%mesh%ele)
!
      end subroutine initialize_reapart_by_vol
!
! ----------------------------------------------------------------------
!
      subroutine analyze_reapart_by_vol
!
      use t_solver_SR
      use t_interpolate_table
      use t_work_for_comm_check
!
      use t_repart_double_numberings
      use t_para_double_numbering
!
      use m_file_format_switch
      use para_itrplte_table_IO_sel
      use copy_repart_and_itp_table
      use const_element_comm_tables
      use const_surface_comm_table
      use check_data_for_repartition
      use mesh_send_recv_check
      use write_diff_4_comm_test
      use nod_phys_send_recv
      use parallel_FEM_mesh_init
      use load_repartition_table
!
      use const_repart_mesh_data
      use compare_mesh_structures
      use redistribute_groups
!
      type(communication_table), save :: ele_comm1
!
      type(calypso_comm_table) :: part_nod_tbl2
      type(calypso_comm_table) :: part_ele_tbl2
      type(communication_table), save :: new_ele_comm2
!
      type(mesh_data), save :: new_fem2
      integer(kind = kint) :: new_numele
!
      type(communication_table), save :: T_ele_comm
      type(communication_table), save :: T_surf_comm
      type(communication_table), save :: T_edge_comm
!
      type(work_for_comm_check), save :: nod_check
      type(work_for_comm_check), save :: ele_check
      type(work_for_comm_check), save :: surf_check
      type(work_for_comm_check), save :: edge_check
!
      type(node_ele_double_number), save :: new_ids_on_org1
      type(node_ele_double_number), save :: new_iele_dbl1
!
      integer(kind = kint) :: irank_read
      integer(kind = kint) :: i, ierr, icount_error, icou, iele
!
!
      if(part_p1%repart_p%trans_tbl_file%iflag_format                   &
     &     .eq. id_no_file) then
        if(my_rank .eq. 0) write(*,*) 'No file to check data transfer'
        return
      end if
!
!
      call set_repart_table_from_file                                   &
     &   (part_p1%repart_p%trans_tbl_file, new_fem2%mesh%ele%numele,    &
     &    part_nod_tbl2, part_ele_tbl2, new_fem2%mesh%nod_comm,         &
     &    new_ele_comm2)
      write(*,*) my_rank, 'new_fem2%mesh%ele%numele', new_fem2%mesh%ele%numele
!
      if(iflag_RPRT_time) call start_elapsed_time(ist_elapsed_RPRT+5)
      if(iflag_debug.gt.0) write(*,*)' FEM_mesh_initialization'
      call FEM_mesh_initialization(new_fem%mesh, new_fem%group,         &
     &                             m_SR_T%SR_sig, m_SR_T%SR_i)
!
!
      call set_repart_node_position(part_nod_tbl2, fem_T%mesh%node,     &
     &    new_fem2%mesh%nod_comm, new_fem2%mesh%node,                   &
     &    m_SR_T%SR_sig, m_SR_T%SR_r, m_SR_T%SR_il)
!
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
!      call const_global_numele_list(fem_T%mesh%ele)
      call const_ele_comm_table(fem_T%mesh%node,                        &
     &    fem_T%mesh%nod_comm, fem_T%mesh%ele, ele_comm1, m_SR_T)
!
      call alloc_double_numbering(fem_T%mesh%node%numnod,               &
     &                            new_ids_on_org1)
      call node_dbl_numbering_to_repart                                 &
     &   (fem_T%mesh%nod_comm, fem_T%mesh%node, part_nod_tbl2,          &
     &    new_ids_on_org1, m_SR_T%SR_sig, m_SR_T%SR_i)
!
      call alloc_double_numbering(new_fem2%mesh%ele%numele,             &
     &                            new_iele_dbl1)
      call double_numbering_4_element(new_fem2%mesh%ele, new_ele_comm2, &
     &    new_iele_dbl1, m_SR_T%SR_sig, m_SR_T%SR_i)
      icou = 0
      do iele = 1, new_fem2%mesh%ele%numele
        if(new_iele_dbl1%irank(iele) .eq. my_rank) icou = icou+1
      end do
!
      new_numele = max(maxval(part_ele_tbl2%item_import),               &
     &                        maxval(new_ele_comm2%item_import))
      call s_const_repart_ele_connect_2                                 &
     &  (fem_T%mesh, ele_comm1, part_nod_tbl2, new_ids_on_org1,         &
     &   new_fem2%mesh%nod_comm, new_fem2%mesh%node, new_ele_comm2,     &
     &   new_iele_dbl1, new_fem2%mesh%ele, part_ele_tbl2,               &
     &   m_SR_T%SR_sig, m_SR_T%SR_i, m_SR_T%SR_il, new_fem%mesh%ele)
      call dealloc_double_numbering(new_iele_dbl1)
      call dealloc_double_numbering(new_ids_on_org1)
!
      if(my_rank .eq. 0) write(*,*) 'Compare read comm tables...'
      call compare_calypso_comm_tbls(repart_nod_tbl1, part_nod_tbl2)
      call calypso_MPI_barrier
      if(my_rank .eq. 0) write(*,*) 'Compareing end!'
!
      call compare_node_comm_types(my_rank, new_fem%mesh%nod_comm,      &
     &                             new_fem2%mesh%nod_comm)
      call compare_node_position(my_rank, new_fem%mesh%node,            &
     &                           new_fem2%mesh%node, icount_error)
      write(*,*) my_rank, 'Compare node: ', icount_error
      call compare_ele_connect(my_rank, new_fem%mesh%ele,               &
     &    new_fem2%mesh%ele, icount_error)
      write(*,*) my_rank, 'Compare element: ', icount_error
!
!
      call s_redistribute_groups                                        &
     &   ((.TRUE.), fem_T%mesh, fem_T%group, ele_comm1,                 &
     &    new_fem2%mesh, new_ele_comm2, part_nod_tbl2, part_ele_tbl2,   &
     &    new_fem2%group, m_SR_T%SR_sig, m_SR_T%SR_i)
      call dealloc_comm_table(ele_comm1)
!
      call compare_mesh_groups(my_rank, new_fem%group, new_fem2%group)
!

      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
      call const_global_numele_list(new_fem%mesh%ele)
      call const_ele_comm_table(new_fem%mesh%node,                      &
     &    new_fem%mesh%nod_comm, new_fem%mesh%ele, T_ele_comm, m_SR_T)
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
      call const_surf_comm_table                                        &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm, T_surf_comm,        &
     &    new_fem%mesh%surf, m_SR_T)
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table'
      call const_edge_comm_table                                        &
     &   (new_fem%mesh%node, new_fem%mesh%nod_comm, T_edge_comm,        &
     &    new_fem%mesh%edge, m_SR_T)
!
!
      if(my_rank .eq. 0) write(*,*) 'check communication table...'
      call node_transfer_test(fem_T%mesh%node, new_fem%mesh%node,       &
     &    new_fem%mesh%nod_comm, repart_nod_tbl1, nod_check,            &
     &    m_SR_T%SR_sig, m_SR_T%SR_r, m_SR_T%SR_il)
!
!
      call ele_send_recv_test(new_fem%mesh%ele, T_ele_comm,             &
     &                        ele_check, m_SR_T%SR_sig, m_SR_T%SR_r)
      call surf_send_recv_test(new_fem%mesh%surf, T_surf_comm,          &
     &                         surf_check, m_SR_T%SR_sig, m_SR_T%SR_r)
      call edge_send_recv_test(new_fem%mesh%edge, T_edge_comm,          &
     &                         edge_check, m_SR_T%SR_sig, m_SR_T%SR_r)
!
      call output_diff_mesh_comm_test(repart_test_name,                 &
     &    nod_check, ele_check, surf_check, edge_check)
!
      call dealloc_ele_comm_test_IO(nod_check)
      call dealloc_ele_comm_test_IO(ele_check)
      call dealloc_ele_comm_test_IO(surf_check)
      call dealloc_ele_comm_test_IO(edge_check)
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_reapart_by_vol
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_const_repart_ele_connect_2(mesh, ele_comm, part_tbl, &
     &          new_ids_on_org, new_comm, new_node, new_ele_comm,       &
     &          new_iele_dbl, new_ele, ele_tbl, SR_sig, SR_i, SR_il,    &
     &          ref_ele)
!
      use t_para_double_numbering
      use t_repart_double_numberings
      use ele_trans_tbl_4_repart
      use compare_mesh_structures
      use const_repart_ele_connect
!
      type(mesh_geometry), intent(in) :: mesh
      type(communication_table), intent(in) :: ele_comm
      type(calypso_comm_table), intent(in) :: part_tbl
      type(node_ele_double_number), intent(in) :: new_ids_on_org
      type(communication_table), intent(in) :: new_comm, new_ele_comm
      type(node_data), intent(in) :: new_node
      type(node_ele_double_number), intent(in) :: new_iele_dbl
      type(element_data), intent(in) :: ref_ele
!
      type(calypso_comm_table), intent(inout) :: ele_tbl
      type(element_data), intent(inout) :: new_ele
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      type(node_ele_double_number) :: org_iele_dbl
      type(node_ele_double_number) :: new_inod_dbl
!
      integer(kind = kint) :: new_numele, icount_error
!
!
      call alloc_double_numbering(new_node%numnod, new_inod_dbl)
      call set_node_double_numbering(new_node, new_comm,                &
     &                               new_inod_dbl, SR_sig, SR_i)
!
      call alloc_double_numbering(mesh%ele%numele, org_iele_dbl)
      call double_numbering_4_element(mesh%ele, ele_comm,               &
     &                                org_iele_dbl, SR_sig, SR_i)
!
!      call const_ele_trans_tbl_for_repart                               &
!     &   (mesh%node, mesh%ele, part_tbl, new_ids_on_org%irank, ele_tbl)
!      call check_element_transfer_tbl(mesh%ele, ele_tbl)
!
!      call trim_overlapped_ele_by_repart                                &
!     &   (mesh, org_iele_dbl, ele_tbl, new_numele, SR_sig, SR_i)
!
!      new_numele = ele_tbl%ntot_import
      new_numele = ref_ele%numele
      call const_reparition_ele_connect_2(mesh%ele, ele_tbl,            &
     &    new_ids_on_org, org_iele_dbl, new_iele_dbl, new_inod_dbl,     &
     &    new_numele, new_comm, new_node, new_ele_comm, new_ele,        &
     &    SR_sig, SR_i, SR_il, ref_ele)
!
      call dealloc_double_numbering(org_iele_dbl)
!
      end subroutine s_const_repart_ele_connect_2
!
! ----------------------------------------------------------------------
!
      subroutine const_reparition_ele_connect_2                         &
     &         (ele, ele_tbl, new_ids_on_org, org_iele_dbl,             &
     &          new_iele_dbl, new_inod_dbl, new_numele,                 &
     &          new_comm, new_node, new_ele_comm,                       &
     &          new_ele, SR_sig, SR_i, SR_il, ref_ele)
!
      use search_ext_node_repartition
      use const_repart_mesh_data
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(node_ele_double_number), intent(in) :: new_ids_on_org
      type(node_ele_double_number), intent(in) :: org_iele_dbl
      type(node_ele_double_number), intent(in) :: new_iele_dbl
      type(node_ele_double_number), intent(in) :: new_inod_dbl
!
      type(communication_table), intent(in) :: new_comm, new_ele_comm
      type(node_data), intent(in) :: new_node
      integer(kind = kint), intent(in) :: new_numele
      type(element_data), intent(in) :: ref_ele
!
      type(element_data), intent(inout) :: new_ele
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
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
      call set_repart_element_connect(new_numele, ele, ele_tbl,         &
     &    new_ids_on_org, ie_newdomain, ie_newnod, new_ele,             &
     &    SR_sig, SR_i, SR_il)
!
      call s_search_ext_node_repartition_2(ele, ele_tbl,                &
     &    org_iele_dbl, new_iele_dbl, new_inod_dbl, ie_newdomain,       &
     &    new_comm, new_node, new_ele_comm, new_ele, SR_sig, SR_i, ref_ele)
      deallocate(ie_newnod, ie_newdomain)
!
      end subroutine const_reparition_ele_connect_2
!
! ----------------------------------------------------------------------
!
      subroutine s_search_ext_node_repartition_2                        &
     &         (ele, ele_tbl, org_iele_dbl, new_iele_dbl, new_inod_dbl, &
     &          ie_newdomain, new_comm, new_node, new_ele_comm,         &
     &          new_ele, SR_sig, SR_i, ref_ele)
!
      use t_para_double_numbering
      use t_repart_double_numberings
      use calypso_SR_type
      use solver_SR_type
      use search_from_list
      use select_copy_from_recv
      use const_repart_ele_connect
      use quicksort
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(communication_table), intent(in) :: new_comm, new_ele_comm
      type(node_data), intent(in) :: new_node
      type(node_ele_double_number), intent(in) :: org_iele_dbl
      type(node_ele_double_number), intent(in) :: new_iele_dbl
      type(node_ele_double_number), intent(in) :: new_inod_dbl
!
      integer(kind = kint), intent(in)                                  &
     &              :: ie_newdomain(ele%numele,ele%nnod_4_ele)
      type(element_data), intent(in) :: ref_ele
!
      type(element_data), intent(inout) :: new_ele
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint), allocatable :: inod_recv(:)
      integer(kind = kint), allocatable :: icount_node(:)
!      integer(kind = kint), allocatable :: item_import_recv(:)
!
      integer(kind = kint), allocatable :: iele_org_local(:)
      integer(kind = kint), allocatable :: iele_org_domain(:)
      integer(kind = kint), allocatable :: ie_domain_recv(:,:)
!
      integer(kind = kint), allocatable :: ie_tmp(:,:)
      integer(kind = kint), allocatable :: i4_recv(:)
!
      integer(kind = kint), allocatable :: ie_local(:,:)
      integer(kind = kint), allocatable :: irank_e(:,:)
!
      integer(kind = kint), allocatable :: num_rev_import_recv(:)
      integer(kind = kint), allocatable :: istack_rev_import_recv(:)
      integer(kind = kint), allocatable :: irank_import_recv(:)
      integer(kind = kint), allocatable :: irev_import(:)
      integer(kind = kint) :: namx_import
!
      integer(kind = kint) :: ip, inod, icou, inum, ist, ied, num, knod
      integer(kind = kint) :: iele, k1, jnum, jnod, jst, jed, knum, num_loop
!
!
      allocate(ie_tmp(new_ele%numele,new_ele%nnod_4_ele))
      allocate(i4_recv(ele_tbl%ntot_import))
      allocate(ie_domain_recv(new_ele%numele,new_ele%nnod_4_ele))
!
!$omp parallel workshare
      ie_tmp(1:new_ele%numele,1:new_ele%nnod_4_ele)            &
     &   =  new_ele%ie(1:new_ele%numele,1:new_ele%nnod_4_ele)
!$omp end parallel workshare
      new_ele%ie(new_ele%numele,new_ele%nnod_4_ele) = 0
!
      num_loop = min(new_ele%numele, ele_tbl%ntot_import)
      do k1 = 1, ele%nnod_4_ele
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import, ie_newdomain(1,k1),        &
     &      i4_recv(1), SR_sig, SR_i)
!$omp parallel workshare
        ie_domain_recv(1:num_loop,k1) = i4_recv(1:num_loop)
!$omp end parallel workshare
      end do
!
      allocate(iele_org_local(new_ele%numele))
      allocate(iele_org_domain(new_ele%numele))
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, org_iele_dbl%index(1),       &
     &    i4_recv(1), SR_sig, SR_i)
!$omp parallel workshare
      iele_org_local(1:num_loop) = i4_recv(1:num_loop)
!$omp end parallel workshare
!
      call calypso_SR_type_int(iflag_import_item, ele_tbl,              &
     &    ele%numele, ele_tbl%ntot_import, org_iele_dbl%irank(1),       &
     &    i4_recv(1), SR_sig, SR_i)
!$omp parallel workshare
      iele_org_domain(1:num_loop) = i4_recv(1:num_loop)
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
     &  (new_node%numnod, new_comm, SR_sig, SR_i, inod_recv)
!
!      allocate(item_import_recv(new_comm%ntot_import))
!!$omp parallel do private(jnum,jnod)
!      do jnum = 1, new_comm%ntot_import
!        jnod = new_comm%item_import(jnum)
!        item_import_recv(jnum) = inod_recv(jnod)
!      end do
!!$omp end parallel do
!
      namx_import = maxval(inod_recv)
      allocate(num_rev_import_recv(namx_import))
      allocate(istack_rev_import_recv(0:namx_import))
      istack_rev_import_recv(0:namx_import) = 0
      num_rev_import_recv(1:namx_import) = 0
!
      do jnum = 1, new_comm%ntot_import
        jnod = new_comm%item_import(jnum)
        inod = inod_recv(jnod)
        num_rev_import_recv(inod) = num_rev_import_recv(inod) + 1
      end do
!
      do inod = 1, namx_import
        istack_rev_import_recv(inod) = istack_rev_import_recv(inod-1)   &
     &                                + num_rev_import_recv(inod)
      end do
      allocate(irank_import_recv(new_comm%ntot_import))
      allocate(irev_import(new_comm%ntot_import))
!
      num_rev_import_recv(1:namx_import) = 0
      do ip = 1, new_comm%num_neib
        ist = new_comm%istack_import(ip-1) + 1
        ied = new_comm%istack_import(ip  )
        do jnum = ist, ied
          jnod = new_comm%item_import(jnum)
          inod = inod_recv(jnod)
          jst = istack_rev_import_recv(inod-1)
          num = num_rev_import_recv(inod) + 1
          num_rev_import_recv(inod) = num
          irank_import_recv(jst+num) =  new_comm%id_neib(ip)
          irev_import(jst+num) = jnum
        end do
      end do
!
      do inod = 1, namx_import
        ist = istack_rev_import_recv(inod-1) + 1
        ied = istack_rev_import_recv(inod  )
        if((ied-ist) .gt. 1) then
          call quicksort_w_index(new_comm%ntot_import,                  &
     &        irank_import_recv, ist, ied, irev_import)
        end if
      end do
!
      icou = 0
      do iele = 1, num_loop
!
        do k1 = 1, new_ele%nnod_4_ele
          ip =   ie_domain_recv(iele,k1)
          inod = new_ele%ie(iele,k1)
          if(ip .eq. my_rank) then
            icount_node(inod) = icount_node(inod) + 1
          else
            knod = -1
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
!              jnum = search_from_sorted_data(inod, ist, ied,           &
!     &                          new_comm%ntot_import, item_import_recv)
!
              jst = istack_rev_import_recv(inod-1) + 1
              jed = istack_rev_import_recv(inod  )
              knum = search_from_sorted_data(ip, jst, jed,              &
     &                          new_comm%ntot_import, irank_import_recv)
!              if(jnum .ne. irev_import(knum)) &
!              write(*,*) new_comm%item_import(jnum), ip,               &
!     &                  jst, jed, knum, irev_import(knum),             &
!     &                  new_comm%item_import(irev_import(knum))
              if(knum.ge.jst .and. knum.le.jed) then
                knod = irev_import(knum)
                jnod = new_comm%item_import(knod)
                new_ele%ie(iele,k1) = jnod
                icount_node(jnod) = icount_node(jnod) + 1
              end if
            end if
!
            if(knod .le. 0) then
              new_ele%ie(iele,k1) = 0
              write(*,*) my_rank, 'Node cannot be found for ',         &
     &           new_ele%iele_global(iele), iele, k1, ip, inod,        &
     &           iele_org_local(iele), iele_org_domain(iele)
              icou = icou + 1
            end if
          end if
        end do
      end do
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
!
      do k1 = 1, new_ele%nnod_4_ele
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, SR_sig, SR_i, new_ele%ie(1,k1))
      end do
!
      allocate(ie_local(new_ele%numele,new_ele%nnod_4_ele))
      allocate(irank_e(new_ele%numele,new_ele%nnod_4_ele))
      ie_local = 0
      irank_e = -1
      do k1 = 1, new_ele%nnod_4_ele
        do iele = 1, new_ele%numele
          if(new_iele_dbl%irank(iele) .eq. my_rank) then
            inod = new_ele%ie(iele,k1)
            ie_local(iele,k1) = new_inod_dbl%index(inod)
            irank_e(iele,k1) =  new_inod_dbl%irank(inod)
          end if
        end do
      end do
!
      do k1 = 1, new_ele%nnod_4_ele
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, SR_sig, SR_i, ie_local(1,k1))
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, SR_sig, SR_i, irank_e(1,k1))
      end do
!
      icou = 0
      do iele = 1, new_ele%numele
        if(new_iele_dbl%irank(iele) .eq. my_rank) cycle
!
        do k1 = 1, new_ele%nnod_4_ele
          ip =   irank_e(iele,k1)
          inod = ie_local(iele,k1)
!
          if(ip .eq. my_rank) then
            new_ele%ie(iele,k1) = inod
            icount_node(inod) = icount_node(inod) + 1
          else
            knod = -1
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
!              jnum = search_from_sorted_data(inod, ist, ied,           &
!     &                          new_comm%ntot_import, item_import_recv)
!
              jst = istack_rev_import_recv(inod-1) + 1
              jed = istack_rev_import_recv(inod  )
              knum = search_from_sorted_data(ip, jst, jed,              &
     &                          new_comm%ntot_import, irank_import_recv)
!              if(jnum .ne. irev_import(knum)) &
!              write(*,*) new_comm%item_import(jnum), ip,               &
!     &                  jst, jed, knum, irev_import(knum),             &
!     &                  new_comm%item_import(irev_import(knum))
              if(knum.ge.jst .and. knum.le.jed) then
                knod = irev_import(knum)
                jnod = new_comm%item_import(knod)
                new_ele%ie(iele,k1) = jnod
                icount_node(jnod) = icount_node(jnod) + 1
              end if
            end if
!
            if(knod .le. 0) then
              new_ele%ie(iele,k1) = 0
              write(*,*) my_rank, 'Node cannot be found for ',         &
     &           new_ele%iele_global(iele), iele, k1, ip, inod,        &
     &           iele_org_local(iele), iele_org_domain(iele)
              icou = icou + 1
            end if
          end if
        end do
      end do
!      deallocate(item_import_recv)
      deallocate(irev_import, irank_import_recv)
      deallocate(istack_rev_import_recv, num_rev_import_recv)
      deallocate(i4_recv, ie_domain_recv)
      deallocate(iele_org_local, iele_org_domain, inod_recv)
      deallocate(icount_node)
!
      end subroutine s_search_ext_node_repartition_2
!
! ----------------------------------------------------------------------
!
      end module analyzer_repart_by_volume
