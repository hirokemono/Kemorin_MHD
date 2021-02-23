!analyzer_comm_test.f90
!      module analyzer_comm_test
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_communication_test
!      subroutine analyze_communication_test
!
      module analyzer_comm_test
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
!
      use t_solver_SR
      use t_ctl_data_comm_test
      use t_mesh_data
      use t_belonged_element_4_node
      use t_file_IO_parameter
      use t_control_param_comm_test
      use t_vector_for_solver
      use t_failed_export_list
      use const_element_comm_table
!
      implicit none
!
      character(len=kchara), parameter                                  &
     &      :: comm_test_name = 'comm_test.dat'
!
      type(comm_test_control), save ::  comm_tctl1
      type(comm_test_files_param), save ::  T_files
      type(mesh_data), save :: test_fem
!
      type(communication_table), save :: T_ele_comm
      type(communication_table), save :: T_surf_comm
      type(communication_table), save :: T_edge_comm
!
!>      Structure for communicatiors for solver
      type(vectors_4_solver), save :: v_sol_T
!
      character(len=kchara), parameter :: txt_edge = 'edge'
      character(len=kchara), parameter :: txt_surf = 'surface'
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_communication_test
!
      use parallel_FEM_mesh_init
      use mesh_send_recv_test
      use nod_phys_send_recv
      use mpi_load_mesh_data
      use const_element_comm_tables
      use const_element_comm_table
!
      type(failed_table), save :: fail_tbl_s, fail_tbl_d
      integer :: i, num_in, num_ex
!
      if(my_rank .eq. 0)  write(*,*) 'check commnication tables start'
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
      call elapsed_label_4_ele_comm_tbl
      call elpsed_label_4_comm_test
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 's_input_control_comm_test'
      call s_input_control_comm_test(comm_tctl1, T_files)
!
!  --  read geometry
!
      if(my_rank .eq. 0) iflag_debug = 1
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(T_files%mesh_file_IO, nprocs, test_fem)
!
      do i = 1, test_fem%mesh%nod_comm%num_neib
        num_in = test_fem%mesh%nod_comm%istack_import(i) &
     &          - test_fem%mesh%nod_comm%istack_import(i-1)
        num_ex = test_fem%mesh%nod_comm%istack_export(i) &
     &          - test_fem%mesh%nod_comm%istack_export(i-1)
!        write(50+my_rank,*) 'id_neib:',                                &
!     &      test_fem%mesh%nod_comm%id_neib(i), num_in, num_ex
      end do
!      close(50+my_rank)
!
!  -------------------------------------------
!
      call FEM_comm_initialization(test_fem%mesh, v_sol_T)
      call FEM_mesh_initialization(test_fem%mesh, test_fem%group)
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_tbl'
!      call const_ele_comm_tbl                                           &
!     &   (test_fem%mesh%node, test_fem%mesh%nod_comm,                   &
!     &    T_ele_comm, test_fem%mesh%ele)
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table2'
      call alloc_failed_export(0, fail_tbl_s)
      call const_ele_comm_table2                                        &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm,                   &
     &    T_ele_comm, test_fem%mesh%ele, fail_tbl_s)
      call dealloc_failed_export(fail_tbl_s)
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table2'
      call alloc_failed_export(0, fail_tbl_s)
      call const_surf_comm_table2                                      &
     &   (test_fem%mesh%node, test_fem%mesh%ele,                       &
     &    test_fem%mesh%nod_comm, T_surf_comm,                         &
     &    test_fem%mesh%surf, fail_tbl_s)
!
      call dealloc_failed_export(fail_tbl_s)
!
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table'
      call alloc_failed_export(0, fail_tbl_d)
      call const_edge_comm_table2                                      &
     &   (test_fem%mesh%node, test_fem%mesh%ele,                       &
     &    test_fem%mesh%nod_comm, T_edge_comm,                         &
     &    test_fem%mesh%edge, fail_tbl_d)
      call dealloc_failed_export(fail_tbl_d)
!
!      call calypso_mpi_barrier
!      call calypso_mpi_abort(0, 'manuke')
!
      end subroutine initialize_communication_test
!
! ----------------------------------------------------------------------
!
      subroutine analyze_communication_test
!
      use t_vector_for_solver
      use t_work_for_comm_check
!
      use calypso_mpi
      use mesh_send_recv_test
      use diff_geometory_comm_test
      use write_diff_4_comm_test
      use mesh_send_recv_check
!
      use set_ele_id_4_node_type
      use const_element_comm_tables
!
      integer(kind = kint), parameter :: N12 = 12
!
      type(work_for_comm_check), save :: nod_check
      type(work_for_comm_check), save :: ele_check
      type(work_for_comm_check), save :: surf_check
      type(work_for_comm_check), save :: edge_check
!
!
      call calypso_mpi_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'node_send_recv_test'
      call node_send_recv_test                                          &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm, nod_check)
      call ele_send_recv_test(test_fem%mesh%node, test_fem%mesh%ele,    &
     &    T_ele_comm, ele_check)
      call surf_send_recv_test(test_fem%mesh%node, test_fem%mesh%surf,  &
     &    T_surf_comm, surf_check)
      call edge_send_recv_test(test_fem%mesh%node, test_fem%mesh%edge,  &
     &    T_edge_comm, edge_check)
!
      call output_diff_mesh_comm_test(comm_test_name,                   &
     &    nod_check, ele_check, surf_check, edge_check)
!
      call dealloc_ele_comm_test_IO(nod_check)
      call dealloc_ele_comm_test_IO(ele_check)
      call dealloc_ele_comm_test_IO(surf_check)
      call dealloc_ele_comm_test_IO(edge_check)
!
!
      if (iflag_debug.gt.0) write(*,*) 'node_send_recv4_test'
      call alloc_iccg_int8_vector(test_fem%mesh%node%numnod, v_sol_T)
      call verify_iccgN_vec_type                                        &
     &   (N12, test_fem%mesh%node%numnod, v_sol_T)
      call node_send_recv4_test                                         &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm, N12, v_sol_T)
!
      call dealloc_iccgN_vector(v_sol_T)
      call dealloc_iccg_int8_vector(v_sol_T)
!
      call output_elapsed_times
!
      end subroutine analyze_communication_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_ele_comm_table2                                 &
     &         (node, nod_comm, ele_comm, ele, fail_tbl)
!
      use m_geometry_constants
      use t_para_double_numbering
      use set_ele_id_4_node_type
      use const_element_comm_table
      use const_element_comm_tables
      use find_belonged_process
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(element_data), intent(inout) :: ele
      type(communication_table), intent(inout) :: ele_comm
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
!
      type(parallel_double_numbering) :: inod_dbl
!
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: k_ref(:)
!
      integer(kind = kint) :: iele, i1, i2, i, inod, ip
!
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl)
!
      allocate(ip_ref(ele%numele))
      allocate(k_ref(ele%numele))
      call find_belonged_pe_4_ele(my_rank, node, inod_dbl%ip_home(1),   &
     &                            ele, ip_ref, k_ref)
!
      call set_ele_id_4_node(node, ele, belongs%blng_ele)
      call alloc_x_ref_ele(node, belongs)
      call sort_inod_4_ele_by_position(ione, ele%numele, ele%x_ele,     &
     &    node, belongs%blng_ele, belongs%x_ref_ele)
!
      call const_comm_table_by_connenct2                                &
     &   (txt_surf, ele%numele, ele%nnod_4_ele, ele%ie,                 &
     &    ele%x_ele, node, nod_comm, inod_dbl, ip_ref, k_ref,           &
     &    belongs%blng_ele, ele_comm, fail_tbl)
      call dealloc_double_numbering(inod_dbl)
      deallocate(ip_ref, k_ref)
!
      call dealloc_x_ref_ele(belongs)
      call dealloc_iele_belonged(belongs%blng_ele)
!
      call const_global_element_id(ele_comm, ele)
      call calypso_mpi_barrier
!
      end subroutine const_ele_comm_table2
!
!-----------------------------------------------------------------------
!
      subroutine const_surf_comm_table2                                 &
     &         (node, ele, nod_comm, surf_comm, surf, fail_tbl)
!
      use m_geometry_constants
      use t_para_double_numbering
      use set_ele_id_4_node_type
      use const_element_comm_table
      use const_element_comm_tables
      use find_belonged_process
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(inout) :: surf_comm
      type(surface_data), intent(inout) :: surf
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
!
      type(parallel_double_numbering) :: inod_dbl
!
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: k_ref(:)
!
      integer(kind = kint) :: isurf, i1, i2, i, inod, ip
!
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl)
!
      allocate(ip_ref(surf%numsurf))
      allocate(k_ref(surf%numsurf))
!
      call find_belonged_pe_4_surf(my_rank, node, inod_dbl%ip_home(1),  &
     &                             surf, ip_ref, k_ref)
!
      call set_surf_id_4_node(node, surf, belongs%blng_surf)
      call alloc_x_ref_surf(node, belongs)
      call sort_inod_4_ele_by_position(ione, surf%numsurf, surf%x_surf, &
     &    node, belongs%blng_surf, belongs%x_ref_surf)
!
      call const_comm_table_by_connenct2                                &
     &   (txt_surf, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &    surf%x_surf, node, nod_comm, inod_dbl, ip_ref, k_ref,         &
     &    belongs%blng_surf, surf_comm, fail_tbl)
      call dealloc_double_numbering(inod_dbl)
      deallocate(ip_ref, k_ref)
!
      call dealloc_x_ref_surf(belongs)
      call dealloc_iele_belonged(belongs%blng_surf)
!
      call const_global_surface_id(surf_comm, surf)
!
      end subroutine const_surf_comm_table2
!
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table2                                 &
     &         (node, ele, nod_comm, edge_comm, edge, fail_tbl)
!
      use m_geometry_constants
      use t_para_double_numbering
      use set_ele_id_4_node_type
      use const_element_comm_table
      use const_element_comm_tables
      use find_belonged_process
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
!
      type(communication_table), intent(inout) :: edge_comm
      type(edge_data), intent(inout) :: edge
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
!
      type(parallel_double_numbering) :: inod_dbl
!
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: k_ref(:)
!
      integer(kind = kint) :: iedge
!
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl)
!
      allocate(ip_ref(edge%numedge))
      allocate(k_ref(edge%numedge))
      call find_belonged_pe_4_edge(my_rank, node, inod_dbl%ip_home(1),  &
     &                             edge, ip_ref, k_ref)
!
!
      if(iflag_debug.gt.0) write(*,*) ' set_edge_id_4_node in edge'
      call set_edge_id_4_node(node, edge, belongs%blng_edge)
      call alloc_x_ref_edge(node, belongs)
      call sort_inod_4_ele_by_position(ione, edge%numedge, edge%x_edge, &
     &    node, belongs%blng_edge, belongs%x_ref_edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' const_comm_table_by_connenct2 in edge'
      call const_comm_table_by_connenct2                                &
     &   (txt_edge, edge%numedge, edge%nnod_4_edge, edge%ie_edge,       &
     &    edge%x_edge, node, nod_comm, inod_dbl, ip_ref, k_ref,         &
     &    belongs%blng_edge, edge_comm, fail_tbl)
      call dealloc_double_numbering(inod_dbl)
      deallocate(ip_ref, k_ref)
!
      call dealloc_x_ref_edge(belongs)
      call dealloc_iele_belonged(belongs%blng_edge)
!
      call const_global_edge_id(edge_comm, edge)
!
      end subroutine const_edge_comm_table2
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_node_ele_double_address                            &
     &         (node, ele, nod_comm, ele_comm, inod_dbl, iele_dbl)
!
      use t_para_double_numbering
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
!
      type(parallel_double_numbering), intent(inout) :: inod_dbl
      type(parallel_double_numbering), intent(inout) :: iele_dbl
!
!
      call set_node_double_numbering(node, nod_comm, inod_dbl)
      call set_ele_double_numbering                                     &
     &   (ele, ele_comm, inod_dbl, iele_dbl)
!
      end subroutine set_node_ele_double_address
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_by_connenct2                          &
     &         (txt, numele, nnod_4_ele, ie, x_ele, node, nod_comm,     &
     &          inod_dbl, ip_ref, k_ref, neib_e, e_comm, fail_tbl)
!
      use t_para_double_numbering
      use m_solver_SR
      use reverse_SR_int
      use find_element_comm_table
      use const_global_element_ids
      use make_element_comm_table_SR
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      type(node_data), intent(in) :: node
      type(element_around_node), intent(in) :: neib_e
      type(communication_table), intent(in) :: nod_comm
      type(parallel_double_numbering), intent(in) :: inod_dbl
!
      integer(kind = kint), intent(in) :: ip_ref(numele)
      integer(kind = kint), intent(in) :: k_ref(numele)
!
      type(communication_table), intent(inout) :: e_comm
      type(failed_table), intent(inout) :: fail_tbl
!
      integer(kind = kint), allocatable :: inod_lc_import(:,:)
      integer(kind = kint), allocatable :: ipe_lc_import(:,:)
      integer(kind = kint), allocatable :: inod_lc_export(:,:)
      integer(kind = kint), allocatable :: ipe_lc_export(:,:)
      real(kind = kreal), allocatable :: xe_import(:)
      real(kind = kreal), allocatable :: xe_export(:)
!
!
      e_comm%num_neib = nod_comm%num_neib
      call alloc_neighbouring_id(e_comm)
      call alloc_import_num(e_comm)
      call calypso_mpi_barrier
!
!      write(*,*) 'count_element_import_num2', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+1)
      call count_element_import_num2                                    &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import, numele, ip_ref)
!
      call alloc_import_item(e_comm)
      allocate(inod_lc_import(e_comm%ntot_import,nnod_4_ele))
      allocate(ipe_lc_import(e_comm%ntot_import,nnod_4_ele))
      allocate(xe_import(3*e_comm%ntot_import))
!
!      write(*,*) 'set_element_import_item2', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+3)
      call set_element_import_item2                                     &
     &   (node%numnod, inod_dbl%id_local(1), inod_dbl%ip_home(1),       &
     &    numele, nnod_4_ele, ie,x_ele, ip_ref, k_ref, e_comm%num_neib, &
     &    e_comm%id_neib, e_comm%istack_import, e_comm%item_import,     &
     &    inod_lc_import, ipe_lc_import, xe_import)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+3)
!
      call alloc_export_num(e_comm)
!
!      write(*,*) 'element_num_reverse_SR', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+4)
      call element_num_reverse_SR                                       &
     &   (e_comm%num_neib, e_comm%id_neib, e_comm%num_import, SR_sig1,  &
     &    e_comm%num_export, e_comm%istack_export, e_comm%ntot_export)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+4)
!
      allocate(inod_lc_export(e_comm%ntot_export,nnod_4_ele))
      allocate(ipe_lc_export(e_comm%ntot_export,nnod_4_ele))
      allocate(xe_export(3*e_comm%ntot_export))
!
!      write(*,*) 'element_data_reverse_SR2', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+5)
      call element_data_reverse_SR2                                     &
     &   (nnod_4_ele, e_comm%num_neib, e_comm%id_neib,                  &
     &    e_comm%istack_import, e_comm%istack_export,                   &
     &    inod_lc_import, ipe_lc_import, xe_import,                     &
     &    inod_lc_export, ipe_lc_export, xe_export)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+5)
      deallocate(inod_lc_import, ipe_lc_import, xe_import)
!
      call alloc_export_item(e_comm)
!      write(*,*) 's_set_element_export_item2', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+6)
      call s_set_element_export_item2                                   &
     &   (txt, node%numnod, numele, nnod_4_ele,                         &
     &    x_ele, neib_e%istack_4_node, neib_e%iele_4_node,              &
     &    e_comm%num_neib, e_comm%istack_export,                        &
     &    inod_lc_export, ipe_lc_export, xe_export, e_comm%item_export)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+6)
!
!
      deallocate(inod_lc_export, ipe_lc_export, xe_export)
!
!      write(*,*) 'check_element_position', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+8)
      call check_element_position(txt, numele, x_ele, e_comm)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+8)
!
      end subroutine const_comm_table_by_connenct2
!
!-----------------------------------------------------------------------
!
      subroutine element_data_reverse_SR2                               &
     &         (nnod_4_ele, num_neib_e, id_neib_e,                      &
     &          istack_import_e, istack_export_e,                       &
     &          inod_lc_import, ipe_lc_import, xe_import,               &
     &          inod_lc_export, ipe_lc_export,xe_export)
!
      use m_solver_SR
      use reverse_SR_real
      use reverse_SR_int
      use reverse_SR_int8
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
!
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
      integer(kind = kint), intent(in) :: nnod_4_ele
!
      real(kind = kreal), intent(in)                                    &
     &         :: xe_import(3*istack_import_e(num_neib_e))
      integer(kind = kint), intent(in)                                  &
     &        :: inod_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &        :: ipe_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
!
      real(kind = kreal), intent(inout)                                 &
     &         :: xe_export(3*istack_export_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &        :: ipe_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint) :: ip, k1
!
!      do ip = 1, istack_import_e(num_neib_e)
!        write(*,*) ip, inod_import_e(ip), xe_import(3*ip-2:3*ip)
!      end do
!
!
      call reverse_send_recv_3(num_neib_e, id_neib_e,                   &
     &    istack_import_e, istack_export_e, xe_import,                  &
     &    SR_sig1, xe_export)
!
      do k1 = 1, nnod_4_ele
        call reverse_send_recv_int(num_neib_e, id_neib_e,               &
     &      istack_import_e, istack_export_e, inod_lc_import(1,k1),     &
     &      SR_sig1, inod_lc_export(1,k1))
        call reverse_send_recv_int(num_neib_e, id_neib_e,               &
     &      istack_import_e, istack_export_e, ipe_lc_import(1,k1),      &
     &      SR_sig1, ipe_lc_export(1,k1))
      end do
!
      end subroutine element_data_reverse_SR2
!
!-----------------------------------------------------------------------
!
      subroutine count_element_import_num2(num_neib, id_neib,           &
     &          num_neib_e, id_neib_e, num_import_e, istack_import_e,   &
     &          ntot_import_e, numele, ip_ref)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ip_ref(numele)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(inout) :: id_neib_e(num_neib_e)
      integer(kind = kint), intent(inout) :: ntot_import_e
      integer(kind = kint), intent(inout) :: num_import_e(num_neib_e)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint) :: ip, inum, iele
      integer(kind = kint), allocatable :: num_import_tmp(:)
!
!
      allocate(num_import_tmp(nprocs))
!
!$omp parallel workshare
      num_import_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
      do iele = 1, numele
        ip = ip_ref(iele)
        if(ip .ne. my_rank) then
          num_import_tmp(ip+1) = num_import_tmp(ip+1) + 1
        end if
      end do
!
      istack_import_e(0) = 0
      do inum = 1, num_neib
        ip = id_neib(inum)
        id_neib_e(inum) =    ip
        num_import_e(inum) = num_import_tmp(ip+1)
        istack_import_e(inum) = istack_import_e(inum-1)                 &
     &                         + num_import_e(inum)
      end do
      ntot_import_e = istack_import_e(num_neib)
!
      deallocate(num_import_tmp)
!
      end subroutine count_element_import_num2
!
!-----------------------------------------------------------------------
!
      subroutine set_element_import_item2(numnod, inod_lc, ip_nod,      &
     &          numele, nnod_4_ele, ie, x_ele, ip_ref, k_ref,           &
     &          num_neib_e, id_neib_e, istack_import_e, item_import_e,  &
     &          inod_lc_import, ipe_lc_import, xe_import)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_lc(numnod)
      integer(kind = kint), intent(in) :: ip_nod(numnod)
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: ip_ref(numele)
      integer(kind = kint), intent(in) :: k_ref(numele)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_import_e(istack_import_e(num_neib_e))
      real(kind = kreal), intent(inout)                                 &
     &        :: xe_import(3*istack_import_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &        :: ipe_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint) :: ip, icou, iele
      integer(kind = kint) :: ist, inum, inod
      integer(kind = kint) :: k1
!
      integer(kind = kint), allocatable :: ip_rev_tmp(:)
      integer(kind = kint), allocatable :: num_import_tmp(:)
!
!
      allocate(ip_rev_tmp(nprocs))
      allocate(num_import_tmp(nprocs))
!
!$omp parallel workshare
      ip_rev_tmp(1:nprocs) =     0
      num_import_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
!$omp parallel do private(ip)
      do inum = 1, num_neib_e
        ip = id_neib_e(inum)
        ip_rev_tmp(ip+1) = inum
      end do
!$omp end parallel do
!
      do iele = 1, numele
        ip =   ip_ref(iele)
        if(ip .ne. my_rank) then
          inum = ip_rev_tmp(ip+1)
          num_import_tmp(ip+1) = num_import_tmp(ip+1) + 1
          icou = istack_import_e(inum-1) + num_import_tmp(ip+1)
!
          item_import_e(icou) = iele
        end if
      end do
!
      deallocate(ip_rev_tmp, num_import_tmp)
!
      do icou = 1, istack_import_e(num_neib_e)
        iele = item_import_e(icou)
!
        xe_import(3*icou-2) = x_ele(iele,1)
        xe_import(3*icou-1) = x_ele(iele,2)
        xe_import(3*icou  ) = x_ele(iele,3)
        do k1 = 1, nnod_4_ele
          inod = ie(iele,k1)
          inod_lc_import(icou,k1) = inod_lc(inod)
          ipe_lc_import(icou,k1) =  ip_nod(inod)
        end do
      end do
!
      end subroutine  set_element_import_item2
!
!-----------------------------------------------------------------------
!
      subroutine s_set_element_export_item2                             &
     &         (txt, numnod, numele, nnod_4_ele, x_ele,                 &
     &          iele_stack_4_node, iele_4_node,                         &
     &          num_neib_e, istack_export_e,                            &
     &          inod_lc_export, ipe_lc_export, xe_export,               &
     &          item_export_e)
!
      use calypso_mpi_int
      use quicksort
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &        :: iele_4_node(iele_stack_4_node(numnod))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      real(kind = kreal), intent(in)                                    &
     &        :: xe_export(3*istack_export_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &        :: ipe_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_export_e(istack_export_e(num_neib_e))
!
      integer(kind = kint) :: ip, iflag, icou, num_gl
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, jed, jnum, jele
      integer(kind = kint) :: k1, kk
      real(kind = kreal) :: dist, dist_min
!
      integer(kind = kint) :: inod_sf_lc
      integer(kind = kint) :: n_search(nnod_4_ele)
      integer(kind = kint) :: idx_sort(nnod_4_ele)
!
!
      icou = 0
      do ip = 1, num_neib_e
        ist = istack_export_e(ip-1) + 1
        ied = istack_export_e(ip)
        do inum = ist, ied
          do k1 = 1, nnod_4_ele
            idx_sort(k1) = k1
            inod_sf_lc = inod_lc_export(inum,k1)
            if(ipe_lc_export(inum,k1) .eq. my_rank) then
              n_search(k1) = iele_stack_4_node(inod_sf_lc)              &
     &                      - iele_stack_4_node(inod_sf_lc-1)
            else
              n_search(k1) = 0
            end if
          end do
          call quicksort_w_index                                        &
     &       (nnod_4_ele, n_search, ione, nnod_4_ele, idx_sort)
!
          iflag = 0
          dist_min = 1.0d30
          do k1 = 1, nnod_4_ele
            kk = idx_sort(k1)
            if(ipe_lc_export(inum,kk) .ne. my_rank) cycle
!
            inod = inod_lc_export(inum,kk)
            jst = iele_stack_4_node(inod-1) + 1
            jed = iele_stack_4_node(inod)
            do jnum = jst, jed
              jele = iele_4_node(jnum)
!
              dist = sqrt((x_ele(jele,1)- xe_export(3*inum-2))**2 &
     &                + (x_ele(jele,2) - xe_export(3*inum-1))**2  &
     &                + (x_ele(jele,3) - xe_export(3*inum  ))**2)
!
              if(dist .le. TINY) then
                item_export_e(inum) = jele
                iflag = 1
                exit
              end if
              dist_min = min(dist_min,dist)
            end do
            if(iflag .eq. 1) exit
          end do
          if(iflag .eq. 0) icou = icou + 1
        end do
      end do
!
      call calypso_mpi_barrier
      call calypso_mpi_allreduce_one_int(icou, num_gl, MPI_SUM)
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &   'Failed export by s_set_element_export_item2', num_gl
!
      end subroutine s_set_element_export_item2
!
!-----------------------------------------------------------------------
!
      end module analyzer_comm_test
