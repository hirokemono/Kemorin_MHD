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
      call const_ele_comm_tbl                                           &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm,                   &
     &    T_ele_comm, test_fem%mesh%ele)
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table2'
      call alloc_failed_export(0, fail_tbl_s)
      call const_surf_comm_table2                                       &
     &   (test_fem%mesh%node, test_fem%mesh%ele,   &
     &    test_fem%mesh%nod_comm, T_ele_comm, &
     &    T_surf_comm, test_fem%mesh%surf, fail_tbl_s)
!
      write(*,*) 'fail_tbl_d', fail_tbl_d%num_fail
      call dealloc_failed_export(fail_tbl_s)
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table2'
      call alloc_failed_export(0, fail_tbl_d)
      call const_edge_comm_table2                                       &
     &   (test_fem%mesh%node, test_fem%mesh%nod_comm,                   &
     &    T_edge_comm, test_fem%mesh%edge, fail_tbl_d)
      call dealloc_failed_export(fail_tbl_d)
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
!
       subroutine find_belonged_pe_4_surf                               &
      &         (surf, numnod, ip_node, isurf, nnod_same, ip_ref)
!
       type(surface_data), intent(in) :: surf
       integer(kind = kint), intent(in) :: numnod
       integer(kind = kint), intent(in) :: ip_node(numnod)
       integer(kind = kint), intent(in) :: isurf
!
       integer(kind = kint), intent(inout) :: nnod_same, ip_ref
!
       integer(kind = kint) ::ip1, ip2, ip3, ip4
!
       ip1 = ip_node(surf%ie_surf(isurf,1))
       ip2 = ip_node(surf%ie_surf(isurf,2))
       ip3 = ip_node(surf%ie_surf(isurf,3))
       ip4 = ip_node(surf%ie_surf(isurf,4))
       if(ip1.eq.ip2 .and. ip1.eq.ip3 .and. ip1.eq.ip4) then
         nnod_same = 4
         ip_ref = ip1
       else if(ip2.eq.ip3 .and. ip2.eq.ip4) then
         nnod_same = 3
         ip_ref = ip4
       else if(ip1.eq.ip3 .and. ip1.eq.ip4) then
         nnod_same = 3
         ip_ref = ip1
       else if(ip1.eq.ip2 .and. ip1.eq.ip4) then
         nnod_same = 3
         ip_ref = ip1
       else if(ip1.eq.ip2 .and. ip1.eq.ip3) then
         nnod_same = 2
         ip_ref = ip1
!
       else if(ip1.eq.ip2 .and. ip3.eq.ip4) then
         nnod_same = 2
         ip_ref = min(ip1, ip4)
       else if(ip2.eq.ip3 .and. ip4.eq.ip1) then
         nnod_same = 2
         ip_ref = min(ip1, ip2)
       else if(ip1.eq.ip2) then
         nnod_same = 2
         ip_ref = ip1
       else if(ip2.eq.ip3) then
         nnod_same = 2
         ip_ref = ip2
       else if(ip3.eq.ip4) then
         nnod_same = 2
         ip_ref = ip3
       else if(ip4.eq.ip1) then
         nnod_same = 2
         ip_ref = ip4
       else if(ip1.eq.ip3) then
         nnod_same = 2
         ip_ref = ip1
       else if(ip2.eq.ip4) then
         nnod_same = 2
         ip_ref = ip2
       else
         nnod_same = 1
         ip_ref = min(ip1, ip2)
         ip_ref = min(ip3, ip_ref)
         ip_ref = min(ip4, ip_ref)
       end if
!
       end subroutine find_belonged_pe_4_surf

! ----------------------------------------------------------------------
!
      subroutine const_surf_comm_table2                                 &
     &         (node, ele, nod_comm, ele_comm, surf_comm, surf, fail_tbl)
!
      use m_geometry_constants
      use m_solver_SR
      use set_ele_id_4_node_type
      use const_element_comm_table
      use set_surface_data
      use solver_SR_type
      use reverse_SR_int
      use reverse_SR_real
      use calypso_mpi_int
      use quicksort
      use set_element_export_item
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm, ele_comm
      type(communication_table), intent(inout) :: surf_comm
      type(surface_data), intent(inout) :: surf
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
      type(failed_table), save :: fail_import
!
      integer(kind = kint) :: jnod_ref(4),  jp_ref(4)
      integer(kind = kint) :: jnod_sf(4),  jp_sf(4)
      integer(kind = kint) :: n_search(4), inod_search(4)
      real(kind = kreal) ::dist_min
      integer :: i1, i2, i3, icou, iflag, jsurf, num_gl
      integer :: i, isurf, iele, k, ist, ied, ip, inum, jst, jnum, jnod
      integer(kind = kint) :: iele1, iele2, k_lc1, k_lc2
      integer(kind = kint) :: ie_ele_one(ele%nnod_4_ele)
      integer(kind = kint) :: ie_surf_one(surf%nnod_4_surf)
      integer(kind = kint) :: ie_surf_new(surf%nnod_4_surf)
!
      integer(kind = kint), allocatable :: nneib_tmp(:)
      integer(kind = kint), allocatable :: isteck_neib_tmp(:)
!
      integer(kind = kint), allocatable :: isend_id_import(:,:)
      integer(kind = kint), allocatable :: irecv_ip_export(:,:,:)
      real(kind = kreal), allocatable :: send_xe_import(:)
      real(kind = kreal), allocatable :: recv_xe_export(:)
!
      integer(kind = kint) :: ip_1, ip_2
      integer(kind = kint) :: iflag_sf(4), ip_min
      integer(kind = kint), allocatable :: isurf_lc(:)
      integer(kind = kint), allocatable :: ip_surf(:)
      integer(kind = kint), allocatable :: isf_recv(:)
      integer(kind = kint), allocatable :: ipe_recv(:)
      integer(kind = kint), allocatable :: icou_recv(:)
      integer(kind = kint), allocatable :: ip_ref(:)
      integer(kind = kint), allocatable :: nnod_same(:)
!
      integer(kind = kint), allocatable :: iflag_e(:)
      integer(kind = kint), allocatable :: ipe_ele(:)
!
      integer(kind = kint), allocatable :: iflag_ele(:)
      integer(kind = kint), allocatable :: iflag_fail(:)
      integer(kind = kint), allocatable :: iflag_flip_sf(:)
      integer(kind = kint), allocatable :: ip_fail(:)
      integer(kind = kint), allocatable :: ip_flip_sf(:)
      integer, allocatable :: inod_lc(:), ip_node(:)
      integer, allocatable :: iele_lc(:), ip_ele(:)
      integer, allocatable :: idir_surf_ele(:,:)
      integer, allocatable :: ie_surf_ele(:,:,:), ip_surf_ele(:,:,:)
!
      integer, allocatable :: idir_surf(:,:)
      integer, allocatable :: iflag_surf(:)
      integer, allocatable :: iflag_bound_send(:,:), iflag_bound_recv(:,:)
!
      type(work_4_ele_comm_table) :: wk_comm
!
      allocate(inod_lc(node%numnod))
      allocate(ip_node(node%numnod))
      ip_node(1:node%numnod) = my_rank
      do i = 1, node%numnod
        inod_lc(i) = i
      end do
      call SOLVER_SEND_RECV_int_type(node%numnod, nod_comm, inod_lc(1))
      call SOLVER_SEND_RECV_int_type(node%numnod, nod_comm, ip_node(1))
!
!
      allocate(iele_lc(0:ele%numele))
      allocate(ip_ele(0:ele%numele))
      ip_ele(0) = -1
      do iele = 1, ele%numele
        iele_lc(iele) = iele
        ip_ele(iele) = ip_node(ele%ie(iele,1))
      end do
      call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm,  &
     &                               iele_lc(1))
!
!
      allocate(iflag_fail(surf%numsurf))
      iflag_fail(1:surf%numsurf) = 0
      do isurf = 1, surf%numsurf
        iele1 = surf%iele_4_surf(isurf,1,1)
        iele2 = surf%iele_4_surf(isurf,2,1)
        k_lc1 = surf%iele_4_surf(isurf,1,2)
        k_lc2 = surf%iele_4_surf(isurf,2,2)
        if(iele2 .eq. 0) cycle
!
        if(iele_lc(iele2) .lt. iele_lc(iele1)) then
          iflag_fail(isurf) = 1
        else if(iele_lc(iele2) .eq. iele_lc(iele1)) then
          if(ip_ele(iele2) .lt. ip_ele(iele1)) then
            iflag_fail(isurf) = 1
          end if
        end if
      end do
!      write(*,*) my_rank,  'flip!', sum(iflag_fail)
!      call flip_surface_connenct(node, ele, iflag_fail, surf)
!
!

!
      allocate(isurf_lc(surf%numsurf))
      allocate(ip_surf(surf%numsurf))
      allocate(isf_recv(surf%numsurf))
      allocate(ipe_recv(surf%numsurf))
      allocate(icou_recv(surf%numsurf))
      isurf_lc(1:surf%numsurf) =   0
      ip_surf(1:surf%numsurf) =   -1
      isf_recv(1:surf%numsurf) =   0
      ipe_recv(1:surf%numsurf) =  -1
      icou_recv(1:surf%numsurf) =  0
!
      allocate(iflag_e(ele%numele))
      allocate(ipe_ele(ele%numele))
!
      allocate(nnod_same(surf%numsurf))
      allocate(ip_ref(surf%numsurf))
      ip_ref = -1
!
      surf%interior_surf(1:surf%numsurf) = 0
       do isurf = 1, surf%numsurf
        isurf_lc(isurf) = isurf
        call find_belonged_pe_4_surf                                  &
      &    (surf, node%numnod, ip_node, isurf,   &
      &     nnod_same(isurf), ip_surf(isurf))
        if(ip_surf(isurf) .eq. my_rank ) surf%interior_surf(isurf) = 1
      end do
!
      do isurf = 1, surf%numsurf
      end do
!
      call quicksort_w_index(surf%numsurf, ip_surf,                     &
     &                       ione, surf%numsurf, isurf_lc)
!
      allocate(nneib_tmp(nprocs))
      allocate(isteck_neib_tmp(0:nprocs))
      nneib_tmp(1:nprocs) =       0
      isteck_neib_tmp(0:nprocs) = 0
!
      do isurf = 1, surf%numsurf
        ip = ip_surf(isurf)
        nneib_tmp(ip+1) = nneib_tmp(ip+1) + 1
      end do
      nneib_tmp(my_rank+1) = 0
      do ip = 1, nprocs
        isteck_neib_tmp(ip) = isteck_neib_tmp(ip-1) + nneib_tmp(ip)
      end do
!
      do ip = 1, nprocs
        ist = isteck_neib_tmp(ip-1) + 1
        ied = isteck_neib_tmp(ip)
        if(ied .gt. ist) then
          call quicksort_int(surf%numsurf, isurf_lc, ist, ied)
        end if
      end do
!
      surf_comm%num_neib = nod_comm%num_neib
      call alloc_neighbouring_id(surf_comm)
      do i = 1, surf_comm%num_neib
        surf_comm%id_neib(i) = nod_comm%id_neib(i)
      end do
!
      call alloc_import_num(surf_comm)
      do i = 1, surf_comm%num_neib
        ip = surf_comm%id_neib(i)
        surf_comm%num_import(i) = nneib_tmp(ip+1)
        surf_comm%istack_import(i) = surf_comm%istack_import(i-1)       &
     &                             + surf_comm%num_import(i)
      end do
      surf_comm%ntot_import                                             &
     &       = surf_comm%istack_import(surf_comm%num_neib)
!
      call alloc_element_rev_imports(surf%numsurf,                      &
     &    surf_comm%ntot_export, surf_comm%ntot_import, wk_comm)
      call alloc_import_item(surf_comm)
!
      do i = 1, surf_comm%num_neib
        ip = surf_comm%id_neib(i)
        ist = surf_comm%istack_import(i-1)
        jst = isteck_neib_tmp(ip)
        do inum = 1, surf_comm%num_import(i)
          surf_comm%item_import(ist+inum) = isurf_lc(jst+inum)
        end do
      end do
!
!
      call alloc_export_num(surf_comm)
      call element_num_reverse_SR                                       &
     &   (surf_comm%num_neib, surf_comm%id_neib, surf_comm%num_import,  &
     &    SR_sig1, surf_comm%num_export, surf_comm%istack_export,       &
     &    surf_comm%ntot_export)
!
      call alloc_export_item(surf_comm)
      allocate(isend_id_import(surf_comm%ntot_import,2))
      allocate(irecv_ip_export(surf_comm%ntot_export,4,2))
!
      do k = 1, 4
        do inum = 1, surf_comm%ntot_import
          isurf = surf_comm%item_import(inum)
          isend_id_import(inum,1) = inod_lc(surf%ie_surf(isurf,k))
          isend_id_import(inum,2) = ip_node(surf%ie_surf(isurf,k))
        end do
        call reverse_send_recv_int                                      &
     &     (surf_comm%num_neib, surf_comm%id_neib, &
     &      surf_comm%istack_import, surf_comm%istack_export,           &
     &      isend_id_import(1,1), SR_sig1, irecv_ip_export(1,k,1))
        call reverse_send_recv_int                                      &
     &     (surf_comm%num_neib, surf_comm%id_neib, &
     &      surf_comm%istack_import, surf_comm%istack_export,           &
     &      isend_id_import(1,2), SR_sig1, irecv_ip_export(1,k,2))
      end do
!
      allocate(send_xe_import(3*surf_comm%ntot_import))
      allocate(recv_xe_export(3*surf_comm%ntot_export))
!
      do inum = 1, surf_comm%ntot_import
        isurf = surf_comm%item_import(inum)
        send_xe_import(3*inum-2) = surf%x_surf(isurf,1)
        send_xe_import(3*inum-1) = surf%x_surf(isurf,2)
        send_xe_import(3*inum  ) = surf%x_surf(isurf,3)
      end do
      call reverse_send_recv_3(surf_comm%num_neib, surf_comm%id_neib,   &
     &    surf_comm%istack_import, surf_comm%istack_export,             &
     &    send_xe_import, SR_sig1, recv_xe_export)
!
!
      call set_surf_id_4_node(node, surf, belongs%blng_surf)
      call alloc_x_ref_surf(node, belongs)
      call sort_inod_4_ele_by_position(ione, surf%numsurf, surf%x_surf, &
     &    node, belongs%blng_surf, belongs%x_ref_surf)
!
      call alloc_failed_export(0, fail_import)
      call belonged_surf_id_4_node(node, surf, belongs%host_surf)
      call calypso_mpi_barrier
!
       write(*,*) my_rank, 'ntot_export: ', surf_comm%ntot_export
!
      icou = 0
      do i = 1, surf_comm%num_neib
        ip =  surf_comm%id_neib(i)
        ist = surf_comm%istack_export(i-1) + 1
        ied = surf_comm%istack_export(i)
        do inum = ist, ied
           jnod_ref(1:4) = irecv_ip_export(1,1:4,1)
           jp_ref(1:4) =   irecv_ip_export(1,1:4,2)
!          if(mod(inum,2500) .eq. 0) write(*,*) my_rank, 'inum', inum
          iflag = 0
          dist_min = 1.0d30
!
          do k = 1, 4
            if(irecv_ip_export(inum,k,2) .eq. my_rank) then
              inod_search(k) = irecv_ip_export(inum,k,1)
              n_search(k) = belongs%blng_surf%nele_4_node(inod_search(k))
            else
              inod_search(k) = 0
              n_search(k) =    0
            end if
          end do
!
          call quicksort_w_index(ifour, n_search,                       &
     &                           ione, ifour, inod_search)
!
          do k = 1, 4
            jnod = inod_search(k)
            jst =  belongs%blng_surf%istack_4_node(jnod-1)
            do jnum = 1, n_search(k)
              jsurf = belongs%blng_surf%iele_4_node(jst+jnum)
!
              jnod_sf(1:4) = inod_lc(surf%ie_surf(jsurf,1:4))
              jp_sf(1:4) =   ip_node(surf%ie_surf(jsurf,1:4))
!
              if(jnod_sf(1) .eq. jnod_ref(1)                            &
     &            .and. jp_sf(1) .eq. jp_ref(1)) then
                if(jnod_sf(2) .eq. jnod_ref(2)                          &
     &              .and. jp_sf(2) .eq. jp_ref(2)) then
                  if(jnod_sf(3) .eq. jnod_ref(3)                        &
     &                .and. jp_sf(3) .eq. jp_ref(3)                     &
     &              .and. jnod_sf(4) .eq. jnod_ref(4)                   &
     &                .and. jp_sf(4) .eq. jp_ref(4)) then
                    surf_comm%item_export(inum) = jsurf
                    iflag = 1
                    exit
                  end if
                else if(jnod_sf(2) .eq. jnod_ref(4)                     &
     &              .and. jp_sf(2) .eq. jp_ref(4)) then
                  if(jnod_sf(3) .eq. jnod_ref(3)                        &
     &                .and. jp_sf(3) .eq. jp_ref(3)                     &
     &              .and. jnod_sf(4) .eq. jnod_ref(2)                   &
     &                .and. jp_sf(4) .eq. jp_ref(2)) then
                    surf_comm%item_export(inum) = jsurf
                    iflag = 1
                    exit
                  end if
                end if
              end if
            end do
            if(iflag .gt. 0) exit
          end do
          if(iflag .eq. 0) then
!             write(*,*) 'Missing imported ',                           &
!     &                     trim(txt), ' by external: ',                &
!     &                     my_rank, inum, item_export_e(inum),         &
!     &                     xe_export(3*inum-2:3*inum), dist_min
            icou = icou + 1
          end if
        end do
      end do
       write(*,*) my_rank, 'Failed: ', icou, surf_comm%ntot_export
!
!      call calypso_mpi_allreduce_one_int(icou, num_gl, MPI_SUM)
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &   'Failed export by s_set_element_export_item', num_gl
!
      call dealloc_iele_belonged(belongs%host_surf)
      call dealloc_x_ref_surf(belongs)
      call dealloc_iele_belonged(belongs%blng_surf)
      call calypso_mpi_barrier
      call calypso_mpi_abort(100,'bakabakabaka')
!





      isf_recv(1:surf%numsurf) = isurf_lc(1:surf%numsurf)
      ipe_recv(1:surf%numsurf) =  ip_surf(1:surf%numsurf)
!
       i1 = 0
       i2 = 0
       do isurf = 1, surf%numsurf
         if(ip_ref(isurf) .lt. 0) i1 = i1 + 1
         if(ip_ref(isurf) .eq. my_rank) i1 = i1 + 1
       end do
      write(*,*) my_rank, 'Missing source domain',  i1, i2, surf%numsurf
!
      do k = 1, 6
        iflag_e(1:ele%numele) = 0
        ipe_ele(1:ele%numele) = -1
        do iele = 1, ele%numele
          isurf = abs(surf%isf_4_ele(iele,k))
          iflag_e(iele) = isurf_lc(isurf)
          ipe_ele(iele) =  ip_surf(isurf)
        end do
        call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm, iflag_e)
        call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm, ipe_ele)
        do iele = 1, ele%numele
          if(ip_ele(iele) .eq. my_rank) cycle
          isurf = abs(surf%isf_4_ele(iele,k))
          if(ipe_ele(iele) .ge. 0) then
            if(ipe_recv(isurf) .eq. -1) then
              isf_recv(isurf) =  iflag_e(iele)
              ipe_recv(isurf) =  ipe_ele(iele)
            else if(ipe_ele(iele) .ne. ipe_recv(isurf)) then
              icou_recv(isurf) = 1
            end if
          end if
        end do
      end do
!
      i1 = 0
      i2 = 0
      i3 = 0
      do isurf = 1, surf%numsurf
        if(ip_surf(isurf) .lt. 0)  i1 = i1 + 1
        if(ipe_recv(isurf) .lt. 0) i2 = i2 + 1
        if(icou_recv(isurf) .gt. 0)  i3 = i3 + 1
      end do
!
      write(*,*) my_rank, 'Original after full internal missing',  &
     &   i1, i2, i3
!
      do isurf = 1, surf%numsurf
        if(ipe_recv(isurf) .lt. 0) then
          write(my_rank+80,*) isurf, ip_ref(isurf),   &
     &      'local node_ID', inod_lc(surf%ie_surf(isurf,1:4)),  &
     &      'locale node_PE', ip_node(surf%ie_surf(isurf,1:4)),  &
     &      'local ele_ID', iele_lc(surf%iele_4_surf(isurf,1:2,1)), &
     &      'local ele_PE', ip_ele(surf%iele_4_surf(isurf,1:2,1))
        end if
      end do
      close(my_rank+80)
!
      call calypso_mpi_barrier
      call calypso_mpi_abort(2,'tako')
!
      call check_surface_to_flip                                 &
     &         (ele, ele_comm, surf, iflag_fail)
!
      iflag_fail(1:surf%numsurf) = 0
      do isurf = 1, surf%numsurf
        iele1 = surf%iele_4_surf(isurf,1,1)
        iele2 = surf%iele_4_surf(isurf,2,1)
        k_lc1 = surf%iele_4_surf(isurf,1,2)
        k_lc2 = surf%iele_4_surf(isurf,2,2)
        if(iele2 .eq. 0) cycle
!
        if(iele_lc(iele2) .lt. iele_lc(iele1)) then
          iflag_fail(isurf) = 1
        else if(iele_lc(iele2) .eq. iele_lc(iele1)) then
          if(ip_ele(iele2) .lt. ip_ele(iele1)) then
            iflag_fail(isurf) = 1
          end if
        end if
      end do
      write(*,*) my_rank,  'flip!', sum(iflag_fail)
      call flip_surface_connenct(node, ele, iflag_fail, surf)
!
!
      call check_surface_to_flip                                 &
     &         (ele, ele_comm, surf, iflag_fail)
!
      allocate(iflag_flip_sf(surf%numsurf))
      iflag_flip_sf(1:surf%numsurf) = 0
      call check_flip_surface_4_boundary(ele_comm, ele, surf,     &
     &                          iflag_flip_sf)
!
      i3 = 0
      do isurf = 1, surf%numsurf
        if(iflag_fail(isurf) .gt. 0                  &
     &     .and. iflag_flip_sf(isurf) .gt. 0) i3 = i3 + 1
      end do
      call calypso_mpi_barrier
!
      i2 = 0
      do isurf = 1, surf%numsurf
        if(iflag_fail(isurf) .gt. 0                  &
     &     .and. iflag_flip_sf(isurf) .eq. 0) i2 = i2 + 1
      end do
!
      i1 = 0
      do isurf = 1, surf%numsurf
        if(iflag_fail(isurf) .eq. 0                  &
     &     .and. iflag_flip_sf(isurf) .gt. 0) i1 = i1 + 1
      end do
      write(*,*) my_rank, 'iflag_fail and iflag_flip_sf', i1, i2, i3
!
!
      call calypso_mpi_barrier
!
      call flip_surface_connenct(node, ele, iflag_fail, surf)
      call check_surface_to_flip                                 &
     &         (ele, ele_comm, surf, iflag_fail)
      call check_flip_surface_4_boundary(ele_comm, ele, surf, iflag_flip_sf)
!
      i3 = 0
      do isurf = 1, surf%numsurf
        if(iflag_fail(isurf) .gt. 0                  &
     &     .and. iflag_flip_sf(isurf) .gt. 0) i3 = i3 + 1
      end do
      call calypso_mpi_barrier
!
      i2 = 0
      do isurf = 1, surf%numsurf
        if(iflag_fail(isurf) .gt. 0                  &
     &     .and. iflag_flip_sf(isurf) .eq. 0) i2 = i2 + 1
      end do
!
      i1 = 0
      do isurf = 1, surf%numsurf
        if(iflag_fail(isurf) .eq. 0                  &
     &     .and. iflag_flip_sf(isurf) .gt. 0) i1 = i1 + 1
      end do
      write(*,*) my_rank, 'iflag_fail and iflag_flip_sf', i1, i2, i3
!
      deallocate(iflag_fail)
!
!

      call set_surf_rotation_flag(ele%numele, surf%numsurf,             &
     &    ele%nnod_4_ele, surf%nnod_4_surf, ele%ie, surf%ie_surf,       &
     &    surf%isf_4_ele, surf%isf_rot_ele)
!
      do isurf = 1, surf%numsurf
        if(iflag_flip_sf(isurf) .gt. 0) then
          write(80+my_rank,*) isurf,    &
     &           ip_ele(surf%iele_4_surf(isurf,1:2,1)), &
     &           iele_lc(surf%iele_4_surf(isurf,1:2,1)), &
     &           surf%iele_4_surf(isurf,1:2,2)
        end if
      end do
      close(80+my_rank)
      deallocate(iflag_flip_sf)
!
      call calypso_mpi_barrier
      call calypso_mpi_abort(2,'tako')
!



!
      call set_surf_id_4_node(node, surf, belongs%blng_surf)
      call alloc_x_ref_surf(node, belongs)
      call sort_inod_4_ele_by_position(ione, surf%numsurf, surf%x_surf, &
     &    node, belongs%blng_surf, belongs%x_ref_surf)
!
      call alloc_failed_export(0, fail_import)
      call belonged_surf_id_4_node(node, surf, belongs%host_surf)
      call const_comm_table_by_connenct2                                &
     &   (txt_surf, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &    surf%interior_surf, surf%x_surf, node, nod_comm,              &
     &    belongs%blng_surf, belongs%x_ref_surf, belongs%host_surf,     &
     &    surf_comm, fail_tbl, fail_import)
      call dealloc_iele_belonged(belongs%host_surf)
      call dealloc_x_ref_surf(belongs)
      call dealloc_iele_belonged(belongs%blng_surf)
!
!
      allocate(idir_surf_ele(ele%numele,6))
      allocate(ie_surf_ele(ele%numele,6,4))
      allocate(ip_surf_ele(ele%numele,6,4))
      do i = 1, 6
        do iele = 1, ele%numele
          idir_surf_ele(iele,i) = surf%isf_4_ele(iele,i)
          isurf = abs(surf%isf_4_ele(iele,i))
          do k = 1, 4
            ie_surf_ele(iele,i,k) = inod_lc(surf%ie_surf(isurf,k))
            ip_surf_ele(iele,i,k) = ip_node(surf%ie_surf(isurf,k))
          end do
        end do
        call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm,  &
     &                                 idir_surf_ele(1,i))
        do k = 1, 4
          call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm,  &
     &                                 ie_surf_ele(1,i,k))
          call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm,  &
     &                                 ip_surf_ele(1,i,k))
        end do
      end do
!
      do iele = 1, ele%numele
      if(ip_ele(iele) .eq. 14 .and. iele_lc(iele) .eq. 115804) then
        write(*,*) my_rank, 'sel_A', iele, &
     &   (inod_lc(ele%ie(iele,k)), ip_node(ele%ie(iele,k)),k=1,8)
        do i = 1, 6
          isurf = abs(surf%isf_4_ele(iele,i))
          write(*,*) my_rank, iele, i, 'sel_A', surf%isf_4_ele(iele,i), &
     &       (inod_lc(surf%ie_surf(isurf,k)), &
     &        ip_node(surf%ie_surf(isurf,k)),k=1,4)
        end do
      end if
      if(ip_ele(iele) .eq. 3 .and. iele_lc(iele) .eq. 127881) then
        write(*,*) my_rank, 'sel_B', iele, &
     &   (inod_lc(ele%ie(iele,k)), ip_node(ele%ie(iele,k)),k=1,8)
        do i = 1, 6
          isurf = abs(surf%isf_4_ele(iele,i))
          write(*,*) my_rank, iele, i, 'sel_B', surf%isf_4_ele(iele,i), &
     &       (inod_lc(surf%ie_surf(isurf,k)), &
     &        ip_node(surf%ie_surf(isurf,k)),k=1,4)
        end do
      end if
      end do
      
      write(*,*) i, 'surf%numsurf_iso', surf%numsurf_iso, surf%numsurf
!
      do i = 1, fail_import%num_fail
        isurf = fail_import%fail_comm(i)%item_fail
        iele = surf%iele_4_surf(isurf,1,1)
        k = surf%iele_4_surf(isurf,1,2)
        write(80+my_rank,*) my_rank, 'import' , i,       &
     &                         fail_import%fail_comm(i)%id_failed, &
     &                         fail_import%fail_comm(i)%item_fail, &
     &                         fail_import%fail_comm(i)%dist_fail, &
     &             'ie_surf_lc', inod_lc(surf%ie_surf(isurf,1:4)), &
     &             'ip_node', ip_node(surf%ie_surf(isurf,1:4)), &
     &             'ie_surf_ele', ie_surf_ele(iele,k,1:4), &
     &             'ip_surf_ele', ip_surf_ele(iele,k,1:4), &
     &             'iele_4_surf_1', surf%iele_4_surf(isurf,1:2,1), &
     &             'iele_4_surf_2', surf%iele_4_surf(isurf,1:2,2),  &
     &             'iele_lc', iele_lc(surf%iele_4_surf(isurf,1:2,1)), &
     &             'ip_ele', ip_ele(surf%iele_4_surf(isurf,1:2,1)), &
     &    'idir', surf%isf_4_ele(iele,k)*idir_surf_ele(iele,k)
      end do
      close(80+my_rank)
      deallocate(inod_lc, ip_node, ip_ele, idir_surf_ele)
      deallocate(ie_surf_ele, ip_surf_ele)
      call dealloc_failed_export(fail_import)
!
      call calypso_mpi_barrier
      call calypso_mpi_abort(2,'tako')
!
      call const_global_surface_id2(surf_comm, surf)
!
      end subroutine const_surf_comm_table2
!
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table2                                 &
     &         (node, nod_comm, edge_comm, edge, fail_tbl)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      type(communication_table), intent(inout) :: edge_comm
      type(edge_data), intent(inout) :: edge
      type(failed_table), intent(inout) :: fail_tbl
!
      type(belonged_table), save :: belongs
      type(failed_table), save :: fail_import
!
!
      if(iflag_debug.gt.0) write(*,*) ' set_edge_id_4_node in edge'
      call set_edge_id_4_node(node, edge, belongs%blng_edge)
      call alloc_x_ref_edge(node, belongs)
      call sort_inod_4_ele_by_position(ione, edge%numedge, edge%x_edge, &
     &    node, belongs%blng_edge, belongs%x_ref_edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' belonged_edge_id_4_node in edge'
      call belonged_edge_id_4_node(node, edge, belongs%host_edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' const_comm_table_by_connenct2 in edge'
      call alloc_failed_export(0, fail_import)
      call const_comm_table_by_connenct2                                &
     &    (txt_edge, edge%numedge, edge%nnod_4_edge, edge%ie_edge,      &
     &    edge%interior_edge, edge%x_edge, node, nod_comm,              &
     &    belongs%blng_edge, belongs%x_ref_edge, belongs%host_edge,     &
     &    edge_comm, fail_tbl, fail_import)
      call dealloc_failed_export(fail_import)
!
      call dealloc_iele_belonged(belongs%host_edge)
      call dealloc_x_ref_edge(belongs)
      call dealloc_iele_belonged(belongs%blng_edge)
!
      call const_global_edge_id2(edge_comm, edge)
!
      end subroutine const_edge_comm_table2
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_global_surface_id2(sf_comm, surf)
!
      use const_global_element_ids
!
      type(communication_table), intent(in) :: sf_comm
      type(surface_data), intent(inout) :: surf
!
!
      call alloc_numsurf_stack(nprocs, surf)
!
      call count_number_of_node_stack                                   &
     &  (surf%numsurf, surf%istack_numsurf)
      call count_number_of_node_stack                                   &
     &  (surf%internal_surf, surf%istack_intersurf)
!
      call set_global_ele_id                                            &
     &   (txt_surf, surf%numsurf, surf%istack_intersurf,                &
     &    surf%interior_surf, sf_comm, surf%isurf_global)
!
      call dealloc_numsurf_stack(surf)
!
      end subroutine const_global_surface_id2
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_edge_id2(ed_comm, edge)
!
      use const_global_element_ids
!
      type(communication_table), intent(in) :: ed_comm
      type(edge_data), intent(inout) :: edge
!
!
      call alloc_numedge_stack(nprocs, edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' count_number_of_node_stack in edge'
      call count_number_of_node_stack                                   &
     &  (edge%numedge, edge%istack_numedge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' count_number_of_node_stack in internal edge'
      call count_number_of_node_stack                                   &
     &  (edge%internal_edge, edge%istack_interedge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' set_global_ele_id in edge'
      call set_global_ele_id                                            &
     &   (txt_edge, edge%numedge, edge%istack_interedge,                &
     &    edge%interior_edge, ed_comm, edge%iedge_global)
!
      end subroutine const_global_edge_id2
!
!  ---------------------------------------------------------------------
!
      subroutine const_comm_table_by_connenct2                          &
     &         (txt, numele, nnod_4_ele, ie, internal_flag, x_ele,      &
     &          node, nod_comm, neib_e, x_ref_ele, host,                &
     &          e_comm, fail_tbl, fail_import)
!
      use m_solver_SR
      use reverse_SR_int
      use find_element_comm_table
      use const_global_element_ids
      use set_element_export_item
      use make_element_comm_table_SR
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      type(node_data), intent(in) :: node
      type(element_around_node), intent(in) :: host
      type(element_around_node), intent(in) :: neib_e
      type(communication_table), intent(in) :: nod_comm
      real(kind = kreal), intent(in)                                    &
     &           :: x_ref_ele(neib_e%istack_4_node(node%numnod))
!
      type(communication_table), intent(inout) :: e_comm
      type(failed_table), intent(inout) :: fail_tbl
      type(failed_table), intent(inout) :: fail_import
!
      type(work_4_ele_comm_table) :: wk_comm
!
!
      e_comm%num_neib = nod_comm%num_neib
      call alloc_neighbouring_id(e_comm)
      call alloc_import_num(e_comm)
!
!      write(*,*) 'count_element_import_num', my_rank
      call count_element_import_num(node%numnod, host%istack_4_node,    &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import)
!
      call alloc_element_rev_imports(node%numnod,                       &
     &    nod_comm%ntot_export, e_comm%ntot_import, wk_comm)
      call alloc_import_item(e_comm)
!
!      write(*,*) 'local_node_id_reverse_SR', my_rank
      call local_node_id_reverse_SR                                     &
     &   (node%numnod, nod_comm%num_neib, nod_comm%id_neib,             &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    SR_sig1, wk_comm%item_local, wk_comm%inod_local)
!
!      write(*,*) 'set_element_import_item', my_rank
      call set_element_import_item(node%numnod, node%internal_node,     &
     &    numele, nnod_4_ele, ie, node%inod_global, x_ele,              &
     &    host%istack_4_node, host%iele_4_node, wk_comm%inod_local,     &
     &    nod_comm%num_neib, nod_comm%istack_import,                    &
     &    nod_comm%item_import, e_comm%num_neib,                        &
     &    e_comm%istack_import, e_comm%item_import,                     &
     &    wk_comm%inod_import_e, wk_comm%inod_import_l,                 &
     &    wk_comm%xe_import)
!
      call alloc_export_num(e_comm)
!
!      write(*,*) 'element_num_reverse_SR', my_rank
      call element_num_reverse_SR                                       &
     &   (e_comm%num_neib, e_comm%id_neib, e_comm%num_import, SR_sig1,  &
     &    e_comm%num_export, e_comm%istack_export, e_comm%ntot_export)
!
      call alloc_element_rev_exports(e_comm%ntot_export, wk_comm)
      call alloc_export_item(e_comm)
!
!      write(*,*) 'element_data_reverse_SR', my_rank
      call element_data_reverse_SR(e_comm%num_neib, e_comm%id_neib,     &
     &    e_comm%istack_import, e_comm%istack_export,                   &
     &    wk_comm%inod_import_e, wk_comm%inod_import_l,                 &
     &    wk_comm%xe_import, wk_comm%inod_export_e,                     &
     &    wk_comm%inod_export_l, wk_comm%xe_export)
!
!      write(*,*) 'set_element_export_item', my_rank
      call s_set_element_export_item(txt, node%numnod, numele,          &
     &    internal_flag, x_ele, neib_e%istack_4_node,                   &
     &    neib_e%iele_4_node, x_ref_ele, nod_comm%num_neib,             &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%istack_export,                        &
     &    wk_comm%inod_export_l, wk_comm%xe_export, e_comm%item_export)
!
      call element_export_item_in_ext                                   &
     &   (txt, node%numnod, numele, node%inod_global,                   &
     &    internal_flag, x_ele, neib_e%istack_4_node,                   &
     &    neib_e%iele_4_node, x_ref_ele, nod_comm%num_neib,             &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    e_comm%num_neib, e_comm%istack_export,                        &
     &    wk_comm%inod_export_e, wk_comm%xe_export, e_comm%item_export, &
     &    fail_tbl)
!
      call dealloc_element_rev_exports(wk_comm)
      call dealloc_element_rev_imports(wk_comm)
!
!
!      write(*,*) 'check_element_position2', my_rank
      call check_element_position2(txt, numele, x_ele, e_comm, fail_import)
!
      end subroutine const_comm_table_by_connenct2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_element_position2(txt, nele, x_ele, e_comm, fail_import)
!
      use t_comm_table
      use calypso_mpi_int
      use solver_SR_type
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(in)  :: x_ele(nele,3)
!
      type(communication_table), intent(in) :: e_comm
      type(failed_table), intent(inout) :: fail_import
!
      type(failed_item) :: fail_comm_in
!
      real(kind = kreal) :: dx, dy, dz
      real(kind = kreal), allocatable :: x_test(:)
      integer(kind = kint) :: iele, iflag, iflag_gl
      integer(kind = kint) :: inum, ip, ist, ied, irank_recv
!
!
      if(i_debug .gt. 0) write(*,*) 'Number of  ', trim(txt),           &
     &           ' for ', my_rank, ': ',   nele, size(x_ele,1)
      allocate(x_test(-2:3*nele))
!
!$omp parallel do
      do iele = 1, nele
        x_test(3*iele-2) = x_ele(iele,1)
        x_test(3*iele-1) = x_ele(iele,2)
        x_test(3*iele  ) = x_ele(iele,3)
      end do
!$omp end parallel do
!
!$omp parallel do private(inum,iele)
      do inum = 1, e_comm%ntot_import
        iele = e_comm%item_import(inum)
        x_test(3*iele-2) = 1.e30
        x_test(3*iele-1) = 1.e30
        x_test(3*iele  ) = 1.e30
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_3_type(nele, e_comm, x_test(1))
!
      iflag = 0
      do iele = 1, nele
        dx = x_test(3*iele-2) - x_ele(iele,1)
        dy = x_test(3*iele-1) - x_ele(iele,2)
        dz = x_test(3*iele  ) - x_ele(iele,3)
        if(     (abs(dx) .ge. TINY)  .or. (abs(dy) .ge. TINY)           &
     &     .or. (abs(dz) .ge. TINY)) then
          irank_recv = -1
          do ip = 1, e_comm%num_neib
            ist = e_comm%istack_import(ip-1) + 1
            ied = e_comm%istack_import(ip)
            do inum = ist, ied
              if(iele .eq. e_comm%item_import(inum)) then
                irank_recv = e_comm%id_neib(ip)
                write(*,*) 'Hit!' , inum
                exit
              end if
            end do
            if(irank_recv .ge. 0) exit
          end do
          call set_failed_export(irank_recv, iele, sqrt(dx*dx+dy*dy+dz*dz),   &
     &                           fail_comm_in)
          call append_failed_export(fail_comm_in, fail_import)
          write(*,*) 'wrong ', trim(txt), ' position at: ',             &
     &         my_rank, iele, x_ele(iele,1:3), dx, dy, dz
        end if
      end do
!
      call calypso_mpi_allreduce_one_int(iflag, iflag_gl, MPI_SUM)
      if(iflag_gl .eq. 0 .and. my_rank .eq. 0) write(*,*)               &
     &     trim(txt), ' position is successfully syncronizad'
!
      deallocate(x_test)
!
      end subroutine check_element_position2
!
!-----------------------------------------------------------------------
!
      subroutine check_flip_surface_4_boundary                          &
     &         (ele_comm, ele, surf, iflag_flip_sf)
!
      use m_solver_SR
      use reverse_SR_int
!
      type(communication_table), intent(in) :: ele_comm
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf

      integer(kind = kint), intent(inout)                               &
     &                     :: iflag_flip_sf(surf%numsurf)
!
      integer :: inum, isurf, iele, k, ist, ied, ip
!
      integer, allocatable :: iflag_surf(:)
      integer, allocatable :: iflag_bound_send(:,:), iflag_bound_recv(:,:)
!
!
      allocate(iflag_surf(surf%numsurf))
!
      iflag_surf(1:surf%numsurf) = -1
      do inum = 1, surf%numsurf_iso
        isurf = surf%isf_isolate(inum)
        iflag_surf(isurf) = my_rank
      end do

      if(my_rank .eq. 19) write(*,*) &
    &      'iflag_surf init', iflag_surf(135069)
!
      allocate(iflag_bound_send(ele_comm%istack_import(ele_comm%num_neib),6))
      allocate(iflag_bound_recv(ele_comm%istack_export(ele_comm%num_neib),6))
      do k = 1, 6
        iflag_bound_send(1:ele_comm%istack_import(ele_comm%num_neib),k) = -1
        do inum = 1, ele_comm%istack_import(ele_comm%num_neib)
          iele = ele_comm%item_import(inum)
          isurf = surf%isf_4_ele(iele,k)
          if(isurf .ge. 0) then
            iflag_bound_send(inum,k) = iflag_surf(isurf)
          end if
        end do
      end do
!
      iflag_bound_recv = -1
      do k = 1, 6
        call reverse_send_recv_int  &
     &     (ele_comm%num_neib, ele_comm%id_neib,                        &
     &      ele_comm%istack_import, ele_comm%istack_export,             &
     &      iflag_bound_send(1,k), SR_sig1, iflag_bound_recv(1,k))
      end do
!
      iflag_flip_sf(1:surf%numsurf) = 0
      do ip = 1, ele_comm%num_neib
        ist = ele_comm%istack_export(ip-1) + 1
        ied = ele_comm%istack_export(ip)
        do k = 1, 6
          do inum = ist, ied
            iele = ele_comm%item_export(inum)
            if(surf%isf_4_ele(iele,k) .lt. 0    &
     &             .and. iflag_bound_recv(inum,k).ge.0) then
              isurf = abs(surf%isf_4_ele(iele,k))
              iflag_surf(isurf) = iflag_bound_recv(inum,k)
              iflag_flip_sf(isurf) = 1
            end if
          end do
        end do
      end do
!
     if(my_rank .eq. 19) write(*,*) &
    &      'iflag_flip_sf_sampleA', iflag_surf(135069),  iflag_flip_sf(135069)
!
      write(*,*) my_rank, 'num. of flip for boundary',                  &
     &          sum(iflag_flip_sf), surf%numsurf
!
      deallocate(iflag_bound_send, iflag_bound_recv)
      deallocate(iflag_surf)

      end subroutine check_flip_surface_4_boundary
!
!-----------------------------------------------------------------------
!
      subroutine check_surface_to_flip                                 &
     &         (ele, ele_comm, surf, iflag_fail)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
      use solver_SR_type
      use m_solver_SR
      use reverse_SR_int
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: ele_comm
      type(surface_data), intent(inout) :: surf
      integer(kind = kint), intent(inout) :: iflag_fail(surf%numsurf)
!
      integer ::  inum, isurf, iele, k, icou1, icou2, icou3
!
      integer(kind = kint), allocatable :: idir_surf(:,:)
!
!
      allocate(idir_surf(ele%numele,6))
      do k = 1, 6
        do iele = 1, ele%numele
          idir_surf(iele,k) = surf%isf_4_ele(iele,k)
        end do
!
        call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm,            &
     &                                 idir_surf(1,k))
      end do
!
      iflag_fail(1:surf%numsurf) = 0
      do k = 1, 6
        do iele = 1, ele%numele
          isurf = abs(surf%isf_4_ele(iele,k))
          if(idir_surf(iele,k)*surf%isf_4_ele(iele,k) .lt. 0)           &
     &             iflag_fail(isurf) = iflag_fail(isurf) + 1
        end do
      end do
      deallocate(idir_surf)
!
      icou2 = 0
      icou1 = 0
      do isurf = 1, surf%numsurf
        if(iflag_fail(isurf) .eq. 2) icou2 = icou2 + 1
        if(iflag_fail(isurf) .eq. 1) icou1 = icou1 + 1
      end do
!
      icou3 = 0
      do inum = 1, surf%numsurf_iso
        isurf = surf%isf_isolate(inum)
        if(iflag_fail(isurf) .eq. 1) then
          icou3 = icou3 + 1
          icou1 = icou1 - 1
        end if
      end do
!
      write(*,*) my_rank, 'num. of inconsist', icou1, icou2, icou3, surf%numsurf
!
      end subroutine check_surface_to_flip
!
!-----------------------------------------------------------------------
!
      subroutine flip_surface_connenct(node, ele, iflag_flip, surf)
!
      use m_geometry_constants
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(surface_data), intent(inout) :: surf
      integer(kind = kint), intent(in) :: iflag_flip(surf%numsurf)
!
      integer :: isurf, k
      integer(kind = kint) :: iele1, iele2, k_lc1, k_lc2
      integer(kind = kint) :: ie_surf_new(surf%nnod_4_surf)
!
!
!
      do isurf = 1, surf%numsurf
        if(iflag_flip(isurf) .eq. 0) cycle
!
        iele1 = surf%iele_4_surf(isurf,1,1)
        iele2 = surf%iele_4_surf(isurf,2,1)
        k_lc1 = surf%iele_4_surf(isurf,1,2)
        k_lc2 = surf%iele_4_surf(isurf,2,2)
        if(iele2 .le. 0) cycle
!
        if(surf%nnod_4_surf .eq. num_quad_sf) then
          do k = 1, num_quad_sf
            ie_surf_new(k) = ele%ie(iele2,node_on_sf_8(k,k_lc2))
          end do
        else if(surf%nnod_4_surf .eq. num_lag_sf) then
          do k = 1, num_lag_sf
            ie_surf_new(k) = ele%ie(iele2,node_on_sf_8(k,k_lc2))
          end do
        else
          do k = 1, num_linear_sf
            ie_surf_new(k) = ele%ie(iele2,node_on_sf_4(k,k_lc2))
          end do
        end if
!
        surf%ie_surf(isurf,1:surf%nnod_4_surf)                  &
     &        = ie_surf_new(1:surf%nnod_4_surf)
        surf%isf_4_ele(iele1,k_lc1) = -isurf
        surf%isf_4_ele(iele2,k_lc2) =  isurf
!
        surf%iele_4_surf(isurf,1,1) = iele2
        surf%iele_4_surf(isurf,2,1) = iele1
        surf%iele_4_surf(isurf,1,2) = k_lc2
        surf%iele_4_surf(isurf,2,2) = k_lc1
!
        if(surf%ie_surf(isurf,1) .le. node%internal_node) then
          surf%interior_surf(isurf) = 1
        else
          surf%interior_surf(isurf) = 0
        end if
      end do
!
      end subroutine flip_surface_connenct
!
!-----------------------------------------------------------------------
!
      end module analyzer_comm_test
