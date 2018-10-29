!analyzer_mesh_test.f90
!
!      module analyzer_mesh_test
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_mesh_test
!      subroutine analyze_mesh_test
!
!..................................................
!
      module analyzer_mesh_test
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
      use m_work_time
!
      implicit none
!
      type(mesh_data), save :: fem_T
      type(element_geometry), save :: ele_mesh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_mesh_test
!
      use m_array_for_send_recv
      use m_default_file_prefix
      use t_ctl_data_mesh_test
      use t_control_param_mesh_test
!
      use copy_mesh_structures
      use set_element_data_4_IO
      use set_surface_data_4_IO
      use set_edge_data_4_IO
      use element_file_IO
      use check_jacobians
      use int_volume_of_domain
      use set_surf_grp_vectors
      use check_surface_groups
      use set_normal_vectors
      use set_edge_vectors
      use mesh_file_IO
      use nod_phys_send_recv
      use sum_normal_4_surf_group
      use set_parallel_file_name
!
      use mpi_load_mesh_data
      use const_jacobians_3d
      use parallel_FEM_mesh_init
      use load_element_mesh_data
      use output_test_mesh
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
!
!>     Stracture for Jacobians
      type(jacobians_type), save :: jacobians_T
      type(shape_finctions_at_points), save :: spfs_T
!
      type(mesh_test_control), save :: mesh_tctl1
      type(mesh_test_files_param) ::  T_meshes
      type(mesh_geometry) :: mesh_IO
      type(surf_edge_IO_file) :: ele_mesh_IO
!
!
      num_elapsed = 11
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                  '
      elapse_labels(2) = 'const_element_comm_tbls2'
!
      elapse_labels(3) = 'const_ele_comm_tbl2'
      elapse_labels(4) = 'const_global_element_id'
      elapse_labels(5) = 'const_surf_comm_table'
      elapse_labels(6) = 'start_elapsed_time'
      elapse_labels(7) = 'const_edge_comm_table'
      elapse_labels(8) = 'const_global_edge_id'
!
      elapse_labels(9) = 'const_comm_table_by_connenct2'
      elapse_labels(10) = 'set_element_export_item2'
      elapse_labels(11) = 'search_target_element3'
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
      call read_control_4_mesh_test(mesh_tctl1)
!
      call set_ctl_params_4_test_mesh(mesh_tctl1, T_meshes)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh                                               &
     &   (T_meshes%mesh_file_IO, nprocs, fem_T, ele_mesh)
!
!  -------------------------------
!
      call start_elapsed_time(1)
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_init_with_IO2'
      call FEM_mesh_init_with_IO2(T_meshes%iflag_output_SURF,           &
     &    T_meshes%mesh_file_IO, fem_T%mesh, fem_T%group, ele_mesh)
      call end_elapsed_time(1)
      call calypso_MPI_barrier
      return
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'pick_surface_group_geometry'
      call pick_surface_group_geometry(ele_mesh%surf,                   &
     &   fem_T%group%surf_grp, fem_T%group%tbls_surf_grp,               &
     &   fem_T%group%surf_grp_geom)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
      allocate(jacobians_T%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (fem_T%mesh%ele%nnod_4_ele, jacobians_T%g_FEM)
      call const_jacobian_volume_normals(my_rank, nprocs,               &
     &    fem_T%mesh, ele_mesh%surf, fem_T%group, spfs_T, jacobians_T)
!
      if (iflag_debug.gt.0) write(*,*) 'const_edge_vector'
      call const_edge_vector(my_rank, nprocs,                           &
     &    fem_T%mesh%node, ele_mesh%edge, spfs_T%spf_1d, jacobians_T)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector_spherical'
      call s_cal_normal_vector_spherical(ele_mesh%surf)
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector_cylindrical'
      call s_cal_normal_vector_cylindrical(ele_mesh%surf)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_spherical'
      call s_cal_edge_vector_spherical(ele_mesh%edge)
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_cylindrical'
      call s_cal_edge_vector_cylindrical(ele_mesh%edge)
!
!  ---------------------------------------------
!     output element, surface, edge data
!  ---------------------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'output_test_mesh_informations'
      call output_test_mesh_informations                               &
     &   (my_rank, fem_T%mesh, ele_mesh, mesh_IO, ele_mesh_IO)
!
      end subroutine initialize_mesh_test
!
! ----------------------------------------------------------------------
!
      subroutine analyze_mesh_test
!
!
      call output_elapsed_times
      call calypso_MPI_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_mesh_test
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine FEM_mesh_init_with_IO2                                 &
     &         (iflag_output_SURF, mesh_file, mesh, group, ele_mesh)
!
      use t_file_IO_parameter
      use t_read_mesh_data
!
      use m_array_for_send_recv
      use m_phys_constants
!
      use nod_phys_send_recv
      use node_monitor_IO
      use const_mesh_information
      use const_element_comm_tables
      use mesh_file_name_by_param
      use parallel_FEM_mesh_init
!
      integer(kind = kint), intent(in) :: iflag_output_SURF
      type(field_io_params), intent(in) :: mesh_file
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
      type(surf_edge_IO_file) :: ele_mesh_IO
      integer(kind = kint) :: iflag_ele_mesh
!
!
      iflag_ele_mesh =  check_exist_ele_mesh(mesh_file, izero)          &
     &                + check_exist_surf_mesh(mesh_file, izero)         &
     &                + check_exist_edge_mesh(mesh_file, izero)
      if(iflag_ele_mesh .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 'mpi_load_element_surface_edge'
        call mpi_load_element_surface_edge                              &
     &     (mesh_file, mesh, ele_mesh, ele_mesh_IO)
      end if
      call calypso_mpi_barrier
!
!  -------------------------------
!      if (iflag_debug.gt.0) write(*,*) 'set_local_nod_4_monitor'
!      call set_local_nod_4_monitor(mesh, group)
!
!  ------  In itialize data communication for FEM data
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(mesh)
!
!  -----    construct geometry informations
!
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
      if(iflag_ele_mesh .eq. 0) return
!
      call start_elapsed_time(2)
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls2'
      call const_element_comm_tbls2(mesh, ele_mesh)
      call end_elapsed_time(2)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_whole_num_of_elements(mesh%ele)
      end if
!
      if(iflag_ele_mesh .ne. 0 .and. iflag_output_SURF .gt. 0) then
        call mpi_output_element_surface_edge                            &
     &         (mesh_file, mesh, ele_mesh, ele_mesh_IO)
      end if
!
!      call deallocate_surface_geom_type(ele_mesh%surf)
!      call dealloc_edge_geometory(ele_mesh%edge)
!
      end subroutine FEM_mesh_init_with_IO2
!
!-----------------------------------------------------------------------
!
      subroutine const_element_comm_tbls2(mesh, ele_mesh)
!
      use set_ele_id_4_node_type
      use t_belonged_element_4_node
      use const_element_comm_tables
!
      type(mesh_geometry), intent(inout) :: mesh
      type(element_geometry), intent(inout) :: ele_mesh
!
      type(belonged_table) :: blng_tbl
!
!
      if(iflag_debug.gt.0) write(*,*) 'const_global_numnod_list'
      call const_global_numnod_list(mesh%node)
!
      if(iflag_debug.gt.0) write(*,*) 'find_position_range'
      call find_position_range(mesh%node)
!
      if(iflag_debug.gt.0) write(*,*) 'const_ele_comm_tbl2'
      call start_elapsed_time(3)
      call const_ele_comm_tbl2(mesh%node, mesh%ele, mesh%nod_comm,      &
     &    blng_tbl, ele_mesh%ele_comm)
      call end_elapsed_time(3)
      call calypso_mpi_barrier
!
      call start_elapsed_time(4)
      if(i_debug.gt.0) write(*,*)' const_global_element_id', my_rank
      call const_global_element_id(mesh%ele, ele_mesh%ele_comm)
      call end_elapsed_time(4)
      call calypso_mpi_barrier
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
      call start_elapsed_time(5)
      call const_surf_comm_table(mesh%node, mesh%nod_comm,              &
     &    ele_mesh%surf, blng_tbl, ele_mesh%surf_comm)
      call end_elapsed_time(5)
!
      if(iflag_debug.gt.0) write(*,*)' const_global_surface_id'
      call start_elapsed_time(6)
      call const_global_surface_id(ele_mesh%surf, ele_mesh%surf_comm)
      call end_elapsed_time(6)
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table'
      call start_elapsed_time(7)
      call const_edge_comm_table(mesh%node, mesh%nod_comm,              &
     &    ele_mesh%edge, blng_tbl, ele_mesh%edge_comm)
      call end_elapsed_time(7)
!
      if(iflag_debug.gt.0) write(*,*)' const_global_edge_id'
      call start_elapsed_time(8)
      call const_global_edge_id(ele_mesh%edge, ele_mesh%edge_comm)
      call end_elapsed_time(8)
!
      end subroutine const_element_comm_tbls2
!
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_tbl2                                    &
     &         (node, ele, nod_comm, belongs, ele_comm)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(belonged_table), intent(inout) :: belongs
      type(communication_table), intent(inout) :: ele_comm
!
      character(len=kchara), parameter :: txt = 'element'
!
!
      call set_ele_id_4_node(node, ele, belongs%blng_ele)
      call alloc_x_ref_ele(node, belongs)
      call sort_inod_4_ele_by_position(ione, ele%numele, ele%x_ele,     &
     &    node, belongs%blng_ele, belongs%x_ref_ele)
!
      call belonged_ele_id_4_node(node, ele, belongs%host_ele)
      call calypso_mpi_barrier
!
      if(i_debug.gt.0) write(*,*)' const_comm_table_by_connenct2',      &
     &                            my_rank
      call start_elapsed_time(9)
      call const_comm_table_by_connenct2                                &
     &   (txt, ele%numele, ele%nnod_4_ele, ele%ie,                      &
     &    ele%interior_ele, ele%x_ele, node, nod_comm,                  &
     &    belongs%blng_ele, belongs%x_ref_ele, belongs%host_ele,        &
     &    ele_comm)
      call end_elapsed_time(9)
      call calypso_mpi_barrier
!
      call dealloc_iele_belonged(belongs%host_ele)
      call dealloc_x_ref_ele(belongs)
      call dealloc_iele_belonged(belongs%blng_ele)
!
      end subroutine const_ele_comm_tbl2
!
!-----------------------------------------------------------------------
!
      subroutine const_comm_table_by_connenct2                          &
     &         (txt, numele, nnod_4_ele, ie, internal_flag, x_ele,      &
     &          node, nod_comm, neib_e, x_ref_ele, host, e_comm)
!
      use t_belonged_element_4_node
      use t_comm_table
      use find_element_comm_table
      use const_global_element_ids
      use make_element_comm_table_SR
      use const_element_comm_table
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
!
      type(work_4_ele_comm_table) :: wk_comm
!
!
      e_comm%num_neib = nod_comm%num_neib
      call allocate_type_neib_id(e_comm)
      call allocate_type_import_num(e_comm)
!
      write(*,*) 'count_element_import_num', my_rank
      call count_element_import_num(node%numnod, host%istack_4_node,    &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    e_comm%num_neib, e_comm%id_neib, e_comm%num_import,           &
     &    e_comm%istack_import, e_comm%ntot_import)
      call calypso_mpi_barrier
!
      call alloc_element_rev_imports(node%numnod,                       &
     &    nod_comm%ntot_export, e_comm%ntot_import, wk_comm)
      call allocate_type_import_item(e_comm)
!
      write(*,*) 'local_node_id_reverse_SR', my_rank
      call local_node_id_reverse_SR                                     &
     &   (node%numnod, nod_comm%num_neib, nod_comm%id_neib,             &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    wk_comm%item_local, wk_comm%inod_local)
      call calypso_mpi_barrier
!
      write(*,*) 'set_element_import_item', my_rank
      call set_element_import_item(node%numnod, node%internal_node,     &
     &    numele, nnod_4_ele, ie, node%inod_global, x_ele,              &
     &    host%istack_4_node, host%iele_4_node, wk_comm%inod_local,     &
     &    nod_comm%num_neib, nod_comm%istack_import,                    &
     &    nod_comm%item_import, e_comm%num_neib,                        &
     &    e_comm%istack_import, e_comm%item_import,                     &
     &    wk_comm%inod_import_e, wk_comm%inod_import_l,                 &
     &    wk_comm%xe_import)
      call calypso_mpi_barrier
!
      call allocate_type_export_num(e_comm)
!
      write(*,*) 'element_num_reverse_SR', my_rank
      call element_num_reverse_SR(e_comm%num_neib, e_comm%id_neib,      &
     &    e_comm%num_import, e_comm%num_export, e_comm%istack_export,   &
     &    e_comm%ntot_export)
      call calypso_mpi_barrier
!
      call alloc_element_rev_exports(e_comm%ntot_export, wk_comm)
      call allocate_type_export_item(e_comm)
!
      write(*,*) 'element_data_reverse_SR', my_rank
      call element_data_reverse_SR(e_comm%num_neib, e_comm%id_neib,     &
     &    e_comm%istack_import, e_comm%istack_export,                   &
     &    wk_comm%inod_import_e, wk_comm%inod_import_l,                 &
     &    wk_comm%xe_import, wk_comm%inod_export_e,                     &
     &    wk_comm%inod_export_l, wk_comm%xe_export)
      call calypso_mpi_barrier
!
      write(*,*) 'set_element_export_item2', my_rank
      call start_elapsed_time(10)
      call set_element_export_item2                                     &
     &   (txt, node%numnod, numele, node%inod_global,                   &
     &    internal_flag, x_ele, neib_e%istack_4_node,                   &
     &    neib_e%iele_4_node, x_ref_ele, nod_comm%num_neib,             &
     &    nod_comm%istack_import, nod_comm%item_import,                 &
     &    nod_comm%istack_export, nod_comm%item_export,                 &
     &    e_comm%num_neib, e_comm%istack_export,                        &
     &    wk_comm%inod_export_e, wk_comm%inod_export_l,                 &
     &    wk_comm%xe_export, e_comm%item_export)
      call end_elapsed_time(10)
      call calypso_mpi_barrier
!
      call dealloc_element_rev_exports(wk_comm)
      call dealloc_element_rev_imports(wk_comm)
!
      write(*,*) 'check_element_position', my_rank
      call check_element_position(txt, numele, x_ele, e_comm)
      call calypso_mpi_barrier
!
      end subroutine const_comm_table_by_connenct2
!
!-----------------------------------------------------------------------
!
      subroutine set_element_export_item2                               &
     &         (txt, numnod, numele, inod_global, internal_flag,        &
     &          x_ele, iele_stack_4_node, iele_4_node, x_ref_ele,       &
     &          num_neib, istack_import, item_import,                   &
     &          istack_export, item_export, num_neib_e,                 &
     &          istack_export_e, inod_export_e, inod_export_l,          &
     &          xe_export, item_export_e)
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &        :: iele_4_node(iele_stack_4_node(numnod))
      real(kind = kreal), intent(in)                                    &
     &        :: x_ref_ele(iele_stack_4_node(numnod))
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &        :: item_import(istack_import(num_neib))
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &        :: item_export(istack_export(num_neib))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      integer(kind = kint), intent(in)                                  &
     &        :: inod_export_l(istack_export_e(num_neib_e))
      integer(kind = kint_gl), intent(in)                               &
     &        :: inod_export_e(istack_export_e(num_neib_e))
      real(kind = kreal), intent(in)                                    &
     &        :: xe_export(3*istack_export_e(num_neib_e))
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_export_e(istack_export_e(num_neib_e))
!
      integer(kind = kint) :: ip, iflag
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, jed, jnum, jnod
      integer(kind = kint) :: kst, num
      integer(kind = kint_gl) :: inod_gl
      real(kind = kreal) :: dist_min
!
!
      do ip = 1, num_neib
        ist = istack_export_e(ip-1) + 1
        ied = istack_export_e(ip)
        jst = istack_import(ip-1) + 1
        jed = istack_import(ip)
!
        do inum = ist, ied
          inod = inod_export_l(inum)
          if(inod .le. 0) cycle
!
          iflag = 0
          dist_min = 1.0d30
!
          do jnum = jst, jed
            jnod = item_import(jnum)
!
            if(inod .eq. jnod) then
              kst = iele_stack_4_node(jnod-1) + 1
              num = iele_stack_4_node(jnod) - iele_stack_4_node(jnod-1)
!
              call start_elapsed_time(11)
              call search_target_element0(numele, internal_flag, x_ele, &
     &            num, iele_4_node(kst), x_ref_ele(kst),                &
     &            xe_export(3*inum-2), item_export_e(inum),             &
     &            dist_min, iflag)
              call end_elapsed_time(11)
              exit
            end if
          end do
!          if(iflag .eq. 0) write(*,*)                                  &
!     &           'Missing imported ', trim(txt), ' by external: ',     &
!     &                     my_rank, inum, item_export_e(inum),         &
!     &                     xe_export(3*inum-2:3*inum), dist_min
        end do
      end do
!
      do ip = 1, num_neib
        ist = istack_export_e(ip-1) + 1
        ied = istack_export_e(ip)
        jst = istack_export(ip-1) + 1
        jed = istack_export(ip)
!
        do inum = ist, ied
          inod_gl = inod_export_e(inum)
          if(item_export_e(inum) .gt. 0) cycle
!
          iflag = 0
          dist_min = 1.0d30
!
          do jnum = jst, jed
            jnod = item_export(jnum)
!
            if(inod_gl .eq. inod_global(jnod)) then
              kst = iele_stack_4_node(jnod-1) + 1
              num = iele_stack_4_node(jnod) - iele_stack_4_node(jnod-1)
!
              call start_elapsed_time(11)
              call search_target_element0(numele, internal_flag, x_ele, &
     &            num, iele_4_node(kst), x_ref_ele(kst),                &
     &            xe_export(3*inum-2), item_export_e(inum),             &
     &            dist_min, iflag)
              call end_elapsed_time(11)
              exit
            end if
          end do
          if(iflag .eq. 0) write(*,*)                                   &
     &           'Missing imported ', trim(txt), ' by internal: ',      &
     &                     my_rank, inum, item_export_e(inum),          &
     &                     xe_export(3*inum-2:3*inum), dist_min, num
        end do
      end do
!
      end subroutine set_element_export_item2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine search_target_element0(numele, internal_flag, x_ele,   &
     &          nele_4_node, iele_4_node, x_ref_ele,                    &
     &          xe_export, item_export_e, dist_min, iflag)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in) :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: nele_4_node
      integer(kind = kint), intent(in) :: iele_4_node(nele_4_node)
      real(kind = kreal), intent(in)   :: x_ref_ele(nele_4_node)
!
      real(kind = kreal), intent(in) :: xe_export(3)
!
      integer(kind = kint), intent(inout) :: item_export_e
      integer(kind = kint), intent(inout) :: iflag
      real(kind = kreal), intent(inout) :: dist_min
!
      integer(kind = kint), parameter :: many = 100
!
!
      if(nele_4_node .lt. 1) then
        return
      else if(nele_4_node .ge. many) then
        call search_target_element3(numele, internal_flag, x_ele,       &
     &      nele_4_node, iele_4_node, x_ref_ele,                        &
     &      xe_export, item_export_e, dist_min, iflag)
      else
        call search_target_element2(numele, internal_flag, x_ele,       &
     &       nele_4_node, iele_4_node, x_ref_ele,                       &
     &       xe_export, item_export_e, dist_min, iflag)
      end if
!
      end subroutine search_target_element0
!
!-----------------------------------------------------------------------
!
      subroutine search_target_element2(numele, internal_flag, x_ele,   &
     &          nele_4_node, iele_4_node, x_ref_ele,                    &
     &          xe_export, item_export_e, dist_min, iflag)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in) :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: nele_4_node
      integer(kind = kint), intent(in) :: iele_4_node(nele_4_node)
      real(kind = kreal), intent(in)   :: x_ref_ele(nele_4_node)
!
      real(kind = kreal), intent(in) :: xe_export(3)
!
      integer(kind = kint), intent(inout) :: item_export_e
      integer(kind = kint), intent(inout) :: iflag
      real(kind = kreal), intent(inout) :: dist_min
!
      real(kind = kreal) :: tiny = 1.0d-11
      integer(kind = kint), parameter :: many = 512
!
      integer(kind = kint) :: knum, kele
      real(kind = kreal) :: dx(3), dist
!
!
      do knum = 1, nele_4_node
        kele = iele_4_node(knum)
        if(internal_flag(kele) .eq. 0) cycle
        dx(1) = (xe_export(1) - x_ele(kele,1))**2
        dx(2) = (xe_export(2) - x_ele(kele,2))**2
        dx(3) = (xe_export(3) - x_ele(kele,3))**2
        dist = sqrt(sum(dx))
!
        if(dist .le. tiny) then
          item_export_e = kele
          iflag = 1
          exit
        end if
        dist_min = min(dist_min,dist)
      end do
!
      end subroutine search_target_element2
!
!-----------------------------------------------------------------------
!
      subroutine search_target_element3(numele, internal_flag, x_ele,   &
     &          nele_4_node, iele_4_node, x_ref_ele,                    &
     &          xe_export, item_export_e, dist_min, iflag)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in) :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: nele_4_node
      integer(kind = kint), intent(in) :: iele_4_node(nele_4_node)
      real(kind = kreal), intent(in)   :: x_ref_ele(nele_4_node)
!
      real(kind = kreal), intent(in) :: xe_export(3)
!
      integer(kind = kint), intent(inout) :: item_export_e
      integer(kind = kint), intent(inout) :: iflag
      real(kind = kreal), intent(inout) :: dist_min
!
      real(kind = kreal) :: tiny = 1.0d-11
      integer(kind = kint), parameter :: many = 512
!
      integer(kind = kint) :: kst, ked, knum, kele
      integer(kind = kint) :: kkst, kknum, kk
      real(kind = kreal) :: dx(3), dist
!
!
      if(nele_4_node .lt. 1) return
      kst = 1
      ked = nele_4_node
      knum = (ked-kst+1) / 2
      do
        kele = iele_4_node(knum)
        if(internal_flag(kele) .gt. 0) then
          dx(1) = (xe_export(1) - x_ele(kele,1))**2
          dx(2) = (xe_export(2) - x_ele(kele,2))**2
          dx(3) = (xe_export(3) - x_ele(kele,3))**2
          dist = sqrt(sum(dx))
!
          if(dist .le. tiny) then
            item_export_e = kele
            iflag = 1
            exit
          end if
          dist_min = min(dist_min,dist)
        end if
!
        if(xe_export(1) .lt. x_ref_ele(knum)) then
          ked = knum
          if(ked .le. kst) exit
          knum = ked - (ked-kst+1) / 2
        else if(xe_export(1) .gt. x_ref_ele(knum)) then
          kst = knum
          if(ked .le. kst) exit
          knum = kst + (ked-kst+1) / 2
        else
          do kk = knum, 1, -1
            if(x_ref_ele(kk) .ne. x_ref_ele(knum)) then
              kkst = kk
              exit
            end if
          end do
          do kk = knum, nele_4_node
            if(x_ref_ele(kk) .ne. x_ref_ele(knum)) then
              kknum = kk - kkst + 1
              exit
            end if
          end do
!
          call search_target_element2(numele, internal_flag, x_ele,     &
     &          kknum, iele_4_node(kkst), x_ref_ele(kkst),              &
     &          xe_export, item_export_e, dist_min, iflag)
          exit
        end if
      end do
!
      end subroutine search_target_element3
!
!-----------------------------------------------------------------------
!
      end module analyzer_mesh_test
