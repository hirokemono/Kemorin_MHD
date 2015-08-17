!trans_filter_moms_newdomain.f90
!      module trans_filter_moms_newdomain
!
!      modified by H. Matsui on Apr., 2008
!
!      subroutine trans_filter_moms_newmesh_para(newmesh,               &
!     &          new_surf_mesh, new_edge_mesh)
!      subroutine trans_filter_moms_newmesh_sgl(newmesh,                &
!     &          new_surf_mesh, new_edge_mesh)
!
      module trans_filter_moms_newdomain
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_ctl_param_newdom_filter
      use m_comm_data_IO
      use m_read_mesh_data
      use m_read_boundary_data
      use mesh_IO_select
      use set_parallel_file_name
!
      implicit none
!
      private :: count_nele_newdomain_para
      private :: count_nele_newdomain_single
      private :: trans_filter_moms_each_domain
      private :: const_filter_moms_newdomain
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine trans_filter_moms_newmesh_para(newmesh,                &
     &          new_surf_mesh, new_edge_mesh)
!
      use calypso_mpi
!
      use set_filter_moms_2_new_mesh
      use t_mesh_data
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(surface_geometry), intent(inout) :: new_surf_mesh
      type(edge_geometry), intent(inout) ::  new_edge_mesh
!
!
      call count_nele_newdomain_para(my_rank)
      call allocate_iele_local_newfilter
!
      call trans_filter_moms_each_domain(my_rank, newmesh,              &
     &    new_surf_mesh, new_edge_mesh)
!
      call deallocate_iele_local_newfilter
!
      end subroutine trans_filter_moms_newmesh_para
!
!   --------------------------------------------------------------------
!
      subroutine trans_filter_moms_newmesh_sgl(newmesh,                 &
     &          new_surf_mesh, new_edge_mesh)
!
      use m_2nd_pallalel_vector
      use set_filter_moms_2_new_mesh
!
      use t_mesh_data
!
      integer(kind = kint) :: ip2, my_rank_2nd
      type(mesh_geometry), intent(inout) :: newmesh
      type(surface_geometry), intent(inout) :: new_surf_mesh
      type(edge_geometry), intent(inout) ::  new_edge_mesh
!
!
      call count_nele_newdomain_single
!
      call allocate_iele_local_newfilter
!
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
        call trans_filter_moms_each_domain(my_rank_2nd, newmesh,        &
     &      new_surf_mesh, new_edge_mesh)
      end do
!
      call deallocate_iele_local_newfilter
!
      end subroutine trans_filter_moms_newmesh_sgl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_nele_newdomain_para(my_rank_2nd)
!
      use set_filter_moms_2_new_mesh
!
      integer(kind = kint), intent(in) :: my_rank_2nd
      integer(kind = kint) :: iele
!
!
      mesh_file_head = target_mesh_head
      call sel_read_mesh_geometry(my_rank_2nd)
!
      mesh_file_head = org_mesh_head
!
      max_gl_ele_newdomain = globalelmid_dummy(1)
      do iele = 2, numele_dummy
        max_gl_ele_newdomain                                            &
     &         = max(max_gl_ele_newdomain,globalelmid_dummy(iele))
      end do
!
      call deallocate_mesh_arrays
      call deallocate_comm_item_IO
!
      end subroutine count_nele_newdomain_para
!
!   --------------------------------------------------------------------
!
      subroutine count_nele_newdomain_single
!
      use m_2nd_pallalel_vector
      use set_filter_moms_2_new_mesh
!
      integer(kind = kint) :: ip2, my_rank_2nd
!
!
      max_gl_ele_newdomain = 0
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
!
        mesh_file_head = target_mesh_head
        iflag_mesh_file_fmt = id_ascii_file_fmt
        call sel_read_geometry_size(my_rank_2nd)
        call deallocate_node_data_dummy
        call deallocate_neib_domain_IO
!
        max_gl_ele_newdomain = max_gl_ele_newdomain + numele_dummy
      end do
!
      end subroutine count_nele_newdomain_single
!
!   --------------------------------------------------------------------
!
      subroutine trans_filter_moms_each_domain(my_rank_2nd, newmesh,    &
     &          new_surf_mesh, new_edge_mesh)
!
      use calypso_mpi
      use m_filter_file_names
      use m_filter_elength
      use filter_moment_IO_select
      use set_filter_moms_2_new_mesh
      use set_element_types_4_IO
      use set_filter_moms_2_new_mesh
      use set_mesh_types
!
      use t_mesh_data
      use t_filter_moments
!
      integer(kind = kint), intent(in) :: my_rank_2nd
      type(mesh_geometry), intent(inout) :: newmesh
      type(surface_geometry), intent(inout) :: new_surf_mesh
      type(edge_geometry), intent(inout) ::  new_edge_mesh
!
!
      type(gradient_filter_mom_type), save :: FEM_momenet1
!
      type(elen_ele_diffs_type) :: elen2_ele
      type(ele_mom_diffs_type), allocatable, save :: moment2_ele(:)
!
!
      iflag_mesh_file_fmt = id_ascii_file_fmt
      mesh_file_head = target_mesh_head
      call sel_read_mesh(my_rank_2nd)
      mesh_file_head = org_mesh_head
!
      call deallocate_boundary_arrays
      call deallocate_node_data_dummy
      call deallocate_comm_item_IO
!
      newmesh%node%numnod = numnod_dummy
      newmesh%node%internal_node = internal_node_dummy
      call copy_ele_connect_type_from_IO(newmesh%ele)
      call set_nnod_surf_edge_for_type(new_surf_mesh, new_edge_mesh,    &
     &    newmesh%ele%nnod_4_ele)
!
!    construct new filter table
!
      if (iflag_set_filter_elen .gt. 0) then
        call alloc_elen_ele_type(newmesh%ele%numele, elen2_ele)
      end if
!
      if (iflag_set_filter_moms .gt. 0) then
        ifmt_filter_file = id_ascii_file_fmt
        filter_file_head = org_filter_moms_head
        call sel_read_num_filter_mom_file                               &
     &     (izero, FEM1_elen, FEM_momenet1)
!
        allocate(moment2_ele(FEM_momenet1%num_filter_moms))
        call alloc_filter_mom_ele_items(newmesh%ele%numele,             &
     &      FEM_momenet1%num_filter_moms, moment2_ele)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'set_iele_table_4_newfilter'
      call set_iele_table_4_newfilter(newmesh%ele)
!
      if (iflag_debug.eq.1) write(*,*) 'const_filter_moms_newdomain'
      call const_filter_moms_newdomain(nprocs, newmesh%node,            &
     &    FEM_momenet1, elen2_ele, moment2_ele)
!
!
!      write new filter moments file
!
      if (iflag_set_filter_moms .gt. 0) then
        ifmt_filter_file = id_ascii_file_fmt
        filter_file_head = new_filter_moms_head
        FEM_momenet1%nnod_fmom = newmesh%node%numnod

        call alloc_filter_moms_ele_type                                 &
     &     (newmesh%ele%numele, FEM_momenet1)
        call copy_filter_moms_ele(newmesh%ele%numele,                   &
     &      FEM_momenet1%num_filter_moms, moment2_ele,                  &
     &      FEM_momenet1%mom_ele)
        call dealloc_filter_mom_ele_items                               &
     &     (FEM_momenet1%num_filter_moms, moment2_ele)
        deallocate(moment2_ele)

        call sel_write_filter_moms_file                                 &
     &     (my_rank_2nd, FEM1_elen, FEM_momenet1)
        call dealloc_filter_moms_ele_type(FEM_momenet1)
      end if
!
!
      if (iflag_set_filter_elen .gt. 0) then
        ifmt_filter_file = id_ascii_file_fmt
        filter_file_head = new_filter_elen_head
        FEM1_elen%nnod_filter_mom = newmesh%node%numnod
        FEM1_elen%nele_filter_mom = newmesh%ele%numele
        call alloc_elen_ele_type                                        &
     &     (FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
        call copy_filter_elen_ele_from_type(elen2_ele)
!
        call sel_write_filter_elen_file(my_rank_2nd, FEM1_elen)
!
        call dealloc_elen_type(elen2_ele)
        call dealloc_elen_type(FEM1_elen%elen_ele)
      end if
!
      call dealloc_ref_1d_mom_type(FEM1_elen%filter_conf)
      call deallocate_ele_connect_type(newmesh%ele)
!
      end subroutine trans_filter_moms_each_domain
!
!   --------------------------------------------------------------------
!
      subroutine const_filter_moms_newdomain(norg_domain, new_node,     &
     &          mom1, elen2_e, mom2_ele)
!
      use m_geometry_data
      use m_filter_file_names
      use m_filter_elength
      use t_filter_moments
      use t_filter_elength
      use filter_moment_IO_select
      use set_element_types_4_IO
      use set_nnod_4_ele_by_type
      use set_filter_moms_2_new_mesh
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: norg_domain
      type(node_data), intent(in) :: new_node
      type(elen_ele_diffs_type), intent(inout) :: elen2_e
      type(gradient_filter_mom_type), intent(inout) :: mom1
      type(ele_mom_diffs_type), intent(inout)                           &
     &               :: mom2_ele(mom1%num_filter_moms)
!
      integer(kind = kint) :: ip, my_rank_org, ierr
!
!
      do ip = 1, norg_domain
        my_rank_org = ip - 1
!
        iflag_mesh_file_fmt = id_ascii_file_fmt
        mesh_file_head = target_mesh_head
        call sel_read_mesh(my_rank_org)
!
        call deallocate_boundary_arrays
        call deallocate_node_data_dummy
        call deallocate_comm_item_IO
!
        call copy_ele_connect_type_from_IO(ele1)
        call set_3D_nnod_4_sfed_by_ele                                 &
     &     (ele1%nnod_4_ele, surf1%nnod_4_surf, edge1%nnod_4_edge)
!
!  read element length data
!
        if (iflag_set_filter_moms .gt. 0) then
          ifmt_filter_file = id_ascii_file_fmt
          filter_file_head = org_filter_moms_head
          call sel_read_filter_moms_file(my_rank_org,                   &
     &        node1%numnod, ele1%numele, FEM1_elen, mom1, ierr)
!
          call set_new_filter_moms_ele                                  &
     &       (mom1, new_node, mom1%num_filter_moms, mom2_ele)
          call dealloc_filter_moms_ele_type(mom1)
          if (ip .lt. norg_domain .or. iflag_set_filter_elen.gt.0) then
            call dealloc_ref_1d_mom_type(FEM1_elen%filter_conf)
          end if
        end if
!
!
        if (iflag_set_filter_elen .gt. 0) then
          ifmt_filter_file = id_ascii_file_fmt
          filter_file_head = org_filter_elen_head
          call sel_read_filter_elen_file(my_rank_org,                   &
     &        node1%numnod, ele1%numele, FEM1_elen, ierr)
!
!          if (iflag_debug.eq.1) write(*,*) 'set_new_elength_ele'
          call set_new_elength_ele(new_node, elen2_e)
!          if (iflag_debug.eq.1) write(*,*) 'dealloc_elen_type'
          call dealloc_elen_type(FEM1_elen%elen_ele)
          if (ip .lt. norg_domain) then
            call dealloc_ref_1d_mom_type(FEM1_elen%filter_conf)
          end if
        end if
!
        call deallocate_ele_connect_type(ele1)
      end do
!
      end subroutine const_filter_moms_newdomain
!
!   --------------------------------------------------------------------
!
      end module trans_filter_moms_newdomain
