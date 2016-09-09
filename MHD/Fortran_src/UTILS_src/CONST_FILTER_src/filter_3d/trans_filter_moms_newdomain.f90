!trans_filter_moms_newdomain.f90
!      module trans_filter_moms_newdomain
!
!      modified by H. Matsui on Apr., 2008
!
!!      subroutine trans_filter_moms_newmesh_para(newmesh,              &
!!     &         (orgmesh, org_ele_mesh, newmesh, new_ele_mesh)
!!      subroutine trans_filter_moms_newmesh_sgl                        &
!!     &         (orgmesh, org_ele_mesh, newmesh, new_ele_mesh)
!!        type(mesh_geometry), intent(inout) :: orgmesh
!!        type(element_geometry), intent(inout) :: org_ele_mesh
!!        type(mesh_geometry), intent(inout) :: newmesh
!!        type(element_geometry), intent(inout) :: new_ele_mesh
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
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
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
      subroutine trans_filter_moms_newmesh_para                         &
     &         (orgmesh, org_ele_mesh, newmesh, new_ele_mesh)
!
      use calypso_mpi
!
      use set_filter_moms_2_new_mesh
!
      type(mesh_geometry), intent(inout) :: orgmesh
      type(element_geometry), intent(inout) :: org_ele_mesh
      type(mesh_geometry), intent(inout) :: newmesh
      type(element_geometry), intent(inout) :: new_ele_mesh
!
!
      call count_nele_newdomain_para(my_rank)
      call allocate_iele_local_newfilter
!
      call trans_filter_moms_each_domain                                &
     &   (my_rank, orgmesh, org_ele_mesh, newmesh, new_ele_mesh)
!
      call deallocate_iele_local_newfilter
!
      end subroutine trans_filter_moms_newmesh_para
!
!   --------------------------------------------------------------------
!
      subroutine trans_filter_moms_newmesh_sgl                          &
     &         (orgmesh, org_ele_mesh, newmesh, new_ele_mesh)
!
      use calypso_mpi
      use m_2nd_pallalel_vector
      use set_filter_moms_2_new_mesh
!
      type(mesh_geometry), intent(inout) :: orgmesh
      type(element_geometry), intent(inout) :: org_ele_mesh
      type(mesh_geometry), intent(inout) :: newmesh
      type(element_geometry), intent(inout) :: new_ele_mesh
!
      integer(kind = kint) :: ip2, my_rank_2nd
!
!
      call count_nele_newdomain_single
!
      call allocate_iele_local_newfilter
!
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
        call trans_filter_moms_each_domain                              &
     &     (my_rank, orgmesh, org_ele_mesh, newmesh, new_ele_mesh)
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
      max_gl_ele_newdomain = ele_IO%iele_global(1)
      do iele = 2, ele_IO%numele
        max_gl_ele_newdomain                                            &
     &         = max(max_gl_ele_newdomain,ele_IO%iele_global(iele))
      end do
!
      call deallocate_ele_connect_type(ele_IO)
      call dealloc_node_geometry_base(nod_IO)
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
        call dealloc_node_geometry_base(nod_IO)
        call deallocate_neib_domain_IO
!
        max_gl_ele_newdomain = max_gl_ele_newdomain + ele_IO%numele
      end do
!
      end subroutine count_nele_newdomain_single
!
!   --------------------------------------------------------------------
!
      subroutine trans_filter_moms_each_domain(my_rank_2nd,             &
     &          orgmesh, org_ele_mesh, newmesh, new_ele_mesh)
!
      use calypso_mpi
      use m_filter_file_names
      use filter_moment_IO_select
      use set_filter_moms_2_new_mesh
      use set_element_data_4_IO
      use set_filter_moms_2_new_mesh
      use set_nnod_4_ele_by_type
!
      use t_filter_elength
      use t_filter_moments
!
      integer(kind = kint), intent(in) :: my_rank_2nd
!
      type(mesh_geometry), intent(inout) :: orgmesh
      type(element_geometry), intent(inout) :: org_ele_mesh
      type(mesh_geometry), intent(inout) :: newmesh
      type(element_geometry), intent(inout) :: new_ele_mesh
!
!
      type(gradient_model_data_type), save :: FEM_elen_t
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
      call deallocate_mesh_groups_IO
      call dealloc_node_geometry_base(nod_IO)
      call deallocate_comm_item_IO
!
      newmesh%node%numnod = nod_IO%numnod
      newmesh%node%internal_node = nod_IO%internal_node
      call copy_ele_connect_from_IO(newmesh%ele)
      call set_3D_nnod_4_sfed_by_ele(newmesh%ele%nnod_4_ele,            &
     &                               new_ele_mesh%surf%nnod_4_surf,     &
     &                               new_ele_mesh%edge%nnod_4_edge)
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
     &     (izero, FEM_elen_t, FEM_momenet1)
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
      call const_filter_moms_newdomain                                  &
     &   (nprocs, newmesh%node, orgmesh%node, orgmesh%ele,              &
     &    org_ele_mesh%surf, org_ele_mesh%edge,                         &
     &    FEM_elen_t, FEM_momenet1, elen2_ele, moment2_ele)
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
     &     (my_rank_2nd, FEM_elen_t, FEM_momenet1)
        call dealloc_filter_moms_ele_type(FEM_momenet1)
      end if
!
!
      if (iflag_set_filter_elen .gt. 0) then
        ifmt_filter_file = id_ascii_file_fmt
        filter_file_head = new_filter_elen_head
        FEM_elen_t%nnod_filter_mom = newmesh%node%numnod
        FEM_elen_t%nele_filter_mom = newmesh%ele%numele
        call alloc_elen_ele_type                                        &
     &     (FEM_elen_t%nele_filter_mom, FEM_elen_t%elen_ele)
!
        call copy_elength_type(FEM_elen_t%nele_filter_mom,              &
     &    elen2_ele%moms,  FEM_elen_t%elen_ele%moms)
        call copy_elen_diffs_type(FEM_elen_t%nele_filter_mom,           &
     &    elen2_ele%diff, FEM_elen_t%elen_ele%diff)
        call copy_elen_diffs_type(FEM_elen_t%nele_filter_mom,           &
     &    elen2_ele%diff2, FEM_elen_t%elen_ele%diff2)
!
        call sel_write_filter_elen_file(my_rank_2nd, FEM_elen_t)
!
        call dealloc_elen_type(elen2_ele)
        call dealloc_elen_type(FEM_elen_t%elen_ele)
      end if
!
      call dealloc_ref_1d_mom_type(FEM_elen_t%filter_conf)
      call deallocate_ele_connect_type(newmesh%ele)
!
      end subroutine trans_filter_moms_each_domain
!
!   --------------------------------------------------------------------
!
      subroutine const_filter_moms_newdomain(norg_domain, new_node,     &
     &          org_node, org_ele, org_surf, org_edge, FEM_elen_org,    &
     &          mom1, elen2_e, mom2_ele)
!
      use m_filter_file_names
      use filter_moment_IO_select
      use set_element_data_4_IO
      use set_nnod_4_ele_by_type
      use set_filter_moms_2_new_mesh
!
      use t_filter_moments
      use t_filter_elength
!
      integer(kind = kint), intent(in) :: norg_domain
      type(node_data), intent(in) :: new_node
!
      type(node_data),    intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(surface_data), intent(inout) :: org_surf
      type(edge_data), intent(inout) :: org_edge
      type(gradient_model_data_type), intent(inout) :: FEM_elen_org
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
        call deallocate_mesh_groups_IO
        call dealloc_node_geometry_base(nod_IO)
        call deallocate_comm_item_IO
!
        call copy_ele_connect_from_IO(org_ele)
        call set_3D_nnod_4_sfed_by_ele(org_ele%nnod_4_ele,              &
     &     org_surf%nnod_4_surf, org_edge%nnod_4_edge)
!
!  read element length data
!
        if (iflag_set_filter_moms .gt. 0) then
          ifmt_filter_file = id_ascii_file_fmt
          filter_file_head = org_filter_moms_head
          call sel_read_filter_moms_file(my_rank_org,                   &
     &        org_node%numnod, org_ele%numele, FEM_elen_org,            &
     &        mom1, ierr)
!
          call set_new_filter_moms_ele                                  &
     &       (org_ele, new_node, mom1, mom1%num_filter_moms, mom2_ele)
          call dealloc_filter_moms_ele_type(mom1)
          if (ip .lt. norg_domain .or. iflag_set_filter_elen.gt.0) then
            call dealloc_ref_1d_mom_type(FEM_elen_org%filter_conf)
          end if
        end if
!
!
        if (iflag_set_filter_elen .gt. 0) then
          ifmt_filter_file = id_ascii_file_fmt
          filter_file_head = org_filter_elen_head
          call sel_read_filter_elen_file(my_rank_org,                   &
     &        org_node%numnod, org_ele%numele, FEM_elen_org, ierr)
!
!          if (iflag_debug.eq.1) write(*,*) 'set_new_elength_ele'
          call set_new_elength_ele                                      &
     &       (org_ele, new_node, FEM_elen_org, elen2_e)
!          if (iflag_debug.eq.1) write(*,*) 'dealloc_elen_type'
          call dealloc_elen_type(FEM_elen_org%elen_ele)
          if (ip .lt. norg_domain) then
            call dealloc_ref_1d_mom_type(FEM_elen_org%filter_conf)
          end if
        end if
!
        call deallocate_ele_connect_type(org_ele)
      end do
!
      end subroutine const_filter_moms_newdomain
!
!   --------------------------------------------------------------------
!
      end module trans_filter_moms_newdomain
