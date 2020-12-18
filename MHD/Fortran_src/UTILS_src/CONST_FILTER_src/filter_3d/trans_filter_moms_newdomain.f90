!trans_filter_moms_newdomain.f90
!      module trans_filter_moms_newdomain
!
!      modified by H. Matsui on Apr., 2008
!
!!      subroutine trans_filter_moms_newmesh_para                       &
!!     &         (newfil_p, orgmesh, newmesh)
!!      subroutine trans_filter_moms_newmesh_sgl                        &
!!     &         (newfil_p, orgmesh, newmesh)
!!        type(ctl_param_newdom_filter), intent(in) :: newfil_p
!!        type(mesh_geometry), intent(inout) :: orgmesh
!!        type(mesh_geometry), intent(inout) :: newmesh
!
      module trans_filter_moms_newdomain
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use mesh_IO_select
      use set_parallel_file_name
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_ctl_param_newdom_filter
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
     &         (newfil_p, orgmesh, newmesh)
!
      use calypso_mpi
!
      use set_filter_moms_2_new_mesh
!
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
!
      type(mesh_geometry), intent(inout) :: orgmesh
      type(mesh_geometry), intent(inout) :: newmesh
!
      integer(kind = kint) :: ierr
!
!
      call count_nele_newdomain_para(my_rank, newfil_p, ierr)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh for conversion is wrong!!')
      end if
!
      call allocate_iele_local_newfilter
!
      call trans_filter_moms_each_domain(my_rank, newfil_p,             &
     &    orgmesh, newmesh, ierr)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Original mesh is wrong!!')
      end if
!
      call deallocate_iele_local_newfilter
!
      end subroutine trans_filter_moms_newmesh_para
!
!   --------------------------------------------------------------------
!
      subroutine trans_filter_moms_newmesh_sgl                          &
     &         (newfil_p, orgmesh, newmesh)
!
      use calypso_mpi
      use m_2nd_pallalel_vector
      use set_filter_moms_2_new_mesh
!
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
!
      type(mesh_geometry), intent(inout) :: orgmesh
      type(mesh_geometry), intent(inout) :: newmesh
!
      integer(kind = kint) :: ip2, my_rank_2nd
      integer(kind = kint) :: ierr
!
!
      call count_nele_newdomain_single(newfil_p)
!
      call allocate_iele_local_newfilter
!
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
        call trans_filter_moms_each_domain(my_rank, newfil_p,           &
     &      orgmesh, newmesh, ierr)
        if(ierr .gt. 0) stop  'Original mesh is wrong!!'
      end do
!
      call deallocate_iele_local_newfilter
!
      end subroutine trans_filter_moms_newmesh_sgl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_nele_newdomain_para                              &
     &         (my_rank_2nd, newfil_p, ierr)
!
      use set_filter_moms_2_new_mesh
!
      integer, intent(in) :: my_rank_2nd
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
!
      integer(kind = kint), intent(inout) :: ierr
!
      type(mesh_geometry) :: mesh_IO_f
!
      integer(kind = kint) :: iele
!
!
      call sel_read_mesh_geometry                                       &
     &   (newfil_p%tgt_mesh_file, my_rank_2nd, mesh_IO_f, ierr)
      if(ierr .gt. 0) return
!
      max_gl_ele_newdomain = mesh_IO_f%ele%iele_global(1)
      do iele = 2, mesh_IO_f%ele%numele
        max_gl_ele_newdomain                                            &
     &      = max(max_gl_ele_newdomain,mesh_IO_f%ele%iele_global(iele))
      end do
!
      call dealloc_mesh_geometry_base(mesh_IO_f)
!
      end subroutine count_nele_newdomain_para
!
!   --------------------------------------------------------------------
!
      subroutine count_nele_newdomain_single(newfil_p)
!
      use m_2nd_pallalel_vector
      use set_filter_moms_2_new_mesh
!
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
!
      type(mesh_geometry) :: mesh_IO_f
      integer :: ip2, my_rank_2nd
      integer(kind = kint) :: ierr
!
!
      max_gl_ele_newdomain = 0
      do ip2 = 1, nprocs_2nd
        my_rank_2nd = ip2 - 1
!
        call sel_read_geometry_size                                     &
     &     (newfil_p%tgt_mesh_file, my_rank_2nd, mesh_IO_f, ierr)
        if(ierr .gt. 0) stop 'new mesh data is wrong'
!
        max_gl_ele_newdomain = max_gl_ele_newdomain                     &
     &                        + mesh_IO_f%ele%numele
!
        call dealloc_node_geometry_IO(mesh_IO_f)
      end do
!
      end subroutine count_nele_newdomain_single
!
!   --------------------------------------------------------------------
!
      subroutine trans_filter_moms_each_domain(my_rank_2nd, newfil_p,   &
     &          orgmesh, newmesh, ierr)
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
      integer, intent(in) :: my_rank_2nd
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
!
      type(mesh_geometry), intent(inout) :: orgmesh
      type(mesh_geometry), intent(inout) :: newmesh
      integer(kind = kint), intent(inout) :: ierr
!
!
      type(gradient_model_data_type), save :: FEM_elen_t
      type(gradient_filter_mom_type), save :: FEM_momenet1
!
      type(mesh_geometry) :: mesh_IO_f
      type(elen_ele_diffs_type) :: elen2_ele
      type(ele_mom_diffs_type), allocatable, save :: moment2_ele(:)
!
!
      call sel_read_mesh_geometry                                       &
     &   (newfil_p%tgt_mesh_file, my_rank_2nd, mesh_IO_f, ierr)
      if(ierr .gt. 0) return
!
      newmesh%node%numnod = mesh_IO_f%node%numnod
      newmesh%node%internal_node = mesh_IO_f%node%internal_node
      call copy_ele_connect_from_IO(mesh_IO_f%ele, newmesh%ele)
      call set_3D_nnod_4_sfed_by_ele(newmesh%ele%nnod_4_ele,            &
     &                               newmesh%surf%nnod_4_surf,          &
     &                               newmesh%edge%nnod_4_edge)
!
      call dealloc_mesh_geometry_base(mesh_IO_f)
!
!    construct new filter table
!
      if (newfil_p%iflag_set_filter_elen .gt. 0) then
        call alloc_elen_ele_type(newmesh%ele%numele, elen2_ele)
      end if
!
      if (newfil_p%iflag_set_filter_moms .gt. 0) then
        ifmt_filter_file = id_ascii_file_fmt
        filter_file_head = newfil_p%org_filter_moms_head
        ierr = 0
        call sel_read_num_filter_mom_file                               &
     &     (0, FEM_elen_t, FEM_momenet1, ierr)
        if(ierr .gt. 0) stop "Error in sel_read_num_filter_mom_file"
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
     &   (newfil_p, nprocs, newmesh%node, orgmesh%node, orgmesh%ele,    &
     &    orgmesh%surf, orgmesh%edge, FEM_elen_t, FEM_momenet1,         &
     &    elen2_ele, moment2_ele)
!
!
!      write new filter moments file
!
      if (newfil_p%iflag_set_filter_moms .gt. 0) then
        ifmt_filter_file = id_ascii_file_fmt
        filter_file_head = newfil_p%new_filter_moms_head
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
      if (newfil_p%iflag_set_filter_elen .gt. 0) then
        ifmt_filter_file = id_ascii_file_fmt
        filter_file_head = newfil_p%new_filter_elen_head
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
      call dealloc_ele_connect(newmesh%ele)
!
      end subroutine trans_filter_moms_each_domain
!
!   --------------------------------------------------------------------
!
      subroutine const_filter_moms_newdomain(newfil_p, norg_domain,     &
     &          new_node, org_node, org_ele, org_surf, org_edge,        &
     &          FEM_elen_org,  mom1, elen2_e, mom2_ele)
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
      integer, intent(in) :: norg_domain
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
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
      type(mesh_geometry) :: mesh_IO_f
      integer :: ip, my_rank_org
      integer(kind = kint) :: ierr
!
!
      do ip = 1, norg_domain
        my_rank_org = ip - 1
!
        call sel_read_mesh_geometry                                     &
     &     (newfil_p%tgt_mesh_file, my_rank_org, mesh_IO_f, ierr)
!
        call copy_ele_connect_from_IO(mesh_IO_f%ele, org_ele)
        call dealloc_mesh_geometry_base(mesh_IO_f)
!
        call set_3D_nnod_4_sfed_by_ele(org_ele%nnod_4_ele,              &
     &     org_surf%nnod_4_surf, org_edge%nnod_4_edge)
!
!  read element length data
!
        if (newfil_p%iflag_set_filter_moms .gt. 0) then
          ifmt_filter_file = id_ascii_file_fmt
          filter_file_head = newfil_p%org_filter_moms_head
          call sel_read_filter_moms_file(my_rank_org,                   &
     &        org_node%numnod, org_ele%numele, FEM_elen_org,            &
     &        mom1, ierr)
!
          call set_new_filter_moms_ele                                  &
     &       (org_ele, new_node, mom1, mom1%num_filter_moms, mom2_ele)
          call dealloc_filter_moms_ele_type(mom1)
          if (ip .lt. norg_domain                                       &
     &         .or. newfil_p%iflag_set_filter_elen.gt.0) then
            call dealloc_ref_1d_mom_type(FEM_elen_org%filter_conf)
          end if
        end if
!
!
        if (newfil_p%iflag_set_filter_elen .gt. 0) then
          ifmt_filter_file = id_ascii_file_fmt
          filter_file_head = newfil_p%org_filter_elen_head
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
        call dealloc_ele_connect(org_ele)
      end do
!
      end subroutine const_filter_moms_newdomain
!
!   --------------------------------------------------------------------
!
      end module trans_filter_moms_newdomain
