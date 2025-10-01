!>@file   cubmesh_311.f90
!!@brief  module cubmesh_311
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Top routine sto make palne mesh
!!
!!@verbatim
!!      subroutine const_single_cubmesh311                              &
!!     &         (elm_type, cube_p, c_size, mesh, group)
!!        type(ctl_param_plane_mesh), intent(in) :: cube_p
!!        type(size_of_cube), intent(inout) :: c_size
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!      subroutine cubmesh311_only(elm_type, cube_p, c_size)
!!      subroutine cubmesh311_w_filter(elm_type, cube_p, c_size)
!!        type(ctl_param_plane_mesh), intent(in) :: cube_p
!!        type(size_of_cube), intent(inout) :: c_size
!!@endverbatim
!
      module cubmesh_311
!
      use m_precision
      use m_machine_parameter
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_cube_position
      use t_local_node_id_cube
      use t_control_param_plane_mesh
      use t_mesh_data
!
      use t_filter_elength
      use t_filter_work_cubmesh
      use t_filter_data_4_plane
!
      implicit none
!
      private :: set_each_plane_mesh, set_each_plane_filter
      private :: write_each_plane_mesh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_single_cubmesh311                                &
     &         (elm_type, cube_p, c_size, mesh, group)
!
      implicit  none
!
      integer(kind=kint), intent(in) :: elm_type
      type(ctl_param_plane_mesh), intent(in) :: cube_p
      type(size_of_cube), intent(inout) :: c_size
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
      type(vertical_position_cube), save :: c_vert1
      type(local_node_id_cube), save :: loc_id1
      type(size_of_each_cube), save :: c_each1
      type(neib_range_cube), save :: nb_rng1
!
!
      call set_plane_range_w_sleeve(elm_type, c_size)
      call set_plane_resolution(c_size)
!
! ***** allocate position of node at z-component
!
      call set_position_4_vartical                                      &
     &   (elm_type, cube_p%iflag_ztype, c_size, c_vert1)
!
! ***** allocate nodal id table
!
      call alloc_node_informations(c_size, loc_id1)
!
! **********   domain loop for each pe   **********
      call set_each_plane_mesh(elm_type, ione, ione, ione, ione,        &
     &    c_size, c_vert1, loc_id1, c_each1, nb_rng1, mesh, group)
      call reset_node_info(loc_id1)
!
      end subroutine const_single_cubmesh311
!
! ----------------------------------------------------------------------
!
      subroutine cubmesh311_only(elm_type, cube_p, c_size)
!
      implicit  none
!
      integer(kind=kint), intent(in) :: elm_type
      type(ctl_param_plane_mesh), intent(in) :: cube_p
      type(size_of_cube), intent(inout) :: c_size
!
      type(vertical_position_cube), save :: c_vert1
      type(local_node_id_cube), save :: loc_id1
      type(size_of_each_cube), save :: c_each1
      type(neib_range_cube), save :: nb_rng1
      type(mesh_geometry), save :: mesh1
      type(mesh_groups), save :: group1
!
      integer(kind=kint)  ::  ipe, jpe, kpe, pe_id
!
!
      call set_plane_range_w_sleeve(elm_type, c_size)
      call set_plane_resolution(c_size)
!
! ***** allocate position of node at z-component
!
      call set_position_4_vartical                                      &
     &   (elm_type, cube_p%iflag_ztype, c_size, c_vert1)
!
! ***** allocate nodal id table
!
      call alloc_node_informations(c_size, loc_id1)
!
! **********   domain loop for each pe   **********
      pe_id = 1

      do kpe = 1, c_size%ndz
        do jpe = 1, c_size%ndy
          do ipe = 1, c_size%ndx
            call set_each_plane_mesh(elm_type, ipe, jpe, kpe, pe_id,    &
     &          c_size, c_vert1, loc_id1, c_each1, nb_rng1,             &
     &          mesh1, group1)
            call write_each_plane_mesh                                  &
     &         (pe_id, cube_p, c_size, mesh1, group1)
            call dealloc_mesh_geometry_base(mesh1)
            call dealloc_groups_data(group1)
!
            write(*,*) 'change domain'
            call reset_node_info(loc_id1)
            pe_id = pe_id + 1
          end do
        end do
      end do
!
      end subroutine cubmesh311_only
!
! ----------------------------------------------------------------------
!
      subroutine cubmesh311_w_filter(elm_type, cube_p, c_size)
!
      implicit  none
!
      integer(kind=kint), intent(in)  ::   elm_type
      type(ctl_param_plane_mesh), intent(in) :: cube_p
      type(size_of_cube), intent(inout) :: c_size
!
      type(vertical_position_cube), save :: c_vert1
      type(local_node_id_cube), save :: loc_id1
      type(size_of_each_cube), save :: c_each1
      type(neib_range_cube), save :: nb_rng1
      type(gradient_model_data_type), save :: FEM_elen_c
      type(filterings_4_cubmesh), save :: c_fils1
      type(filter_data_4_plane), save :: cube_fil1
      type(mesh_geometry), save :: mesh1
      type(mesh_groups), save :: group1
!
      integer(kind=kint)  ::  ipe, jpe, kpe, pe_id, ierr
!
!
      call set_plane_range_w_sleeve(elm_type, c_size)
      call set_plane_resolution(c_size)
!
! ***** allocate position of node at z-component
!
      call set_position_4_vartical                                      &
     &   (elm_type, cube_p%iflag_ztype, c_size, c_vert1)
!
! ***** allocate nodal id table
!
      call alloc_node_informations(c_size, loc_id1)
!
!    allocate work array
!
      call alloc_work_4_filter_nod(c_size, c_fils1%c_fil_nod)
!
!     set one-dimensional moments
!
      FEM_elen_c%filter_conf%nf_type = cube_p%iflag_filter
      call alloc_filter_4_plane(c_size%ndepth, c_size%nz_all,           &
     &    FEM_elen_c%filter_conf%nf_type, cube_fil1)
      call read_z_filter_info(cube_p, c_size,                           &
     &    FEM_elen_c%filter_conf%nf_type, cube_fil1, ierr)
      if(ierr .gt. 0) stop 'Read file error'
!
! **********   domain loop for each pe   **********
!
      pe_id = 1

      do kpe = 1, c_size%ndz
        do jpe = 1, c_size%ndy
          do ipe = 1, c_size%ndx
            call set_each_plane_mesh(elm_type, ipe, jpe, kpe, pe_id,    &
     &          c_size, c_vert1, loc_id1, c_each1, nb_rng1,             &
     &          mesh1, group1)
            call write_each_plane_mesh                                  &
     &         (pe_id, cube_p, c_size, mesh1, group1)
            call dealloc_mesh_geometry_base(mesh1)
            call dealloc_groups_data(group1)
!   construct filtering information
            call set_each_plane_filter                                  &
     &         (pe_id, cube_p, c_size, loc_id1,                         &
     &          c_each1, nb_rng1, FEM_elen_c, c_fils1, cube_fil1)
!
            write(*,*) 'change domain'
            call reset_node_info(loc_id1)
            call reset_work_4_filter_nod(c_fils1%c_fil_nod)
            pe_id = pe_id + 1
          end do
        end do
      end do
!
      call dealloc_filter_4_plane(cube_fil1)
!
      end subroutine cubmesh311_w_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_each_plane_mesh(elm_type, ipe, jpe, kpe, pe_id,    &
     &          c_size, c_vert, loc_id, c_each, nb_rng, mesh, group)
!
      use cube_mesh_fiile_IO
!
      use set_cube_node
      use set_cube_ele_connect
      use set_import_cube
      use write_nod_grp_cube
      use write_ele_grp_cube
      use write_surf_grp_cube
      use merge_periodic_comm_table
!
      implicit none
!
      integer(kind = kint), intent(in) :: elm_type
      integer(kind = kint), intent(in) :: ipe, jpe, kpe
      integer(kind = kint), intent(in) :: pe_id
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(local_node_id_cube), intent(inout) :: loc_id
      type(size_of_each_cube), intent(inout) :: c_each
      type(neib_range_cube), intent(inout) :: nb_rng
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
      type(communication_table), save :: comm
!
! ***** set and write basic local model parameters
!                                       .. pe nod per 1 line

      call set_each_cube_resolution                                     &
     &   (elm_type, kpe, c_size, c_each)
      call set_range_4_neighbour(ipe, jpe, kpe, c_size, nb_rng)
!
!
! .....  1.parallel information (pe_id start from 0, not 1)
!                                       .. set neighbor pe
      call set_neigbouring_plane                                        &
     &   (c_size, nb_rng, pe_id, ipe, jpe, comm)
!
      call sort_neighboring_pes(comm, mesh%nod_comm)
!
! .....  2.mesh information (nodes and elements in partition)
!
      call set_range_4_nodeloop(c_size, kpe, nb_rng)
!
! ***** set coordinate off set (starting corner for pe node)
! ***** set nodal position off set (i,j,k starting position -1)
      call init_node_para_4_each_pe                                     &
     &   (c_size, ipe, jpe, kpe, nb_rng)
      call set_offset_of_domain(c_size, ipe, jpe, kpe, nb_rng)
      call set_node(c_size, c_each, c_vert, nb_rng,                     &
     &    ipe, jpe, loc_id, mesh%node)
!
! .....  2.2 element (connection)
!
      call set_ele_connect(c_size, c_each, nb_rng, loc_id,              &
     &    elm_type, ipe, jpe, mesh%ele)
!
! .....  3.import / export information
! ***** set and write import nodes
      call set_import_data                                              &
     &   (c_size, nb_rng, loc_id, ipe, jpe, comm)
!
! ***** set and write export nodes
      call set_export_data                                              &
     &   (c_size, nb_rng, loc_id, ipe, jpe, comm)
!
      call sort_communication_table(comm, mesh%nod_comm)
!
! .....  4.group information
      call const_node_group(c_size, c_each, loc_id,                     &
     &    ipe, jpe, kpe, group%nod_grp)
      call const_element_group                                          &
     &   (c_size, c_each, nb_rng, kpe, group%ele_grp)
      call const_surface_group                                          &
     &   (c_size, c_each, kpe, group%surf_grp)
!
      end subroutine set_each_plane_mesh
!
! ----------------------------------------------------------------------
!
      subroutine set_each_plane_filter(pe_id, cube_p, c_size, loc_id,   &
     &          c_each, nb_rng, FEM_elen, c_fils, cube_fil)
!
      use neib_nod_cube
!
      implicit none
!
      integer(kind = kint), intent(in) :: pe_id
!
      type(ctl_param_plane_mesh), intent(in) :: cube_p
      type(size_of_cube), intent(in) :: c_size
      type(local_node_id_cube), intent(in) :: loc_id
      type(size_of_each_cube), intent(in) :: c_each
      type(neib_range_cube), intent(in) :: nb_rng
      type(filter_data_4_plane), intent(in) :: cube_fil
!
      type(gradient_model_data_type), intent(inout) :: FEM_elen
      type(filterings_4_cubmesh), intent(inout) :: c_fils
!
!
      if(cube_p%iflag_filter .le. 0) return
      call alloc_work_4_filter_ele(c_size, c_each, c_fils%c_fil_ele)
!
      write(*,*) 'neighboring_node'
      call neighboring_node                                             &
     &   (pe_id, cube_p, c_size, c_each, nb_rng, loc_id,                &
     &    cube_fil, FEM_elen, c_fils%c_fil_nod)
!
      write(*,*) 'deallocate_work_4_filter_ele'
      call dealloc_work_4_filter_nod(c_fils%c_fil_ele)
!
      end subroutine set_each_plane_filter
!
! ----------------------------------------------------------------------
!
      subroutine write_each_plane_mesh                                  &
     &         (pe_id, cube_p, c_size, mesh, group)
!
      use m_fem_mesh_labels
      use cube_mesh_fiile_IO
!
      use mesh_data_IO
      use mesh_file_IO_b
      use set_mesh_file_names
!
      implicit none
!
      integer(kind = kint), intent(in) :: pe_id
!
      type(ctl_param_plane_mesh), intent(in) :: cube_p
      type(size_of_cube), intent(in) :: c_size
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
!
      character(len=kchara) :: binary_name
      integer(kind=kint ), parameter  ::  l_out = 10
      integer(kind = kint) :: ierr
!
      integer :: id_rank
!
      id_rank = int(pe_id - 1)
!
!       Mesh file output
      if(cube_p%mesh_file%iflag_format .eq. id_binary_file_fmt) then
        binary_name = set_mesh_file_name(cube_p%mesh_file%file_prefix,  &
     &                                   cube_p%mesh_file%iflag_format, &
     &                                   id_rank)
        call write_mesh_file_b(id_rank, binary_name,                    &
     &                         mesh, group, ierr)
      else
        call open_mesh_file(cube_p%mesh_file%file_prefix,               &
     &      l_out, id_rank, c_size)
        call write_geometry_data(l_out, id_rank, mesh)
        call write_mesh_groups(l_out, group)
        close(l_out)
      end if
!
!
      end subroutine write_each_plane_mesh
!
! ----------------------------------------------------------------------
!
      end module cubmesh_311
