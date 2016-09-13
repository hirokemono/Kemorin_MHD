!>@file   load_mesh_data.f90
!!@brief  module load_mesh_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy FEM mesh data from IO structure
!!
!!@verbatim
!!      subroutine input_mesh                                           &
!!     &         (my_rank, mesh, group, nnod_4_surf, nnod_4_edge, ierr)
!!      subroutine input_mesh_geometry(my_rank, mesh, ierr)
!!      subroutine output_mesh(my_rank, mesh, group)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!
!!      subroutine set_mesh                                             &
!!     &         (mesh, group, nnod_4_surf, nnod_4_edge)
!!      subroutine set_mesh_geometry_data(mesh_IO, mesh)
!!
!!      subroutine set_grp_data_from_IO(nod_grp, ele_grp, surf_grp)
!!      subroutine set_grp_data_to_IO(nod_grp, ele_grp, surf_grp)
!!        type(mesh_groups), intent(inout) :: mesh_group_IO
!!        type(group_data), intent(inout) :: nod_grp, ele_grp
!!        type(surface_group_data), intent(inout) :: surf_grp
!!@endverbatim
!
      module load_mesh_data
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh                                             &
     &         (my_rank, mesh, group, nnod_4_surf, nnod_4_edge, ierr)
!
      use mesh_IO_select
      use set_nnod_4_ele_by_type
!
      integer(kind = kint), intent(in) :: my_rank
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      integer(kind = kint), intent(inout) :: nnod_4_surf, nnod_4_edge
      integer(kind = kint), intent(inout) :: ierr
!
      type(mesh_data) :: fem_IO_i
!
!
      call sel_read_mesh(my_rank, fem_IO_i, ierr)
      call set_mesh(fem_IO_i, mesh, group, nnod_4_surf, nnod_4_edge)
!
      end subroutine input_mesh
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh_geometry(my_rank, mesh, ierr)
!
      use mesh_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
      integer(kind = kint), intent(inout) :: ierr
!
      type(mesh_geometry) :: mesh_IO_i
!
!
      call sel_read_mesh_geometry(my_rank, mesh_IO_i, ierr)
      call set_mesh_geometry_data(mesh_IO_i, mesh)
!
      end subroutine input_mesh_geometry
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_mesh(my_rank, mesh, group)
!
      use mesh_IO_select
      use set_comm_table_4_IO
      use set_element_data_4_IO
      use copy_mesh_structures
!
      integer(kind = kint), intent(in) :: my_rank
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
      type(mesh_data) :: fem_IO_i
!
!
      call copy_comm_tbl_type(mesh%nod_comm, fem_IO_i%mesh%nod_comm)
      call copy_node_geometry_types(mesh%node, fem_IO_i%mesh%node)
      call copy_ele_connect_to_IO(mesh%ele, fem_IO_i%mesh%ele)
!
      call set_grp_data_to_IO                                           &
     &   (group%nod_grp, group%ele_grp, group%surf_grp, fem_IO_i%group)
!
!       save mesh information
      call sel_write_mesh_file(my_rank, fem_IO_i)
!
      end subroutine output_mesh
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_mesh                                               &
     &         (fem_IO, mesh, group, nnod_4_surf, nnod_4_edge)
!
      use set_nnod_4_ele_by_type
!
      type(mesh_data), intent(inout) :: fem_IO
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      integer(kind = kint), intent(inout) :: nnod_4_surf, nnod_4_edge
!
!
      call set_mesh_geometry_data(fem_IO%mesh, mesh)
      call set_grp_data_from_IO(fem_IO%group,                           &
     &    group%nod_grp, group%ele_grp, group%surf_grp)
      call dealloc_groups_data(fem_IO%group)
!
      call set_3D_nnod_4_sfed_by_ele                                    &
     &   (mesh%ele%nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      end subroutine set_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_mesh_geometry_data(mesh_IO, mesh)
!
      use set_comm_table_4_IO
      use set_element_data_4_IO
      use copy_mesh_structures
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call copy_comm_tbl_type(mesh_IO%nod_comm, mesh%nod_comm)
      call copy_node_geometry_types(mesh_IO%node, mesh%node)
      call copy_ele_connect_from_IO(mesh_IO%ele, mesh%ele)
!
      call deallocate_type_comm_tbl(mesh_IO%nod_comm)
      call dealloc_node_geometry_base(mesh_IO%node)
      call deallocate_ele_connect_type(mesh_IO%ele)
!
      call allocate_sph_node_geometry(mesh%node)
!
      end subroutine set_mesh_geometry_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_grp_data_from_IO                                   &
     &        (mesh_group_IO, nod_grp, ele_grp, surf_grp)
!
      use set_group_types_4_IO
!
      type(mesh_groups), intent(inout) :: mesh_group_IO
      type(group_data), intent(inout) :: nod_grp, ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
!
      call set_gruop_stracture(mesh_group_IO%nod_grp, nod_grp)
      call set_gruop_stracture(mesh_group_IO%ele_grp, ele_grp)
      call set_surf_grp_stracture(mesh_group_IO%surf_grp, surf_grp)
!
      end subroutine set_grp_data_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_grp_data_to_IO(nod_grp, ele_grp, surf_grp,         &
     &          mesh_group_IO)
!
      use set_group_types_4_IO
!
      type(group_data), intent(inout) :: nod_grp, ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
      type(mesh_groups), intent(inout) :: mesh_group_IO
!
!
      call set_gruop_stracture(nod_grp, mesh_group_IO%nod_grp)
      call set_gruop_stracture(ele_grp, mesh_group_IO%ele_grp)
      call set_surf_grp_stracture(surf_grp, mesh_group_IO%surf_grp)
!
      end subroutine set_grp_data_to_IO
!
!-----------------------------------------------------------------------
!
      end module load_mesh_data
