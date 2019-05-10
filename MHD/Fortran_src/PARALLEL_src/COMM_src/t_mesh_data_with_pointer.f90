!>@file   t_mesh_data_with_pointer.f90
!!@brief  module t_mesh_data_with_pointer
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy FEM mesh data from IO structure
!!
!!@verbatim
!!      subroutine init_mesh_geometry_type(mesh_p)
!!      subroutine finalize_mesh_geometry_type(mesh_p)
!!
!!      subroutine init_mesh_group_type(group_p)
!!      subroutine finalize_mesh_group_type(group_p)
!!
!!      subroutine link_pointer_mesh                                    &
!!     &         (mesh_org, group_org  mesh_p, group_p)
!!      subroutine link_pointer_elemesh(ele_mesh_org, ele_mesh_p)
!!
!!      subroutine mpi_input_mesh_p(mesh_file, mesh_p, group_p)
!!      subroutine input_mesh_p                                         &
!!     &         (id_rank, mesh_file, mesh_p, group_p, ierr)
!!      subroutine const_mesh_infos_p(id_rank, mesh_p, group_p)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_geometry_p), intent(inout) :: mesh_p
!!        type(mesh_groups_p), intent(inout) ::   group_p
!!@endverbatim
!
      module t_mesh_data_with_pointer
!
      use m_precision
      use m_machine_parameter
      use t_mesh_data
!
      implicit none
!
!>     Structure for grid data
!>        (position, connectivity, and communication)
      type mesh_geometry_p
!>     Structure for node communication
        type(communication_table), pointer :: nod_comm
!>     Structure for node position
        type(node_data), pointer ::           node
!>     Structure for element position and connectivity
        type(element_data), pointer ::        ele
!
!>     Structure for surface position and connectivity
        type(surface_data), pointer :: surf
!>     Structure for edge position and connectivity
        type(edge_data),  pointer :: edge
      end type mesh_geometry_p
!
!>     Structure for group data (node, element, surface, and infinity)
      type mesh_groups_p
!>     Structure for node group
        type (group_data), pointer ::             nod_grp
!>     Structure for element group
        type (group_data), pointer ::             ele_grp
!>     Structure for surface group
        type (surface_group_data), pointer ::     surf_grp
!
!>     Structure for node data on surface group
        type (surface_node_grp_data) ::   surf_nod_grp
!>     Structure for grometry data for surface group
        type (surface_group_geometry) ::  surf_grp_geom
!
!>     Structure for element group connectivity
        type (element_group_table), pointer :: tbls_ele_grp
!>     Structure for surface group connectivity
        type (surface_group_table), pointer :: tbls_surf_grp
!
!>     Structure for infinity surface
        type (scalar_surf_BC_list) ::    infty_grp
      end type mesh_groups_p
!
!>     Structure for mesh data
!>        (position, connectivity, group, and communication)
      type mesh_data_p
!>     Structure for grid data
        type(mesh_geometry_p) :: mesh
!>     Structure for group data
        type(mesh_groups_p) ::   group
      end type mesh_data_p
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_mesh_geometry_type(mesh_p)
!
      type(mesh_geometry_p), intent(inout) :: mesh_p
!
      allocate(mesh_p%nod_comm)
      allocate(mesh_p%node)
      allocate(mesh_p%ele)
!
      end subroutine init_mesh_geometry_type
!
!------------------------------------------------------------------
!
      subroutine finalize_mesh_geometry_type(mesh_p)
!
      type(mesh_geometry_p), intent(inout) :: mesh_p
!
      deallocate(mesh_p%nod_comm, mesh_p%node, mesh_p%ele)
!
      end subroutine finalize_mesh_geometry_type
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine init_mesh_group_type(group_p)
!
      type(mesh_groups_p), intent(inout) :: group_p
!
      allocate(group_p%nod_grp)
      allocate(group_p%ele_grp)
      allocate(group_p%surf_grp)
!
      allocate(group_p%tbls_ele_grp)
      allocate(group_p%tbls_surf_grp)
!
      end subroutine init_mesh_group_type
!
!------------------------------------------------------------------
!
      subroutine finalize_mesh_group_type(group_p)
!
      type(mesh_groups_p), intent(inout) :: group_p
!
      deallocate(group_p%nod_grp, group_p%ele_grp, group_p%surf_grp)
      deallocate(group_p%tbls_surf_grp, group_p%tbls_ele_grp)
!
      end subroutine finalize_mesh_group_type
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine init_element_mesh_type(mesh_p)
!
      type(mesh_geometry_p), intent(inout) :: mesh_p
!
      allocate(mesh_p%surf)
      allocate(mesh_p%edge)
!
      end subroutine init_element_mesh_type
!
!------------------------------------------------------------------
!
      subroutine finalize_element_mesh_type(mesh_p)
!
      type(mesh_geometry_p), intent(inout) :: mesh_p
!
      deallocate(mesh_p%surf, mesh_p%edge)
!
      end subroutine finalize_element_mesh_type
!
!------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_pointer_mesh                                      &
     &         (mesh_org, group_org, mesh_p, group_p)
!
      type(mesh_geometry), intent(in), target :: mesh_org
      type(mesh_groups), intent(in), target :: group_org
!
      type(mesh_geometry_p), intent(inout) :: mesh_p
      type(mesh_groups_p), intent(inout) :: group_p
!
!
      mesh_p%nod_comm => mesh_org%nod_comm
      mesh_p%node => mesh_org%node
      mesh_p%ele =>  mesh_org%ele
      mesh_p%surf => mesh_org%surf
      mesh_p%edge => mesh_org%edge
!
      group_p%nod_grp =>  group_org%nod_grp
      group_p%ele_grp =>  group_org%ele_grp
      group_p%surf_grp => group_org%surf_grp
!
      group_p%tbls_ele_grp =>  group_org%tbls_ele_grp
      group_p%tbls_surf_grp => group_org%tbls_surf_grp
!
      end subroutine link_pointer_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine unlink_pointer_mesh(mesh_p, group_p)
!
      type(mesh_geometry_p), intent(inout) :: mesh_p
      type(mesh_groups_p), intent(inout) :: group_p
!
!
      nullify(group_p%tbls_ele_grp, group_p%tbls_surf_grp)
      nullify(group_p%surf_grp, group_p%ele_grp, group_p%nod_grp)

      nullify(mesh_p%surf, mesh_p%edge)
      nullify(mesh_p%ele, mesh_p%node, mesh_p%nod_comm)
!
      end subroutine unlink_pointer_mesh
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine mpi_input_mesh_p(mesh_file, mesh_p, group_p)
!
      use calypso_mpi
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_file_IO_parameter
!
      use mesh_MPI_IO_select
      use set_nnod_4_ele_by_type
      use load_mesh_data
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry_p), intent(inout) :: mesh_p
      type(mesh_groups_p), intent(inout) ::   group_p
!
      type(mesh_data) :: fem_IO_m
!
!
      call sel_mpi_read_mesh                                            &
     &   (nprocs, my_rank, mesh_file, fem_IO_m%mesh, fem_IO_m%group)
!
!
      call set_mesh_geometry_data(fem_IO_m%mesh,                        &
     &    mesh_p%nod_comm, mesh_p%node, mesh_p%ele)
      call set_grp_data_from_IO(fem_IO_m%group,                         &
     &    group_p%nod_grp, group_p%ele_grp, group_p%surf_grp)
      call dealloc_groups_data(fem_IO_m%group)
!
      call set_3D_nnod_4_sfed_by_ele(mesh_p%ele%nnod_4_ele,             &
     &   mesh_p%surf%nnod_4_surf, mesh_p%edge%nnod_4_edge)
!
      end subroutine mpi_input_mesh_p
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh_p                                           &
     &         (id_rank, mesh_file, mesh_p, group_p, ierr)
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_file_IO_parameter
!
      use mesh_IO_select
      use set_nnod_4_ele_by_type
      use load_mesh_data
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry_p), intent(inout) :: mesh_p
      type(mesh_groups_p), intent(inout) ::   group_p
      integer(kind = kint), intent(inout) :: ierr
!
      type(mesh_data) :: fem_IO_i
!
!
      call sel_read_mesh                                                &
     &   (mesh_file, id_rank, fem_IO_i%mesh, fem_IO_i%group, ierr)
!
      call set_mesh_geometry_data(fem_IO_i%mesh,                        &
     &    mesh_p%nod_comm, mesh_p%node, mesh_p%ele)
      call set_grp_data_from_IO(fem_IO_i%group,                         &
     &    group_p%nod_grp, group_p%ele_grp, group_p%surf_grp)
      call dealloc_groups_data(fem_IO_i%group)
!
      call set_3D_nnod_4_sfed_by_ele(mesh_p%ele%nnod_4_ele,             &
     &    mesh_p%surf%nnod_4_surf, mesh_p%edge%nnod_4_edge)
!
      end subroutine input_mesh_p
!
! -----------------------------------------------------------------------
!
      subroutine const_mesh_infos_p(id_rank, mesh_p, group_p)
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_group_connects
      use t_surface_group_connect
      use t_surface_group_geometry
      use t_surface_data
      use t_edge_data
!
      use const_surface_data
      use set_surf_edge_mesh
      use set_connects_4_surf_group
      use const_mesh_information
!      use check_surface_groups
!
      integer, intent(in) :: id_rank
      type(mesh_geometry_p), intent(inout) :: mesh_p
      type(mesh_groups_p), intent(inout) ::   group_p
!
!
       if (iflag_debug.gt.0) write(*,*) 'const_nod_ele_infos'
      call const_nod_ele_infos(id_rank, mesh_p%node, mesh_p%ele,        &
     &    group_p%nod_grp, group_p%ele_grp, group_p%surf_grp)
!
      if (iflag_debug.gt.0) write(*,*) 'set_local_element_info'
      call set_local_element_info(mesh_p%surf, mesh_p%edge)
!
      if (iflag_debug.gt.0) write(*,*) 'set_surface_and_edge'
      call set_surface_and_edge                                         &
     &   (mesh_p%node, mesh_p%ele, mesh_p%surf, mesh_p%edge)
!
      if (iflag_debug.gt.0) write(*,*) 'const_ele_list_4_surface'
      call const_ele_list_4_surface(mesh_p%ele, mesh_p%surf)
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_node_4_surf_group'
      call set_node_4_surf_group(mesh_p%node, mesh_p%ele, mesh_p%surf,  &
     &    group_p%surf_grp, group_p%surf_nod_grp)
!      call check_surface_node_id(id_rank, group_p%surf_nod_grp)
!
!      if (iflag_debug.gt.0) then
!        call check_surf_nod_4_sheard_para                              &
!     &     (id_rank, group_p%surf_grp%num_grp, group_p%surf_nod_grp)
!      end if
!
!
       if (iflag_debug.eq.1) write(*,*) 'const_group_connectiviy_1st'
      call const_group_type_info                                        &
     &   (mesh_p%node, mesh_p%ele, mesh_p%surf, mesh_p%edge,            &
     &    group_p%ele_grp, group_p%surf_grp,                            &
     &    group_p%tbls_ele_grp, group_p%tbls_surf_grp)
!
      end subroutine const_mesh_infos_p
!
! ----------------------------------------------------------------------
!
      end module t_mesh_data_with_pointer
