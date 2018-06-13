!> @file  parallel_sleeve_extension.f90
!!      module parallel_sleeve_extension
!!
!! @author  H. Matsui
!! @date Programmed in June, 2018
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine para_sleeve_extension(mesh, group, ele_mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!@endverbatim
!
      module parallel_sleeve_extension
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_group_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine para_sleeve_extension(mesh, group, ele_mesh)
!
      use t_para_double_numbering
      use t_next_node_ele_4_node
!
      use const_mesh_information
      use const_element_comm_tables
      use set_table_4_RHS_assemble
      use extend_comm_table
      use extend_element_connect
      use extend_group_table
      use copy_mesh_structures
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(element_geometry), intent(inout) :: ele_mesh
!
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save :: newgroup
      type(element_geometry), save :: new_ele_mesh
      type(next_nod_ele_table), save :: next_tbl
      type(parallel_double_numbering), save :: dbl_id1
!
!
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbl_only(mesh, ele_mesh)
!
      if (iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (mesh, next_tbl%neib_ele, next_tbl%neib_nod)
!
      call alloc_double_numbering(mesh%node%numnod, dbl_id1)
      call set_para_double_numbering                                    &
     &   (mesh%node%internal_node, mesh%nod_comm, dbl_id1)
!
      call extend_node_comm_table                                       &
     &   (mesh%nod_comm, mesh%node, dbl_id1, next_tbl%neib_nod,         &
     &    newmesh%nod_comm, newmesh%node)
!
      call extend_ele_connectivity                                      &
     &   (mesh%nod_comm, ele_mesh%ele_comm, mesh%node, mesh%ele,        &
     &    dbl_id1, next_tbl%neib_ele, newmesh%nod_comm, newmesh%node,   &
     &    newmesh%ele)
!
      call dealloc_double_numbering(dbl_id1)
!
      call alloc_sph_node_geometry(newmesh%node)
      call set_nod_and_ele_infos(newmesh%node, newmesh%ele)
      call const_element_comm_tbl_only(newmesh, new_ele_mesh)
!
!
      call s_extend_group_table                                         &
     &   (nprocs, newmesh%nod_comm, new_ele_mesh%ele_comm,              &
     &    newmesh%node, newmesh%ele, group, newgroup)
!
      call dealloc_next_nod_ele_table(next_tbl)
      call dealloc_ele_comm_tbl_only(mesh, ele_mesh)
      call dealloc_mesh_infomations(mesh, group, ele_mesh)
!
      call set_mesh_data_from_type(newmesh, newgroup, mesh%nod_comm,    &
     &    mesh%node, mesh%ele, ele_mesh%surf, ele_mesh%edge,            &
     &    group%nod_grp, group%ele_grp, group%surf_grp)
      call deallocate_sph_node_geometry(newmesh%node)
      call dealloc_ele_comm_tbl_only(newmesh, new_ele_mesh)
      call deallocate_ele_geometry_type(newmesh%ele)
      call deallocate_ele_param_smp_type(newmesh%ele)
      call deallocate_node_param_smp_type(newmesh%node)
!
      end subroutine para_sleeve_extension
!
! ----------------------------------------------------------------------
!
      end module parallel_sleeve_extension
