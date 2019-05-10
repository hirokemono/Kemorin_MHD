!> @file  parallel_sleeve_extension.f90
!!      module parallel_sleeve_extension
!!
!! @author  H. Matsui
!! @date Programmed in June, 2018
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine para_sleeve_extension(mesh, group)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
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
      use t_comm_table
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine para_sleeve_extension(mesh, group)
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
      use set_nnod_4_ele_by_type
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
      type(communication_table), save :: ele_comm
      type(communication_table), save :: new_ele_comm
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save :: newgroup
      type(next_nod_ele_table), save :: next_tbl
      type(parallel_double_numbering), save :: dbl_id1
!
!
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbl_only'
      call const_element_comm_tbl_only(mesh, ele_comm)
!
      if (iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (mesh, next_tbl%neib_ele, next_tbl%neib_nod)
!
      call alloc_double_numbering(mesh%node%numnod, dbl_id1)
      if (iflag_debug.gt.0) write(*,*) 'set_para_double_numbering'
      call set_para_double_numbering                                    &
     &   (mesh%node%internal_node, mesh%nod_comm, dbl_id1)
!
      if (iflag_debug.gt.0) write(*,*) 'extend_node_comm_table'
      call extend_node_comm_table                                       &
     &   (mesh%nod_comm, mesh%node, dbl_id1, next_tbl%neib_nod,         &
     &    newmesh%nod_comm, newmesh%node)
!
      if (iflag_debug.gt.0) write(*,*) 'extend_ele_connectivity'
      call extend_ele_connectivity                                      &
     &   (mesh%nod_comm, ele_comm, mesh%node, mesh%ele,                 &
     &    dbl_id1, next_tbl%neib_ele, newmesh%nod_comm, newmesh%node,   &
     &    newmesh%ele)
      newmesh%ele%first_ele_type                                        &
     &   = set_cube_eletype_from_num(newmesh%ele%nnod_4_ele)
!
      call dealloc_double_numbering(dbl_id1)
!
      call alloc_sph_node_geometry(newmesh%node)
      call set_nod_and_ele_infos(newmesh%node, newmesh%ele)
      call const_element_comm_tbl_only(newmesh, new_ele_comm)
!
!
      if (iflag_debug.gt.0) write(*,*) 's_extend_group_table'
      call s_extend_group_table                                         &
     &   (nprocs, newmesh%nod_comm, new_ele_comm,                       &
     &    newmesh%node, newmesh%ele, group, newgroup)
!
      call dealloc_next_nod_ele_table(next_tbl)
      call dealloc_comm_table(ele_comm)
      call dealloc_numele_stack(mesh%ele)
      call dealloc_mesh_infomations(mesh, group)
!
      if (iflag_debug.gt.0) write(*,*) 'set_mesh_data_from_type'
      call set_mesh_data_from_type(newmesh, newgroup, mesh, group)
!
      call deallocate_sph_node_geometry(newmesh%node)
      call dealloc_comm_table(new_ele_comm)
      call dealloc_numele_stack(newmesh%ele)
      call deallocate_ele_geometry_type(newmesh%ele)
      call deallocate_ele_param_smp_type(newmesh%ele)
      call deallocate_node_param_smp_type(newmesh%node)
!
      end subroutine para_sleeve_extension
!
! ----------------------------------------------------------------------
!
      end module parallel_sleeve_extension
