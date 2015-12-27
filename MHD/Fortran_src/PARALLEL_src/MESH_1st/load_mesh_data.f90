!>@file   load_mesh_data.f90
!!@brief  module load_mesh_data
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Copy FEM mesh data from structure to 1st mesh module
!!
!!@verbatim
!!      subroutine input_mesh_1st(my_rank)
!!      subroutine output_mesh_1st(my_rank)
!!
!!      subroutine set_mesh_from_type(mesh, group)
!!      subroutine compare_mesh_type_vs_1st(my_rank, mesh, group)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!@endverbatim
      module load_mesh_data
!
      use m_precision
      use m_machine_parameter
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh_1st(my_rank)
!
      use load_mesh_type_data
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call input_mesh                                                   &
     &   (my_rank, nod_comm, node1, ele1, nod_grp1, ele_grp1, sf_grp1,  &
     &    surf1%nnod_4_surf, edge1%nnod_4_edge)
!
      end subroutine input_mesh_1st
!
! -----------------------------------------------------------------------
!
      subroutine output_mesh_1st(my_rank)
!
      use load_mesh_type_data
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call output_mesh(my_rank, nod_comm, node1, ele1,                  &
     &                 nod_grp1, ele_grp1, sf_grp1)
!
      end subroutine output_mesh_1st
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_mesh_from_type(mesh, group)
!
      use t_mesh_data
      use copy_mesh_structures
      use set_nnod_4_ele_by_type
!
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
!
      call copy_mesh_geometry_from_type(mesh, nod_comm, node1, ele1)
      call group_data_from_type(group)
!
      call allocate_sph_node_geometry(mesh%node)
      call allocate_ele_geometry_type(ele1)
      call set_3D_nnod_4_sfed_by_ele                                   &
     &   (ele1%nnod_4_ele, surf1%nnod_4_surf, edge1%nnod_4_edge)
!
      call deallocate_ele_connect_type(mesh%ele)
      call deallocate_node_geometry_type(mesh%node)
!
      end subroutine set_mesh_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine compare_mesh_type_vs_1st(my_rank, mesh, group)
!
      use t_mesh_data
      use compare_mesh_structures
!
      integer(kind = kint), intent(in)  :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
!
      call compare_mesh_type(my_rank, nod_comm, node1, ele1, mesh)
      call compare_group_type_vs_1st(my_rank, group)
!
      end subroutine compare_mesh_type_vs_1st
!
!  ---------------------------------------------------------------------
!
      end module load_mesh_data
