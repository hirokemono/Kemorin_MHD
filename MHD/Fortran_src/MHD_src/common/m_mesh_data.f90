!m_mesh_data.f90
!      module m_mesh_data
!
!      Written by H. Matsui on July, 2006
!      Modified by H. Matsui on June, 2007
!
!
!!      subroutine const_mesh_informations(my_rank)
!!      subroutine const_nod_ele_infos_1st(my_rank)
!!      subroutine const_element_comm_tables_1st
!!      subroutine deallocate_mesh_infomations
!!      subroutine deallocate_nod_ele_infos
!
      module m_mesh_data
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
!
      implicit none
!
!>     Structure for grid data
!>        (position, connectivity, and communication)
      type(mesh_geometry), save :: mesh1
!mesh1%nod_comm
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_mesh_informations(my_rank)
!
      use m_geometry_data
      use m_group_data
      use const_mesh_information
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank,                                    &
     &    node1, ele1, surf1, edge1, nod_grp1, ele_grp1, sf_grp1,       &
     &    ele_grp_tbl1, sf_grp_tbl1, sf_grp_nod1)
!
      end subroutine const_mesh_informations
!
! ----------------------------------------------------------------------
!
      subroutine const_nod_ele_infos_1st(my_rank)
!
      use m_geometry_data
      use m_group_data
      use const_mesh_information
!
      integer(kind = kint), intent(in) :: my_rank
!
!
       if (iflag_debug.eq.1) write(*,*) 'const_nod_ele_infos'
      call const_nod_ele_infos(my_rank,                                 &
     &    node1, ele1, nod_grp1, ele_grp1, sf_grp1)
!
      end subroutine const_nod_ele_infos_1st
!
! ----------------------------------------------------------------------
!
      subroutine const_element_comm_tables_1st
!
      use m_geometry_data
      use m_nod_comm_table
      use const_element_comm_tables
!
!
      call const_element_comm_tbls(node1, ele1, surf1, edge1,           &
     &    mesh1%nod_comm, ele_comm, surf_comm, edge_comm)
!
      end subroutine const_element_comm_tables_1st
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_mesh_infomations
!
      use t_mesh_data
      use m_nod_comm_table
!
      use m_geometry_data
      use m_group_data
!
!
      call dealloc_mesh_infomations(mesh1%nod_comm,                     &
     &    node1, ele1, surf1, edge1, nod_grp1, ele_grp1, sf_grp1,       &
     &    ele_grp_tbl1, sf_grp_tbl1, sf_grp_nod1)
!
      end subroutine deallocate_mesh_infomations
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_nod_ele_infos
!
      use t_mesh_data
      use m_nod_comm_table
!
      use m_geometry_data
      use m_group_data
!
!
      call dealloc_nod_ele_infos                                        &
     &   (mesh1%nod_comm, node1, ele1, surf1, edge1,                    &
     &    nod_grp1, ele_grp1, sf_grp1)
!
      end subroutine deallocate_nod_ele_infos
!
! ----------------------------------------------------------------------
!
      end module m_mesh_data
