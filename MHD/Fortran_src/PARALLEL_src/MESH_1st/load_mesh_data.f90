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
      end module load_mesh_data
