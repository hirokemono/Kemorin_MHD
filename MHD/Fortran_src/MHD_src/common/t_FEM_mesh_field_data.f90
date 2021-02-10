!>@file   t_FEM_mesh_field_data.f90
!!@brief  module t_FEM_mesh_field_data
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in July, 2006
!!@n    Mmodified by H. Matsui in June, 2007
!!@n    Mmodified by H. Matsui in Sep., 2017
!
!> @brief Strcture for FEM mesh and nodal field
!!
!!@verbatim
!!      subroutine alloc_FEM_mesh_field_items(FEM_mesh_data)
!!      subroutine dealloc_FEM_mesh_field_items(FEM_mesh_data)
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_mesh_data
!!@endverbatim
      module t_FEM_mesh_field_data
!
      use m_precision
!
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_vector_for_solver
!
      implicit none
!
!>      Structure of FEM mesh and field structures
      type FEM_mesh_field_data
!>        Label for simulation
        character(len=kchara) :: label_sim
!
!>        Structure for FEM mesh data
!!         (position, connectivity, communication, and groups)
        type(mesh_data), pointer :: geofem
!>        Structure for nodal field data
        type(phys_data), pointer :: field
!>        Address for nodal fields
        type(phys_address) :: iphys
!
!>        Structure for vectors for solver
        type(vectors_4_solver) :: v_sol
      end type FEM_mesh_field_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_FEM_mesh_field_items(FEM_mesh_data)
!
      type(FEM_mesh_field_data), intent(inout) :: FEM_mesh_data
!
!
      allocate(FEM_mesh_data%geofem)
      allocate(FEM_mesh_data%field)
!
      end subroutine alloc_FEM_mesh_field_items
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_FEM_mesh_field_items(FEM_mesh_data)
!
      type(FEM_mesh_field_data), intent(inout) :: FEM_mesh_data
!
      deallocate(FEM_mesh_data%geofem, FEM_mesh_data%field)
!
      end subroutine dealloc_FEM_mesh_field_items
!
! ----------------------------------------------------------------------
!
      end module t_FEM_mesh_field_data
