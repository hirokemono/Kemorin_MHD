!>@file   t_FEM_mesh_field_4_viz.f90
!!@brief  module t_FEM_mesh_field_4_viz
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in July, 2006
!!@n    Modified by H. Matsui in June, 2007
!!@n    Modified by H. Matsui in Sep., 2017
!!@n    Modified by H. Matsui in Feb., 2021
!
!> @brief Strcture for FEM mesh and nodal field
!!
!!@verbatim
!!      subroutine alloc_FEM_mesh_field_4_viz(FEM_mesh_fld_v)
!!      subroutine dealloc_FEM_mesh_field_4_viz(FEM_mesh_fld_v)
!!        type(FEM_mesh_field_for_viz), intent(inout) :: FEM_mesh_fld_v
!!@endverbatim
      module t_FEM_mesh_field_4_viz
!
      use m_precision
!
      use t_mesh_data
      use t_phys_data
      use t_vector_for_solver
!
      implicit none
!
!>      Structure of FEM mesh and field structures
      type FEM_mesh_field_for_viz
!>        Label for simulation
        character(len=kchara) :: label_sim
!
!>        Structure for FEM mesh data
!!         (position, connectivity, communication, and groups)
        type(mesh_data) :: geofem
!>        Structure for nodal field data
        type(phys_data) :: field
!
!>        Structure for vectors for solver
        type(vectors_4_solver) :: v_sol
      end type FEM_mesh_field_for_viz
!
! ----------------------------------------------------------------------
!
!      contains
!
! ----------------------------------------------------------------------
!
!      subroutine alloc_FEM_mesh_field_4_viz(FEM_mesh_fld_v)
!
!      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_mesh_fld_v
!
!      allocate(FEM_mesh_fld_v%geofem)
!      allocate(FEM_mesh_fld_v%field)
!
!      end subroutine alloc_FEM_mesh_field_4_viz
!
! ----------------------------------------------------------------------
!
!      subroutine dealloc_FEM_mesh_field_4_viz(FEM_mesh_fld_v)
!
!      type(FEM_mesh_field_for_viz), intent(inout) :: FEM_mesh_fld_v
!
!      deallocate(FEM_mesh_fld_v%geofem, FEM_mesh_fld_v%field)
!
!      end subroutine dealloc_FEM_mesh_field_4_viz
!
! ----------------------------------------------------------------------
!
      end module t_FEM_mesh_field_4_viz
