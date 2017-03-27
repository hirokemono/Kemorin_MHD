!
!      module m_type_AMG_mesh
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine allocate_MG_mesh_file_heads
!      subroutine deallocate_MG_mesh_file_heads
!
      module m_type_AMG_mesh
!
      use m_precision
!
      use m_type_AMG_data
      use t_mesh_data
      use t_interpolate_table
      use t_jacobians
      use t_table_FEM_const
      use t_work_FEM_integration
!
      implicit  none
!
!
!      type(mesh_data), target, save :: MG_mesh(max_MG_level)
!      type(element_geometry), save ::  MG_ele_mesh(max_MG_level)
!   mesh data structure
!
      integer(kind = kint), save :: iflag_MG_commute_by_ele = 0
      type(interpolate_table), save :: MG_c2f_ele_tbl(max_MG_level)
!   interpolation table structure for elemnent averaging
!
      type(jacobians_type), save :: MG_jacobians(max_MG_level)
!   jacobians for mesh
!
!      type(tables_4_FEM_assembles), save ::    MG_FEM_tbl(max_MG_level)
      type(arrays_finite_element_mat), save :: MG_FEM_mat(max_MG_level)
!   table for FEM assemble
!
      end module m_type_AMG_mesh
