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
      use t_next_node_ele_4_node
      use t_work_FEM_integration
!
      implicit  none
!
!
      type(mesh_data), target, save :: MG_mesh(max_MG_level)
      type(element_geometry), save ::  MG_ele_mesh(max_MG_level)
!   mesh data structure
!
!      type(MG_itp_table), save :: MG_itp(max_MG_level)
!   interpolation table structure for interpolation
!
!
      integer(kind = kint), save :: iflag_MG_commute_by_ele = 0
      type(interpolate_table), save :: MG_c2f_ele_tbl(max_MG_level)
!   interpolation table structure for elemnent averaging
!
      type(jacobians_type), save :: MG_jacobians(max_MG_level)
!   jacobians for mesh
!
      type(tables_4_FEM_assembles), save ::    MG_FEM_tbl(max_MG_level)
      type(arrays_finite_element_mat), save :: MG_FEM_mat(max_MG_level)
!   table for FEM assemble
!
!      work structure for matrix assemble
!
      type(next_nod_ele_table) :: MG_next_table(max_MG_level)
!
      character(len = kchara), allocatable :: MG_mesh_file_head(:)
!
      character(len = kchara), allocatable :: MG_f2c_tbl_head(:)
      character(len = kchara), allocatable :: MG_c2f_tbl_head(:)
      character(len = kchara), allocatable :: MG_f2c_eletbl_head(:)
!
      integer(kind = kint), allocatable :: ifmt_MG_mesh_file(:)
      integer(kind = kint), allocatable :: ifmt_MG_table_file(:)
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine allocate_MG_mesh_file_heads
!
      use m_type_AMG_data
!
      allocate( MG_mesh_file_head(num_MG_level) )
!
      allocate( MG_f2c_tbl_head(num_MG_level) )
      allocate( MG_c2f_tbl_head(num_MG_level) )
      allocate( MG_f2c_eletbl_head(num_MG_level) )
!
      allocate( ifmt_MG_mesh_file(num_MG_level) )
      allocate( ifmt_MG_table_file(num_MG_level) )
!
      ifmt_MG_mesh_file =  0
      ifmt_MG_table_file = 0
!
      end subroutine allocate_MG_mesh_file_heads
!
!------------------------------------------------------------------
!
      subroutine deallocate_MG_mesh_file_heads
!
      deallocate( MG_mesh_file_head )
!
      deallocate( MG_f2c_tbl_head, MG_c2f_tbl_head )
      deallocate( MG_f2c_eletbl_head )
      deallocate( ifmt_MG_mesh_file, ifmt_MG_table_file )
!
      end subroutine deallocate_MG_mesh_file_heads
!
!------------------------------------------------------------------
!
      end module m_type_AMG_mesh
