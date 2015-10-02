!
!     module cal_ff_smp_to_ffs_type
!
!     Written by H. Matsui on March, 2009
!
!      subroutine set_ff_nl_smp_2_ff_type(numdir, mesh, rhs_tbl, fem_rhs)
!        integer(kind = kint), intent(in) :: numdir
!        type(mesh_geometry), intent(in) :: mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(finite_ele_matrices), intent(inout) :: fem_rhs
!      subroutine cal_ff_smp_2_ff_type (numdir, mesh, rhs_tbl, rhs)
!        integer(kind = kint), intent(in) :: numdir
!        type(mesh_geometry), intent(in) :: mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(finite_ele_mat_node), intent(inout) :: rhs
!      subroutine cal_ff_smp_2_ml_type(mesh, rhs_tbl, rhs, lump)
!        type(mesh_geometry), intent(in) :: mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(finite_ele_mat_node), intent(in) :: rhs
!        type(lumped_mass_matrices), intent(inout) :: lump
!      subroutine cal_ff_smp_2_scalar_type(mesh, rhs_tbl, rhs,          &
!     &           lump, scalar)
!        type(mesh_geometry), intent(in) :: mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(finite_ele_mat_node), intent(in) :: rhs
!        type(lumped_mass_matrices), intent(in) :: lump
!        real(kind=kreal), intent(inout) :: scalar(mesh%node%numnod)
!      subroutine cal_ff_smp_2_vector_type(mesh, rhs_tbl, rhs,          &
!     &           lump, vector)
!        type(mesh_geometry), intent(in) :: mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(finite_ele_mat_node), intent(in) :: rhs
!        type(lumped_mass_matrices), intent(in) :: lump
!        real(kind=kreal), intent(inout) :: vector(mesh%node%numnod,3)
!      subroutine cal_ff_smp_2_tensor_type(mesh, rhs_tbl, rhs_t,        &
!     &           lump, tensor)
!        type(mesh_geometry), intent(in) :: mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(finite_ele_mat_node), intent(in) :: rhs_t
!        type(lumped_mass_matrices), intent(in) :: lump
!
      module cal_ff_smp_to_ffs_type
!
      use m_precision
!
      use m_machine_parameter
      use t_mesh_data
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine set_ff_nl_smp_2_ff_type(numdir, mesh, rhs_tbl, fem_rhs)
!
      use cal_rhs_node_order
!
      integer(kind = kint), intent(in) :: numdir
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(finite_ele_matrices), intent(inout) :: fem_rhs
!
!
      call s_set_ff_nl_smp_2_ff(mesh%node%numnod,                       &
     &    np_smp, mesh%node%max_nod_smp, mesh%node%istack_nod_smp,      &
     &    rhs_tbl%node_sort_list_smp, numdir,                           &
     &    fem_rhs%f_l%ff_smp, fem_rhs%f_nl%ff_smp,                      &
     &    fem_rhs%f_l%ff, fem_rhs%f_nl%ff)
!
      end subroutine set_ff_nl_smp_2_ff_type
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_ff_type (numdir, mesh, rhs_tbl, rhs)
!
      use cal_rhs_node_order
!
      integer(kind = kint), intent(in) :: numdir
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(finite_ele_mat_node), intent(inout) :: rhs
!
!
      call s_cal_ff_smp_2_ff (mesh%node%numnod,                         &
     &    np_smp, mesh%node%max_nod_smp, mesh%node%istack_nod_smp,      &
     &    rhs_tbl%node_sort_list_smp, numdir, rhs%ff_smp, rhs%ff)
!
      end subroutine cal_ff_smp_2_ff_type
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_ml_type(mesh, rhs_tbl, rhs, lump)
!
      use cal_rhs_node_order
!
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(finite_ele_mat_node), intent(in) :: rhs
!
      type(lumped_mass_matrices), intent(inout) :: lump
!
!
      call s_cal_ff_smp_2_ml (mesh%node%numnod,                         &
     &    np_smp, mesh%node%max_nod_smp, mesh%node%istack_nod_smp,      &
     &    rhs_tbl%node_sort_list_smp, lump%ml, lump%ml_o, rhs%ff_smp)
!
      end subroutine cal_ff_smp_2_ml_type
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_scalar_type(mesh, rhs_tbl, rhs,           &
     &           lump, scalar)
!
      use cal_rhs_node_order
!
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(finite_ele_mat_node), intent(in) :: rhs
      type(lumped_mass_matrices), intent(in) :: lump
!
      real(kind=kreal), intent(inout) :: scalar(mesh%node%numnod)
!
!
      call s_cal_ff_smp_2_scalar (mesh%node%numnod,                     &
     &    np_smp, mesh%node%max_nod_smp, mesh%node%istack_nod_smp,      &
     &    rhs_tbl%node_sort_list_smp, scalar, rhs%ff_smp, lump%ml)
!
      end subroutine cal_ff_smp_2_scalar_type
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_vector_type(mesh, rhs_tbl, rhs,           &
     &           lump, vector)
!
      use cal_rhs_node_order
!
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(finite_ele_mat_node), intent(in) :: rhs
      type(lumped_mass_matrices), intent(in) :: lump
!
      real(kind=kreal), intent(inout) :: vector(mesh%node%numnod,3)
!
!
      call s_cal_ff_smp_2_vector (mesh%node%numnod,                    &
     &    np_smp, mesh%node%max_nod_smp, mesh%node%istack_nod_smp,     &
     &    rhs_tbl%node_sort_list_smp, vector, rhs%ff_smp, lump%ml)
!
      end subroutine cal_ff_smp_2_vector_type
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_tensor_type(mesh, rhs_tbl, rhs_t,         &
     &           lump, tensor)
!
      use m_phys_constants
      use cal_rhs_node_order
!
      type(mesh_geometry), intent(in) :: mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(finite_ele_mat_node), intent(in) :: rhs_t
      type(lumped_mass_matrices), intent(in) :: lump
!
      real(kind=kreal), intent(inout)                                  &
     &                 :: tensor(mesh%node%numnod,n_sym_tensor)
!
!
      call s_cal_ff_smp_2_tensor (mesh%node%numnod,                    &
     &    np_smp, mesh%node%max_nod_smp, mesh%node%istack_nod_smp,     &
     &    rhs_tbl%node_sort_list_smp, tensor, rhs_t%ff_smp, lump%ml)
!
      end subroutine cal_ff_smp_2_tensor_type
!
! ----------------------------------------------------------------------
!
      end module cal_ff_smp_to_ffs_type
