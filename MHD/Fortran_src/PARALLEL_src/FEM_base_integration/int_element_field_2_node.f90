!>@file   int_element_field_2_node.f90
!!@brief  module int_element_field_2_node
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief  Evaluate nodal field from element averaged field
!!
!!@verbatim
!!      subroutine cal_ele_scalar_2_node                                &
!!     &         (node, ele, jacs, rhs_tbl, m_lump,                     &
!!     &          ntot_comp_ele, ifield_ele, scalar_ele,                &
!!     &          ntot_comp_nod, ifield_nod, scalar_nod, fem_wk, rhs_l)
!!      subroutine cal_ele_vector_2_node                                &
!!     &         (node, ele, jacs, rhs_tbl, m_lump,                     &
!!     &          vector_ele, vector_nod, fem_wk, rhs_l)
!!      subroutine cal_ele_sym_tensor_2_node                            &
!!     &         (node, ele, jacs, rhs_tbl, m_lump,                     &
!!     &          tensor_ele, tensor_nod, fem_wk, rhs_l)
!!
!!      subroutine int_area_ele_scalar_2_node                           &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, iele_fsmp_stack,   &
!!     &          scalar_ele, fem_wk, rhs_l)
!!      subroutine int_area_ele_vector_2_node                           &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, iele_fsmp_stack,   &
!!     &          vector_ele, fem_wk, rhs_l)
!!
!!      subroutine int_grp_ele_scalar_2_node                            &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, iele_fsmp_stack,   &
!!     &          nele_grp, iele_grp, scalar_ele, fem_wk, rhs_l)
!!      subroutine int_grp_ele_vector_2_node                            &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, iele_fsmp_stack,   &
!!     &          nele_grp, iele_grp, vector_ele, fem_wk, rhs_l)
!!        type(node_data), intent(in) ::    node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_type), intent(in) :: jacs
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: rhs_l
!!@endverbatim
!
      module int_element_field_2_node
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
      use t_jacobians
      use t_fem_gauss_int_coefs
      use t_table_FEM_const
      use t_finite_element_mat
!
      use cal_skv_to_ff_smp
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_ele_scalar_2_node                                  &
     &         (node, ele, jacs, rhs_tbl, m_lump,                       &
     &          ntot_comp_ele, ifield_ele, scalar_ele,                  &
     &          ntot_comp_nod, ifield_nod, scalar_nod, fem_wk, rhs_l)
!
      use cal_ff_smp_to_ffs
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      integer(kind = kint), intent(in) :: ntot_comp_ele, ntot_comp_nod
      integer(kind = kint), intent(in) :: ifield_ele, ifield_nod
      real(kind = kreal), intent(in)                                    &
     &                    :: scalar_ele(ele%numele,ntot_comp_ele)
      real(kind = kreal), intent(inout)                                 &
     &                    :: scalar_nod(node%numnod,ntot_comp_nod)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
!
!
      call int_area_ele_scalar_2_node                                   &
     &   (node, ele, jacs%g_FEM, jacs%jac_3d, rhs_tbl,                  &
     &    ele%istack_ele_smp,  scalar_ele(1,ifield_ele), fem_wk, rhs_l)
      call cal_ff_smp_2_scalar(node, rhs_tbl, rhs_l%ff_smp,             &
     &    m_lump%ml, n_scalar, ione, scalar_nod(1,ifield_nod))
!
      end subroutine cal_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_ele_vector_2_node                                  &
     &         (node, ele, jacs, rhs_tbl, m_lump,                       &
     &          vector_ele, vector_nod, fem_wk, rhs_l)
!
      use cal_ff_smp_to_ffs
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      real(kind = kreal), intent(in)                                    &
     &                   :: vector_ele(ele%numele,n_vector)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: vector_nod(node%numnod,n_vector)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
!
!
      call int_area_ele_vector_2_node                                   &
     &   (node, ele, jacs%g_FEM, jacs%jac_3d, rhs_tbl,                  &
     &    ele%istack_ele_smp, vector_ele(1,1), fem_wk, rhs_l)
      call cal_ff_smp_2_vector(node, rhs_tbl, rhs_l%ff_smp,             &
     &    m_lump%ml, n_vector, ione, vector_nod(1,1))
!
      end subroutine cal_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_ele_sym_tensor_2_node                              &
     &         (node, ele, jacs, rhs_tbl, m_lump,                       &
     &          tensor_ele, tensor_nod, fem_wk, rhs_l)
!
      use cal_ff_smp_to_ffs
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      real(kind = kreal), intent(in)                                    &
     &                   :: tensor_ele(ele%numele,n_sym_tensor)
      real(kind = kreal), intent(inout)                                 &
     &                   :: tensor_nod(node%numnod,n_sym_tensor)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
!
!
      call cal_ele_vector_2_node(node, ele, jacs, rhs_tbl, m_lump,      &
     &    tensor_ele(1,1), tensor_nod(1,1), fem_wk, rhs_l)
      call cal_ele_vector_2_node(node, ele, jacs, rhs_tbl, m_lump,      &
     &    tensor_ele(1,4), tensor_nod(1,4), fem_wk, rhs_l)
!
      end subroutine cal_ele_sym_tensor_2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_area_ele_scalar_2_node                             &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, iele_fsmp_stack,     &
     &          scalar_ele, fem_wk, rhs_l)
!
      use fem_skv_scalar_on_ele
      use fem_skv_mass_mat
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: scalar_ele(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
!
!
      call reset_ff_smp(n_vector, node, rhs_l)
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      if (ele%nnod_4_ele .eq. num_t_linear) then
        call fem_skv_scalar_on_ele_m                                    &
     &     (ele%numele, ele%nnod_4_ele, iele_fsmp_stack,                &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, g_FEM%max_int_point,          &
     &      jac_3d%xjac, jac_3d%an, scalar_ele, fem_wk%sk6)
      else
        call fem_skv_mass_mat_diag_HRZ                                  &
     &     (ele%numele, ele%nnod_4_ele, np_smp, iele_fsmp_stack,        &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, g_FEM%max_int_point,          &
     &      jac_3d%xjac, jac_3d%an, fem_wk%sk6)
        call fem_skv_scalar_on_ele_HRZ_m(ele%numele, ele%nnod_4_ele,    &
     &      iele_fsmp_stack, ele%volume_ele, fem_wk%me_diag,            &
     &      scalar_ele, fem_wk%sk6)
      end if
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, rhs_l%ff_smp)
!
      end subroutine int_area_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_area_ele_vector_2_node                             &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, iele_fsmp_stack,     &
     &          vector_ele, fem_wk, rhs_l)
!
      use fem_skv_vector_on_ele
      use fem_skv_mass_mat
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: vector_ele(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
!
!
      call reset_ff_smp(n_vector, node, rhs_l)
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      if (ele%nnod_4_ele .eq. num_t_linear) then
        call fem_skv_vector_on_ele_m                                    &
     &     (ele%numele, ele%nnod_4_ele, iele_fsmp_stack,                &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, g_FEM%max_int_point,          &
     &      jac_3d%xjac, jac_3d%an, vector_ele, fem_wk%sk6)
      else
        call fem_skv_mass_mat_diag_HRZ                                  &
     &     (ele%numele, ele%nnod_4_ele, np_smp, iele_fsmp_stack,        &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, g_FEM%max_int_point,          &
     &      jac_3d%xjac, jac_3d%an, fem_wk%sk6)
        call fem_skv_vector_on_ele_HRZ_m(ele%numele, ele%nnod_4_ele,    &
     &      iele_fsmp_stack, ele%volume_ele, fem_wk%me_diag,            &
     &      vector_ele, fem_wk%sk6)
      end if
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, rhs_l%ff_smp)
!
      end subroutine int_area_ele_vector_2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_grp_ele_scalar_2_node                              &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, iele_fsmp_stack,     &
     &          nele_grp, iele_grp, scalar_ele, fem_wk, rhs_l)
!
      use fem_skv_scalar_on_ele
      use fem_grp_skv_mass_mat
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: scalar_ele(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
!
!
      call reset_ff_smp(n_vector, node, rhs_l)
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      if (ele%nnod_4_ele .eq. num_t_linear) then
        call fem_skv_scalar_on_ele_grp_m(ele%numele, ele%nnod_4_ele,    &
     &      iele_fsmp_stack, nele_grp, iele_grp,                        &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, g_FEM%max_int_point,          &
     &      jac_3d%xjac, jac_3d%an, scalar_ele, fem_wk%sk6)
      else
        call fem_grp_skv_mass_mat_diag_HRZ(ele%numele, ele%nnod_4_ele,  &
     &      np_smp, iele_fsmp_stack, nele_grp, iele_grp,                &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, g_FEM%max_int_point,          &
     &      jac_3d%xjac, jac_3d%an, fem_wk%sk6)
        call fem_skv_scalar_on_ele_grp_HRZ(ele%numele, ele%nnod_4_ele,  &
     &      iele_fsmp_stack, ele%volume_ele, nele_grp, iele_grp,        &
     &      fem_wk%me_diag, scalar_ele, fem_wk%sk6)
      end if
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, rhs_l%ff_smp)
!
      end subroutine int_grp_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_grp_ele_vector_2_node                              &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, iele_fsmp_stack,     &
     &          nele_grp, iele_grp, vector_ele, fem_wk, rhs_l)
!
      use fem_skv_vector_on_ele
      use fem_grp_skv_mass_mat
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: vector_ele(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
!
!
      call reset_ff_smp(n_vector, node, rhs_l)
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      if (ele%nnod_4_ele .eq. num_t_linear) then
        call fem_skv_vector_on_ele_grp_m(ele%numele, ele%nnod_4_ele,    &
     &      iele_fsmp_stack, nele_grp, iele_grp,                        &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, g_FEM%max_int_point,          &
     &      jac_3d%xjac, jac_3d%an, vector_ele, fem_wk%sk6)
      else
        call fem_grp_skv_mass_mat_diag_HRZ(ele%numele, ele%nnod_4_ele,  &
     &      np_smp, iele_fsmp_stack, nele_grp, iele_grp,                &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, g_FEM%max_int_point,          &
     &      jac_3d%xjac, jac_3d%an, fem_wk%sk6)
        call fem_skv_vector_on_ele_grp_HRZ(ele%numele, ele%nnod_4_ele,  &
     &      iele_fsmp_stack, ele%volume_ele, nele_grp, iele_grp,        &
     &      fem_wk%me_diag, vector_ele, fem_wk%sk6)
      end if
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, rhs_l%ff_smp)
!
      end subroutine int_grp_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      end module int_element_field_2_node
