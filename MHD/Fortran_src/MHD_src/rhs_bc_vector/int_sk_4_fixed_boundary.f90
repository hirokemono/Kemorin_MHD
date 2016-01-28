!
!     module int_sk_4_fixed_boundary
!
!      Written by H. Matsui on Oct., 2005
!
!!      subroutine int_vol_sk_po_bc(i_p_phi, iak_diff_v, node, ele,     &
!!     &         nod_fld, jac_3d_l, rhs_tbl, FEM_elens, fem_wk, f_l)
!!      subroutine int_vol_sk_mp_bc(i_m_phi, iak_diff_b, node, ele,     &
!!     &          nod_fld, jac_3d_l, rhs_tbl, FEM_elens, fem_wk, f_l)
!!      subroutine int_vol_sk_mag_p_ins_bc(i_m_phi, iak_diff_b,         &
!!     &          node, ele, nod_fld, jac_3d_l, rhs_tbl, FEM_elens,     &
!!     &          fem_wk, f_l)
!!
!!      subroutine int_sk_4_fixed_temp(i_temp, iak_diff_t, node, ele,   &
!!     &          nod_fld, jac1_3d, rhs_tbl, FEM_elens, fem_wk, f_l)
!!      subroutine int_sk_4_fixed_part_temp(i_par_temp, iak_diff_t,     &
!!     &          node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,      &
!!     &          fem_wk, f_l)
!!      subroutine int_sk_4_fixed_velo(i_velo, iak_diff_v, node, ele,   &
!!     &          nod_fld, jac1_3d, rhs_tbl, FEM_elens, fem_wk, f_l)
!!      subroutine int_sk_4_fixed_vector_p(i_vecp, iak_diff_b,          &
!!     &          node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,      &
!!     &          fem_wk, f_l)
!!      subroutine int_sk_4_fixed_magne(i_magne, iak_diff_b,            &
!!     &          node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,      &
!!     &          fem_wk, f_l)
!!      subroutine int_sk_4_fixed_composition(i_light, iak_diff_c,      &
!!     &          node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,      &
!!     &          fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac1_3d
!!        type(jacobians_3d), intent(in) :: jac_3d_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_sk_4_fixed_boundary
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
      use m_control_parameter
      use m_t_int_parameter
      use m_ele_material_property
      use m_SGS_model_coefs
!
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sk_po_bc(i_p_phi, iak_diff_v, node, ele,       &
     &          nod_fld, jac_3d_l, rhs_tbl, FEM_elens, fem_wk, f_l)
!
      use m_bc_data_velo
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
      use cal_ff_smp_to_ffs
!
      integer(kind = kint), intent(in) :: i_p_phi, iak_diff_v
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_poisson_surf                             &
     &    (node, ele, nod_fld, jac_3d_l, rhs_tbl, FEM_elens,            &
     &     intg_point_poisson, nod_bc1_p%ibc_end,                       &
     &     nod_bc1_p%num_idx_ibc, nod_bc1_p%ele_bc_id,                  &
     &     nod_bc1_p%ibc_stack_smp, nod_bc1_p%ibc_shape,                &
     &     ifilter_final, i_p_phi, ak_diff(1,iak_diff_v), fem_wk, f_l)
      else
        call int_vol_fixed_poisson_surf                                 &
     &    (node, ele, nod_fld, jac_3d_l, rhs_tbl, intg_point_poisson,   &
     &     nod_bc1_p%ibc_end, nod_bc1_p%num_idx_ibc,                    &
     &     nod_bc1_p%ele_bc_id, nod_bc1_p%ibc_stack_smp,                &
     &     nod_bc1_p%ibc_shape, i_p_phi, fem_wk, f_l)
      end if
!
      call cal_ff_smp_2_ff                                              &
     &   (node, rhs_tbl, n_scalar, f_l%ff_smp, f_l%ff)
!
      end subroutine int_vol_sk_po_bc
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sk_mp_bc(i_m_phi, iak_diff_b, node, ele,       &
     &          nod_fld, jac_3d_l, rhs_tbl, FEM_elens, fem_wk, f_l)
!
      use m_bc_data_magne
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
      use cal_ff_smp_to_ffs
!
      integer(kind = kint), intent(in) :: i_m_phi, iak_diff_b
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_poisson_surf                             &
     &     (node, ele, nod_fld, jac_3d_l, rhs_tbl, FEM_elens,           &
     &      intg_point_poisson,  nod_bc1_f%ibc_end,                     &
     &      nod_bc1_f%num_idx_ibc, nod_bc1_f%ele_bc_id,                 &
     &      nod_bc1_f%ibc_stack_smp, nod_bc1_f%ibc_shape,               &
     &      ifilter_final, i_m_phi, ak_diff(1,iak_diff_b), fem_wk, f_l)
      else
        call int_vol_fixed_poisson_surf                                 &
     &     (node, ele, nod_fld, jac_3d_l, rhs_tbl, intg_point_poisson,  &
     &      nod_bc1_f%ibc_end, nod_bc1_f%num_idx_ibc,                   &
     &      nod_bc1_f%ele_bc_id, nod_bc1_f%ibc_stack_smp,               &
     &      nod_bc1_f%ibc_shape, i_m_phi, fem_wk, f_l)
      end if
!
      call cal_ff_smp_2_ff                                              &
     &   (node, rhs_tbl, n_scalar, f_l%ff_smp, f_l%ff)
!      call check_ff(my_rank, n_scalar, node%numnod, f_l)
!
      end subroutine int_vol_sk_mp_bc
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sk_mag_p_ins_bc(i_m_phi, iak_diff_b,           &
     &          node, ele, nod_fld, jac_3d_l, rhs_tbl, FEM_elens,       &
     &          fem_wk, f_l)
!
      use m_bc_data_magne
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
      use cal_ff_smp_to_ffs
!
      integer(kind = kint), intent(in) :: i_m_phi, iak_diff_b
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_poisson_surf(node, ele, nod_fld,         &
     &      jac_3d_l, rhs_tbl, FEM_elens, intg_point_poisson,           &
     &      nod_bc1_fins%ibc_end, nod_bc1_fins%num_idx_ibc,             &
     &      nod_bc1_fins%ele_bc_id, nod_bc1_fins%ibc_stack_smp,         &
     &      nod_bc1_fins%ibc_shape,  ifilter_final, i_m_phi,            &
     &      ak_diff(1,iak_diff_b), fem_wk, f_l)
      else
        call int_vol_fixed_poisson_surf(node, ele, nod_fld,             &
     &      jac_3d_l, rhs_tbl, intg_point_poisson,                      &
     &      nod_bc1_fins%ibc_end, nod_bc1_fins%num_idx_ibc,             &
     &      nod_bc1_fins%ele_bc_id, nod_bc1_fins%ibc_stack_smp,         &
     &      nod_bc1_fins%ibc_shape, i_m_phi, fem_wk, f_l)
      end if
!
      call cal_ff_smp_2_ff                                              &
     &   (node, rhs_tbl, n_scalar, f_l%ff_smp, f_l%ff)
!
      end subroutine int_vol_sk_mag_p_ins_bc
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_temp(i_temp, iak_diff_t, node, ele,     &
     &          nod_fld, jac1_3d, rhs_tbl, FEM_elens, fem_wk, f_l)
!
      use m_bc_data_ene
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
      integer(kind = kint), intent(in) :: i_temp, iak_diff_t
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac1_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_temp .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_scalar_surf(node, ele, nod_fld,          &
     &      jac1_3d, rhs_tbl, FEM_elens, intg_point_t_evo,              &
     &      nod_bc1_t%ibc_end, nod_bc1_t%num_idx_ibc,                   &
     &      nod_bc1_t%ele_bc_id, nod_bc1_t%ibc_stack_smp,               &
     &      nod_bc1_t%ibc_shape, ifilter_final, i_temp,                 &
     &      ak_diff(1,iak_diff_t), ak_d_temp, coef_imp_t, fem_wk, f_l)
      else
        call int_vol_fixed_scalar_surf (node, ele, nod_fld,             &
     &      jac1_3d, rhs_tbl, intg_point_t_evo,                         &
     &      nod_bc1_t%ibc_end, nod_bc1_t%num_idx_ibc,                   &
     &      nod_bc1_t%ele_bc_id, nod_bc1_t%ibc_stack_smp,               &
     &      nod_bc1_t%ibc_shape, i_temp, ak_d_temp,                     &
     &      coef_imp_t, fem_wk, f_l)
      end if
!
      end subroutine int_sk_4_fixed_temp
!
! ----------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_part_temp(i_par_temp, iak_diff_t,       &
     &          node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,        &
     &          fem_wk, f_l)
!
      use m_bc_data_ene
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
      integer(kind = kint), intent(in) :: i_par_temp, iak_diff_t
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac1_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_temp .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_scalar_surf(node, ele, nod_fld,          &
     &      jac1_3d, rhs_tbl, FEM_elens, intg_point_t_evo,              &
     &      nod_bc1_t%ibc_end, nod_bc1_t%num_idx_ibc,                   &
     &      nod_bc1_t%ele_bc_id, nod_bc1_t%ibc_stack_smp,               &
     &      nod_bc1_t%ibc_shape, ifilter_final, i_par_temp,             &
     &      ak_diff(1,iak_diff_t), ak_d_temp, coef_imp_t, fem_wk, f_l)
      else
        call int_vol_fixed_scalar_surf(node, ele, nod_fld,              &
     &      jac1_3d, rhs_tbl, intg_point_t_evo,                         &
     &      nod_bc1_t%ibc_end, nod_bc1_t%num_idx_ibc,                   &
     &      nod_bc1_t%ele_bc_id, nod_bc1_t%ibc_stack_smp,               &
     &      nod_bc1_t%ibc_shape, i_par_temp, ak_d_temp,                 &
     &      coef_imp_t, fem_wk, f_l)
      end if
!
      end subroutine int_sk_4_fixed_part_temp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_velo(i_velo, iak_diff_v, node, ele,     &
     &          nod_fld, jac1_3d, rhs_tbl, FEM_elens, fem_wk, f_l)
!
      use m_bc_data_velo
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
      integer(kind = kint), intent(in) :: i_velo, iak_diff_v
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac1_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_vector_surf                              &
     &     (node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,            &
     &      intg_point_t_evo, nod_bc1_v%nmax_idx_ibc,                   &
     &      nod_bc1_v%ibc_end, nod_bc1_v%num_idx_ibc,                   &
     &      nod_bc1_v%ele_bc_id, nod_bc1_v%ibc_stack_smp,               &
     &      nod_bc1_v%ibc_shape, ifilter_final, i_velo,                 &
     &      ak_diff(1,iak_diff_v), ak_d_velo, coef_imp_v, fem_wk, f_l)
!
        call int_vol_fixed_rotate_sgs_surf                              &
     &     (node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,            &
     &      intg_point_t_evo, nod_bc1_rot%ibc_end,                      &
     &      nod_bc1_rot%num_idx_ibc, nod_bc1_rot%ele_bc_id,             &
     &      nod_bc1_rot%ibc_stack_smp, nod_bc1_rot%ibc_shape,           &
     &      ifilter_final, i_velo, ak_diff(1,iak_diff_v),               &
     &      ak_d_velo, coef_imp_v, fem_wk, f_l)
      else
        call int_vol_fixed_vector_surf                                  &
     &     (node, ele, nod_fld, jac1_3d, rhs_tbl, intg_point_t_evo,     &
     &      nod_bc1_v%nmax_idx_ibc, nod_bc1_v%ibc_end,                  &
     &      nod_bc1_v%num_idx_ibc, nod_bc1_v%ele_bc_id,                 &
     &      nod_bc1_v%ibc_stack_smp, nod_bc1_v%ibc_shape,               &
     &      i_velo, ak_d_velo, coef_imp_v, fem_wk, f_l)
!
        call int_vol_fixed_rotate_surf                                  &
     &     (node, ele, nod_fld, jac1_3d, rhs_tbl, intg_point_t_evo,     &
     &      nod_bc1_rot%ibc_end, nod_bc1_rot%num_idx_ibc,               &
     &      nod_bc1_rot%ele_bc_id, nod_bc1_rot%ibc_stack_smp,           &
     &      nod_bc1_rot%ibc_shape, i_velo, ak_d_velo, coef_imp_v,       &
     &      fem_wk, f_l)
      end if
!
      end subroutine int_sk_4_fixed_velo
!
! ----------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_vector_p(i_vecp, iak_diff_b,            &
     &          node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,        &
     &          fem_wk, f_l)
!
      use m_bc_data_magne
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
      integer(kind = kint), intent(in) :: i_vecp, iak_diff_b
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac1_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_vector_surf                              &
     &     (node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,            &
     &      intg_point_t_evo, nod_bc1_a%nmax_idx_ibc,                   &
     &      nod_bc1_a%ibc_end, nod_bc1_a%num_idx_ibc,                   &
     &      nod_bc1_a%ele_bc_id, nod_bc1_a%ibc_stack_smp,               &
     &      nod_bc1_a%ibc_shape, ifilter_final, i_vecp,                 &
     &      ak_diff(1,iak_diff_b), ak_d_magne, coef_imp_b, fem_wk, f_l)
      else
        call int_vol_fixed_vector_surf                                  &
     &     (node, ele, nod_fld, jac1_3d, rhs_tbl, intg_point_t_evo,     &
     &      nod_bc1_a%nmax_idx_ibc, nod_bc1_a%ibc_end,                  &
     &      nod_bc1_a%num_idx_ibc, nod_bc1_a%ele_bc_id,                 &
     &      nod_bc1_a%ibc_stack_smp, nod_bc1_a%ibc_shape,               &
     &      i_vecp, ak_d_magne, coef_imp_b, fem_wk, f_l)
       end if
!
       end subroutine int_sk_4_fixed_vector_p
!
! ----------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_magne(i_magne, iak_diff_b,              &
     &          node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,        &
     &          fem_wk, f_l)
!
      use m_bc_data_magne
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
      integer(kind = kint), intent(in) :: i_magne, iak_diff_b
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac1_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_vector_surf                              &
     &     (node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,            &
     &      intg_point_t_evo, nod_bc1_b%nmax_idx_ibc2,                  &
     &      nod_bc1_b%ibc_end, nod_bc1_b%num_idx_ibc,                   &
     &      nod_bc1_b%ele_bc_id, nod_bc1_b%ibc_stack_smp,               &
     &      nod_bc1_b%ibc_shape, ifilter_final, i_magne,                &
     &      ak_diff(1,iak_diff_b), ak_d_magne, coef_imp_b, fem_wk, f_l)
      else
        call int_vol_fixed_vector_surf                                  &
     &     (node, ele, nod_fld, jac1_3d, rhs_tbl,                       &
     &      intg_point_t_evo, nod_bc1_b%nmax_idx_ibc2,                  &
     &      nod_bc1_b%ibc_end, nod_bc1_b%num_idx_ibc,                   &
     &      nod_bc1_b%ele_bc_id, nod_bc1_b%ibc_stack_smp,               &
     &      nod_bc1_b%ibc_shape, i_magne, ak_d_magne, coef_imp_b,       &
     &      fem_wk, f_l)
       end if
!
       end subroutine int_sk_4_fixed_magne
!
! ----------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_composition(i_light, iak_diff_c,        &
     &          node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,        &
     &          fem_wk, f_l)
!
      use m_bc_data_ene
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
      integer(kind = kint), intent(in) :: i_light, iak_diff_c
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac1_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_composit .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_scalar_surf                              &
     &     (node, ele, nod_fld, jac1_3d, rhs_tbl, FEM_elens,            &
     &      intg_point_t_evo, nod_bc1_c%ibc_end,                        &
     &      nod_bc1_c%num_idx_ibc, nod_bc1_c%ele_bc_id,                 &
     &      nod_bc1_c%ibc_stack_smp, nod_bc1_c%ibc_shape,               &
     &      ifilter_final, i_light, ak_diff(1,iak_diff_c),              &
     &      ak_d_composit, coef_imp_c, fem_wk, f_l)
      else
        call int_vol_fixed_scalar_surf                                  &
     &     (node, ele, nod_fld, jac1_3d, rhs_tbl,                       &
     &      intg_point_t_evo, nod_bc1_c%ibc_end,                        &
     &      nod_bc1_c%num_idx_ibc, nod_bc1_c%ele_bc_id,                 &
     &      nod_bc1_c%ibc_stack_smp, nod_bc1_c%ibc_shape,               &
     &      i_light, ak_d_composit, coef_imp_c, fem_wk, f_l)
      end if
!
       end subroutine int_sk_4_fixed_composition
!
! ----------------------------------------------------------------------
!
      end module int_sk_4_fixed_boundary
