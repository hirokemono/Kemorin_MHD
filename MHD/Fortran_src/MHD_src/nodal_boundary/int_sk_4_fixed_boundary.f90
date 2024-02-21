!
!     module int_sk_4_fixed_boundary
!
!      Written by H. Matsui on Oct., 2005
!
!!      subroutine int_vol_sk_po_bc(iflag_commute_velo, ifilter_final,  &
!!     &          num_int, i_p_phi, node, ele, nod_fld, g_FEM, jac_3d_l,&
!!     &          rhs_tbl, FEM_elens, nod_bc_p, ak_diff, fem_wk, f_l)
!!      subroutine int_vol_sk_mp_bc(iflag_commute_magne, ifilter_final, &
!!     &          num_int, i_m_phi, node, ele, nod_fld, g_FEM, jac_3d_l,&
!!     &          rhs_tbl, FEM_elens, nod_bc_f, ak_diff, fem_wk, f_l)
!!      subroutine int_vol_sk_mag_p_ins_bc                              &
!!     &         (iflag_commute_magne, ifilter_final, num_int, i_m_phi, &
!!     &          node, ele, nod_fld, g_FEM, jac_3d_l, rhs_tbl,         &
!!     &          FEM_elens, nod_bc_fins, ak_diff, fem_wk, f_l)
!!
!!      subroutine int_sk_fixed_temp                                    &
!!     &         (iflag_commute, ifilter_final, num_int, i_temp,        &
!!     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,&
!!     &          nod_bc_t, ak_d, ak_diff, coef_imp, fem_wk, f_l)
!!      subroutine int_sk_4_fixed_velo(iflag_commute_velo,              &
!!     &          ifilter_final, num_int, i_velo, node, ele, nod_fld,   &
!!     &          fl_prop, g_FEM, jac_3d, rhs_tbl, FEM_elens,           &
!!     &          nod_bc_v, nod_bc_rot, ak_d, ak_diff, fem_wk, f_l)
!!      subroutine int_sk_4_fixed_vector                                &
!!     &         (iflag_commute, ifilter_final, num_int, i_field,       &
!!     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,&
!!     &          nod_bc, ak_d, coef_imp, ak_diff, fem_wk, f_l)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_3d), intent(in) :: jac_3d_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_f
!!        type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_t
!!        type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_c
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_sk_4_fixed_boundary
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_physical_property
      use t_SGS_control_parameter
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_FEM_SGS_model_coefs
      use t_nodal_bc_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sk_po_bc(iflag_commute_velo, ifilter_final,    &
     &          num_int, i_p_phi, node, ele, nod_fld, g_FEM, jac_3d_l,  &
     &          rhs_tbl, FEM_elens, nod_bc_p, ak_diff, fem_wk, f_l)
!
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
      use cal_ff_smp_to_ffs
!
      integer(kind = kint), intent(in) :: iflag_commute_velo
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind = kint), intent(in) :: i_p_phi
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_p
      real (kind = kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_poisson_surf(node, ele, nod_fld,         &
     &      g_FEM, jac_3d_l, rhs_tbl, FEM_elens, num_int,               &
     &      nod_bc_p%ibc_end, nod_bc_p%num_idx_ibc, nod_bc_p%ele_bc_id, &
     &      nod_bc_p%ibc_stack_smp, nod_bc_p%ibc_shape, ifilter_final,  &
     &      i_p_phi, ak_diff, fem_wk, f_l)
      else
        call int_vol_fixed_poisson_surf                                 &
     &    (node, ele, nod_fld, g_FEM, jac_3d_l, rhs_tbl, num_int,       &
     &     nod_bc_p%ibc_end, nod_bc_p%num_idx_ibc,                      &
     &     nod_bc_p%ele_bc_id, nod_bc_p%ibc_stack_smp,                  &
     &     nod_bc_p%ibc_shape, i_p_phi, fem_wk, f_l)
      end if
!
      call cal_ff_smp_2_ff                                              &
     &   (node, rhs_tbl, n_scalar, f_l%ff_smp, f_l%ff)
!
      end subroutine int_vol_sk_po_bc
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sk_mp_bc(iflag_commute_magne, ifilter_final,   &
     &          num_int, i_m_phi, node, ele, nod_fld, g_FEM, jac_3d_l,  &
     &          rhs_tbl, FEM_elens, nod_bc_f, ak_diff, fem_wk, f_l)
!
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
      use cal_ff_smp_to_ffs
!
      integer(kind = kint), intent(in) :: iflag_commute_magne
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind = kint), intent(in) :: i_m_phi
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_f
      real (kind = kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_poisson_surf(node, ele, nod_fld,         &
     &      g_FEM, jac_3d_l, rhs_tbl, FEM_elens, num_int,               &
     &      nod_bc_f%ibc_end, nod_bc_f%num_idx_ibc, nod_bc_f%ele_bc_id, &
     &      nod_bc_f%ibc_stack_smp, nod_bc_f%ibc_shape, ifilter_final,  &
     &      i_m_phi, ak_diff, fem_wk, f_l)
      else
        call int_vol_fixed_poisson_surf                                 &
     &     (node, ele, nod_fld, g_FEM, jac_3d_l, rhs_tbl, num_int,      &
     &      nod_bc_f%ibc_end, nod_bc_f%num_idx_ibc,                     &
     &      nod_bc_f%ele_bc_id, nod_bc_f%ibc_stack_smp,                 &
     &      nod_bc_f%ibc_shape, i_m_phi, fem_wk, f_l)
      end if
!
      call cal_ff_smp_2_ff                                              &
     &   (node, rhs_tbl, n_scalar, f_l%ff_smp, f_l%ff)
!      call check_ff(my_rank, n_scalar, node, f_l)
!
      end subroutine int_vol_sk_mp_bc
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sk_mag_p_ins_bc                                &
     &         (iflag_commute_magne, ifilter_final, num_int, i_m_phi,   &
     &          node, ele, nod_fld, g_FEM, jac_3d_l, rhs_tbl,           &
     &          FEM_elens, nod_bc_fins, ak_diff, fem_wk, f_l)
!
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
      use cal_ff_smp_to_ffs
!
      integer(kind = kint), intent(in) :: iflag_commute_magne
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind = kint), intent(in) :: i_m_phi
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_fins
      real (kind = kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_poisson_surf(node, ele, nod_fld,         &
     &      g_FEM, jac_3d_l, rhs_tbl, FEM_elens, num_int,               &
     &      nod_bc_fins%ibc_end, nod_bc_fins%num_idx_ibc,               &
     &      nod_bc_fins%ele_bc_id, nod_bc_fins%ibc_stack_smp,           &
     &      nod_bc_fins%ibc_shape, ifilter_final, i_m_phi,              &
     &      ak_diff, fem_wk, f_l)
      else
        call int_vol_fixed_poisson_surf                                 &
     &     (node, ele, nod_fld, g_FEM, jac_3d_l, rhs_tbl, num_int,      &
     &      nod_bc_fins%ibc_end, nod_bc_fins%num_idx_ibc,               &
     &      nod_bc_fins%ele_bc_id, nod_bc_fins%ibc_stack_smp,           &
     &      nod_bc_fins%ibc_shape, i_m_phi, fem_wk, f_l)
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
      subroutine int_sk_fixed_temp                                      &
     &         (iflag_commute, ifilter_final, num_int, i_temp,          &
     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,  &
     &          nod_bc_t, ak_d, ak_diff, coef_imp, fem_wk, f_l)
!
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
      integer(kind = kint), intent(in) :: iflag_commute
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind = kint), intent(in) :: i_temp
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(scaler_fixed_nod_bc_type), intent(in) :: nod_bc_t
!
      real(kind = kreal), intent(in) :: coef_imp
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
      real (kind = kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_scalar_surf                              &
     &     (node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,      &
     &      num_int, nod_bc_t%ibc_end, nod_bc_t%num_idx_ibc,            &
     &      nod_bc_t%ele_bc_id, nod_bc_t%ibc_stack_smp,                 &
     &      nod_bc_t%ibc_shape, ifilter_final, i_temp,                  &
     &      ak_diff, ak_d, coef_imp, fem_wk, f_l)
      else
        call int_vol_fixed_scalar_surf                                  &
     &     (node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, num_int,        &
     &      nod_bc_t%ibc_end, nod_bc_t%num_idx_ibc,                     &
     &      nod_bc_t%ele_bc_id, nod_bc_t%ibc_stack_smp,                 &
     &      nod_bc_t%ibc_shape, i_temp, ak_d, coef_imp, fem_wk, f_l)
      end if
!
      end subroutine int_sk_fixed_temp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_velo(iflag_commute_velo,                &
     &          ifilter_final, num_int, i_velo, node, ele, nod_fld,     &
     &          fl_prop, g_FEM, jac_3d, rhs_tbl, FEM_elens,             &
     &          nod_bc_v, nod_bc_rot, ak_d, ak_diff, fem_wk, f_l)
!
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
      integer(kind = kint), intent(in) :: iflag_commute_velo
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind = kint), intent(in) :: i_velo
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(vect_fixed_nod_bc_type), intent(in)  :: nod_bc_v
      type(scaler_rotaion_nod_bc_type), intent(in)  :: nod_bc_rot
!
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_sk_4_fixed_vector                                        &
     &   (iflag_commute_velo, ifilter_final, num_int, i_velo,           &
     &    node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,        &
     &    nod_bc_v, ak_d, fl_prop%coef_imp, ak_diff, fem_wk, f_l)
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
        call int_vol_fixed_rotate_sgs_surf                              &
     &     (node, ele, nod_fld, g_FEM, jac_3d,                          &
     &      rhs_tbl, FEM_elens, num_int, nod_bc_rot%ibc_end,            &
     &      nod_bc_rot%num_idx_ibc, nod_bc_rot%ele_bc_id,               &
     &      nod_bc_rot%ibc_stack_smp, nod_bc_rot%ibc_shape,             &
     &      ifilter_final, i_velo, ak_diff, ak_d, fl_prop%coef_imp,     &
     &      fem_wk, f_l)
      else
        call int_vol_fixed_rotate_surf                                  &
     &     (node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, num_int,        &
     &      nod_bc_rot%ibc_end, nod_bc_rot%num_idx_ibc,                 &
     &      nod_bc_rot%ele_bc_id, nod_bc_rot%ibc_stack_smp,             &
     &      nod_bc_rot%ibc_shape, i_velo, ak_d, fl_prop%coef_imp,       &
     &      fem_wk, f_l)
      end if
!
      end subroutine int_sk_4_fixed_velo
!
! ----------------------------------------------------------------------
!
      subroutine int_sk_4_fixed_vector                                  &
     &         (iflag_commute, ifilter_final, num_int, i_field,         &
     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,  &
     &          nod_bc, ak_d, coef_imp, ak_diff, fem_wk, f_l)
!
      use int_vol_fixed_field_ele
      use int_vol_fixed_fld_sgs_ele
!
      integer(kind = kint), intent(in) :: iflag_commute
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind = kint), intent(in) :: i_field
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc
!
      real(kind = kreal), intent(in) :: coef_imp
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
      real (kind = kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iflag_commute .eq. id_SGS_commute_ON) then
        call int_vol_fixed_sgs_vector_surf(node, ele, nod_fld, g_FEM,   &
     &      jac_3d, rhs_tbl, FEM_elens, num_int, nod_bc%nmax_idx_ibc2,  &
     &      nod_bc%ibc_end, nod_bc%num_idx_ibc,  nod_bc%ele_bc_id,      &
     &      nod_bc%ibc_stack_smp, nod_bc%ibc_shape, ifilter_final,      &
     &      i_field, ak_diff, ak_d, coef_imp, fem_wk, f_l)
      else
        call int_vol_fixed_vector_surf(node, ele, nod_fld,              &
     &      g_FEM, jac_3d, rhs_tbl, num_int, nod_bc%nmax_idx_ibc2,      &
     &      nod_bc%ibc_end, nod_bc%num_idx_ibc, nod_bc%ele_bc_id,       &
     &      nod_bc%ibc_stack_smp,  nod_bc%ibc_shape,                    &
     &      i_field, ak_d, coef_imp, fem_wk, f_l)
       end if
!
       end subroutine int_sk_4_fixed_vector
!
! ----------------------------------------------------------------------
!
      end module int_sk_4_fixed_boundary
