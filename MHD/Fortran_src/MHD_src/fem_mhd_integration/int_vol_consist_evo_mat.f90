!>@file   int_vol_consist_evo_mat.f90
!!@brief  module int_vol_consist_evo_mat
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!      Modified in Aug, 2007
!
!>@brief  Integration for Crank-Nocolson matrix 
!!        with consistent mass matrix
!!
!!@verbatim
!!      subroutine int_vol_crank_mat_consist                            &
!!     &        (num_int, ele, fl_prop, cd_prop, ht_prop, cp_prop,      &
!!     &          g_FEM, jac_3d, rhs_tbl, MG_mat_fl_q, MG_mat_full_cd_q,&
!!     &          fem_wk, mat_velo, mat_magne, mat_temp, mat_light)
!!        type(element_data), intent(in) :: ele
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(table_mat_const), intent(in) :: MG_mat_fl_q
!!        type(table_mat_const), intent(in) :: MG_mat_full_cd_q
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!!@endverbatim
!
      module int_vol_consist_evo_mat
!
      use m_precision
      use m_phys_constants
!
      use t_physical_property
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_table_FEM_const
      use t_sorted_node_MHD
      use t_finite_element_mat
      use t_solver_djds
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_crank_mat_consist                              &
     &         (num_int, ele, fl_prop, cd_prop, ht_prop, cp_prop,       &
     &          g_FEM, jac_3d, rhs_tbl, MG_mat_fl_q, MG_mat_full_cd_q,  &
     &          fem_wk, mat_velo, mat_magne, mat_temp, mat_light)
!
      use fem_skv_mass_mat
      use cal_skv_to_ff_smp
      use add_skv1_to_crs_matrix
!
      integer(kind = kint), intent(in) :: num_int
      type(element_data), intent(in) :: ele
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_fl_q
      type(table_mat_const), intent(in) :: MG_mat_full_cd_q
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
        call fem_skv_mass_matrix                                        &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, ele%istack_ele_smp,                                 &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, num_int, jac_3d%xjac,         &
     &      jac_3d%an, jac_3d%an, k2, fem_wk%sk6)
!
        if (fl_prop%iflag_scheme .eq. id_Crank_nicolson_cmass           &
     &      .and. fl_prop%coef_velo.gt.0.0d0 ) then
          call add_skv1_to_crs_matrix33(ele, rhs_tbl,                   &
     &        MG_mat_fl_q, k2, fem_wk%sk6,                              &
     &        mat_velo%num_non0, mat_velo%aiccg)
        end if
!
        if ( ht_prop%iflag_scheme .eq. id_Crank_nicolson_cmass          &
     &      .and. ht_prop%coef_advect.gt.0.0d0 ) then
          call add_skv1_to_crs_matrix11(ele, rhs_tbl,                   &
     &        MG_mat_fl_q, k2, fem_wk%sk6,                              &
     &        mat_temp%num_non0, mat_temp%aiccg)
        end if
!
        if ( cp_prop%iflag_scheme .eq. id_Crank_nicolson_cmass          &
     &      .and. cp_prop%coef_advect .gt. 0.0d0) then
          call add_skv1_to_crs_matrix11(ele, rhs_tbl,                   &
     &        MG_mat_fl_q, k2, fem_wk%sk6,                              &
     &        mat_light%num_non0, mat_light%aiccg)
        end if
!
        if ( cd_prop%iflag_Bevo_scheme .eq. id_Crank_nicolson_cmass     &
     &      .and. cd_prop%coef_magne.gt.0.0d0) then
          call add_skv1_to_crs_matrix33                                 &
     &       (ele, rhs_tbl, MG_mat_full_cd_q,                           &
     &        k2, fem_wk%sk6, mat_magne%num_non0, mat_magne%aiccg)
        end if
!
        if ( cd_prop%iflag_Aevo_scheme .eq. id_Crank_nicolson_cmass     &
     &      .and. cd_prop%coef_magne.gt.0.0d0) then
          call add_skv1_to_crs_matrix33                                 &
     &       (ele, rhs_tbl, MG_mat_full_cd_q,                           &
     &        k2, fem_wk%sk6, mat_magne%num_non0, mat_magne%aiccg)
        end if
      end do
!
      end subroutine int_vol_crank_mat_consist
!
! ----------------------------------------------------------------------
!
      end module int_vol_consist_evo_mat
