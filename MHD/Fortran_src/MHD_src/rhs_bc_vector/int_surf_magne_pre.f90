!
!      module int_surf_magne_pre
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine int_surf_magne_pre_ele                               &
!!     &         (iak_diff_uxb, node, ele, surf, sf_grp,                &
!!     &          iphys, nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1,          &
!!     &          FEM1_elen, fem1_wk, f1_l, f1_nl)
!!      subroutine int_surf_magne_monitor(i_field, iak_diff_uxb,        &
!!     &          node, ele, surf, sf_grp, iphys, nod_fld1,             &
!!     &          jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, fem1_wk,       &
!!     &          f1_l, f1_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld1
!!        type(jacobians_2d), intent(in) :: jac1_sf_grp_2d_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
!!        type(gradient_model_data_type), intent(in) :: FEM1_elen
!!        type(work_finite_element_mat), intent(inout) :: fem1_wk
!!        type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
!
      module int_surf_magne_pre
!
      use m_precision
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
!
      use m_control_parameter
      use m_phys_constants
      use m_surf_data_magne
!
      use int_surf_div_induct_tsr_sgs
      use int_surf_fixed_gradients
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_magne_pre_ele                                 &
     &         (iak_diff_uxb, node, ele, surf, sf_grp,                  &
     &          iphys, nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1,            &
     &          FEM1_elen, fem1_wk, f1_l, f1_nl)
!
      integer(kind= kint), intent(in) :: iak_diff_uxb
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld1
      type(jacobians_2d), intent(in) :: jac1_sf_grp_2d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
      type(gradient_model_data_type), intent(in) :: FEM1_elen
!
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
!
!
      call int_sf_grad_velocity(node, ele, surf, sf_grp,                &
     &    jac1_sf_grp_2d_q, rhs_tbl1, Bsf1_bcs%grad,                    &
     &    intg_point_t_evo, ak_d_magne, fem1_wk, f1_l)
!
       if (iflag_SGS_induction .ne. id_SGS_none                         &
     &     .and. iflag_commute_induction .eq. id_SGS_commute_ON) then
         call int_surf_div_induct_t_sgs                                 &
     &      (node, ele, surf, sf_grp, nod_fld1, jac1_sf_grp_2d_q,       &
     &       rhs_tbl1, FEM1_elen, Bsf1_bcs%sgs, intg_point_t_evo,       &
     &       ifilter_final, iak_diff_uxb, iphys%i_SGS_induct_t,         &
     &       iphys%i_velo, iphys%i_magne, fem1_wk, f1_nl)
      end if
!
!      call int_free_slip_surf_sph_out(node, ele, surf, sf_grp,         &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   Asf1_bcs%free_sph_out%ngrp_sf_dat,                            &
!     &   Asf1_bcs%free_sph_out%id_grp_sf_dat, &
!     &   iphys%i_vecp, fem_wk, f_l)
!      call int_free_slip_surf_sph_in(node, ele, surf, sf_grp,          &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   Asf1_bcs%free_sph_in%ngrp_sf_dat,                             &
!     &   Asf1_bcs%free_sph_in%id_grp_sf_dat,                           &
!     &   iphys%i_vecp, fem_wk, f_l)
!
      end subroutine int_surf_magne_pre_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_magne_monitor(i_field, iak_diff_uxb,          &
     &          node, ele, surf, sf_grp, iphys, nod_fld1,               &
     &          jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, fem1_wk,         &
     &          f1_l, f1_nl)
!
      integer(kind= kint), intent(in) :: i_field, iak_diff_uxb
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld1
      type(jacobians_2d), intent(in) :: jac1_sf_grp_2d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
      type(gradient_model_data_type), intent(in) :: FEM1_elen
!
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
!
!
      if (i_field .eq. iphys%i_b_diffuse) then
        call int_sf_grad_velocity(node, ele, surf, sf_grp,              &
     &      jac1_sf_grp_2d_q, rhs_tbl1, Bsf1_bcs%grad,                  &
     &      intg_point_t_evo, ak_d_magne, fem1_wk, f1_l)
      end if
!
      if (i_field .eq. iphys%i_SGS_induction) then
        if (iflag_SGS_induction .ne. id_SGS_none                        &
     &     .and. iflag_commute_induction .eq. id_SGS_commute_ON) then
          call int_surf_div_induct_t_sgs                                &
     &       (node, ele, surf, sf_grp, nod_fld1, jac1_sf_grp_2d_q,      &
     &        rhs_tbl1, FEM1_elen, Bsf1_bcs%sgs, intg_point_t_evo,      &
     &        ifilter_final, iak_diff_uxb, iphys%i_SGS_induct_t,        &
     &        iphys%i_velo, iphys%i_magne, fem1_wk, f1_nl)
        end if
      end if
!
!      call int_free_slip_surf_sph_out(node, ele, surf, sf_grp,         &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   Asf1_bcs%free_sph_out%ngrp_sf_dat,                            &
!     &   Asf1_bcs%free_sph_out%id_grp_sf_dat,                          &
!     &   iphys%i_vecp, fem_wk, f_l)
!      call int_free_slip_surf_sph_in(node, ele, surf, sf_grp,          &
!     &   nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, n_int,                  &
!     &   Asf1_bcs%free_sph_in%ngrp_sf_dat,                             &
!     &   Asf1_bcs%free_sph_in%id_grp_sf_dat,                           &
!     &   iphys%i_vecp, fem_wk, f_l)
!
      end subroutine int_surf_magne_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_magne_pre
