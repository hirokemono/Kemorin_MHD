!int_surf_fixed_gradients.f90
!      module int_surf_fixed_gradients
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_sf_h_flux                                        &
!!     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,         &
!!     &          n_int, fem_wk, f_l)
!!      subroutine int_sf_torque                                        &
!!     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,         &
!!     &          n_int, fem_wk, f_l)
!!      subroutine int_sf_grad_vecp                                     &
!!     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,         &
!!     &          n_int, fem_wk, f_l)
!!      subroutine int_sf_grad_magne                                    &
!!     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,         &
!!     &          n_int, fem_wk, f_l)
!!      subroutine int_sf_grad_composition                              &
!!     &          (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,        &
!!     &           n_int, fem_wk, f_l)
!!
!!      subroutine int_sf_grad_press                                    &
!!     &         (node, ele, surf, sf_grp, jac_sf_grp_l, rhs_tbl,       &
!!     &          n_int, fem_wk, f_l)
!!      subroutine int_sf_grad_magne_p                                  &
!!     &         (node, ele, surf, sf_grp, jac_sf_grp_l, rhs_tbl,       &
!!     &          n_int, fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_surf_fixed_gradients
!
      use m_precision
!
      use m_constants
      use m_ele_material_property
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
!
      use fem_surf_skv_poisson_type
      use cal_skv_to_ff_smp
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_h_flux                                          &
     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,           &
     &          n_int, fem_wk, f_l)
!
      use m_surf_data_temp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (ngrp_sf_fix_hf .le. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      call fem_surf_skv_norm_grad_galerkin                              &
     &   (ele, surf, sf_grp, jac_sf_grp,                                &
     &    ngrp_sf_fix_hf, nele_sf_fix_hf, ngrp_sf_fix_hf,               &
     &    id_grp_sf_fix_hf, ist_ele_sf_fix_hf,                          &
     &    sf_apt_fix_hf, n_int, ione, ak_d_temp, fem_wk%sk6)
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_sf_h_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_torque                                          &
     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,           &
     &          n_int, fem_wk, f_l)
!
      use m_surf_data_torque
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: nd
!
!
      if ( sum(ngrp_sf_fix_tq) .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        if (ngrp_sf_fix_tq(nd).gt.0) then
          call fem_surf_skv_norm_grad_galerkin                          &
     &       (ele, surf, sf_grp, jac_sf_grp,                            &
     &        nmax_sf_fix_tq, nmax_ele_sf_fix_tq, ngrp_sf_fix_tq(nd),   &
     &        id_grp_sf_fix_tq(1,nd), ist_ele_sf_fix_tq(0,nd),          &
     &        sf_apt_fix_tq(1,nd), n_int, nd, ak_d_velo, fem_wk%sk6)
        end if
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_sf_torque
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_vecp                                       &
     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,           &
     &          n_int, fem_wk, f_l)
!
     use m_surf_data_vector_p
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: nd
!
!
      if ( sum(ngrp_sf_fix_grad_a) .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        if (ngrp_sf_fix_grad_a(nd).gt.0) then
          call fem_surf_skv_norm_grad_galerkin                          &
     &       (ele, surf, sf_grp, jac_sf_grp,                            &
     &        nmax_sf_fix_grad_a, nmax_ele_sf_fix_grad_a,               &
     &        ngrp_sf_fix_grad_a(nd), id_grp_sf_fix_grad_a(1,nd),       &
     &        ist_ele_sf_fix_grad_a(0,nd), sf_apt_fix_grad_a(1,nd),     &
     &        n_int, nd, ak_d_magne, fem_wk%sk6)
        end if
      end do
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_sf_grad_vecp
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_magne                                      &
     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,           &
     &          n_int, fem_wk, f_l)
!
     use m_surf_data_magne
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: nd
!
!
      if ( sum(ngrp_sf_fix_grad_b) .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        if (ngrp_sf_fix_grad_b(nd).gt.0) then
          call fem_surf_skv_norm_grad_galerkin                          &
     &       (ele, surf, sf_grp, jac_sf_grp,                            &
     &        nmax_sf_fix_grad_b, nmax_ele_sf_fix_grad_b,               &
     &        ngrp_sf_fix_grad_b(nd), id_grp_sf_fix_grad_b(1,nd),       &
     &        ist_ele_sf_fix_grad_b(0,nd), sf_apt_fix_grad_b(1,nd),     &
     &        n_int, nd, ak_d_magne, fem_wk%sk6)
        end if
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_sf_grad_magne
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_composition                                &
     &          (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl,          &
     &           n_int, fem_wk, f_l)
!
      use m_surf_data_composition
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (ngrp_sf_fix_cmg .le. 0) return
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      call fem_surf_skv_norm_grad_galerkin                              &
     &   (ele, surf, sf_grp, jac_sf_grp,                                &
     &     ngrp_sf_fix_cmg, nele_sf_fix_cmg, ngrp_sf_fix_cmg,           &
     &     id_grp_sf_fix_cmg, ist_ele_sf_fix_cmg, sf_apt_fix_cmg,       &
     &     n_int, ione, ak_d_composit, fem_wk%sk6)
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_sf_grad_composition
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_press                                      &
     &         (node, ele, surf, sf_grp, jac_sf_grp_l, rhs_tbl,         &
     &          n_int, fem_wk, f_l)
!
      use m_surf_data_press
      use cal_skv_to_ff_smp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (ngrp_sf_fix_pg .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      call fem_surf_skv_norm_poisson_pg                                 &
     &   (ele, surf, sf_grp, jac_sf_grp_l,                              &
     &    ngrp_sf_fix_pg, nele_sf_fix_pg, ngrp_sf_fix_pg,               &
     &    id_grp_sf_fix_pg, ist_ele_sf_fix_pg, sf_apt_fix_pg,           &
     &    n_int, fem_wk%sk6)
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_sf_grad_press
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_magne_p                                    &
     &         (node, ele, surf, sf_grp, jac_sf_grp_l, rhs_tbl,         &
     &          n_int, fem_wk, f_l)
!
      use m_surf_data_magne_p
      use cal_skv_to_ff_smp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (ngrp_sf_fix_mpg .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      call fem_surf_skv_norm_poisson_pg                                 &
     &   (ele, surf, sf_grp, jac_sf_grp_l,                              &
     &    ngrp_sf_fix_mpg, nele_sf_fix_mpg, ngrp_sf_fix_mpg,            &
     &    id_grp_sf_fix_mpg, ist_ele_sf_fix_mpg, sf_apt_fix_mpg,        &
     &    n_int, fem_wk%sk6)
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_sf_grad_magne_p
!
!-----------------------------------------------------------------------
!
      end module int_surf_fixed_gradients
