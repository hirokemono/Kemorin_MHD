!int_surf_fixed_gradients.f90
!      module int_surf_fixed_gradients
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_sf_scalar_flux                                   &
!!     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl, grad_sf,&
!!     &          n_int, ak_d, fem_wk, f_l)
!!      subroutine int_sf_grad_velocity                                 &
!!     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl, grad_sf,&
!!     &          n_int, ak_d, fem_wk, f_l)
!!      subroutine int_sf_grad_velocity                                 &
!!     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl, grad_sf,&
!!     &          n_int, ak_d, fem_wk, f_l)
!!
!!      subroutine int_sf_grad_press(node, ele, surf, sf_grp,           &
!!     &          jac_sf_grp_l, rhs_tbl, grad_sf, n_int, fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(scaler_surf_flux_bc_type), intent(in) :: grad_sf(3)
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_surf_fixed_gradients
!
      use m_precision
!
      use m_constants
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use m_fem_gauss_int_coefs
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_surface_bc_data
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
      subroutine int_sf_scalar_flux                                     &
     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl, grad_sf,  &
     &          n_int, ak_d, fem_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(scaler_surf_flux_bc_type), intent(in) :: grad_sf
!
      integer(kind=kint), intent(in) :: n_int
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (grad_sf%ngrp_sf_fix_fx .le. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      call fem_surf_skv_norm_grad_galerkin                              &
     &   (ele, surf, sf_grp, g_FEM1, jac_sf_grp, grad_sf,               &
     &    n_int, ione, ak_d, fem_wk%sk6)
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_sf_scalar_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_velocity                                   &
     &         (node, ele, surf, sf_grp, jac_sf_grp, rhs_tbl, grad_sf,  &
     &          n_int, ak_d, fem_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(scaler_surf_flux_bc_type), intent(in) :: grad_sf(3)
!
      integer(kind = kint), intent(in) :: n_int
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: nd, num
!
!
      num =  grad_sf(1)%ngrp_sf_fix_fx + grad_sf(2)%ngrp_sf_fix_fx      &
     &     + grad_sf(3)%ngrp_sf_fix_fx
      if (num .le. 0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do nd = 1, n_vector
        if (grad_sf(nd)%ngrp_sf_fix_fx .gt. 0) then
          call fem_surf_skv_norm_grad_galerkin                          &
     &       (ele, surf, sf_grp, g_FEM1, jac_sf_grp, grad_sf(nd),       &
     &        n_int, nd, ak_d, fem_wk%sk6)
        end if
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_sf_grad_velocity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_press(node, ele, surf, sf_grp,             &
     &          jac_sf_grp_l, rhs_tbl, grad_sf, n_int, fem_wk, f_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(scaler_surf_flux_bc_type), intent(in) :: grad_sf
!
      integer(kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (grad_sf%ngrp_sf_fix_fx .eq. 0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      call fem_surf_skv_norm_poisson_pg(ele, surf, sf_grp,              &
     &    g_FEM1, jac_sf_grp_l, grad_sf, n_int, fem_wk%sk6)
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_sf_grad_press
!
!-----------------------------------------------------------------------
!
      end module int_surf_fixed_gradients
