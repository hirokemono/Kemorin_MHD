!int_surf_fixed_gradients.f90
!      module int_surf_fixed_gradients
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine int_sf_h_flux(ele, surf, sf_grp, jac_sf_grp, n_int)
!      subroutine int_sf_torque(ele, surf, sf_grp, jac_sf_grp, n_int)
!      subroutine int_sf_grad_vecp(ele, surf, sf_grp, jac_sf_grp, n_int)
!      subroutine int_sf_grad_magne                                     &
!     &         (ele, surf, sf_grp, jac_sf_grp, n_int)
!      subroutine int_sf_grad_composition                               &
!     &         (ele, surf, sf_grp, jac_sf_grp, n_int)
!
!      subroutine int_sf_grad_press(ele, surf, sf_grp, jac_sf_grp)
!      subroutine int_sf_grad_magne_p(ele, surf, sf_grp, jac_sf_grp)
!
      module int_surf_fixed_gradients
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_geometry_data
      use m_ele_material_property
      use m_sorted_node
      use m_finite_element_matrix
      use m_phys_constants
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
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
      subroutine int_sf_h_flux(ele, surf, sf_grp, jac_sf_grp, n_int)
!
      use m_surf_data_temp
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      integer(kind=kint), intent(in) :: n_int
!
!
      if (ngrp_sf_fix_hf .le. 0) return
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      call fem_surf_skv_norm_grad_galerkin                              &
     &   (ele, surf, sf_grp, jac_sf_grp,                                &
     &    ngrp_sf_fix_hf, nele_sf_fix_hf, ngrp_sf_fix_hf,               &
     &    id_grp_sf_fix_hf, ist_ele_sf_fix_hf,                          &
     &    sf_apt_fix_hf, n_int, ione, ak_d_temp, fem1_wk%sk6)
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_sf_h_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_torque(ele, surf, sf_grp, jac_sf_grp, n_int)
!
      use m_surf_data_torque
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint) :: nd
!
!
      if ( sum(ngrp_sf_fix_tq) .le. 0) return
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do nd = 1, n_vector
        if (ngrp_sf_fix_tq(nd).gt.0) then
          call fem_surf_skv_norm_grad_galerkin                          &
     &       (ele, surf, sf_grp, jac_sf_grp,                            &
     &        nmax_sf_fix_tq, nmax_ele_sf_fix_tq, ngrp_sf_fix_tq(nd),   &
     &        id_grp_sf_fix_tq(1,nd), ist_ele_sf_fix_tq(0,nd),          &
     &        sf_apt_fix_tq(1,nd), n_int, nd, ak_d_velo, fem1_wk%sk6)
        end if
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    ff_smp, fem1_wk%sk6)
!
      end subroutine int_sf_torque
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_vecp(ele, surf, sf_grp, jac_sf_grp, n_int)
!
     use m_surf_data_vector_p
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      integer(kind=kint), intent(in) :: n_int
      integer(kind = kint) :: nd
!
!
      if ( sum(ngrp_sf_fix_grad_a) .le. 0) return
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do nd = 1, n_vector
        if (ngrp_sf_fix_grad_a(nd).gt.0) then
          call fem_surf_skv_norm_grad_galerkin                          &
     &       (ele, surf, sf_grp, jac_sf_grp,                            &
     &        nmax_sf_fix_grad_a, nmax_ele_sf_fix_grad_a,               &
     &        ngrp_sf_fix_grad_a(nd), id_grp_sf_fix_grad_a(1,nd),       &
     &        ist_ele_sf_fix_grad_a(0,nd), sf_apt_fix_grad_a(1,nd),     &
     &        n_int, nd, ak_d_magne, fem1_wk%sk6)
        end if
      end do
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    ff_smp, fem1_wk%sk6)
!
      end subroutine int_sf_grad_vecp
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_magne                                      &
     &         (ele, surf, sf_grp, jac_sf_grp, n_int)
!
     use m_surf_data_magne
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      integer(kind=kint), intent(in) :: n_int
!
      integer(kind = kint) :: nd
!
!
      if ( sum(ngrp_sf_fix_grad_b) .le. 0) return
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do nd = 1, n_vector
        if (ngrp_sf_fix_grad_b(nd).gt.0) then
          call fem_surf_skv_norm_grad_galerkin                          &
     &       (ele, surf, sf_grp, jac_sf_grp,                            &
     &        nmax_sf_fix_grad_b, nmax_ele_sf_fix_grad_b,               &
     &        ngrp_sf_fix_grad_b(nd), id_grp_sf_fix_grad_b(1,nd),       &
     &        ist_ele_sf_fix_grad_b(0,nd), sf_apt_fix_grad_b(1,nd),     &
     &        n_int, nd, ak_d_magne, fem1_wk%sk6)
        end if
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    ff_smp, fem1_wk%sk6)
!
      end subroutine int_sf_grad_magne
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_composition                                &
     &          (ele, surf, sf_grp, jac_sf_grp, n_int)
!
      use m_surf_data_composition
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
      integer(kind=kint), intent(in) :: n_int
!
!
      if (ngrp_sf_fix_cmg .le. 0) return
!
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      call fem_surf_skv_norm_grad_galerkin                              &
     &   (ele, surf, sf_grp, jac_sf_grp,                                &
     &     ngrp_sf_fix_cmg, nele_sf_fix_cmg, ngrp_sf_fix_cmg,           &
     &     id_grp_sf_fix_cmg, ist_ele_sf_fix_cmg, sf_apt_fix_cmg,       &
     &     n_int, ione, ak_d_composit, fem1_wk%sk6)
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_sf_grad_composition
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_press(ele, surf, sf_grp, jac_sf_grp)
!
      use m_surf_data_press
      use cal_skv_to_ff_smp
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
!
      if (ngrp_sf_fix_pg .eq. 0) return
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      call fem_surf_skv_norm_poisson_pg(ele, surf, sf_grp, jac_sf_grp,  &
     &    ngrp_sf_fix_pg, nele_sf_fix_pg, ngrp_sf_fix_pg,               &
     &    id_grp_sf_fix_pg, ist_ele_sf_fix_pg, sf_apt_fix_pg,           &
     &    intg_point_poisson, fem1_wk%sk6)
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_sf_grad_press
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_magne_p(ele, surf, sf_grp, jac_sf_grp)
!
      use m_surf_data_magne_p
      use cal_skv_to_ff_smp
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
!
!
      if (ngrp_sf_fix_mpg .eq. 0) return
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
      call fem_surf_skv_norm_poisson_pg(ele, surf, sf_grp, jac_sf_grp,  &
     &    ngrp_sf_fix_mpg, nele_sf_fix_mpg, ngrp_sf_fix_mpg,            &
     &    id_grp_sf_fix_mpg, ist_ele_sf_fix_mpg, sf_apt_fix_mpg,        &
     &    intg_point_poisson, fem1_wk%sk6)
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, ff_smp)
!
      end subroutine int_sf_grad_magne_p
!
!-----------------------------------------------------------------------
!
      end module int_surf_fixed_gradients
