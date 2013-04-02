!
!      module set_bc_grad_potentials
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine set_boundary_grad_press
!      subroutine set_boundary_grad_magne_p
!
      module set_bc_grad_potentials
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
      use m_finite_element_matrix
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_boundary_grad_press
!
      use m_surf_data_press
      use fem_surf_skv_poisson_1st
      use cal_skv_to_ff_smp_1st
!
!
      if (ngrp_sf_fix_pg .eq. 0) return
      call reset_sk6(n_scalar)
!
      call fem_surf_skv_norm_poisson_1(ngrp_sf_fix_pg, nele_sf_fix_pg,  &
     &    ngrp_sf_fix_pg, id_grp_sf_fix_pg, ist_ele_sf_fix_pg,          &
     &    sf_apt_fix_pg, intg_point_poisson, sk6)
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine set_boundary_grad_press
!
!-----------------------------------------------------------------------
!
      subroutine set_boundary_grad_magne_p
!
      use m_surf_data_magne_p
      use fem_surf_skv_poisson_1st
      use cal_skv_to_ff_smp_1st
!
!
      if (ngrp_sf_fix_mpg .eq. 0) return
      call reset_sk6(n_scalar)
!
      call fem_surf_skv_norm_poisson_1(ngrp_sf_fix_mpg,                 &
     &    nele_sf_fix_mpg, ngrp_sf_fix_mpg, id_grp_sf_fix_mpg,          &
     &    ist_ele_sf_fix_mpg, sf_apt_fix_mpg, intg_point_poisson, sk6)
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine set_boundary_grad_magne_p
!
!-----------------------------------------------------------------------
!
      end module set_bc_grad_potentials
