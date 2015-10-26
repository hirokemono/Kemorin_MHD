!
!      module cal_mod_vel_potential
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!      subroutine cal_mod_potential
!
      module cal_mod_vel_potential
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_mod_potential
!
      use m_finite_element_matrix
      use m_geometry_data
      use m_group_data
      use m_jacobian_sf_grp
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_velocity_sgs
      use int_surf_fixed_gradients
      use int_surf_normal_fields
      use set_boundary_potentials
      use cal_solver_MHD
!
!
      call reset_ff(node1%numnod, f1_l)
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
!    take divergence of velocity
!
      call int_vol_divergence_velo
!
      call int_surf_normal_velocity                                     &
     &   (node1, ele1, surf1, sf_grp1, nod_fld1, jac1_sf_grp_2d_l)
!
!      call int_surf_sgs_div_velo
!
!   set boundary condition for wall
!
      call int_sf_grad_press                                            &
     &   (node1, ele1, surf1, sf_grp1, jac1_sf_grp_2d_l, rhs_tbl1,      &
     &    intg_point_poisson, fem1_wk, f1_l)
!
!   add boundary term for fixed velocity
!
      call int_vol_sk_po_bc
!
!   add boundary term for fixed pressure
!
      call set_boundary_ff
!
!   solve Poission equation
!
      call cal_sol_mod_po
!
      call set_boundary_phi
!
      end subroutine cal_mod_potential
!
!-----------------------------------------------------------------------
!
      end module cal_mod_vel_potential
