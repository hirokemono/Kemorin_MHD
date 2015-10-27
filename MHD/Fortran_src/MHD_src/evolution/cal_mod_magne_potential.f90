!
!      module cal_mod_magne_potential
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine cal_mag_potential
!
      module cal_mod_magne_potential
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_mag_potential
!
      use calypso_mpi
      use m_control_parameter
      use m_geometry_data
      use m_group_data
      use m_finite_element_matrix
      use m_jacobian_sf_grp
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_velocity_sgs
      use int_surf_fixed_gradients
      use set_boundary_potentials
      use set_magne_boundary
      use int_surf_normal_fields
      use cal_solver_MHD
!
!
      call reset_ff(node1%numnod, f1_l)
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_divergence_magne
!      call int_surf_sgs_div_magne
!
      call int_surf_normal_magne                                        &
     &   (node1, ele1, surf1, sf_grp1, nod_fld1, jac1_sf_grp_2d_l,      &
     &    rhs_tbl1, fem1_wk, f1_l)
      call int_sf_grad_magne_p                                          &
     &   (node1, ele1, surf1, sf_grp1, jac1_sf_grp_2d_l, rhs_tbl1,      &
     &    intg_point_poisson, fem1_wk, f1_l)
!
      call int_vol_sk_mp_bc
!
      call set_boundary_fmag
!
      call cal_sol_mag_po
!
      call set_boundary_m_phi
!
      end subroutine cal_mag_potential
!
! ----------------------------------------------------------------------
!
      end module cal_mod_magne_potential
