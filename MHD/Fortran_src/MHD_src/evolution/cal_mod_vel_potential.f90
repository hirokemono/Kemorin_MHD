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
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_velocity_sgs
      use set_boundary_potentials
      use set_bc_grad_potentials
      use int_surf_normal_fields
      use cal_solver_MHD
!
!
      call reset_ff
      call reset_ff_smps
!
!    take divergence of velocity
!
      call int_vol_divergence_velo
!
      call int_surf_normal_velocity
!
!      call int_surf_sgs_div_velo
!
!   set boundary condition for wall
!
      call set_boundary_grad_press
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
