!
!      module cal_electric_potential
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!      subroutine cal_scalar_potential
!
      module cal_electric_potential
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
      subroutine cal_scalar_potential
!
      use m_finite_element_matrix
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_velocity_sgs
      use set_boundary_potentials
      use int_surf_normal_fields
      use cal_solver_MHD
!
!
      call reset_ff
      call reset_ff_smps
!
      call int_vol_divergence_vect_p
!
!      call int_surf_sgs_div_vect_p
!
      call int_surf_normal_vector_p
!
      call int_vol_sk_mp_bc
!
      call set_boundary_fmag
!
      call cal_sol_mag_po
!
      call set_boundary_m_phi
!
      end subroutine cal_scalar_potential
!
!-----------------------------------------------------------------------
!
      end module cal_electric_potential
