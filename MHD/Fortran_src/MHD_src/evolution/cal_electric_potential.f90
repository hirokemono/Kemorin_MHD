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
      use m_geometry_data
      use m_machine_parameter
      use m_group_data
      use m_finite_element_matrix
      use m_jacobian_sf_grp
!
      use int_vol_fractional_div
      use int_sk_4_fixed_boundary
      use int_surf_div_velocity_sgs
      use set_boundary_potentials
      use int_surf_normal_fields
      use cal_solver_MHD
!
!
      call reset_ff(node1%numnod, f1_l)
      call reset_ff_smps
!
      if (iflag_debug .gt. 0)  write(*,*) 'int_vol_divergence_vect_p'
      call int_vol_divergence_vect_p
!
!      call int_surf_sgs_div_vect_p
!
      call int_surf_normal_vector_p                                     &
     &   (ele1, surf1, sf_grp1, jac1_sf_grp_2d_l)
!
      call int_vol_sk_mp_bc
!
      call set_boundary_fmag
!
      if (iflag_debug .gt. 0)  write(*,*) 'cal_sol_mag_po'
      call cal_sol_mag_po
!
      if (iflag_debug .gt. 0)  write(*,*) 'set_boundary_m_phi'
      call set_boundary_m_phi
!
      end subroutine cal_scalar_potential
!
!-----------------------------------------------------------------------
!
      end module cal_electric_potential
