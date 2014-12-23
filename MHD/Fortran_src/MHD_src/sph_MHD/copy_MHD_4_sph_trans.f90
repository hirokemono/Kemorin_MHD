!>@file   copy_MHD_4_sph_trans.f90
!!@brief  module copy_MHD_4_sph_trans
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Copy spectrum data and field data to spherical transform buffer
!!       for dynamo simulation
!!
!!@verbatim
!!  routines for backward transform
!!      subroutine copy_mhd_field_from_trans
!!      subroutine swap_phi_mhd_field_from_trans
!!@endverbatim
!
      module copy_MHD_4_sph_trans
!
      use m_precision
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
!
      implicit  none
!
      private :: copy_scalar_from_MHD_trans, copy_vector_from_MHD_trans
      private :: copy_force_from_MHD_trans
      private :: swap_phi_scalar_from_MHD_trans
      private :: swap_phi_vector_from_MHD_trans
      private :: swap_phi_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_field_from_trans
!
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
!$omp parallel
      call copy_vector_from_MHD_trans(irtp%i_velo, b_trns%i_velo)
      call copy_vector_from_MHD_trans(irtp%i_vort, b_trns%i_vort)
      call copy_vector_from_MHD_trans(irtp%i_magne, b_trns%i_magne)
      call copy_vector_from_MHD_trans(irtp%i_current, b_trns%i_current)
!
      call copy_scalar_from_MHD_trans(irtp%i_temp, b_trns%i_temp)
      call copy_scalar_from_MHD_trans(irtp%i_light, b_trns%i_light)
!
!   advection flag
      call copy_force_from_MHD_trans                                    &
     &   (irtp%i_m_advect, f_trns%i_m_advect)
!   Coriolis flag
      call copy_force_from_MHD_trans                                    &
     &   (irtp%i_coriolis, f_trns%i_coriolis)
!   Lorentz flag
      call copy_force_from_MHD_trans                                    &
     &   (irtp%i_lorentz, f_trns%i_lorentz)
!
!   induction flag
      call copy_force_from_MHD_trans                                    &
     &   (irtp%i_vp_induct, f_trns%i_vp_induct)
!   divergence of heat flux flag
      call copy_force_from_MHD_trans(irtp%i_h_flux, f_trns%i_h_flux)
!
!   divergence of composition flux flag
      call copy_force_from_MHD_trans(irtp%i_c_flux, f_trns%i_c_flux)
!$omp end parallel
!
      end subroutine copy_mhd_field_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_mhd_field_from_trans
!
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
!$omp parallel
      call swap_phi_vector_from_MHD_trans(irtp%i_velo, b_trns%i_velo)
      call swap_phi_vector_from_MHD_trans(irtp%i_vort, b_trns%i_vort)
      call swap_phi_vector_from_MHD_trans(irtp%i_magne, b_trns%i_magne)
      call swap_phi_vector_from_MHD_trans                               &
     &   (irtp%i_current, b_trns%i_current)
!
      call swap_phi_scalar_from_MHD_trans(irtp%i_temp, b_trns%i_temp)
      call swap_phi_scalar_from_MHD_trans(irtp%i_light, b_trns%i_light)
!
!   advection flag
      call swap_phi_force_from_MHD_trans                                &
     &   (irtp%i_m_advect, f_trns%i_m_advect)
!   Coriolis flag
      call swap_phi_force_from_MHD_trans                                &
     &   (irtp%i_coriolis, f_trns%i_coriolis)
!   Lorentz flag
      call swap_phi_force_from_MHD_trans                                &
     &   (irtp%i_lorentz, f_trns%i_lorentz)
!
!   induction flag
      call swap_phi_force_from_MHD_trans                                &
     &   (irtp%i_vp_induct, f_trns%i_vp_induct)
!   divergence of heat flux flag
      call swap_phi_force_from_MHD_trans                                &
     &   (irtp%i_h_flux, f_trns%i_h_flux)
!
!   divergence of composition flux flag
      call swap_phi_force_from_MHD_trans                                &
     &   (irtp%i_c_flux, f_trns%i_c_flux)
!$omp end parallel
!
      end subroutine swap_phi_mhd_field_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_sph_spectr_data
      use copy_field_smp
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_scalar_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,    &
     &    fld_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine copy_scalar_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_sph_spectr_data
      use copy_field_smp
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_vector_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,    &
     &    fld_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine copy_vector_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_force_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_sph_spectr_data
      use copy_field_smp
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call copy_nod_vector_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,    &
     &    frc_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine copy_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine swap_phi_scalar_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_sph_spectr_data
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call swap_phi_scalar_from_trans                                   &
     &   (nnod_rtp, frc_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine swap_phi_scalar_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_vector_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_sph_spectr_data
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call swap_phi_vector_from_trans                                   &
     &   (nnod_rtp, fld_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine swap_phi_vector_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_force_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_sph_spectr_data
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call swap_phi_vector_from_trans                                   &
     &   (nnod_rtp, frc_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine swap_phi_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      end module copy_MHD_4_sph_trans
