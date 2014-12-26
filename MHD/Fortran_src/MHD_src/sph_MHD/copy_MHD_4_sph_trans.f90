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
!!      subroutine select_mhd_field_from_trans
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
      private :: sel_scalar_from_MHD_trans, sel_vector_from_MHD_trans
      private :: sel_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine select_mhd_field_from_trans
!
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
!$omp parallel
      call sel_vector_from_MHD_trans(irtp%i_velo, b_trns%i_velo)
      call sel_vector_from_MHD_trans(irtp%i_vort, b_trns%i_vort)
      call sel_vector_from_MHD_trans(irtp%i_magne, b_trns%i_magne)
      call sel_vector_from_MHD_trans(irtp%i_current, b_trns%i_current)
!
      call sel_scalar_from_MHD_trans(irtp%i_temp, b_trns%i_temp)
      call sel_scalar_from_MHD_trans(irtp%i_light, b_trns%i_light)
!
!   advection flag
      call sel_force_from_MHD_trans                                     &
     &   (irtp%i_m_advect, f_trns%i_m_advect)
!   Coriolis flag
      call sel_force_from_MHD_trans                                     &
     &   (irtp%i_coriolis, f_trns%i_coriolis)
!   Lorentz flag
      call sel_force_from_MHD_trans                                     &
     &   (irtp%i_lorentz, f_trns%i_lorentz)
!
!   induction flag
      call sel_force_from_MHD_trans                                     &
     &   (irtp%i_vp_induct, f_trns%i_vp_induct)
!   divergence of heat flux flag
      call sel_force_from_MHD_trans(irtp%i_h_flux, f_trns%i_h_flux)
!
!   divergence of composition flux flag
      call sel_force_from_MHD_trans(irtp%i_c_flux, f_trns%i_c_flux)
!$omp end parallel
!
      end subroutine select_mhd_field_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_scalar_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_sph_spectr_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call sel_scalar_from_trans                                        &
     &   (nnod_rtp, fld_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine sel_scalar_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine sel_vector_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_sph_spectr_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call sel_vector_from_trans                                        &
     &   (nnod_rtp, fld_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine sel_vector_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine sel_force_from_MHD_trans(is_spec, i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_sph_spectr_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: is_spec, i_trns
!
!
      if( (is_spec*i_trns) .le. 0) return
      call sel_vector_from_trans                                        &
     &   (nnod_rtp, frc_rtp(1,i_trns), d_rtp(1,is_spec) )
!
      end subroutine sel_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      end module copy_MHD_4_sph_trans
