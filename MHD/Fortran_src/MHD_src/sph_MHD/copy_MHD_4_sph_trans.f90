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
!!      subroutine copy_mhd_vec_spec_to_trans
!!      subroutine copy_mhd_scl_spec_to_trans
!!
!!      subroutine copy_mhd_vec_fld_from_trans
!!      subroutine copy_mhd_scl_fld_from_trans
!!
!!  routines for forward transform
!!      subroutine copy_mhd_scalar_fld_to_trans
!!      subroutine copy_mhd_scalar_spec_from_trans
!!
!!      subroutine copy_mhd_vec_fld_to_trans
!!      subroutine copy_mhd_vec_spec_from_trans
!!@endverbatim
!
      module copy_MHD_4_sph_trans
!
      use m_precision
!
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_vec_spec_to_trans
!
      use copy_spectr_4_sph_trans
!
!
!$omp parallel
      call copy_vec_spec_to_trans(3*nvector_rj_2_rtp,                     &
     &    ipol%i_velo, 3*b_trns%i_velo-2)
      call copy_vec_spec_to_trans(3*nvector_rj_2_rtp,                     &
     &    ipol%i_vort, 3*b_trns%i_vort-2)
      call copy_vec_spec_to_trans(3*nvector_rj_2_rtp,                     &
     &    ipol%i_magne, 3*b_trns%i_magne-2)
      call copy_vec_spec_to_trans(3*nvector_rj_2_rtp,                     &
     &    ipol%i_current, 3*b_trns%i_current-2)
!$omp end parallel
!
      end subroutine copy_mhd_vec_spec_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_scl_spec_to_trans
!
      use copy_spectr_4_sph_trans
!
!
!$omp parallel
      call copy_scalar_spec_to_trans(nscalar_rj_2_rtp,                  &
     &      ipol%i_temp, b_trns%i_temp)
      call copy_scalar_spec_to_trans(nscalar_rj_2_rtp,                  &
     &      ipol%i_light, b_trns%i_light)
!$omp end parallel
!
      end subroutine copy_mhd_scl_spec_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_vec_fld_from_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
      call copy_vec_fld_from_trans(3*nvector_rj_2_rtp,                    &
     &    irtp%i_velo, 3*b_trns%i_velo-2)
      call copy_vec_fld_from_trans(3*nvector_rj_2_rtp,                    &
     &    irtp%i_vort, 3*b_trns%i_vort-2)
      call copy_vec_fld_from_trans(3*nvector_rj_2_rtp,                    &
     &    irtp%i_magne, 3*b_trns%i_magne-2)
      call copy_vec_fld_from_trans(3*nvector_rj_2_rtp,                    &
     &    irtp%i_current, 3*b_trns%i_current-2)
!$omp end parallel
!
      end subroutine copy_mhd_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_scl_fld_from_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
      call copy_scalar_fld_from_trans(nscalar_rj_2_rtp,                 &
     &      irtp%i_temp, b_trns%i_temp)
      call copy_scalar_fld_from_trans(nscalar_rj_2_rtp,                 &
     &      irtp%i_light, b_trns%i_light)
!$omp end parallel
!
      end subroutine copy_mhd_scl_fld_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_vec_fld_to_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
!   advection flag
      call copy_vec_fld_to_trans(3*nvector_rtp_2_rj,                      &
     &      irtp%i_m_advect, 3*f_trns%i_m_advect-2)
!   Coriolis flag
      call copy_vec_fld_to_trans(3*nvector_rtp_2_rj,                      &
     &      irtp%i_coriolis, 3*f_trns%i_coriolis-2)
!   Lorentz flag
      call copy_vec_fld_to_trans(3*nvector_rtp_2_rj,                      &
     &      irtp%i_lorentz, 3*f_trns%i_lorentz-2)
!
!   induction flag
      call copy_vec_fld_to_trans(3*nvector_rtp_2_rj,                      &
     &      irtp%i_vp_induct, 3*f_trns%i_vp_induct-2)
!   divergence of heat flux flag
      call copy_vec_fld_to_trans(3*nvector_rtp_2_rj,                      &
     &      irtp%i_h_flux, 3*f_trns%i_h_flux-2)
!
!   divergence of composition flux flag
      call copy_vec_fld_to_trans(3*nvector_rtp_2_rj,                      &
     &      irtp%i_c_flux, 3*f_trns%i_c_flux-2)
!$omp end parallel
!
      end  subroutine copy_mhd_vec_fld_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_vec_spec_from_trans
!
      use copy_spectr_4_sph_trans
!
!
!$omp parallel
!   advection flag
      call copy_vec_spec_from_trans(3*nvector_rtp_2_rj,                   &
     &      ipol%i_m_advect, 3*f_trns%i_m_advect-2)
!   Coriolis flag
      call copy_vec_spec_from_trans(3*nvector_rtp_2_rj,                   &
     &      ipol%i_coriolis, 3*f_trns%i_coriolis-2)
!   Lorentz flag
      call copy_vec_spec_from_trans(3*nvector_rtp_2_rj,                   &
     &      ipol%i_lorentz, 3*f_trns%i_lorentz-2)
!
!   induction flag
      call copy_vec_spec_from_trans(3*nvector_rtp_2_rj,                   &
     &      ipol%i_vp_induct, 3*f_trns%i_vp_induct-2)
!
!   divergence of heat flux flag
      call copy_vec_spec_from_trans(3*nvector_rtp_2_rj,                   &
     &      ipol%i_h_flux, 3*f_trns%i_h_flux-2)
!   divergence of composition flux flag
      call copy_vec_spec_from_trans(3*nvector_rtp_2_rj,                   &
     &      ipol%i_c_flux, 3*f_trns%i_c_flux-2)
!$omp end parallel
!
      end  subroutine copy_mhd_vec_spec_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_MHD_4_sph_trans
