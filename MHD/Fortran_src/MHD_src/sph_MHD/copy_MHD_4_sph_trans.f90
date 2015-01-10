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
!   advection flag
      call sel_force_from_MHD_trans                                     &
     &   (f_trns%i_m_advect, irtp%i_m_advect)
!   Coriolis flag
      call sel_force_from_MHD_trans                                     &
     &   (f_trns%i_coriolis, irtp%i_coriolis)
!   Lorentz flag
      call sel_force_from_MHD_trans                                     &
     &   (f_trns%i_lorentz, irtp%i_lorentz)
!
!   induction flag
      call sel_force_from_MHD_trans                                     &
     &   (f_trns%i_vp_induct, irtp%i_vp_induct)
!   divergence of heat flux flag
      call sel_force_from_MHD_trans(f_trns%i_h_flux, irtp%i_h_flux)
!
!   divergence of composition flux flag
      call sel_force_from_MHD_trans(f_trns%i_c_flux, irtp%i_c_flux)
!$omp end parallel
!
      end subroutine select_mhd_field_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_force_from_MHD_trans(i_trns, irtp_fld)
!
      use m_addresses_trans_sph_MHD
      use m_sph_spectr_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: irtp_fld, i_trns
!
!
      if( (irtp_fld*i_trns) .le. 0) return
      call sel_vector_from_trans                                        &
     &   (nnod_rtp, frc_rtp(1,i_trns), d_rtp(1,irtp_fld) )
!
      end subroutine sel_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      end module copy_MHD_4_sph_trans
