!>@file   copy_sph_MHD_4_send_recv.f90
!!@brief  module copy_sph_MHD_4_send_recv
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Copy spectrum data and field data to spherical transform buffer
!!       for dynamo simulation
!!
!!@verbatim
!!  routines for backward transform
!!      subroutine copy_mhd_spectr_to_send(ncomp_send, n_WS, WS)
!!
!!  routines for forward transform
!!      subroutine copy_mhd_fields_to_send(ncomp_send, n_WS, WS)
!!      subroutine copy_mhd_spectr_from_recv(ncomp_recv, n_WR, WR)
!!@endverbatim
!
      module copy_sph_MHD_4_send_recv
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_spectr_to_send(ncomp_send, n_WS, WS)
!
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_velo, b_trns%i_velo, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_vort, b_trns%i_vort, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_magne, b_trns%i_magne, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_current, b_trns%i_current, n_WS, WS)
!
      call sel_sph_rj_scalar_to_send                                    &
     &   (ncomp_send, ipol%i_temp, b_trns%i_temp, n_WS, WS)
      call sel_sph_rj_scalar_to_send                                    &
     &   (ncomp_send, ipol%i_light, b_trns%i_light, n_WS, WS)
!
      end subroutine copy_mhd_spectr_to_send
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_fields_to_send(ncomp_send, n_WS, WS)
!
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
!   advection flag
      call sel_sph_rtp_vector_to_send                                   &
     &   (ncomp_send, ipol%i_m_advect, b_trns%i_m_advect, n_WS, WS)
!   Coriolis flag
      call sel_sph_rtp_vector_to_send                                   &
     &   (ncomp_send, ipol%i_coriolis, b_trns%i_coriolis, n_WS, WS)
!   Lorentz flag
      call sel_sph_rtp_vector_to_send                                   &
     &   (ncomp_send, ipol%i_lorentz, b_trns%i_lorentz, n_WS, WS)
!   induction flag
      call sel_sph_rtp_vector_to_send                                   &
     &   (ncomp_send, ipol%i_vp_induct, b_trns%i_vp_induct, n_WS, WS)
!   heat flux flag
      call sel_sph_rtp_vector_to_send                                   &
     &   (ncomp_send, ipol%i_h_flux, b_trns%i_h_flux, n_WS, WS)
!   composition flux flag
      call sel_sph_rtp_vector_to_send                                   &
     &   (ncomp_send, ipol%i_c_flux, b_trns%i_c_flux, n_WS, WS)
!
      end subroutine copy_mhd_fields_to_send
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_spectr_from_recv(ncomp_recv, n_WR, WR)
!
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
!$omp parallel
!   advection flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_m_advect, f_trns%i_m_advect, n_WR, WR)
!   Coriolis flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_coriolis, f_trns%i_coriolis, n_WR, WR)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_rot_Coriolis, f_trns%i_rot_Coriolis, n_WR, WR)
!   Lorentz flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_lorentz, f_trns%i_lorentz, n_WR, WR)
!
!   induction flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_vp_induct, f_trns%i_vp_induct, n_WR, WR)
!
!   heat flux flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_h_flux, f_trns%i_h_flux, n_WR, WR)
!   composition flux flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_c_flux, f_trns%i_c_flux, n_WR, WR)
!$omp end parallel
!
      end  subroutine copy_mhd_spectr_from_recv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rtp_vector_to_send                             &
     &         (ncomp_send, i_field, i_send, n_WS, WS)
!
      use m_sph_communicators
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: i_field, i_send
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if(i_field*i_send .eq. 0) return
      call sel_calypso_to_send_vector(ncomp_send, nnod_rtp, n_WS,       &
     &    nmax_sr_rtp, nneib_domain_rtp, istack_sr_rtp, item_sr_rtp,    &
     &    ntot_phys_rtp, i_field, i_send, d_rtp, WS)
!
      end subroutine sel_sph_rtp_vector_to_send
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_rtp_scalar_to_send                             &
     &         (ncomp_send, i_field, i_send, n_WS, WS)
!
      use m_sph_communicators
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: i_field, i_send
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if(i_field*i_send .eq. 0) return
      call sel_calypso_to_send_scalar(ncomp_send, nnod_rtp, n_WS,       &
     &    nmax_sr_rtp, nneib_domain_rtp, istack_sr_rtp, item_sr_rtp,    &
     &    ntot_phys_rtp, i_field, i_send, d_rtp, WS)
!
      end subroutine sel_sph_rtp_scalar_to_send
!
!-----------------------------------------------------------------------
!
      end module copy_sph_MHD_4_send_recv
