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
!!      subroutine copy_forces_to_snapshot_rtp                          &
!!     &          (m_folding, sph_rtp, f_trns, ncomp_rtp_2_rj, node,    &
!!     &           iphys, frc_rtp, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module copy_MHD_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_spheric_rtp_data
      use t_addresses_sph_transform
!
      implicit  none
!
      private :: copy_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_forces_to_snapshot_rtp                            &
     &          (m_folding, sph_rtp, f_trns, ncomp_rtp_2_rj, node,      &
     &           iphys, frc_rtp, nod_fld)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: f_trns
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
!
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: ncomp_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &     :: frc_rtp(sph_rtp%nnod_rtp,ncomp_rtp_2_rj)
!
      type(phys_data), intent(inout) :: nod_fld
!
!
!   advection flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_m_advect, iphys%i_m_advect,                          &
     &    m_folding, sph_rtp, node, ncomp_rtp_2_rj, frc_rtp, nod_fld)
!   Coriolis flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_coriolis, iphys%i_coriolis,                          &
     &    m_folding, sph_rtp, node, ncomp_rtp_2_rj, frc_rtp, nod_fld)
!   Lorentz flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_lorentz, iphys%i_lorentz,                            &
     &    m_folding, sph_rtp, node, ncomp_rtp_2_rj, frc_rtp, nod_fld)
!
!   induction flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_vp_induct, iphys%i_vp_induct,                        &
     &    m_folding, sph_rtp, node, ncomp_rtp_2_rj, frc_rtp, nod_fld)
!   divergence of heat flux flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_h_flux, iphys%i_h_flux,                              &
     &    m_folding, sph_rtp, node, ncomp_rtp_2_rj, frc_rtp, nod_fld)
!
!   divergence of composition flux flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_c_flux, iphys%i_c_flux,                              &
     &    m_folding, sph_rtp, node, ncomp_rtp_2_rj, frc_rtp, nod_fld)
!
      end subroutine copy_forces_to_snapshot_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_force_from_MHD_trans                              &
     &         (i_trns, i_field, m_folding, sph_rtp, node,              &
     &          ncomp_rtp_2_rj, frc_rtp, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: ncomp_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: frc_rtp(sph_rtp%nnod_rtp,ncomp_rtp_2_rj)
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nodal_vector_from_trans                                 &
     &   (sph_rtp, m_folding, ncomp_rtp_2_rj, i_trns, frc_rtp,          &
     &    node%numnod, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine copy_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      end module copy_MHD_4_sph_trans
