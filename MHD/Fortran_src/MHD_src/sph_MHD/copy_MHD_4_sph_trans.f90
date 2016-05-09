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
!!      subroutine copy_forces_to_snapshot_rtp                          &
!!     &          (m_folding, sph_rtp, node, iphys, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module copy_MHD_4_sph_trans
!
      use m_precision
      use m_machine_parameter
      use m_spheric_parameter
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_spheric_rtp_data
!
      implicit  none
!
      private :: sel_force_from_MHD_trans, copy_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine select_mhd_field_from_trans
!
      use m_node_phys_data
      use m_addresses_trans_sph_MHD
!
!
!   advection flag
      call sel_force_from_MHD_trans(f_trns%i_m_advect)
!   Coriolis flag
      call sel_force_from_MHD_trans(f_trns%i_coriolis)
!   Lorentz flag
      call sel_force_from_MHD_trans(f_trns%i_lorentz)
!
!   induction flag
      call sel_force_from_MHD_trans(f_trns%i_vp_induct)
!   divergence of heat flux flag
      call sel_force_from_MHD_trans(f_trns%i_h_flux)
!
!   divergence of composition flux flag
      call sel_force_from_MHD_trans(f_trns%i_c_flux)
!
      end subroutine select_mhd_field_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_forces_to_snapshot_rtp                            &
     &          (m_folding, sph_rtp, node, iphys, nod_fld)
!
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
!   advection flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_m_advect, iphys%i_m_advect,                          &
     &    m_folding, sph_rtp, node, nod_fld)
!   Coriolis flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_coriolis, iphys%i_coriolis,                          &
     &    m_folding, sph_rtp, node, nod_fld)
!   Lorentz flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_lorentz, iphys%i_lorentz,                            &
     &    m_folding, sph_rtp, node, nod_fld)
!
!   induction flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_vp_induct, iphys%i_vp_induct,                        &
     &    m_folding, sph_rtp, node, nod_fld)
!   divergence of heat flux flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_h_flux, iphys%i_h_flux,                              &
     &    m_folding, sph_rtp, node, nod_fld)
!
!   divergence of composition flux flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_c_flux, iphys%i_c_flux,                              &
     &    m_folding, sph_rtp, node, nod_fld)
!
      end subroutine copy_forces_to_snapshot_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_force_from_MHD_trans(i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_trns
!
!
      if(i_trns .le. 0) return
      call sel_vector_from_trans                                        &
     &   (nnod_rtp, nidx_rtp, ione, sph_rj1%istack_inod_rj_smp,         &
     &    nnod_rtp, frc_rtp(1,i_trns), frm_rtp(1,i_trns) )
!
      end subroutine sel_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_force_from_MHD_trans                              &
     &         (i_trns, i_field, m_folding, sph_rtp, node, nod_fld)
!
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nodal_vector_from_trans                                 &
     &   (sph_rtp, m_folding, ncomp_rtp_2_rj, i_trns, frm_rtp,          &
     &    node%numnod, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine copy_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      end module copy_MHD_4_sph_trans
