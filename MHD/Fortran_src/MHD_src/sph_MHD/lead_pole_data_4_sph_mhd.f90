!> @file  lead_pole_data_4_sph_mhd.f90
!!      module lead_pole_data_4_sph_mhd
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief Spherical transform at poles
!!
!!@verbatim
!!      subroutine lead_pole_fields_4_sph_mhd                           &
!!     &         (sph_params, sph_rtp, trns_snap, fls_pl,               &
!!     &          node, iphys, nod_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(in) :: trns_snap
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module lead_pole_data_4_sph_mhd
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine lead_pole_fields_4_sph_mhd                             &
     &         (sph_params, sph_rtp, trns_snap, fls_pl,                 &
     &          node, iphys, nod_fld)
!
      use m_machine_parameter
      use m_spheric_constants
      use m_work_pole_sph_trans
!
      use pole_energy_flux_sph
      use copy_MHD_4_pole_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      real(kind = kreal), intent(inout)                                 &
     &           :: fls_pl(nnod_pole,trns_snap%ncomp_rj_2_rtp)
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(sph_params%iflag_shell_mode .eq. iflag_MESH_same) return
!
      if (iflag_debug.eq.1) write(*,*) 'copy_snap_vec_from_pole_trans'
      call copy_snap_vec_from_pole_trans                                &
     &   (sph_params%m_folding, sph_rtp,                                &
     &    trns_snap%b_trns, trns_snap%ncomp_rj_2_rtp, fls_pl,           &
     &    node, iphys, nod_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'pole_nonlinear_sph_MHD'
      call pole_nonlinear_sph_MHD(sph_rtp, node, iphys, nod_fld)
      if (iflag_debug.eq.1) write(*,*) 'pole_energy_flux_rtp'
      call pole_energy_flux_rtp(sph_rtp, node, iphys, nod_fld)
!
      end subroutine lead_pole_fields_4_sph_mhd
!
!-----------------------------------------------------------------------
!
      end module lead_pole_data_4_sph_mhd
