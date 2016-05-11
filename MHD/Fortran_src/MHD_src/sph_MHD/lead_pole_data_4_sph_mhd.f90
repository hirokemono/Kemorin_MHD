!> @file  lead_pole_data_4_sph_mhd.f90
!!      module lead_pole_data_4_sph_mhd
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief Spherical transform at poles
!!
!!@verbatim
!!      subroutine lead_pole_fields_4_sph_mhd(node, iphys, nod_fld)
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
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine lead_pole_fields_4_sph_mhd(node, iphys, nod_fld)
!
      use m_machine_parameter
      use m_spheric_constants
      use m_spheric_parameter
      use pole_energy_flux_sph
      use copy_MHD_4_pole_trans
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(sph_param1%iflag_shell_mode .eq. iflag_MESH_same) return
!
      if (iflag_debug.eq.1) write(*,*) 'copy_snap_vec_from_pole_trans'
      call copy_snap_vec_from_pole_trans                                &
     &   (sph_param1%m_folding, sph_rtp1, node, iphys, nod_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'pole_nonlinear_sph_MHD'
      call pole_nonlinear_sph_MHD(node, iphys, nod_fld)
      if (iflag_debug.eq.1) write(*,*) 'pole_energy_flux_rtp'
      call pole_energy_flux_rtp(node, iphys, nod_fld)
!
      end subroutine lead_pole_fields_4_sph_mhd
!
!-----------------------------------------------------------------------
!
      end module lead_pole_data_4_sph_mhd
