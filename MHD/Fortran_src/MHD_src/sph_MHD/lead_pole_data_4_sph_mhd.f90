!> @file  lead_pole_data_4_sph_mhd.f90
!!      module lead_pole_data_4_sph_mhd
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief Spherical transform at poles
!!
!!@verbatim
!!      subroutine lead_pole_fields_4_sph_mhd
!!@endverbatim
!
      module lead_pole_data_4_sph_mhd
!
      use m_precision
      use m_constants
!
      implicit  none
!
      private :: pole_back_trans_4_MHD, pole_back_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine lead_pole_fields_4_sph_mhd
!
      use m_machine_parameter
      use m_spheric_constants
      use m_spheric_parameter
      use pole_energy_flux_sph
!
!
      if(iflag_shell_mode .eq. iflag_MESH_same) return
!
      if (iflag_debug.eq.1) write(*,*) 'pole_back_trans_4_MHD'
      call pole_back_trans_4_MHD
      if (iflag_debug.eq.1) write(*,*) 'pole_back_trans_snapshot_MHD'
      call pole_back_trans_snapshot_MHD
!
      if (iflag_debug.eq.1) write(*,*) 'pole_nonlinear_sph_MHD'
      call pole_nonlinear_sph_MHD
      if (iflag_debug.eq.1) write(*,*) 'pole_energy_flux_rtp'
      call pole_energy_flux_rtp
!
      end subroutine lead_pole_fields_4_sph_mhd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pole_back_trans_4_MHD
!
      use m_solver_SR
      use m_work_4_sph_trans
      use m_addresses_trans_sph_MHD
      use spherical_SRs_N
      use pole_sph_transform
      use copy_sph_MHD_4_send_recv
      use copy_MHD_4_pole_trans
!
!
      if(ncomp_rj_2_rtp .le. 0) return
!
      call check_calypso_rj_2_rlm_buf_N(ncomp_rj_2_rtp)
!
      call copy_mhd_spectr_to_send(ncomp_rj_2_rtp, n_WS, WS)
!
      call pole_backward_transforms(ncomp_rj_2_rtp,                     &
     &    nvector_rj_2_rtp, nscalar_rj_2_rtp, n_WR, WR(1))
!
      call copy_mhd_vec_from_pole_trans
      call copy_mhd_scl_from_pole_trans
!
      end subroutine pole_back_trans_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine pole_back_trans_snapshot_MHD
!
      use m_solver_SR
      use m_addresses_trans_sph_snap
      use spherical_SRs_N
      use pole_sph_transform
      use copy_snap_4_sph_trans
      use copy_MHD_4_pole_trans
!
!
      if(ncomp_snap_rj_2_rtp .le. 0) return
!
      call check_calypso_rj_2_rlm_buf_N(ncomp_snap_rj_2_rtp)
      call copy_snap_spectr_to_send(ncomp_snap_rj_2_rtp, n_WS, WS(1))
!
      call pole_backward_transforms(ncomp_snap_rj_2_rtp,                &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp, n_WR, WR)
!
      call copy_snap_vec_from_pole_trans
      call copy_snap_scl_from_pole_trans
!
      end subroutine pole_back_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      end module lead_pole_data_4_sph_mhd
