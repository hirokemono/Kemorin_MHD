!>@file   const_coriolis_sph_rlm.f90
!!@brief  module const_coriolis_sph_rlm
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!
!>@brief  Evaluate Coriolis term
!!
!!
!!@verbatim
!!      subroutine init_sum_coriolis_rlm(l_truncation, sph_rlm, leg)
!!      subroutine sum_coriolis_rlm(ncomp_trans, sph_rlm, comm_rlm,     &
!!     &          omega_sph, trns_MHD, leg, n_WR, WR)
!!      subroutine copy_coriolis_terms_rlm                              &
!!     &         (ncomp_trans, sph_rlm, comm_rlm, trns_MHD, n_WS, WS)
!!        type(sph_rlm_grid), intent(in)  :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!@endverbatim
!
      module const_coriolis_sph_rlm
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_control_parameter
      use m_physical_property
!
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_poloidal_rotation
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sum_coriolis_rlm(l_truncation, sph_rlm, leg)
!
      use calypso_mpi
      use m_boundary_params_sph_MHD
      use m_gaunt_coriolis_rlm
      use m_coriolis_terms_rlm
      use interact_coriolis_rlm
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      integer(kind = kint), intent(in) :: l_truncation
!
      integer(kind = kint) :: m
!
!
      call alloacte_gaunt_coriolis_rlm(sph_rlm%nidx_rlm(2))
      call alloc_coriolis_coef_tri_rlm(sph_rlm%nidx_rlm(2))
      call allocate_d_coriolis_rlm                                      &
     &   (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm(2))
!
!
      idx_rlm_ICB = find_local_radius_rlm_address(sph_rlm%nidx_rlm(1),  &
     &             sph_rlm%idx_gl_1d_rlm_r, sph_bc_U%kr_in)
      idx_rlm_degree_zero                                               &
     &           = find_local_sph_rlm_address(sph_rlm%nidx_rlm(2),      &
     &                      sph_rlm%idx_gl_1d_rlm_j, izero, izero)
      do m = -1, 1
        idx_rlm_degree_one(m)                                           &
     &           = find_local_sph_rlm_address(sph_rlm%nidx_rlm(2),      &
     &                         sph_rlm%idx_gl_1d_rlm_j, ione, m)
      end do
!
!
      if(iflag_debug.eq.1) write(*,*) 'cal_gaunt_coriolis_rlm'
      call cal_gaunt_coriolis_rlm(l_truncation,                         &
     &    sph_rlm%nidx_rlm(2), sph_rlm%idx_gl_1d_rlm_j)
!
      if(iflag_debug.eq.1) write(*,*) 'interact_rot_coriolis_rlm'
      call interact_rot_coriolis_rlm                                    &
     &   (sph_rlm%nidx_rlm(2), leg%g_sph_rlm)
!
      end subroutine init_sum_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      subroutine sum_coriolis_rlm(ncomp_trans, sph_rlm, comm_rlm,       &
     &          omega_sph, trns_MHD, leg, n_WR, WR)
!
      use t_boundary_params_sph_MHD
      use m_boundary_params_sph_MHD
      use m_coriolis_terms_rlm
      use sum_coriolis_terms_rlm
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_rotation), intent(in) :: omega_sph
      type(address_4_sph_trans), intent(in) :: trns_MHD 
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp_trans, n_WR
      real(kind = kreal), intent(in) :: WR(n_WR)
!
!
      if( iflag_4_coriolis .eq. id_turn_OFF) return
!
      call sum_rot_coriolis_rlm_10(trns_MHD%b_trns,                     &
     &    sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%a_r_1d_rlm_r,     &
     &    leg%g_sph_rlm, omega_sph%ws_rlm, fl_prop1%coef_cor,           &
     &    ncomp_trans, n_WR, comm_rlm%irev_sr, WR)
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call inner_core_rot_z_coriolis_rlm                              &
     &     (trns_MHD%b_trns, sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,        &
     &      sph_rlm%radius_1d_rlm_r, omega_sph%ws_rlm,                  &
     &      fl_prop1%coef_cor, ncomp_trans, n_WR, comm_rlm%irev_sr, WR)
      end if
!
!      call sum_div_coriolis_rlm_10                                     &
!     &   (trns_MHD%b_trns, sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,         &
!     &    sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r,               &
!     &    omega_sph%ws_rlm, fl_prop1%coef_cor, ncomp_trans,            &
!     &    n_WR, comm_rlm%irev_sr, WR)
!      call sum_r_coriolis_bc_rlm_10                                    &
!     &   (trns_MHD%b_trns, sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,         &
!     &    sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r,               &
!     &    omega_sph%ws_rlm, fl_prop1%coef_cor, ncomp_trans,            &
!     &    kr_in_U_rlm, n_WR, comm_rlm%irev_sr, WR, d_cor_in_rlm)
!      call sum_r_coriolis_bc_rlm_10                                    &
!     &   (trns_MHD%b_trns, sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,         &
!     &    sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r,               &
!     &    omega_sph%ws_rlm, fl_prop1%coef_cor, ncomp_trans,            &
!     &    kr_out_U_rlm, n_WR, comm_rlm%irev_sr, WR, d_cor_out_rlm)
!
      end subroutine sum_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      subroutine copy_coriolis_terms_rlm                                &
     &         (ncomp_trans, sph_rlm, comm_rlm, trns_MHD, n_WS, WS)
!
      use m_sph_communicators
      use m_sel_spherical_SRs
      use m_coriolis_terms_rlm
      use sum_coriolis_terms_rlm
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(address_4_sph_trans), intent(in) :: trns_MHD
!
      integer(kind = kint), intent(in) :: ncomp_trans, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if( iflag_4_coriolis .eq. id_turn_OFF) return
!
      call sel_calypso_to_send_scalar                                   &
     &   (ncomp_trans, sph_rlm%nnod_rlm, n_WS,                          &
     &    comm_rlm%nneib_domain, comm_rlm%istack_sr,                    &
     &    comm_rlm%item_sr, ncomp_coriolis_rlm, ip_rlm_rot_cor,         &
     &    trns_MHD%f_trns%i_rot_Coriolis, d_cor_rlm(1,1), WS(1))
      call sel_calypso_to_send_scalar                                   &
     &   (ncomp_trans, sph_rlm%nnod_rlm, n_WS,                          &
     &    comm_rlm%nneib_domain, comm_rlm%istack_sr,                    &
     &    comm_rlm%item_sr, ncomp_coriolis_rlm, it_rlm_rot_cor,         &
     &    (trns_MHD%f_trns%i_rot_Coriolis+2), d_cor_rlm(1,1), WS(1))
!
      end subroutine copy_coriolis_terms_rlm
!
! -----------------------------------------------------------------------
!
      end module  const_coriolis_sph_rlm
