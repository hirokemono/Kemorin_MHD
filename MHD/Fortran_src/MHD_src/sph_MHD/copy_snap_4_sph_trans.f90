!>@file   copy_snap_4_sph_trans.f90
!!@brief  module copy_snap_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!  routines for backward transform
!!      subroutine copy_snap_spectr_to_send
!!
!!      subroutine copy_snap_vec_fld_from_trans
!!      subroutine copy_snap_scl_fld_from_trans
!!
!!  routines for forward transform
!!      subroutine copy_snap_spectr_to_send(ncomp_send, n_WS, WS)
!!
!!      subroutine copy_snap_vec_fld_to_trans
!!      subroutine copy_snap_vec_spec_from_trans(ncomp_recv, n_WR, WR)
!!@endverbatim
!
      module copy_snap_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use m_sph_phys_address
      use m_addresses_trans_sph_snap
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_spectr_to_send(ncomp_send, n_WS, WS)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!      Vectors
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_velo, bsnap_trns%i_velo, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_vort, bsnap_trns%i_vort, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_magne, bsnap_trns%i_magne, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_current, bsnap_trns%i_current, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_v_diffuse, bsnap_trns%i_v_diffuse, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_w_diffuse, bsnap_trns%i_w_diffuse, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_vp_diffuse, bsnap_trns%i_vp_diffuse, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_b_diffuse, bsnap_trns%i_b_diffuse, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_inertia, bsnap_trns%i_rot_inertia, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_Coriolis, bsnap_trns%i_rot_Coriolis, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_Lorentz, bsnap_trns%i_rot_Lorentz, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_buoyancy, bsnap_trns%i_rot_buoyancy, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_comp_buo, bsnap_trns%i_rot_comp_buo, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_press_grad, bsnap_trns%i_press_grad, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_induction, bsnap_trns%i_induction, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_grad_t, bsnap_trns%i_grad_t, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_grad_composit, bsnap_trns%i_grad_composit, n_WS, WS)
!
!      Scalar fields
!
      call sel_sph_rj_scalar_to_send(ncomp_send,                        &
     &    ipol%i_temp, bsnap_trns%i_temp, n_WS, WS)
      call sel_sph_rj_scalar_to_send(ncomp_send,                        &
     &    ipol%i_light, bsnap_trns%i_light, n_WS, WS)
!
      call sel_sph_rj_scalar_to_send(ncomp_send,                        &
     &    ipol%i_press, bsnap_trns%i_press, n_WS, WS)
      call sel_sph_rj_scalar_to_send(ncomp_send,                        &
     &    ipol%i_par_temp, bsnap_trns%i_par_temp, n_WS, WS)
      call sel_sph_rj_scalar_to_send(ncomp_send,                        &
     &    ipol%i_t_diffuse, bsnap_trns%i_t_diffuse, n_WS, WS)
      call sel_sph_rj_scalar_to_send(ncomp_send,                        &
     &    ipol%i_c_diffuse, bsnap_trns%i_c_diffuse, n_WS, WS)
!
      call sel_sph_rj_scalar_to_send(ncomp_send,                        &
     &    ipol%i_div_Coriolis, bsnap_trns%i_div_Coriolis, n_WS, WS)
!
      end subroutine copy_snap_spectr_to_send
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_fld_from_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
      call copy_vec_fld_from_trans(irtp%i_velo, bsnap_trns%i_velo)
      call copy_vec_fld_from_trans(irtp%i_vort, bsnap_trns%i_vort)
      call copy_vec_fld_from_trans(irtp%i_magne, bsnap_trns%i_magne)
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_current, bsnap_trns%i_current)
!
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_v_diffuse, bsnap_trns%i_v_diffuse)
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_w_diffuse, bsnap_trns%i_w_diffuse)
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_vp_diffuse, bsnap_trns%i_vp_diffuse)
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_b_diffuse, bsnap_trns%i_b_diffuse)
!
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_rot_inertia, bsnap_trns%i_rot_inertia)
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_rot_Coriolis, bsnap_trns%i_rot_Coriolis)
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_rot_Lorentz, bsnap_trns%i_rot_Lorentz)
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_rot_buoyancy, bsnap_trns%i_rot_buoyancy)
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_rot_comp_buo, bsnap_trns%i_rot_comp_buo)
!
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_press_grad, bsnap_trns%i_press_grad)
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_induction, bsnap_trns%i_induction)
!
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_grad_t, bsnap_trns%i_grad_t)
      call copy_vec_fld_from_trans                                      &
     &   (irtp%i_grad_composit, bsnap_trns%i_grad_composit)
!$omp end parallel
!
      end subroutine copy_snap_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_scl_fld_from_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
      call copy_scalar_fld_from_trans(irtp%i_temp, bsnap_trns%i_temp)
      call copy_scalar_fld_from_trans(irtp%i_light, bsnap_trns%i_light)
!
      call copy_scalar_fld_from_trans(irtp%i_press, bsnap_trns%i_press)
      call copy_scalar_fld_from_trans                                   &
     &      (irtp%i_par_temp, bsnap_trns%i_par_temp)
      call copy_scalar_fld_from_trans                                   &
     &      (irtp%i_t_diffuse, bsnap_trns%i_t_diffuse)
      call copy_scalar_fld_from_trans                                   &
     &      (irtp%i_c_diffuse, bsnap_trns%i_c_diffuse)
!
      call copy_scalar_fld_from_trans                                   &
     &      (irtp%i_div_Coriolis, bsnap_trns%i_div_Coriolis)
!$omp end parallel
!
      end subroutine copy_snap_scl_fld_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_fld_to_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
      call copy_vec_fld_to_trans                                        &
     &    (irtp%i_coriolis, fsnap_trns%i_coriolis)
!
      call copy_vec_fld_to_trans                                        &
     &    (irtp%i_electric, fsnap_trns%i_electric)
      call copy_vec_fld_to_trans                                        &
     &    (irtp%i_poynting, fsnap_trns%i_poynting)
!
      call copy_vec_fld_to_trans                                        &
     &    (irtp%i_mag_stretch, fsnap_trns%i_mag_stretch)
!$omp end parallel
!
      end  subroutine copy_snap_vec_fld_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_scl_fld_to_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
      call copy_scalar_fld_to_trans(irtp%i_me_gen, fsnap_trns%i_me_gen)
      call copy_scalar_fld_to_trans(irtp%i_ujb, fsnap_trns%i_ujb)
      call copy_scalar_fld_to_trans                                     &
     &   (irtp%i_nega_ujb, fsnap_trns%i_nega_ujb)
      call copy_scalar_fld_to_trans                                     &
     &   (irtp%i_buo_gen, fsnap_trns%i_buo_gen)
      call copy_scalar_fld_to_trans                                     &
     &   (irtp%i_c_buo_gen, fsnap_trns%i_c_buo_gen)
      call copy_scalar_fld_to_trans                                     &
     &   (irtp%i_f_buo_gen, fsnap_trns%i_f_buo_gen)
!$omp end parallel
!
      end  subroutine copy_snap_scl_fld_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_spec_from_trans(ncomp_recv, n_WR, WR)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &    ipol%i_coriolis, fsnap_trns%i_coriolis, n_WR, WR)
!
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &    ipol%i_electric, fsnap_trns%i_electric, n_WR, WR)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &    ipol%i_poynting, fsnap_trns%i_poynting, n_WR, WR)
!
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &    ipol%i_mag_stretch, fsnap_trns%i_mag_stretch, n_WR, WR)
!
!      Scalars
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_me_gen, fsnap_trns%i_me_gen, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_ujb, fsnap_trns%i_ujb, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_nega_ujb, fsnap_trns%i_nega_ujb, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_buo_gen, fsnap_trns%i_buo_gen, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_c_buo_gen, fsnap_trns%i_c_buo_gen, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_f_buo_gen, fsnap_trns%i_f_buo_gen, n_WR, WR)
!
      end  subroutine copy_snap_vec_spec_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_snap_4_sph_trans
