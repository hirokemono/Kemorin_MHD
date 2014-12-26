!>@file   copy_snap_4_sph_trans.f90
!!@brief  module copy_snap_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!      subroutine copy_snap_vec_fld_from_trans
!!      subroutine copy_tmp_vec_fld_from_trans
!!
!!      subroutine copy_snap_vec_fld_to_trans
!!      subroutine copy_tmp_scl_fld_to_trans
!!@endverbatim
!
      module copy_snap_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use m_sph_phys_address
      use select_sph_fld_4_sph_trans
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_fld_from_trans
!
      use m_addresses_trans_sph_snap
!
!
!$omp parallel
!  Copy vectors
      call sel_vector_fld_from_trans(irtp%i_velo, bsnap_trns%i_velo)
      call sel_vector_fld_from_trans(irtp%i_vort, bsnap_trns%i_vort)
      call sel_vector_fld_from_trans(irtp%i_magne, bsnap_trns%i_magne)
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_current, bsnap_trns%i_current)
!
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_v_diffuse, bsnap_trns%i_v_diffuse)
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_w_diffuse, bsnap_trns%i_w_diffuse)
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_vp_diffuse, bsnap_trns%i_vp_diffuse)
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_b_diffuse, bsnap_trns%i_b_diffuse)
!
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_rot_inertia, bsnap_trns%i_rot_inertia)
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_rot_Coriolis, bsnap_trns%i_rot_Coriolis)
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_rot_Lorentz, bsnap_trns%i_rot_Lorentz)
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_rot_buoyancy, bsnap_trns%i_rot_buoyancy)
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_rot_comp_buo, bsnap_trns%i_rot_comp_buo)
!
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_press_grad, bsnap_trns%i_press_grad)
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_induction, bsnap_trns%i_induction)
!
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_grad_t, bsnap_trns%i_grad_t)
      call sel_vector_fld_from_trans                                    &
     &   (irtp%i_grad_composit, bsnap_trns%i_grad_composit)
!
!  Copy scalars
      call sel_scalar_fld_from_trans(irtp%i_temp, bsnap_trns%i_temp)
      call sel_scalar_fld_from_trans(irtp%i_light, bsnap_trns%i_light)
!
      call sel_scalar_fld_from_trans(irtp%i_press, bsnap_trns%i_press)
      call sel_scalar_fld_from_trans                                    &
     &      (irtp%i_par_temp, bsnap_trns%i_par_temp)
      call sel_scalar_fld_from_trans                                    &
     &      (irtp%i_t_diffuse, bsnap_trns%i_t_diffuse)
      call sel_scalar_fld_from_trans                                    &
     &      (irtp%i_c_diffuse, bsnap_trns%i_c_diffuse)
!
      call sel_scalar_fld_from_trans                                    &
     &      (irtp%i_div_Coriolis, bsnap_trns%i_div_Coriolis)
!$omp end parallel
!
      end subroutine copy_snap_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_vec_fld_from_trans
!
      use m_addresses_trans_sph_tmp
!
!
!$omp parallel
      call sel_vector_fld_from_trans                                    &
     &    (irtp%i_grad_vx, btmp_trns%i_grad_vx)
      call sel_vector_fld_from_trans                                    &
     &    (irtp%i_grad_vy, btmp_trns%i_grad_vy)
      call sel_vector_fld_from_trans                                    &
     &    (irtp%i_grad_vz, btmp_trns%i_grad_vz)
!$omp end parallel
!
      end subroutine copy_tmp_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_fld_to_trans
!
      use m_addresses_trans_sph_snap
!
!
!$omp parallel
      call sel_vector_fld_to_trans                                      &
     &    (irtp%i_coriolis, fsnap_trns%i_coriolis)
!
      call sel_vector_fld_to_trans                                      &
     &    (irtp%i_electric, fsnap_trns%i_electric)
      call sel_vector_fld_to_trans                                      &
     &    (irtp%i_poynting, fsnap_trns%i_poynting)
!
      call sel_vector_fld_to_trans                                      &
     &    (irtp%i_mag_stretch, fsnap_trns%i_mag_stretch)
!
!
      call sel_scalar_fld_to_trans(irtp%i_me_gen, fsnap_trns%i_me_gen)
      call sel_scalar_fld_to_trans(irtp%i_ujb, fsnap_trns%i_ujb)
      call sel_scalar_fld_to_trans                                      &
     &   (irtp%i_nega_ujb, fsnap_trns%i_nega_ujb)
      call sel_scalar_fld_to_trans                                      &
     &   (irtp%i_buo_gen, fsnap_trns%i_buo_gen)
      call sel_scalar_fld_to_trans                                      &
     &   (irtp%i_c_buo_gen, fsnap_trns%i_c_buo_gen)
      call sel_scalar_fld_to_trans                                      &
     &   (irtp%i_f_buo_gen, fsnap_trns%i_f_buo_gen)
!$omp end parallel
!
      end  subroutine copy_snap_vec_fld_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_scl_fld_to_trans
!
      use m_addresses_trans_sph_tmp
!
!
!$omp parallel
      call sel_scalar_fld_to_trans                                      &
     &    (irtp%i_grad_vx, ftmp_trns%i_grad_vx)
      call sel_scalar_fld_to_trans                                      &
     &    (irtp%i_grad_vy, ftmp_trns%i_grad_vy)
      call sel_scalar_fld_to_trans                                      &
     &    (irtp%i_grad_vz, ftmp_trns%i_grad_vz)
!$omp end parallel
!
      end  subroutine copy_tmp_scl_fld_to_trans
!
!-----------------------------------------------------------------------
!
      end module copy_snap_4_sph_trans
