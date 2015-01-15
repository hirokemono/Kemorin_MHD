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
!!@endverbatim
!
      module copy_snap_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use m_sph_phys_address
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
      call copy_vector_from_snap_trans(bsnap_trns%i_velo, irtp%i_velo)
      call copy_vector_from_snap_trans(bsnap_trns%i_vort, irtp%i_vort)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_magne, irtp%i_magne)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_current, irtp%i_current)
!
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_v_diffuse, irtp%i_v_diffuse)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_w_diffuse, irtp%i_w_diffuse)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_vp_diffuse, irtp%i_vp_diffuse)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_b_diffuse, irtp%i_b_diffuse)
!
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_rot_inertia, irtp%i_rot_inertia)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_rot_Coriolis, irtp%i_rot_Coriolis)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_rot_Lorentz, irtp%i_rot_Lorentz)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_rot_buoyancy, irtp%i_rot_buoyancy)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_rot_comp_buo, irtp%i_rot_comp_buo)
!
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_press_grad, irtp%i_press_grad)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_induction, irtp%i_induction)
!
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_grad_t, irtp%i_grad_t)
      call copy_vector_from_snap_trans                                  &
     &   (bsnap_trns%i_grad_composit, irtp%i_grad_composit)
!
!  Copy scalars
      call copy_scalar_from_snap_trans(bsnap_trns%i_temp, irtp%i_temp)
      call copy_scalar_from_snap_trans                                  &
     &      (bsnap_trns%i_light, irtp%i_light)
!
      call copy_scalar_from_snap_trans                                  &
     &      (bsnap_trns%i_press, irtp%i_press)
      call copy_scalar_from_snap_trans                                  &
     &      (bsnap_trns%i_par_temp, irtp%i_par_temp)
      call copy_scalar_from_snap_trans                                  &
     &      (bsnap_trns%i_filter_temp, irtp%i_filter_temp)
      call copy_scalar_from_snap_trans                                  &
     &      (bsnap_trns%i_t_diffuse, irtp%i_t_diffuse)
      call copy_scalar_from_snap_trans                                  &
     &      (bsnap_trns%i_c_diffuse, irtp%i_c_diffuse)
!
      call copy_scalar_from_snap_trans                                  &
     &      (bsnap_trns%i_div_Coriolis, irtp%i_div_Coriolis)
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
      call copy_vector_from_tmp_trans                                   &
     &    (btmp_trns%i_grad_vx, irtp%i_grad_vx)
      call copy_vector_from_tmp_trans                                   &
     &    (btmp_trns%i_grad_vy, irtp%i_grad_vy)
      call copy_vector_from_tmp_trans                                   &
     &    (btmp_trns%i_grad_vz, irtp%i_grad_vz)
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
      call copy_vector_from_snap_force                                  &
     &    (fsnap_trns%i_coriolis, irtp%i_Coriolis)
!
      call copy_vector_from_snap_force                                  &
     &    (fsnap_trns%i_electric, irtp%i_electric)
      call copy_vector_from_snap_force                                  &
     &    (fsnap_trns%i_poynting, irtp%i_poynting)
!
      call copy_vector_from_snap_force                                  &
     &    (fsnap_trns%i_mag_stretch, irtp%i_mag_stretch)
!
!
      call copy_scalar_from_snap_force                                  &
     &   (fsnap_trns%i_me_gen, irtp%i_me_gen)
      call copy_scalar_from_snap_force(fsnap_trns%i_ujb, irtp%i_ujb)
      call copy_scalar_from_snap_force                                  &
     &   (fsnap_trns%i_nega_ujb, irtp%i_nega_ujb)
      call copy_scalar_from_snap_force                                  &
     &   (fsnap_trns%i_buo_gen, irtp%i_buo_gen)
      call copy_scalar_from_snap_force                                  &
     &   (fsnap_trns%i_c_buo_gen, irtp%i_c_buo_gen)
      call copy_scalar_from_snap_force                                  &
     &   (fsnap_trns%i_f_buo_gen, irtp%i_f_buo_gen)
!
      call copy_scalar_from_snap_force                                  &
     &   (fsnap_trns%i_velo_scale, irtp%i_velo_scale)
      call copy_scalar_from_snap_force                                  &
     &   (fsnap_trns%i_magne_scale, irtp%i_magne_scale)
      call copy_scalar_from_snap_force                                  &
     &   (fsnap_trns%i_temp_scale, irtp%i_temp_scale)
      call copy_scalar_from_snap_force                                  &
     &   (fsnap_trns%i_comp_scale, irtp%i_comp_scale)
!$omp end parallel
!
      end  subroutine copy_snap_vec_fld_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_snap_trans(i_trns, irtp_fld)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_addresses_trans_sph_snap
      use m_sph_spectr_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: irtp_fld, i_trns
!
!
      if( (irtp_fld*i_trns) .le. 0) return
      call copy_scalar_from_trans(nnod_rtp, inod_rtp_smp_stack,         &
     &    nnod_rtp, fld_snap_rtp(1,i_trns), d_rtp(1,irtp_fld) )
!
      end subroutine copy_scalar_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_snap_trans(i_trns, irtp_fld)
!
      use m_addresses_trans_sph_snap
      use m_sph_spectr_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: irtp_fld, i_trns
!
!
      if( (irtp_fld*i_trns) .le. 0) return
      call copy_vector_from_trans(nnod_rtp, inod_rtp_smp_stack,         &
     &    nnod_rtp, fld_snap_rtp(1,i_trns), d_rtp(1,irtp_fld) )
!
      end subroutine copy_vector_from_snap_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_snap_force(i_trns, irtp_fld)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_addresses_trans_sph_snap
      use m_sph_spectr_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: irtp_fld, i_trns
!
!
      if( (irtp_fld*i_trns) .le. 0) return
      call copy_scalar_from_trans(nnod_rtp, inod_rtp_smp_stack,         &
     &    nnod_rtp, frc_snap_rtp(1,i_trns), d_rtp(1,irtp_fld) )
!
      end subroutine copy_scalar_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_snap_force(i_trns, irtp_fld)
!
      use m_addresses_trans_sph_snap
      use m_sph_spectr_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: irtp_fld, i_trns
!
!
      if( (irtp_fld*i_trns) .le. 0) return
      call copy_vector_from_trans(nnod_rtp, inod_rtp_smp_stack,         &
     &    nnod_rtp, frc_snap_rtp(1,i_trns), d_rtp(1,irtp_fld) )
!
      end subroutine copy_vector_from_snap_force
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_tmp_trans(i_trns, irtp_fld)
!
      use m_addresses_trans_sph_tmp
      use m_sph_spectr_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: irtp_fld, i_trns
!
!
      if( (irtp_fld*i_trns) .le. 0) return
      call copy_vector_from_trans(nnod_rtp, inod_rtp_smp_stack,         &
     &    nnod_rtp, fld_tmp_rtp(1,i_trns), d_rtp(1,irtp_fld) )
!
      end subroutine copy_vector_from_tmp_trans
!
!-----------------------------------------------------------------------
!
      end module copy_snap_4_sph_trans
