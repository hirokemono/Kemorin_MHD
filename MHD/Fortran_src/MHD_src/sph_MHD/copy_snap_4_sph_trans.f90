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
!!      subroutine copy_snap_vec_spec_to_trans
!!      subroutine copy_snap_scl_spec_to_trans
!!
!!      subroutine copy_snap_vec_fld_from_trans
!!      subroutine copy_snap_scl_fld_from_trans
!!
!!  routines for forward transform
!!      subroutine copy_snap_scl_fld_to_trans
!!      subroutine copy_snap_scl_spec_from_trans
!!
!!      subroutine copy_snap_vec_fld_to_trans
!!      subroutine copy_snap_vec_spec_from_trans
!!@endverbatim
!
      module copy_snap_4_sph_trans
!
      use m_precision
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
      subroutine copy_snap_vec_spec_to_trans
!
      use copy_spectr_4_sph_trans
!
!
!$omp parallel
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_velo, 3*bsnap_trns%i_velo-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_vort, 3*bsnap_trns%i_vort-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_magne, 3*bsnap_trns%i_magne-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_current, 3*bsnap_trns%i_current-2)
!
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_v_diffuse, 3*bsnap_trns%i_v_diffuse-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_w_diffuse, 3*bsnap_trns%i_w_diffuse-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_vp_diffuse, 3*bsnap_trns%i_vp_diffuse-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_b_diffuse, 3*bsnap_trns%i_b_diffuse-2)
!
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_rot_inertia, 3*bsnap_trns%i_rot_inertia-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_rot_Coriolis, 3*bsnap_trns%i_rot_Coriolis-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_rot_Lorentz, 3*bsnap_trns%i_rot_Lorentz-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_rot_buoyancy, 3*bsnap_trns%i_rot_buoyancy-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_rot_comp_buo, 3*bsnap_trns%i_rot_comp_buo-2)
!
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_press_grad, 3*bsnap_trns%i_press_grad-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_induction, 3*bsnap_trns%i_induction-2)
!
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_grad_t, 3*bsnap_trns%i_grad_t-2)
      call copy_vec_spec_to_trans(3*nvector_snap_rj_2_rtp,                &
     &      ipol%i_grad_composit, 3*bsnap_trns%i_grad_composit-2)
!$omp end parallel
!
      end subroutine copy_snap_vec_spec_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_scl_spec_to_trans
!
      use copy_spectr_4_sph_trans
!
!
!$omp parallel
      call copy_scalar_spec_to_trans(nscalar_snap_rj_2_rtp,             &
     &      ipol%i_temp, bsnap_trns%i_temp)
      call copy_scalar_spec_to_trans(nscalar_snap_rj_2_rtp,             &
     &      ipol%i_light, bsnap_trns%i_light)
!
      call copy_scalar_spec_to_trans(nscalar_snap_rj_2_rtp,             &
     &      ipol%i_press, bsnap_trns%i_press)
      call copy_scalar_spec_to_trans(nscalar_snap_rj_2_rtp,             &
     &      ipol%i_par_temp, bsnap_trns%i_par_temp)
      call copy_scalar_spec_to_trans(nscalar_snap_rj_2_rtp,             &
     &      ipol%i_t_diffuse, bsnap_trns%i_t_diffuse)
      call copy_scalar_spec_to_trans(nscalar_snap_rj_2_rtp,             &
     &      ipol%i_c_diffuse, bsnap_trns%i_c_diffuse)
!
      call copy_scalar_spec_to_trans(nscalar_snap_rj_2_rtp,             &
     &      ipol%i_div_Coriolis, bsnap_trns%i_div_Coriolis)
!$omp end parallel
!
      end subroutine copy_snap_scl_spec_to_trans
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
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_velo, 3*bsnap_trns%i_velo-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_vort, 3*bsnap_trns%i_vort-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_magne, 3*bsnap_trns%i_magne-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_current, 3*bsnap_trns%i_current-2)
!
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_v_diffuse, 3*bsnap_trns%i_v_diffuse-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_w_diffuse, 3*bsnap_trns%i_w_diffuse-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_vp_diffuse, 3*bsnap_trns%i_vp_diffuse-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_b_diffuse, 3*bsnap_trns%i_b_diffuse-2)
!
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_rot_inertia, 3*bsnap_trns%i_rot_inertia-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_rot_Coriolis, 3*bsnap_trns%i_rot_Coriolis-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_rot_Lorentz, 3*bsnap_trns%i_rot_Lorentz-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_rot_buoyancy, 3*bsnap_trns%i_rot_buoyancy-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_rot_comp_buo, 3*bsnap_trns%i_rot_comp_buo-2)
!
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_press_grad, 3*bsnap_trns%i_press_grad-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_induction, 3*bsnap_trns%i_induction-2)
!
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_grad_t, 3*bsnap_trns%i_grad_t-2)
      call copy_vec_fld_from_trans(3*nvector_snap_rj_2_rtp,               &
     &    irtp%i_grad_composit, 3*bsnap_trns%i_grad_composit-2)
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
        call copy_scalar_fld_from_trans(nscalar_snap_rj_2_rtp,          &
     &      irtp%i_temp, bsnap_trns%i_temp)
        call copy_scalar_fld_from_trans(nscalar_snap_rj_2_rtp,          &
     &      irtp%i_light, bsnap_trns%i_light)
!
        call copy_scalar_fld_from_trans(nscalar_snap_rj_2_rtp,          &
     &      irtp%i_press, bsnap_trns%i_press)
        call copy_scalar_fld_from_trans(nscalar_snap_rj_2_rtp,          &
     &      irtp%i_par_temp, bsnap_trns%i_par_temp)
        call copy_scalar_fld_from_trans(nscalar_snap_rj_2_rtp,          &
     &      irtp%i_t_diffuse, bsnap_trns%i_t_diffuse)
        call copy_scalar_fld_from_trans(nscalar_snap_rj_2_rtp,          &
     &      irtp%i_c_diffuse, bsnap_trns%i_c_diffuse)
!
        call copy_scalar_fld_from_trans(nscalar_snap_rj_2_rtp,          &
     &      irtp%i_div_Coriolis, bsnap_trns%i_div_Coriolis)
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
      call copy_vec_fld_to_trans(3*nvector_snap_rtp_2_rj,                 &
     &      irtp%i_coriolis, 3*fsnap_trns%i_coriolis-2)
!
      call copy_vec_fld_to_trans(3*nvector_snap_rtp_2_rj,                 &
     &      irtp%i_electric, 3*fsnap_trns%i_electric-2)
      call copy_vec_fld_to_trans(3*nvector_snap_rtp_2_rj,                 &
     &      irtp%i_poynting, 3*fsnap_trns%i_poynting-2)
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
      call copy_scalar_fld_to_trans(nscalar_snap_rtp_2_rj,              &
     &      irtp%i_me_gen, fsnap_trns%i_me_gen)
      call copy_scalar_fld_to_trans(nscalar_snap_rtp_2_rj,              &
     &      irtp%i_ujb, fsnap_trns%i_ujb)
      call copy_scalar_fld_to_trans(nscalar_snap_rtp_2_rj,              &
     &      irtp%i_nega_ujb, fsnap_trns%i_nega_ujb)
      call copy_scalar_fld_to_trans(nscalar_snap_rtp_2_rj,              &
     &      irtp%i_buo_gen, fsnap_trns%i_buo_gen)
      call copy_scalar_fld_to_trans(nscalar_snap_rtp_2_rj,              &
     &      irtp%i_c_buo_gen, fsnap_trns%i_c_buo_gen)
      call copy_scalar_fld_to_trans(nscalar_snap_rtp_2_rj,              &
     &      irtp%i_f_buo_gen, fsnap_trns%i_f_buo_gen)
!$omp end parallel
!
      end  subroutine copy_snap_scl_fld_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_spec_from_trans
!
      use copy_spectr_4_sph_trans
!
!
!$omp parallel
      call copy_vec_spec_from_trans(3*nvector_snap_rtp_2_rj,              &
     &    ipol%i_coriolis, 3*fsnap_trns%i_coriolis-2)
!
      call copy_vec_spec_from_trans(3*nvector_snap_rtp_2_rj,              &
     &    ipol%i_electric, 3*fsnap_trns%i_electric-2)
      call copy_vec_spec_from_trans(3*nvector_snap_rtp_2_rj,              &
     &    ipol%i_poynting, 3*fsnap_trns%i_poynting-2)
!$omp end parallel
!
      end  subroutine copy_snap_vec_spec_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_scl_spec_from_trans
!
      use copy_spectr_4_sph_trans
!
!
!$omp parallel
      call copy_scalar_spec_from_trans(nscalar_snap_rtp_2_rj,           &
     &      ipol%i_me_gen, fsnap_trns%i_me_gen)
      call copy_scalar_spec_from_trans(nscalar_snap_rtp_2_rj,           &
     &      ipol%i_ujb, fsnap_trns%i_ujb)
      call copy_scalar_spec_from_trans(nscalar_snap_rtp_2_rj,           &
     &      ipol%i_nega_ujb, fsnap_trns%i_nega_ujb)
      call copy_scalar_spec_from_trans(nscalar_snap_rtp_2_rj,           &
     &      ipol%i_buo_gen, fsnap_trns%i_buo_gen)
      call copy_scalar_spec_from_trans(nscalar_snap_rtp_2_rj,           &
     &      ipol%i_c_buo_gen, fsnap_trns%i_c_buo_gen)
      call copy_scalar_spec_from_trans(nscalar_snap_rtp_2_rj,           &
     &      ipol%i_f_buo_gen, fsnap_trns%i_f_buo_gen)
!$omp end parallel
!
      end  subroutine copy_snap_scl_spec_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_snap_4_sph_trans
