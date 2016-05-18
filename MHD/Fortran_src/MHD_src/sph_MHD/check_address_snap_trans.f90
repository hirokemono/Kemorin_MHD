!>@file   check_address_snap_trans.f90
!!@brief  module check_address_snap_trans
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!        subroutine check_address_trans_sph_snap(trns_snap)
!!        type(address_4_sph_trans), intent(in) :: trns_snap
!!@endverbatim
!
      module check_address_snap_trans
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
      private :: check_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine check_address_trans_sph_snap(trns_snap)
!
      type(address_4_sph_trans), intent(in) :: trns_snap
!
      call check_addresses_snapshot_trans                               &
     &   (trns_snap%b_trns, trns_snap%f_trns,                           &
     &    trns_snap%ncomp_rj_2_rtp, trns_snap%nvector_rj_2_rtp,         &
     &    trns_snap%nscalar_rj_2_rtp, trns_snap%ncomp_rtp_2_rj,         &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj)
!
      end subroutine check_address_trans_sph_snap
!
!-----------------------------------------------------------------------
!
      subroutine check_addresses_snapshot_trans(bs_trns, fs_trns,       &
     &          ncomp_snap_rj_2_rtp, nvector_snap_rj_2_rtp,             &
     &          nscalar_snap_rj_2_rtp, ncomp_snap_rtp_2_rj,             &
     &          nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj)
!
      use m_node_phys_data
      use m_sph_phys_address
!
      type(phys_address), intent(in) :: bs_trns, fs_trns
      integer(kind = kint), intent(in) :: ncomp_snap_rj_2_rtp
      integer(kind = kint), intent(in) :: nvector_snap_rj_2_rtp
      integer(kind = kint), intent(in) :: nscalar_snap_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_snap_rtp_2_rj
      integer(kind = kint), intent(in) :: nvector_snap_rtp_2_rj
      integer(kind = kint), intent(in) :: nscalar_snap_rtp_2_rj
!
      write(*,*) 'ncomp_snap_rj_2_rtp', ncomp_snap_rj_2_rtp
      write(*,*) 'ncomp_snap_rtp_2_rj', ncomp_snap_rtp_2_rj
!
      write(*,*) 'nvector_snap_rj_2_rtp', nvector_snap_rj_2_rtp
      if(bs_trns%i_velo .gt. 0) write(*,*)                              &
     &            'bs_trns%i_velo', bs_trns%i_velo,                     &
     &            ipol%i_velo, iphys%i_velo
      if(bs_trns%i_vort .gt. 0) write(*,*)                              &
     &            'bs_trns%i_vort', bs_trns%i_vort,                     &
     &            ipol%i_vort, iphys%i_vort
      if(bs_trns%i_magne .gt. 0) write(*,*)                             &
     &            'bs_trns%i_magne', bs_trns%i_magne,                   &
     &            ipol%i_magne, iphys%i_magne
      if(bs_trns%i_current .gt. 0) write(*,*)                           &
     &            'bs_trns%i_current', bs_trns%i_current,               &
     &            ipol%i_current, iphys%i_current
!
      if(bs_trns%i_v_diffuse .gt. 0) write(*,*)                         &
     &            'bs_trns%i_v_diffuse', bs_trns%i_v_diffuse,           &
     &            ipol%i_v_diffuse, iphys%i_v_diffuse
      if(bs_trns%i_w_diffuse .gt. 0) write(*,*)                         &
     &            'bs_trns%i_w_diffuse', bs_trns%i_w_diffuse,           &
     &            ipol%i_w_diffuse, iphys%i_w_diffuse
      if(bs_trns%i_vp_diffuse .gt. 0) write(*,*)                        &
     &            'bs_trns%i_vp_diffuse', bs_trns%i_vp_diffuse,         &
     &            ipol%i_vp_diffuse, iphys%i_vp_diffuse
      if(bs_trns%i_b_diffuse .gt. 0) write(*,*)                         &
     &            'bs_trns%i_b_diffuse', bs_trns%i_b_diffuse,           &
     &            ipol%i_b_diffuse, iphys%i_b_diffuse
!
      if(bs_trns%i_rot_inertia .gt. 0) write(*,*)                       &
     &         'bs_trns%i_rot_inertia', bs_trns%i_rot_inertia,          &
     &          ipol%i_rot_inertia, iphys%i_rot_inertia
      if(bs_trns%i_rot_Coriolis .gt. 0) write(*,*)                      &
     &         'bs_trns%i_rot_Coriolis', bs_trns%i_rot_Coriolis,        &
     &          ipol%i_rot_Coriolis, iphys%i_rot_Coriolis
      if(bs_trns%i_rot_Lorentz .gt. 0) write(*,*)                       &
     &         'bs_trns%i_rot_Lorentz',  bs_trns%i_rot_Lorentz,         &
     &          ipol%i_rot_Lorentz, iphys%i_rot_Lorentz
      if(bs_trns%i_rot_buoyancy .gt. 0) write(*,*)                      &
     &         'bs_trns%i_rot_buoyancy',  bs_trns%i_rot_buoyancy,       &
     &          ipol%i_rot_buoyancy, iphys%i_rot_buoyancy
      if(bs_trns%i_rot_comp_buo .gt. 0) write(*,*)                      &
     &         'bs_trns%i_rot_comp_buo',  bs_trns%i_rot_comp_buo,       &
     &          ipol%i_rot_comp_buo, iphys%i_rot_comp_buo
!
      if(bs_trns%i_press_grad .gt. 0) write(*,*)                        &
     &            'bs_trns%i_press_grad', bs_trns%i_press_grad,         &
     &             ipol%i_press_grad, iphys%i_press_grad
      if(bs_trns%i_induction .gt. 0) write(*,*)                         &
     &            'bs_trns%i_induction', bs_trns%i_induction,           &
     &            ipol%i_induction, iphys%i_induction
!
      if(bs_trns%i_grad_t .gt. 0) write(*,*)                            &
     &            'bs_trns%i_grad_t',    bs_trns%i_grad_t,              &
     &            ipol%i_grad_t, iphys%i_grad_t
      if(bs_trns%i_grad_composit .gt. 0) write(*,*)                     &
     &            'bs_trns%i_grad_composit',                            &
     &            bs_trns%i_grad_composit,                              &
     &            ipol%i_grad_composit, iphys%i_grad_composit
!
      if(bs_trns%i_grad_vx .gt. 0) write(*,*)                           &
     &            'bs_trns%i_grad_vx', bs_trns%i_grad_vx,               &
     &            ipol%i_grad_vx, iphys%i_grad_vx
      if(bs_trns%i_grad_vy .gt. 0) write(*,*)                           &
     &            'bs_trns%i_grad_vy', bs_trns%i_grad_vy,               &
     &            ipol%i_grad_vy, iphys%i_grad_vy
      if(bs_trns%i_grad_vz .gt. 0) write(*,*)                           &
     &            'bs_trns%i_grad_vz', bs_trns%i_grad_vz,               &
     &            ipol%i_grad_vz, iphys%i_grad_vz
      write(*,*)
!
      write(*,*) 'nscalar_snap_rj_2_rtp', nscalar_snap_rj_2_rtp
      if(bs_trns%i_temp .gt. 0) write(*,*)                              &
     &            'bs_trns%i_temp', bs_trns%i_temp,                     &
     &            ipol%i_temp, iphys%i_temp
      if(bs_trns%i_light .gt. 0) write(*,*)                             &
     &            'bs_trns%i_light', bs_trns%i_light,                   &
     &            ipol%i_light, iphys%i_light
      if(bs_trns%i_press .gt. 0) write(*,*)                             &
     &            'bs_trns%i_press', bs_trns%i_press,                   &
     &            ipol%i_press, iphys%i_press
      if(bs_trns%i_par_temp .gt. 0) write(*,*)                          &
     &            'bs_trns%i_par_temp', bs_trns%i_par_temp,             &
     &            ipol%i_par_temp, iphys%i_par_temp
!
      if(bs_trns%i_filter_temp .gt. 0) write(*,*)                       &
     &            'bs_trns%i_filter_temp', bs_trns%i_filter_temp,       &
     &            ipol%i_filter_temp, iphys%i_filter_temp
!
      if(bs_trns%i_t_diffuse .gt. 0) write(*,*)                         &
     &            'bs_trns%i_t_diffuse', bs_trns%i_t_diffuse,           &
     &            ipol%i_t_diffuse, iphys%i_t_diffuse
      if(bs_trns%i_c_diffuse .gt. 0) write(*,*)                         &
     &            'bs_trns%i_c_diffuse', bs_trns%i_c_diffuse,           &
     &            ipol%i_c_diffuse, iphys%i_c_diffuse
!
      if(bs_trns%i_div_Coriolis .gt. 0) write(*,*)                      &
     &          'bs_trns%i_div_Coriolis', bs_trns%i_div_Coriolis,       &
     &          ipol%i_div_Coriolis, iphys%i_div_Coriolis
      write(*,*)
!
!
      write(*,*) 'nvector_snap_rtp_2_rj', nvector_snap_rtp_2_rj
      if(fs_trns%i_coriolis .gt. 0) write(*,*)                          &
     &            'fs_trns%i_coriolis',  fs_trns%i_coriolis,            &
     &            ipol%i_coriolis, iphys%i_coriolis
!
      if(fs_trns%i_electric .gt. 0) write(*,*)                          &
     &            'fs_trns%i_electric',  fs_trns%i_electric,            &
     &            ipol%i_electric, iphys%i_electric
      if(fs_trns%i_poynting .gt. 0) write(*,*)                          &
     &            'fs_trns%i_poynting',  fs_trns%i_poynting,            &
     &            ipol%i_poynting, iphys%i_poynting
!
      if(fs_trns%i_mag_stretch .gt. 0) write(*,*)                       &
     &            'fs_trns%i_mag_stretch',                              &
     &            fs_trns%i_mag_stretch,  ipol%i_mag_stretch,           &
     &            iphys%i_mag_stretch
      write(*,*)
!
      write(*,*) 'nscalar_snap_rtp_2_rj', nscalar_snap_rtp_2_rj
      if(fs_trns%i_me_gen .gt. 0) write(*,*)                            &
     &            'fs_trns%i_me_gen', fs_trns%i_me_gen,                 &
     &            ipol%i_me_gen, iphys%i_me_gen
      if(fs_trns%i_ujb .gt. 0) write(*,*)                               &
     &            'fs_trns%i_ujb', fs_trns%i_ujb,                       &
     &            ipol%i_ujb, iphys%i_ujb
      if(fs_trns%i_nega_ujb .gt. 0) write(*,*)                          &
     &            'fs_trns%i_nega_ujb',  fs_trns%i_nega_ujb,            &
     &            ipol%i_nega_ujb, iphys%i_nega_ujb
!
      if(fs_trns%i_buo_gen .gt. 0) write(*,*)                           &
     &            'fs_trns%i_buo_gen',   fs_trns%i_buo_gen,             &
     &            ipol%i_buo_gen, iphys%i_buo_gen
      if(fs_trns%i_c_buo_gen .gt. 0) write(*,*)                         &
     &            'fs_trns%i_c_buo_gen', fs_trns%i_c_buo_gen,           &
     &            ipol%i_c_buo_gen, iphys%i_c_buo_gen
      if(fs_trns%i_f_buo_gen .gt. 0) write(*,*)                         &
     &            'fs_trns%i_f_buo_gen', fs_trns%i_f_buo_gen,           &
     &            ipol%i_f_buo_gen, iphys%i_f_buo_gen
!
      if(fs_trns%i_velo_scale .gt. 0) write(*,*)                        &
     &            'fs_trns%i_velo_scale', fs_trns%i_velo_scale,         &
     &            ipol%i_velo_scale, iphys%i_velo_scale
      if(fs_trns%i_magne_scale .gt. 0) write(*,*)                       &
     &            'fs_trns%i_magne_scale', fs_trns%i_magne_scale,       &
     &            ipol%i_magne_scale, iphys%i_magne_scale
      if(fs_trns%i_temp_scale .gt. 0) write(*,*)                        &
     &            'fs_trns%i_temp_scale', fs_trns%i_temp_scale,         &
     &            ipol%i_temp_scale, iphys%i_temp_scale
      if(fs_trns%i_comp_scale .gt. 0) write(*,*)                        &
     &            'fs_trns%i_comp_scale', fs_trns%i_comp_scale,         &
     &            ipol%i_comp_scale, iphys%i_comp_scale
        write(*,*)
!
      end subroutine check_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!
      end module check_address_snap_trans
