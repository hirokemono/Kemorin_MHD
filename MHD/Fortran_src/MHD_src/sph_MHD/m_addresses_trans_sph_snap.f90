!>@file   m_addresses_trans_sph_snap.f90
!!@brief  module m_addresses_trans_sph_snap
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine allocate_snap_trans_rtp
!!      subroutine deallocate_snap_trans_rtp
!!
!!      subroutine set_addresses_snapshot_trans
!!      subroutine check_addresses_snapshot_trans
!!@endverbatim
!
      module m_addresses_trans_sph_snap
!
      use m_precision
!
      use t_phys_address
!
      implicit none
!
!>      number of components for backward spherical harmonics transform
      integer(kind = kint) :: ncomp_snap_rj_2_rtp = 0
!>      number of components
!!      for backward vector spherical harmonics transform
      integer(kind = kint) :: nvector_snap_rj_2_rtp = 0
!>      number of scalars for backward spherical harmonics transform
      integer(kind = kint) :: nscalar_snap_rj_2_rtp = 0
!>      number of tensors for backward spherical harmonics transform
      integer(kind = kint) :: ntensor_snap_rj_2_rtp = 0
!
!>      number of components for forward spherical harmonics transform
      integer(kind = kint) :: ncomp_snap_rtp_2_rj = 0
!>      number of components
!!      for forward vector spherical harmonics transform
      integer(kind = kint) :: nvector_snap_rtp_2_rj = 0
!>      number of scalars for forward spherical harmonics transform
      integer(kind = kint) :: nscalar_snap_rtp_2_rj = 0
!>      number of tensors for forward spherical harmonics transform
      integer(kind = kint) :: ntensor_snap_rtp_2_rj = 0
!
!>    addresses for fields to backward transform
      type(phys_address), save :: bsnap_trns
!
!>    addresses for forces to forward transform
      type(phys_address), save :: fsnap_trns
!
!>      field data to evaluate nonliear terms in grid space
      real(kind = kreal), allocatable :: fld_snap_rtp(:,:)
!>      Nonoliear terms data in grid space
      real(kind = kreal), allocatable :: frc_snap_rtp(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_snap_trans_rtp
!
      use m_spheric_parameter
!
!
      allocate(fld_snap_rtp(nnod_rtp,ncomp_snap_rj_2_rtp))
      allocate(frc_snap_rtp(nnod_rtp,ncomp_snap_rtp_2_rj))
      if(ncomp_snap_rj_2_rtp .gt. 0) fld_snap_rtp = 0.0d0
      if(ncomp_snap_rtp_2_rj .gt. 0) frc_snap_rtp = 0.0d0
!
      end subroutine allocate_snap_trans_rtp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_snap_trans_rtp
!
      deallocate(fld_snap_rtp, frc_snap_rtp)
!
      end subroutine deallocate_snap_trans_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_addresses_snapshot_trans
!
      use m_work_4_sph_trans
      use m_node_phys_address
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
!
      nvector_snap_rtp_2_rj = 0
      call add_vec_trans_flag(ipol%i_coriolis, iphys%i_coriolis,        &
     &    nvector_snap_rtp_2_rj, fsnap_trns%i_coriolis)
      call add_vec_trans_flag(ipol%i_electric, iphys%i_electric,        &
     &    nvector_snap_rtp_2_rj, fsnap_trns%i_electric)
      call add_vec_trans_flag(ipol%i_poynting, iphys%i_poynting,        &
     &    nvector_snap_rtp_2_rj, fsnap_trns%i_poynting)
      call add_vec_trans_flag(ipol%i_mag_stretch, iphys%i_mag_stretch,  &
     &    nvector_snap_rtp_2_rj, fsnap_trns%i_mag_stretch)
      ncomp_snap_rtp_2_rj = 3*nvector_snap_rtp_2_rj
!
      nscalar_snap_rtp_2_rj = 0
      call add_scalar_trans_flag(ipol%i_me_gen, iphys%i_me_gen,         &
     &    ncomp_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_me_gen)
      call add_scalar_trans_flag(ipol%i_ujb, iphys%i_ujb,               &
     &    ncomp_snap_rtp_2_rj, nscalar_snap_rtp_2_rj, fsnap_trns%i_ujb)
      call add_scalar_trans_flag(ipol%i_nega_ujb, iphys%i_nega_ujb,     &
     &    ncomp_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_nega_ujb)
!
      call add_scalar_trans_flag(ipol%i_buo_gen, iphys%i_buo_gen,       &
     &    ncomp_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_buo_gen)
      call add_scalar_trans_flag(ipol%i_c_buo_gen, iphys%i_c_buo_gen,   &
     &    ncomp_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_c_buo_gen)
      call add_scalar_trans_flag(ipol%i_f_buo_gen, iphys%i_f_buo_gen,   &
     &    ncomp_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_f_buo_gen)
!
      call add_scalar_trans_flag(ipol%i_velo_scale, iphys%i_velo_scale, &
     &    ncomp_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_velo_scale)
      call add_scalar_trans_flag                                        &
     &   (ipol%i_magne_scale, iphys%i_magne_scale,                      &
     &    ncomp_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_magne_scale)
      call add_scalar_trans_flag(ipol%i_temp_scale, iphys%i_temp_scale, &
     &    ncomp_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_temp_scale)
      call add_scalar_trans_flag(ipol%i_comp_scale, iphys%i_comp_scale, &
     &    ncomp_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                   &
     &    fsnap_trns%i_comp_scale)
      ncomp_snap_rtp_2_rj = ncomp_snap_rtp_2_rj + nscalar_snap_rtp_2_rj
!
!
      nvector_snap_rj_2_rtp = 0
!      if(b_trns%i_velo .eq. 0) then
        call add_vec_trans_flag(ipol%i_velo, iphys%i_velo,              &
     &      nvector_snap_rj_2_rtp, bsnap_trns%i_velo)
!      end if
!      if(b_trns%i_vort .eq. 0) then
        call add_vec_trans_flag(ipol%i_vort, iphys%i_vort,              &
     &      nvector_snap_rj_2_rtp, bsnap_trns%i_vort)
!      end if
!      if(b_trns%i_magne .eq. 0) then
        call add_vec_trans_flag(ipol%i_magne, iphys%i_magne,            &
     &      nvector_snap_rj_2_rtp, bsnap_trns%i_magne)
!      end if
!      if(b_trns%i_current .eq. 0) then
        call add_vec_trans_flag(ipol%i_current, iphys%i_current,        &
     &      nvector_snap_rj_2_rtp, bsnap_trns%i_current)
!      end if
!
      call add_vec_trans_flag(ipol%i_v_diffuse, iphys%i_v_diffuse,      &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_v_diffuse)
      call add_vec_trans_flag(ipol%i_w_diffuse, iphys%i_w_diffuse,      &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_w_diffuse)
      call add_vec_trans_flag(ipol%i_vp_diffuse, iphys%i_vp_diffuse,    &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_vp_diffuse)
      call add_vec_trans_flag(ipol%i_b_diffuse, iphys%i_b_diffuse,      &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_b_diffuse)
!
      call add_vec_trans_flag(ipol%i_rot_inertia, iphys%i_rot_inertia,  &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_rot_inertia)
      call add_vec_trans_flag(ipol%i_rot_Coriolis,                      &
     &    iphys%i_rot_Coriolis, nvector_snap_rj_2_rtp,                  &
     &    bsnap_trns%i_rot_Coriolis)
      call add_vec_trans_flag(ipol%i_rot_Lorentz, iphys%i_rot_Lorentz,  &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_rot_Lorentz)
      call add_vec_trans_flag(ipol%i_rot_buoyancy,                      &
     &    iphys%i_rot_buoyancy, nvector_snap_rj_2_rtp,                  &
     &    bsnap_trns%i_rot_buoyancy)
      call add_vec_trans_flag(ipol%i_rot_comp_buo,                      &
     &    iphys%i_rot_comp_buo, nvector_snap_rj_2_rtp,                  &
     &    bsnap_trns%i_rot_comp_buo)
!
      call add_vec_trans_flag(ipol%i_press_grad, iphys%i_press_grad,    &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_press_grad)
      call add_vec_trans_flag(ipol%i_induction, iphys%i_induction,      &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_induction)
!
      call add_vec_trans_flag(ipol%i_grad_t, iphys%i_grad_t,            &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_grad_t)
      call add_vec_trans_flag(ipol%i_grad_composit,                     &
     &    iphys%i_grad_composit, nvector_snap_rj_2_rtp,                 &
     &    bsnap_trns%i_grad_composit)
      ncomp_snap_rj_2_rtp = 3*nvector_snap_rj_2_rtp
!
!
      nscalar_snap_rj_2_rtp = 0
!      if(b_trns%i_temp.eq.0 .or. ipol%i_par_temp.gt.0) then
        call add_scalar_trans_flag(ipol%i_temp, iphys%i_temp,           &
     &    ncomp_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_temp)
!      end if
!      if(b_trns%i_light .eq. 0) then
        call add_scalar_trans_flag(ipol%i_light, iphys%i_light,         &
     &    ncomp_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_light)
!      end if
!
      call add_scalar_trans_flag(ipol%i_press, iphys%i_press,           &
     &    ncomp_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_press)
      call add_scalar_trans_flag(ipol%i_par_temp, iphys%i_par_temp,     &
     &    ncomp_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_par_temp)
      call add_scalar_trans_flag                                        &
     &   (ipol%i_filter_temp, iphys%i_filter_temp,                      &
     &    ncomp_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_filter_temp)
      call add_scalar_trans_flag(ipol%i_t_diffuse, iphys%i_t_diffuse,   &
     &    ncomp_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_t_diffuse)
      call add_scalar_trans_flag(ipol%i_c_diffuse, iphys%i_c_diffuse,   &
     &    ncomp_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                   &
     &    bsnap_trns%i_c_diffuse)
!
      call add_scalar_trans_flag(ipol%i_div_Coriolis,                   &
     &    iphys%i_div_Coriolis, ncomp_snap_rj_2_rtp,                    &
     &    nscalar_snap_rj_2_rtp, bsnap_trns%i_div_Coriolis)
      ncomp_snap_rj_2_rtp = ncomp_snap_rj_2_rtp + nscalar_snap_rj_2_rtp
!
!
      ncomp_sph_trans = max(ncomp_sph_trans, ncomp_snap_rtp_2_rj)
      ncomp_sph_trans = max(ncomp_sph_trans, ncomp_snap_rj_2_rtp)
!
      nvector_sph_trans = max(nvector_sph_trans, nvector_snap_rj_2_rtp)
      nvector_sph_trans = max(nvector_sph_trans, nvector_snap_rtp_2_rj)
      nscalar_sph_trans = max(nscalar_sph_trans,                        &
     &                 (nscalar_snap_rj_2_rtp+6*ntensor_snap_rj_2_rtp))
      nscalar_sph_trans = max(nscalar_sph_trans,                        &
     &                 (nscalar_snap_rtp_2_rj+6*ntensor_snap_rtp_2_rj))
!
      end subroutine set_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!
      subroutine check_addresses_snapshot_trans
!
      use m_work_4_sph_trans
      use m_node_phys_address
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
      write(*,*) 'ncomp_snap_rj_2_rtp', ncomp_snap_rj_2_rtp
      write(*,*) 'ncomp_snap_rtp_2_rj', ncomp_snap_rtp_2_rj
!
      write(*,*) 'nvector_snap_rj_2_rtp', nvector_snap_rj_2_rtp
      if(bsnap_trns%i_velo .gt. 0) write(*,*)                           &
     &            'bsnap_trns%i_velo', bsnap_trns%i_velo,               &
     &            ipol%i_velo, iphys%i_velo
      if(bsnap_trns%i_vort .gt. 0) write(*,*)                           &
     &            'bsnap_trns%i_vort', bsnap_trns%i_vort,               &
     &            ipol%i_vort, iphys%i_vort
      if(bsnap_trns%i_magne .gt. 0) write(*,*)                          &
     &            'bsnap_trns%i_magne', bsnap_trns%i_magne,             &
     &            ipol%i_magne, iphys%i_magne
      if(bsnap_trns%i_current .gt. 0) write(*,*)                        &
     &            'bsnap_trns%i_current', bsnap_trns%i_current,         &
     &            ipol%i_current, iphys%i_current
!
      if(bsnap_trns%i_v_diffuse .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_v_diffuse', bsnap_trns%i_v_diffuse,     &
     &            ipol%i_v_diffuse, iphys%i_v_diffuse
      if(bsnap_trns%i_w_diffuse .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_w_diffuse', bsnap_trns%i_w_diffuse,     &
     &            ipol%i_w_diffuse, iphys%i_w_diffuse
      if(bsnap_trns%i_vp_diffuse .gt. 0) write(*,*)                     &
     &            'bsnap_trns%i_vp_diffuse', bsnap_trns%i_vp_diffuse,   &
     &            ipol%i_vp_diffuse, iphys%i_vp_diffuse
      if(bsnap_trns%i_b_diffuse .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_b_diffuse', bsnap_trns%i_b_diffuse,     &
     &            ipol%i_b_diffuse, iphys%i_b_diffuse
!
      if(bsnap_trns%i_rot_inertia .gt. 0) write(*,*)                    &
     &         'bsnap_trns%i_rot_inertia', bsnap_trns%i_rot_inertia,    &
     &          ipol%i_rot_inertia, iphys%i_rot_inertia
      if(bsnap_trns%i_rot_Coriolis .gt. 0) write(*,*)                   &
     &         'bsnap_trns%i_rot_Coriolis', bsnap_trns%i_rot_Coriolis,  &
     &          ipol%i_rot_Coriolis, iphys%i_rot_Coriolis
      if(bsnap_trns%i_rot_Lorentz .gt. 0) write(*,*)                    &
     &         'bsnap_trns%i_rot_Lorentz',  bsnap_trns%i_rot_Lorentz,   &
     &          ipol%i_rot_Lorentz, iphys%i_rot_Lorentz
      if(bsnap_trns%i_rot_buoyancy .gt. 0) write(*,*)                   &
     &         'bsnap_trns%i_rot_buoyancy',  bsnap_trns%i_rot_buoyancy, &
     &          ipol%i_rot_buoyancy, iphys%i_rot_buoyancy
      if(bsnap_trns%i_rot_comp_buo .gt. 0) write(*,*)                   &
     &         'bsnap_trns%i_rot_comp_buo',  bsnap_trns%i_rot_comp_buo, &
     &          ipol%i_rot_comp_buo, iphys%i_rot_comp_buo
!
      if(bsnap_trns%i_press_grad .gt. 0) write(*,*)                     &
     &            'bsnap_trns%i_press_grad', bsnap_trns%i_press_grad,   &
     &             ipol%i_press_grad, iphys%i_press_grad
      if(bsnap_trns%i_induction .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_induction', bsnap_trns%i_induction,     &
     &            ipol%i_induction, iphys%i_induction
!
      if(bsnap_trns%i_grad_t .gt. 0) write(*,*)                         &
     &            'bsnap_trns%i_grad_t',    bsnap_trns%i_grad_t,        &
     &            ipol%i_grad_t, iphys%i_grad_t
      if(bsnap_trns%i_grad_composit .gt. 0) write(*,*)                  &
     &            'bsnap_trns%i_grad_composit',                         &
     &            bsnap_trns%i_grad_composit,                           &
     &            ipol%i_grad_composit, iphys%i_grad_composit
      write(*,*)
!
      write(*,*) 'nscalar_snap_rj_2_rtp', nscalar_snap_rj_2_rtp
      if(bsnap_trns%i_temp .gt. 0) write(*,*)                           &
     &            'bsnap_trns%i_temp', bsnap_trns%i_temp,               &
     &            ipol%i_temp, iphys%i_temp
      if(bsnap_trns%i_light .gt. 0) write(*,*)                          &
     &            'bsnap_trns%i_light', bsnap_trns%i_light,             &
     &            ipol%i_light, iphys%i_light
      if(bsnap_trns%i_press .gt. 0) write(*,*)                          &
     &            'bsnap_trns%i_press', bsnap_trns%i_press,             &
     &            ipol%i_press, iphys%i_press
      if(bsnap_trns%i_par_temp .gt. 0) write(*,*)                       &
     &            'bsnap_trns%i_par_temp', bsnap_trns%i_par_temp,       &
     &            ipol%i_par_temp, iphys%i_par_temp
!
      if(bsnap_trns%i_filter_temp .gt. 0) write(*,*)                    &
     &            'bsnap_trns%i_filter_temp', bsnap_trns%i_filter_temp, &
     &            ipol%i_filter_temp, iphys%i_filter_temp
!
      if(bsnap_trns%i_t_diffuse .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_t_diffuse', bsnap_trns%i_t_diffuse,     &
     &            ipol%i_t_diffuse, iphys%i_t_diffuse
      if(bsnap_trns%i_c_diffuse .gt. 0) write(*,*)                      &
     &            'bsnap_trns%i_c_diffuse', bsnap_trns%i_c_diffuse,     &
     &            ipol%i_c_diffuse, iphys%i_c_diffuse
!
      if(bsnap_trns%i_div_Coriolis .gt. 0) write(*,*)                   &
     &          'bsnap_trns%i_div_Coriolis', bsnap_trns%i_div_Coriolis, &
     &          ipol%i_div_Coriolis, iphys%i_div_Coriolis
      write(*,*)
!
!
      write(*,*) 'nvector_snap_rtp_2_rj', nvector_snap_rtp_2_rj
      if(fsnap_trns%i_coriolis .gt. 0) write(*,*)                       &
     &            'fsnap_trns%i_coriolis',  fsnap_trns%i_coriolis,      &
     &            ipol%i_coriolis, iphys%i_coriolis
!
      if(fsnap_trns%i_electric .gt. 0) write(*,*)                       &
     &            'fsnap_trns%i_electric',  fsnap_trns%i_electric,      &
     &            ipol%i_electric, iphys%i_electric
      if(fsnap_trns%i_poynting .gt. 0) write(*,*)                       &
     &            'fsnap_trns%i_poynting',  fsnap_trns%i_poynting,      &
     &            ipol%i_poynting, iphys%i_poynting
!
      if(fsnap_trns%i_mag_stretch .gt. 0) write(*,*)                    &
     &            'fsnap_trns%i_mag_stretch',                           &
     &            fsnap_trns%i_mag_stretch,  ipol%i_mag_stretch,        &
     &            iphys%i_mag_stretch
      write(*,*)
!
      write(*,*) 'nscalar_snap_rtp_2_rj', nscalar_snap_rtp_2_rj
      if(fsnap_trns%i_me_gen .gt. 0) write(*,*)                         &
     &            'fsnap_trns%i_me_gen', fsnap_trns%i_me_gen,           &
     &            ipol%i_me_gen, iphys%i_me_gen
      if(fsnap_trns%i_ujb .gt. 0) write(*,*)                            &
     &            'fsnap_trns%i_ujb', fsnap_trns%i_ujb,                 &
     &            ipol%i_ujb, iphys%i_ujb
      if(fsnap_trns%i_nega_ujb .gt. 0) write(*,*)                       &
     &            'fsnap_trns%i_nega_ujb',  fsnap_trns%i_nega_ujb,      &
     &            ipol%i_nega_ujb, iphys%i_nega_ujb
!
      if(fsnap_trns%i_buo_gen .gt. 0) write(*,*)                        &
     &            'fsnap_trns%i_buo_gen',   fsnap_trns%i_buo_gen,       &
     &            ipol%i_buo_gen, iphys%i_buo_gen
      if(fsnap_trns%i_c_buo_gen .gt. 0) write(*,*)                      &
     &            'fsnap_trns%i_c_buo_gen', fsnap_trns%i_c_buo_gen,     &
     &            ipol%i_c_buo_gen, iphys%i_c_buo_gen
      if(fsnap_trns%i_f_buo_gen .gt. 0) write(*,*)                      &
     &            'fsnap_trns%i_f_buo_gen', fsnap_trns%i_f_buo_gen,     &
     &            ipol%i_f_buo_gen, iphys%i_f_buo_gen
!
      if(fsnap_trns%i_velo_scale .gt. 0) write(*,*)                     &
     &            'fsnap_trns%i_velo_scale', fsnap_trns%i_velo_scale,   &
     &            ipol%i_velo_scale, iphys%i_velo_scale
      if(fsnap_trns%i_magne_scale .gt. 0) write(*,*)                    &
     &            'fsnap_trns%i_magne_scale', fsnap_trns%i_magne_scale, &
     &            ipol%i_magne_scale, iphys%i_magne_scale
      if(fsnap_trns%i_temp_scale .gt. 0) write(*,*)                     &
     &            'fsnap_trns%i_temp_scale', fsnap_trns%i_temp_scale,   &
     &            ipol%i_temp_scale, iphys%i_temp_scale
      if(fsnap_trns%i_comp_scale .gt. 0) write(*,*)                     &
     &            'fsnap_trns%i_comp_scale', fsnap_trns%i_comp_scale,   &
     &            ipol%i_comp_scale, iphys%i_comp_scale
        write(*,*)
!
      end subroutine check_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_scalar_trans_flag(is_fld, irtp_fld,                &
     &          ncomp_vec, num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld, ncomp_vec
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
!
      if( (is_fld*irtp_fld) .gt. 0) then
        num_trans = num_trans + 1
        itrans = num_trans + ncomp_vec
      end if
!
      end subroutine add_scalar_trans_flag
!
!-----------------------------------------------------------------------
!
      subroutine add_vec_trans_flag(is_fld, irtp_fld,                   &
     &          num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
!
      if( (is_fld*irtp_fld) .gt. 0) then
        num_trans = num_trans + 1
        itrans = 3*num_trans - 2
      end if
!
      end subroutine add_vec_trans_flag
!
!-----------------------------------------------------------------------
!
      end module m_addresses_trans_sph_snap
