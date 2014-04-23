!>@file   m_addresses_trans_sph_tmp.f90
!!@brief  module m_addresses_trans_sph_tmp
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_temporal_trans
!!      subroutine check_addresses_temporal_trans
!!@endverbatim
!
      module m_addresses_trans_sph_tmp
!
      use m_precision
!
      use t_phys_address
!
      implicit none
!
!>      number of components
!!      for backward vector spherical harmonics transform
      integer(kind = kint) :: nvector_tmp_rj_2_rtp = 0
!>      number of scalars for backward spherical harmonics transform
      integer(kind = kint) :: nscalar_tmp_rj_2_rtp = 0
!>      number of tensors for backward spherical harmonics transform
      integer(kind = kint) :: ntensor_tmp_rj_2_rtp = 0
!
!>      number of components
!!      for forward vector spherical harmonics transform
      integer(kind = kint) :: nvector_tmp_rtp_2_rj = 0
!>      number of scalars for forward spherical harmonics transform
      integer(kind = kint) :: nscalar_tmp_rtp_2_rj = 0
!>      number of tensors for forward spherical harmonics transform
      integer(kind = kint) :: ntensor_tmp_rtp_2_rj = 0
!
!>    addresses for fields to backward transform
      type(phys_address), save :: btmp_trns
!
!>    addresses for forces to forward transform
      type(phys_address), save :: ftmp_trns
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_temporal_trans
!
      use m_work_4_sph_trans
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
!
      integer(kind = kint) :: ncomp_fwd, ncomp_bwd
!
!
      nscalar_tmp_rtp_2_rj = 0
      call add_transform_flag(ipol%i_grad_vx, irtp%i_grad_vx,           &
     &    nscalar_tmp_rtp_2_rj, ftmp_trns%i_grad_vx)
      call add_transform_flag(ipol%i_grad_vy, irtp%i_grad_vy,           &
     &    nscalar_tmp_rtp_2_rj, ftmp_trns%i_grad_vy)
      call add_transform_flag(ipol%i_grad_vz, irtp%i_grad_vz,           &
     &    nscalar_tmp_rtp_2_rj, ftmp_trns%i_grad_vz)
!
!
      nvector_tmp_rtp_2_rj = 0
!
!
      nscalar_tmp_rj_2_rtp = 0
!
!
      nvector_tmp_rj_2_rtp = 0
      call add_vec_trans_flag(ipol%i_grad_vx, irtp%i_grad_vx,           &
     &    nvector_tmp_rj_2_rtp, btmp_trns%i_grad_vx)
      call add_vec_trans_flag(ipol%i_grad_vy, irtp%i_grad_vy,           &
     &    nvector_tmp_rj_2_rtp, btmp_trns%i_grad_vy)
      call add_vec_trans_flag(ipol%i_grad_vz, irtp%i_grad_vz,           &
     &    nvector_tmp_rj_2_rtp, btmp_trns%i_grad_vz)
!
!
      nb_sph_trans = max(nb_sph_trans,nscalar_tmp_rtp_2_rj)
      nb_sph_trans = max(nb_sph_trans,nvector_tmp_rtp_2_rj)
      nb_sph_trans = max(nb_sph_trans,nscalar_tmp_rj_2_rtp)
      nb_sph_trans = max(nb_sph_trans,nvector_tmp_rj_2_rtp)
!
      ncomp_bwd = 3*nvector_tmp_rj_2_rtp + nscalar_tmp_rj_2_rtp
      ncomp_fwd = 3*nvector_tmp_rtp_2_rj + nscalar_tmp_rtp_2_rj
      ncomp_sph_trans = max(ncomp_bwd,ncomp_fwd)
!
      end subroutine set_addresses_temporal_trans
!
!-----------------------------------------------------------------------
!
      subroutine check_addresses_temporal_trans
!
      use m_work_4_sph_trans
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
      write(*,*) 'nvector_tmp_rj_2_rtp', nvector_tmp_rj_2_rtp
      if(btmp_trns%i_velo .gt. 0) write(*,*)                           &
     &            'btmp_trns%i_velo', btmp_trns%i_velo,               &
     &            ipol%i_velo, irtp%i_velo
      if(btmp_trns%i_vort .gt. 0) write(*,*)                           &
     &            'btmp_trns%i_vort', btmp_trns%i_vort,               &
     &            ipol%i_vort, irtp%i_vort
      if(btmp_trns%i_magne .gt. 0) write(*,*)                          &
     &            'btmp_trns%i_magne', btmp_trns%i_magne,             &
     &            ipol%i_magne, irtp%i_magne
      if(btmp_trns%i_current .gt. 0) write(*,*)                        &
     &            'btmp_trns%i_current', btmp_trns%i_current,         &
     &            ipol%i_current, irtp%i_current
!
      if(btmp_trns%i_v_diffuse .gt. 0) write(*,*)                      &
     &            'btmp_trns%i_v_diffuse', btmp_trns%i_v_diffuse,     &
     &            ipol%i_v_diffuse, irtp%i_v_diffuse
      if(btmp_trns%i_w_diffuse .gt. 0) write(*,*)                      &
     &            'btmp_trns%i_w_diffuse', btmp_trns%i_w_diffuse,     &
     &            ipol%i_w_diffuse, irtp%i_w_diffuse
      if(btmp_trns%i_vp_diffuse .gt. 0) write(*,*)                     &
     &            'btmp_trns%i_vp_diffuse', btmp_trns%i_vp_diffuse,   &
     &            ipol%i_vp_diffuse, irtp%i_vp_diffuse
      if(btmp_trns%i_b_diffuse .gt. 0) write(*,*)                      &
     &            'btmp_trns%i_b_diffuse', btmp_trns%i_b_diffuse,     &
     &            ipol%i_b_diffuse, irtp%i_b_diffuse
!
      if(btmp_trns%i_rot_inertia .gt. 0) write(*,*)                    &
     &         'btmp_trns%i_rot_inertia', btmp_trns%i_rot_inertia,    &
     &          ipol%i_rot_inertia, irtp%i_rot_inertia
      if(btmp_trns%i_rot_Coriolis .gt. 0) write(*,*)                   &
     &         'btmp_trns%i_rot_Coriolis', btmp_trns%i_rot_Coriolis,  &
     &          ipol%i_rot_Coriolis, irtp%i_rot_Coriolis
      if(btmp_trns%i_rot_Lorentz .gt. 0) write(*,*)                    &
     &         'btmp_trns%i_rot_Lorentz',  btmp_trns%i_rot_Lorentz,   &
     &          ipol%i_rot_Lorentz, irtp%i_rot_Lorentz
      if(btmp_trns%i_rot_buoyancy .gt. 0) write(*,*)                   &
     &         'btmp_trns%i_rot_buoyancy',  btmp_trns%i_rot_buoyancy, &
     &          ipol%i_rot_buoyancy, irtp%i_rot_buoyancy
      if(btmp_trns%i_rot_comp_buo .gt. 0) write(*,*)                   &
     &         'btmp_trns%i_rot_comp_buo',  btmp_trns%i_rot_comp_buo, &
     &          ipol%i_rot_comp_buo, irtp%i_rot_comp_buo
!
      if(btmp_trns%i_press_grad .gt. 0) write(*,*)                     &
     &            'btmp_trns%i_press_grad', btmp_trns%i_press_grad,   &
     &             ipol%i_press_grad, irtp%i_press_grad
      if(btmp_trns%i_induction .gt. 0) write(*,*)                      &
     &            'btmp_trns%i_induction', btmp_trns%i_induction,     &
     &            ipol%i_induction, irtp%i_induction
!
      if(btmp_trns%i_grad_t .gt. 0) write(*,*)                         &
     &            'btmp_trns%i_grad_t',    btmp_trns%i_grad_t,        &
     &            ipol%i_grad_t, irtp%i_grad_t
      if(btmp_trns%i_grad_composit .gt. 0) write(*,*)                  &
     &            'btmp_trns%i_grad_composit',                         &
     &            btmp_trns%i_grad_composit,                           &
     &            ipol%i_grad_composit, irtp%i_grad_composit
      write(*,*)
!
      write(*,*) 'nscalar_tmp_rj_2_rtp', nscalar_tmp_rj_2_rtp
      if(btmp_trns%i_temp .gt. 0) write(*,*)                           &
     &            'btmp_trns%i_temp', btmp_trns%i_temp,               &
     &            ipol%i_temp, irtp%i_temp
      if(btmp_trns%i_light .gt. 0) write(*,*)                          &
     &            'btmp_trns%i_light', btmp_trns%i_light,             &
     &            ipol%i_light, irtp%i_light
      if(btmp_trns%i_press .gt. 0) write(*,*)                          &
     &            'btmp_trns%i_press', btmp_trns%i_press,             &
     &            ipol%i_press, irtp%i_press
      if(btmp_trns%i_par_temp .gt. 0) write(*,*)                       &
     &            'btmp_trns%i_par_temp', btmp_trns%i_par_temp,       &
     &            ipol%i_par_temp, irtp%i_par_temp
!
      if(btmp_trns%i_t_diffuse .gt. 0) write(*,*)                      &
     &            'btmp_trns%i_t_diffuse', btmp_trns%i_t_diffuse,     &
     &            ipol%i_t_diffuse, irtp%i_t_diffuse
      if(btmp_trns%i_c_diffuse .gt. 0) write(*,*)                      &
     &            'btmp_trns%i_c_diffuse', btmp_trns%i_c_diffuse,     &
     &            ipol%i_c_diffuse, irtp%i_c_diffuse
      write(*,*)
!
!
      write(*,*) 'nvector_tmp_rtp_2_rj', nvector_tmp_rtp_2_rj
      if(ftmp_trns%i_coriolis .gt. 0) write(*,*)                       &
     &            'ftmp_trns%i_coriolis',  ftmp_trns%i_coriolis,      &
     &            ipol%i_coriolis, irtp%i_coriolis
!
      if(ftmp_trns%i_electric .gt. 0) write(*,*)                       &
     &            'ftmp_trns%i_electric',  ftmp_trns%i_electric,      &
     &            ipol%i_electric, irtp%i_electric
      if(ftmp_trns%i_poynting .gt. 0) write(*,*)                       &
     &            'ftmp_trns%i_poynting',  ftmp_trns%i_poynting,      &
     &            ipol%i_poynting, irtp%i_poynting
!
      if(ftmp_trns%i_mag_stretch .gt. 0) write(*,*)                    &
     &            'ftmp_trns%i_mag_stretch',                           &
     &            ftmp_trns%i_mag_stretch,  ipol%i_mag_stretch,        &
     &            irtp%i_mag_stretch
      write(*,*)
!
      write(*,*) 'nscalar_tmp_rtp_2_rj', nscalar_tmp_rtp_2_rj
      if(ftmp_trns%i_me_gen .gt. 0) write(*,*)                         &
     &            'ftmp_trns%i_me_gen', ftmp_trns%i_me_gen,           &
     &            ipol%i_me_gen, irtp%i_me_gen
      if(ftmp_trns%i_ujb .gt. 0) write(*,*)                            &
     &            'ftmp_trns%i_ujb', ftmp_trns%i_ujb,                 &
     &            ipol%i_ujb, irtp%i_ujb
      if(ftmp_trns%i_nega_ujb .gt. 0) write(*,*)                       &
     &            'ftmp_trns%i_nega_ujb',  ftmp_trns%i_nega_ujb,      &
     &            ipol%i_nega_ujb, irtp%i_nega_ujb
!
      if(ftmp_trns%i_buo_gen .gt. 0) write(*,*)                        &
     &            'ftmp_trns%i_buo_gen',   ftmp_trns%i_buo_gen,       &
     &            ipol%i_buo_gen, irtp%i_buo_gen
      if(ftmp_trns%i_c_buo_gen .gt. 0) write(*,*)                      &
     &            'ftmp_trns%i_c_buo_gen', ftmp_trns%i_c_buo_gen,     &
     &            ipol%i_c_buo_gen, irtp%i_c_buo_gen
      if(ftmp_trns%i_f_buo_gen .gt. 0) write(*,*)                      &
     &            'ftmp_trns%i_f_buo_gen', ftmp_trns%i_f_buo_gen,     &
     &            ipol%i_f_buo_gen, irtp%i_f_buo_gen
        write(*,*)
!
      end subroutine check_addresses_temporal_trans
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
        num_trans = num_trans + 3
        itrans = num_trans - 2
      end if
!
      end subroutine add_vec_trans_flag
!
!-----------------------------------------------------------------------
!
      end module m_addresses_trans_sph_tmp
