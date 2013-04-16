!>@file   m_addresses_trans_sph_snap.f90
!!@brief  module m_addresses_trans_sph_snap
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical hermonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
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
!>      number of vectors for backward spherical hermonics transform
      integer(kind = kint) :: nvector_snap_rj_2_rtp = 0
!>      number of scalars for backward spherical hermonics transform
      integer(kind = kint) :: nscalar_snap_rj_2_rtp = 0
!>      number of tensors for backward spherical hermonics transform
      integer(kind = kint) :: ntensor_snap_rj_2_rtp = 0
!
!>      number of vectors for forward spherical hermonics transform
      integer(kind = kint) :: nvector_snap_rtp_2_rj = 0
!>      number of scalars for forward spherical hermonics transform
      integer(kind = kint) :: nscalar_snap_rtp_2_rj = 0
!>      number of tensors for forward spherical hermonics transform
      integer(kind = kint) :: ntensor_snap_rtp_2_rj = 0
!
!>    addresses for fields to backward transform
      type(phys_address), save :: bsnap_trns
!
!>    addresses for forces to forward transform
      type(phys_address), save :: fsnap_trns
!
      private :: add_transform_flag
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_snapshot_trans
!
      use m_work_4_sph_trans
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
      nscalar_snap_rtp_2_rj = 0
      call add_transform_flag(ipol%i_me_gen, irtp%i_me_gen,             &
     &    nscalar_snap_rtp_2_rj, fsnap_trns%i_me_gen)
      call add_transform_flag(ipol%i_ujb, irtp%i_ujb,                   &
     &    nscalar_snap_rtp_2_rj, fsnap_trns%i_ujb)
      call add_transform_flag(ipol%i_nega_ujb, irtp%i_nega_ujb,         &
     &    nscalar_snap_rtp_2_rj, fsnap_trns%i_nega_ujb)
      call add_transform_flag(ipol%i_buo_gen, irtp%i_buo_gen,           &
     &    nscalar_snap_rtp_2_rj, fsnap_trns%i_buo_gen)
      call add_transform_flag(ipol%i_c_buo_gen, irtp%i_c_buo_gen,       &
     &    nscalar_snap_rtp_2_rj, fsnap_trns%i_c_buo_gen)
      call add_transform_flag(ipol%i_f_buo_gen, irtp%i_f_buo_gen,       &
     &    nscalar_snap_rtp_2_rj, fsnap_trns%i_f_buo_gen)
!
!
!
      nscalar_snap_rj_2_rtp = 0
      if(b_trns%i_temp.eq.0 .or. ipol%i_par_temp.gt.0) then
        call add_transform_flag(ipol%i_temp, irtp%i_temp,               &
     &    nscalar_snap_rj_2_rtp, bsnap_trns%i_temp)
      end if
      if(b_trns%i_light .eq. 0) then
        call add_transform_flag(ipol%i_light, irtp%i_light,             &
     &    nscalar_snap_rj_2_rtp, bsnap_trns%i_light)
      end if
!
      call add_transform_flag(ipol%i_press, irtp%i_press,               &
     &    nscalar_snap_rj_2_rtp, bsnap_trns%i_press)
      call add_transform_flag(ipol%i_par_temp, irtp%i_par_temp,         &
     &    nscalar_snap_rj_2_rtp, bsnap_trns%i_par_temp)
      call add_transform_flag(ipol%i_t_diffuse, irtp%i_t_diffuse,       &
     &    nscalar_snap_rj_2_rtp, bsnap_trns%i_t_diffuse)
      call add_transform_flag(ipol%i_c_diffuse, irtp%i_c_diffuse,       &
     &    nscalar_snap_rj_2_rtp, bsnap_trns%i_c_diffuse)
!
!
!
      nvector_snap_rj_2_rtp = 0
      if(b_trns%i_velo .eq. 0) then
        call add_transform_flag(ipol%i_velo, irtp%i_velo,               &
     &      nvector_snap_rj_2_rtp, bsnap_trns%i_velo)
      end if
      if(b_trns%i_vort .eq. 0) then
        call add_transform_flag(ipol%i_vort, irtp%i_vort,               &
     &      nvector_snap_rj_2_rtp, bsnap_trns%i_vort)
      end if
      if(b_trns%i_magne .eq. 0) then
        call add_transform_flag(ipol%i_magne, irtp%i_magne,             &
     &      nvector_snap_rj_2_rtp, bsnap_trns%i_magne)
      end if
      if(b_trns%i_current .eq. 0) then
        call add_transform_flag(ipol%i_current, irtp%i_current,         &
     &      nvector_snap_rj_2_rtp, bsnap_trns%i_current)
      end if
!
      call add_transform_flag(ipol%i_v_diffuse, irtp%i_v_diffuse,       &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_v_diffuse)
      call add_transform_flag(ipol%i_w_diffuse, irtp%i_w_diffuse,       &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_w_diffuse)
      call add_transform_flag(ipol%i_vp_diffuse, irtp%i_vp_diffuse,     &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_vp_diffuse)
      call add_transform_flag(ipol%i_b_diffuse, irtp%i_b_diffuse,       &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_b_diffuse)
      call add_transform_flag(ipol%i_grad_t, irtp%i_grad_t,             &
     &    nvector_snap_rj_2_rtp, bsnap_trns%i_grad_t)
      call add_transform_flag(ipol%i_grad_composit,                     &
     &    irtp%i_grad_composit, nvector_snap_rj_2_rtp,                  &
     &    bsnap_trns%i_grad_composit)
!
!
      nb_sph_trans = max(nb_sph_trans,nscalar_snap_rtp_2_rj)
      nb_sph_trans = max(nb_sph_trans,nscalar_snap_rj_2_rtp)
      nb_sph_trans = max(nb_sph_trans,nvector_snap_rj_2_rtp)
!
      end subroutine set_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!
      subroutine check_addresses_snapshot_trans
!
      use m_work_4_sph_trans
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
!
!
        write(*,*) 'bsnap_trns%i_velo', bsnap_trns%i_velo,              &
     &            ipol%i_velo, irtp%i_velo
        write(*,*) 'bsnap_trns%i_vort', bsnap_trns%i_vort,              &
     &            ipol%i_vort, irtp%i_vort
        write(*,*) 'bsnap_trns%i_magne', bsnap_trns%i_magne,            &
     &            ipol%i_magne, irtp%i_magne
        write(*,*) 'bsnap_trns%i_current', bsnap_trns%i_current,        &
     &            ipol%i_current, irtp%i_current
!
        write(*,*) 'bsnap_trns%i_v_diffuse', bsnap_trns%i_v_diffuse,    &
     &            ipol%i_v_diffuse, irtp%i_v_diffuse
        write(*,*) 'bsnap_trns%i_w_diffuse', bsnap_trns%i_w_diffuse,    &
     &            ipol%i_w_diffuse, irtp%i_w_diffuse
        write(*,*) 'bsnap_trns%i_vp_diffuse', bsnap_trns%i_vp_diffuse,  &
     &            ipol%i_vp_diffuse, irtp%i_vp_diffuse
        write(*,*) 'bsnap_trns%i_b_diffuse', bsnap_trns%i_b_diffuse,    &
     &            ipol%i_b_diffuse, irtp%i_b_diffuse
        write(*,*) 'bsnap_trns%i_t_diffuse', bsnap_trns%i_t_diffuse,    &
     &            ipol%i_t_diffuse, irtp%i_t_diffuse
        write(*,*) 'bsnap_trns%i_c_diffuse', bsnap_trns%i_c_diffuse,    &
     &            ipol%i_c_diffuse, irtp%i_c_diffuse
!
        write(*,*) 'bsnap_trns%i_grad_t',    bsnap_trns%i_grad_t,       &
     &            ipol%i_grad_t, irtp%i_grad_t
        write(*,*) 'bsnap_trns%i_grad_composit',                        &
     &            bsnap_trns%i_grad_composit,                           &
     &            ipol%i_grad_composit, irtp%i_grad_composit
!
        write(*,*) 'bsnap_trns%i_temp', bsnap_trns%i_temp,              &
     &            ipol%i_temp, irtp%i_temp
        write(*,*) 'bsnap_trns%i_light', bsnap_trns%i_light,            &
     &            ipol%i_light, irtp%i_light
        write(*,*) 'bsnap_trns%i_press', bsnap_trns%i_press,            &
     &            ipol%i_press, irtp%i_press
        write(*,*) 'bsnap_trns%i_par_temp', bsnap_trns%i_par_temp,      &
     &            ipol%i_par_temp, irtp%i_par_temp
!
        write(*,*) 'fsnap_trns%i_me_gen', fsnap_trns%i_me_gen,          &
     &            ipol%i_me_gen, irtp%i_me_gen
        write(*,*) 'fsnap_trns%i_ujb', fsnap_trns%i_ujb,                &
     &            ipol%i_ujb, irtp%i_ujb
        write(*,*) 'fsnap_trns%i_nega_ujb',  fsnap_trns%i_nega_ujb,     &
     &            ipol%i_nega_ujb, irtp%i_nega_ujb
        write(*,*) 'fsnap_trns%i_buo_gen',   fsnap_trns%i_buo_gen,      &
     &            ipol%i_buo_gen, irtp%i_buo_gen
        write(*,*) 'fsnap_trns%i_c_buo_gen', fsnap_trns%i_c_buo_gen,    &
     &            ipol%i_c_buo_gen, irtp%i_c_buo_gen
        write(*,*) 'fsnap_trns%i_f_buo_gen', fsnap_trns%i_f_buo_gen,    &
     &            ipol%i_f_buo_gen, irtp%i_f_buo_gen
!
      end subroutine check_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_transform_flag(is_fld, irtp_fld,                   &
     &          num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
!
      if( (is_fld*irtp_fld) .gt. 0) then
        num_trans = num_trans + 1
        itrans = num_trans
      end if
!
      end subroutine add_transform_flag
!
!-----------------------------------------------------------------------
!
      end module m_addresses_trans_sph_snap
