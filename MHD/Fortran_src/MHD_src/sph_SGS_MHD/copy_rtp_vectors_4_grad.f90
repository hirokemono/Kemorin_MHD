!>@file   copy_rtp_vectors_4_grad.f90
!!@brief  module copy_rtp_vectors_4_grad
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine copy_vectors_rtp_4_grad                              &
!!     &         (sph, b_trns, fn_trns, trns_b_MHD, trns_f_ngSGS)
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: b_trns, fn_trns
!!        type(address_each_sph_trans), intent(in) :: trns_b_MHD
!!        type(address_each_sph_trans), intent(inout) :: trns_f_ngSGS
!!      subroutine copy_filter_vecs_rtp_4_grad                          &
!!     &         (sph, bs_trns, fd_trns, trns_b_SGS, trns_f_ngDNMC)
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: bs_trns, fd_trns
!!        type(address_each_sph_trans), intent(in) :: trns_b_SGS
!!        type(address_each_sph_trans), intent(inout) :: trns_f_ngDNMC
!!
!!      subroutine copy_vect_to_grad_vect_rtp                           &
!!     &         (sph_rtp, ib_vect, if_grad_vx, if_grad_vy, if_grad_vz, &
!!     &          ncomp_rj_2_rtp, ncomp_rtp_2_rj, fld_rtp, frc_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!@endverbatim
!
      module copy_rtp_vectors_4_grad
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_spheric_rtp_data
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_vectors_rtp_4_grad                                &
     &         (sph, b_trns, fn_trns, trns_b_MHD, trns_f_ngSGS)
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: b_trns, fn_trns
!
      type(address_each_sph_trans), intent(in) :: trns_b_MHD
      type(address_each_sph_trans), intent(inout) :: trns_f_ngSGS
!
!
      if(b_trns%i_velo .gt. 0) then
        call sel_scalar_from_trans(sph%sph_rtp, b_trns%i_velo,          &
     &      fn_trns%i_grad_vx, fn_trns%i_grad_vy, fn_trns%i_grad_vz,    &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns%i_vort .gt. 0) then
        call sel_scalar_from_trans(sph%sph_rtp, b_trns%i_vort,          &
     &      fn_trns%i_grad_wx, fn_trns%i_grad_wy, fn_trns%i_grad_wz,    &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns%i_vecp .gt. 0) then
        call sel_scalar_from_trans(sph%sph_rtp, b_trns%i_vecp,          &
     &      fn_trns%i_grad_ax, fn_trns%i_grad_ay, fn_trns%i_grad_az,    &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns%i_magne .gt. 0) then
        call sel_scalar_from_trans(sph%sph_rtp, b_trns%i_magne,         &
     &      fn_trns%i_grad_bx, fn_trns%i_grad_by, fn_trns%i_grad_bz,    &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns%i_current .gt. 0) then
        call sel_scalar_from_trans(sph%sph_rtp, b_trns%i_current,       &
     &      fn_trns%i_grad_jx, fn_trns%i_grad_jy, fn_trns%i_grad_jz,    &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
!
      end subroutine copy_vectors_rtp_4_grad
!
! -----------------------------------------------------------------------
!
      subroutine copy_filter_vecs_rtp_4_grad                            &
     &         (sph, bs_trns, fd_trns, trns_b_SGS, trns_f_ngDNMC)
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: bs_trns, fd_trns
!
      type(address_each_sph_trans), intent(in) :: trns_b_SGS
      type(address_each_sph_trans), intent(inout) :: trns_f_ngDNMC
!
!
      if(bs_trns%i_filter_velo .gt. 0) then
        call sel_scalar_from_trans(sph%sph_rtp,                         &
     &      bs_trns%i_filter_velo, fd_trns%i_grad_filter_vx,            &
     &      fd_trns%i_grad_filter_vy, fd_trns%i_grad_filter_vz,         &
     &      trns_b_SGS%ncomp, trns_f_ngDNMC%ncomp,                      &
     &      trns_b_SGS%fld_rtp, trns_f_ngDNMC%fld_rtp)
      end if
      if(bs_trns%i_filter_vort .gt. 0) then
        call sel_scalar_from_trans(sph%sph_rtp,                         &
     &      bs_trns%i_filter_vort, fd_trns%i_grad_filter_wx,            &
     &      fd_trns%i_grad_filter_wy, fd_trns%i_grad_filter_wz,         &
     &      trns_b_SGS%ncomp, trns_f_ngDNMC%ncomp,                      &
     &      trns_b_SGS%fld_rtp, trns_f_ngDNMC%fld_rtp)
      end if
      if(bs_trns%i_filter_vecp .gt. 0) then
        call sel_scalar_from_trans(sph%sph_rtp,                         &
     &      bs_trns%i_filter_vecp, fd_trns%i_grad_filter_ax,            &
     &      fd_trns%i_grad_filter_ay, fd_trns%i_grad_filter_az,         &
     &      trns_b_SGS%ncomp, trns_f_ngDNMC%ncomp,                      &
     &      trns_b_SGS%fld_rtp, trns_f_ngDNMC%fld_rtp)
      end if
      if(bs_trns%i_filter_magne .gt. 0) then
        call sel_scalar_from_trans(sph%sph_rtp,                         &
     &      bs_trns%i_filter_magne, fd_trns%i_grad_filter_bx,           &
     &      fd_trns%i_grad_filter_by, fd_trns%i_grad_filter_bz,         &
     &      trns_b_SGS%ncomp, trns_f_ngDNMC%ncomp,                      &
     &      trns_b_SGS%fld_rtp, trns_f_ngDNMC%fld_rtp)
      end if
      if(bs_trns%i_filter_current .gt. 0) then
        call sel_scalar_from_trans(sph%sph_rtp,                         &
     &      bs_trns%i_filter_current, fd_trns%i_grad_filter_jx,         &
     &      fd_trns%i_grad_filter_jy, fd_trns%i_grad_filter_jz,         &
     &      trns_b_SGS%ncomp, trns_f_ngDNMC%ncomp,                      &
     &      trns_b_SGS%fld_rtp, trns_f_ngDNMC%fld_rtp)
      end if
!
      end subroutine copy_filter_vecs_rtp_4_grad
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_vect_to_grad_vect_rtp                             &
     &         (sph_rtp, ib_vect, if_grad_vx, if_grad_vy, if_grad_vz,   &
     &          ncomp_rj_2_rtp, ncomp_rtp_2_rj, fld_rtp, frc_rtp)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: ib_vect
      integer(kind = kint), intent(in) :: if_grad_vx, if_grad_vy
      integer(kind = kint), intent(in) :: if_grad_vz
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_rtp_2_rj
!
      real(kind = kreal), intent(in)                                    &
     &           :: fld_rtp(sph_rtp%nnod_rtp,ncomp_rj_2_rtp)
      real(kind = kreal), intent(inout)                                 &
     &           :: frc_rtp(sph_rtp%nnod_rtp,ncomp_rtp_2_rj)
!
!
      if(if_grad_vx .gt. 0) call sel_scalar_from_trans                  &
     &     (sph_rtp, fld_rtp(1,ib_vect  ), frc_rtp(1,if_grad_vx) )
      if(if_grad_vy .gt. 0) call sel_scalar_from_trans                  &
     &      (sph_rtp, fld_rtp(1,ib_vect+1), frc_rtp(1,if_grad_vy) )
      if(if_grad_vz .gt. 0) call sel_scalar_from_trans                  &
     &      (sph_rtp, fld_rtp(1,ib_vect+2), frc_rtp(1,if_grad_vz) )
!
      end subroutine copy_vect_to_grad_vect_rtp
!
! -----------------------------------------------------------------------
!
      end module copy_rtp_vectors_4_grad
