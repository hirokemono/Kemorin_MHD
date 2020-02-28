!>@file   copy_rtp_vectors_4_grad.f90
!!@brief  module copy_rtp_vectors_4_grad
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine copy_filter_vecs_rtp_4_grad                          &
!!     &         (sph, bs_trns, fd_trns, trns_b_SGS, trns_DYNG)
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: bs_trns, fd_trns
!!        type(address_each_sph_trans), intent(in) :: trns_b_SGS
!!        type(address_each_sph_trans), intent(inout) :: trns_DYNG
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
      subroutine copy_filter_vecs_rtp_4_grad                            &
     &         (sph, bs_trns, fd_trns, trns_b_SGS, trns_DYNG)
!
      use sph_poynting_flux_smp
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: bs_trns, fd_trns
!
      type(address_each_sph_trans), intent(in) :: trns_b_SGS
      type(address_each_sph_trans), intent(inout) :: trns_DYNG
!
!
      if(bs_trns%i_filter_velo .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp,                    &
     &      bs_trns%i_filter_velo, fd_trns%diff_fil_vect%i_grad_vx,     &
     &      fd_trns%diff_fil_vect%i_grad_vy,                            &
     &      fd_trns%diff_fil_vect%i_grad_vz,                            &
     &      trns_b_SGS%ncomp, trns_DYNG%ncomp,                          &
     &      trns_b_SGS%fld_rtp, trns_DYNG%fld_rtp)
      end if
      if(bs_trns%i_filter_vort .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp,                    &
     &      bs_trns%i_filter_vort, fd_trns%diff_fil_vect%i_grad_wx,     &
     &      fd_trns%diff_fil_vect%i_grad_wy,                            &
     &      fd_trns%diff_fil_vect%i_grad_wz,                            &
     &      trns_b_SGS%ncomp, trns_DYNG%ncomp,                          &
     &      trns_b_SGS%fld_rtp, trns_DYNG%fld_rtp)
      end if
      if(bs_trns%i_filter_vecp .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp,                    &
     &      bs_trns%i_filter_vecp, fd_trns%diff_fil_vect%i_grad_ax,     &
     &      fd_trns%diff_fil_vect%i_grad_ay,                            &
     &      fd_trns%diff_fil_vect%i_grad_az,                            &
     &      trns_b_SGS%ncomp, trns_DYNG%ncomp,                          &
     &      trns_b_SGS%fld_rtp, trns_DYNG%fld_rtp)
      end if
      if(bs_trns%i_filter_magne .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp,                    &
     &      bs_trns%i_filter_magne, fd_trns%diff_fil_vect%i_grad_bx,    &
     &      fd_trns%diff_fil_vect%i_grad_by,                            &
     &      fd_trns%diff_fil_vect%i_grad_bz,                            &
     &      trns_b_SGS%ncomp, trns_DYNG%ncomp,                          &
     &      trns_b_SGS%fld_rtp, trns_DYNG%fld_rtp)
      end if
      if(bs_trns%i_filter_current .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp,                    &
     &      bs_trns%i_filter_current, fd_trns%diff_fil_vect%i_grad_jx,  &
     &      fd_trns%diff_fil_vect%i_grad_jy,                            &
     &      fd_trns%diff_fil_vect%i_grad_jz,                            &
     &      trns_b_SGS%ncomp, trns_DYNG%ncomp,                          &
     &      trns_b_SGS%fld_rtp, trns_DYNG%fld_rtp)
      end if
!
      end subroutine copy_filter_vecs_rtp_4_grad
!
! -----------------------------------------------------------------------
!
      end module copy_rtp_vectors_4_grad
