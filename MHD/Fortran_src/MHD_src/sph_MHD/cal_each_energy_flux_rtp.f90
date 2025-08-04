!>@file  cal_each_energy_flux_rtp.f90
!!      module cal_each_energy_flux_rtp
!!
!!@author  T. Kera (Tohoku University) and H. Matsui (Tokyo Tech.)
!!@date Programmed by T. Kera in Aug., 2021
!!      Modified by H. Matsui in Aug., 2025
!
!>@brief Evaluate energy fluxes of nonlinear terms (and Coriolis)
!!
!!@verbatim
!!      subroutine cal_work_of_lorentz_on_node                          &
!!     &         (bs_trns_base, f_trns_frc, fs_trns_eflux,              &
!!     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp, &
!!     &          ntot_comp_flx, flx_rtp)
!!      subroutine cal_work_of_inertia_on_node                          &
!!     &         (bs_trns_base, f_trns_frc, fs_trns_eflux,              &
!!     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp, &
!!     &          ntot_comp_flx, flx_rtp)
!!      subroutine cal_work_of_coriolis_on_node                         &
!!     &         (bs_trns_base, f_trns_frc, fs_trns_eflux,              &
!!     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp, &
!!     &          ntot_comp_flx, flx_rtp)
!!      subroutine cal_ene_flux_by_induct_on_node                       &
!!     &         (bs_trns_base, ipol_frc, fs_trns_eflux,                &
!!     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp, &
!!     &          ntot_comp_flx, flx_rtp)
!!        type(base_field_address), intent(in) :: bs_trns_base
!!        type(base_force_address), intent(in) :: f_trns_frc
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(energy_flux_address), intent(in) :: fs_trns_eflux
!!        integer(kind = kint), intent(in) :: nnod
!!        integer(kind = kint), intent(in) :: ntot_comp_fld
!!        integer(kind = kint), intent(in) :: ntot_comp_frc
!!        integer(kind = kint), intent(in) :: ntot_comp_flx
!!        real(kind = kreal), intent(inout)                             &
!!     &                     :: flx_rtp(nnod,ntot_comp_flx)
!!@endverbatim
!
      module cal_each_energy_flux_rtp
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_phys_address
      use t_spheric_rtp_data
      use t_physical_property
      use t_addresses_sph_transform
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_work_of_lorentz_on_node                            &
     &         (bs_trns_base, f_trns_frc, fs_trns_eflux,                &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp,   &
     &          ntot_comp_flx, flx_rtp)
!
      use cal_products_smp
!
      type(base_field_address), intent(in) :: bs_trns_base
      type(base_force_address), intent(in) :: f_trns_frc
      type(energy_flux_address), intent(in) :: fs_trns_eflux
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld
      integer(kind = kint), intent(in) :: ntot_comp_frc
      integer(kind = kint), intent(in) :: ntot_comp_flx
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
      real(kind = kreal), intent(in) :: frc_rtp(nnod,ntot_comp_frc)
!
      real(kind = kreal), intent(inout)                                 &
     &                     :: flx_rtp(nnod,ntot_comp_flx)
!
!
      if(fs_trns_eflux%i_ujb .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      frc_rtp(1,f_trns_frc%i_lorentz),                            &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_ujb) )
      end if
!
      if(fs_trns_eflux%i_nega_ujb .gt. 0) then
        call cal_dot_prod_w_coef_smp(nnod, dminus,                      &
     &      frc_rtp(1,f_trns_frc%i_lorentz),                            &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_nega_ujb))
      end if
!
      end subroutine cal_work_of_lorentz_on_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_work_of_inertia_on_node                            &
     &         (bs_trns_base, f_trns_frc, fs_trns_eflux,                &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp,   &
     &          ntot_comp_flx, flx_rtp)
!
      use cal_products_smp
!
      type(base_field_address), intent(in) :: bs_trns_base
      type(base_force_address), intent(in) :: f_trns_frc
      type(energy_flux_address), intent(in) :: fs_trns_eflux
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld
      integer(kind = kint), intent(in) :: ntot_comp_frc
      integer(kind = kint), intent(in) :: ntot_comp_flx
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
      real(kind = kreal), intent(in) :: frc_rtp(nnod,ntot_comp_frc)
!
      real(kind = kreal), intent(inout)                                 &
     &                     :: flx_rtp(nnod,ntot_comp_flx)
!
      if(fs_trns_eflux%i_m_advect_work .gt. 0) then
        call cal_dot_prod_w_coef_smp(nnod, dminus,                      &
     &      frc_rtp(1,f_trns_frc%i_m_advect),                           &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_m_advect_work) )
      end if
!
      if(fs_trns_eflux%i_uwu .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      frc_rtp(1,f_trns_frc%i_m_advect),                           &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_uwu) )
      end if
!
      if(fs_trns_eflux%i_Coriolis_work .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      frc_rtp(1,f_trns_frc%i_coriolis),                           &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_Coriolis_work) )
      end if
!
      if(fs_trns_eflux%i_work_against_Coriolis .gt. 0) then
        call cal_dot_prod_w_coef_smp(nnod, dminus,                      &
     &      frc_rtp(1,f_trns_frc%i_coriolis),                           &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_work_against_Coriolis) )
      end if
!
      end subroutine cal_work_of_inertia_on_node
!
!-----------------------------------------------------------------------
!
      subroutine cal_work_of_coriolis_on_node                           &
     &         (bs_trns_base, f_trns_frc, fs_trns_eflux,                &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp,   &
     &          ntot_comp_flx, flx_rtp)
!
      use cal_products_smp
!
      type(base_field_address), intent(in) :: bs_trns_base
      type(base_force_address), intent(in) :: f_trns_frc
      type(energy_flux_address), intent(in) :: fs_trns_eflux
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld
      integer(kind = kint), intent(in) :: ntot_comp_frc
      integer(kind = kint), intent(in) :: ntot_comp_flx
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
      real(kind = kreal), intent(in) :: frc_rtp(nnod,ntot_comp_frc)
!
      real(kind = kreal), intent(inout)                                 &
     &                     :: flx_rtp(nnod,ntot_comp_flx)
!
!
      if(fs_trns_eflux%i_Coriolis_work .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      frc_rtp(1,f_trns_frc%i_coriolis),                           &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_Coriolis_work) )
      end if
!
      if(fs_trns_eflux%i_work_against_Coriolis .gt. 0) then
        call cal_dot_prod_w_coef_smp(nnod, dminus,                      &
     &      frc_rtp(1,f_trns_frc%i_coriolis),                           &
     &      fld_rtp(1,bs_trns_base%i_velo),                             &
     &      flx_rtp(1,fs_trns_eflux%i_work_against_Coriolis) )
      end if
!
      end subroutine cal_work_of_coriolis_on_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_ene_flux_by_induct_on_node                         &
     &         (bs_trns_base, ipol_frc, fs_trns_eflux,                  &
     &          nnod, ntot_comp_fld, fld_rtp, ntot_comp_frc, frc_rtp,   &
     &          ntot_comp_flx, flx_rtp)
!
      use cal_products_smp
!
      type(base_field_address), intent(in) :: bs_trns_base
      type(base_force_address), intent(in) :: ipol_frc
      type(energy_flux_address), intent(in) :: fs_trns_eflux
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ntot_comp_fld
      integer(kind = kint), intent(in) :: ntot_comp_frc
      integer(kind = kint), intent(in) :: ntot_comp_flx
      real(kind = kreal), intent(in) :: fld_rtp(nnod,ntot_comp_fld)
      real(kind = kreal), intent(in) :: frc_rtp(nnod,ntot_comp_frc)
!
      real(kind = kreal), intent(inout)                                 &
     &                     :: flx_rtp(nnod,ntot_comp_flx)
!
!
      if(fs_trns_eflux%i_me_gen .gt. 0) then
        call cal_dot_prod_no_coef_smp(nnod,                             &
     &      frc_rtp(1,ipol_frc%i_induction),                            &
     &      fld_rtp(1,bs_trns_base%i_magne),                            &
     &      flx_rtp(1,fs_trns_eflux%i_me_gen) )
      end if
!
      end subroutine cal_ene_flux_by_induct_on_node
!
!-----------------------------------------------------------------------
!
      end module cal_each_energy_flux_rtp
