!>@file   cal_filtered_sph_fields.f90
!!@brief  module cal_filtered_sph_fields
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine cal_filtered_sph_rj_fields                           &
!!     &         (sph_rj, ipol, sph_filters, rj_fld)
!!@endverbatim
!!
!
      module cal_filtered_sph_fields
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_sph_filtering_data
      use t_sph_filtering
!
      implicit none
!
      private :: cal_sph_base_filtering_fields
      private :: cal_sph_wide_filtering_fields
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_sph_rj_fields                             &
     &         (sph_rj, ipol, sph_filters, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_filters_type), intent(in) :: sph_filters(1)
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_base_filtering_fields                                &
     &   (sph_rj, ipol, sph_filters(1), rj_fld)
      call cal_sph_wide_filtering_fields                                &
     &   (sph_rj, ipol, sph_filters(1), rj_fld)
!
      end subroutine cal_filtered_sph_rj_fields
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_sph_rj_forces                             &
     &         (sph_rj, ipol, sph_filters, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_filters_type), intent(in) :: sph_filters(1)
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_wide_filtering_forces                                &
     &   (sph_rj, ipol, sph_filters(1), rj_fld)
      call cal_sph_base_filtering_forces                                &
     &   (sph_rj, ipol, sph_filters(1), rj_fld)
!
      end subroutine cal_filtered_sph_rj_forces
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_sph_base_filtering_fields                          &
     &         (sph_rj, ipol, sph_filters, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_filters_type), intent(in) :: sph_filters
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter(ipol%i_velo, ipol%i_filter_velo,           &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_vort, ipol%i_filter_vort,           &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_magne, ipol%i_filter_magne,         &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_current, ipol%i_filter_current,     &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
!
      call scalar_sph_filter(ipol%i_temp, ipol%i_filter_temp,           &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
      call scalar_sph_filter(ipol%i_light, ipol%i_filter_comp,          &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
      end subroutine cal_sph_base_filtering_fields
!
! ----------------------------------------------------------------------
!
      subroutine cal_sph_wide_filtering_fields                          &
     &         (sph_rj, ipol, sph_wide_f, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_filters_type), intent(in) :: sph_wide_f
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter(ipol%i_filter_velo, ipol%i_wide_fil_velo,  &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_filter_vort, ipol%i_wide_fil_vort,  &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol%i_filter_magne, ipol%i_wide_fil_magne,                   &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol%i_filter_current, ipol%i_wide_fil_current,               &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
!
      call scalar_sph_filter(ipol%i_filter_temp, ipol%i_wide_fil_temp,  &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call scalar_sph_filter(ipol%i_filter_comp, ipol%i_wide_fil_comp,  &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      end subroutine cal_sph_wide_filtering_fields
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_sph_base_filtering_forces                          &
     &         (sph_rj, ipol, sph_filters, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_filters_type), intent(in) :: sph_filters
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter(ipol%i_m_advect, ipol%i_SGS_inertia,       &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_lorentz, ipol%i_SGS_Lorentz,        &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_vp_induct, ipol%i_SGS_vp_induct,    &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_h_flux, ipol%i_SGS_h_flux,          &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_c_flux, ipol%i_SGS_c_flux,          &
     &    sph_rj, sph_filters%r_filter, sph_filters%sph_filter, rj_fld)
!
      end subroutine cal_sph_base_filtering_forces
!
! ----------------------------------------------------------------------
!
      subroutine cal_sph_wide_filtering_forces                          &
     &         (sph_rj, ipol, sph_wide_f, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_filters_type), intent(in) :: sph_wide_f
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter                                            &
     &   (ipol%i_SGS_inertia, ipol%i_wide_SGS_inertia,                  &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol%i_SGS_Lorentz, ipol%i_wide_SGS_Lorentz,                  &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol%i_SGS_vp_induct, ipol%i_wide_SGS_vp_induct,              &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_SGS_h_flux, ipol%i_wide_SGS_h_flux, &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_SGS_c_flux, ipol%i_wide_SGS_c_flux, &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      end subroutine cal_sph_wide_filtering_forces
!
! ----------------------------------------------------------------------
!
      end module cal_filtered_sph_fields
