!>@file   cal_filtered_sph_fields.f90
!!@brief  module cal_filtered_sph_fields
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine cal_sph_base_filtering_fields                        &
!!     &         (sph_rj, ipol_base, ipol_fil, sph_base_f, rj_fld)
!!       Input:   rj_fld(1:is_fld)
!!          is_fld = i_velo, base%i_vort, base%i_magne, base%i_current,
!!                                   base%i_temp, base%i_light
!!       Output:  rj_fld(1:is_fld)
!!           filter_fld%i_current, filter_fld%i_temp, filter_fld%i_light
!!          is_fld = filter_fld%i_velo, filter_fld%i_vort, 
!!                   filter_fld%i_magne, 
!!      subroutine cal_sph_wide_filtering_fields                        &
!!     &         (sph_rj, ipol_base, ipol_wfl, sph_wide_f, rj_fld)
!!       Input:   rj_fld(1:is_fld)
!!          is_fld = i_velo, base%i_vort, base%i_magne, base%i_current,
!!                                   base%i_temp, base%i_light
!!       Output:  rj_fld(1:is_fld)
!!           wide_filter_fld%i_velo, wide_filter_fld%i_vort,
!!           wide_filter_fld%base%i_magne, wide_filter_fld%i_current, 
!!           wide_filter_fld%i_temp, wide_filter_fld%i_light
!!
!!      subroutine cal_sph_base_filtering_forces                        &
!!     &         (sph_rj, ipol_frc, ipol_SGS, sph_base_f, rj_fld)
!!       Input:   rj_fld(1:is_fld)
!!          is_fld = forces%i_m_advect, forces%i_lorentz, 
!!                   forces%i_vp_induct, forces%i_h_flux,
!!                   forces%i_c_flux
!!       Output:  rj_fld(1:is_fld)
!!          is_fld = SGS_term%i_SGS_inertia, SGS_term%i_SGS_Lorentz,
!!                  SGS_term%i_SGS_vp_induct, 
!!                  SGS_term%i_SGS_h_flux, SGS_term%i_SGS_c_flux, 
!!      subroutine cal_sph_wide_filtering_forces                        &
!!     &         (sph_rj, ipol_frc, ipol_wSGS, sph_wide_f, rj_fld)
!!       Input:   rj_fld(1:is_fld)
!!          is_fld = forces%i_m_advect, forces%i_lorentz, 
!!                  forces%i_vp_induct, forces%i_h_flux,
!!                  forces%i_c_flux
!!       Output:  rj_fld(1:is_fld)
!!          is_fld = wide_SGS%i_SGS_inertia, wide_SGS%i_SGS_Lorentz,
!!                  wide_SGS%i_SGS_vp_induct,
!!                  wide_SGS%i_SGS_h_flux, wide_SGS%i_SGS_c_flux
!!      subroutine cal_sph_dble_filtering_forces                        &
!!     &         (sph_rj, ipol_SGS, ipol_dSGS, sph_base_f, rj_fld)
!!       Input:   rj_fld(1:is_fld)
!!          is_fld = SGS_term%i_SGS_inertia, SGS_term%i_SGS_Lorentz, 
!!                  SGS_term%i_SGS_vp_induct, 
!!                  SGS_term%i_SGS_h_flux, SGS_term%i_SGS_c_flux, 
!!       Output:  rj_fld(1:is_fld)
!!          is_fld = dble_SGS%i_SGS_inertia, dble_SGS%i_SGS_Lorentz,
!!                  dble_SGS%i_SGS_vp_induct,
!!                  dble_SGS%i_SGS_h_flux, dble_SGS%i_SGS_c_flux
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(base_field_address), intent(in) :: ipol_fil
!!        type(base_field_address), intent(in) :: ipol_wfl
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(SGS_term_address), intent(in) :: ipol_SGS
!!        type(SGS_term_address), intent(in) :: ipol_dSGS
!!        type(sph_filters_type), intent(in) :: sph_base_f
!!        type(sph_filters_type), intent(in) :: sph_wide_f
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!
      module cal_filtered_sph_fields
!
      use m_precision
      use m_constants
!
      use t_SGS_control_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_sph_filtering_data
      use t_sph_filtering
      use t_SGS_term_labels
      use t_base_force_labels
      use t_base_field_labels
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
!>      Get @f$ \overline{\tilde{u}_{i}) } @f$,
!!        @f$ \overline{\tilde{\omega}_{i}) } @f$,
!!        @f$ \overline{\tilde{B}_{i}) } @f$,
!!        @f$ \overline{\tilde{J}_{i}) } @f$,
!!        @f$ \overline{\tilde{T}) } @f$, and 
!!        @f$ \overline{\tilde{C}) } @f$,
!!          from @f$ \tilde{u}_{i} @f$, @f$ \tilde{\omega}_{i} @f$, 
!!        @f$ \tilde{B}_{i} @f$, @f$ \tilde{J}_{i} @f$, 
!!        @f$ \tilde{T} @f$, and @f$ \tilde{C} @f$
      subroutine cal_sph_base_filtering_fields                          &
     &         (sph_rj, ipol_base, ipol_fil, sph_base_f, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_base
      type(base_field_address), intent(in) :: ipol_fil
      type(sph_filters_type), intent(in) :: sph_base_f
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter(ipol_base%i_velo, ipol_fil%i_velo,         &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol_base%i_vort, ipol_fil%i_vort,         &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol_base%i_magne, ipol_fil%i_magne,       &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol_base%i_current, ipol_fil%i_current,   &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
!
      call scalar_sph_filter(ipol_base%i_temp, ipol_fil%i_temp,         &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call scalar_sph_filter(ipol_base%i_light, ipol_fil%i_light,       &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      end subroutine cal_sph_base_filtering_fields
!
! ----------------------------------------------------------------------
!
!>      get @f$ \overline{\overline{\tilde{u}_{i}) }} @f$,
!!        @f$ \overline{\overline{\tilde{\omega}_{i}) }} @f$,
!!        @f$ \overline{\overline{\tilde{B}_{i}) }} @f$,
!!        @f$ \overline{\overline{\tilde{J}_{i}) }} @f$,
!!        @f$ \overline{\overline{\tilde{T}) }} @f$, and 
!!        @f$ \overline{\overline{\tilde{C}) }} @f$,
!!          from @f$ \tilde{u}_{i} @f$, @f$ \tilde{\omega}_{i} @f$, 
!!        @f$ \tilde{B}_{i} @f$, @f$ \tilde{J}_{i} @f$, 
!!        @f$ \tilde{T} @f$, and @f$ \tilde{C} @f$, 
      subroutine cal_sph_wide_filtering_fields                          &
     &         (sph_rj, ipol_base, ipol_wfl, sph_wide_f, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_base
      type(base_field_address), intent(in) :: ipol_wfl
      type(sph_filters_type), intent(in) :: sph_wide_f
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter(ipol_base%i_velo, ipol_wfl%i_velo,         &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol_base%i_vort, ipol_wfl%i_vort,         &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol_base%i_magne, ipol_wfl%i_magne,       &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol_base%i_current, ipol_wfl%i_current,   &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
!
      call scalar_sph_filter(ipol_base%i_temp, ipol_wfl%i_temp,         &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call scalar_sph_filter(ipol_base%i_light, ipol_wfl%i_light,       &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      end subroutine cal_sph_wide_filtering_fields
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
!>      Get @f$ \overline{(e_{ijk}\tilde{\omega}_{j}\tilde{u}_{k})} @f$,
!!      @f$ \overline{(e_{ijk}\tilde{J}_{j}\tilde{B}_{k})} @f$,
!!      @f$ \overline{(e_{ijk}\tilde{u}_{j}\tilde{B}_{k})} @f$,
!!      @f$ \overline{ \tilde{u}_{i} \tilde{T}) } @f$, and
!!      @f$ \overline{ \tilde{u}_{i} \tilde{C}) } @f$
!!        from  @f$  (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k} @f$, 
!!      @f$  (e_{ijk} \tilde{J}_{j} \tilde{B}_{k} @f$,
!!      @f$  (e_{ijk} \tilde{u}_{j} \tilde{B}_{k} @f$,
!!      @f$ \tilde{u}_{i} \tilde{T}) @f$, and 
!!      @f$ \tilde{u}_{i} \tilde{C}) @f$
      subroutine cal_sph_base_filtering_forces                          &
     &         (sph_rj, ipol_frc, ipol_SGS, sph_base_f, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_force_address), intent(in) :: ipol_frc
      type(SGS_term_address), intent(in) :: ipol_SGS
      type(sph_filters_type), intent(in) :: sph_base_f
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter                                            &
     &   (ipol_frc%i_m_advect, ipol_SGS%i_SGS_inertia,                  &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_frc%i_lorentz, ipol_SGS%i_SGS_Lorentz,                   &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_frc%i_vp_induct, ipol_SGS%i_SGS_vp_induct,               &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_frc%i_h_flux, ipol_SGS%i_SGS_h_flux,                     &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_frc%i_c_flux, ipol_SGS%i_SGS_c_flux,                     &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      end subroutine cal_sph_base_filtering_forces
!
! ----------------------------------------------------------------------
!
!>      Get @f$ \overline{\overline{
!!             (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k})}} @f$, 
!!      @f$ \overline{\overline{(e_{ijk}\tilde{J}_{j}\tilde{B}_{k})}} @f$,
!!      @f$ \overline{\overline{(e_{ijk}\tilde{u}_{j}\tilde{B}_{k})}} @f$,
!!      @f$ \overline{\overline{ \tilde{u}_{i} \tilde{T}) }} @f$, and
!!      @f$ \overline{\overline{ \tilde{u}_{i} \tilde{C}) }} @f$
!!        from  @f$  (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k} @f$, 
!!      @f$  (e_{ijk} \tilde{J}_{j} \tilde{B}_{k} @f$,
!!      @f$  (e_{ijk} \tilde{u}_{j} \tilde{B}_{k} @f$,
!!      @f$ \tilde{u}_{i} \tilde{T}) @f$, and 
!!      @f$ \tilde{u}_{i} \tilde{C}) @f$
      subroutine cal_sph_wide_filtering_forces                          &
     &         (sph_rj, ipol_frc, ipol_wSGS, sph_wide_f, rj_fld)
!
      use sph_filtering 
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_force_address), intent(in) :: ipol_frc
      type(SGS_term_address), intent(in) :: ipol_wSGS
      type(sph_filters_type), intent(in) :: sph_wide_f
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter                                            &
     &   (ipol_frc%i_m_advect, ipol_wSGS%i_SGS_inertia,                 &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_frc%i_lorentz, ipol_wSGS%i_SGS_Lorentz,                  &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_frc%i_vp_induct, ipol_wSGS%i_SGS_vp_induct,              &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_frc%i_h_flux, ipol_wSGS%i_SGS_h_flux,                    &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_frc%i_c_flux, ipol_wSGS%i_SGS_c_flux,                    &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      end subroutine cal_sph_wide_filtering_forces
!
! ----------------------------------------------------------------------
!
!>      Get @f$ \overline{\overline{
!!             (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k})}} @f$, 
!!      @f$ \overline{\overline{(e_{ijk}\tilde{J}_{j}\tilde{B}_{k})}} @f$,
!!      @f$ \overline{\overline{(e_{ijk}\tilde{u}_{j}\tilde{B}_{k})}} @f$,
!!      @f$ \overline{\overline{ \tilde{u}_{i} \tilde{T}) }} @f$, and
!!      @f$ \overline{\overline{ \tilde{u}_{i} \tilde{C}) }} @f$
!!        from  @f$  (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k} @f$, 
!!      @f$  (e_{ijk} \tilde{J}_{j} \tilde{B}_{k} @f$,
!!      @f$  (e_{ijk} \tilde{u}_{j} \tilde{B}_{k} @f$,
!!      @f$ \tilde{u}_{i} \tilde{T}) @f$, and 
!!      @f$ \tilde{u}_{i} \tilde{C}) @f$
      subroutine cal_sph_dble_filtering_forces                          &
     &         (sph_rj, ipol_SGS, ipol_dSGS, sph_base_f, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(SGS_term_address), intent(in) :: ipol_SGS
      type(SGS_term_address), intent(in) :: ipol_dSGS
      type(sph_filters_type), intent(in) :: sph_base_f
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter                                            &
     &   (ipol_SGS%i_SGS_inertia, ipol_dSGS%i_SGS_inertia,              &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_SGS%i_SGS_Lorentz, ipol_dSGS%i_SGS_Lorentz,              &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_SGS%i_SGS_vp_induct, ipol_dSGS%i_SGS_vp_induct,          &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_SGS%i_SGS_h_flux, ipol_dSGS%i_SGS_h_flux,                &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol_SGS%i_SGS_c_flux, ipol_dSGS%i_SGS_c_flux,                &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      end subroutine cal_sph_dble_filtering_forces
!
! ----------------------------------------------------------------------
!
      end module cal_filtered_sph_fields
