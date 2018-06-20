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
!!     &         (sph_rj, ipol, SGS_param, dynamic_SPH, rj_fld)
!!      subroutine cal_filtered_sph_rj_forces                           &
!!     &         (sph_rj, ipol, SGS_param, dynamic_SPH, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine cal_sph_base_filtering_fields                        &
!!     &         (sph_rj, ipol, sph_base_f, rj_fld)
!!       Input:   rj_fld(1:is_fld)
!!          is_fld = i_velo, i_vort, i_magne, i_current, i_temp, i_light
!!       Output:  rj_fld(1:is_fld)
!!                  i_filter_current, i_filter_temp, i_filter_comp, 
!!          is_fld = i_filter_velo, i_filter_vort, i_filter_magne, 
!!      subroutine cal_sph_wide_filtering_fields                        &
!!     &         (sph_rj, ipol, sph_wide_f, rj_fld)
!!       Input:   rj_fld(1:is_fld)
!!          is_fld = i_velo, i_vort, i_magne, i_current, i_temp, i_light
!!       Output:  rj_fld(1:is_fld)
!!                  i_wide_fil_velo, i_wide_fil_vort, i_wide_fil_magne,
!!                  i_wide_fil_current, i_wide_fil_temp, i_wide_fil_comp
!!
!!      subroutine cal_sph_base_filtering_forces                        &
!!     &         (sph_rj, ipol, sph_base_f, rj_fld)
!!       Input:   rj_fld(1:is_fld)
!!          is_fld = i_m_advect, i_lorentz, i_vp_induct,
!!                  i_h_flux, i_c_flux
!!       Output:  rj_fld(1:is_fld)
!!          is_fld = i_SGS_inertia, i_SGS_Lorentz, i_SGS_vp_induct, 
!!                  i_SGS_h_flux, i_SGS_c_flux, 
!!      subroutine cal_sph_wide_filtering_forces                        &
!!     &         (sph_rj, ipol, sph_wide_f, rj_fld)
!!       Input:   rj_fld(1:is_fld)
!!          is_fld = i_m_advect, i_lorentz, i_vp_induct,
!!                  i_h_flux, i_c_flux
!!       Output:  rj_fld(1:is_fld)
!!          is_fld = i_wide_SGS_inertia, i_wide_SGS_Lorentz,
!!                  i_wide_SGS_vp_induct,
!!                  i_wide_SGS_h_flux, i_wide_SGS_c_flux
!!      subroutine cal_sph_dble_filtering_forces                        &
!!     &         (sph_rj, ipol, sph_base_f, rj_fld)
!!       Input:   rj_fld(1:is_fld)
!!          is_fld = i_SGS_inertia, i_SGS_Lorentz, i_SGS_vp_induct, 
!!                  i_SGS_h_flux, i_SGS_c_flux, 
!!       Output:  rj_fld(1:is_fld)
!!          is_fld = i_dbl_SGS_inertia, i_dbl_SGS_Lorentz,
!!                  i_dbl_SGS_vp_induct,
!!                  i_dbl_SGS_h_flux, i_dbl_SGS_c_flux
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
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
     &         (sph_rj, ipol, sph_base_f, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_filters_type), intent(in) :: sph_base_f
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter(ipol%i_velo, ipol%i_filter_velo,           &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_vort, ipol%i_filter_vort,           &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_magne, ipol%i_filter_magne,         &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_current, ipol%i_filter_current,     &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
!
      call scalar_sph_filter(ipol%i_temp, ipol%i_filter_temp,           &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call scalar_sph_filter(ipol%i_light, ipol%i_filter_comp,          &
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
      call vector_sph_filter(ipol%i_velo, ipol%i_wide_fil_velo,         &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_vort, ipol%i_wide_fil_vort,         &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol%i_magne, ipol%i_wide_fil_magne,                          &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol%i_current, ipol%i_wide_fil_current,                      &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
!
      call scalar_sph_filter(ipol%i_temp, ipol%i_wide_fil_temp,         &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call scalar_sph_filter(ipol%i_light, ipol%i_wide_fil_comp,        &
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
     &         (sph_rj, ipol, sph_base_f, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_filters_type), intent(in) :: sph_base_f
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter(ipol%i_m_advect, ipol%i_SGS_inertia,       &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_lorentz, ipol%i_SGS_Lorentz,        &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_vp_induct, ipol%i_SGS_vp_induct,    &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_h_flux, ipol%i_SGS_h_flux,          &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_c_flux, ipol%i_SGS_c_flux,          &
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
     &   (ipol%i_m_advect, ipol%i_wide_SGS_inertia,                     &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol%i_lorentz, ipol%i_wide_SGS_Lorentz,                      &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol%i_vp_induct, ipol%i_wide_SGS_vp_induct,                  &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_h_flux, ipol%i_wide_SGS_h_flux,     &
     &    sph_rj, sph_wide_f%r_filter, sph_wide_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_c_flux, ipol%i_wide_SGS_c_flux,     &
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
     &         (sph_rj, ipol, sph_base_f, rj_fld)
!
      use sph_filtering
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(sph_filters_type), intent(in) :: sph_base_f
      type(phys_data), intent(inout) :: rj_fld
!
!
      call vector_sph_filter                                            &
     &   (ipol%i_SGS_inertia, ipol%i_dbl_SGS_inertia,                   &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol%i_SGS_Lorentz, ipol%i_dbl_SGS_Lorentz,                   &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter                                            &
     &   (ipol%i_SGS_vp_induct, ipol%i_dbl_SGS_vp_induct,               &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_SGS_h_flux, ipol%i_dbl_SGS_h_flux,  &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      call vector_sph_filter(ipol%i_SGS_c_flux, ipol%i_dbl_SGS_c_flux,  &
     &    sph_rj, sph_base_f%r_filter, sph_base_f%sph_filter, rj_fld)
!
      end subroutine cal_sph_dble_filtering_forces
!
! ----------------------------------------------------------------------
!
      end module cal_filtered_sph_fields
