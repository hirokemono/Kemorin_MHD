!> @file  sel_diff_induction_MHD.f90
!!      module sel_diff_induction_MHD
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Selection of time evolution of induction terms
!!
!!@verbatim
!!      subroutine sel_diff_induction_MHD_adams                         &
!!     &         (SGS_param, dt, cd_prop, ipol, ipol_LES, rj_fld)
!!      subroutine sel_diff_induction_MHD_euler                           
!!     &         (SGS_param, dt, cd_prop, ipol, ipol_LES, rj_fld)
!!      subroutine sel_ini_adams_mag_induct                             &
!!     &         (SGS_param, cd_prop, ipol, ipol_LES, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        real(kind = kreal), intent(in) :: dt
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module sel_diff_induction_MHD
!
      use m_precision
!
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_explicit_term_labels
      use t_SGS_control_parameter
      use t_SGS_term_labels
      use t_phys_address
      use t_SGS_model_addresses
      use t_physical_property
      use t_phys_data
!
      implicit  none
!
      private :: sel_diff_induction_SGS_MHD_adams
      private :: sel_diff_induction_SGS_MHD_euler
      private :: sel_ini_adams_mag_induct_w_SGS
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_diff_induction_MHD_adams                           &
     &         (SGS_param, dt, cd_prop, ipol, ipol_LES, rj_fld)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      real(kind = kreal), intent(in) :: dt
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
      if(cd_prop%iflag_4_filter_induction .eqv. .FALSE.) then
        call sel_diff_induction_SGS_MHD_adams                           &
     &     (SGS_param%iflag_SGS_uxb, dt, cd_prop,                       &
     &      ipol%base, ipol%exp_work, ipol%forces,                      &
     &      ipol%diffusion, ipol_LES%SGS_term, rj_fld)
      else
        call sel_diff_induction_SGS_MHD_adams                           &
     &     (SGS_param%iflag_SGS_uxb, dt, cd_prop,                       &
     &      ipol%base, ipol%exp_work, ipol_LES%force_by_filter,         &
     &      ipol%diffusion, ipol_LES%SGS_term, rj_fld)
      end if
!
      end subroutine sel_diff_induction_MHD_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_diff_induction_MHD_euler                           &
     &         (SGS_param, dt, cd_prop, ipol, ipol_LES, rj_fld)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      real(kind = kreal), intent(in) :: dt
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
      if(cd_prop%iflag_4_filter_induction .eqv. .FALSE.) then
        call sel_diff_induction_SGS_MHD_euler                           &
     &     (SGS_param%iflag_SGS_uxb, dt, cd_prop,                       &
     &      ipol%base, ipol%forces, ipol%diffusion,                     &
     &      ipol_LES%SGS_term, rj_fld)
      else
        call sel_diff_induction_SGS_MHD_euler                           &
     &     (SGS_param%iflag_SGS_uxb, dt, cd_prop,                       &
     &      ipol%base, ipol_LES%force_by_filter, ipol%diffusion,        &
     &      ipol_LES%SGS_term, rj_fld)
      end if
!
      end subroutine sel_diff_induction_MHD_euler
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_mag_induct                               &
     &         (SGS_param, cd_prop, ipol, ipol_LES, rj_fld)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
      if(cd_prop%iflag_4_filter_induction .eqv. .FALSE.) then
        call sel_ini_adams_mag_induct_w_SGS                             &
     &     (SGS_param%iflag_SGS_uxb, ipol%exp_work,                     &
     &      ipol%forces, ipol_LES%SGS_term, rj_fld)
      else
        call sel_ini_adams_mag_induct_w_SGS                             &
     &     (SGS_param%iflag_SGS_uxb, ipol%exp_work,                     &
     &      ipol_LES%force_by_filter, ipol_LES%SGS_term, rj_fld)
      end if
!
      end subroutine sel_ini_adams_mag_induct
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_diff_induction_SGS_MHD_adams(iflag_SGS, dt,        &
     &          cd_prop, ipol_base, ipol_exp, ipol_frc, ipol_dif,       &
     &          ipol_SGS, rj_fld)
!
      use cal_explicit_terms
      use cal_explicit_SGS_induction
!
      integer(kind = kint), intent(in) :: iflag_SGS
      real(kind = kreal), intent(in) :: dt
!
      type(conductive_property), intent(in) :: cd_prop
      type(base_field_address), intent(in) :: ipol_base
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(SGS_term_address), intent(in) :: ipol_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
      if(iflag_SGS .eq. id_SGS_none) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'cal_diff_induction_MHD_adams'
        call cal_diff_induction_MHD_adams                               &
     &     (cd_prop, ipol_base, ipol_exp, ipol_frc, ipol_dif,           &
     &      dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'cal_diff_induction_wSGS_adams'
        call cal_diff_induction_wSGS_adams                              &
     &     (ipol_base, ipol_exp, ipol_frc, ipol_dif, ipol_SGS,          &
     &      dt, cd_prop%coef_exp, rj_fld%n_point, rj_fld%ntot_phys,     &
     &      rj_fld%d_fld)
      end if
!
      end subroutine sel_diff_induction_SGS_MHD_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_diff_induction_SGS_MHD_euler(iflag_SGS, dt,        &
     &          cd_prop, ipol_base, ipol_frc, ipol_dif, ipol_SGS,       &
     &          rj_fld)
!
      use cal_explicit_terms
      use cal_explicit_SGS_induction
!
      integer(kind = kint), intent(in) :: iflag_SGS
      real(kind = kreal), intent(in) :: dt
!
      type(conductive_property), intent(in) :: cd_prop
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(SGS_term_address), intent(in) :: ipol_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
      if(iflag_SGS .eq. id_SGS_none) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'cal_diff_induction_MHD_euler'
        call cal_diff_induction_MHD_euler                               &
     &     (cd_prop, ipol_base, ipol_frc, ipol_dif,                     &
     &      dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'cal_diff_induction_wSGS_euler'
        call cal_diff_induction_wSGS_euler                              &
     &     (ipol_base, ipol_frc, ipol_dif, ipol_SGS, dt,                &
     &      cd_prop%coef_exp, rj_fld%n_point, rj_fld%ntot_phys,         &
     &      rj_fld%d_fld)
      end if
!
      end subroutine sel_diff_induction_SGS_MHD_euler
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_mag_induct_w_SGS                         &
     &         (iflag_SGS, ipol_exp, ipol_frc, ipol_SGS, rj_fld)
!
      use cal_explicit_terms
      use cal_explicit_SGS_induction
!
      integer(kind = kint), intent(in) :: iflag_SGS
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(SGS_term_address), intent(in) :: ipol_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(iflag_SGS .eq. id_SGS_none) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'set_ini_adams_mag_induct'
        call set_ini_adams_mag_induct(ipol_exp, ipol_frc,               &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'SGS_ini_adams_mag_induct'
        call SGS_ini_adams_mag_induct(ipol_exp, ipol_frc, ipol_SGS,     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_ini_adams_mag_induct_w_SGS
!
! ----------------------------------------------------------------------
!
      end module sel_diff_induction_MHD
