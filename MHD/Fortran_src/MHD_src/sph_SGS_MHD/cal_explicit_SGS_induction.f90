!> @file  cal_explicit_SGS_induction.f90
!!      module cal_explicit_SGS_induction
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate time evolution of SGS induction by explicit scheme
!!
!!@verbatim
!!      subroutine sel_diff_induction_MHD_adams(iflag_SGS, dt, cd_prop, &
!!     &          ipol_bse, ipol_exp, ipol_frc, ipol_dif, ipol_SGS,     &
!!     &          rj_fld)
!!      subroutine sel_diff_induction_MHD_euler(iflag_SGS, dt, cd_prop, &
!!     &          ipol_bse, ipol_frc, ipol_dif, ipol_SGS, rj_fld)
!!      subroutine sel_ini_adams_mag_induct(iflag_SGS, cd_prop,         &
!!     &          ipol_exp, ipol_frc, ipol_SGS, rj_fld)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(base_field_address), intent(in) :: ipol_bse
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(diffusion_address), intent(in) :: ipol_dif
!!        type(SGS_term_address), intent(in) :: ipol_SGS
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_explicit_SGS_induction
!
      use m_precision
      use m_t_step_parameter
!
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_explicit_term_labels
      use t_SGS_control_parameter
      use t_SGS_term_labels
      use t_physical_property
      use t_phys_data
!
      implicit  none
!
      private :: cal_diff_induction_wSGS_adams
      private :: cal_diff_induction_wSGS_euler
      private :: SGS_ini_adams_mag_induct
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_diff_induction_MHD_adams(iflag_SGS, dt, cd_prop,   &
     &          ipol_bse, ipol_exp, ipol_frc, ipol_dif, ipol_SGS,       &
     &          rj_fld)
!
      use cal_explicit_terms
!
      integer(kind = kint), intent(in) :: iflag_SGS
      real(kind = kreal), intent(in) :: dt
!
      type(conductive_property), intent(in) :: cd_prop
      type(base_field_address), intent(in) :: ipol_bse
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(SGS_term_address), intent(in) :: ipol_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
      if(iflag_SGS .gt. id_SGS_none) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'cal_diff_induction_wSGS_adams'
        call cal_diff_induction_wSGS_adams                              &
     &     (ipol_bse, ipol_exp, ipol_frc, ipol_dif, ipol_SGS,           &
     &      dt, cd_prop%coef_exp, rj_fld%n_point, rj_fld%ntot_phys,     &
     &      rj_fld%d_fld)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'cal_diff_induction_MHD_adams'
        call cal_diff_induction_MHD_adams                               &
     &     (ipol_bse, ipol_exp, ipol_frc, ipol_dif,                     &
     &      dt, cd_prop%coef_exp, rj_fld%n_point, rj_fld%ntot_phys,     &
     &      rj_fld%d_fld)
      end if
!
      end subroutine sel_diff_induction_MHD_adams
!
! ----------------------------------------------------------------------
!
      subroutine sel_diff_induction_MHD_euler(iflag_SGS, dt, cd_prop,   &
     &          ipol_bse, ipol_frc, ipol_dif, ipol_SGS, rj_fld)
!
      use cal_explicit_terms
!
      integer(kind = kint), intent(in) :: iflag_SGS
      real(kind = kreal), intent(in) :: dt
!
      type(conductive_property), intent(in) :: cd_prop
      type(base_field_address), intent(in) :: ipol_bse
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(SGS_term_address), intent(in) :: ipol_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
      if(iflag_SGS .gt. id_SGS_none) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'cal_diff_induction_wSGS_euler'
        call cal_diff_induction_wSGS_euler                              &
     &     (ipol_bse, ipol_frc, ipol_dif, ipol_SGS, dt,                 &
     &      cd_prop%coef_exp, rj_fld%n_point, rj_fld%ntot_phys,         &
     &      rj_fld%d_fld)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'cal_diff_induction_MHD_euler'
        call cal_diff_induction_MHD_euler                               &
     &     (ipol_bse, ipol_frc, ipol_dif, dt, cd_prop%coef_exp,         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_diff_induction_MHD_euler
!
! ----------------------------------------------------------------------
!
      subroutine sel_ini_adams_mag_induct(iflag_SGS, cd_prop,           &
     &          ipol_exp, ipol_frc, ipol_SGS, rj_fld)
!
      use cal_explicit_terms
!
      integer(kind = kint), intent(in) :: iflag_SGS
      type(conductive_property), intent(in) :: cd_prop
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(SGS_term_address), intent(in) :: ipol_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(cd_prop%iflag_Bevo_scheme .eq. id_no_evolution) return
      if(iflag_SGS .gt. id_SGS_none) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'SGS_ini_adams_mag_induct'
        call SGS_ini_adams_mag_induct(ipol_exp, ipol_frc, ipol_SGS,     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'set_ini_adams_mag_induct'
        call set_ini_adams_mag_induct(ipol_exp, ipol_frc,               &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_ini_adams_mag_induct
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_wSGS_adams                          &
     &         (ipol_bse, ipol_exp, ipol_frc, ipol_dif, ipol_SGS, dt,   &
     &          coef_exp, nnod_rj, ntot_phys_rj, d_rj)
!
      type(base_field_address), intent(in) :: ipol_bse
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(SGS_term_address), intent(in) :: ipol_SGS
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: coef_exp
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_bse%i_magne  ) = d_rj(inod,ipol_bse%i_magne  )   &
     &           + dt*(coef_exp * d_rj(inod,ipol_dif%i_b_diffuse  )     &
     &                 + adam_0 * d_rj(inod,ipol_frc%i_induction  )     &
     &                 + adam_0 * d_rj(inod,ipol_SGS%i_SGS_induction  ) &
     &            + adam_1 * d_rj(inod,ipol_exp%i_pre_uxb  ))
        d_rj(inod,ipol_bse%i_magne+2) = d_rj(inod,ipol_bse%i_magne+2)   &
     &           + dt*(coef_exp * d_rj(inod,ipol_dif%i_b_diffuse+2)     &
     &                 + adam_0 * d_rj(inod,ipol_frc%i_induction+2)     &
     &                 + adam_0 * d_rj(inod,ipol_SGS%i_SGS_induction+2) &
     &            + adam_1 * d_rj(inod,ipol_exp%i_pre_uxb+2))
!
        d_rj(inod,ipol_exp%i_pre_uxb  )                                 &
     &             =  d_rj(inod,ipol_frc%i_induction  )                 &
     &              + d_rj(inod,ipol_SGS%i_SGS_induction  )
        d_rj(inod,ipol_exp%i_pre_uxb+2)                                 &
     &             =  d_rj(inod,ipol_frc%i_induction+2)                 &
     &              + d_rj(inod,ipol_SGS%i_SGS_induction+2)
      end do
!$omp end parallel do
!
      end subroutine cal_diff_induction_wSGS_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_wSGS_euler                          &
     &         (ipol_bse, ipol_frc, ipol_dif, ipol_SGS, dt,             &
     &          coef_exp, nnod_rj, ntot_phys_rj, d_rj)
!
      type(base_field_address), intent(in) :: ipol_bse
      type(base_force_address), intent(in) :: ipol_frc
      type(diffusion_address), intent(in) :: ipol_dif
      type(SGS_term_address), intent(in) :: ipol_SGS
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: coef_exp
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_bse%i_magne  ) = d_rj(inod,ipol_bse%i_magne  )   &
     &          + dt*(coef_exp * d_rj(inod,ipol_dif%i_b_diffuse  )      &
     &                         + d_rj(inod,ipol_frc%i_induction  )      &
     &                         + d_rj(inod,ipol_SGS%i_SGS_induction  ))
        d_rj(inod,ipol_bse%i_magne+2) = d_rj(inod,ipol_bse%i_magne+2)   &
     &          + dt*(coef_exp * d_rj(inod,ipol_dif%i_b_diffuse+2)      &
     &                         + d_rj(inod,ipol_frc%i_induction+2)      &
     &                         + d_rj(inod,ipol_SGS%i_SGS_induction+2))
      end do
!$omp end parallel do
!
      end subroutine cal_diff_induction_wSGS_euler
!
! ----------------------------------------------------------------------
!
      subroutine SGS_ini_adams_mag_induct                               &
     &         (ipol_exp, ipol_frc, ipol_SGS,                           &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_frc
      type(SGS_term_address), intent(in) :: ipol_SGS
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint)  :: inod
!
!
!$omp parallel do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol_exp%i_pre_uxb  )                                 &
     &        =  d_rj(inod,ipol_frc%i_induction  )                      &
     &         + d_rj(inod,ipol_SGS%i_SGS_induction  )
        d_rj(inod,ipol_exp%i_pre_uxb+2)                                 &
     &        =  d_rj(inod,ipol_frc%i_induction+2)                      &
     &         + d_rj(inod,ipol_SGS%i_SGS_induction+2)
      end do
!$omp end parallel do
!
      end subroutine SGS_ini_adams_mag_induct
!
! ----------------------------------------------------------------------
!
      end module cal_explicit_SGS_induction
