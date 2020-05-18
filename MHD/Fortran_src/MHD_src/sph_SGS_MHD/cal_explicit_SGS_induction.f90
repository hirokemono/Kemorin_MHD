!> @file  cal_explicit_SGS_induction.f90
!!      module cal_explicit_SGS_induction
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate time evolution of SGS induction by explicit scheme
!!
!!@verbatim
!!      subroutine cal_diff_induction_wSGS_adams                        &
!!     &         (ipol_base, ipol_exp, ipol_frc, ipol_dif, ipol_SGS, dt,&
!!     &          coef_exp, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine cal_diff_induction_wSGS_euler                        &
!!     &         (ipol_base, ipol_frc, ipol_dif, ipol_SGS, dt,          &
!!     &          coef_exp, nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine SGS_ini_adams_mag_induct                             &
!!     &         (ipol_exp, ipol_frc, ipol_SGS,                         &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(diffusion_address), intent(in) :: ipol_dif
!!        type(SGS_term_address), intent(in) :: ipol_SGS
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
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_diff_induction_wSGS_adams                          &
     &         (ipol_base, ipol_exp, ipol_frc, ipol_dif, ipol_SGS, dt,  &
     &          coef_exp, nnod_rj, ntot_phys_rj, d_rj)
!
      type(base_field_address), intent(in) :: ipol_base
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
        d_rj(inod,ipol_base%i_magne  ) = d_rj(inod,ipol_base%i_magne  ) &
     &           + dt*(coef_exp * d_rj(inod,ipol_dif%i_b_diffuse  )     &
     &                 + adam_0 * d_rj(inod,ipol_frc%i_induction  )     &
     &                 + adam_0 * d_rj(inod,ipol_SGS%i_SGS_induction  ) &
     &            + adam_1 * d_rj(inod,ipol_exp%i_pre_uxb  ))
        d_rj(inod,ipol_base%i_magne+2) = d_rj(inod,ipol_base%i_magne+2) &
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
     &         (ipol_base, ipol_frc, ipol_dif, ipol_SGS, dt,            &
     &          coef_exp, nnod_rj, ntot_phys_rj, d_rj)
!
      type(base_field_address), intent(in) :: ipol_base
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
        d_rj(inod,ipol_base%i_magne  ) = d_rj(inod,ipol_base%i_magne  ) &
     &          + dt*(coef_exp * d_rj(inod,ipol_dif%i_b_diffuse  )      &
     &                         + d_rj(inod,ipol_frc%i_induction  )      &
     &                         + d_rj(inod,ipol_SGS%i_SGS_induction  ))
        d_rj(inod,ipol_base%i_magne+2) = d_rj(inod,ipol_base%i_magne+2) &
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
