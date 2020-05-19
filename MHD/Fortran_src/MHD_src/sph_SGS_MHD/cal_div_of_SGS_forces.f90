!>@file   cal_div_of_SGS_forces.f90
!!@brief  module cal_div_of_SGS_forces
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Apr. 2020
!
!>@brief Evaluate divergence of forces by SGS term 
!!       for pressure evaluation
!!
!!@verbatim
!!      subroutine sum_div_of_SGS_forces                                &
!!     &         (ipol_base, ipol_div_SGS, rj_fld)
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(SGS_term_address), intent(in) :: ipol_div_SGS
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_div_of_SGS_forces
!
      use m_precision
      use t_base_field_labels
      use t_base_force_labels
      use t_SGS_term_labels
!
      implicit  none
!
      private :: set_SGS_forces_to_div_force
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sum_div_of_SGS_forces                                  &
     &         (ipol_base, ipol_div_SGS, rj_fld)
!
      use t_phys_data
      use cal_div_of_forces
!
      type(base_field_address), intent(in) :: ipol_base
      type(SGS_term_address), intent(in) :: ipol_div_SGS
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      if(ipol_div_SGS%i_SGS_inertia  .ne. 0                             &
     &  .and.  ipol_div_SGS%i_SGS_Lorentz .ne. 0) then
        call set_SGS_forces_to_div_force                                &
     &     (ipol_base%i_press, ipol_div_SGS,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
!
        if(ipol_div_SGS%i_SGS_inertia .ne. id_turn_OFF) then
          call add_term_to_div_force                                    &
     &       (ipol_base%i_press, ipol_div_SGS%i_SGS_inertia,            &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(ipol_div_SGS%i_SGS_Lorentz .ne. izero) then
          call add_term_to_div_force                                    &
     &       (ipol_base%i_press, ipol_div_SGS%i_SGS_Lorentz,            &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
      end if
!$omp end parallel
!
      end subroutine sum_div_of_SGS_forces
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
!
      subroutine set_SGS_forces_to_div_force                            &
     &         (is_press, ipol_div_SGS, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: is_press
      type(SGS_term_address), intent(in) :: ipol_div_SGS
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,is_press) =  d_rj(inod,is_press)                      &
     &                       - d_rj(inod,ipol_div_SGS%i_SGS_inertia)    &
     &                       + d_rj(inod,ipol_div_SGS%i_SGS_Lorentz)
      end do
!$omp end do nowait
!
      end subroutine set_SGS_forces_to_div_force
!
! ----------------------------------------------------------------------
!
      end module cal_div_of_SGS_forces
