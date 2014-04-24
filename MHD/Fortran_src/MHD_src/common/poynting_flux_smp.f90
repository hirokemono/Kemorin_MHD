!>@file   poynting_flux_smp.f90
!!@brief  module poynting_flux_smp
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!
!>@brief Evaluate poynting flux in physical space
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_electric_field_smp(np_smp, nnod, inod_smp_stack, &
!!     &          coef_d, current, uxb, e_field)
!!      subroutine cal_poynting_flux_smp(np_smp, nnod, inod_smp_stack,  &
!!     &          coef_d, current, uxb, b_field, poynting)
!!      subroutine cal_rtp_magnetic_streach(nnod, nidx_rtp,             &
!!     &          a_r_1d_rtp_r, cot_theta_1d_rtp, b_field, u_field,     &
!!     &          grad_ux, grad_uy, grad_uz, magne_streach)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  nnod     Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param coef_d        Coefficient for magnetic diffusion
!!
!!@n @param  b_field(nnod,3)     Magnetic field @f$ B_{i} @f$
!!@n @param  current(nnod,3)     Current density
!!                                 @f$e_{ijk) \partial_{j} B_{k})@f$
!!@n @param  uxb(nnod,3)         Induction @f$(e_{ijk) u{j} B_{k})@f$
!!@n @param  e_field(nnod,3)     Electric field
!!              @f$ E_{i} = \sigma^{-1} J_{i} - (e_{ijk) u{j} B_{k})@f$
!!@n @param  poynting(nnod,3)    Poynting flux @f$(e_{ijk) E{j} B_{k})@f$
!
      module poynting_flux_smp
!
      use m_precision
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_electric_field_smp(np_smp, nnod, inod_smp_stack,   &
     &          coef_d, current, uxb, e_field)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef_d
      real (kind=kreal), intent(in) :: current(nnod,3), uxb(nnod,3)
      real (kind=kreal), intent(inout) :: e_field(nnod,3)
!
      integer(kind = kint) :: ip, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          e_field(inod,1) = coef_d * current(inod,1) - uxb(inod,1)
          e_field(inod,2) = coef_d * current(inod,2) - uxb(inod,2)
          e_field(inod,3) = coef_d * current(inod,3) - uxb(inod,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_electric_field_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_poynting_flux_smp(np_smp, nnod, inod_smp_stack,    &
     &          coef_d, current, uxb, b_field, poynting)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef_d
      real (kind=kreal), intent(in) :: current(nnod,3), uxb(nnod,3)
      real (kind=kreal), intent(in) :: b_field(nnod,3)
      real (kind=kreal), intent(inout) :: poynting(nnod,3)
!
      integer(kind = kint) :: ip, inod, ist, ied
      real (kind=kreal) :: e_fld(3)
!
!$omp do private(inod,ist,ied,e_fld)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          e_fld(1) = coef_d * current(inod,1) - uxb(inod,1)
          e_fld(2) = coef_d * current(inod,2) - uxb(inod,2)
          e_fld(3) = coef_d * current(inod,3) - uxb(inod,3)
!
          poynting(inod,1)                                              &
     &         = e_fld(2)*b_field(inod,3) - e_fld(3)*b_field(inod,2)
          poynting(inod,2)                                              &
     &         = e_fld(3)*b_field(inod,1) - e_fld(1)*b_field(inod,3)
          poynting(inod,3)                                              &
     &         = e_fld(1)*b_field(inod,2) - e_fld(2)*b_field(inod,1)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_poynting_flux_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rtp_magnetic_streach(nnod, nidx_rtp,               &
     &          a_r_1d_rtp_r, cot_theta_1d_rtp, b_field, u_field,       &
     &          grad_ux, grad_uy, grad_uz, magne_streach)
!
      integer (kind=kint), intent(in) :: nnod
      integer (kind=kint), intent(in) :: nidx_rtp(3)
      real (kind=kreal), intent(in) :: a_r_1d_rtp_r(nnod)
      real (kind=kreal), intent(in) :: cot_theta_1d_rtp(nidx_rtp(2))
      real (kind=kreal), intent(in) :: grad_ux(nnod,3), grad_uy(nnod,3)
      real (kind=kreal), intent(in) :: grad_uz(nnod,3)
      real (kind=kreal), intent(in) :: u_field(nnod,3), b_field(nnod,3)
      real (kind=kreal), intent(inout) :: magne_streach(nnod,3)
!
      integer(kind = kint) :: mp, lt, kr, inod
!
!$omp parallel do private(kr,lt,inod)
      do mp = 1, nidx_rtp(3)
        do lt = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            inod = kr + (lt-1) * nidx_rtp(1)                            &
     &                + (mp-1) * nidx_rtp(1)*nidx_rtp(2)
!
            magne_streach(inod,1) =  grad_ux(inod,1)*b_field(inod,1)    &
     &                             + grad_ux(inod,2)*b_field(inod,2)    &
     &                             + grad_ux(inod,3)*b_field(inod,3)    &
     &                           - (b_field(inod,2)*u_field(inod,2)     &
     &                            + b_field(inod,3)*u_field(inod,3))    &
     &                           * a_r_1d_rtp_r(inod)
            magne_streach(inod,2) =  grad_uy(inod,1)*b_field(inod,1)    &
     &                             + grad_uy(inod,2)*b_field(inod,2)    &
     &                             + grad_uy(inod,3)*b_field(inod,3)    &
     &                           - (b_field(inod,3)*u_field(inod,3)     &
     &                            * cot_theta_1d_rtp(lt)                &
     &                            - b_field(inod,2)*u_field(inod,1))    &
     &                           * a_r_1d_rtp_r(inod)
            magne_streach(inod,3) =  grad_uz(inod,1)*b_field(inod,1)    &
     &                             + grad_uz(inod,2)*b_field(inod,2)    &
     &                             + grad_uz(inod,3)*b_field(inod,3)    &
     &                           + (b_field(inod,3)*u_field(inod,1)     &
     &                            + b_field(inod,3)*u_field(inod,2)     &
     &                            * cot_theta_1d_rtp(lt) )              &
     &                           * a_r_1d_rtp_r(inod)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rtp_magnetic_streach
!
! -----------------------------------------------------------------------
!
      end module poynting_flux_smp
