!>@file   cal_fluxes.f90
!!@brief  module cal_fluxes
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Evaluate fluxes on grid
!!
!!@verbatim
!!      subroutine cal_flux_tensor(i_v1, i_v2, i_flux, nod_fld)
!!      subroutine cal_maxwell_tensor                                   &
!!     &         (ex_magne, i_magne, i_mxwl, nod_fld)
!!      subroutine cal_induction_tensor                                 &
!!     &         (i_magne, i_velo, i_idct, nod_fld)
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module cal_fluxes
!
      use m_precision
      use m_machine_parameter
      use t_phys_data
!
      implicit none
!
      private :: cal_flux_tensor_smp
      private :: cal_maxwell_tensor_smp, cal_induction_tensor_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_flux_tensor(i_v1, i_v2, i_flux, nod_fld)
!
      integer(kind = kint), intent(in) :: i_v1, i_v2, i_flux
      type(phys_data), intent(inout) :: nod_fld
!
!
!$omp parallel
      call cal_flux_tensor_smp(nod_fld%n_point,                         &
     &    nod_fld%d_fld(1,i_v1), nod_fld%d_fld(1,i_v2),                 &
     &    nod_fld%d_fld(1,i_flux))
!$omp end parallel
!
       end subroutine cal_flux_tensor
!
!-----------------------------------------------------------------------
!
      subroutine cal_maxwell_tensor                                     &
     &         (ex_magne, i_magne, i_mxwl, nod_fld)
!
      integer(kind = kint), intent(in) :: i_magne, i_mxwl
      real(kind = kreal), intent(in) :: ex_magne(3)
      type(phys_data), intent(inout) :: nod_fld
!
!
!$omp parallel
      call cal_maxwell_tensor_smp(nod_fld%n_point,                      &
     &    nod_fld%d_fld(1,i_magne), ex_magne, nod_fld%d_fld(1,i_mxwl))
!$omp end parallel
!
       end subroutine cal_maxwell_tensor
!
!-----------------------------------------------------------------------
!
      subroutine cal_induction_tensor                                   &
     &         (i_magne, i_velo, i_idct, nod_fld)
!
      integer(kind = kint), intent(in) :: i_magne, i_velo, i_idct
      type(phys_data), intent(inout) :: nod_fld
!
!
!$omp parallel
      call cal_induction_tensor_smp(nod_fld%n_point,                    &
     &    nod_fld%d_fld(1,i_magne), nod_fld%d_fld(1,i_velo),            &
     &    nod_fld%d_fld(1,i_idct))
!$omp end parallel
!
       end subroutine cal_induction_tensor
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_flux_tensor_smp(nnod, vec1, vec2, flux)
!
      integer (kind=kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: vec1(nnod,3)
      real(kind = kreal), intent(in) :: vec2(nnod,3)
!
      real(kind = kreal), intent(inout) :: flux(nnod,6)
!
!
!$omp workshare
      flux(1:nnod,1) = vec1(1:nnod,1) * vec2(1:nnod,1)
      flux(1:nnod,2) = vec1(1:nnod,1) * vec2(1:nnod,2)
      flux(1:nnod,3) = vec1(1:nnod,1) * vec2(1:nnod,3)
      flux(1:nnod,4) = vec1(1:nnod,2) * vec2(1:nnod,2)
      flux(1:nnod,5) = vec1(1:nnod,2) * vec2(1:nnod,3)
      flux(1:nnod,6) = vec1(1:nnod,3) * vec2(1:nnod,3)
!$omp end workshare nowait
!
       end subroutine cal_flux_tensor_smp
!
!-----------------------------------------------------------------------
!
      subroutine cal_maxwell_tensor_smp(nnod, magne, ex_magne, mxwl)
!
      integer (kind=kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: magne(nnod,3)
      real(kind = kreal), intent(in) :: ex_magne(3)
!
      real(kind = kreal), intent(inout) :: mxwl(nnod,6)
!
!
!$omp workshare
      mxwl(1:nnod,1) = ( magne(1:nnod,1)+ex_magne(1) )                  &
     &               * ( magne(1:nnod,1)+ex_magne(1) )
      mxwl(1:nnod,2) = ( magne(1:nnod,1)+ex_magne(1) )                  &
     &               * ( magne(1:nnod,2)+ex_magne(2) )
      mxwl(1:nnod,3) = ( magne(1:nnod,1)+ex_magne(1) )                  &
     &               * ( magne(1:nnod,3)+ex_magne(3) )
      mxwl(1:nnod,4) = ( magne(1:nnod,2)+ex_magne(2) )                  &
     &               * ( magne(1:nnod,2)+ex_magne(2) )
      mxwl(1:nnod,5) = ( magne(1:nnod,2)+ex_magne(2) )                  &
     &               * ( magne(1:nnod,3)+ex_magne(3) )
      mxwl(1:nnod,6) = ( magne(1:nnod,3)+ex_magne(3) )                  &
     &               * ( magne(1:nnod,3)+ex_magne(3) )
!$omp end workshare nowait
!
       end subroutine cal_maxwell_tensor_smp
!
!-----------------------------------------------------------------------
!
      subroutine cal_induction_tensor_smp(nnod, magne, velocity, idct)
!
      integer (kind=kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: magne(nnod,3)
      real(kind = kreal), intent(in) :: velocity(nnod,3)
!
      real(kind = kreal), intent(inout) :: idct(nnod,3)
!
!
!$omp workshare
      idct(1:nnod,1) = magne(1:nnod,2) * velocity(1:nnod,1)             &
     &               - magne(1:nnod,1) * velocity(1:nnod,2)
      idct(1:nnod,2) = magne(1:nnod,3) * velocity(1:nnod,1)             &
     &               - magne(1:nnod,1) * velocity(1:nnod,3)
      idct(1:nnod,3) = magne(1:nnod,3) * velocity(1:nnod,2)             &
     &               - magne(1:nnod,2) * velocity(1:nnod,3) 
!$omp end workshare nowait
!
       end subroutine cal_induction_tensor_smp
!
!-----------------------------------------------------------------------
!
      end module cal_fluxes
