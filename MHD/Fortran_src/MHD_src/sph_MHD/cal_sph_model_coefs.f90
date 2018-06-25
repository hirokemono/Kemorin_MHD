!!@brief  module cal_sph_model_coefs
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine sel_sph_model_coefs                                  &
!!     &         (numdir, ngrp_dynamic, stab_wt, ifld_sgs, icomp_sgs,   &
!!     &          nfld_sgs, ncomp_sgs,  sgs_zl, sgs_zt, sgs_c)
!!      subroutine cal_scalar_sph_model_coefs                           &
!!     &         (ngrp_dynamic, stab_wt, sgs_zl, sgs_zt, sgs_c)
!!@endverbatim
!
      module cal_sph_model_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
      private :: cal_vector_sph_model_coefs
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_sph_model_coefs                                    &
     &         (numdir, ngrp_dynamic, stab_wt, ifld_sgs, icomp_sgs,     &
     &          nfld_sgs, ncomp_sgs,  sgs_zl, sgs_zt, sgs_c)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numdir, ngrp_dynamic
      integer(kind = kint), intent(in) :: nfld_sgs, ncomp_sgs
      integer(kind = kint), intent(in) :: ifld_sgs, icomp_sgs
      real(kind = kreal), intent(in) :: sgs_zl(ngrp_dynamic,ncomp_sgs)
      real(kind = kreal), intent(in) :: sgs_zt(ngrp_dynamic,ncomp_sgs)
      real(kind = kreal), intent(in) :: stab_wt
!
      real(kind = kreal), intent(inout) :: sgs_c(ngrp_dynamic,nfld_sgs)
!
!
      if(numdir .eq. n_vector) then
        call cal_vector_sph_model_coefs(ngrp_dynamic, stab_wt,          &
     &      sgs_zl(1,icomp_sgs), sgs_zt(1,icomp_sgs),                   &
     &      sgs_c(1,ifld_sgs))
      else
        call cal_scalar_sph_model_coefs(ngrp_dynamic, stab_wt,          &
     &      sgs_zl(1,icomp_sgs), sgs_zt(1,icomp_sgs),                   &
     &      sgs_c(1,ifld_sgs))
      end if
!
      end subroutine sel_sph_model_coefs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_scalar_sph_model_coefs                             &
     &         (ngrp_dynamic, stab_wt, sgs_zl, sgs_zt, sgs_c)
!
      integer(kind = kint), intent(in) :: ngrp_dynamic
      real(kind = kreal), intent(in) :: sgs_zl(ngrp_dynamic)
      real(kind = kreal), intent(in) :: sgs_zt(ngrp_dynamic)
      real(kind = kreal), intent(in) :: stab_wt
!
      real(kind = kreal), intent(inout) :: sgs_c(ngrp_dynamic)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, ngrp_dynamic
        if( sgs_zt(inod) .ne. zero) then
!          sgs_c(inod) = one
!        else
          sgs_c(inod) = (one - stab_wt) * sgs_c(inod)                   &
     &                 + sgs_zl(inod) * stab_wt / sgs_zt(inod)
        end if
      end do
!$omp end parallel do
!
      end subroutine cal_scalar_sph_model_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine cal_vector_sph_model_coefs                             &
     &         (ngrp_dynamic, stab_wt, sgs_zl, sgs_zt, sgs_c)
!
      integer(kind = kint), intent(in) :: ngrp_dynamic
      real(kind = kreal), intent(in) :: sgs_zl(ngrp_dynamic,3)
      real(kind = kreal), intent(in) :: sgs_zt(ngrp_dynamic,3)
      real(kind = kreal), intent(in) :: stab_wt
!
      real(kind = kreal), intent(inout) :: sgs_c(ngrp_dynamic)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: rflag
!
!
!$omp parallel do private(rflag)
      do inod = 1, ngrp_dynamic
        rflag = sgs_zt(inod,1) * sgs_zt(inod,2) * sgs_zt(inod,3)
        if(rflag .ne. zero) then
!          sgs_c(inod) = one
!        else
          sgs_c(inod) = (one - stab_wt) * sgs_c(inod)                   &
     &                + (sgs_zl(inod,1) / sgs_zt(inod,1)                &
     &                 + sgs_zl(inod,2) / sgs_zt(inod,2)                &
     &                 + sgs_zl(inod,3) / sgs_zt(inod,3))               &
     &               * stab_wt / three
        end if
      end do
!$omp end parallel do
!
      end subroutine cal_vector_sph_model_coefs
!
!  ---------------------------------------------------------------------
!
      end module cal_sph_model_coefs
 