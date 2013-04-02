!radial_int_for_sph_spec.f90
!      module radial_int_for_sph_spec
!
!     Written by H. Matsui on Feb., 2008
!
!      subroutine radial_integration(nri, kg_st, kg_ed, radius,         &
!     &          ntot_comp, f_org, f_int)
!  Evaluate radial integration f_int =  \int f_org r^{2} dr
!
!
      module radial_int_for_sph_spec
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: radial_int_by_trapezoid, radial_int_by_simpson
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine radial_integration(nri, kg_st, kg_ed, radius,          &
     &          ntot_comp, f_org, f_int)
!
      integer(kind = kint),  intent(in) :: nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
      integer(kind = kint),  intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: f_org(ntot_comp,nri)
!
      real(kind = kreal), intent(inout) :: f_int(ntot_comp)
!
!
      if( mod( (kg_ed-kg_st),2) .eq. 0) then
        call radial_int_by_simpson(nri, kg_st, kg_ed, radius,           &
     &      ntot_comp, f_org, f_int)
      else
        call radial_int_by_trapezoid(nri, kg_st, kg_ed, radius,         &
     &      ntot_comp, f_org, f_int)
      end if
!
      end subroutine radial_integration
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine radial_int_by_trapezoid(nri, kg_st, kg_ed, radius,     &
     &          ntot_comp, f_org, f_int)
!
      integer(kind = kint),  intent(in) :: nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
      integer(kind = kint),  intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: f_org(ntot_comp,nri)
!
      real(kind = kreal), intent(inout) :: f_int(ntot_comp)
!
      integer(kind = kint) :: icomp, kr, kst
      real(kind = kreal) :: dr1
!
!
      if(kg_st .eq. 0) then
        kst = 1
        dr1 = radius(1)
!$omp parallel do private(icomp)
        do icomp = 1, ntot_comp
          f_int(icomp) = f_org(icomp,1) * dr1 * half
        end do
!$omp end parallel do
      else
        kst = kg_st
!$omp parallel do private(icomp)
        do icomp = 1, ntot_comp
          f_int(icomp) = zero
        end do
!$omp end parallel do
      end if
!
!$omp parallel private(dr1)
      do kr = kst, kg_ed-1
        dr1 = radius(kr+1) - radius(kr)
!$omp do private(icomp)
        do icomp = 1, ntot_comp
          f_int(icomp) = f_int(icomp) + half * dr1                      &
     &                 + (f_org(icomp,kr  ) + f_org(icomp,kr+1))
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine radial_int_by_trapezoid
!
! -----------------------------------------------------------------------
!
      subroutine radial_int_by_simpson(nri, kg_st, kg_ed, radius,       &
     &          ntot_comp, f_org, f_int)
!
      integer(kind = kint),  intent(in) :: nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
      integer(kind = kint),  intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: f_org(ntot_comp,nri)
!
      real(kind = kreal), intent(inout) :: f_int(ntot_comp)
!
      integer(kind = kint) :: icomp, kr, kst
      real(kind = kreal) :: dr1, dr2, drs, coef
!
!
      if(kg_st .eq. 0) then
        kst = 2
        dr1 = radius(1)
        dr2 = radius(2) - radius(1)
        drs = radius(2)
        coef = drs*drs / (6.0d0*dr1*dr2)
!$omp parallel do private(icomp)
        do icomp = 1, ntot_comp
          f_int(icomp) = half * drs * (f_org(icomp,2))                  &
     &          + coef * (f_org(icomp,1)*drs - f_org(icomp,2)*dr1 )
        end do
!$omp end parallel do
      else
        kst = kg_st
!$omp parallel do private(icomp)
        do icomp = 1, ntot_comp
          f_int(icomp) = zero
        end do
!$omp end parallel do
      end if
!
!$omp parallel private(dr1,dr2,drs,coef)
      do kr = kst, kg_ed-1, 2
        dr1 = radius(kr+1) - radius(kr)
        dr2 = radius(kr+2) - radius(kr+1)
        drs = radius(kr+2) - radius(kr)
        coef = drs*drs / (6.0d0*dr1*dr2)
!$omp do private(icomp)
        do icomp = 1, ntot_comp
          f_int(icomp) = f_int(icomp)                                   &
     &          + half * drs * (f_org(icomp,kr  ) + f_org(icomp,kr+2))  &
     &          + coef * (f_org(icomp,kr+1)*drs                         &
     &            - (f_org(icomp,kr+2)*dr1 + f_org(icomp,kr  )*dr2))
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine radial_int_by_simpson
!
! -----------------------------------------------------------------------
!
      end module radial_int_for_sph_spec
