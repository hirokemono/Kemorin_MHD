!> @file  buoyancy_flux.f90
!!      module buoyancy_flux
!!
!! @author  H. Matsui
!! @date Programmed in June, 2005
!
!> @brief Evaluate buoyancy flux
!!
!!@verbatim
!!      subroutine cal_gravity_flux(node, i_grav, coef, grav,           &
!!     &          i_velo, i_scalar, i_flux, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module buoyancy_flux
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      private :: cal_gravity_flux_self
      private :: cal_gravity_flux_radial, cal_gravity_flux_const
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_gravity_flux(node, i_grav, coef, grav,             &
     &          i_velo, i_scalar, i_flux, nod_fld)
!
      use t_geometry_data
      use t_phys_data
      use t_physical_property
!
      type(node_data), intent(in) :: node
      integer (kind = kint), intent(in) :: i_scalar, i_flux, i_velo
      integer(kind = kint), intent(in) :: i_grav
      real(kind = kreal), intent(in) :: grav(3)
      real (kind = kreal), intent(in) :: coef
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if      (i_grav .eq. iflag_const_g) then
        call cal_gravity_flux_const                                     &
     &     (node%numnod, node%istack_nod_smp, grav,                     &
     &      coef, nod_fld%ntot_phys, i_scalar, i_velo, i_flux,          &
     &      nod_fld%d_fld)
      else if (i_grav .eq. iflag_radial_g) then
        call cal_gravity_flux_radial                                    &
     &     (node%numnod, node%istack_nod_smp, node%xx, node%a_r,        &
     &      coef, nod_fld%ntot_phys, i_scalar, i_velo, i_flux,          &
     &      nod_fld%d_fld)
      else if (i_grav .eq. iflag_self_r_g) then
        call cal_gravity_flux_self                                      &
     &     (node%numnod, node%istack_nod_smp, node%xx,                  &
     &      coef, nod_fld%ntot_phys, i_scalar, i_velo, i_flux,          &
     &      nod_fld%d_fld)
      end if
!
      end subroutine cal_gravity_flux
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_gravity_flux_self(numnod, inod_smp_stack, xx,      &
     &          coef_buo, ncomp_nod, i_scalar, i_velo, i_flux, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: coef_buo
!
      integer(kind = kint), intent(in) :: i_scalar, i_velo, i_flux
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, ip, ist, ied
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_flux) = coef_buo * d_nod(inod,i_scalar)          &
     &                    * ( d_nod(inod,i_velo  ) * xx(inod,1)   &
     &                      + d_nod(inod,i_velo+1) * xx(inod,2)   &
     &                      + d_nod(inod,i_velo+2) * xx(inod,3))
         end do
       end do
!$omp end parallel do
!
      end subroutine cal_gravity_flux_self
!
! -----------------------------------------------------------------------
!
      subroutine cal_gravity_flux_radial(numnod, inod_smp_stack, xx,    &
     &          a_radius, coef_buo, ncomp_nod, i_scalar, i_velo,        &
     &          i_flux, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: coef_buo
!
      integer (kind = kint), intent(in) :: i_scalar, i_velo, i_flux
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
            d_nod(inod,i_flux) = coef_buo * d_nod(inod,i_scalar)        &
     &       * (d_nod(inod,i_velo  ) * xx(inod,1)*a_radius(inod)        &
     &        + d_nod(inod,i_velo+1) * xx(inod,2)*a_radius(inod)        &
     &        + d_nod(inod,i_velo+2) * xx(inod,3)*a_radius(inod))
         end do
       end do
!$omp end parallel do
!
!
      end subroutine cal_gravity_flux_radial
!
! -----------------------------------------------------------------------
!
      subroutine cal_gravity_flux_const(numnod, inod_smp_stack, grav,   &
     &          coef_buo, ncomp_nod, i_scalar, i_velo, i_flux, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef_buo
      real(kind = kreal), intent(in) :: grav(3)
!
      integer(kind = kint), intent(in) :: i_scalar, i_velo, i_flux
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_flux) = coef_buo * d_nod(inod,i_scalar)          &
     &              * ( d_nod(inod,i_velo  ) * grav(1)                  &
     &                + d_nod(inod,i_velo+1) * grav(2)                  &
     &                + d_nod(inod,i_velo+2) * grav(3) )
         end do
       end do
!$omp end parallel do
!
      end subroutine cal_gravity_flux_const
!
! -----------------------------------------------------------------------
!
      end module buoyancy_flux
