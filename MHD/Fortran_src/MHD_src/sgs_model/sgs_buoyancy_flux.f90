!sgs_buoyancy_flux.f90
!     module sgs_buoyancy_flux
!
!      Written by H. Matsui on June, 2005
!
!!      subroutine cal_SGS_gravity_flux                                 &
!!     &         (node, coef, i_sgs, i_flux, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: nod_fld
!
      module sgs_buoyancy_flux
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
      private :: cal_SGS_gravity_flux_self
      private :: cal_SGS_gravity_flux_radial
      private :: cal_SGS_gravity_flux_const
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_SGS_gravity_flux                                   &
     &         (node, coef, i_sgs, i_flux, nod_fld)
!
      use m_physical_property
      use t_geometry_data
      use t_phys_data
!
      integer (kind = kint), intent(in) :: i_sgs, i_flux
      real (kind = kreal), intent(in) :: coef
      type(node_data), intent(in) :: node
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if      (i_grav .eq. iflag_const_g) then
        call cal_SGS_gravity_flux_const                                 &
     &     (node%numnod, node%istack_nod_smp, grav,                     &
     &      coef, nod_fld%ntot_phys, i_sgs, i_flux, nod_fld%d_fld)
      else if (i_grav .eq. iflag_radial_g) then
        call cal_SGS_gravity_flux_radial                                &
     &     (node%numnod, node%istack_nod_smp, node%xx, node%a_r,        &
     &      coef, nod_fld%ntot_phys, i_sgs, i_flux, nod_fld%d_fld)
      else if (i_grav .eq. iflag_self_r_g) then
        call cal_SGS_gravity_flux_self                                  &
     &     (node%numnod, node%istack_nod_smp, node%xx,                  &
     &      coef, nod_fld%ntot_phys, i_sgs, i_flux, nod_fld%d_fld)
      end if
!
      end subroutine cal_SGS_gravity_flux
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_SGS_gravity_flux_self(numnod, inod_smp_stack,      &
     &          xx, coef_buo, ncomp_nod, i_sgs, i_flux, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind = kint), intent(in) :: i_sgs, i_flux
      real (kind = kreal), intent(in) :: coef_buo
!
      real (kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, ip, ist, ied
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_flux) = coef_buo                                 &
     &                        * ( d_nod(inod,i_sgs  ) * xx(inod,1)      &
     &                          + d_nod(inod,i_sgs+1) * xx(inod,2)      &
     &                          + d_nod(inod,i_sgs+2) * xx(inod,3) )
         end do
       end do
!$omp end parallel do
!
      end subroutine cal_SGS_gravity_flux_self
!
! -----------------------------------------------------------------------
!
      subroutine cal_SGS_gravity_flux_radial(numnod, inod_smp_stack,   &
     &          xx, a_radius, coef_buo, ncomp_nod, i_sgs, i_flux,      &
     &          d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: a_radius(numnod)
!
      integer (kind = kint), intent(in) :: i_sgs, i_flux
      real (kind = kreal), intent(in) :: coef_buo
!
      real (kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
            d_nod(inod,i_flux) = coef_buo                               &
     &          * (d_nod(inod,i_sgs  ) * xx(inod,1) * a_radius(inod)    &
     &           + d_nod(inod,i_sgs+1) * xx(inod,2) * a_radius(inod)    &
     &           + d_nod(inod,i_sgs+2) * xx(inod,3) * a_radius(inod) )
         end do
       end do
!$omp end parallel do
!
!
      end subroutine cal_SGS_gravity_flux_radial
!
! -----------------------------------------------------------------------
!
      subroutine cal_SGS_gravity_flux_const(numnod, inod_smp_stack,     &
     &          grav, coef_buo, ncomp_nod, i_sgs, i_flux, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: i_sgs, i_flux
      real (kind = kreal), intent(in) :: coef_buo
      real (kind = kreal), intent(in) :: grav(3)
!
      real (kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          d_nod(inod,i_flux) = coef_buo                                 &
     &              * ( d_nod(inod,i_sgs  ) * grav(1)                   &
     &                + d_nod(inod,i_sgs+1) * grav(2)                   &
     &                + d_nod(inod,i_sgs+2) * grav(3) )
         end do
       end do
!$omp end parallel do
!
      end subroutine cal_SGS_gravity_flux_const
!
! -----------------------------------------------------------------------
!
      end module sgs_buoyancy_flux
