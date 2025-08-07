!>@file   set_buoyancy_at_node.f90
!!@brief  module set_buoyancy_at_node
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!        modified by H. Matsui in Oct., 2005
!!        modified by H. Matsui in July, 2010
!!        modified by H. Matsui in Aug., 2025
!!
!>@brief  Buoyancy evaluation at each node
!!
!!@verbatim
!!      subroutine add_gravity_2_each_node(i_field, i_res, i_grav,      &
!!     &                                   coef, grav, node, nod_fld)
!!        integer(kind = kint), intent(in) :: i_field, i_res
!!        integer(kind = kint), intent(in) :: i_grav
!!        real(kind = kreal), intent(in) :: coef
!!        real(kind = kreal), intent(in) :: grav(3)
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine set_boussinesq_density_2_node(numnod, inod_smp_stack,&
!!     &          c_t, c_d, ncomp_nod, i_t, i_d, i_rho, d_nod)
!!        integer(kind = kint), intent(in) :: numnod, ncomp_nod
!!        integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!!        integer(kind = kint), intent(in) :: i_t, i_d, i_rho
!!        real(kind = kreal), intent(in) :: c_t, c_d
!!        real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!!
!!      subroutine int_vol_buoyancy_nod(numnod, inod_smp_stack,         &
!!     &          ncomp_nod, i_fc, d_nod, ml_o_fl, ff)
!!        integer(kind = kint), intent(in) :: numnod
!!        integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!!        integer (kind=kint), intent(in) :: ncomp_nod, i_fc
!!        real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
!!        real (kind=kreal), intent(in) :: ml_o_fl(numnod)
!!        real (kind=kreal), intent(inout) :: ff(numnod,3)
!!@endverbatim
!
      module set_buoyancy_at_node
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!
      private :: add_const_g_2_each_node, add_radial_g_2_each_node
      private :: add_self_g_2_each_node
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine add_gravity_2_each_node(i_field, i_res, i_grav,        &
     &                                   coef, grav, node, nod_fld)
!
      use t_geometry_data
      use t_phys_data
      use t_physical_property
!
      integer(kind = kint), intent(in) :: i_field, i_res
      integer(kind = kint), intent(in) :: i_grav
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: grav(3)
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
       if      (i_grav .eq. iflag_const_g) then
         call add_const_g_2_each_node                                   &
     &      (node%numnod, node%istack_nod_smp, coef, grav,              &
     &       nod_fld%d_fld(1,i_field), nod_fld%d_fld(1,i_res))
       else if (i_grav .eq. iflag_radial_g) then
         call add_radial_g_2_each_node                                  &
     &      (node%numnod, node%istack_nod_smp, node%xx, node%a_r,       &
     &       coef, nod_fld%d_fld(1,i_field), nod_fld%d_fld(1,i_res))
       else if (i_grav .eq. iflag_self_r_g) then
         call add_self_g_2_each_node                                    &
     &      (node%numnod, node%istack_nod_smp, node%xx, coef,           &
     &       nod_fld%d_fld(1,i_field), nod_fld%d_fld(1,i_res))
       end if
!
      end subroutine add_gravity_2_each_node
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_boussinesq_density_2_node(numnod, inod_smp_stack,  &
     &          c_t, c_d, ncomp_nod, i_t, i_d, i_rho, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: i_t, i_d, i_rho
      real(kind = kreal), intent(in) :: c_t, c_d
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: iproc, inod
      integer(kind = kint) :: ist, ied
      real(kind = kreal) :: cratio
!
!
       cratio = c_d/c_t
!$omp parallel do private(inod,ist,ied)
       do iproc = 1, np_smp
         ist = inod_smp_stack(iproc-1) + 1
         ied = inod_smp_stack(iproc)
!cdir nodep
         do inod = ist, ied
           d_nod(inod,i_rho  ) = -(d_nod(inod,i_t)                      &
     &                          + cratio * d_nod(inod,i_d) )
         end do
       end do
!$omp end parallel do
!
      end subroutine set_boussinesq_density_2_node
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_const_g_2_each_node(numnod, inod_smp_stack,        &
     &                                   coef, grav, scalar, d_nod)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: grav(3)
      real(kind = kreal), intent(in) :: scalar(numnod)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,3)
!
      integer(kind = kint) :: iproc, inod
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(inod,ist,ied) 
       do iproc = 1, np_smp
         ist = inod_smp_stack(iproc-1) + 1
         ied = inod_smp_stack(iproc)
!cdir nodep
         do inod = ist, ied
           d_nod(inod,1) = coef * grav(1) * scalar(inod)
           d_nod(inod,2) = coef * grav(2) * scalar(inod)
           d_nod(inod,3) = coef * grav(3) * scalar(inod)
         end do
       end do
!$omp end parallel do
!
      end subroutine add_const_g_2_each_node
!
!  ---------------------------------------------------------------------
!
      subroutine add_radial_g_2_each_node(numnod, inod_smp_stack,       &
     &          xx, a_radius, coef, scalar, d_nod)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) ::xx(numnod,3)
      real(kind = kreal), intent(in) ::a_radius(numnod)
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: scalar(numnod)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,3)
!
      integer(kind = kint) :: iproc, inod
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(inod,ist,ied) 
       do iproc = 1, np_smp
         ist = inod_smp_stack(iproc-1) + 1
         ied = inod_smp_stack(iproc)
!cdir nodep
         do inod = ist, ied
           d_nod(inod,1) = d_nod(inod,1) + coef * scalar(inod)          &
     &                                  * xx(inod,1) * a_radius(inod)
           d_nod(inod,2) = d_nod(inod,2) + coef * scalar(inod)          &
     &                                  * xx(inod,2) * a_radius(inod)
           d_nod(inod,3) = d_nod(inod,3) + coef * scalar(inod)          &
     &                                  * xx(inod,3) * a_radius(inod)
         end do
       end do
!$omp end parallel do
!
      end subroutine add_radial_g_2_each_node
!
!  ---------------------------------------------------------------------
!
      subroutine add_self_g_2_each_node(numnod, inod_smp_stack,         &
     &                                  xx, coef, scalar, d_nod)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) ::xx(numnod,3)
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: scalar(numnod)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,3)
!
      integer(kind = kint) :: iproc, inod
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(inod,ist,ied) 
       do iproc = 1, np_smp
         ist = inod_smp_stack(iproc-1) + 1
         ied = inod_smp_stack(iproc)
!cdir nodep
         do inod = ist, ied
           d_nod(inod,1) = d_nod(inod,1) + coef*xx(inod,1)*scalar(inod)
           d_nod(inod,2) = d_nod(inod,2) + coef*xx(inod,2)*scalar(inod)
           d_nod(inod,3) = d_nod(inod,3) + coef*xx(inod,3)*scalar(inod)
         end do
       end do
!$omp end parallel do
!
      end subroutine add_self_g_2_each_node
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vol_buoyancy_nod(numnod, inod_smp_stack,           &
     &          ncomp_nod, i_fc, d_nod, ml_o_fl, ff)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: ncomp_nod, i_fc
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
      real (kind=kreal), intent(in) :: ml_o_fl(numnod)
!
      real (kind=kreal), intent(inout) :: ff(numnod,3)
!
      integer (kind=kint) :: iproc, inod
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!cdir nodep
        do inod = ist, ied
          ff(inod,1) = ff(inod,1) + d_nod(inod,i_fc  ) * ml_o_fl(inod)
          ff(inod,2) = ff(inod,2) + d_nod(inod,i_fc+1) * ml_o_fl(inod)
          ff(inod,3) = ff(inod,3) + d_nod(inod,i_fc+2) * ml_o_fl(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine int_vol_buoyancy_nod
!
! ----------------------------------------------------------------------
!
      end module set_buoyancy_at_node
