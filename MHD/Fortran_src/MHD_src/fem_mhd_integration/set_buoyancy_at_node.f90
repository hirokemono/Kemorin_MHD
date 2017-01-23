!set_buoyancy_at_node.f90
!      module set_buoyancy_at_node
!
!      Written by H. Matsui on July, 2010
!
!!      subroutine set_gravity_2_each_node                              &
!!     &          (i_field, i_res, i_grav, coef, grav, node, nod_fld)
!!      subroutine set_double_gravity_2_each_node(i_f1, i_f2, i_r1,     &
!!     &          i_grav, c1, c2, grav, node, nod_fld)
!!      subroutine set_boussinesq_density_2_node(numnod, inod_smp_stack,&
!!     &          c_t, c_d, ncomp_nod, i_t, i_d, i_rho, d_nod)
!!
!!      subroutine int_vol_buoyancy_nod(numnod, inod_smp_stack,         &
!!     &          ncomp_nod, i_fc, d_nod, ml_o_fl, ff)
!
      module set_buoyancy_at_node
!
      use m_precision
!
      use m_machine_parameter
!      use m_physical_property
!
      implicit none
!
!
      private :: const_g_2_each_node, const_double_g_2_each_node
      private :: radial_g_2_each_node, radial_double_g_2_each_node
      private :: self_g_2_each_node, self_double_g_2_each_node
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_gravity_2_each_node                                &
     &          (i_field, i_res, i_grav, coef, grav, node, nod_fld)
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
         call const_g_2_each_node                                       &
     &      (node%numnod, node%istack_nod_smp, coef, grav,              &
     &       nod_fld%ntot_phys, i_field, i_res, nod_fld%d_fld)
       else if (i_grav .eq. iflag_radial_g) then
         call radial_g_2_each_node                                      &
     &      (node%numnod, node%istack_nod_smp, node%xx, node%a_r,       &
     &       coef, nod_fld%ntot_phys, i_field, i_res, nod_fld%d_fld)
       else if (i_grav .eq. iflag_self_r_g) then
         call self_g_2_each_node                                        &
     &      (node%numnod, node%istack_nod_smp, node%xx, coef,           &
     &       nod_fld%ntot_phys, i_field, i_res, nod_fld%d_fld)
       end if
!
      end subroutine set_gravity_2_each_node
!
!  ---------------------------------------------------------------------
!
      subroutine set_double_gravity_2_each_node(i_f1, i_f2, i_res,      &
     &          i_grav, c1, c2, grav, node, nod_fld)
!
      use t_geometry_data
      use t_phys_data
      use t_physical_property
!
      integer(kind = kint), intent(in) :: i_f1, i_f2, i_res
      integer(kind = kint), intent(in) :: i_grav
      real(kind = kreal), intent(in) :: c1, c2
      real(kind = kreal), intent(in) :: grav(3)
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
!
       if     (i_grav .eq. iflag_const_g) then
         call const_double_g_2_each_node                                &
     &      (node%numnod, node%istack_nod_smp, c1, c2, grav, &
     &       nod_fld%ntot_phys, i_f1, i_f2, i_res, nod_fld%d_fld)
       else if(i_grav .eq. iflag_radial_g) then
         call radial_double_g_2_each_node                               &
     &      (node%numnod, node%istack_nod_smp, node%xx, node%a_r,       &
     &       c1, c2, nod_fld%ntot_phys, i_f1, i_f2, i_res,             &
     &       nod_fld%d_fld)
       else if(i_grav .eq. iflag_self_r_g) then
         call self_double_g_2_each_node                                 &
     &      (node%numnod, node%istack_nod_smp, node%xx, c1, c2,         &
     &       nod_fld%ntot_phys, i_f1, i_f2, i_res, nod_fld%d_fld)
       end if
!
      end subroutine set_double_gravity_2_each_node
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
      subroutine const_g_2_each_node(numnod, inod_smp_stack,            &
     &          coef, grav, ncomp_nod, i_field, i_res, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: i_field, i_res
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: grav(3)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
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
           d_nod(inod,i_res  ) = coef * grav(1) * d_nod(inod,i_field)
           d_nod(inod,i_res+1) = coef * grav(2) * d_nod(inod,i_field)
           d_nod(inod,i_res+2) = coef * grav(3) * d_nod(inod,i_field)
         end do
       end do
!$omp end parallel do
!
      end subroutine const_g_2_each_node
!
!  ---------------------------------------------------------------------
!
      subroutine radial_g_2_each_node(numnod, inod_smp_stack, xx,       &
     &          a_radius, coef, ncomp_nod, i_field, i_res, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) ::xx(numnod,3)
      real(kind = kreal), intent(in) ::a_radius(numnod)
!
      integer(kind = kint), intent(in) :: i_field, i_res
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
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
           d_nod(inod,i_res  ) = coef * xx(inod,1) * a_radius(inod)     &
     &                        * d_nod(inod,i_field)
           d_nod(inod,i_res+1) = coef * xx(inod,2) * a_radius(inod)     &
     &                        * d_nod(inod,i_field)
           d_nod(inod,i_res+2) = coef * xx(inod,3) * a_radius(inod)     &
     &                        * d_nod(inod,i_field)
         end do
       end do
!$omp end parallel do
!
      end subroutine radial_g_2_each_node
!
!  ---------------------------------------------------------------------
!
      subroutine self_g_2_each_node(numnod, inod_smp_stack, xx,         &
     &          coef, ncomp_nod, i_field, i_res, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) ::xx(numnod,3)
!
      integer(kind = kint), intent(in) :: i_field, i_res
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
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
           d_nod(inod,i_res  ) = coef*xx(inod,1) * d_nod(inod,i_field)
           d_nod(inod,i_res+1) = coef*xx(inod,2) * d_nod(inod,i_field)
           d_nod(inod,i_res+2) = coef*xx(inod,3) * d_nod(inod,i_field)
         end do
       end do
!$omp end parallel do
!
      end subroutine self_g_2_each_node
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_double_g_2_each_node(numnod, inod_smp_stack,     &
     &          c1, c2, grav, ncomp_nod, i_f1, i_f2, i_r1, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: i_f1, i_f2, i_r1
      real(kind = kreal), intent(in) :: c1, c2
      real(kind = kreal), intent(in) :: grav(3)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
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
           d_nod(inod,i_r1  ) = grav(1) * (c1*d_nod(inod,i_f1)          &
     &                                   + c2*d_nod(inod,i_f2) )
           d_nod(inod,i_r1+1) = grav(2) * (c1*d_nod(inod,i_f1)          &
     &                                   + c2*d_nod(inod,i_f2) )
           d_nod(inod,i_r1+2) = grav(3) * (c1*d_nod(inod,i_f1)          &
     &                                   + c2*d_nod(inod,i_f2) )
         end do
       end do
!$omp end parallel do
!
      end subroutine const_double_g_2_each_node
!
!  ---------------------------------------------------------------------
!
      subroutine radial_double_g_2_each_node(numnod, inod_smp_stack,    &
     &          xx, a_radius, c1, c2, ncomp_nod, i_f1, i_f2, i_r1,      &
     &          d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: a_radius(numnod)
!
      integer(kind = kint), intent(in) :: i_f1, i_f2, i_r1
      real(kind = kreal), intent(in) :: c1, c2
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
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
           d_nod(inod,i_r1  ) = xx(inod,1) * a_radius(inod)             &
     &                        * ( c1*d_nod(inod,i_f1)                   &
     &                          + c2*d_nod(inod,i_f2) )
           d_nod(inod,i_r1+1) = xx(inod,2) * a_radius(inod)             &
     &                        * ( c1*d_nod(inod,i_f1)                   &
     &                          + c2*d_nod(inod,i_f2) )
           d_nod(inod,i_r1+2) = xx(inod,3) * a_radius(inod)             &
     &                        * ( c1*d_nod(inod,i_f1)                   &
     &                          + c2*d_nod(inod,i_f2) )
         end do
       end do
!$omp end parallel do
!
      end subroutine radial_double_g_2_each_node
!
!  ---------------------------------------------------------------------
!
      subroutine self_double_g_2_each_node(numnod, inod_smp_stack, xx,  &
     &          c1, c2, ncomp_nod, i_f1, i_f2, i_r1, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) ::xx(numnod,3)
!
      integer(kind = kint), intent(in) :: i_f1, i_f2, i_r1
      real(kind = kreal), intent(in) :: c1, c2
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
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
           d_nod(inod,i_r1  ) = xx(inod,1) * ( c1 * d_nod(inod,i_f1)    &
     &                                       + c2 * d_nod(inod,i_f2) )
           d_nod(inod,i_r1+1) = xx(inod,2) * ( c1 * d_nod(inod,i_f1)    &
     &                                       + c2 * d_nod(inod,i_f2) )
           d_nod(inod,i_r1+2) = xx(inod,3) * ( c1 * d_nod(inod,i_f1)    &
     &                                       + c2 * d_nod(inod,i_f2) )
         end do
       end do
!$omp end parallel do
!
      end subroutine self_double_g_2_each_node
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
