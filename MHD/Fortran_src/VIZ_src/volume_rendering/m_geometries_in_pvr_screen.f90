!m_geometries_in_pvr_screen.f90
!      module m_geometries_in_pvr_screen
!
!        programmed by H.Matsui on Aug., 2011
!
!      subroutine cal_field_4_pvr(numnod, numele, nnod_4_ele,           &
!     &          inod_smp_stack, iele_smp_stack, xx, radius,            &
!     &          a_radius, s_cylinder, a_s_cylinder, ie, a_vol_ele,     &
!     &          ntot_int_3d, dnx, xjac, num_nod_phys, num_tot_nod_phys,&
!     &          istack_nod_component, d_nod)
!
      module m_geometries_in_pvr_screen
!
      use m_precision
!
      use m_constants
      use m_control_params_4_pvr
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_field_4_pvr(numnod, numele, nnod_4_ele,            &
     &          inod_smp_stack, iele_smp_stack, xx, radius,             &
     &          a_radius, s_cylinder, a_s_cylinder, ie, a_vol_ele,      &
     &          ntot_int_3d, dnx, xjac, num_nod_phys, num_tot_nod_phys, &
     &          istack_nod_component, d_nod, proj)
!
      use t_geometries_in_pvr_screen
      use cal_gradient_on_element
      use convert_components_4_viz
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      real(kind = kreal), intent(in) :: a_vol_ele(numele)
!
      integer (kind = kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx(numele,nnod_4_ele,ntot_int_3d,3)
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_component(0:num_nod_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,num_tot_nod_phys)
!
      type(pvr_projected_type), intent(in) :: proj
!
!
      integer(kind = kint) :: i_pvr
      integer(kind = kint) :: i_field, ist_fld, num_comp
!
!
      do i_pvr = 1, num_pvr
        i_field = id_pvr_output(i_pvr)
        ist_fld = istack_nod_component(i_field-1)
        num_comp = istack_nod_component(i_field) - ist_fld
        call convert_comps_4_viz(numnod, inod_smp_stack, xx, radius,    &
     &      a_radius, s_cylinder, a_s_cylinder, ione,                   &
     &      num_comp, icomp_pvr_output(i_pvr), d_nod(1,ist_fld+1),      &
     &      proj%field_pvr(i_pvr)%d_pvr)
!
        call fem_gradient_on_element(iele_smp_stack,                    &
     &          numnod, numele, nnod_4_ele, ie, a_vol_ele,              &
     &          ntot_int_3d, ione, dnx, xjac,                           &
     &     proj%field_pvr(i_pvr)%grad_ele, proj%field_pvr(i_pvr)%d_pvr)
      end do
!
      end subroutine cal_field_4_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine set_pixel_on_pvr_screen(i_pvr, pixel_xy)
!
      use t_geometries_in_pvr_screen
      use set_projection_matrix
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
!
      pixel_xy%num_pixel_x = n_pvr_pixel(1,i_pvr)
      pixel_xy%num_pixel_y = n_pvr_pixel(2,i_pvr)
      call allocate_pixel_position_pvr(pixel_xy)
!
      call set_pixel_points_on_project(n_pvr_pixel(1,i_pvr),            &
          n_pvr_pixel(2,i_pvr), pixel_xy%pixel_point_x,                 &
     &    pixel_xy%pixel_point_y)
!
      end subroutine set_pixel_on_pvr_screen
!
!  ---------------------------------------------------------------------
!
      end module m_geometries_in_pvr_screen
