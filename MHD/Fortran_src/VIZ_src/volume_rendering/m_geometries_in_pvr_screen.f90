!m_geometries_in_pvr_screen.f90
!      module m_geometries_in_pvr_screen
!
      module m_geometries_in_pvr_screen
!
!        programmed by H.Matsui on Aug., 2011
!
!      subroutine cal_field_4_pvr(numnod, numele, nnod_4_ele,           &
!     &          inod_smp_stack, iele_smp_stack, xx, radius,            &
!     &          a_radius, s_cylinder, a_s_cylinder, ie, a_vol_ele,     &
!     &          ntot_int_3d, dnx, xjac, num_nod_phys, num_tot_nod_phys,&
!     &          istack_nod_component, d_nod)
!
      use m_precision
!
      use m_constants
      use m_control_params_4_pvr
!
      implicit  none
!
!
      integer(kind = kint) :: nnod_pvr, nele_pvr
      integer(kind = kint), allocatable :: istack_nod_pvr(:)
      real(kind = kreal), allocatable :: x_nod_sim(:,:)
      real(kind = kreal), allocatable :: x_nod_model(:,:)
      real(kind = kreal), allocatable :: x_nod_screen(:,:)
!
      real(kind = kreal), allocatable :: d_nod_pvr(:,:)
      real(kind = kreal), allocatable :: grad_ele_pvr(:,:,:)
!
      integer(kind = kint) :: ntot_pixel_x, ntot_pixel_y
      integer(kind = kint), allocatable :: istack_pixel_x(:)
      integer(kind = kint), allocatable :: istack_pixel_y(:)
      real(kind = kreal), allocatable :: pixel_point_x(:)
      real(kind = kreal), allocatable :: pixel_point_y(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_node_position_pvr
!
      use m_machine_parameter
!
!
      allocate(istack_nod_pvr(0:np_smp))
      allocate(x_nod_sim(nnod_pvr,4))
      allocate(x_nod_model(nnod_pvr,4))
      allocate(x_nod_screen(nnod_pvr,4))
      x_nod_sim =   0.0d0
      x_nod_model =  0.0d0
      x_nod_screen = 0.0d0
!
      end subroutine allocate_node_position_pvr
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_data_4_pvr
!
      allocate(d_nod_pvr(nnod_pvr,num_pvr))
      allocate(grad_ele_pvr(nele_pvr,3,num_pvr))
      d_nod_pvr =    0.0d0
      grad_ele_pvr = 0.0d0
!
      end subroutine allocate_nod_data_4_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_num_pixel_pvr
!
      allocate(istack_pixel_x(0:num_pvr))
      allocate(istack_pixel_y(0:num_pvr))
      istack_pixel_x =  0
      istack_pixel_y =  0
!
      end subroutine allocate_num_pixel_pvr
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pixel_position_pvr
!
      allocate(pixel_point_x(ntot_pixel_x))
      allocate(pixel_point_y(ntot_pixel_y))
      pixel_point_x =  0.0d0
      pixel_point_y =  0.0d0
!
      end subroutine allocate_pixel_position_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_node_position_pvr
!
      deallocate(istack_nod_pvr)
      deallocate(x_nod_sim, x_nod_model, x_nod_screen)
!
      end subroutine deallocate_node_position_pvr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nod_data_4_pvr
!
      deallocate(d_nod_pvr, grad_ele_pvr)
!
      end subroutine deallocate_nod_data_4_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pixel_position_pvr
!
      deallocate(istack_pixel_x, istack_pixel_y)
      deallocate(pixel_point_x, pixel_point_y)
!
      end subroutine deallocate_pixel_position_pvr
!
! -----------------------------------------------------------------------
!
      subroutine cal_field_4_pvr(numnod, numele, nnod_4_ele,            &
     &          inod_smp_stack, iele_smp_stack, xx, radius,             &
     &          a_radius, s_cylinder, a_s_cylinder, ie, a_vol_ele,      &
     &          ntot_int_3d, dnx, xjac, num_nod_phys, num_tot_nod_phys, &
     &          istack_nod_component, d_nod)
!
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
     &      d_nod_pvr(1,i_pvr) )
!
        call fem_gradient_on_element(iele_smp_stack,                    &
     &          numnod, numele, nnod_4_ele, ie, a_vol_ele,              &
     &          ntot_int_3d, itwo, dnx, xjac,                           &
     &          grad_ele_pvr(1,1,i_pvr), d_nod_pvr(1,i_pvr))
      end do
!
      end subroutine cal_field_4_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine set_pixel_on_pvr_screen
!
      use set_projection_matrix
!
      integer(kind = kint) :: i_pvr, ist, jst
!
!
      call allocate_num_pixel_pvr
!
      do i_pvr = 1, num_pvr
        istack_pixel_x(i_pvr) = istack_pixel_x(i_pvr-1)                 &
     &                            + n_pvr_pixel(1,i_pvr)
        istack_pixel_y(i_pvr) = istack_pixel_y(i_pvr-1)                 &
     &                            + n_pvr_pixel(2,i_pvr)
      end do
      ntot_pixel_x = istack_pixel_x(num_pvr)
      ntot_pixel_y = istack_pixel_y(num_pvr)
!
      call allocate_pixel_position_pvr
!
      do i_pvr = 1, num_pvr
        ist = istack_pixel_x(i_pvr-1) + 1
        jst = istack_pixel_y(i_pvr-1) + 1
        call set_pixel_points_on_project(n_pvr_pixel(1,i_pvr),          &
            n_pvr_pixel(2,i_pvr), pixel_point_x(ist),                   &
     &      pixel_point_y(jst))
      end do
!
      end subroutine set_pixel_on_pvr_screen
!
!  ---------------------------------------------------------------------
!
      end module m_geometries_in_pvr_screen
