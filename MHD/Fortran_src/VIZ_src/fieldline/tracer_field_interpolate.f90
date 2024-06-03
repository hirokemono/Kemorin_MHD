!>@file  tracer_field_interpolate.f90
!!       module tracer_field_interpolate
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief extend field line in each domain
!!
!!@verbatim
!!      subroutine cal_fields_on_line(isurf, xi_surf, xyz_surf,         &
!!     &                              surf, nod_fld, viz_fields, c_tgt)
!!        integer(kind = kint), intent(in) :: isurf
!!        real(kind = kreal), intent(in) :: xi_surf(2)
!!        real(kind = kreal), intent(in) :: xyz_surf(3)
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: c_tgt(viz_fields%ntot_color_comp)
!!      subroutine cal_fields_in_element(iele, xi_cube, xyz, ele,       &
!!     &                                 nod_fld, viz_fields, c_tgt)
!!        integer(kind = kint), intent(in) :: iele(1)
!!        real(kind = kreal), intent(in) :: xi_cube(3)
!!        real(kind = kreal), intent(in) :: xyz(3)
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: c_tgt(viz_fields%ntot_color_comp)
!!@endverbatim
!
      module tracer_field_interpolate
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
!
      use t_geometry_data
      use t_surface_data
      use t_phys_data
      use t_ctl_params_viz_fields
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_fields_on_line(isurf, xi_surf, xyz_surf,           &
     &                              surf, nod_fld, viz_fields, c_tgt)
!
      use coordinate_converter
      use convert_components_4_viz
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: isurf
      real(kind = kreal), intent(in) :: xi_surf(2)
      real(kind = kreal), intent(in) :: xyz_surf(3)
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_tgt(viz_fields%ntot_color_comp)
!
      real(kind = kreal) :: r(1), theta(1), phi(1)
      real(kind = kreal) :: a_r(1), rs(1), a_rs(1)
      real(kind = kreal) :: c_xyz(9)
      integer(kind = kint) :: istack_single(0:1) = (/0, 1/)
!
      integer(kind = kint) :: inum, ifield, ist, jst, nd
!
      call position_2_sph(ione, xyz_surf(1), r, theta, phi,             &
     &                    a_r, rs, a_rs )
      do inum = 1, viz_fields%num_color_fields
        ifield = viz_fields%ifleld_color_field(inum)
        if(ifield .le. 0) then
          jst = viz_fields%istack_color_field(inum-1)
          c_tgt(jst+1) = zero
        else
          ist = nod_fld%istack_component(ifield-1)
          jst = viz_fields%istack_color_field(inum-1)
          do nd = 1, viz_fields%ncomp_org_color_field(inum)
            call cal_field_on_surf_scalar(nod_fld%n_point,              &
     &          surf%numsurf, surf%nnod_4_surf, surf%ie_surf,           &
     &          isurf, xi_surf, nod_fld%d_fld(1,ist+nd), c_xyz(nd))
          end do
          call convert_comps_4_viz                                      &
     &       (ione, istack_single, xyz_surf(1), r, a_r, rs, a_rs,       &
     &        viz_fields%ncomp_color_field(inum),                       &
     &        viz_fields%ncomp_org_color_field(inum),                   &
     &        viz_fields%icomp_color_field(inum),                       &
     &         c_xyz(1), c_tgt(jst+1))
        end if
      end do
!
      end subroutine cal_fields_on_line
!
!  ---------------------------------------------------------------------
!
      subroutine cal_fields_in_element(iele, xi_cube, xyz, ele,         &
     &                                 nod_fld, viz_fields, c_tgt)
!
      use coordinate_converter
      use convert_components_4_viz
      use sel_interpolate_scalar
!
      integer(kind = kint), intent(in) :: iele(1)
      real(kind = kreal), intent(in) :: xi_cube(3)
      real(kind = kreal), intent(in) :: xyz(3)
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(ctl_params_viz_fields), intent(in) :: viz_fields
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: c_tgt(viz_fields%ntot_color_comp)
!
      real(kind = kreal) :: r(1), theta(1), phi(1)
      real(kind = kreal) :: a_r(1), rs(1), a_rs(1)
      real(kind = kreal) :: c_xyz(9)
      integer(kind = kint), parameter                                   &
     &                   :: istack_tbl_wtype_smp(0:4) = (/0,0,0,0,1/)
      integer(kind = kint), parameter :: istack_single(0:1) = (/0,1/)
!
      integer(kind = kint) :: inum, ifield, ist, jst, nd
!
      call position_2_sph(ione, xyz(1), r, theta, phi,                  &
     &                    a_r, rs, a_rs )
      do inum = 1, viz_fields%num_color_fields
        ifield = viz_fields%ifleld_color_field(inum)
        if(ifield .le. 0) then
          jst = viz_fields%istack_color_field(inum-1)
          c_tgt(jst+1) = zero
        else
          ist = nod_fld%istack_component(ifield-1)
          jst = viz_fields%istack_color_field(inum-1)
          do nd = 1, viz_fields%ncomp_org_color_field(inum)
            call s_sel_interpolate_scalar_ele                           &
     &         (1, nod_fld%n_point, ele%numele, ele%nnod_4_ele, ele%ie, &
     &          nod_fld%d_fld(1,ist+nd), istack_single, ione,           &
     &          iele, xi_cube, c_xyz(nd))
          end do
          call convert_comps_4_viz                                      &
     &       (ione, istack_single, xyz(1), r, a_r, rs, a_rs,            &
     &        viz_fields%ncomp_color_field(inum),                       &
     &        viz_fields%ncomp_org_color_field(inum),                   &
     &        viz_fields%icomp_color_field(inum),                       &
     &         c_xyz(1), c_tgt(jst+1))
        end if
      end do
!
      end subroutine cal_fields_in_element
!
!  ---------------------------------------------------------------------
!
      end module tracer_field_interpolate

