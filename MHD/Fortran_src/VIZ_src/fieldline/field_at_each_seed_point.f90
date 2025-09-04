!>@file   field_at_each_seed_point.f90
!!@brief  module field_at_each_seed_point
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Evaluate field data at each seed points
!!
!!@verbatim
!!      subroutine cal_FLINE_element_size(node, ele, ele_size)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        real(kind = kreal), intent(inout) :: ele_size(ele%numele)
!!
!!      integer(kind = kint) function                                   &
!!     &      surface_mode_in_each_ele(error_level, xi_surf_start_fline)
!!        real(kind = kreal), intent(in) :: error_level
!!        real(kind = kreal), intent(in) :: xi_surf_start_fline(3)
!!
!!      subroutine cal_each_seed_field_in_ele                           &
!!     &         (node, ele, nod_fld, fline_fields, iphys_4_fline,      &
!!     &          iele_surf_start_fline, xi_surf_start_fline,           &
!!     &          xx_surf_start_fline, v_fline_start, c_fline_start)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: fline_fields
!!        integer(kind = kint), intent(in) :: iphys_4_fline
!!        integer(kind = kint), intent(in) :: iele_surf_start_fline(1)
!!        real(kind = kreal), intent(in) :: xi_surf_start_fline(3)
!!        real(kind = kreal), intent(in) :: xx_surf_start_fline(3)
!!        real(kind = kreal), intent(inout) :: v_fline_start(4)
!!        real(kind = kreal), intent(inout)                             &
!!     &         :: c_fline_start(fline_fields%ntot_color_comp)
!!@endverbatim
!
      module field_at_each_seed_point
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_phys_data
      use t_ctl_params_viz_fields
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_FLINE_element_size(node, ele, ele_size)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      real(kind = kreal), intent(inout) :: ele_size(ele%numele)
!
      real(kind = kreal) :: x(ele%nnod_4_ele)
      real(kind = kreal) :: y(ele%nnod_4_ele)
      real(kind = kreal) :: z(ele%nnod_4_ele)
      real(kind = kreal) :: size_max(3)
      integer(kind = kint) :: inod, iele, k1
!
!$omp parallel do private(iele,k1,inod,x,y,z)
      do iele = 1, ele%numele
        do k1 = 1, ele%nnod_4_ele
          inod = ele%ie(iele,k1)
          x(k1) = node%xx(inod,1)
          y(k1) = node%xx(inod,2)
          z(k1) = node%xx(inod,3)
        end do
        size_max(1) = maxval(x) - minval(x)
        size_max(2) = maxval(y) - minval(y)
        size_max(3) = maxval(z) - minval(z)
        ele_size(iele) = sqrt(size_max(1)*size_max(1)                   &
     &                      + size_max(2)*size_max(2)                   &
     &                      + size_max(3)*size_max(3))
      end do
!$omp end parallel do
!
      end subroutine cal_FLINE_element_size
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &      surface_mode_in_each_ele(error_level, xi_surf_start_fline)
!
      real(kind = kreal), intent(in) :: error_level
      real(kind = kreal), intent(in) :: xi_surf_start_fline(3)
!
      integer(kind = kint) :: i_mode
!
      i_mode = 0
      if(abs(xi_surf_start_fline(1)+one) .lt. error_level) i_mode = 1
      if(abs(xi_surf_start_fline(1)-one) .lt. error_level) i_mode = 2
      if(abs(xi_surf_start_fline(2)+one) .lt. error_level) i_mode = 3
      if(abs(xi_surf_start_fline(2)-one) .lt. error_level) i_mode = 4
      if(abs(xi_surf_start_fline(3)+one) .lt. error_level) i_mode = 5
      if(abs(xi_surf_start_fline(3)-one) .lt. error_level) i_mode = 6
!
      surface_mode_in_each_ele = i_mode
!
      end function surface_mode_in_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine cal_each_seed_field_in_ele                             &
     &         (node, ele, nod_fld, fline_fields, iphys_4_fline,        &
     &          iele_surf_start_fline, xi_surf_start_fline,             &
     &          xx_surf_start_fline, v_fline_start, c_fline_start)
!
      use sel_interpolate_scalar
      use extend_field_line
      use trace_in_element
      use tracer_field_interpolate
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      type(ctl_params_viz_fields), intent(in) :: fline_fields
      integer(kind = kint), intent(in) :: iphys_4_fline
!
      integer(kind = kint), intent(in) :: iele_surf_start_fline(1)
      real(kind = kreal), intent(in) :: xi_surf_start_fline(3)
      real(kind = kreal), intent(in) :: xx_surf_start_fline(3)
!
      real(kind = kreal), intent(inout) :: v_fline_start(4)
      real(kind = kreal), intent(inout)                                 &
     &         :: c_fline_start(fline_fields%ntot_color_comp)
!
!      real(kind = kreal) :: position_check(3)
! 
      call sel_sgl_interpolate_scalar_ele                               &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    nod_fld%d_fld(1,iphys_4_fline), iele_surf_start_fline(1),     &
     &    xi_surf_start_fline, v_fline_start(1))
      call sel_sgl_interpolate_scalar_ele                               &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    nod_fld%d_fld(1,iphys_4_fline+1), iele_surf_start_fline(1),   &
     &    xi_surf_start_fline, v_fline_start(2))
      call sel_sgl_interpolate_scalar_ele                               &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    nod_fld%d_fld(1,iphys_4_fline+2), iele_surf_start_fline(1),   &
     &    xi_surf_start_fline, v_fline_start(3))
      v_fline_start(4) = one
!
      call cal_fields_in_element(iele_surf_start_fline,                 &
     &    xi_surf_start_fline, xx_surf_start_fline,                     &
     &    ele, nod_fld, fline_fields, c_fline_start(1))
!
      end subroutine cal_each_seed_field_in_ele
!
!  ---------------------------------------------------------------------
!
      end module field_at_each_seed_point
