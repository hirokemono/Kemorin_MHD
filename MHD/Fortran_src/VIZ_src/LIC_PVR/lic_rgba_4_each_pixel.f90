!>@file  lic_rgba_4_each_pixel.f90
!!       module lic_rgba_4_each_pixel
!!
!!@author H. Matsui
!!@date   Programmed in July. 2006
!
!> @brief Structures for parameteres for volume rendering
!!
!!@verbatim
!!      subroutine s_lic_rgba_4_each_pixel(viewpoint_vec,               &
!!     &          xin_model, xout_model, c_data, grad, o_data,          &
!!     &          color_param, rgba_pixel)
!!      subroutine lic_color_plane_with_light                           &
!!     &         (viewpoint_vec, xout_model, c_data, grad,              &
!!     &          opa_current, color_param, rgba_pixel)
!!      subroutine set_rgba_4_surface_boundary                          &
!!     &         (viewpoint_vec, xout_model, surf_normal,               &
!!     &          opa_current, color_param, rgba_pixel)
!!
!!      subroutine composite_lic_alpha_blending(rgba_src, rgba_tgt)
!!@endverbatim
!
      module lic_rgba_4_each_pixel
!
      use m_precision
      use m_constants
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_lic_rgba_4_each_pixel(viewpoint_vec,                 &
     &          xin_model, xout_model, c_data, grad, o_data,            &
     &          color_param, avr_ray_len, rgba_pixel)
!
      use t_control_params_4_pvr
      use set_color_4_pvr
      use set_rgba_4_each_pixel
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: c_data, o_data
      real(kind = kreal), intent(in) :: grad(3), avr_ray_len
      real(kind = kreal), intent(in) :: xin_model(3), xout_model(3)
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4)
!
      integer(kind = kint) :: num_of_features
      real(kind = kreal) :: color(3), ray_vec(3)
      real(kind = kreal) :: anb_opacity, opa_current, ray_length
      real(kind = kreal), allocatable :: rgb(:)
!
!
      ray_vec = xout_model - xin_model
      ray_length = sqrt(ray_vec(1)*ray_vec(1) + ray_vec(2)*ray_vec(2)   &
     &                + ray_vec(3)*ray_vec(3))
!      ray_length = sqrt((xout_model(1)-xin_model(1))**2                &
!    &                 + (xout_model(2)-xin_model(2))**2                &
!    &                 + (xout_model(3)-xin_model(3))**2)
!
      num_of_features = color_param%num_opacity_pnt
      anb_opacity = color_param%pvr_opacity_param(1,num_of_features)
!
      call compute_opacity(color_param%id_pvr_color(3), anb_opacity,    &
     &    num_of_features, color_param%pvr_opacity_param,               &
     &    o_data, opa_current)
!
      call value_to_rgb(color_param%id_pvr_color(2),                    &
     &    color_param%id_pvr_color(1), color_param%num_pvr_datamap_pnt, &
     &    color_param%pvr_datamap_param, c_data, color)
!
!
      !color(1:3) = ray_vec(1:3) / ray_length
      allocate(rgb(4))
      call phong_reflection(viewpoint_vec,                              &
     &    color_param%num_pvr_lights, color_param%xyz_pvr_lights,       &
     &    grad, color_param%pvr_lighting_real,                          &
     &    xin_model, xout_model, color, rgb(1))
!rgb(1:3) = color(1:3)
!
      !rgb(1:3) = rgb(1:3) * opa_current * ray_length
      !rgb(4) =   opa_current * ray_length
      rgb(4) = 1.0d0                                                    &
     &        - (1.0d0 - opa_current)**(ray_length / avr_ray_len)
      rgb(1:3) = rgb(1:3) * rgb(4)
      !rgb(4) =   opa_current
      if(rgb(4) .gt. one) rgb(4) = one
      if(rgb(4) .lt. zero) rgb(4) = zero
!
      call composite_lic_alpha_blending(rgb, rgba_pixel)
      deallocate(rgb)
!
      end subroutine s_lic_rgba_4_each_pixel
!
! ----------------------------------------------------------------------
!
      subroutine lic_color_plane_with_light                             &
     &         (viewpoint_vec, xout_model, c_data, grad,                &
     &          opa_current, color_param, rgba_pixel)
!
      use t_control_params_4_pvr
      use set_color_4_pvr
      use set_rgba_4_each_pixel
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: c_data, grad(3)
      real(kind = kreal), intent(in) :: xout_model(3)
      real(kind = kreal), intent(in) :: opa_current
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4)
!
      real(kind = kreal) :: color(3)
      real(kind = kreal), allocatable :: rgb(:)
!
!
      call value_to_rgb(color_param%id_pvr_color(2),                    &
     &    color_param%id_pvr_color(1), color_param%num_pvr_datamap_pnt, &
     &    color_param%pvr_datamap_param, c_data, color)
!
!
      allocate(rgb(4))
      call phong_reflection(viewpoint_vec,                              &
     &    color_param%num_pvr_lights, color_param%xyz_pvr_lights,       &
     &    grad, color_param%pvr_lighting_real,                          &
     &    xout_model, xout_model, color, rgb(1))
!
      rgb(1:3) = rgb(1:3) * opa_current
      rgb(4) =   opa_current
!
      call composite_lic_alpha_blending(rgb, rgba_pixel)
      deallocate(rgb)
!
      end subroutine lic_color_plane_with_light
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine composite_lic_alpha_blending(rgba_src, rgba_tgt)
!
      real(kind = kreal), intent(in) :: rgba_src(4)
      real(kind = kreal), intent(inout) :: rgba_tgt(4)
!
      rgba_tgt(4) = rgba_src(4) + rgba_tgt(4) * (one - rgba_src(4))
      rgba_tgt(1:3) =  rgba_src(1:3)                                    &
     &               + rgba_tgt(1:3) * (one - rgba_src(4))
!       rgba_tgt(4) = rgba_src(4) * (one - rgba_tgt(4)) + rgba_tgt(4)
!       rgba_tgt(1:3) =  rgba_src(1:3) * (one - rgba_tgt(4))              &
!      &               + rgba_tgt(1:3)
!
      end subroutine composite_lic_alpha_blending
!
! ----------------------------------------------------------------------
!
      end module lic_rgba_4_each_pixel
