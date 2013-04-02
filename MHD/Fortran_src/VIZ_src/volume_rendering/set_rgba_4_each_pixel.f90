!
!      module set_rgba_4_each_pixel
!
      module set_rgba_4_each_pixel
!
!      Written by H. Matsui on July, 2006
!
      use m_precision
      use m_constants
!
      implicit  none
!
      real(kind = kreal), parameter :: EPSILON = 1.0d-9
      private :: EPSILON
!
!
      private :: rendering_with_light
!
!      subroutine s_set_rgba_4_each_pixel(i_pvr, xin_model, xout_model, &
!     &          c_data, grad_data, rgba)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_rgba_4_each_pixel(i_pvr, xin_model, xout_model,  &
     &          c_data, grad_data, rgba_pixel)
!
      use m_control_params_4_pvr
      use m_geometries_in_pvr_screen
      use set_color_4_pvr
!
      integer(kind = kint), intent(in) :: i_pvr
      real(kind = kreal), intent(in) :: c_data, grad_data(3)
      real(kind = kreal), intent(in) :: xin_model(3), xout_model(3)
      real(kind = kreal), intent(inout) :: rgba_pixel(4)
!
      integer(kind = kint) :: ist_dmap, ist_dopt
      integer(kind = kint) :: num_of_features, ist_lgt
      real(kind = kreal) :: color(3)
      real(kind = kreal) :: anb_opacity, opa_current
!
!
      ist_dopt =  istack_opacity_pnt(i_pvr-1)
      ist_dmap =  istack_pvr_datamap_pnt(i_pvr-1)
      ist_lgt =   istack_pvr_lights(i_pvr-1)
      num_of_features = num_opacity_pnt(i_pvr)
      anb_opacity = pvr_opacity_param(1,ist_dopt+num_of_features)
!
      call compute_opacity(id_pvr_color(3,i_pvr), anb_opacity,          &
     &    num_of_features, pvr_opacity_param(1,ist_dopt+1),             &
     &    c_data, opa_current)
!
      call value_to_rgb(id_pvr_color(2,i_pvr), id_pvr_color(1,i_pvr),   &
     &    num_pvr_datamap_pnt(i_pvr), pvr_datamap_param(1,ist_dmap+1),  &
     &    c_data, color)
!
      call rendering_with_light(viewpoint_vec(1,i_pvr),                 &
     &    num_pvr_lights(i_pvr), xyz_pvr_lights(1,ist_lgt+1),           &
     &    grad_data, pvr_lighting_real(1,i_pvr),                        &
     &    xin_model, xout_model, one, color, opa_current, rgba_pixel )
!
      end subroutine s_set_rgba_4_each_pixel
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine compute_opacity(transfer_function_style, opa_value,    &
     &          num_of_features, fea_point, value, opacity_local)
!
      integer(kind = kint), intent(in) :: transfer_function_style
      real(kind = kreal), intent(in) :: opa_value
      integer(kind = kint), intent(in) :: num_of_features
      real(kind = kreal), intent(in) :: fea_point(3,num_of_features)
      real(kind = kreal), intent(in) :: value
!
      real(kind = kreal), intent(out) :: opacity_local
!
      integer(kind = kint) :: i, j, min_type
      real(kind = kreal) ::  mint, t, grad_norm
!
!
      opacity_local = zero
      if     (transfer_function_style .eq. 1) then
        opacity_local = opa_value
      else if(transfer_function_style .eq. 3) then
        mint = 1.0d-17
        do i = 1, num_of_features
          t = abs(value - fea_point(1,i))
          if(t .lt. mint) then
            mint = t
            min_type = i
          end if
          if(mint .lt. fea_point(2,min_type)) then
            opacity_local = opa_value + fea_point(3,min_type)          &
     &                                 * (fea_point(2,min_type)-mint)  &
     &                                 / fea_point(2,min_type)
          else
            opacity_local = opa_value
          end if
        end do
!
      else if(transfer_function_style .eq. 4) then
        opacity_local = opa_value
        do i = 1, num_of_features
          if(value.ge.fea_point(1,i)                                    &
     &         .and. value.le.fea_point(2,i)) then
            opacity_local = fea_point(3,i)
          end if
        end do
!
      else if(transfer_function_style .eq. 5) then
        opacity_local = opa_value
        do i = 1, num_of_features-1
          if(value.ge.fea_point(1,i)                                    &
     &         .and. value.le.fea_point(1,i+1)) then
            opacity_local = fea_point(3,i)                              &
     &                     + (fea_point(3,i+1) - fea_point(3,i))        &
     &                     * (value - fea_point(1,i)-mint)              &
     &                      / (fea_point(1,i+1) - fea_point(1,i))
          end if
        end do
      end if
!
      end subroutine compute_opacity
!
! ----------------------------------------------------------------------
!
      subroutine rendering_with_light(view_point_d,                     &
     &          num_of_lights, light_point, norm_v, k_ads,              &
     &          in_point, out_point, tav_length,                        &
     &          color, opa_current, accum_rgba)
!
      real(kind = kreal), intent(in) :: view_point_d(3)
      real(kind = kreal), intent(in) :: k_ads(3)
      real(kind = kreal), intent(in) :: norm_v(3)
      integer(kind = kint), intent(in) :: num_of_lights
      real(kind = kreal), intent(in) :: light_point(3,num_of_lights)
!
      real(kind = kreal), intent(in) :: in_point(3)
      real(kind = kreal), intent(in) :: out_point(3)
      real(kind = kreal), intent(in) :: tav_length
!
      real(kind = kreal), intent(in) :: opa_current
      real(kind = kreal), intent(in) :: color(3)
!
      real(kind = kreal), intent(inout) :: accum_rgba(4)
!
      integer(kind = kint) :: i, j
      real(kind = kreal) :: cosalpha, costheta
      real(kind = kreal) :: lp(3), vp(3), hp(3)
      real(kind = kreal) :: lp_norm, vp_norm, hp_norm, norm
      real(kind = kreal) :: inprodLN, inprodVN, inprodHN
      real(kind = kreal) :: length, coff_i
      real(kind = kreal) :: rgb(4), coef, vo(3), x_mid(3)
!
!
      rgb(1:3) = zero
      x_mid(1:3) = half*(out_point(1:3) +  in_point(1:3))
      vo(1:3) = view_point_d(1:3) - norm_v(1:3)
      length = sqrt( (out_point(1)-in_point(1))**2                      &
     &             + (out_point(2)-in_point(2))**2                      &
     &             + (out_point(3)-in_point(3))**2 )
      coff_i = (length / tav_length)
!
      do j = 1, num_of_lights
        lp(1:3) = light_point(1:3,j) - x_mid(1:3)
        vp(1:3) = - x_mid(1:3)
        hp(1:3) = (lp(1:3) + vp(1:3)) / two
!
        lp_norm = sqrt( lp(1)*lp(1) + lp(2)*lp(2) + lp(3)*lp(3) )
        vp_norm = sqrt( vp(1)*vp(1) + vp(2)*vp(2) + vp(3)*vp(3) )
        hp_norm = sqrt( hp(1)*hp(1) + hp(2)*hp(2) + hp(3)*hp(3) )
        norm =    sqrt( vo(1)*vo(1) + vo(2)*vo(2) + vo(3)*vo(3) )
!
        if(abs(lp_norm) .gt. EPSILON) lp(1:3) = lp(1:3) / lp_norm
        if(abs(vp_norm) .gt. EPSILON) vp(1:3) = vp(1:3) / vp_norm
        if(abs(hp_norm) .gt. EPSILON) hp(1:3) = hp(1:3) / hp_norm
        if(abs(norm) .gt.    EPSILON) vo(1:3) = vo(1:3) / norm
!
        inprodLN = vo(1)*lp(1) + vo(2)*lp(2) + vo(3)*lp(3)
        inprodVN = vo(1)*vp(1) + vo(2)*vp(2) + vo(3)*vp(3)
        inprodHN = vo(1)*hp(1) + vo(2)*hp(2) + vo(3)*hp(3)
!
        cosalpha = inprodLN
        costheta = inprodLN*inprodVN                                    &
     &            - sqrt(one-inprodLN*inprodLN)                         &
     &             *sqrt(one-inprodVN*inprodVN)
        cosalpha = abs(cosalpha)
!
        if(cosalpha .gt. zero) then
          coef = coff_i * (k_ads(1) + k_ads(2)*cosalpha                 &
     &                   + k_ads(3)*costheta**6)
        else
          coef = coff_i * k_ads(1)
        end if
        rgb(1:3) = rgb(1:3) + color(1:3) * coef
      end do
!
!
      rgb(1:3) = rgb(1:3) * opa_current
      rgb(4) =   opa_current*coff_i
      call alpha_blending(rgb, accum_rgba)
!
      end subroutine rendering_with_light
!
! ----------------------------------------------------------------------
!
      subroutine alpha_blending(rgba_src, rgba_tgt)
!
      real(kind = kreal), intent(in) :: rgba_src(4)
      real(kind = kreal), intent(inout) :: rgba_tgt(4)
!
!
      rgba_tgt(1:3) = rgba_tgt(1:3)                                     &
     &                   + rgba_src(1:3) * (one - rgba_tgt(4))
      rgba_tgt(4) = rgba_tgt(4)                                         &
     &                   + rgba_src(4) * (one - rgba_tgt(4))
      rgba_tgt(4) = min(one,rgba_tgt(4))
!
      end subroutine alpha_blending
!
! ----------------------------------------------------------------------
!
!      subroutine sum_blending
!
!
!      end subroutine sum_blending
!
! ----------------------------------------------------------------------
!
      end module set_rgba_4_each_pixel
