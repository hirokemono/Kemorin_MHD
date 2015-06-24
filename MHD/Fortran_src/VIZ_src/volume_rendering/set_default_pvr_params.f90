!set_default_pvr_params.f90
!      module set_default_pvr_params
!
!        programmed by H.Matsui on May. 2009
!
!      subroutine check_pvr_parameters(i_pvr)
!!      subroutine set_default_pvr_data_params                          &
!!     &         (d_minmax_pvr, color_param)
!
      module set_default_pvr_params
!
      use m_precision
!
      use m_constants
      use t_control_params_4_pvr
!
      implicit none
!
      private :: set_default_viewpoint_pvr
      private :: set_default_lookatpoint_pvr
      private :: set_default_up_dir_pvr, set_default_light_pvr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_pvr_parameters(i_pvr, view_param)
!
      use m_control_params_4_pvr
      use m_mesh_outline_pvr
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_view_parameter), intent(inout) :: view_param
!
!
      if (view_param%iflag_viewpoint .eq. 0) then
        call set_default_viewpoint_pvr(center_g(1,i_pvr),               &
     &      xx_minmax_g(1,1,i_pvr), view_param%viewpoint_vec)
      end if
!
      if (view_param%iflag_lookpoint .eq. 0) then
        call set_default_lookatpoint_pvr                                &
     &     (center_g(1,i_pvr), view_param%lookat_vec)
      end if
!
      if (view_param%iflag_updir .eq. 0) then
        call set_default_up_dir_pvr(view_param%up_direction_vec)
      end if
!
      if (color_params(i_pvr)%iflag_pvr_lights .eq. 0) then
        call set_default_light_pvr                                     &
     &     (center_g(1,i_pvr), xx_minmax_g(1,1,i_pvr), color_params(i_pvr))
      end if
!
      end subroutine check_pvr_parameters
!
! -----------------------------------------------------------------------
!
      subroutine set_default_viewpoint_pvr                              &
     &         (center_g, xx_minmax_g, viewpoint_vec)
!
      real(kind = kreal), intent(in) :: center_g(3)
      real(kind = kreal), intent(in) :: xx_minmax_g(2,3)
      real(kind = kreal), intent(inout) :: viewpoint_vec(3)
!
!
      viewpoint_vec(1) = center_g(1)
      viewpoint_vec(2) = xx_minmax_g(2,2) + 1.5d0 * ( xx_minmax_g(2,2)  &
     &                                              - xx_minmax_g(1,2))
      viewpoint_vec(3) = xx_minmax_g(2,3) + 1.5d0 * ( xx_minmax_g(2,3)  &
     &                                              - xx_minmax_g(1,3))
!
      end subroutine set_default_viewpoint_pvr
!
! -----------------------------------------------------------------------
!
      subroutine set_default_lookatpoint_pvr(center_g, lookat_vec)
!
      real(kind = kreal), intent(in) :: center_g(3)
      real(kind = kreal), intent(inout) :: lookat_vec(3)
!
!
      lookat_vec(1:3) = center_g(1:3)
!
      end subroutine set_default_lookatpoint_pvr
!
! -----------------------------------------------------------------------
!
      subroutine set_default_up_dir_pvr(up_direction_vec)
!
      real(kind = kreal), intent(inout) :: up_direction_vec(3)
!
!
      up_direction_vec(1:3) = zero
      up_direction_vec(2) =   one
!
      end subroutine set_default_up_dir_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_default_light_pvr                                  &
     &         (center_g, xx_minmax_g, color_param)
!
      use t_control_params_4_pvr
!
      real(kind = kreal), intent(in) :: xx_minmax_g(2,3)
      real(kind = kreal), intent(in) :: center_g(3)
      type(pvr_colormap_parameter), intent(inout) :: color_param
!
!
      color_param%num_pvr_lights = 1
      color_param%xyz_pvr_lights(1,1) = center_g(1)
      color_param%xyz_pvr_lights(2,1) = xx_minmax_g(2,2)                &
     &                        + 0.1d0 * ( xx_minmax_g(2,2)              &
     &                                  - xx_minmax_g(1,2) )
      color_param%xyz_pvr_lights(3,1) = xx_minmax_g(2,3)                &
     &                        + 2.0d0 * ( xx_minmax_g(2,3)              &
     &                                  - xx_minmax_g(1,3) )
!
      end subroutine set_default_light_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_default_pvr_data_params                            &
     &         (d_minmax_pvr, color_param)
!
      use t_control_params_4_pvr
      use set_color_4_pvr
!
      real(kind = kreal), intent(in) :: d_minmax_pvr(2)
      type(pvr_colormap_parameter), intent(inout) :: color_param
!
!
      if(color_param%id_pvr_color(2) .eq. iflag_automatic) then
        color_param%pvr_datamap_param(1,1) = d_minmax_pvr(1)
        color_param%pvr_datamap_param(1,2) = d_minmax_pvr(2)
        color_param%pvr_datamap_param(2,1) = zero
        color_param%pvr_datamap_param(2,2) = one
      end if
!
      end subroutine set_default_pvr_data_params
!
! -----------------------------------------------------------------------
!
      end module set_default_pvr_params
