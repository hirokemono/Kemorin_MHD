!set_default_pvr_params.f90
!      module set_default_pvr_params
!
!        programmed by H.Matsui on May. 2009
!
!      subroutine check_pvr_parameters(i_pvr)
!      subroutine set_default_pvr_data_params(i_pvr)
!
      module set_default_pvr_params
!
      use m_precision
!
      use m_constants
      use m_control_params_4_pvr
      use m_mesh_outline_pvr
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
      subroutine check_pvr_parameters(i_pvr)
!
      integer(kind = kint), intent(in) :: i_pvr
!
!
      if (iflag_viewpoint_vec(i_pvr) .eq. 0) then
        call set_default_viewpoint_pvr(i_pvr)
      end if
!
      if (iflag_lookpoint_vec(i_pvr) .eq. 0) then
        call set_default_lookatpoint_pvr(i_pvr)
      end if
!
      if (iflag_updir_vec(i_pvr) .eq. 0) then
        call set_default_up_dir_pvr(i_pvr)
      end if
!
      if (iflag_pvr_lights(i_pvr) .eq. 0) then
        call set_default_light_pvr(i_pvr)
      end if
!
      end subroutine check_pvr_parameters
!
! -----------------------------------------------------------------------
!
      subroutine set_default_viewpoint_pvr(i_pvr)
!
      integer(kind = kint), intent(in) :: i_pvr
!
!
      viewpoint_vec(1,i_pvr) = center_g(1,i_pvr)
      viewpoint_vec(2,i_pvr) = xx_minmax_g(2,2,i_pvr)                   &
     &                        + 1.5d0 * ( xx_minmax_g(2,2,i_pvr)        &
     &                                  - xx_minmax_g(1,2,i_pvr) )
      viewpoint_vec(3,i_pvr) = xx_minmax_g(2,3,i_pvr)                   &
     &                        + 1.5d0 * ( xx_minmax_g(2,3,i_pvr)        &
     &                                  - xx_minmax_g(1,3,i_pvr) )
!
      end subroutine set_default_viewpoint_pvr
!
! -----------------------------------------------------------------------
!
      subroutine set_default_lookatpoint_pvr(i_pvr)
!
      integer(kind = kint), intent(in) :: i_pvr
!
!
      lookat_vec(1:3,i_pvr) = center_g(1:3,i_pvr)
!
      end subroutine set_default_lookatpoint_pvr
!
! -----------------------------------------------------------------------
!
      subroutine set_default_up_dir_pvr(i_pvr)
!
      integer(kind = kint), intent(in) :: i_pvr
!
!
      up_direction_vec(1,i_pvr) = zero
      up_direction_vec(2,i_pvr) = one
      up_direction_vec(3,i_pvr) = zero
!
      end subroutine set_default_up_dir_pvr
!
! -----------------------------------------------------------------------
!
      subroutine set_default_light_pvr(i_pvr)
!
      integer(kind = kint), intent(in) :: i_pvr
!
!
      xyz_pvr_lights(1,i_pvr) = center_g(1,i_pvr)
      xyz_pvr_lights(2,i_pvr) = xx_minmax_g(2,2,i_pvr)                  &
     &                        + 0.1d0 * ( xx_minmax_g(2,2,i_pvr)        &
     &                                  - xx_minmax_g(1,2,i_pvr) )
      xyz_pvr_lights(3,i_pvr) = xx_minmax_g(2,3,i_pvr)                  &
     &                        + 2.0d0 * ( xx_minmax_g(2,3,i_pvr)        &
     &                                  - xx_minmax_g(1,3,i_pvr) )
!
      end subroutine set_default_light_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_default_pvr_data_params(i_pvr)
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: ist
!
!
      if (id_pvr_color(1,i_pvr) .eq. 1) then
        ist = istack_pvr_datamap_pnt(i_pvr-1)
        pvr_datamap_param(1,ist+1) = d_minmax_pvr(1,i_pvr)
        pvr_datamap_param(1,ist+2) = d_minmax_pvr(2,i_pvr)
        pvr_datamap_param(2,ist+1) = zero
        pvr_datamap_param(2,ist+2) = one
      end if
!
      end subroutine set_default_pvr_data_params
!
! -----------------------------------------------------------------------
!
      end module set_default_pvr_params
