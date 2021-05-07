!>@file   t_control_param_pvr_isosurf.f90
!!@brief  module t_control_param_pvr_isosurf
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set each PVR parameters from control
!!
!!@verbatim
!!      subroutine check_pvr_field_control                              &
!!     &         (pvr_ctl, num_nod_phys, phys_nod_name)
!!
!!      subroutine set_control_field_4_pvr(field_ctl, comp_ctl,         &
!!     &          num_nod_phys, phys_nod_name, fld_param, icheck_ncomp)
!!      subroutine set_control_pvr(pvr_ctl, ele_grp, surf_grp, pvr_area,&
!!     &          view_param, draw_param, color_param, cbar_param)
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!!        type(pvr_field_parameter), intent(inout) :: fld_param
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!        type(rendering_parameter), intent(inout) :: draw_param
!!        type(viz_area_parameter), intent(inout) :: pvr_area
!!        type(pvr_colormap_parameter), intent(inout) :: color_param
!!        type(pvr_colorbar_parameter), intent(inout) :: cbar_param
!!      subroutine set_control_pvr_movie(movie, view_param)
!!        type(pvr_movie_ctl), intent(in) :: movie
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!      subroutine set_pvr_stereo_control(pvr_ctl, view_param)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!@endverbatim
!
      module t_control_param_pvr_isosurf
!
      use m_precision
!
      use m_constants
      use m_error_IDs
      use t_control_data_4_pvr
      use calypso_mpi
!
      use set_field_comp_for_viz
      use output_image_sel_4_png
!
      implicit  none
!
!>  Structure for field data on projected coordinate
      type pvr_isosurf_parameter
!>        Number of isosurfaces
        integer(kind = kint) :: num_isosurf
!>        Number of isosurfaces
        integer(kind = kint), allocatable :: itype_isosurf(:)
!>        field value for isosurfaces
        real(kind = kreal), allocatable :: iso_value(:)
!>        Opacity value for isosurfaces
        real(kind = kreal), allocatable :: iso_opacity(:)
!
!>        field paramters for isosurface
        type(pvr_field_parameter), allocatable :: iso_fld_param(:)
!>        field paramters for isosurface
        integer(kind = kint), allocatable :: icheck_iso_ncomp(:)
      end type pvr_isosurf_parameter
!
      private :: set_control_pvr_render_area, set_control_pvr_isosurfs
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurf_params(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      allocate(draw_param%itype_isosurf(draw_param%num_isosurf))
      allocate(draw_param%iso_value(draw_param%num_isosurf))
      allocate(draw_param%iso_opacity(draw_param%num_isosurf))
      allocate(draw_param%iso_fld_param(draw_param%num_isosurf))
      allocate(draw_param%icheck_iso_ncomp(draw_param%num_isosurf))
!
      if(draw_param%num_isosurf .gt. 0) draw_param%icheck_iso_ncomp = 0
      if(draw_param%num_isosurf .gt. 0) draw_param%itype_isosurf = 0
      if(draw_param%num_isosurf .gt. 0) draw_param%iso_value = zero
      if(draw_param%num_isosurf .gt. 0) draw_param%iso_opacity = zero
!
      end subroutine alloc_pvr_isosurf_params
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurf_params(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      deallocate(draw_param%itype_isosurf)
      deallocate(draw_param%iso_value, draw_param%iso_opacity)
      deallocate(draw_param%iso_fld_param, draw_param%icheck_iso_ncomp)
!
      end subroutine dealloc_pvr_isosurf_params
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_pvr_isosurfs(pvr_isos_c, draw_param)
!
      use t_control_data_pvr_isosurfs
      use t_geometries_in_pvr_screen
      use pvr_surface_enhancement
!
      type(pvr_isosurfs_ctl), intent(in) :: pvr_isos_c
!
      type(rendering_parameter), intent(inout) :: draw_param
!
      integer(kind = kint) ::  i
      character(len = kchara) :: tmpchara
!
      integer(kind = kint) :: iflag
!
!
      draw_param%num_isosurf = pvr_isos_c%num_pvr_iso_ctl
      if(draw_param%num_isosurf .le. 0) return
!
      call alloc_pvr_isosurfaces(draw_param)
!
      do i = 1, draw_param%num_isosurf
        iflag = pvr_isos_c%pvr_iso_ctl(i)%isosurf_data_ctl%iflag
        if(iflag .le. 0) then
          pvr_isos_c%pvr_iso_ctl(i)%isosurf_data_ctl%charavalue         &
     &                                   = pvr_field_ctl%charavalue
          pvr_isos_c%pvr_iso_ctl(i)%isosurf_data_ctl%iflag = 1
        end if
        iflag = pvr_isos_c%pvr_iso_ctl(i)%isosurf_comp_ctl%iflag
        if(iflag .le. 0) then
          pvr_isos_c%pvr_iso_ctl(i)%isosurf_data_ctl%charavalue         &
     &                                   = mag_flags(1)
          pvr_isos_c%pvr_iso_ctl(i)%isosurf_data_ctl%iflag = 1
        end if
        call set_control_field_4_pvr                                    &
     &     (pvr_isos_c%pvr_iso_ctl(i)%isosurf_data_ctl,                 &
     &      pvr_isos_c%pvr_iso_ctl(i)%isosurf_comp_ctl,                 &
     &      num_nod_phys, phys_nod_name,                                &
     &      iso_fld_param(i), icheck_iso_ncomp(i))
!
        if(iflag .gt. 0) then
          draw_param%iso_value(i)                                       &
     &        = pvr_isos_c%pvr_iso_ctl(i)%isosurf_comp_ctl%charavalue
        end if
!
        iflag = pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%iflag
        if(iflag .gt. 0) then
          draw_param%iso_value(i)                                       &
     &        = pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%realvalue
        end if
!
        iflag = pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%iflag
        if(iflag .gt. 0) then
          draw_param%iso_opacity(i)                                     &
     &        = pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%realvalue
        end if
!
        iflag = pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%iflag
        if(iflag .gt. 0) then
          tmpchara                                                      &
     &      = pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%charavalue
          if(cmp_no_case(tmpchara, LABEL_DECREASE)) then
            draw_param%itype_isosurf(i) = IFLAG_SHOW_REVERSE
          else if(cmp_no_case(tmpchara, LABEL_DECREASE)) then
            draw_param%itype_isosurf(i) = IFLAG_SHOW_FORWARD
          else
            draw_param%itype_isosurf(i) = IFLAG_SHOW_FORWARD
          end if
        end if
      end do
!
      end subroutine set_control_pvr_isosurfs
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_pvr_isosurf
