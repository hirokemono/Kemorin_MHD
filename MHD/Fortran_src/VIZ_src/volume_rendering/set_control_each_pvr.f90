!>@file   set_control_each_pvr.f90
!!@brief  module set_control_each_pvr
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set each PVR parameters from control
!!
!!@verbatim
!!      subroutine set_control_pvr                                      &
!!     &         (ele_grp, surf_grp, nod_fld, pvr_ctl, pvr_area,        &
!!     &          draw_param, pvr_section_p, pvr_isos_p,                &
!!     &          color_param, cbar_param)
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(phys_data), intent(in) :: nod_fld
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!!        type(pvr_field_parameter), intent(inout) :: fld_param
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!        type(rendering_parameter), intent(inout) :: draw_param
!!        type(pvr_section_parameter), intent(inout) :: pvr_section_p
!!        type(pvr_isosurfs_parameter), intent(inout) :: pvr_isos_p
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
      module set_control_each_pvr
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
      private :: set_control_pvr_render_area
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr                                        &
     &         (ele_grp, surf_grp, nod_fld, pvr_ctl, pvr_area,          &
     &          draw_param, pvr_section_p, pvr_isos_p,                  &
     &          color_param, cbar_param)
!
      use t_phys_data
      use t_group_data
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_control_param_pvr_section
      use t_control_param_pvr_isosurf
      use set_color_4_pvr
      use set_rgba_4_each_pixel
      use set_coefs_of_sections
      use set_control_pvr_color
      use skip_comment_f
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: surf_grp
      type(phys_data), intent(in) :: nod_fld
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
      type(rendering_parameter), intent(inout) :: draw_param
      type(pvr_section_parameter), intent(inout) :: pvr_section_p
      type(pvr_isosurfs_parameter), intent(inout) :: pvr_isos_p
      type(viz_area_parameter), intent(inout) :: pvr_area
      type(pvr_colormap_parameter), intent(inout) :: color_param
      type(pvr_colorbar_parameter), intent(inout) :: cbar_param
!
!
      call set_control_pvr_render_area(pvr_ctl%render_area_c,           &
     &    ele_grp, surf_grp, pvr_area, draw_param)
!
      call set_control_pvr_sections(pvr_ctl%pvr_scts_c, pvr_section_p)
!
      call set_control_pvr_isosurfs                                     &
     &   (nod_fld%num_phys, nod_fld%phys_name,                          &
     &    pvr_ctl%pvr_isos_c, pvr_isos_p)
!
!    set colormap setting
      call set_control_pvr_lighting(pvr_ctl%light, color_param)
      call set_control_pvr_colormap                                     &
     &   (pvr_ctl%cmap_cbar_c%color, color_param)
      call set_control_pvr_colorbar                                     &
     &   (pvr_ctl%cmap_cbar_c%cbar_ctl, cbar_param)
!
      end subroutine set_control_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr_render_area                            &
     &         (render_area_c, ele_grp, surf_grp, pvr_area, draw_param)
!
      use t_group_data
      use t_control_data_pvr_area
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use skip_comment_f
      use pvr_surface_enhancement
      use set_area_4_viz
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: surf_grp
      type(pvr_render_area_ctl), intent(in) :: render_area_c
!
      type(rendering_parameter), intent(inout) :: draw_param
      type(viz_area_parameter), intent(inout) :: pvr_area
!
!
      call count_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    render_area_c%pvr_area_ctl%num,                               &
     &    render_area_c%pvr_area_ctl%c_tbl,                             &
     &    pvr_area%nele_grp_area_pvr)
!
      if (pvr_area%nele_grp_area_pvr .le. 0) then
        call calypso_MPI_abort(ierr_PVR, 'set correct element group')
      else
        call alloc_pvr_element_group(pvr_area)
      end if
!
!
      call s_set_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    render_area_c%pvr_area_ctl%num,                               &
     &    render_area_c%pvr_area_ctl%c_tbl,                             &
     &    pvr_area%nele_grp_area_pvr, pvr_area%id_ele_grp_area_pvr)
!
!
      if (render_area_c%surf_enhanse_ctl%num .gt. 0) then
        call set_pvr_bc_enhanse_flag(surf_grp,                          &
     &      render_area_c%surf_enhanse_ctl%num,                         &
     &      render_area_c%surf_enhanse_ctl%c1_tbl,                      &
     &      render_area_c%surf_enhanse_ctl%c2_tbl,                      &
     &      render_area_c%surf_enhanse_ctl%vect,                        &
     &      draw_param%iflag_enhanse, draw_param%enhansed_opacity)
      else
         draw_param%iflag_enhanse = IFLAG_NONE
      end if
!
      end subroutine set_control_pvr_render_area
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr_movie(movie, view_param)
!
      use t_control_data_pvr_movie
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use skip_comment_f
!
      type(pvr_movie_ctl), intent(in) :: movie
      type(pvr_view_parameter), intent(inout) :: view_param
!
      character(len = kchara) :: tmpchara
!
!
      if      (movie%num_frames_ctl%iflag .gt.    0                     &
     &   .and. movie%rotation_axis_ctl%iflag .gt. 0) then
        tmpchara = movie%rotation_axis_ctl%charavalue
        if     (cmp_no_case(tmpchara, 'x')) then
          view_param%iprm_pvr_rot(1) = 1
          view_param%iflag_rotate_snap = 1
        else if(cmp_no_case(tmpchara, 'y')) then
          view_param%iprm_pvr_rot(1) = 2
          view_param%iflag_rotate_snap = 1
        else if(cmp_no_case(tmpchara, 'z')) then
          view_param%iprm_pvr_rot(1) = 3
          view_param%iflag_rotate_snap = 1
        else
          view_param%iprm_pvr_rot(1) =   0
          view_param%iflag_rotate_snap = 0
        end if
!
        view_param%iprm_pvr_rot(2) = movie%num_frames_ctl%intvalue
      else
        view_param%iflag_rotate_snap = 0
        view_param%iprm_pvr_rot(1) = 0
        view_param%iprm_pvr_rot(2) = 1
        view_param%iflag_rotate_snap = 0
      end if
!
      if(view_param%iflag_rotate_snap .eq. 0) then
        view_param%istart_rot = 0
        view_param%iend_rot =   0
      else
        view_param%istart_rot = 1
        view_param%iend_rot =   view_param%iprm_pvr_rot(2)
      end if
!
      end subroutine set_control_pvr_movie
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_stereo_control(pvr_ctl, view_param)
!
      use t_control_params_4_pvr
      use set_area_4_viz
      use skip_comment_f
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      type(pvr_view_parameter), intent(inout) :: view_param
!
!
      view_param%iflag_stereo_pvr = 0
      view_param%iflag_anaglyph = 0
      if(yes_flag(pvr_ctl%streo_ctl%charavalue)) then
        view_param%iflag_stereo_pvr = 1
!
        if(yes_flag(pvr_ctl%anaglyph_ctl%charavalue)) then
          view_param%iflag_anaglyph = 1
        else
        end if
      end if
!
      end subroutine set_pvr_stereo_control
!
!  ---------------------------------------------------------------------
!
      end module set_control_each_pvr
