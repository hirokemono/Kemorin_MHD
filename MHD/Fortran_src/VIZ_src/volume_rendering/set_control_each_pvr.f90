!>@file   set_control_each_pvr.f90
!!@brief  module set_control_each_pvr
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
!!     &          view_param, field_pvr, color_param, cbar_param)
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!!        type(pvr_field_parameter), intent(inout) :: fld_param
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!        type(pvr_projected_field), intent(inout) :: field_pvr
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
      private :: set_control_pvr_isosurf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine check_pvr_field_control                                &
     &         (pvr_ctl, num_nod_phys, phys_nod_name)
!
      use t_control_params_4_pvr
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!
      integer(kind = kint) :: num_field, num_phys_viz
      character(len = kchara) :: tmpfield(1)
!
!
      tmpfield(1) = pvr_ctl%pvr_field_ctl%charavalue
      call check_field_4_viz(num_nod_phys, phys_nod_name,               &
     &    ione, tmpfield, num_field, num_phys_viz)
      if(num_field .eq. 0) then
        call calypso_MPI_abort(ierr_PVR,'set correct field name')
      end if
!
      end subroutine check_pvr_field_control
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_control_field_4_pvr(field_ctl, comp_ctl,           &
     &          num_nod_phys, phys_nod_name, fld_param, icheck_ncomp)
!
      use t_control_params_4_pvr
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
      type(read_character_item), intent(in) :: field_ctl
      type(read_character_item), intent(in) :: comp_ctl
!
      type(pvr_field_parameter), intent(inout) :: fld_param
      integer(kind = kint), intent(inout) :: icheck_ncomp(1)
!
      integer(kind = kint) :: ifld_tmp(1), icomp_tmp(1), ncomp_tmp(1)
      character(len = kchara) :: fldname_tmp(1)
      character(len = kchara) :: tmpfield(1), tmpcomp(1)
!
!
      tmpfield(1) = field_ctl%charavalue
      tmpcomp(1) =  comp_ctl%charavalue
      call set_components_4_viz                                         &
     &   (num_nod_phys, phys_nod_name, ione, tmpfield, tmpcomp, ione,   &
     &    ifld_tmp, icomp_tmp, icheck_ncomp, ncomp_tmp, fldname_tmp)
      fld_param%id_field =          ifld_tmp(1)
      fld_param%id_component =      icomp_tmp(1)
      fld_param%num_original_comp = ncomp_tmp(1)
      fld_param%field_name =        fldname_tmp(1)
!
      end subroutine set_control_field_4_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr(pvr_ctl, ele_grp, surf_grp, pvr_area,  &
     &          field_pvr, color_param, cbar_param)
!
      use t_group_data
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use set_area_4_viz
      use set_color_4_pvr
      use set_rgba_4_each_pixel
      use pvr_surface_enhancement
      use set_coefs_of_sections
      use set_control_pvr_color
      use skip_comment_f
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: surf_grp
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!
      type(pvr_projected_field), intent(inout) :: field_pvr
      type(viz_area_parameter), intent(inout) :: pvr_area
      type(pvr_colormap_parameter), intent(inout) :: color_param
      type(pvr_colorbar_parameter), intent(inout) :: cbar_param
!
      integer(kind = kint) :: id_section_method, ierr, i
!
!
      call count_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    pvr_ctl%pvr_area_ctl%num, pvr_ctl%pvr_area_ctl%c_tbl,         &
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
     &    pvr_ctl%pvr_area_ctl%num, pvr_ctl%pvr_area_ctl%c_tbl,         &
     &    pvr_area%nele_grp_area_pvr, pvr_area%id_ele_grp_area_pvr)
!
!
      if (pvr_ctl%surf_enhanse_ctl%num .gt. 0) then
        call set_pvr_bc_enhanse_flag(surf_grp,                          &
     &      pvr_ctl%surf_enhanse_ctl%num,                               &
     &      pvr_ctl%surf_enhanse_ctl%c1_tbl,                            &
     &      pvr_ctl%surf_enhanse_ctl%c2_tbl,                            &
     &      pvr_ctl%surf_enhanse_ctl%vect,                              &
     &      field_pvr%iflag_enhanse, field_pvr%enhansed_opacity)
      else
         field_pvr%iflag_enhanse = IFLAG_NONE
      end if
!
!
      field_pvr%num_sections = pvr_ctl%num_pvr_sect_ctl
      if(field_pvr%num_sections .gt. 0) then
        call alloc_pvr_sections(field_pvr)
!
        do i = 1, field_pvr%num_sections
          call s_set_coefs_of_sections                                  &
     &       (pvr_ctl%pvr_sect_ctl(i)%psf_c, id_section_method,         &
     &        field_pvr%coefs(1:10,i), ierr)
          if(ierr .gt. 0) call calypso_mpi_abort                        &
     &         (ierr, 'Set section parameters for pvr_ctl')
!
          if(pvr_ctl%pvr_sect_ctl(i)%opacity_ctl%iflag .gt. 0) then
            field_pvr%sect_opacity(i)                                   &
     &        = pvr_ctl%pvr_sect_ctl(i)%opacity_ctl%realvalue
          end if
        end do
      end if
!
!
      call set_control_pvr_isosurf(pvr_ctl%pvr_isos_c, field_pvr)
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
      subroutine set_control_pvr_isosurf(pvr_isos_c, field_pvr)
!
      use t_control_data_pvr_isosurfs
      use t_geometries_in_pvr_screen
      use pvr_surface_enhancement
!
      type(pvr_isosurfs_ctl), intent(in) :: pvr_isos_c
!
      type(pvr_projected_field), intent(inout) :: field_pvr
!
      integer(kind = kint) ::  i
      character(len = kchara) :: tmpchara
!
      integer(kind = kint) :: iflag
!
!
      field_pvr%num_isosurf = pvr_isos_c%num_pvr_iso_ctl
      if(field_pvr%num_isosurf .le. 0) return
!
      call alloc_pvr_isosurfaces(field_pvr)
!
      do i = 1, field_pvr%num_isosurf
        iflag = pvr_isos_c%pvr_iso_ctl(i)%isosurf_value_ctl%iflag
        if(iflag .gt. 0) then
          field_pvr%iso_value(i)                                        &
     &        = pvr_isos_c%pvr_iso_ctl(i)%isosurf_value_ctl%realvalue
        end if
!
        iflag = pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%iflag
        if(iflag .gt. 0) then
          field_pvr%iso_opacity(i)                                      &
     &        = pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%realvalue
        end if
!
        iflag = pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%iflag
        if(iflag .gt. 0) then
          tmpchara                                                      &
     &      = pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%charavalue
          if(cmp_no_case(tmpchara, LABEL_DECREASE)) then
            field_pvr%itype_isosurf(i) = IFLAG_SHOW_REVERSE
          else if(cmp_no_case(tmpchara, LABEL_DECREASE)) then
            field_pvr%itype_isosurf(i) = IFLAG_SHOW_FORWARD
          else
            field_pvr%itype_isosurf(i) = IFLAG_SHOW_FORWARD
          end if
        end if
      end do
!
      end subroutine set_control_pvr_isosurf
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
