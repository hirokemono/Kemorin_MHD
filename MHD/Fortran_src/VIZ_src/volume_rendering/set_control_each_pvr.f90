!set_control_each_pvr
!      module set_control_each_pvr
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine set_pvr_file_control                                 &
!!     &         (pvr, num_nod_phys, phys_nod_name, file_param)
!!      subroutine set_control_field_4_pvr(field_ctl, comp_ctl,         &
!!     &          num_nod_phys, phys_nod_name, fld_param, icheck_ncomp)
!!      subroutine set_control_pvr(pvr, ele_grp, surf_grp, pvr_area,    &
!!     &          view_param, field_pvr, color_param, cbar_param)
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(pvr_parameter_ctl), intent(inout) :: pvr
!!        type(pvr_field_parameter), intent(inout) :: fld_param
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!        type(pvr_projected_field), intent(inout) :: field_pvr
!!        type(viz_area_parameter), intent(inout) :: pvr_area
!!        type(pvr_colormap_parameter), intent(inout) :: color_param
!!        type(pvr_colorbar_parameter), intent(inout) :: cbar_param
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
      private :: set_control_pvr_movie
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_file_control                                   &
     &         (pvr, num_nod_phys, phys_nod_name, file_param)
!
      use t_control_params_4_pvr
      use set_area_4_viz
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(pvr_parameter_ctl), intent(in) :: pvr
      type(pvr_output_parameter), intent(inout) :: file_param
!
      integer(kind = kint) :: num_field, num_phys_viz
      character(len = kchara) :: tmpfield(1)
      character(len = kchara) :: tmpchara
!
!
      if(pvr%file_head_ctl%iflag .gt. 0) then
        file_param%pvr_prefix = pvr%file_head_ctl%charavalue
      else 
        file_param%pvr_prefix = 'pvr'
      end if
!
      tmpchara = pvr%file_fmt_ctl%charavalue
      if     (cmp_no_case(tmpchara, 'ucd')                              &
     &   .or. cmp_no_case(tmpchara, 'udt')) then
        file_param%id_pvr_file_type = 0
      else if(cmp_no_case(tmpchara, 'png')) then
        file_param%id_pvr_file_type = iflag_PNG
      else if(cmp_no_case(tmpchara, 'bmp')) then
        file_param%id_pvr_file_type = iflag_BMP
      else
        file_param%id_pvr_file_type = iflag_BMP
      end if
!
!
      tmpchara = pvr%transparent_ctl%charavalue
      if     (cmp_no_case(tmpchara, 'rgba')                             &
     &   .or. cmp_no_case(tmpchara, 'transparent')) then
        file_param%id_pvr_transparent = 1
      else if(cmp_no_case(tmpchara, 'rgb')                              &
     &   .or. cmp_no_case(tmpchara, 'solid')) then
        file_param%id_pvr_transparent = 0
      else
        file_param%id_pvr_transparent = 0
      end if
!
      file_param%iflag_monitoring = 0
      if(yes_flag(pvr%monitoring_ctl%charavalue)) then
        file_param%iflag_monitoring = 1
      end if
!
      file_param%iflag_anaglyph = 0
      if(yes_flag(pvr%anaglyph_ctl%charavalue)) then
        file_param%iflag_anaglyph = 1
      end if
!
      tmpfield(1) = pvr%pvr_field_ctl%charavalue
      call check_field_4_viz(num_nod_phys, phys_nod_name,               &
     &    ione, tmpfield, num_field, num_phys_viz)
      if(num_field .eq. 0) then
        call calypso_MPI_abort(ierr_PVR,'set correct field name')
      end if
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'pvr_prefix: ', trim(file_param%pvr_prefix)
        write(*,*) 'id_pvr_file_type', file_param%id_pvr_file_type
        write(*,*) 'id_pvr_transparent', file_param%id_pvr_transparent
      end if
!
      end subroutine set_pvr_file_control
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
      subroutine set_control_pvr(pvr, ele_grp, surf_grp, pvr_area,      &
     &          view_param, field_pvr, color_param, cbar_param)
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
!
      type(pvr_parameter_ctl), intent(inout) :: pvr
      type(pvr_view_parameter), intent(inout) :: view_param
      type(pvr_projected_field), intent(inout) :: field_pvr
      type(viz_area_parameter), intent(inout) :: pvr_area
      type(pvr_colormap_parameter), intent(inout) :: color_param
      type(pvr_colorbar_parameter), intent(inout) :: cbar_param
!
      integer(kind = kint) :: id_section_method, ierr, i
      character(len = kchara) :: tmpchara
!
!
      view_param%iflag_stereo_pvr = 0
      if(yes_flag(pvr%streo_ctl%charavalue)) then
        view_param%iflag_stereo_pvr = 1
      end if
!
      call count_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    pvr%pvr_area_ctl%num, pvr%pvr_area_ctl%c_tbl,                 &
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
     &    pvr%pvr_area_ctl%num, pvr%pvr_area_ctl%c_tbl,                 &
     &    pvr_area%nele_grp_area_pvr, pvr_area%id_ele_grp_area_pvr)
!
!
      if (pvr%surf_enhanse_ctl%num .gt. 0) then
        call set_pvr_bc_enhanse_flag(surf_grp,                          &
     &      pvr%surf_enhanse_ctl%num, pvr%surf_enhanse_ctl%c1_tbl,      &
     &      pvr%surf_enhanse_ctl%c2_tbl, pvr%surf_enhanse_ctl%vect,     &
     &      field_pvr%iflag_enhanse, field_pvr%enhansed_opacity)
      else
         field_pvr%iflag_enhanse = IFLAG_NONE
      end if
!
!
      field_pvr%num_sections = pvr%num_pvr_sect_ctl
      if(field_pvr%num_sections .gt. 0) then
        call alloc_pvr_sections(field_pvr)
!
        do i = 1, field_pvr%num_sections
          call s_set_coefs_of_sections                                  &
     &       (pvr%pvr_sect_ctl(i)%psf_c, id_section_method,             &
     &        field_pvr%coefs(1:10,i), ierr)
          if(ierr .gt. 0) call calypso_mpi_abort                        &
     &         (ierr, 'Set section parameters for PVR')
!
          if(pvr%pvr_sect_ctl(i)%opacity_ctl%iflag .gt. 0) then
            field_pvr%sect_opacity(i)                                   &
     &        = pvr%pvr_sect_ctl(i)%opacity_ctl%realvalue
          end if
        end do
      end if
!
!
      field_pvr%num_isosurf = pvr%num_pvr_iso_ctl
      if(field_pvr%num_isosurf .gt. 0) then
        call alloc_pvr_isosurfaces(field_pvr)
!
        do i = 1, field_pvr%num_isosurf
          if(pvr%pvr_iso_ctl(i)%isosurf_value_ctl%iflag .gt. 0) then
            field_pvr%iso_value(i)                                      &
     &        = pvr%pvr_iso_ctl(i)%isosurf_value_ctl%realvalue
          end if
          if(pvr%pvr_iso_ctl(i)%opacity_ctl%iflag .gt. 0) then
            field_pvr%iso_opacity(i)                                    &
     &        = pvr%pvr_iso_ctl(i)%opacity_ctl%realvalue
          end if
          if(pvr%pvr_iso_ctl(i)%isosurf_type_ctl%iflag .gt. 0) then
            tmpchara = pvr%pvr_iso_ctl(i)%isosurf_type_ctl%charavalue
            if(cmp_no_case(tmpchara, LABEL_DECREASE)) then
              field_pvr%itype_isosurf(i) = IFLAG_SHOW_REVERSE
            else if(cmp_no_case(tmpchara, LABEL_DECREASE)) then
              field_pvr%itype_isosurf(i) = IFLAG_SHOW_FORWARD
            else
              field_pvr%itype_isosurf(i) = IFLAG_SHOW_FORWARD
            end if
          end if
        end do
      end if
!
      call set_control_pvr_movie(pvr%movie, view_param)
!
!    set colormap setting
      call set_control_pvr_lighting(pvr%color, color_param)
      call set_control_pvr_colormap(pvr%color, color_param)
!
!    set colorbar setting
      call set_control_pvr_colorbar(pvr%cbar_ctl, cbar_param)
!
      end subroutine set_control_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr_movie(movie, view_param)
!
      use t_control_data_pvr_misc
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
      end module set_control_each_pvr
