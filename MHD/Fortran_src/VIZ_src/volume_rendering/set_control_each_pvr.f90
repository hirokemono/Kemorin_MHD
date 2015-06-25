!set_control_each_pvr
!      module set_control_each_pvr
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine set_pvr_file_control                                 &
!!     &         (pvr, num_nod_phys, phys_nod_name, file_param)
!!      subroutine set_control_pvr(pvr, num_mat, mat_name,              &
!!     &          num_nod_phys, phys_nod_name, fld_param, view_param,   &
!!     &          color_param, cbar_param)
!
      module set_control_each_pvr
!
      use m_precision
!
      use m_constants
      use m_error_IDs
      use m_control_data_4_pvr
      use calypso_mpi
!
      use set_field_comp_for_viz
!
      implicit  none
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
      type(pvr_ctl), intent(in) :: pvr
      type(pvr_output_parameter), intent(inout) :: file_param
!
!
      integer(kind = kint) :: num_field, num_phys_viz
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
        file_param%id_pvr_file_type = 12
      else if(cmp_no_case(tmpchara, 'bmp')) then
        file_param%id_pvr_file_type = 11
      else
        file_param%id_pvr_file_type = 11
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
      call check_field_4_viz(num_nod_phys, phys_nod_name,               &
     &    ione, pvr%pvr_field_ctl, num_field, num_phys_viz)
      if(num_field .eq. 0) then
        call calypso_MPI_abort(ierr_PVR,'set correct field name')
      end if
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'pvr_prefix', file_param%pvr_prefix
        write(*,*) 'id_pvr_file_type', file_param%id_pvr_file_type
        write(*,*) 'id_pvr_transparent', file_param%id_pvr_transparent
      end if
!
      end subroutine set_pvr_file_control
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr(pvr, num_mat, mat_name,                &
     &          num_nod_phys, phys_nod_name, fld_param, view_param,     &
     &          color_param, cbar_param)
!
      use t_control_params_4_pvr
      use set_pvr_modelview_matrix
      use set_area_4_viz
      use set_color_4_pvr
      use set_rgba_4_each_pixel
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(pvr_ctl), intent(inout) :: pvr
      type(pvr_field_parameter), intent(inout) :: fld_param
      type(pvr_view_parameter), intent(inout) :: view_param
      type(pvr_colormap_parameter), intent(inout) :: color_param
      type(pvr_colorbar_parameter), intent(inout) :: cbar_param
!
      integer(kind = kint) :: i, icheck_ncomp(1), ist
      integer(kind = kint) :: ifld_tmp(1), icomp_tmp(1), ncomp_tmp(1)
      character(len = kchara) :: fldname_tmp(1)
      character(len = kchara) :: tmpchara
!
!
      call set_components_4_viz                                         &
     &   (num_nod_phys, phys_nod_name, ione, pvr%pvr_field_ctl,         &
     &    pvr%pvr_comp_ctl, ione, ifld_tmp, icomp_tmp,                  &
     &    icheck_ncomp, ncomp_tmp, fldname_tmp)
      fld_param%id_pvr_output =    ifld_tmp(1)
      fld_param%icomp_pvr_output = icomp_tmp(1)
      fld_param%ncomp_pvr_org =    ncomp_tmp(1)
      fld_param%name_pvr_output =  fldname_tmp(1)
!
      if (icheck_ncomp(1) .gt. 1)                                       &
     &     call calypso_MPI_abort(ierr_PVR, 'set scalar for rendering')
!
!
      call count_area_4_viz(num_mat, mat_name,                          &
     &    pvr%pvr_area_ctl%num, pvr%pvr_area_ctl%c_tbl,                 &
     &    fld_param%nele_grp_area_pvr)
!
      if (fld_param%nele_grp_area_pvr .le. 0) then
        call calypso_MPI_abort(ierr_PVR, 'set correct element group')
      else
        call alloc_pvr_element_group(fld_param)
      end if
!
      call s_set_area_4_viz(num_mat, mat_name,                          &
     &    pvr%pvr_area_ctl%num, pvr%pvr_area_ctl%c_tbl,                 &
     &    fld_param%nele_grp_area_pvr, fld_param%id_ele_grp_area_pvr)
!
      if(pvr%ambient_coef_ctl%iflag .gt. 0) then
        color_param%pvr_lighting_real(1)                                &
     &      = pvr%ambient_coef_ctl%realvalue
      else
        color_param%pvr_lighting_real(1) = 0.5
      end if
!
!
!

      if(pvr%diffuse_coef_ctl%iflag .gt. 0) then
        color_param%pvr_lighting_real(2)                                &
     &      = pvr%diffuse_coef_ctl%realvalue
      else
        color_param%pvr_lighting_real(2) = 5.0
      end if
!
      if(pvr%specular_coef_ctl%iflag .gt. 0) then
        color_param%pvr_lighting_real(3)                                &
     &      = pvr%specular_coef_ctl%realvalue
      else
        color_param%pvr_lighting_real(3) = 1.0
      end if
!
!
      if(pvr%light_position_ctl%num .gt. 0) then
        color_param%num_pvr_lights = pvr%light_position_ctl%num
      else
        color_param%num_pvr_lights = 1
      end if
!
      call alloc_light_posi_in_view(color_param)
!
      if(pvr%light_position_ctl%num .gt. 0) then
        do i = 1, color_param%num_pvr_lights
          color_param%xyz_pvr_lights(1,i)                               &
     &          = pvr%light_position_ctl%vec1(i)
          color_param%xyz_pvr_lights(2,i)                               &
     &          = pvr%light_position_ctl%vec2(i)
          color_param%xyz_pvr_lights(3,i)                               &
     &          = pvr%light_position_ctl%vec3(i)
        end do
        color_param%iflag_pvr_lights = 1
      else
        color_param%xyz_pvr_lights(1,1) = one
        color_param%xyz_pvr_lights(2,1) = one
        color_param%xyz_pvr_lights(3,1) = one
      end if
!
!
      if      (pvr%num_frames_ctl%iflag .gt.    0                       &
     &   .and. pvr%rotation_axis_ctl%iflag .gt. 0) then
        tmpchara = pvr%rotation_axis_ctl%charavalue
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
        view_param%iprm_pvr_rot(2) = pvr%num_frames_ctl%intvalue
      else
        view_param%iflag_rotate_snap = 0
        view_param%iprm_pvr_rot(1) = 0
        view_param%iprm_pvr_rot(2) = 1
      end if
!
!    set colormap setting
!
      color_param%id_pvr_color(1) = iflag_rainbow
      if( pvr%colormap_ctl%iflag .gt. 0) then
        tmpchara = pvr%colormap_ctl%charavalue
        if     (cmp_no_case(tmpchara, hd_rainbow)) then
          color_param%id_pvr_color(1) = iflag_rainbow
        else if(cmp_no_case(tmpchara, hd_readblue)) then
          color_param%id_pvr_color(1) = iflag_readblue
        else if(cmp_no_case(tmpchara, hd_grayscale)) then
          color_param%id_pvr_color(1) = iflag_grayscale
        end if
      end if
!
      color_param%id_pvr_color(2) = iflag_automatic
      color_param%num_pvr_datamap_pnt = 2
      if( pvr%data_mapping_ctl%iflag .gt. 0) then
        tmpchara = pvr%data_mapping_ctl%charavalue
        if      (cmp_no_case(tmpchara, hd_nonlinear)                    &
     &      .or. cmp_no_case(tmpchara, hd_colorlist)) then
          if(pvr%colortbl_ctl%num .gt. 0) then
            color_param%id_pvr_color(2) = iflag_colorlist
            color_param%num_pvr_datamap_pnt = pvr%colortbl_ctl%num
          end if
        else if (cmp_no_case(tmpchara, hd_linear)                       &
     &      .or. cmp_no_case(tmpchara, hd_minmax)) then
          if(      pvr%range_min_ctl%iflag .gt. 0                       &
     &       .and. pvr%range_max_ctl%iflag .gt. 0) then
            color_param%id_pvr_color(2) = iflag_minmax
          end if
        end if
      end if
!
!
      call alloc_pvr_color_parameteres(color_param)
!
      if (color_param%id_pvr_color(2) .eq. iflag_minmax) then
        color_param%pvr_datamap_param(1,1)                              &
     &        = pvr%range_min_ctl%realvalue
        color_param%pvr_datamap_param(1,2)                              &
     &        = pvr%range_max_ctl%realvalue
        color_param%pvr_datamap_param(2,1) = zero
        color_param%pvr_datamap_param(2,2) = one
!
      else if(color_param%id_pvr_color(2) .eq. iflag_colorlist) then
        do i = 1, color_param%num_pvr_datamap_pnt
          color_param%pvr_datamap_param(1,i) = pvr%colortbl_ctl%vec1(i)
          color_param%pvr_datamap_param(2,i) = pvr%colortbl_ctl%vec2(i)
        end do
!
      else
        color_param%pvr_datamap_param(1,1) = zero
        color_param%pvr_datamap_param(1,2) = zero
        color_param%pvr_datamap_param(2,1) = zero
        color_param%pvr_datamap_param(2,2) = one
      end if
!
!
!
      color_param%id_pvr_color(3) = iflag_anbient
      color_param%num_opacity_pnt = 0
      if( pvr%opacity_style_ctl%iflag .gt. 0) then
        tmpchara = pvr%opacity_style_ctl%charavalue
!        if     (cmp_no_case(tmpchara, hd_intensity) then
!          color_param%id_pvr_color(3) = iflag_intense
!        end if
        if     (cmp_no_case(tmpchara, hd_pointdelta)) then
          if( pvr%opacity_ctl%num .gt. 0) then
            color_param%id_pvr_color(3) = iflag_pointdelta
            color_param%num_opacity_pnt = pvr%opacity_ctl%num
          end if
!
        else if(cmp_no_case(tmpchara, hd_pointrange)) then
          if( pvr%opacity_ctl%num .gt. 0) then
            color_param%id_pvr_color(3) = iflag_pointrange
            color_param%num_opacity_pnt = pvr%opacity_ctl%num
          end if
!
        else if(cmp_no_case(tmpchara, hd_pointlinear)) then
          if( pvr%opacity_ctl%num .gt. 0) then
            color_param%id_pvr_color(3) = iflag_pointlinear
            color_param%num_opacity_pnt = pvr%opacity_ctl%num
          end if
        end if
      end if
!
      call alloc_pvr_opacity_list(color_param)
!
      if    (color_param%id_pvr_color(3) .eq. iflag_pointdelta          &
     &  .or. color_param%id_pvr_color(3) .eq. iflag_pointrange) then
        do i = 1, color_param%num_opacity_pnt
          color_param%pvr_opacity_param(1,i) = pvr%opacity_ctl%vec1(i)
          color_param%pvr_opacity_param(2,i) = pvr%opacity_ctl%vec2(i)
          color_param%pvr_opacity_param(3,i) = pvr%opacity_ctl%vec3(i)
          color_param%pvr_max_opacity                                   &
     &       = max(color_param%pvr_max_opacity,                         &
     &             color_param%pvr_opacity_param(3,i))
        end do
!
      else if(color_param%id_pvr_color(3) .eq. iflag_pointlinear) then
        do i = 1, color_param%num_opacity_pnt
          color_param%pvr_opacity_param(1,i) = pvr%opacity_ctl%vec1(i)
          color_param%pvr_opacity_param(2,i) = pvr%opacity_ctl%vec1(i)
          color_param%pvr_opacity_param(3,i) = pvr%opacity_ctl%vec3(i)
          color_param%pvr_max_opacity                                   &
     &       = max(color_param%pvr_max_opacity,                         &
     &             color_param%pvr_opacity_param(3,i))
        end do
      end if
!
      ist = color_param%num_opacity_pnt + 1
      color_param%pvr_opacity_param(1,ist) = zero
      color_param%pvr_opacity_param(2,ist) = one
      if( pvr%fix_opacity_ctl%iflag .gt. 0) then
        color_param%pvr_opacity_param(3,ist)                            &
     &       = pvr%fix_opacity_ctl%realvalue
      else
        color_param%pvr_opacity_param(3,ist) = 0.001
      end if
      color_param%pvr_max_opacity                                       &
     &       = max(color_param%pvr_max_opacity,                         &
     &             color_param%pvr_opacity_param(3,ist))
!
!    set colorbar setting
!
      cbar_param%iflag_pvr_colorbar = 0
      if( pvr%colorbar_switch_ctl%iflag .gt. 0) then
        tmpchara = pvr%colorbar_switch_ctl%charavalue
        if   (cmp_no_case(tmpchara, 'on')                               &
     &   .or. cmp_no_case(tmpchara, 'data')                             &
     &   .or. cmp_no_case(tmpchara, 'equi_data')) then
          cbar_param%iflag_pvr_colorbar = 1
        end if
      end if
!
      if (cbar_param%iflag_pvr_colorbar .gt. 0) then
        if( pvr%colorbar_scale_ctl%iflag .gt. 0) then
          tmpchara = pvr%colorbar_scale_ctl%charavalue
          if  (cmp_no_case(tmpchara, 'on')) then
            cbar_param%iflag_pvr_cbar_nums = 1
!
            if (pvr%font_size_ctl%iflag .gt. 0) then
              cbar_param%iscale_font = pvr%font_size_ctl%intvalue
            else
              cbar_param%iscale_font = 1
            end if
!
            if (pvr%ngrid_cbar_ctl%iflag .gt. 0) then
              cbar_param%ntick_pvr_colorbar                             &
     &                      = pvr%ngrid_cbar_ctl%intvalue + 2
            else
              cbar_param%ntick_pvr_colorbar = 3
            end if
!
            if (pvr%zeromarker_flag_ctl%iflag .gt. 0) then
              tmpchara = pvr%zeromarker_flag_ctl%charavalue
              if  (cmp_no_case(tmpchara, 'on')) then
                cbar_param%iflag_pvr_zero_mark = 1
              else
                cbar_param%iflag_pvr_zero_mark = 0
              end if
            else
              cbar_param%iflag_pvr_zero_mark = 0
            end if
!
          end if
        end if
!
        if( pvr%cbar_range_ctl%iflag .gt. 0) then
          cbar_param%cbar_range(1:2)                                    &
     &                = pvr%cbar_range_ctl%realvalue(1:2)
        end if
!
      end if
!
!   set transfer matrix
!
      call s_set_pvr_modelview_matrix(pvr%mat, view_param)
!
      end subroutine set_control_pvr
!
!  ---------------------------------------------------------------------
!
      end module set_control_each_pvr
