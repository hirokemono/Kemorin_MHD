!set_control_each_pvr
!      module set_control_each_pvr
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine count_control_pvr(i_pvr, pvr,                         &
!     &          num_mat, mat_name, num_nod_phys, phys_nod_name)
!      subroutine set_control_pvr(i_pvr, pvr, num_mat, mat_name,        &
!     &          num_nod_phys, phys_nod_name, view_param)
!
      module set_control_each_pvr
!
      use m_precision
!
      use m_constants
      use m_error_IDs
      use m_control_data_4_pvr
      use m_control_params_4_pvr
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
      subroutine count_control_pvr(i_pvr, pvr,                          &
     &          num_mat, mat_name, num_nod_phys, phys_nod_name)
!
      use set_area_4_viz
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(pvr_ctl), intent(in) :: pvr
      integer(kind = kint), intent(in) :: i_pvr
!
      integer(kind = kint) :: num_field, num_phys_viz
      character(len = kchara) :: tmpchara
!
!
      if(pvr%file_head_ctl%iflag .gt. 0) then
        pvr_header(i_pvr) = pvr%file_head_ctl%charavalue
      else 
        pvr_header(i_pvr) = 'pvr'
      end if
!
      tmpchara = pvr%file_fmt_ctl%charavalue
      if     (cmp_no_case(tmpchara, 'ucd')                              &
     &   .or. cmp_no_case(tmpchara, 'udt')) then
        id_pvr_file_type(i_pvr) = 0
      else if(cmp_no_case(tmpchara, 'png')) then
        id_pvr_file_type(i_pvr) = 12
      else if(cmp_no_case(tmpchara, 'bmp')) then
        id_pvr_file_type(i_pvr) = 11
      else
        id_pvr_file_type(i_pvr) = 11
      end if
!
!
      tmpchara = pvr%transparent_ctl%charavalue
      if     (cmp_no_case(tmpchara, 'rgba')                             &
     &   .or. cmp_no_case(tmpchara, 'transparent')) then
        id_pvr_transparent(i_pvr) = 1
      else if(cmp_no_case(tmpchara, 'rgb')                              &
     &   .or. cmp_no_case(tmpchara, 'solid')) then
        id_pvr_transparent(i_pvr) = 0
      else
        id_pvr_transparent(i_pvr) = 0
      end if
!
      call check_field_4_viz(num_nod_phys, phys_nod_name,               &
     &    ione, pvr%pvr_field_ctl, num_field, num_phys_viz)
      if(num_field .eq. 0) then
        call calypso_MPI_abort(ierr_PVR,'set correct field name')
      end if
!
!
      call count_area_4_viz(num_mat, mat_name,                          &
     &    pvr%pvr_area_ctl%num, pvr%pvr_area_ctl%c_tbl,                 &
     &    nele_grp_area_pvr(i_pvr) )
      istack_grp_area_pvr(i_pvr) = istack_grp_area_pvr(i_pvr-1)         &
     &                          + nele_grp_area_pvr(i_pvr)
!
      if ( nele_grp_area_pvr(i_pvr) .eq. 0)                             &
     &   call calypso_MPI_abort(ierr_PVR, 'set correct element group')
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'i_pvr', i_pvr
        write(*,*) 'pvr_header(i_pvr)', pvr_header(i_pvr)
        write(*,*) 'id_pvr_file_type(i_pvr)', id_pvr_file_type(i_pvr)
        write(*,*) 'id_pvr_transparent(i_pvr)',                         &
     &      id_pvr_transparent(i_pvr)
        write(*,*) 'istack_grp_area_pvr(i_pvr)',                        &
     &            istack_grp_area_pvr(i_pvr)
        write(*,*) 'nele_grp_area_pvr(i_pvr)',                          &
     &            nele_grp_area_pvr(i_pvr)
      end if
!
      end subroutine count_control_pvr
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr(i_pvr, pvr, num_mat, mat_name,         &
     &          num_nod_phys, phys_nod_name, view_param)
!
      use set_pvr_modelview_matrix
      use set_area_4_viz
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_ctl), intent(inout) :: pvr
      type(pvr_view_parameter), intent(inout) :: view_param
!
      integer(kind = kint) :: ist, i, icheck_ncomp(1)
      real(kind = kreal), allocatable :: pvr_param_tmp(:,:)
!
      character(len = kchara) :: tmpchara
!
!
      call set_components_4_viz                                         &
     &   (num_nod_phys, phys_nod_name, ione, pvr%pvr_field_ctl,         &
     &    pvr%pvr_comp_ctl, ione, id_pvr_output(i_pvr),                 &
     &    icomp_pvr_output(i_pvr), icheck_ncomp(1),                     &
     &    ncomp_pvr_org(i_pvr), name_pvr_output(i_pvr) )
!
      ist = istack_grp_area_pvr(i_pvr-1) + 1
      call s_set_area_4_viz(num_mat, mat_name,                          &
     &    pvr%pvr_area_ctl%num, pvr%pvr_area_ctl%c_tbl,                 &
     &    nele_grp_area_pvr(i_pvr), id_ele_grp_area_pvr(ist) )
!
      if (icheck_ncomp(1) .gt. 1)                                       &
     &     call calypso_MPI_abort(ierr_PVR, 'set scalar for rendering')
!
!
      if(pvr%ambient_coef_ctl%iflag .gt. 0) then
        pvr_lighting_real(1,i_pvr) = pvr%ambient_coef_ctl%realvalue
      else
        pvr_lighting_real(1,i_pvr) = 0.5
      end if
!
      if(pvr%diffuse_coef_ctl%iflag .gt. 0) then
        pvr_lighting_real(2,i_pvr) = pvr%diffuse_coef_ctl%realvalue
      else
        pvr_lighting_real(2,i_pvr) = 5.0
      end if
!
      if(pvr%specular_coef_ctl%iflag .gt. 0) then
        pvr_lighting_real(3,i_pvr) = pvr%specular_coef_ctl%realvalue
      else
        pvr_lighting_real(3,i_pvr) = 1.0
      end if
!
!
      if(pvr%light_position_ctl%num .gt. 0) then
        num_pvr_lights(i_pvr) = pvr%light_position_ctl%num
      else
        num_pvr_lights(i_pvr) = 1
      end if
!
      ist = istack_pvr_lights(i_pvr-1)
      istack_pvr_lights(i_pvr) = ist + num_pvr_lights(i_pvr)
      ntot_pvr_lights =          ist + num_pvr_lights(i_pvr)
!
      allocate( pvr_param_tmp(3,ist) )
      do i = 1, ist
        pvr_param_tmp(1:3,i) = xyz_pvr_lights(1:3,i)
      end do
!
      deallocate( xyz_pvr_lights )
      allocate( xyz_pvr_lights(3,ntot_pvr_lights) )
!
      do i = 1, ist
        xyz_pvr_lights(1:3,i) = pvr_param_tmp(1:3,i)
      end do
      deallocate( pvr_param_tmp )
!
      if(pvr%light_position_ctl%num .gt. 0) then
        do i = 1, num_pvr_lights(i_pvr)
          xyz_pvr_lights(1,ist+i) = pvr%light_position_ctl%vec1(i)
          xyz_pvr_lights(2,ist+i) = pvr%light_position_ctl%vec2(i)
          xyz_pvr_lights(3,ist+i) = pvr%light_position_ctl%vec3(i)
        end do
        iflag_pvr_lights(i_pvr) = 1
      else
        xyz_pvr_lights(1,ist+1) = one
        xyz_pvr_lights(2,ist+1) = one
        xyz_pvr_lights(3,ist+1) = one
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
          view_param%iprm_pvr_rot(1) = 0
          view_param%iflag_rotation =  0
        end if
!
        view_param%iprm_pvr_rot(2) = pvr%num_frames_ctl%intvalue
      else
        view_param%iprm_pvr_rot(1) = 0
        view_param%iprm_pvr_rot(2) = 1
      end if
!
!    set colormap setting
!
      id_pvr_color(1,i_pvr) = 2
      if( pvr%colormap_ctl%iflag .gt. 0) then
        tmpchara = pvr%colormap_ctl%charavalue
        if     (cmp_no_case(tmpchara, 'rainbow')) then
          id_pvr_color(1,i_pvr) = 2
        else if(cmp_no_case(tmpchara, 'red_blue')) then
          id_pvr_color(1,i_pvr) = 1
        else if(cmp_no_case(tmpchara, 'grayscale')) then
          id_pvr_color(1,i_pvr) = 3
        end if
      end if
!
      id_pvr_color(2,i_pvr) = 1
      num_pvr_datamap_pnt(i_pvr) = 2
      if( pvr%data_mapping_ctl%iflag .gt. 0) then
        tmpchara = pvr%data_mapping_ctl%charavalue
        if      (cmp_no_case(tmpchara, 'nonlinear')                     &
     &      .or. cmp_no_case(tmpchara, 'colormap_list')) then
          if(pvr%colortbl_ctl%num .gt. 0) then
            id_pvr_color(2,i_pvr) = 3
            num_pvr_datamap_pnt(i_pvr) = pvr%colortbl_ctl%num
          end if
        else if (cmp_no_case(tmpchara, 'linear')                        &
     &      .or. cmp_no_case(tmpchara, 'minmax')) then
          if(      pvr%range_min_ctl%iflag .gt. 0                       &
     &       .and. pvr%range_max_ctl%iflag .gt. 0) then
            id_pvr_color(2,i_pvr) = 2
          end if
        end if
      end if
!
!
      ist = istack_pvr_datamap_pnt(i_pvr-1)
      istack_pvr_datamap_pnt(i_pvr) = ist + num_pvr_datamap_pnt(i_pvr)
      ntot_pvr_datamap_pnt =          ist + num_pvr_datamap_pnt(i_pvr)
!
      allocate( pvr_param_tmp(2,ist) )
      do i = 1, ist
        pvr_param_tmp(1:2,i) = pvr_datamap_param(1:2,i)
      end do
!
      deallocate( pvr_datamap_param )
      allocate( pvr_datamap_param(2,ntot_pvr_datamap_pnt) )
!
      do i = 1, ist
        pvr_datamap_param(1:2,i) = pvr_param_tmp(1:2,i)
      end do
      deallocate( pvr_param_tmp )
!
      if (id_pvr_color(2,i_pvr) .eq. 2) then
        pvr_datamap_param(1,ist+1) = pvr%range_min_ctl%realvalue
        pvr_datamap_param(1,ist+2) = pvr%range_max_ctl%realvalue
        pvr_datamap_param(2,ist+1) = zero
        pvr_datamap_param(2,ist+2) = one
!
      else if(id_pvr_color(2,i_pvr) .eq. 3) then
        do i = 1, num_pvr_datamap_pnt(i_pvr)
          pvr_datamap_param(1,ist+i) = pvr%colortbl_ctl%vec1(i)
          pvr_datamap_param(2,ist+i) = pvr%colortbl_ctl%vec2(i)
        end do
!
      else
        pvr_datamap_param(1,ist+1) = zero
        pvr_datamap_param(1,ist+2) = zero
        pvr_datamap_param(2,ist+1) = zero
        pvr_datamap_param(2,ist+2) = one
      end if
!
!
!
      id_pvr_color(3,i_pvr) = 1
      num_opacity_pnt(i_pvr) = 0
      if( pvr%opacity_style_ctl%iflag .gt. 0) then
        tmpchara = pvr%opacity_style_ctl%charavalue
!        if     (cmp_no_case(tmpchara, 'intense_chenge') then
!          id_pvr_color(3,i_pvr) = 2
!        end if
        if     (cmp_no_case(tmpchara, 'point_delta')) then
          if( pvr%opacity_ctl%num .gt. 0) then
            id_pvr_color(3,i_pvr) = 3
            num_opacity_pnt(i_pvr) = pvr%opacity_ctl%num
          end if
!
        else if(cmp_no_case(tmpchara, 'point_ranges')) then
          if( pvr%opacity_ctl%num .gt. 0) then
            id_pvr_color(3,i_pvr) = 4
            num_opacity_pnt(i_pvr) = pvr%opacity_ctl%num
          end if
!
        else if(cmp_no_case(tmpchara, 'point_linear')) then
          if( pvr%opacity_ctl%num .gt. 0) then
            id_pvr_color(3,i_pvr) = 5
            num_opacity_pnt(i_pvr) = pvr%opacity_ctl%num
          end if
        end if
      end if
!
      ist = istack_opacity_pnt(i_pvr-1)
      istack_opacity_pnt(i_pvr) = ist + num_opacity_pnt(i_pvr) + 1
      ntot_opacity_pnt =          ist + num_opacity_pnt(i_pvr) + 1
!
      allocate( pvr_param_tmp(3,ist) )
      do i = 1, ist
        pvr_param_tmp(1:3,i) = pvr_opacity_param(1:3,i)
      end do
!
      deallocate( pvr_opacity_param )
      allocate( pvr_opacity_param(3,ntot_opacity_pnt) )
!
      do i = 1, ist
        pvr_opacity_param(1:3,i) = pvr_param_tmp(1:3,i)
      end do
      deallocate( pvr_param_tmp )
!
!
      if    (id_pvr_color(3,i_pvr) .eq. 3                               &
     &  .or. id_pvr_color(3,i_pvr) .eq. 4) then
        do i = 1, num_opacity_pnt(i_pvr)
          pvr_opacity_param(1,ist+i) = pvr%opacity_ctl%vec1(i)
          pvr_opacity_param(2,ist+i) = pvr%opacity_ctl%vec2(i)
          pvr_opacity_param(3,ist+i) = pvr%opacity_ctl%vec3(i)
          pvr_max_opacity(i_pvr)                                        &
     &       = max(pvr_max_opacity(i_pvr),pvr_opacity_param(3,ist+i))
        end do
!
      else if(id_pvr_color(3,i_pvr) .eq. 5 ) then
        do i = 1, num_opacity_pnt(i_pvr)
          pvr_opacity_param(1,ist+i) = pvr%opacity_ctl%vec1(i)
          pvr_opacity_param(2,ist+i) = pvr%opacity_ctl%vec1(i)
          pvr_opacity_param(3,ist+i) = pvr%opacity_ctl%vec3(i)
          pvr_max_opacity(i_pvr)                                        &
     &       = max(pvr_max_opacity(i_pvr),pvr_opacity_param(3,ist+i))
        end do
      end if
!
      ist = istack_opacity_pnt(i_pvr)
      pvr_opacity_param(1,ist) = zero
      pvr_opacity_param(2,ist) = one
      if( pvr%fix_opacity_ctl%iflag .gt. 0) then
        pvr_opacity_param(3,ist) = pvr%fix_opacity_ctl%realvalue
      else
        pvr_opacity_param(3,ist) = 0.001
      end if
      pvr_max_opacity(i_pvr)                                            &
     &       = max(pvr_max_opacity(i_pvr),pvr_opacity_param(3,ist))
!
!    set colorbar setting
!
      iflag_pvr_colorbar(i_pvr) = 0
      if( pvr%colorbar_switch_ctl%iflag .gt. 0) then
        tmpchara = pvr%colorbar_switch_ctl%charavalue
        if   (cmp_no_case(tmpchara, 'on')                               &
     &   .or. cmp_no_case(tmpchara, 'data')                             &
     &   .or. cmp_no_case(tmpchara, 'equi_data')) then
          iflag_pvr_colorbar(i_pvr) = 1
        end if
      end if
!
      if ( iflag_pvr_colorbar(i_pvr) .gt. 0) then
        if( pvr%colorbar_scale_ctl%iflag .gt. 0) then
          tmpchara = pvr%colorbar_scale_ctl%charavalue
          if  (cmp_no_case(tmpchara, 'on')) then
            iflag_pvr_cbar_nums(i_pvr) = 1
!
            if (pvr%font_size_ctl%iflag .gt. 0) then
              iscale_font(i_pvr) = pvr%font_size_ctl%intvalue
            else
              iscale_font(i_pvr) = 1
            end if
!
            if (pvr%ngrid_cbar_ctl%iflag .gt. 0) then
              ntick_pvr_colorbar(i_pvr)                                 &
     &                      = pvr%ngrid_cbar_ctl%intvalue + 2
            else
              ntick_pvr_colorbar(i_pvr) = 3
            end if
!
            if (pvr%zeromarker_flag_ctl%iflag .gt. 0) then
              tmpchara = pvr%zeromarker_flag_ctl%charavalue
              if  (cmp_no_case(tmpchara, 'on')) then
                iflag_pvr_zero_mark(i_pvr) = 1
              else
                iflag_pvr_zero_mark(i_pvr) = 0
              end if
            else
              iflag_pvr_zero_mark(i_pvr) = 0
            end if
!
          end if
        end if
!
        if( pvr%cbar_range_ctl%iflag .gt. 0) then
          cbar_range(1:2,i_pvr) = pvr%cbar_range_ctl%realvalue(1:2)
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
