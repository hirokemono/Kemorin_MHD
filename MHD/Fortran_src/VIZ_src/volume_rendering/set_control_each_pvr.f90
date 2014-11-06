!set_control_each_pvr
!      module set_control_each_pvr
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine count_control_pvr(i_pvr, pvr,                         &
!     &          num_mat, mat_name, num_nod_phys, phys_nod_name)
!      subroutine set_control_pvr(i_pvr, pvr, num_mat, mat_name,        &
!     &          num_nod_phys, phys_nod_name)
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
      integer(kind = kint) :: num_field
!
!
      if(pvr%i_pvr_file_head .gt. 0) then
        pvr_header(i_pvr) = pvr%pvr_file_head_ctl
      else 
        pvr_header(i_pvr) = 'pvr'
      end if
!
      if     (pvr%pvr_output_type_ctl .eq. 'ucd'                        &
     &   .or. pvr%pvr_output_type_ctl .eq. 'UCD'                        &
     &   .or. pvr%pvr_output_type_ctl .eq. 'udt'                        &
     &   .or. pvr%pvr_output_type_ctl .eq. 'UDT') then
        id_pvr_file_type(i_pvr) = 0
      else if(pvr%pvr_output_type_ctl .eq. 'PNG'                        &
     &   .or. pvr%pvr_output_type_ctl .eq. 'png') then
        id_pvr_file_type(i_pvr) = 12
      else if(pvr%pvr_output_type_ctl .eq. 'BMP'                        &
     &   .or. pvr%pvr_output_type_ctl .eq. 'bmp') then
        id_pvr_file_type(i_pvr) = 11
      else
        id_pvr_file_type(i_pvr) = 11
      end if
!
!
      if     (pvr%pvr_transparent_ctl .eq. 'rgba'                       &
     &   .or. pvr%pvr_transparent_ctl .eq. 'RGBA'                       &
     &   .or. pvr%pvr_transparent_ctl .eq. 'transparent'                &
     &   .or. pvr%pvr_transparent_ctl .eq. 'Transparent'                &
     &   .or. pvr%pvr_transparent_ctl .eq. 'TRANSPARENT') then
        id_pvr_transparent(i_pvr) = 1
      else if(pvr%pvr_transparent_ctl .eq. 'rgb'                        &
     &   .or. pvr%pvr_transparent_ctl .eq. 'RGB'                        &
     &   .or. pvr%pvr_transparent_ctl .eq. 'solid'                      &
     &   .or. pvr%pvr_transparent_ctl .eq. 'Solid'                      &
     &   .or. pvr%pvr_transparent_ctl .eq. 'SOLID') then
        id_pvr_transparent(i_pvr) = 0
      else
        id_pvr_transparent(i_pvr) = 0
      end if
!
      call check_field_4_viz(num_nod_phys, phys_nod_name,               &
     &    ione, pvr%pvr_field_ctl, num_field )
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
     &          num_nod_phys, phys_nod_name)
!
      use set_pvr_modelview_matrix
      use set_area_4_viz
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(pvr_ctl), intent(inout) :: pvr
      integer(kind = kint), intent(in) :: i_pvr
!
      integer(kind = kint) :: ist, i, icheck_ncomp(1)
      real(kind = kreal), allocatable :: pvr_param_tmp(:,:)
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
      if(pvr%i_ambient .gt. 0) then
        pvr_lighting_real(1,i_pvr) = pvr%ambient_coef_ctl
      else
        pvr_lighting_real(1,i_pvr) = 0.5
      end if
!
      if(pvr%i_diffuse .gt. 0) then
        pvr_lighting_real(2,i_pvr) = pvr%diffuse_coef_ctl
      else
        pvr_lighting_real(2,i_pvr) = 5.0
      end if
!
      if(pvr%i_specular .gt. 0) then
        pvr_lighting_real(3,i_pvr) = pvr%specular_coef_ctl
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
      if( (pvr%i_movie_rot_frame*pvr%i_movie_rot_axis) .gt. 0) then
        if     ( pvr%rotation_axis_ctl .eq. 'x'                         &
     &      .or. pvr%rotation_axis_ctl .eq. 'X' ) then
          iprm_pvr_rot(1,i_pvr) = 1
          iflag_rotation = 1
        else if( pvr%rotation_axis_ctl .eq. 'y'                         &
     &      .or. pvr%rotation_axis_ctl .eq. 'Y' ) then
          iprm_pvr_rot(1,i_pvr) = 2
          iflag_rotation = 1
        else if( pvr%rotation_axis_ctl .eq. 'z'                         &
     &      .or. pvr%rotation_axis_ctl .eq. 'Z' ) then
          iprm_pvr_rot(1,i_pvr) = 3
          iflag_rotation = 1
        else
          iprm_pvr_rot(1,i_pvr) = 0
        end if
!
        iprm_pvr_rot(2,i_pvr) = pvr%num_frames_ctl
        max_rotation = max(max_rotation, iprm_pvr_rot(2,i_pvr))
      else
        iprm_pvr_rot(1,i_pvr) = 0
        iprm_pvr_rot(2,i_pvr) = 1
      end if
!
!    set colormap setting
!
      id_pvr_color(1,i_pvr) = 2
      if( pvr%i_colormap .gt. 0) then
        if   ( pvr%pvr_colormap_ctl .eq. 'rainbow'                      &
     &    .or. pvr%pvr_colormap_ctl .eq. 'Rainbow'                      &
     &    .or. pvr%pvr_colormap_ctl .eq. 'RAINBOW' ) then
          id_pvr_color(1,i_pvr) = 2
        else if( pvr%pvr_colormap_ctl .eq. 'red_blue'                   &
     &      .or. pvr%pvr_colormap_ctl .eq. 'Red_blue'                   &
     &      .or. pvr%pvr_colormap_ctl .eq. 'Red_Blue'                   &
     &      .or. pvr%pvr_colormap_ctl .eq. 'READ_BLUE' ) then
          id_pvr_color(1,i_pvr) = 1
        else if( pvr%pvr_colormap_ctl .eq. 'grayscale'                  &
     &      .or. pvr%pvr_colormap_ctl .eq. 'Grayscale'                  &
     &      .or. pvr%pvr_colormap_ctl .eq. 'GRAYSCALE' ) then
          id_pvr_color(1,i_pvr) = 3
        end if
      end if
!
      id_pvr_color(2,i_pvr) = 1
      num_pvr_datamap_pnt(i_pvr) = 2
      if( pvr%i_data_mapping .gt. 0) then
        if     ( pvr%pvr_data_mapping_ctl .eq. 'nonlinear'              &
     &      .or. pvr%pvr_data_mapping_ctl .eq. 'Nonlinear'              &
     &      .or. pvr%pvr_data_mapping_ctl .eq. 'NONLINEAR'              &
     &      .or. pvr%pvr_data_mapping_ctl .eq. 'colormap_list'          &
     &      .or. pvr%pvr_data_mapping_ctl .eq. 'Colormap_list'          &
     &      .or. pvr%pvr_data_mapping_ctl .eq. 'COLORMAP_LIST' ) then
          if(pvr%colortbl_ctl%num .gt. 0) then
            id_pvr_color(2,i_pvr) = 3
            num_pvr_datamap_pnt(i_pvr) = pvr%colortbl_ctl%num
          end if
        else if( pvr%pvr_data_mapping_ctl .eq. 'linear'                 &
     &      .or. pvr%pvr_data_mapping_ctl .eq. 'Linear'                 &
     &      .or. pvr%pvr_data_mapping_ctl .eq. 'LINEAR'                 &
     &      .or. pvr%pvr_data_mapping_ctl .eq. 'minmax'                 &
     &      .or. pvr%pvr_data_mapping_ctl .eq. 'Minmax'                 &
     &      .or. pvr%pvr_data_mapping_ctl .eq. 'MINMAX' ) then
          if( (pvr%i_pvr_range_min*pvr%i_pvr_range_max) .gt. 0) then
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
        pvr_datamap_param(1,ist+1) = pvr%pvr_range_min_ctl
        pvr_datamap_param(1,ist+2) = pvr%pvr_range_max_ctl
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
      if( pvr%i_opacity_style .gt. 0) then
!        if     ( pvr%opacity_style_ctl .eq. 'intense_chenge'           &
!     &      .or. pvr%opacity_style_ctl .eq. 'Intense_chenge'           &
!     &      .or. pvr%opacity_style_ctl .eq. 'Intense_Chenge'           &
!     &      .or. pvr%opacity_style_ctl .eq. 'INTENSE_CHENGE' ) then
!          id_pvr_color(3,i_pvr) = 2
!        end if
        if( pvr%opacity_style_ctl .eq. 'point_delta'                    &
     &      .or. pvr%opacity_style_ctl .eq. 'Point_delta'               &
     &      .or. pvr%opacity_style_ctl .eq. 'Point_Delta'               &
     &      .or. pvr%opacity_style_ctl .eq. 'POINT_DELTA' ) then
!
          if( pvr%opacity_ctl%num .gt. 0) then
            id_pvr_color(3,i_pvr) = 3
            num_opacity_pnt(i_pvr) = pvr%opacity_ctl%num
          end if
!
        else if( pvr%opacity_style_ctl .eq. 'point_ranges'              &
     &      .or. pvr%opacity_style_ctl .eq. 'Point_ranges'              &
     &      .or. pvr%opacity_style_ctl .eq. 'Point_Ranges'              &
     &      .or. pvr%opacity_style_ctl .eq. 'POINT_RANGES' ) then
!
          if( pvr%opacity_ctl%num .gt. 0) then
            id_pvr_color(3,i_pvr) = 4
            num_opacity_pnt(i_pvr) = pvr%opacity_ctl%num
          end if
!
        else if( pvr%opacity_style_ctl .eq. 'point_linear'              &
     &      .or. pvr%opacity_style_ctl .eq. 'Point_linear'              &
     &      .or. pvr%opacity_style_ctl .eq. 'Point_Linear'              &
     &      .or. pvr%opacity_style_ctl .eq. 'POINT_LINEAR' ) then
!
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
      if( pvr%i_constant_opacity .gt. 0) then
        pvr_opacity_param(3,ist) = pvr%constant_opacity_ctl
      else
        pvr_opacity_param(3,ist) = 0.001
      end if
      pvr_max_opacity(i_pvr)                                            &
     &       = max(pvr_max_opacity(i_pvr),pvr_opacity_param(3,ist))
!
!    set colorbar setting
!
      iflag_pvr_colorbar(i_pvr) = 0
      if( pvr%i_colorbar_switch .gt. 0) then
        if   ( pvr%colorbar_switch_ctl .eq. 'on'                        &
     &    .or. pvr%colorbar_switch_ctl .eq. 'On'                        &
     &    .or. pvr%colorbar_switch_ctl .eq. 'ON'                        &
     &    .or. pvr%colorbar_switch_ctl .eq. 'data'                      &
     &    .or. pvr%colorbar_switch_ctl .eq. 'Data'                      &
     &    .or. pvr%colorbar_switch_ctl .eq. 'DATA'                      &
     &    .or. pvr%colorbar_switch_ctl .eq. 'equi_data'                 &
     &    .or. pvr%colorbar_switch_ctl .eq. 'Equi_data'                 &
     &    .or. pvr%colorbar_switch_ctl .eq. 'Equi_Data'                 &
     &    .or. pvr%colorbar_switch_ctl .eq. 'EQUI_DATA') then
          iflag_pvr_colorbar(i_pvr) = 1
        end if
      end if
!
      if ( iflag_pvr_colorbar(i_pvr) .gt. 0) then
        if( pvr%i_colorbar_scale .gt. 0) then
          if  ( pvr%colorbar_scale_ctl .eq. 'on'                        &
     &     .or. pvr%colorbar_scale_ctl .eq. 'On'                        &
     &     .or. pvr%colorbar_scale_ctl .eq. 'ON') then
            iflag_pvr_cbar_nums(i_pvr) = 1
!
            if (pvr%i_pvr_font_size .gt. 0) then
              iscale_font(i_pvr) = pvr%font_size_ctl
            else
              iscale_font(i_pvr) = 1
            end if
!
            if (pvr%i_pvr_numgrid_cbar .gt. 0) then
              ntick_pvr_colorbar(i_pvr) = pvr%numgrid_pvr_cbar_ctl + 2
            else
              ntick_pvr_colorbar(i_pvr) = 3
            end if
!
            if (pvr%i_zeromarker_flag .gt. 0) then
              if(    pvr%zeromarker_flag_ctl .eq. 'on'                  &
     &         .or.  pvr%zeromarker_flag_ctl .eq. 'On'                  &
     &         .or.  pvr%zeromarker_flag_ctl .eq. 'ON') then
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
        if( pvr%i_cbar_range .gt. 0) then
          cbar_range(1:2,i_pvr) = pvr%cbar_range_ctl(1:2)
        end if
!
      end if
!
!   set transfer matrix
!
      call s_set_pvr_modelview_matrix(i_pvr, pvr%mat)
!
      end subroutine set_control_pvr
!
!  ---------------------------------------------------------------------
!
      end module set_control_each_pvr
