!set_control_each_fline.f90
!      module set_control_each_fline
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine count_control_4_fline                                &
!!     &         (i_fln, fln, ele, ele_grp, sf_grp)
!!      subroutine set_control_4_fline                                  &
!!     &         (i_fln, fln, ele, ele_grp, sf_grp, nod_fld)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fline_ctl), intent(inout) :: fln
!!      subroutine set_iflag_fline_used_ele(i_fln, ele, ele_grp)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: ele_grp
!
      module set_control_each_fline
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_error_IDs
      use m_machine_parameter
      use m_control_params_4_fline
      use t_control_data_4_fline
      use t_geometry_data
      use t_group_data
!
      use set_area_4_viz
      use set_field_comp_for_viz
      use set_fields_for_fieldline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_control_4_fline                                  &
     &         (i_fln, fln, ele, ele_grp, sf_grp)
!
      use set_area_4_viz
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
!
      integer(kind = kint), intent(in) :: i_fln
      type(fline_ctl), intent(in) :: fln
!
      character(len=kchara) :: character_256
!
!
      if(fln%fline_file_head_ctl%iflag .gt. 0) then
        fline_header(i_fln) =  fln%fline_file_head_ctl%charavalue
      else
        fline_header(i_fln) =  'field_line'
      end if
!
      character_256 = fln%fline_output_type_ctl%charavalue
      if     (character_256 .eq. 'ucd'                                  &
     &   .or. character_256 .eq. 'UCD'                                  &
     &   .or. character_256 .eq. 'udt'                                  &
     &   .or. character_256 .eq. 'UDT') then
        id_fline_file_type(i_fln) = 0
      else if(character_256.eq. 'OpenDX') then
        id_fline_file_type(i_fln) = 1
      end if
!
      call count_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    fln%fline_area_grp_ctl%num, fln%fline_area_grp_ctl%c_tbl,     &
     &     nele_grp_area_fline(i_fln) )
      istack_grp_area_fline(i_fln) = istack_grp_area_fline(i_fln-1)     &
     &                              + nele_grp_area_fline(i_fln)
!
      if ( nele_grp_area_fline(i_fln) .eq. 0)                           &
     &  call calypso_MPI_abort(ierr_mesh, 'set correct element group')
!
!
      character_256 = fln%starting_type_ctl%charavalue
      if      (character_256 .eq. 'surface_group'                       &
     &    .or. character_256 .eq. 'Surface_group'                       &
     &    .or. character_256 .eq. 'SURFACE_GROUP') then
        id_fline_start_type(i_fln) =  0
      else if (character_256 .eq. 'surface_list'                        &
     &    .or. character_256 .eq. 'Surface_list'                        &
     &    .or. character_256 .eq. 'SURFACE_LIST') then 
        id_fline_start_type(i_fln) =  1
      else if (character_256 .eq. 'position_list'                       &
     &    .or. character_256 .eq. 'Position_list'                       &
     &    .or. character_256 .eq. 'position_list') then 
        id_fline_start_type(i_fln) =  2
      end if
!
!
      character_256 = fln%line_direction_ctl%charavalue
      if      (character_256 .eq. 'forward'                             &
     &    .or. character_256 .eq. 'Forward'                             &
     &    .or. character_256 .eq. 'Forward') then
        id_fline_direction(i_fln) =  1
      else if (character_256 .eq. 'backward'                            &
     &    .or. character_256 .eq. 'Backward'                            &
     &    .or. character_256 .eq. 'BACKWARD') then 
        id_fline_direction(i_fln) = -1
      else if (character_256 .eq. 'both'                                &
     &    .or. character_256 .eq. 'Both'                                &
     &    .or. character_256 .eq. 'BOTH') then 
        id_fline_direction(i_fln) =  0
      end if
!
!
      if(id_fline_start_type(i_fln) .eq.  0) then
        character_256 = fln%starting_type_ctl%charavalue
        if      (character_256 .eq. 'amplitude'                         &
     &      .or. character_256 .eq. 'Amplitude'                         &
     &      .or. character_256 .eq. 'AMPLITUDE') then
          id_fline_start_dist(i_fln) =  0
        else if (character_256 .eq. 'area_size'                         &
     &      .or. character_256 .eq. 'Area_size'                         &
     &      .or. character_256 .eq. 'AREA_SIZE') then 
          id_fline_start_dist(i_fln) =  1
        end if
!
        if(fln%num_fieldline_ctl%iflag .gt. 0) then
          num_each_field_line(i_fln) = fln%num_fieldline_ctl%intvalue
        else
          num_each_field_line(i_fln) = 8
        end if
!
        if(fln%max_line_stepping_ctl%iflag .gt. 0) then
          max_line_stepping(i_fln) = fln%max_line_stepping_ctl%intvalue
        else
          max_line_stepping(i_fln) = 1000
        end if
!
        if(fln%start_surf_grp_ctl%iflag .gt. 0) then
          call set_surf_grp_id_4_viz(sf_grp%num_grp, sf_grp%grp_name,   &
     &        fln%start_surf_grp_ctl%charavalue,                        &
     &        igrp_start_fline_surf_grp(i_fln))
        end if
!
        call count_nsurf_for_starting(i_fln, ele, sf_grp)
!
      else if(id_fline_start_type(i_fln) .eq.  1) then
        if(fln%seed_surface_ctl%num .gt. 0) then
          num_each_field_line(i_fln) = fln%seed_surface_ctl%num
        end if
      else if(id_fline_start_type(i_fln) .eq.  2) then
        if(fln%seed_point_ctl%num .gt. 0) then
          num_each_field_line(i_fln) = fln%seed_point_ctl%num
        end if
      end if
      istack_each_field_line(i_fln) = istack_each_field_line(i_fln-1)   &
     &                               + num_each_field_line(i_fln)
!
!
      end subroutine count_control_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_fline                                    &
     &         (i_fln, fln, ele, ele_grp, sf_grp, nod_fld)
!
      use set_components_flags
      use set_area_4_viz
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: i_fln
      type(fline_ctl), intent(inout) :: fln
!
      integer(kind = kint) :: i, ist, ncomp(1), ncomp_org(1)
      character(len=kchara) :: tmpfield(1)
      character(len=kchara) :: tmpcomp(1)
      character(len=kchara) :: tmpchara(1)
!
!
      tmpfield(1) = fln%fline_field_ctl%charavalue
      tmpcomp(1) =  'vector'
      call set_components_4_viz                                         &
     &   (nod_fld%num_phys, nod_fld%phys_name, ione, tmpfield, tmpcomp,         &
     &    ione, ifield_4_fline(i_fln), icomp_4_fline(i_fln),            &
     &    ncomp, ncomp_org, tmpchara)
      if(icomp_4_fline(i_fln) .ne. icomp_VECTOR) then
        call calypso_MPI_abort(ierr_fld,                                &
     &      'Choose vector field for field line')
      end if
!
      tmpfield(1) = fln%fline_color_field_ctl%charavalue
      tmpcomp(1) = fln%fline_color_comp_ctl%charavalue
      call set_components_4_viz                                         &
     &   (nod_fld%num_phys, nod_fld%phys_name, ione, tmpfield, tmpcomp, &
     &    ione, ifield_linecolor(i_fln), icomp_linecolor(i_fln),        &
     &    ncomp, ncomp_org, name_color_output(i_fln))
      if(ncomp(1) .ne. ione) then
        call calypso_MPI_abort(ierr_fld,'field color should be scalar')
      end if
!
      ist = istack_grp_area_fline(i_fln-1) + 1
      call s_set_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    fln%fline_area_grp_ctl%num, fln%fline_area_grp_ctl%c_tbl,     &
     &    istack_grp_area_fline(i_fln), id_ele_grp_area_fline(ist) )
!
!
      ist = istack_each_field_line(i_fln-1)
      if(id_fline_start_type(i_fln) .eq.  0) then
        call set_isurf_for_starting(i_fln, ele, sf_grp)
      else if(id_fline_start_type(i_fln) .eq.  1) then
        do i = 1, num_each_field_line(i_fln)
          id_gl_surf_start_fline(1,i+ist)                               &
     &          = fln%seed_surface_ctl%int1(i)
          id_gl_surf_start_fline(2,i+ist)                               &
     &          = fln%seed_surface_ctl%int2(i)
        end do
      else if(id_fline_start_type(i_fln) .eq.  2) then
        do i = 1, num_each_field_line(i_fln)
          xx_surf_start_fline(1,i+ist) = fln%seed_point_ctl%vec1(i)
          xx_surf_start_fline(2,i+ist) = fln%seed_point_ctl%vec2(i)
          xx_surf_start_fline(3,i+ist) = fln%seed_point_ctl%vec3(i)
        end do
      end if
!
      end subroutine set_control_4_fline
!
!  ---------------------------------------------------------------------
!
      subroutine set_iflag_fline_used_ele(i_fln, ele, ele_grp)
!
      use set_iflag_for_used_ele
!
      integer(kind = kint), intent(in) :: i_fln
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
!
!
      integer(kind = kint) :: jst_grp
!
!
      jst_grp = istack_grp_area_fline(i_fln-1) + 1
      call s_set_iflag_for_used_ele                                     &
     &   (ele%numele, ele%interior_ele, ele_grp%num_grp,                &
     &    ele_grp%num_item, ele_grp%istack_grp, ele_grp%item_grp,       &
     &    nele_grp_area_fline(i_fln), id_ele_grp_area_fline(jst_grp),   &
     &    iflag_fline_used_ele(1,i_fln))
!
      end subroutine set_iflag_fline_used_ele
!
!  ---------------------------------------------------------------------
!
      end module set_control_each_fline
