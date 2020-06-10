!
!      module t_control_params_4_iso
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine count_control_4_iso                                  &
!!     &         (iso_c, ele_grp, num_nod_phys, phys_nod_name,          &
!!     &          iso_fld, iso_param, iso_def, iso_file_IO)
!!      subroutine set_control_4_iso                                    &
!!     &         (iso_c, ele_grp, num_nod_phys, phys_nod_name,          &
!!     &          iso_fld, iso_param, iso_def)
!!        type(group_data), intent(in) :: ele_grp
!!        type(iso_ctl), intent(in) :: iso_c
!!        type(phys_data), intent(inout) :: iso_fld
!!        type(psf_parameters), intent(inout) :: iso_param
!!        type(isosurface_define), intent(inout) :: iso_def
!!        type(field_IO_params), intent(inout) :: iso_file_IO
!
      module t_control_params_4_iso
!
      use m_precision
!
      implicit  none
!
!
      character(len=kchara), parameter :: default_iso_prefix = 'iso'
!
      character(len=kchara), parameter :: cflag_const_iso = 'const'
      character(len=kchara), parameter :: cflag_field_iso = 'field'
!
      integer(kind = kint), parameter :: iflag_constant_iso = -1
      integer(kind = kint), parameter :: iflag_field_iso =     1
!
!
      type isosurface_define
        integer(kind = kint) :: id_isosurf_data
        integer(kind = kint) :: id_isosurf_comp
        real(kind=kreal) :: isosurf_value
        real(kind=kreal) :: result_value_iso
!
        integer(kind = kint) :: id_iso_result_type
      end type isosurface_define
!
      private :: default_iso_prefix
      private :: set_merged_iso_file_ctl, choose_para_iso_file_format
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_control_4_iso                                    &
     &         (iso_c, ele_grp, num_nod_phys, phys_nod_name,            &
     &          iso_fld, iso_param, iso_def, iso_file_IO)
!
      use m_file_format_switch
      use parallel_ucd_IO_select
      use set_field_comp_for_viz
      use t_group_data
      use t_file_IO_parameter
      use t_control_data_4_iso
      use t_phys_data
      use t_psf_patch_data
!
      use set_area_4_viz
      use skip_comment_f
!
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(iso_ctl), intent(in) :: iso_c
      type(phys_data), intent(inout) :: iso_fld
      type(psf_parameters), intent(inout) :: iso_param
      type(isosurface_define), intent(inout) :: iso_def
      type(field_IO_params), intent(inout) :: iso_file_IO
!
      character(len=kchara) :: tmpchara
!
!
      call set_merged_iso_file_ctl(default_iso_prefix,                  &
     &    iso_c%iso_file_head_ctl, iso_c%iso_output_type_ctl,           &
     &    iso_file_IO)
      if((iso_file_IO%iflag_format/iflag_single) .eq. 0) then
        iso_file_IO%iflag_format = iso_file_IO%iflag_format             &
     &                            + iflag_single
      end if
!
      if     (iso_c%iso_out_field_ctl%num .gt. 0                        &
     &  .and. iso_c%result_value_iso_ctl%iflag .gt. 0) then
        tmpchara = iso_c%iso_result_type_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_field_iso)) then
          iso_def%id_iso_result_type = iflag_field_iso
        else if(cmp_no_case(tmpchara, cflag_const_iso)) then
          iso_def%id_iso_result_type = iflag_constant_iso
        end if
!
      else if(iso_c%iso_out_field_ctl%num .eq. 0) then
        iso_def%id_iso_result_type = iflag_constant_iso
      else
        iso_def%id_iso_result_type = iflag_field_iso
      end if
!
      iso_fld%num_phys =     izero
      iso_fld%num_phys_viz = izero
      if      (iso_def%id_iso_result_type .eq. iflag_constant_iso) then
        iso_fld%num_phys =     ione
        iso_fld%num_phys_viz = ione
      else if (iso_def%id_iso_result_type .eq. iflag_field_iso) then
        call check_field_4_viz(num_nod_phys, phys_nod_name,             &
     &     iso_c%iso_out_field_ctl%num, iso_c%iso_out_field_ctl%c1_tbl, &
     &     iso_fld%num_phys, iso_fld%num_phys_viz)
      end if
!
      call count_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &    iso_c%iso_area_ctl%num, iso_c%iso_area_ctl%c_tbl,             &
     &    iso_param%nele_grp_area)
!
      end subroutine count_control_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_iso                                      &
     &         (iso_c, ele_grp, num_nod_phys, phys_nod_name,            &
     &          iso_fld, iso_param, iso_def)
!
      use set_area_4_viz
      use set_field_comp_for_viz
      use t_group_data
      use t_control_data_4_iso
      use t_phys_data
      use t_psf_patch_data
!
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(iso_ctl), intent(in) :: iso_c
      type(phys_data), intent(inout) :: iso_fld
      type(psf_parameters), intent(inout) :: iso_param
      type(isosurface_define), intent(inout) :: iso_def
!
      integer(kind = kint) :: ncomp, ncomp_org
      character(len=kchara) :: tmpchara
!
!
!
      call set_one_component_4_viz(num_nod_phys, phys_nod_name,         &
     &    iso_c%isosurf_data_ctl%charavalue,                            &
     &    iso_c%isosurf_comp_ctl%charavalue,                            &
     &    iso_def%id_isosurf_data, iso_def%id_isosurf_comp,             &
     &    ncomp, ncomp_org, tmpchara)
!
      iso_def%isosurf_value = iso_c%isosurf_value_ctl%realvalue
!
      call alloc_output_comps_psf(iso_fld%num_phys, iso_param)
      if (iso_def%id_iso_result_type .eq. iflag_constant_iso) then
        iso_def%result_value_iso = iso_c%result_value_iso_ctl%realvalue
        iso_param%id_output(1) = iflag_constant_iso
        iso_param%icomp_output(1) = 0
        iso_fld%num_component(1) = 1
        iso_fld%phys_name(1) =     'color'
!
      else if (iso_def%id_iso_result_type .eq. iflag_field_iso) then
        call set_components_4_viz(num_nod_phys, phys_nod_name,          &
     &     iso_c%iso_out_field_ctl%num, iso_c%iso_out_field_ctl%c1_tbl, &
     &     iso_c%iso_out_field_ctl%c2_tbl, iso_fld%num_phys,            &
     &     iso_param%id_output, iso_param%icomp_output,                 &
     &     iso_fld%num_component, iso_param%ncomp_org,                  &
     &     iso_fld%phys_name)
      end if
!
      call alloc_area_group_psf(iso_param)
      call s_set_area_4_viz(ele_grp%num_grp, ele_grp%grp_name,          &
     &     iso_c%iso_area_ctl%num, iso_c%iso_area_ctl%c_tbl,            &
     &     iso_param%nele_grp_area, iso_param%id_ele_grp_area)
!
      end subroutine set_control_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine set_merged_iso_file_ctl(default_prefix,                &
     &          file_prefix_ctl, file_format_ctl, ucd_param)
!
      use t_control_array_character
      use t_file_IO_parameter
!
      character(len = kchara), intent(in) :: default_prefix
      type(read_character_item), intent(in) :: file_prefix_ctl
      type(read_character_item), intent(in) :: file_format_ctl
      type(field_IO_params), intent(inout) :: ucd_param
!
!
      ucd_param%iflag_IO = file_prefix_ctl%iflag
      if(ucd_param%iflag_IO .eq. 0) then
        ucd_param%iflag_format = -1
        ucd_param%file_prefix = default_prefix
        return
      else
        ucd_param%file_prefix = file_prefix_ctl%charavalue
      end if
!
      ucd_param%iflag_format                                            &
     &        = choose_para_iso_file_format(file_format_ctl%charavalue, &
     &                                      file_format_ctl%iflag)
!
      end subroutine set_merged_iso_file_ctl
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function choose_para_iso_file_format         &
     &                            (file_fmt_ctl, i_file_fmt)
!
      use m_merged_field_fmt_labels
      use m_field_file_format
      use t_multi_flag_labels
!
      integer(kind= kint), intent(in) :: i_file_fmt
      character(len=kchara), intent(in) :: file_fmt_ctl
!
!
      call init_mgd_field_type_flags
!
      if(i_file_fmt .eq. 0) then
        choose_para_iso_file_format = iflag_sgl_vtk
        return
      end if
!
      if     (check_mul_flags(file_fmt_ctl, mgd_udt_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_ucd_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, udt_flags)                  &
     &   .or. check_mul_flags(file_fmt_ctl, ucd_flags)) then
        choose_para_iso_file_format = iflag_sgl_ucd
!
      else if(check_mul_flags(file_fmt_ctl, mgd_udt_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_ucd_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, udt_gz_flags)               &
     &   .or. check_mul_flags(file_fmt_ctl, ucd_gz_flags)) then
        choose_para_iso_file_format = iflag_sgl_ucd_gz
!
      else if(check_mul_flags(file_fmt_ctl, mgd_vtd_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_vtk_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, vtd_flags)                  &
     &   .or. check_mul_flags(file_fmt_ctl, vtk_flags)) then
        choose_para_iso_file_format = iflag_sgl_vtk
!
      else if(check_mul_flags(file_fmt_ctl, mgd_vtd_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_vtk_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, vtd_gz_flags)               &
     &   .or. check_mul_flags(file_fmt_ctl, vtk_gz_flags)) then
        choose_para_iso_file_format = iflag_sgl_vtk_gz
!
!
      else if(check_mul_flags(file_fmt_ctl, mgd_iso_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_psf_labels)             &
     &   .or. check_mul_flags(file_fmt_ctl, iso_flags)                  &
     &   .or. check_mul_flags(file_fmt_ctl, psf_flags)) then
        choose_para_iso_file_format = iflag_sgl_ucd_bin
!
      else if(check_mul_flags(file_fmt_ctl, mgd_iso_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, mgd_psf_gz_labels)          &
     &   .or. check_mul_flags(file_fmt_ctl, iso_gz_flags)               &
     &   .or. check_mul_flags(file_fmt_ctl, psf_gz_flags)) then
        choose_para_iso_file_format = iflag_sgl_ucd_bin_gz
      else
        choose_para_iso_file_format = iflag_sgl_vtk
      end if
      call dealloc_mgd_field_type_flags
!
      end function choose_para_iso_file_format
!
! -----------------------------------------------------------------------
!
      end module t_control_params_4_iso
