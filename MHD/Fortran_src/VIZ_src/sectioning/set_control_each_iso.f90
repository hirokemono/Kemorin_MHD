!set_control_each_iso.f90
!      module set_control_each_iso
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine count_control_4_iso(i_iso, iso,                       &
!     &          num_mat, mat_name, num_nod_phys, phys_nod_name)
!      subroutine set_control_4_iso(i_iso, iso, num_mat, mat_name,      &
!     &          num_nod_phys, phys_nod_name)
!
      module set_control_each_iso
!
      use m_precision
!
      use m_constants
      use m_control_data_4_iso
      use m_control_params_4_iso
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
      subroutine count_control_4_iso(i_iso, iso, num_mat, mat_name,     &
     &          num_nod_phys, phys_nod_name, iso_fld)
!
      use m_field_file_format
      use m_file_format_switch
      use t_phys_data
!
      use set_area_4_viz
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: i_iso
      type(iso_ctl), intent(in) :: iso
      type(phys_data), intent(inout) :: iso_fld
!
!
      if(iso%i_iso_file_head .gt. 0) then
        iso_header(i_iso) = iso%iso_file_head_ctl
      else
        iso_header(i_iso) =  'iso'
      end if
!
      call choose_ucd_file_format(iso%iso_output_type_ctl,              &
     &    iso%i_iso_out_type, itype_iso_file(i_iso) )
!
      if(iso%iso_out_field_ctl%num .eq. 0) then
        id_iso_result_type(i_iso) = iflag_constant_iso
      else
        id_iso_result_type(i_iso) = iflag_field_iso
      end if
!
      if      (id_iso_result_type(i_iso) .eq. iflag_constant_iso) then
        iso_fld%num_phys = ione
        istack_iso_output(i_iso) = istack_iso_output(i_iso-1) + ione
      else if ( id_iso_result_type(i_iso) .eq. iflag_field_iso) then
        call check_field_4_viz(num_nod_phys, phys_nod_name,             &
     &      iso%iso_out_field_ctl%num, iso%iso_out_field_ctl%c1_tbl,    &
     &      iso_fld%num_phys)
      end if
      istack_iso_output(i_iso) = istack_iso_output(i_iso-1)             &
     &                          + iso_fld%num_phys
!
      call count_area_4_viz(num_mat, mat_name,                          &
     &    iso%iso_area_ctl%num, iso%iso_area_ctl%c_tbl,                 &
     &    nele_grp_area_iso(i_iso))
      istack_grp_area_iso(i_iso) = istack_grp_area_iso(i_iso-1)         &
     &                            + nele_grp_area_iso(i_iso)
!
      end subroutine count_control_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_iso(i_iso, iso, num_mat, mat_name,       &
     &          num_nod_phys, phys_nod_name, iso_fld, iso_param)
!
      use set_area_4_viz
      use t_phys_data
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: i_iso
      type(iso_ctl), intent(in) :: iso
      type(phys_data), intent(inout) :: iso_fld
      type(psf_parameters), intent(inout) :: iso_param
!
      integer(kind = kint) :: ist, ncomp(1), ncomp_org(1)
      character(len=kchara) :: tmpchara(1)
!
!
!
      call set_components_4_viz(num_nod_phys, phys_nod_name,            &
     &    ione, iso%isosurf_data_ctl, iso%isosurf_comp_ctl,             &
     &    ione, id_isosurf_data(i_iso), id_isosurf_comp(i_iso),         &
     &    ncomp, ncomp_org, tmpchara)
!
      isosurf_value(i_iso) = iso%isosurf_value_ctl
!
      call alloc_output_comps_psf(iso_fld%num_phys, iso_param)
      if (id_iso_result_type(i_iso) .eq. iflag_constant_iso) then
        result_value_iso(i_iso) = iso%result_value_iso_ctl
        iso_param%id_output(1) = iflag_constant_iso
        iso_param%icomp_output(1) = 0
        iso_fld%num_component(1) = 1
        iso_fld%phys_name(1) =     'color'
!
      else if (id_iso_result_type(i_iso) .eq. iflag_field_iso) then
        call set_components_4_viz(num_nod_phys, phys_nod_name,          &
     &      iso%iso_out_field_ctl%num, iso%iso_out_field_ctl%c1_tbl,    &
     &      iso%iso_out_field_ctl%c2_tbl, iso_fld%num_phys,             &
     &      iso_param%id_output, iso_param%icomp_output,                &
     &      iso_fld%num_component, iso_param%ncomp_org,                 &
     &      iso_fld%phys_name)
      end if
!
      ist = istack_grp_area_iso(i_iso-1) + 1
      call s_set_area_4_viz(num_mat, mat_name,                          &
     &     iso%iso_area_ctl%num, iso%iso_area_ctl%c_tbl,                &
     &     nele_grp_area_iso(i_iso), id_ele_grp_area_iso(ist) )
!
      end subroutine set_control_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_control_each_iso
