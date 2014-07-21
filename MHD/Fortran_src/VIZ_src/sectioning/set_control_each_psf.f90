!set_control_each_psf.f90
!      module set_control_each_psf
!
!      subroutine count_control_4_psf(i_psf, psf,                       &
!     &          num_mat, mat_name, num_nod_phys, phys_nod_name)
!      subroutine set_control_4_psf(i_psf, psf, num_mat, mat_name,      &
!     &          num_surf, surf_name, num_nod_phys, phys_nod_name)
!
!        programmed by H.Matsui on May. 2006
!
      module set_control_each_psf
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_control_data_4_psf
      use m_control_params_4_psf
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
      subroutine count_control_4_psf(i_psf, psf, num_mat, mat_name,     &
     &          num_nod_phys, phys_nod_name, psf_fld, psf_param)
!
      use m_field_file_format
      use m_file_format_switch
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
      integer(kind = kint), intent(in) :: i_psf
      type(psf_ctl), intent(in) :: psf
      type(phys_data), intent(inout) :: psf_fld
      type(psf_parameters), intent(inout) :: psf_param
!
!
      if(psf%i_psf_file_head .gt. 0) then
        psf_header(i_psf) =  psf%psf_file_head_ctl
      else
        psf_header(i_psf) =  'psf'
      end if
!
      call choose_ucd_file_format(psf%psf_output_type_ctl,              &
     &    psf%i_psf_out_type, itype_psf_file(i_psf) )
!
      call check_field_4_viz(num_nod_phys, phys_nod_name,               &
     &   psf%psf_out_field_ctl%num, psf%psf_out_field_ctl%c1_tbl,       &
     &   psf_fld%num_phys)
!
      call count_area_4_viz(num_mat, mat_name,                          &
     &    psf%psf_area_ctl%num, psf%psf_area_ctl%c_tbl,                 &
     &    psf_param%nele_grp_area)
!
      if (psf_param%nele_grp_area .eq. 0)                               &
     &  call calypso_MPI_abort(100, 'set correct element group')
!
      end subroutine count_control_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_psf(i_psf, psf, num_mat, mat_name,       &
     &          num_surf, surf_name, num_nod_phys, phys_nod_name,       &
     &          psf_fld, psf_param)
!
      use set_cross_section_coefs
      use set_area_4_viz
      use t_phys_data
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: i_psf
      type(psf_ctl), intent(inout) :: psf
      type(phys_data), intent(inout) :: psf_fld
      type(psf_parameters), intent(inout) :: psf_param
!
!
      if     (psf%section_method_ctl.eq. 'equation') then
        id_section_method(i_psf) = 1
        call set_coefs_4_psf(psf%psf_coefs_ctl%num,                     &
     &      psf%psf_coefs_ctl%c_tbl,  psf%psf_coefs_ctl%vect,           &
     &      const_psf(1,i_psf) )
        call deallocate_psf_coefs_ctl(psf)
!
      else if(psf%section_method_ctl.eq. 'sphere') then
        id_section_method(i_psf) = 2
        call set_coefs_4_sphere(psf, const_psf(1,i_psf))
!
      else if(psf%section_method_ctl.eq. 'ellipsoid') then
        id_section_method(i_psf) = 3
        call set_coefs_4_ellipsode(psf, const_psf(1,i_psf) )
        call deallocate_psf_axis_ctl(psf)
        call deallocate_psf_center_ctl(psf)
!
      else if(psf%section_method_ctl.eq. 'hyperboloid') then
        id_section_method(i_psf) = 4
        call set_coefs_4_hyperboloide(psf, const_psf(1,i_psf) )
        call deallocate_psf_axis_ctl(psf)
        call deallocate_psf_center_ctl(psf)
!
      else if(psf%section_method_ctl.eq. 'paraboloid') then
        id_section_method(i_psf) = 5
        call set_coefs_4_parabolic(psf, const_psf(1,i_psf) )
        call deallocate_psf_axis_ctl(psf)
        call deallocate_psf_center_ctl(psf)
!
      else if(psf%section_method_ctl.eq. 'group') then
        id_section_method(i_psf) = 0
        call set_surf_grp_id_4_viz(num_surf, surf_name,                 &
     &      psf%psf_group_name_ctl, id_psf_group(i_psf) )
      end if
!
!
      call alloc_output_comps_psf(psf_fld%num_phys, psf_param)
      if ( psf_fld%num_phys .gt. 0 ) then
        call set_components_4_viz(num_nod_phys, phys_nod_name,          &
     &      psf%psf_out_field_ctl%num, psf%psf_out_field_ctl%c1_tbl,    &
     &      psf%psf_out_field_ctl%c2_tbl, psf_fld%num_phys,             &
     &      psf_param%id_output, psf_param%icomp_output,                &
     &      psf_fld%num_component, psf_param%ncomp_org,                 &
     &      psf_fld%phys_name)
      end if
!
      call alloc_area_group_psf(psf_param)
      call s_set_area_4_viz(num_mat, mat_name,                          &
     &    psf%psf_area_ctl%num, psf%psf_area_ctl%c_tbl,                 &
     &    psf_param%nele_grp_area, psf_param%id_ele_grp_area)
!
      end subroutine set_control_4_psf
!
!  ---------------------------------------------------------------------
!
      end module set_control_each_psf
