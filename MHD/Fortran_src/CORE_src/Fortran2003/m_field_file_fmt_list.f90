!>@file   m_field_file_fmt_list.f90
!!@brief  module m_field_file_fmt_list
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief Integer flags for field data file format
!!
!!
!!@verbatim
!!      subroutine iso_file_format_list_array(array_c)
!!      subroutine psf_file_format_list_array(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
!!
!!      subroutine set_primary_psf_format_flag(i_format, name_c)
!!
!!      type(C_ptr) function c_link_psf_file_format_list()              &
!!     &          bind(C, NAME = 'c_link_psf_file_format_list')
!!      type(C_ptr) function c_link_iso_file_format_list()              &
!!     &          bind(C, NAME = 'c_link_iso_file_format_list')
!!@endverbatim
!!
!
      module m_field_file_fmt_list
!
      use ISO_C_BINDING
!
      use m_precision
      use m_file_format_labels
      use t_read_control_elements
      use t_multi_flag_labels
!
      use m_field_file_format_labels
      use m_merged_field_fmt_labels
      use t_multi_flag_labels
      use t_control_array_character
!
      implicit    none
!
      type(ctl_array_chara), save, private, target :: psf_file_fmt_list
      type(ctl_array_chara), save, private, target :: iso_flux_org_list
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_primary_psf_format_flag(i_format, name_c)
!
      use m_merged_field_fmt_labels
      use m_field_file_format
      use t_multi_flag_labels
!
      integer(kind = kint), intent(in) :: i_format
      character(len=kchara), intent(inout) :: name_c
!
      character(len=kchara) :: prim_name
!
!
      if     (i_format .eq. iflag_sgl_udt) then
        prim_name = udt_name(1)
      else if(i_format .eq. iflag_sgl_udt_gz) then
        prim_name = udt_gz_flags%flags(1)
!
      else if(i_format .eq. iflag_sgl_ucd) then
        prim_name = ucd_name(1)
      else if(i_format .eq. iflag_sgl_ucd_gz) then
        prim_name = ucd_gz_flags%flags(1)
!
      else if(i_format .eq. iflag_sgl_vtd) then
        prim_name = vtd_name(1)
      else if(i_format .eq. iflag_sgl_vtd_gz) then
        prim_name = vtd_gz_flags%flags(1)
!
      else if(i_format .eq. iflag_sgl_vtk) then
        prim_name = vtk_name(1)
      else if(i_format .eq. iflag_sgl_vtk_gz) then
        prim_name = vtk_gz_flags%flags(1)
!
      else if(i_format .eq. iflag_sgl_ucd_bin) then
        prim_name = iso_name(1)
      else if(i_format .eq. iflag_sgl_ucd_bin_gz) then
        prim_name = iso_gz_flags%flags(1)
!
      else if(i_format .eq. iflag_sgl_udt_bin) then
        prim_name = psf_name(1)
      else if(i_format .eq. iflag_sgl_udt_bin_gz) then
        prim_name = psf_gz_flags%flags(1)
      else
        prim_name = vtk_name(1)
      end if
      name_c = trim(prim_name) // char(0)
!
      end subroutine set_primary_psf_format_flag
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine psf_file_format_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
      character(len = kchara) :: tmpchara
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      tmpchara = udt_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = ucd_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = vtk_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = vtd_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = iso_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = psf_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
!
      tmpchara = udt_gz_flags%flags(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = ucd_gz_flags%flags(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = vtk_gz_flags%flags(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = vtd_gz_flags%flags(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = iso_gz_flags%flags(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = psf_gz_flags%flags(1)
      call append_c_to_ctl_array(tmpchara, array_c)
!
      end subroutine psf_file_format_list_array
!
! ----------------------------------------------------------------------
!
      subroutine iso_file_format_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
      character(len = kchara) :: tmpchara
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      tmpchara = ucd_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = vtk_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = iso_name(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = ucd_gz_flags%flags(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = vtk_gz_flags%flags(1)
      call append_c_to_ctl_array(tmpchara, array_c)
      tmpchara = iso_gz_flags%flags(1)
      call append_c_to_ctl_array(tmpchara, array_c)
!
      end subroutine iso_file_format_list_array
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_psf_file_format_list()                &
     &          bind(C, NAME = 'c_link_psf_file_format_list')
!
      if(.not. allocated(psf_file_fmt_list%c_tbl))                      &
     &           call psf_file_format_list_array(psf_file_fmt_list)
      c_link_psf_file_format_list = C_loc(psf_file_fmt_list)
      end function c_link_psf_file_format_list
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_iso_file_format_list()                &
     &          bind(C, NAME = 'c_link_iso_file_format_list')
!
      if(.not. allocated(iso_flux_org_list%c_tbl))                      &
     &           call iso_file_format_list_array(iso_flux_org_list)
      c_link_iso_file_format_list = C_loc(iso_flux_org_list)
      end function c_link_iso_file_format_list
!
! ----------------------------------------------------------------------
!
      end module m_field_file_fmt_list
