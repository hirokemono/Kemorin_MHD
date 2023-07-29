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
!!      subroutine init_mgd_field_type_flags()
!!      subroutine dealloc_mgd_field_type_flags()
!!
!!      integer(c_int) function num_label_psf_format_f() bind(c)
!!      subroutine set_ctl_label_psf_format_f(names)  bind(c)
!!      integer(c_int) function num_label_iso_format_f() bind(c)
!!      subroutine set_ctl_label_iso_format_f(names) bind(c)
!!
!!      subroutine set_primary_psf_format_flag_f(name_c)  bind(c)
!!      subroutine set_primary_iso_format_flag_f(name_c)  bind(c)
!!
!!      subroutine iso_file_format_list_array(array_c)
!!      subroutine psf_file_format_list_array(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
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
!
      implicit    none
!
!>     Label for splitted UCD into mesh and data: 'UDT'
      character(len = kchara) :: hd_udt
!>     Label for UCD 'UCD'
      character(len = kchara) :: hd_ucd
!>     Label for VTK: 'VTK'
      character(len = kchara) :: hd_vtk
!>     Label for splitted VTK mesh and data: 'VTD'
      character(len = kchara) :: hd_vtd
!>     Label for isosurface binary: 'ISO'
      character(len = kchara) :: hd_iso
!>     Label for sections binary: 'PSF'
      character(len = kchara) :: hd_psf
!
!>     Label for gzipped splitted UCD into mesh and data: 'UDT_gzip'
      character(len = kchara) :: hd_udt_gz
!>     Label for gzipped UCD: 'UCD_gzip'
      character(len = kchara) :: hd_ucd_gz
!>     Label for gzipped VTK: 'VTK_gzip'
      character(len = kchara) :: hd_vtk_gz
!>     Label for gzipped splitted VTK mesh and data: 'VTD_gzip'
      character(len = kchara) :: hd_vtd_gz
!>     Label for gzipped isosurface binary: 'ISO_gzip'
      character(len = kchara) :: hd_iso_gz
!>     Label for gzipped sections binary: 'PSF_gzip'
      character(len = kchara) :: hd_psf_gz
!
!
!>     Label for merged splitted UCD into mesh and data: 'merged_UDT'
      character(len = kchara) :: hd_mgd_udt
!>     Label for merged UCD: 'merged_UCD'
      character(len = kchara) :: hd_mgd_ucd
!>     Label for merged VTK: 'merged_VTK'
      character(len = kchara) :: hd_mgd_vtk
!>     Label for merged splitted VTK mesh and data: 'merged_VTD'
      character(len = kchara) :: hd_mgd_vtd
!>     Label for merged isosurface binary: 'merged_ISO'
      character(len = kchara) :: hd_mgd_iso
!>     Label for merged sections binary: 'merged_PSF'
      character(len = kchara) :: hd_mgd_psf
!
!>     Label for gzipped merged splitted UCD: 'merged_UDT_gz'
      character(len = kchara) :: hd_mgd_udt_gz
!>     Label for gzipped merged UCD: 'merged_UCD_gz'
      character(len = kchara) :: hd_mgd_ucd_gz
!>     Label for gzipped merged VTK: 'merged_VTK_gz'
      character(len = kchara) :: hd_mgd_vtk_gz
!>     Label for gzipped merged VTK mesh and field: 'merged_VTD_gz'
      character(len = kchara) :: hd_mgd_vtd_gz
!>     Label for gzipped merged isosurface binary: 'merged_ISO_gz'
      character(len = kchara) :: hd_mgd_iso_gz
!>     Label for gzipped merged sections binary: 'merged_PSF_gz'
      character(len = kchara) :: hd_mgd_psf_gz
!>     Label for parallel HDF5: 'HDF5'
      character(len = kchara) :: hd_mgd_hdf5 = hdf5_names(1)
!
!
      integer(kind = kint), parameter :: n_label_psf_format = 12
      integer(kind = kint), parameter :: n_label_iso_format =  6
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_psf_format_f() bind(c)
      num_label_psf_format_f = n_label_psf_format
      return
      end function num_label_psf_format_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_psf_format_f(names_c)  bind(c)
!
      type(C_ptr), value :: names_c
      character(len=kchara), pointer :: names_f(:)
!
      call c_f_pointer(names_c, names_f, [n_label_psf_format])
      call set_ctl_label_psf_format(names_f)
      end subroutine set_ctl_label_psf_format_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_iso_format_f() bind(c)
      num_label_iso_format_f = n_label_iso_format
      return
      end function num_label_iso_format_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_iso_format_f(names_c)  bind(c)
!
      type(C_ptr), value :: names_c
      character(len=kchara), pointer :: names_f(:)
!
      call c_f_pointer(names_c, names_f, [n_label_iso_format])
      call set_ctl_label_iso_format(names_f)
      end subroutine set_ctl_label_iso_format_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_primary_psf_format_flag_f(name_c)  bind(c)
!
      use skip_comment_f
      use set_sections_file_ctl
!
      type(C_ptr), value :: name_c
!
      integer(kind = kint) ::  i_format
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(name_c, name_f, [1])
      i_format = sel_psf_file_format(name_f(1))
      call set_primary_psf_format_flag(i_format, name_f(1))
!
      end subroutine set_primary_psf_format_flag_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_primary_iso_format_flag_f(name_c)  bind(c)
!
      use skip_comment_f
      use set_isosurface_file_ctl
!
      type(C_ptr), value :: name_c
!
      integer(kind = kint) ::  i_format
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(name_c, name_f, [1])
      name_f(1) = fill_from_null(name_f(1))
      i_format = sel_iso_file_format(name_f(1))
      call set_primary_psf_format_flag(i_format, name_f(1))
!
      end subroutine set_primary_iso_format_flag_f
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_psf_file_format_flags
!
      call init_mgd_field_type_flags()
!
      hd_udt = udt_name(1)
      hd_ucd = ucd_name(1)
      hd_vtk = vtk_name(1)
      hd_vtd = vtd_name(1)
      hd_iso = iso_name(1)
      hd_psf = psf_name(1)
!
      hd_udt_gz = udt_gz_flags%flags(1)
      hd_ucd_gz = ucd_gz_flags%flags(1)
      hd_vtk_gz = vtk_gz_flags%flags(1)
      hd_vtd_gz = vtd_gz_flags%flags(1)
      hd_iso_gz = iso_gz_flags%flags(1)
      hd_psf_gz = psf_gz_flags%flags(1)
!
      hd_mgd_udt = mgd_udt_labels%flags(1)
      hd_mgd_ucd = mgd_ucd_labels%flags(1)
      hd_mgd_vtk = mgd_vtk_labels%flags(1)
      hd_mgd_vtd = mgd_vtd_labels%flags(1)
      hd_mgd_iso = mgd_iso_labels%flags(1)
      hd_mgd_psf = mgd_psf_labels%flags(1)
!
      hd_mgd_udt_gz = mgd_udt_gz_labels%flags(2)
      hd_mgd_ucd_gz = mgd_ucd_gz_labels%flags(2)
      hd_mgd_vtk_gz = mgd_vtk_gz_labels%flags(2)
      hd_mgd_vtd_gz = mgd_vtd_gz_labels%flags(2)
      hd_mgd_iso_gz = mgd_iso_gz_labels%flags(2)
      hd_mgd_psf_gz = mgd_psf_gz_labels%flags(2)
!
      hd_mgd_hdf5 = hdf5_names(1)
!
      call dealloc_mgd_field_type_flags()
!
      end subroutine init_psf_file_format_flags
!
! -----------------------------------------------------------------------
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
      call init_psf_file_format_flags
!
      if     (i_format .eq. iflag_sgl_udt) then
        prim_name = hd_udt
      else if(i_format .eq. iflag_sgl_udt_gz) then
        prim_name = hd_udt_gz
!
      else if(i_format .eq. iflag_sgl_ucd) then
        prim_name = hd_ucd
      else if(i_format .eq. iflag_sgl_ucd_gz) then
        prim_name = hd_ucd_gz
!
      else if(i_format .eq. iflag_sgl_vtd) then
        prim_name = hd_vtd
      else if(i_format .eq. iflag_sgl_vtd_gz) then
        prim_name = hd_vtd_gz
!
      else if(i_format .eq. iflag_sgl_vtk) then
        prim_name = hd_vtk
      else if(i_format .eq. iflag_sgl_vtk_gz) then
        prim_name = hd_vtk_gz
!
      else if(i_format .eq. iflag_sgl_ucd_bin) then
        prim_name = hd_iso
      else if(i_format .eq. iflag_sgl_ucd_bin_gz) then
        prim_name = hd_iso_gz
!
      else if(i_format .eq. iflag_sgl_udt_bin) then
        prim_name = hd_psf
      else if(i_format .eq. iflag_sgl_udt_bin_gz) then
        prim_name = hd_psf_gz
      else
        prim_name = hd_vtk
      end if
      name_c = trim(prim_name) // char(0)
!
      end subroutine set_primary_psf_format_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ctl_label_psf_format(names)
!
      character(len=kchara), intent(inout) :: names(n_label_psf_format)
!
!
      call init_psf_file_format_flags
!
      call set_control_labels(hd_udt, names( 1))
      call set_control_labels(hd_ucd, names( 2))
      call set_control_labels(hd_vtk, names( 3))
      call set_control_labels(hd_vtd, names( 4))
      call set_control_labels(hd_iso, names( 5))
      call set_control_labels(hd_psf, names( 6))
!
      call set_control_labels(hd_udt_gz, names( 7))
      call set_control_labels(hd_ucd_gz, names( 8))
      call set_control_labels(hd_vtk_gz, names( 9))
      call set_control_labels(hd_vtd_gz, names(10))
      call set_control_labels(hd_iso_gz, names(11))
      call set_control_labels(hd_psf_gz, names(12))
!
      end subroutine set_ctl_label_psf_format
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_label_iso_format(names)
!
      character(len=kchara), intent(inout) :: names(n_label_iso_format)
!
!
      call init_psf_file_format_flags
!
      call set_control_labels(hd_ucd, names( 1))
      call set_control_labels(hd_vtk, names( 2))
      call set_control_labels(hd_iso, names( 3))
!
      call set_control_labels(hd_ucd_gz, names( 4))
      call set_control_labels(hd_vtk_gz, names( 5))
      call set_control_labels(hd_iso_gz, names( 6))
!
      end subroutine set_ctl_label_iso_format
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine psf_file_format_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(hd_udt, array_c)
      call append_c_to_ctl_array(hd_ucd, array_c)
      call append_c_to_ctl_array(hd_vtk, array_c)
      call append_c_to_ctl_array(hd_vtd, array_c)
      call append_c_to_ctl_array(hd_iso, array_c)
      call append_c_to_ctl_array(hd_psf, array_c)
!
      call append_c_to_ctl_array(hd_udt_gz, array_c)
      call append_c_to_ctl_array(hd_ucd_gz, array_c)
      call append_c_to_ctl_array(hd_vtk_gz, array_c)
      call append_c_to_ctl_array(hd_vtd_gz, array_c)
      call append_c_to_ctl_array(hd_iso_gz, array_c)
      call append_c_to_ctl_array(hd_psf_gz, array_c)
!
      end subroutine psf_file_format_list_array
!
! ----------------------------------------------------------------------
!
      subroutine iso_file_format_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(hd_ucd,    array_c)
      call append_c_to_ctl_array(hd_vtk,    array_c)
      call append_c_to_ctl_array(hd_iso,    array_c)
      call append_c_to_ctl_array(hd_ucd_gz, array_c)
      call append_c_to_ctl_array(hd_vtk_gz, array_c)
      call append_c_to_ctl_array(hd_iso_gz, array_c)
!
      end subroutine iso_file_format_list_array
!
! ----------------------------------------------------------------------
!
      end module m_field_file_fmt_list
