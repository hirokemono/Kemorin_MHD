!>@file   m_line_from_psf.f90
!!@brief  module m_line_from_psf
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Pick data on line defined by two surfaces
!!
!!@verbatim
!!      subroutine pick_psf_by_sections(nd, xref)
!!      subroutine write_psf_line_data(iflag_format, file_header, istep)
!!@endverbatim
!
      module m_line_from_psf
!
      use m_precision
      use m_constants
!
      use t_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_psf_line_data(iflag_format, file_header, istep,  &
     &          line)
!
      use m_geometry_constants
      use ucd_IO_select
!
      character(len=kchara), intent(in) :: file_header
      integer(kind = kint), intent(in) :: istep, iflag_format
      type(ucd_data), intent(inout) :: line
!
      integer(kind = kint), parameter :: delete_process = -1
!
!
      line%ifmt_file =   iflag_format
      line%file_prefix = file_header
      call sel_write_ucd_file(delete_process, istep, line)
      call deallocate_ucd_mesh(line)
!
      end subroutine write_psf_line_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_psf_by_sections(nd, xref, line)
!
      use m_psf_edge_connect
      use const_section_from_triangle
!
      integer(kind = kint), intent(in) :: nd
      real(kind = kreal), intent(in) :: xref
      type(ucd_data), intent(inout) :: line
!
      real(kind = kreal), allocatable :: ref_tri(:)
      integer(kind = kint) :: i1
!
!   Count number of lines
!
      allocate(ref_tri(numnod_psf))
!
      do i1 = 1, numnod_psf
        if(nd.eq.1 .or. nd.eq.2 .or. nd.eq.3) then
          ref_tri(i1) = xx_psf(i1,nd)
        else if(nd .eq. 11) then
          ref_tri(i1) = rtp_psf(i1,1)
        else if(nd .eq. 21) then
          ref_tri(i1) = ss_psf(i1)
        else
          ref_tri(i1) = rtp_psf(i1,1)
        end if
      end do
!
!
      call allocate_edge_section_flags(numnod_psf, numedge_psf)
      call count_section_fld_in_triangle(numnod_psf, numedge_psf,       &
     &    nfield_psf, ncomptot_psf, iedge_psf, ref_tri, xref,           &
     &    line%num_field, line%ntot_comp, line%nnod)
!
!
      call allocate_ucd_nodal_data(line)
!
      call set_section_fld_in_triangle(numnod_psf, numedge_psf,         &
     &    nfield_psf, ncomptot_psf, xx_psf, iedge_psf,                  &
     &    ncomp_psf, psf_data_name, d_nod_psf, ref_tri, xref,           &
     &    line%nnod, line%inod_global, line%xx,                         &
     &    line%num_comp, line%phys_name, line%d_ucd)
!
      deallocate(ref_tri)
!
      call count_sections_in_triangle                                   &
     &   (numele_psf, ie_psf, ie_edge_psf, line%nele, line%nnod_4_ele)
      call allocate_ucd_ele(line)
!
      call set_sections_in_triangle(numele_psf, ie_psf, ie_edge_psf,    &
     &    line%nele, line%iele_global, line%ie)
!
      call deallocate_edge_section_flags
!
      end subroutine pick_psf_by_sections
!
!-----------------------------------------------------------------------
!
      end module m_line_from_psf
