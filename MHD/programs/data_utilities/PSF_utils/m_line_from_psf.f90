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
      use m_geometry_constants
      use m_psf_edge_connect
      use const_section_from_triangle
      use set_node_on_edge_quad_psf
!
      integer(kind = kint), intent(in) :: nd
      real(kind = kreal), intent(in) :: xref
      type(ucd_data), intent(inout) :: line
!
      real(kind = kreal) :: coef_line(10)
      integer(kind = kint) :: nline_on_node
      integer(kind = kint), allocatable :: iedge_4_line(:)
      real(kind = kreal), allocatable :: coef_on_edge(:,:)
      real(kind = kreal), allocatable :: ref_tri(:)
      integer(kind = kint) :: i1, num
      integer(kind = kint) :: istack_smp(0:1)
!
!   Count number of lines
!
      allocate(ref_tri(numnod_psf))
!
      coef_line(1:10) = zero
      if(nd.eq.1 .or. nd.eq.2 .or. nd.eq.3) then
        do i1 = 1, numnod_psf
          ref_tri(i1) = xx_psf(i1,nd)
        end do
        coef_line(nd+6) = one
        coef_line(10)   = -xref
      else if(nd .eq. 11) then
        do i1 = 1, numnod_psf
          ref_tri(i1) = rtp_psf(i1,1)
        end do
        coef_line(1:3) = one
        coef_line(10)  = -xref*xref
      else if(nd .eq. 21) then
        do i1 = 1, numnod_psf
          ref_tri(i1) = ss_psf(i1)
        end do
      else
        do i1 = 1, numnod_psf
          ref_tri(i1) = rtp_psf(i1,1)
        end do
        coef_line(1:2) = one
        coef_line(10)  = -xref*xref
      end if
!
!
      call allocate_edge_section_flags(numnod_psf, numedge_psf)
      call count_section_fld_in_triangle(numnod_psf, numedge_psf,       &
     &    nfield_psf, ncomptot_psf, iedge_psf, ref_tri, xref,           &
     &    line%num_field, line%ntot_comp, line%nnod, nline_on_node)
!
!
      call allocate_ucd_nodal_data(line)
      allocate(iedge_4_line(line%nnod))
      allocate(coef_on_edge(line%nnod,2))
      if(line%nnod .gt. 0) then
        iedge_4_line = 0
        coef_on_edge = 0.0d0
      end if
      istack_smp(0) = nline_on_node
      istack_smp(1) = int(line%nnod)
!
      call set_section_list_in_triangle(numnod_psf, numedge_psf,        &
     &    istack_smp(1), nline_on_node, iedge_4_line)
!
      call set_node_on_edge_4_quad_psf(numnod_psf, numedge_psf,         &
     &    num_linear_edge, iedge_psf, xx_psf, coef_line,                &
     &    istack_smp(1), istack_smp, iedge_4_line, coef_on_edge)
!
      call set_section_fld_in_triangle(numnod_psf, numedge_psf,         &
     &    nfield_psf, ncomptot_psf, xx_psf, iedge_psf,                  &
     &    ncomp_psf, psf_data_name, d_nod_psf, line%nnod,               &
     &    nline_on_node, iedge_4_line, coef_on_edge, line%inod_global,  &
     &    line%xx, line%num_comp, line%phys_name, line%d_ucd)
!
      deallocate(ref_tri, iedge_4_line, coef_on_edge)
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
