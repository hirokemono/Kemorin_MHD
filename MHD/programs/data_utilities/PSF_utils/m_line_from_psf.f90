!>@file   m_line_from_psf.f90
!!@brief  module m_line_from_psf
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Pick data on line defined by two surfaces
!!
!!@verbatim
!!      subroutine write_psf_line_data                                  &
!!     &         (istep_start, istep, line_ucd_param, line_time, line)
!!        type(field_IO_params), intent(in) :: line_ucd_param
!!        type(time_data), intent(inout) :: line_time
!!        type(ucd_data), intent(inout) :: line
!!      subroutine pick_psf_by_sections                                 &
!!     &         (nd, xref, psf_nod, psf_ele, psf_phys, line)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(phys_data), intent(in) :: psf_phys
!!        type(ucd_data), intent(inout) :: line
!!@endverbatim
!
      module m_line_from_psf
!
      use m_precision
      use m_constants
!
      use t_time_data
      use t_ucd_data
      use t_file_IO_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_psf_line_data                                    &
     &         (istep_start, istep, line_ucd_param, line_time, line)
!
      use m_geometry_constants
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep_start, istep
      type(field_IO_params), intent(in) :: line_ucd_param
      type(time_data), intent(inout) :: line_time
      type(ucd_data), intent(inout) :: line
!
      integer, parameter :: delete_process = -1
!
!
      if(istep .eq. istep_start) then
        call sel_write_grd_file(delete_process, line_ucd_param, line)
      end if
!
      call sel_write_ucd_file                                           &
     &   (delete_process, istep, line_ucd_param, line_time, line)
      call deallocate_ucd_mesh(line)
!
      end subroutine write_psf_line_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_psf_by_sections                                   &
     &         (nd, xref, psf_nod, psf_ele, psf_phys, line)
!
      use t_geometry_data
      use t_phys_data
      use t_ucd_data
      use m_geometry_constants
      use m_psf_edge_connect
      use const_section_from_triangle
      use set_node_on_edge_quad_psf
!
      integer(kind = kint), intent(in) :: nd
      real(kind = kreal), intent(in) :: xref
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
!
      type(ucd_data), intent(inout) :: line
!
      real(kind = kreal) :: coef_line(10)
      integer(kind = kint), allocatable :: iedge_4_line(:)
      real(kind = kreal), allocatable :: coef_on_edge(:,:)
      real(kind = kreal), allocatable :: ref_tri(:)
      integer(kind = kint) :: i1
      integer(kind = kint) :: istack_smp(0:1)
!
!   Count number of lines
!
      allocate(ref_tri(psf_nod%numnod))
!
      coef_line(1:10) = zero
      if(nd.eq.1 .or. nd.eq.2 .or. nd.eq.3) then
        do i1 = 1, psf_nod%numnod
          ref_tri(i1) = psf_nod%xx(i1,nd)
        end do
        coef_line(nd+6) = one
        coef_line(10)   = -xref
      else if(nd .eq. 11) then
        do i1 = 1, psf_nod%numnod
          ref_tri(i1) = psf_nod%rr(i1)
        end do
        coef_line(1:3) = one
        coef_line(10)  = -xref*xref
      else if(nd .eq. 21) then
        do i1 = 1, psf_nod%numnod
          ref_tri(i1) = psf_nod%ss(i1)
        end do
      else
        do i1 = 1, psf_nod%numnod
          ref_tri(i1) = psf_nod%rr(i1)
        end do
        coef_line(1:2) = one
        coef_line(10)  = -xref*xref
      end if
!
!
      call allocate_edge_section_flags(numedge_psf)
      call count_section_fld_in_triangle(psf_nod%numnod, numedge_psf,   &
     &    psf_phys%num_phys, psf_phys%ntot_phys, iedge_psf,             &
     &    ref_tri, xref, line%num_field, line%ntot_comp, line%nnod)
!
!
      call allocate_ucd_nodal_data(line)
      allocate(iedge_4_line(line%nnod))
      allocate(coef_on_edge(line%nnod,2))
      if(line%nnod .gt. 0) then
        iedge_4_line = 0
        coef_on_edge = 0.0d0
      end if
      istack_smp(0) = 0
      istack_smp(1) = int(line%nnod, KIND(istack_smp(0)))
!
      call set_section_list_in_triangle(numedge_psf,                    &
     &    istack_smp(1), iedge_4_line)
!
      call set_node_on_edge_4_quad_psf(psf_nod%numnod, numedge_psf,     &
     &    num_linear_edge, iedge_psf, psf_nod%xx, coef_line, ref_tri,   &
     &    istack_smp(1), istack_smp, iedge_4_line, coef_on_edge)
!
      call set_section_fld_in_triangle(psf_nod%numnod, numedge_psf,     &
     &    psf_phys%num_phys, psf_phys%ntot_phys, psf_nod%xx, iedge_psf, &
     &    psf_phys%num_component, psf_phys%phys_name, psf_phys%d_fld,   &
     &    line%nnod, iedge_4_line, coef_on_edge, line%inod_global,      &
     &    line%xx, line%num_comp, line%phys_name, line%d_ucd)
!
      deallocate(ref_tri, iedge_4_line, coef_on_edge)
!
      call count_sections_in_triangle                                   &
     &   (psf_ele%numele, psf_ele%ie, ie_edge_psf,                      &
     &    line%nele, line%nnod_4_ele)
      call allocate_ucd_ele(line)
!
      call set_sections_in_triangle(psf_ele%numele, psf_ele%ie,         &
     &    ie_edge_psf, line%nele, line%iele_global, line%ie)
!
      call deallocate_edge_section_flags
!
      end subroutine pick_psf_by_sections
!
!-----------------------------------------------------------------------
!
      end module m_line_from_psf
