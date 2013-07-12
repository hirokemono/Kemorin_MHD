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
!!      subroutine write_psf_line_data(file_header, istep)
!!      subroutine deallocate_psf_data_on_line
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
      type(ucd_data), save, private :: line
!
      private :: allocate_psf_data_on_line, deallocate_psf_data_on_line
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_data_on_line
!
      use m_psf_results
!
!
      call alloc_ucd_node_t(line)
!
      line%num_field =   nfield_psf
      line%ntot_comp = ncomptot_psf
      call alloc_ucd_phys_name_t(line)
!
      if(line%num_field .gt. 0) then
        line%num_comp =     ncomp_psf
        line%phys_name = psf_data_name
      end if
      line%istack_comp = istack_comp_psf
!
      call alloc_ucd_phys_data_t(line)
!
      line%nele = line%nnod/3 + 1
      line%nnod_4_ele = 3
      call alloc_ucd_ele_t(line)
!
      end subroutine allocate_psf_data_on_line
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_psf_data_on_line
!
!
      call dealloc_ucd_data_t(line)
      call dealloc_ucd_ele_t(line)
      call dealloc_ucd_node_t(line)
!
      end subroutine deallocate_psf_data_on_line
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_psf_line_data(file_header, istep)
!
      use m_geometry_constants
      use ucd_type_file_IO
!
      character(len=kchara), intent(in) :: file_header
      integer(kind = kint), intent(in) :: istep
      integer(kind = kint), parameter :: delete_process = -1
!
!
      line%header_name = file_header
      call write_ucd_type_file(delete_process, istep, line)
      call deallocate_psf_data_on_line
!
      end subroutine write_psf_line_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_psf_by_sections(nd, xref)
!
      use m_psf_edge_connect
!
      integer(kind = kint), intent(in) :: nd
      real(kind = kreal), intent(in) :: xref
!
      integer(kind = kint) :: iedge, k1, i1, i2, icou
      real(kind = kreal) :: c1, c2, coef1
!
!
      line%nnod = 0
      do i1 = 1, numnod_psf
        if(nd.eq.1 .or. nd.eq.2 .or. nd.eq.3) then
          c1 = xx_psf(i1,nd)
        else if(nd .eq. 11) then
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2 +xx_psf(i1,3)**2)
        else if(nd .eq. 21) then
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2)
        else
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2 +xx_psf(i1,3)**2)
        end if
!
        if(c1 .eq. xref) line%nnod = line%nnod + 1
      end do
!
      do iedge = 1, numedge_psf
        i1 = iedge_psf(iedge,1)
        i2 = iedge_psf(iedge,2)
!
        if(nd.eq.1 .or. nd.eq.2 .or. nd.eq.3) then
          c1 = xx_psf(i1,nd)
          c2 = xx_psf(i2,nd)
        else if(nd .eq. 11) then
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2 +xx_psf(i1,3)**2)
          c2 = sqrt(xx_psf(i2,1)**2 + xx_psf(i2,2)**2 +xx_psf(i2,3)**2)
        else if(nd .eq. 21) then
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2)
          c2 = sqrt(xx_psf(i2,1)**2 + xx_psf(i2,2)**2)
        else
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2 +xx_psf(i1,3)**2)
          c2 = sqrt(xx_psf(i2,1)**2 + xx_psf(i2,2)**2 +xx_psf(i2,3)**2)
        end if
!
        if( ((c1-xref)*(c2-xref)) .lt. 0.0d0) then
          line%nnod = line%nnod + 1
        end if
      end do
!
      call allocate_psf_data_on_line
!
      do i1 = 1, line%nnod
        line%inod_global(i1) = i1
      end do
!
      icou = 0
      do iedge = 1, line%nele
        line%iele_global(iedge) = iedge
        do k1 = 1, 3
          icou = icou + 1
          line%ie(iedge,k1) = ione + mod(icou-1,line%nnod)
        end do
      end do
!
!
      icou = 0
      do i1 = 1, numnod_psf
        if(nd.eq.1 .or. nd.eq.2 .or. nd.eq.3) then
          c1 = xx_psf(i1,nd)
        else if(nd .eq. 11) then
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2 +xx_psf(i1,3)**2)
        else if(nd .eq. 21) then
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2)
        else
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2 +xx_psf(i1,3)**2)
        end if
!
        if(c1 .eq. xref) then
          icou = icou + 1
          line%xx(icou,1:3) = xx_psf(i1,1:3)
          line%d_ucd(icou,1:ncomptot_psf)                               &
     &                         = d_nod_psf(i1,1:ncomptot_psf)
        end if
      end do
!
      do iedge = 1, numedge_psf
        i1 = iedge_psf(iedge,1)
        i2 = iedge_psf(iedge,2)
!
        if(nd.eq.1 .or. nd.eq.2 .or. nd.eq.3) then
          c1 = xx_psf(i1,nd)
          c2 = xx_psf(i2,nd)
        else if(nd .eq. 11) then
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2 +xx_psf(i1,3)**2)
          c2 = sqrt(xx_psf(i2,1)**2 + xx_psf(i2,2)**2 +xx_psf(i2,3)**2)
        else if(nd .eq. 21) then
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2)
          c2 = sqrt(xx_psf(i2,1)**2 + xx_psf(i2,2)**2)
        else
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2 +xx_psf(i1,3)**2)
          c2 = sqrt(xx_psf(i2,1)**2 + xx_psf(i2,2)**2 +xx_psf(i2,3)**2)
        end if
!
        if( ((c1-xref)*(c2-xref)) .lt. 0.0d0) then
          coef1 =  (c2 - xref)  / (c2 - c1)
          write(*,*) 'find on edge', icou, coef1
!
          icou = icou + 1
          line%xx(icou,1:3) =  coef1 * xx_psf(i1,1:3)                   &
     &               + (one - coef1) * xx_psf(i2,1:3)
          line%d_ucd(icou,1:ncomptot_psf)                               &
     &                         =  coef1 * d_nod_psf(i1,1:ncomptot_psf)  &
     &                  + (one - coef1) * d_nod_psf(i2,1:ncomptot_psf)
        end if
      end do
!
      end subroutine pick_psf_by_sections
!
!-----------------------------------------------------------------------
!
      end module m_line_from_psf
