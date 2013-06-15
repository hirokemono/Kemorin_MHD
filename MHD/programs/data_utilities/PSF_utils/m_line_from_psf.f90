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
!!      subroutine write_psf_line_data
!!      subroutine deallocate_psf_data_on_line
!!@endverbatim
!
      module m_line_from_psf
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint),  parameter :: id_line_result = 14
      character(len=kchara) :: line_udt_name
      character(len=kchara) :: line_udt_head
!
      integer(kind = kint) :: numnod_line
      integer(kind = kint) :: numele_line
      real(kind = kreal), allocatable :: xx_line(:,:)
!
      integer(kind = kint), allocatable :: inod_line(:)
!
      integer(kind = kint), allocatable :: iele_line(:)
      integer(kind = kint), allocatable :: ie_line(:,:)
!
      integer(kind = kint) :: nfield_line, ncomptot_line
      integer(kind = kint), allocatable :: ncomp_line(:)
      integer(kind = kint), allocatable :: istack_comp_line(:)
      character(len=kchara), allocatable :: line_data_name(:)
      real(kind = kreal), allocatable :: d_nod_line(:,:)
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
      nfield_line =   nfield_psf
      ncomptot_line = ncomptot_psf
      allocate(ncomp_line(nfield_line))
      allocate(istack_comp_line(0:nfield_line))
      allocate(line_data_name(nfield_line))
!
      if(nfield_line .gt. 0) then
        ncomp_line =     ncomp_psf
        line_data_name = psf_data_name
      end if
      istack_comp_line = istack_comp_psf
!
      allocate(inod_line(numnod_line) )
      allocate(xx_line(numnod_line,3))
      allocate(d_nod_line(numnod_line,ncomptot_psf))
      if(numnod_line .gt. 0) then
        xx_line = 0.0d0
        d_nod_line = 0.0d0
      end if
!
      numele_line = numnod_line/3 + 1
      allocate(iele_line(numele_line))
      allocate(ie_line(numele_line,3))
      if(numele_line .gt. 0) ie_line = 0
!
      end subroutine allocate_psf_data_on_line
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_psf_data_on_line
!
      use m_psf_results
!
!
      deallocate(iele_line, ie_line)
      deallocate(inod_line, xx_line, d_nod_line)
      deallocate(ncomp_line, istack_comp_line, line_data_name)
!
      end subroutine deallocate_psf_data_on_line
!
!-----------------------------------------------------------------------
!
      subroutine write_psf_line_data
!
      use m_geometry_constants
      use udt_data_IO
!
      write(*,*) 'PSF UCD data: ', trim(line_udt_name)
      open(id_line_result, file=line_udt_name, form='formatted')
!
      call write_udt_mesh_header(id_line_result, numnod_line,           &
     &    numele_line, ncomptot_line)
!
      call write_single_udt_data(id_line_result, numnod_line,           &
     &    ithree, inod_line, xx_line)
      call write_single_grd_connect(id_line_result, num_triangle,       &
     &    numele_line, iele_line, ie_line)
!
      call write_udt_field_header(id_line_result, nfield_line,          &
     &    ncomp_line, line_data_name)
      call write_single_udt_data(id_line_result, numnod_line,           &
     &    ncomptot_line, inod_line, d_nod_line)
!
      close(id_line_result)
!
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
      numnod_line = 0
      do i1 = 1, numnod_psf
        if(nd.eq.1 .or. nd.eq.2 .or. nd.eq.3) then
          c1 = xx_psf(i1,nd)
        else if(nd .eq. 11) then
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2 +xx_psf(i1,3)**2)
        else if(nd .eq. 21) then
          c1 = sqrt(xx_psf(i1,1)**2 + xx_psf(i1,2)**2)
        end if
        if(c1 .eq. xref) numnod_line = numnod_line + 1
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
        end if
!
        if( ((c1-xref)*(c2-xref)) .lt. 0.0d0) then
          numnod_line = numnod_line + 1
        end if
      end do
!
      call allocate_psf_data_on_line
!
      do i1 = 1, numnod_line
        inod_line(i1) = i1
      end do
!
      icou = 0
      do iedge = 1, numele_line
        iele_line(iedge) = iedge
        do k1 = 1, 3
          icou = icou + 1
          ie_line(iedge,k1) = ione + mod(icou-1,numnod_line)
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
        end if
        if(c1 .eq. xref) then
          icou = icou + 1
          xx_line(icou,1:3) = xx_psf(i1,1:3)
          d_nod_line(icou,1:ncomptot_psf)                               &
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
        end if
!
        if( ((c1-xref)*(c2-xref)) .lt. 0.0d0) then
          coef1 =  (c2 - xref)  / (c2 - c1)
          write(*,*) 'find on edge', icou, coef1
!
          icou = icou + 1
          xx_line(icou,1:3) =  coef1 * xx_psf(i1,1:3)                   &
     &               + (one - coef1) * xx_psf(i2,1:3)
          d_nod_line(icou,1:ncomptot_psf)                               &
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
