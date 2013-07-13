!>@file  gz_vtk_data_IO.f90
!!       module gz_vtk_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for gzipped VTK data segments
!!
!!@verbatim
!!      subroutine write_gz_vtk_data(nnod, num_field, ntot_comp,        &
!!     &          ncomp_field, field_name, d_nod)
!!      subroutine write_gz_vtk_mesh(nnod, nele, nnod_ele, xx, ie)
!!
!!      subroutine write_gz_vtk_fields_head(nnod)
!!      subroutine write_gz_vtk_each_field_head(ncomp_field, field_name)
!!
!!      subroutine write_gz_vtk_each_field(ntot_nod, ncomp_field, nnod,&
!!     &          d_nod)
!!
!!      subroutine write_gz_vtk_node_head(nnod)
!!
!!      subroutine write_gz_vtk_connect_head(nele, nnod_ele)
!!      subroutine write_gz_vtk_cell_type(nele, nnod_ele)
!!
!!      subroutine write_gz_vtk_connect_data(ntot_ele, nnod_ele,        &
!!     &          nele, ie)
!!@endverbatim
!
      module gz_vtk_data_IO
!
      use m_precision
      use m_constants
!
      use skip_gz_comment
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_gz_vtk_data(nnod, num_field, ntot_comp,          &
     &          ncomp_field, field_name, d_nod)
!
      integer (kind=kint), intent(in) :: nnod
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint) :: icou, j
!
!
      call write_gz_vtk_fields_head(nnod)
!
      IF(ntot_comp .ge. 1) then
        icou = 1
        do j = 1, num_field
          call write_gz_vtk_each_field_head(ncomp_field(j),             &
     &        field_name(j) )
          call write_gz_vtk_each_field(nnod, ncomp_field(j), nnod,      &
     &        d_nod(1,icou)  )
          icou = icou + ncomp_field(j)
        end do
      end if
!
      end subroutine write_gz_vtk_data
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_mesh(nnod, nele, nnod_ele, xx, ie)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
!
      call write_gz_vtk_node_head(nnod)
      call write_gz_vtk_each_field(nnod, n_vector, nnod, xx)
!
      call write_gz_vtk_connect_head(nele, nnod_ele)
      call write_gz_vtk_connect_data(nele, nnod_ele, nele, ie)
!
      call write_gz_vtk_cell_type(nele, nnod_ele)
!
      end subroutine write_gz_vtk_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_fields_head(nnod)
!
      integer(kind=kint ), intent(in) :: nnod
!
!
      write(textbuf,'(a1)')        char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,i10,a1)') 'POINT_DATA ', nnod, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      end subroutine write_gz_vtk_fields_head
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_each_field_head(ncomp_field, field_name)
!
      use m_phys_constants
!
      integer(kind=kint ), intent(in) :: ncomp_field
      character(len=kchara), intent(in) :: field_name
!
!
      if (ncomp_field .eq. n_scalar) then
        write(textbuf,'(a,a,a,i10,a1)') 'SCALARS ', trim(field_name),   &
     &                        ' double ', ione, char(0)
        call write_compress_txt(nbuf, textbuf)
!
        write(textbuf,'(a,a1)') 'LOOKUP_TABLE default', char(0)
        call write_compress_txt(nbuf, textbuf)
      else if (ncomp_field .eq. n_vector) then
        write(textbuf,'(a,a,a,a1)') 'VECTORS ', trim(field_name),       &
     &                        ' double', char(0)
        call write_compress_txt(nbuf, textbuf)
      else if (ncomp_field .eq. n_sym_tensor) then
        write(textbuf,'(a,a,a,a1)') 'TENSORS ', trim(field_name),       &
     &                        ' double', char(0)
        call write_compress_txt(nbuf, textbuf)
      end if
!
      end subroutine write_gz_vtk_each_field_head
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_each_field(ntot_nod, ncomp_field, nnod,   &
     &          d_nod)
!
      use m_phys_constants
!
      integer (kind=kint), intent(in) :: ntot_nod, ncomp_field
      integer (kind=kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: d_nod(ntot_nod,ncomp_field)
!
      integer(kind = kint) :: inod, nd, nd2
!
!
      if (ncomp_field .eq. n_sym_tensor) then
        do inod = 1, nnod
          do nd2 = 1, 3
            write(textbuf,'(1p3e23.12,a1)')                             &
     &             (d_nod(inod,1+l_sim_t(nd,nd2)), nd=1,3), char(0)
            call write_compress_txt(nbuf, textbuf)
          end do
        end do
      else if(ncomp_field .eq. n_vector) then
        do inod = 1, nnod
          write(textbuf,'(1p3e23.12,a1)')                               &
     &               d_nod(inod,1:ncomp_field), char(0)
          call write_compress_txt(nbuf, textbuf)
        end do
      else if(ncomp_field .eq. n_scalar) then
        do inod = 1, nnod
          write(textbuf,'(1pe23.12,a1)') d_nod(inod,1), char(0)
          call write_compress_txt(nbuf, textbuf)
        end do
      end if
!
      end subroutine write_gz_vtk_each_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_node_head(nnod)
!
      integer(kind = kint), intent(in) :: nnod
!
!
      write(textbuf,'(a,a1)') '# vtk DataFile Version 2.0', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)')                                           &
     &              'converted data of tri-linear hexahedral element',  &
     &              char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') 'ASCII', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') 'DATASET UNSTRUCTURED_GRID', char(0)
      call write_compress_txt(nbuf, textbuf)
!
!
      write(textbuf,'(a,i10,a,a1)')  'POINTS ', nnod,                   &
     &                              ' double', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      end subroutine write_gz_vtk_node_head
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_connect_head(nele, nnod_ele)
!
      integer(kind = kint), intent(in) :: nele, nnod_ele
!
      integer(kind = kint) :: nums
!
!
      nums = nele*(nnod_ele+1)
      write(textbuf,'(a,2i10,a1)') 'CELLS ', nele, nums, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      end subroutine write_gz_vtk_connect_head
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_cell_type(nele, nnod_ele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele, nnod_ele
!
      integer(kind = kint) :: iele, icellid
!
!
      if (nnod_ele .eq. num_t_linear) then
        icellid = 12
      else if (nnod_ele .eq. num_t_quad) then
        icellid = 25
      else if (nnod_ele .eq. num_triangle) then
        icellid = 5
      else if (nnod_ele .eq. num_linear_edge) then
        icellid = 3
      end if
!
      write(textbuf,'(a,i10,a1)') 'CELL_TYPES ', nele, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      do iele = 1, nele
        write(textbuf,'(i5,a1)') icellid, char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine write_gz_vtk_cell_type
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_connect_data(ntot_ele, nnod_ele,          &
     &          nele, ie)
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_ele
      integer(kind = kint), intent(in) :: nele
      integer(kind = kint), intent(in) :: ie(ntot_ele,nnod_ele)
!
      integer(kind = kint) :: iele
      integer(kind = kint), dimension(nnod_ele) :: ie0
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a1,i3,a9)')   '(', (nnod_ele+1), '(i10),a1)'
!
      do iele = 1, nele
        ie0(1:nnod_ele) = ie(iele,1:nnod_ele) - 1
        write(textbuf,fmt_txt) nnod_ele, ie0(1:nnod_ele), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine write_gz_vtk_connect_data
!
! -----------------------------------------------------------------------
!
      end module gz_vtk_data_IO
