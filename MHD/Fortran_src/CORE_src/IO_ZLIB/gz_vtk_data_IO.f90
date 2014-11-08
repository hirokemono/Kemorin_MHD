!>@file  gz_vtk_data_IO.f90
!!       module gz_vtk_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for gzipped VTK data segments
!!
!!@verbatim
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
!!
!!
!!      subroutine read_gz_vtk_fields_head(nnod)
!!      subroutine read_gz_vtk_each_field_head(iflag_end,               &
!!     &          ncomp_field, field_name)
!!
!!      subroutine read_gz_vtk_each_field(ntot_nod, ncomp_field, nnod,&
!!     &          d_nod)
!!
!!      subroutine read_gz_vtk_node_head(nnod)
!!
!!      subroutine read_gz_vtk_connect_head(nele, nnod_ele)
!!      subroutine read_gz_vtk_cell_type(nele)
!!
!!      subroutine read_gz_vtk_connect_data(ntot_ele, nnod_ele,         &
!!     &          nele, ie)
!!@endverbatim
!!
!!@n @param iflag_end              Integer flag for the end of file
!!@n @param nnod                   Number of nodes
!!@n @param nele                   Number of elements
!!@n @param nnod_ele               Number of nodes for each element
!!@n @param xx(nnod,3)             position of nodes
!!@n @param nnod_ele               number of nodes for each element
!!@n @param ie(nele,nnod_ele)      element connectivity
!!@n @param ntot_comp              total number of components
!!@n @param ncomp_field(num_field) number of components
!!@n @param field_name(num_field)  list of field names
!!@n @param d_nod(nnod,ntot_comp)  field data
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
      subroutine write_gz_vtk_fields_head(nnod)
!
      integer(kind=kint ), intent(in) :: nnod
!
!
      write(textbuf,'(a1)')        char(0)
      call gz_write_textbuf_f
!
      write(textbuf,'(a,i15,a1)') 'POINT_DATA ', nnod, char(0)
      call gz_write_textbuf_f
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
        write(textbuf,'(a,a,a,i15,a1)') 'SCALARS ', trim(field_name),   &
     &                        ' double ', ione, char(0)
        call gz_write_textbuf_f
!
        write(textbuf,'(a,a1)') 'LOOKUP_TABLE default', char(0)
        call gz_write_textbuf_f
      else if (ncomp_field .eq. n_vector) then
        write(textbuf,'(a,a,a,a1)') 'VECTORS ', trim(field_name),       &
     &                        ' double', char(0)
        call gz_write_textbuf_f
      else if (ncomp_field .eq. n_sym_tensor) then
        write(textbuf,'(a,a,a,a1)') 'TENSORS ', trim(field_name),       &
     &                        ' double', char(0)
        call gz_write_textbuf_f
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
            call gz_write_textbuf_f
          end do
        end do
      else if(ncomp_field .eq. n_vector) then
        do inod = 1, nnod
          write(textbuf,'(1p3e23.12,a1)')                               &
     &               d_nod(inod,1:ncomp_field), char(0)
          call gz_write_textbuf_f
        end do
      else if(ncomp_field .eq. n_scalar) then
        do inod = 1, nnod
          write(textbuf,'(1pe23.12,a1)') d_nod(inod,1), char(0)
          call gz_write_textbuf_f
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
      call gz_write_textbuf_f
!
      write(textbuf,'(a,a1)')                                           &
     &              'converted data of tri-linear hexahedral element',  &
     &              char(0)
      call gz_write_textbuf_f
!
      write(textbuf,'(a,a1)') 'ASCII', char(0)
      call gz_write_textbuf_f
!
      write(textbuf,'(a,a1)') 'DATASET UNSTRUCTURED_GRID', char(0)
      call gz_write_textbuf_f
!
!
      write(textbuf,'(a,i15,a,a1)')  'POINTS ', nnod,                   &
     &                              ' double', char(0)
      call gz_write_textbuf_f
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
      call gz_write_textbuf_f
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
      write(textbuf,'(a,i15,a1)') 'CELL_TYPES ', nele, char(0)
      call gz_write_textbuf_f
!
      do iele = 1, nele
        write(textbuf,'(i5,a1)') icellid, char(0)
        call gz_write_textbuf_f
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
      write(fmt_txt,'(a1,i3,a9)')   '(', (nnod_ele+1), '(i15),a1)'
!
      do iele = 1, nele
        ie0(1:nnod_ele) = ie(iele,1:nnod_ele) - 1
        write(textbuf,fmt_txt) nnod_ele, ie0(1:nnod_ele), char(0)
        call gz_write_textbuf_f
      end do
!
      end subroutine write_gz_vtk_connect_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_fields_head(nnod)
!
      integer(kind=kint ), intent(inout) :: nnod
      character(len=kchara)  :: label
!
!
      call skip_gz_comment_chara_int(label, nnod)
!
      end subroutine read_gz_vtk_fields_head
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_each_field_head(iflag_end,                 &
     &          ncomp_field, field_name)
!
      use m_phys_constants
!
      integer(kind=kint ), intent(inout) :: iflag_end, ncomp_field
      character(len=kchara), intent(inout) :: field_name
!
      integer(kind = kint) :: nchara
      character(len=kchara)  :: vtk_fld_type
!
!
      call get_one_line_from_gz_f
      if(nchara .eq. izero) go to 99
!
      read(textbuf,*) vtk_fld_type, field_name
      if(vtk_fld_type .eq. 'TENSORS') then
        ncomp_field = n_sym_tensor
      else if(vtk_fld_type .eq. 'VECTORS') then
        ncomp_field = n_vector
      else
        call get_one_line_from_gz_f
        ncomp_field = n_scalar
      end if
      iflag_end = izero
      return
!
  99  continue
      iflag_end = ione
      return
!
      end subroutine read_gz_vtk_each_field_head
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_each_field(ntot_nod, ncomp_field, nnod,   &
     &          d_nod)
!
      use m_phys_constants
!
      integer (kind=kint), intent(in) :: ntot_nod, ncomp_field
      integer (kind=kint), intent(in) :: nnod
      real(kind = kreal), intent(inout) :: d_nod(ntot_nod,ncomp_field)
!
      integer(kind = kint) :: nchara
      integer(kind = kint) :: inod
      real(kind = kreal) :: rtmp
!
!
      if (ncomp_field .eq. n_sym_tensor) then
        do inod = 1, nnod
          call get_one_line_from_gz_f
          read(textbuf,*) d_nod(inod,1:3)
          call get_one_line_from_gz_f
          read(textbuf,*) rtmp, d_nod(inod,4:5)
          call get_one_line_from_gz_f
          read(textbuf,*) rtmp, rtmp, d_nod(inod,6)
        end do
      else
        do inod = 1, nnod
          call get_one_line_from_gz_f
          read(textbuf,*) d_nod(inod,1:ncomp_field)
        end do
      end if
!
      end subroutine read_gz_vtk_each_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_node_head(nnod)
!
      integer(kind = kint), intent(inout) :: nnod
!
      integer(kind = kint) :: nchara
      character(len=kchara) :: tmpchara
!
!
      call skip_gz_comment_chara(tmpchara)
      call get_one_line_from_gz_f
      call get_one_line_from_gz_f
!
      call get_one_line_from_gz_f
      read(textbuf,'(a,i15,a)')  tmpchara, nnod
!
      end subroutine read_gz_vtk_node_head
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_connect_head(nele, nnod_ele)
!
      integer(kind = kint), intent(inout) :: nele, nnod_ele
!
      integer(kind = kint) :: nchara, nums
      character(len=kchara) :: tmpchara
!
!
      call get_one_line_from_gz_f
      read(textbuf,*) tmpchara, nele, nums
      nnod_ele = nums / nele - 1
!
      end subroutine read_gz_vtk_connect_head
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_cell_type(nele)
!
      integer(kind = kint), intent(in) :: nele
!
      integer(kind = kint) :: nchara, iele
!
!
      call get_one_line_from_gz_f
!
      do iele = 1, nele
        call get_one_line_from_gz_f
      end do
!
      end subroutine read_gz_vtk_cell_type
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_connect_data(ntot_ele, nnod_ele,           &
     &          nele, ie)
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_ele
      integer(kind = kint), intent(in) :: nele
      integer(kind = kint), intent(inout) :: ie(ntot_ele,nnod_ele)
!
      integer(kind = kint) :: nchara, iele, itmp
!
!
      do iele = 1, nele
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, ie(iele,1:nnod_ele)
        ie(iele,1:nnod_ele) = ie(iele,1:nnod_ele) + 1
      end do
!
      end subroutine read_gz_vtk_connect_data
!
! -----------------------------------------------------------------------
!
      end module gz_vtk_data_IO
