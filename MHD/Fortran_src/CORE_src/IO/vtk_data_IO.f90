!>@file  vtk_data_IO.f90
!!       module vtk_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for VTK data segments
!!
!!@verbatim
!!      subroutine write_vtk_data(id_vtk, nnod, num_field, ntot_comp,   &
!!     &          ncomp_field, field_name, d_nod)
!!      subroutine write_vtk_mesh(id_vtk, nnod, nele, nnod_ele, xx, ie)
!!
!!      subroutine write_vtk_fields_head(id_vtk, nnod)
!!      subroutine write_vtk_each_field_head(id_vtk, ncomp_field,       &
!!     &          field_name)
!!
!!      subroutine write_vtk_each_field(id_vtk, ntot_nod, ncomp_field,  &
!!     &          nnod, d_nod)
!!
!!      subroutine write_vtk_node_head(id_vtk, nnod)
!!
!!      subroutine write_vtk_connect_head(id_vtk, nele, nnod_ele)
!!      subroutine write_vtk_cell_type(id_vtk, nele, nnod_ele)
!!
!!      subroutine write_vtk_connect_data(id_vtk, ntot_ele, nnod_ele,   &
!!     &          nele, ie)
!!@endverbatim
!
      module vtk_data_IO
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_vtk_data(id_vtk, nnod, num_field, ntot_comp,     &
     &          ncomp_field, field_name, d_nod)
!
      integer (kind=kint), intent(in) :: nnod
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint) :: icou, j
!
!
      call write_vtk_fields_head(id_vtk, nnod)
!
      IF(ntot_comp .ge. 1) then
        icou = 1
        do j = 1, num_field
          call write_vtk_each_field_head(id_vtk, ncomp_field(j),        &
     &        field_name(j) )
          call write_vtk_each_field(id_vtk, nnod, ncomp_field(j),       &
     &        nnod, d_nod(1,icou) )
          icou = icou + ncomp_field(j)
        end do
      end if
!
      end subroutine write_vtk_data
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_mesh(id_vtk, nnod, nele, nnod_ele,  xx, ie)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: nnod, nele, nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer(kind = kint), intent(in) ::  id_vtk
!
!
      call write_vtk_node_head(id_vtk, nnod)
      call write_vtk_each_field(id_vtk, nnod, ithree, nnod, xx)
!
      call write_vtk_connect_head(id_vtk, nele, nnod_ele)
      call write_vtk_connect_data(id_vtk, nele, nnod_ele, nele, ie)
!
      call write_vtk_cell_type(id_vtk, nele, nnod_ele)
!
      end subroutine write_vtk_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_fields_head(id_vtk, nnod)
!
      integer(kind=kint ), intent(in) :: nnod
      integer(kind = kint), intent(in) :: id_vtk
!
!
      write(id_vtk,'(a)')
      write(id_vtk,'(a,i10)') 'POINT_DATA ', nnod
! 
      end subroutine write_vtk_fields_head
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_each_field_head(id_vtk, ncomp_field,         &
     &          field_name)
!
      use m_phys_constants
!
      integer(kind=kint ), intent(in) :: ncomp_field
      character(len=kchara), intent(in) :: field_name
!
      integer(kind = kint), intent(in) ::  id_vtk
!
!
      if (ncomp_field .eq. n_scalar) then
        write(id_vtk,'(a,a,a,i10)') 'SCALARS ', trim(field_name),       &
     &                        ' double ', ione
        write(id_vtk,'(a)') 'LOOKUP_TABLE default'
      else if (ncomp_field .eq. n_vector) then
        write(id_vtk,'(a,a,a)') 'VECTORS ', trim(field_name), ' double'
      else if (ncomp_field .eq. n_sym_tensor) then
        write(id_vtk,'(a,a,a)') 'TENSORS ', trim(field_name), ' double'
      end if
!
      end subroutine write_vtk_each_field_head
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_each_field(id_vtk, ntot_nod, ncomp_field,    &
     &          nnod, d_nod)
!
      use m_phys_constants
!
      integer (kind=kint), intent(in) :: ntot_nod, nnod, ncomp_field
      real(kind = kreal), intent(in) :: d_nod(ntot_nod,ncomp_field)
!
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint) :: inod, nd, nd2
!
!
      if (ncomp_field .eq. n_sym_tensor) then
        do inod = 1, nnod
          do nd2 = 1, 3
            write(id_vtk,'(1p3e23.12)')                                 &
     &             (d_nod(inod,1+l_sim_t(nd,nd2)), nd=1,3)
          end do
        end do
      else
        do inod = 1, nnod
          write(id_vtk,'(1p3e23.12)') d_nod(inod,1:ncomp_field)
        end do
      end if
!
      end subroutine write_vtk_each_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_node_head(id_vtk, nnod)
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) ::  id_vtk
!
!
      write(id_vtk,'(a)') '# vtk DataFile Version 2.0'
      write(id_vtk,'(a)')                                               &
     &              'converted data of tri-linear hexahedral element'
      write(id_vtk,'(a)') 'ASCII'
      write(id_vtk,'(a)') 'DATASET UNSTRUCTURED_GRID'
!
      write(id_vtk,'(a,i10,a)')  'POINTS ', nnod, ' double'
!
      end subroutine write_vtk_node_head
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_connect_head(id_vtk, nele, nnod_ele)
!
      integer(kind = kint), intent(in) :: nele, nnod_ele
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint) :: nums
!
!
      nums = nele * (nnod_ele+1)
      write(id_vtk,'(a,2i10)') 'CELLS ', nele, nums
!
      end subroutine write_vtk_connect_head
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_cell_type(id_vtk, nele, nnod_ele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele, nnod_ele
      integer(kind = kint), intent(in) ::  id_vtk
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
      write(id_vtk,'(a,i10)') 'CELL_TYPES ', nele
      do iele = 1, nele
        write(id_vtk,'(i5)') icellid
      end do
!
      end subroutine write_vtk_cell_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_connect_data(id_vtk, ntot_ele, nnod_ele,     &
     &          nele, ie)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: id_vtk
      integer(kind = kint), intent(in) :: ntot_ele, nnod_ele
      integer(kind = kint), intent(in) :: nele
      integer(kind = kint), intent(in) :: ie(ntot_ele,nnod_ele)
!
      integer(kind = kint) :: iele
      integer(kind = kint), dimension(nnod_ele) :: ie0
!
!
      do iele = 1, nele
        ie0(1:nnod_ele) = ie(iele,1:nnod_ele) - 1
        write(id_vtk,'(30i10)') nnod_ele, ie0(1:nnod_ele)
      end do
!
      end subroutine  write_vtk_connect_data
!
! ----------------------------------------------------------------------
!
      end module vtk_data_IO
