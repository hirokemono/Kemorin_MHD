!>@file  vtk_file_IO.f90
!!       module vtk_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in March, 2007
!!@n      Modified by H. Matsui in July, 2013
!
!> @brief Output VTK file
!!
!!@verbatim
!!      subroutine write_vtk_file(file_name, id_vtk,                    &
!!     &          nnod, nele, nnod_ele, xx, ie, num_field,  ntot_comp,  &
!!     &          ncomp_field, field_name, d_nod)
!!      subroutine write_vtk_phys(file_name, id_vtk,                    &
!!     &          nnod, num_field, ntot_comp, ncomp_field,              &
!!     &          field_name, d_nod)
!!      subroutine write_vtk_grid(file_name, id_vtk,                    &
!!     &          nnod, nele, nnod_ele, xx, ie)
!!@endverbatim
!
      module vtk_file_IO
!
      use m_precision
      use m_constants
!
      use vtk_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_vtk_file(file_name, id_vtk,                      &
     &          nnod, nele, nnod_ele, xx, ie, num_field,  ntot_comp,    &
     &          ncomp_field, field_name, d_nod)
!
      integer(kind = kint), intent(in) ::  id_vtk
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
!
      open(id_vtk, file=file_name, form='formatted', status ='unknown')
!
      call write_vtk_mesh(id_vtk, nnod, nele, nnod_ele, xx, ie)
!
      call write_vtk_data(id_vtk, nnod, num_field, ntot_comp,           &
     &    ncomp_field, field_name, d_nod)
!
      close(id_vtk)
!
      end subroutine write_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_phys(file_name, id_vtk,                      &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod)
!
      integer(kind = kint), intent(in) ::  id_vtk
      character(len=kchara), intent(in) :: file_name
!
      integer (kind=kint), intent(in) :: nnod
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
!
      open(id_vtk, file=file_name, form='formatted', status ='unknown')
!
      call write_vtk_data(id_vtk, nnod, num_field, ntot_comp,           &
     &    ncomp_field, field_name, d_nod)
!
      close(id_vtk)
!
      end subroutine write_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_grid(file_name, id_vtk,                      &
     &          nnod, nele, nnod_ele, xx, ie)
!
      integer(kind = kint), intent(in) ::  id_vtk
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
!
      open(id_vtk, file=file_name, form='formatted', status ='unknown')
      call write_vtk_mesh(id_vtk, nnod, nele, nnod_ele, xx, ie)
      close(id_vtk)
!
      end subroutine write_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module vtk_file_IO
