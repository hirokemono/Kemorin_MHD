!
!      module gz_vtk_file_IO
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine write_gz_vtk_file(gzip_name,                          &
!     &          nnod, nele, nnod_ele, xx, ie, num_field,  ntot_comp,   &
!     &          ncomp_field, field_name, d_nod)
!      subroutine write_gz_vtk_phys(gzip_name, nnod, num_field,         &
!     &          ntot_comp, ncomp_field, field_name, d_nod)
!      subroutine write_gz_vtk_grid(gzip_name, nnod, nele, nnod_ele,    &
!     &          xx, ie)
!
!      subroutine output_multi_gz_vtk_file(gzip_name,                   &
!     &         ntot_nod, ist_nod, ied_nod, xx,                         &
!     &         ntot_ele, nnod_ele, ist_ele, ied_ele, ie,               &
!     &         num_field, ntot_comp, ncomp_field, field_name, d_nod)
!      subroutine output_gz_multi_vtk_phys(gzip_name,                   &
!     &         ntot_nod, ist_nod, ied_nod,                             &
!     &         num_field, ntot_comp, ncomp_field, field_name, d_nod)
!      subroutine output_gz_multi_vtk_grid(gzip_name,                   &
!     &         ntot_nod, ist_nod, ied_nod, xx,                         &
!     &         ntot_ele, nnod_ele, ist_ele, ied_ele, ie)
!
      module gz_vtk_file_IO
!
      use m_precision
      use m_constants
!
      use gz_vtk_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_gz_vtk_file(gzip_name,                           &
     &          nnod, nele, nnod_ele, xx, ie, num_field,  ntot_comp,    &
     &          ncomp_field, field_name, d_nod)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: gzip_name
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
      call open_wt_gzfile(gzip_name)
      call write_gz_vtk_mesh(nnod, nele, nnod_ele, xx, ie)
      call write_gz_vtk_data(nnod, num_field, ntot_comp, ncomp_field,   &
     &    field_name, d_nod)
!
      call close_gzfile
!
      end subroutine write_gz_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_phys(gzip_name, nnod, num_field,          &
     &          ntot_comp, ncomp_field, field_name, d_nod)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: gzip_name
!
      integer (kind=kint), intent(in) :: nnod
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
!
      call open_wt_gzfile(gzip_name)
      call write_gz_vtk_data(nnod, num_field, ntot_comp, ncomp_field,   &
     &    field_name, d_nod)
      call close_gzfile
!
      end subroutine write_gz_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_grid(gzip_name, nnod, nele, nnod_ele,     &
     &          xx, ie)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: gzip_name
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
!
      call open_wt_gzfile(gzip_name)
      call write_gz_vtk_mesh(nnod, nele, nnod_ele, xx, ie)
      call close_gzfile
!
      end subroutine write_gz_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_multi_gz_vtk_file(gzip_name,                    &
     &         ntot_nod, ist_nod, ied_nod, xx,                          &
     &         ntot_ele, nnod_ele, ist_ele, ied_ele, ie,                &
     &         num_field, ntot_comp, ncomp_field, field_name, d_nod)
!
      character(len = kchara), intent(in) :: gzip_name
!
      integer(kind = kint), intent(in) :: ntot_nod
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      real(kind = kreal), intent(in) :: xx(ntot_nod, 3)
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_ele
      integer(kind = kint), intent(in) :: ist_ele, ied_ele
      integer(kind = kint), intent(in) :: ie(ntot_ele,nnod_ele)
!
      integer(kind = kint), intent(in) :: num_field, ntot_comp
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len = kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(ntot_nod, ntot_comp)
!
!
      write(*,*) 'write ascii VTK data: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_multi_gz_vtk_mesh(ntot_nod, ist_nod, ied_nod,          &
     &    ntot_ele, nnod_ele, ist_ele, ied_ele, xx, ie)
      call write_multi_gz_vtk_data(ntot_nod, ist_nod, ied_nod,          &
     &    num_field, ntot_comp, ncomp_field, field_name, d_nod)
!
      call close_gzfile
!
      end subroutine output_multi_gz_vtk_file
!
! ----------------------------------------------------------------------
!
      subroutine output_gz_multi_vtk_phys(gzip_name,                    &
     &         ntot_nod, ist_nod, ied_nod,                              &
     &         num_field, ntot_comp, ncomp_field, field_name, d_nod)
!
      character(len = kchara), intent(in) :: gzip_name
!
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len = kchara), intent(in) :: field_name(num_field)
!
      integer(kind = kint), intent(in) :: ntot_nod, ntot_comp
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      real(kind = kreal), intent(in) :: d_nod(ntot_nod, ntot_comp)
!
!
      write(*,*) 'write ascii VTK field data: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_multi_gz_vtk_data(ntot_nod, ist_nod, ied_nod,          &
     &    num_field, ntot_comp, ncomp_field, field_name, d_nod)
!
      call close_gzfile
!
      end subroutine output_gz_multi_vtk_phys
!
! ----------------------------------------------------------------------
!
      subroutine output_gz_multi_vtk_grid(gzip_name,                    &
     &         ntot_nod, ist_nod, ied_nod, xx,                          &
     &         ntot_ele, nnod_ele, ist_ele, ied_ele, ie)
!
      character(len = kchara), intent(in) :: gzip_name
!
      integer(kind = kint), intent(in) :: ntot_nod
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      real(kind = kreal), intent(in) :: xx(ntot_nod, 3)
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_ele
      integer(kind = kint), intent(in) :: ist_ele, ied_ele
      integer(kind = kint), intent(in) :: ie(ntot_ele,nnod_ele)
!
!
      write(*,*) 'write ascii VTK grid data: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_multi_gz_vtk_mesh(ntot_nod, ist_nod, ied_nod,          &
     &    ntot_ele, nnod_ele, ist_ele, ied_ele, xx, ie)
!
      call close_gzfile
!
      end subroutine output_gz_multi_vtk_grid
!
! ----------------------------------------------------------------------
!
      end module gz_vtk_file_IO
