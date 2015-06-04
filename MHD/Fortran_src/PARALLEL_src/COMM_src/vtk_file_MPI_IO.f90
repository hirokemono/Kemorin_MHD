!>@file  vtk_file_MPI_IO.f90
!!       module vtk_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine write_vtk_file_mpi(file_name, ucd, m_ucd)
!!      subroutine write_vtk_phys_mpi(file_name, ucd, m_ucd)
!!      subroutine write_vtk_grid_mpi(file_name, ucd, m_ucd)
!!@endverbatim
!
      module vtk_file_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use vtk_data_to_buffer
      use t_ucd_data
      use m_merged_ucd_data
!
      implicit none
!
      character(len=65536), private :: textbuf
!
      private :: write_vtk_data_mpi, write_vtk_mesh_mpi
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_vtk_file_mpi(file_name, ucd, m_ucd)
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call MPI_FILE_OPEN                                                &
     &   (CALYPSO_COMM, file_name, MPI_MODE_WRONLY+MPI_MODE_CREATE,     &
     &    MPI_INFO_NULL, id_vtk, ierr_MPI)
!
      ioff_gl = 0
      call write_vtk_mesh_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call write_vtk_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call MPI_FILE_CLOSE(id_vtk, ierr_MPI)
!
      end subroutine write_vtk_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_phys_mpi(file_name, ucd, m_ucd)
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call MPI_FILE_OPEN                                                &
     &   (CALYPSO_COMM, file_name, MPI_MODE_WRONLY+MPI_MODE_CREATE,     &
     &    MPI_INFO_NULL, id_vtk, ierr_MPI)
!
      ioff_gl = 0
      call write_vtk_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call MPI_FILE_CLOSE(id_vtk, ierr_MPI)
!
      end subroutine write_vtk_phys_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_grid_mpi(file_name, ucd, m_ucd)
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call MPI_FILE_OPEN                                                &
     &   (CALYPSO_COMM, file_name, MPI_MODE_WRONLY+MPI_MODE_CREATE,     &
     &    MPI_INFO_NULL, id_vtk, ierr_MPI)
      ioff_gl = 0
!
      call write_vtk_mesh_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call MPI_FILE_CLOSE(id_vtk, ierr_MPI)
!
      end subroutine write_vtk_grid_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_data_mpi(id_vtk, ioff_gl,                    &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged_intnod)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint) :: icou, j
      integer(kind = kint_gl) :: nt_nod
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      nt_nod = istack_merged_intnod(nprocs)
!
      ioffset = int(ioff_gl)
      ilength = len(vtk_fields_head(nt_nod))
      if(my_rank .eq. 0) then
        write(textbuf,'(a,a1)') vtk_fields_head(nt_nod), char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &                      MPI_CHARACTER, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength
!
      if(ntot_comp .le. 0) return
      icou = 1
      do j = 1, num_field
        if ( ncomp_field(j) .eq. n_scalar) then
          call write_vtk_header_mpi(id_vtk, ioff_gl,                    &
     &        vtk_scalar_head(field_name(j)))
          call write_vtk_scalar_mpi(id_vtk, ioff_gl,                    &
     &          nnod, d_nod(1,icou), istack_merged_intnod)
!
        else if ( ncomp_field(j) .eq. n_vector) then
          call write_vtk_header_mpi(id_vtk, ioff_gl,                    &
     &        vtk_vector_head(field_name(j)))
!
          call write_vtk_vecotr_mpi(id_vtk, ioff_gl,                    &
     &          nnod, d_nod(1,icou), istack_merged_intnod)
!
        else if ( ncomp_field(j) .eq. n_sym_tensor) then
          call write_vtk_header_mpi(id_vtk, ioff_gl,                    &
     &        vtk_tensor_head(field_name(j)))
!
          call write_vtk_tensor_mpi(id_vtk, ioff_gl,                    &
     &          nnod, d_nod(1,icou), istack_merged_intnod)
        end if
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine write_vtk_data_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_mesh_mpi(id_vtk, ioff_gl,                    &
     &          nnod, nele, nnod_ele, xx, ie,                           &
     &          istack_merged_intnod, istack_merged_ele)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_ele(0:nprocs)
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nnod, nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint_gl) :: ie0(nnod_ele)
      integer(kind = kint_gl) :: iele, nt_nod, nt_ele
      integer(kind = kint) :: icellid
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      nt_nod = istack_merged_intnod(nprocs)
      nt_ele = istack_merged_ele(nprocs)
!
      call write_vtk_header_mpi                                         &
     &   (id_vtk, ioff_gl, vtk_node_head(nt_nod))
      call write_vtk_vecotr_mpi(id_vtk, ioff_gl,                        &
     &    nnod, xx(1,1), istack_merged_intnod)
!
      call write_vtk_header_mpi                                         &
     &   (id_vtk, ioff_gl, vtk_connect_head(nt_ele, nnod_ele))
!
      call write_vtk_connect_mpi(id_vtk, ioff_gl, nt_ele,               &
     &   nele, nnod_ele, ie, istack_merged_ele(my_rank))
!
!
      call write_vtk_header_mpi                                         &
     &   (id_vtk, ioff_gl, vtk_cell_type_head(nt_ele))
!
      icellid = vtk_cell_type(nnod_ele)
      write(textbuf,'(a,a1)') vtk_each_cell_type(icellid), char(0)
      ilength = len(vtk_each_cell_type(icellid))
      ioffset = int(ioff_gl + ilength * istack_merged_ele(my_rank))
      do iele = 1, nele
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength, MPI_CHARACTER,    &
     &                      sta1, ierr_MPI)
        ioffset = ioffset + ilength
      end do
      ioff_gl = ioff_gl + ilength * nt_ele
!
      end subroutine write_vtk_mesh_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_header_mpi(id_vtk, ioff_gl, header_txt)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=*), intent(in) :: header_txt
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
!
!      write(textbuf,'(a,a1)') ≈
      ilength = len(header_txt)
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, header_txt, ilength, MPI_CHARACTER, &
     &                      sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength
!
      end subroutine write_vtk_header_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_scalar_mpi(id_vtk, ioff_gl,                  &
     &          nnod, vect, istack_merged_intnod)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: nnod
      real(kind = kreal), intent(in) :: vect(nnod)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
      integer(kind = kint_gl) :: inod, num
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      ilength = len(vtk_each_scalar(zero))
      ioffset = int(ioff_gl                                             &
     &         + ilength * istack_merged_intnod(my_rank))
      do inod = 1, num
        write(textbuf,'(a,a1)')                                         &
     &      vtk_each_scalar(vect(inod)), char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &      MPI_CHARACTER, sta1, ierr_MPI)
        ioffset = ioffset + ilength
      end do
      ioff_gl = ioff_gl + ilength * istack_merged_intnod(nprocs)
!
      end subroutine write_vtk_scalar_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_vecotr_mpi(id_vtk, ioff_gl,                  &
     &          nnod, vect, istack_merged_intnod)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: nnod
      real(kind = kreal), intent(in) :: vect(nnod,3)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
      integer(kind = kint_gl) :: inod, num
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      ilength = len(vtk_each_vector(zero, zero, zero))
      ioffset = int(ioff_gl                                             &
     &             + ilength * istack_merged_intnod(my_rank))
      do inod = 1, num
        write(textbuf,'(a,a1)')                                         &
     &     vtk_each_vector(vect(inod,1), vect(inod,2), vect(inod,3)),   &
     &     char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &      MPI_CHARACTER, sta1, ierr_MPI)
        ioffset = ioffset + ilength
      end do
      ioff_gl = ioff_gl + ilength * istack_merged_intnod(nprocs)
!
      end subroutine write_vtk_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_tensor_mpi(id_vtk, ioff_gl,                  &
     &          nnod, vect, istack_merged_intnod)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: nnod
      real(kind = kreal), intent(in) :: vect(nnod,6)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
      integer(kind = kint_gl) :: inod, num
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      ilength = len(vtk_each_vector(zero, zero, zero))
      ioffset = int(ioff_gl                                             &
     &             + ilength * istack_merged_intnod(my_rank))
      do inod = 1, num
        write(textbuf,'(a,a1)')                                         &
     &     vtk_each_vector(vect(inod,1), vect(inod,2), vect(inod,3)),   &
     &     char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &      MPI_CHARACTER, sta1, ierr_MPI)
        ioffset = ioffset + ilength
        write(textbuf,'(a,a1)')                                         &
     &     vtk_each_vector(vect(inod,2), vect(inod,4), vect(inod,5)),   &
     &     char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &      MPI_CHARACTER, sta1, ierr_MPI)
        ioffset = ioffset + ilength
        write(textbuf,'(a,a1)')                                         &
     &     vtk_each_vector(vect(inod,3), vect(inod,5), vect(inod,6)),   &
     &     char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &      MPI_CHARACTER, sta1, ierr_MPI)
        ioffset = ioffset + ilength
      end do
      ioff_gl = ioff_gl + 3*ilength * istack_merged_intnod(nprocs)
!
      end subroutine write_vtk_tensor_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_connect_mpi(id_vtk, ioff_gl, nt_ele,         &
     &          nele, nnod_ele, ie, istack)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele, nt_ele, istack
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint_gl) :: ie0(nnod_ele)
      integer(kind = kint_gl) :: iele
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      ie0(1:nnod_ele) = 0
      ilength = len(vtk_each_connect(nnod_ele, ie0))
      ioffset = int(ioff_gl + ilength * istack)
      do iele = 1, nele
        ie0(1:nnod_ele) = ie(iele,1:nnod_ele) - 1
        write(textbuf,'(a,a1)') vtk_each_connect(nnod_ele,ie0), char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength, MPI_CHARACTER,    &
     &                      sta1, ierr_MPI)
        ioffset = ioffset + ilength
      end do
      ioff_gl = ioff_gl + ilength * nt_ele
!
      end subroutine write_vtk_connect_mpi
!
! -----------------------------------------------------------------------
!
      end module vtk_file_MPI_IO
