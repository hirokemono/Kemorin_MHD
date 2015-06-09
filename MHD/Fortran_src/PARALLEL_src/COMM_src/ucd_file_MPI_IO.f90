!>@file  ucd_file_MPI_IO.f90
!!       module ucd_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine write_ucd_file_mpi(file_name, ucd, m_ucd)
!!      subroutine write_ucd_phys_mpi(file_name, ucd, m_ucd)
!!      subroutine write_ucd_grid_mpi(file_name, ucd, m_ucd)
!!@endverbatim
!
      module ucd_file_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use vtk_data_to_buffer
      use t_ucd_data
      use m_merged_ucd_data
      use ucd_data_to_buffer
!
      implicit none
!
      character(len=65536), private :: textbuf
!
      private :: write_ucd_data_mpi, write_ucd_mesh_mpi
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_ucd_file_mpi(file_name, ucd, m_ucd)
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
      call write_ucd_mesh_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%ntot_comp,            &
     &    ucd%xx, ucd%ie,           &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call write_ucd_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call MPI_FILE_CLOSE(id_vtk, ierr_MPI)
!
      end subroutine write_ucd_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_phys_mpi(file_name, ucd, m_ucd)
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
      call write_ucd_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call MPI_FILE_CLOSE(id_vtk, ierr_MPI)
!
      end subroutine write_ucd_phys_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_grid_mpi(file_name, ucd, m_ucd)
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
      call write_ucd_mesh_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%ntot_comp,            &
     &    ucd%xx, ucd%ie,           &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call MPI_FILE_CLOSE(id_vtk, ierr_MPI)
!
      end subroutine write_ucd_grid_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_ucd_data_mpi(id_vtk, ioff_gl,                    &
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
      integer(kind = kint) :: j
      integer(kind = kint_gl) :: inod, num, inod_gl
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
      real(kind = kreal)  :: dat_1(ntot_comp)
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      ioffset = int(ioff_gl)
      ilength = len(ucd_num_comps(num_field, ncomp_field))
      if(my_rank .eq. 0) then
        write(textbuf,'(a,a1)')                                         &
     &          ucd_num_comps(num_field, ncomp_field), char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength
!
      do j = 1, num_field
        ioffset = int(ioff_gl)
        ilength = len(ucd_field_name(field_name(j)))
        if(my_rank .eq. 0) then
          write(textbuf,'(a,a1)')                                       &
     &                ucd_field_name(field_name(j)), char(0)
          call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
          call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                 &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
        end if
        ioff_gl = ioff_gl + ilength
      end do
!
      inod_gl = 1 + istack_merged_intnod(my_rank)
      dat_1(1:ntot_comp) = d_nod(1,1:ntot_comp)
      ilength = len(ucd_each_field(inod_gl, ntot_comp, dat_1))
      ioffset = int(ioff_gl + ilength * istack_merged_intnod(my_rank))
      do inod = 1, num
        inod_gl =    inod + istack_merged_intnod(my_rank)
        dat_1(1:ntot_comp) = d_nod(inod,1:ntot_comp)
        write(textbuf,'(a,a1)')                                         &
     &      ucd_each_field(inod_gl, ntot_comp, dat_1), char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &      CALYPSO_CHARACTER, sta1, ierr_MPI)
        ioffset = ioffset + ilength
      end do
      ioff_gl = ioff_gl + ilength * istack_merged_intnod(nprocs)
!
      end subroutine write_ucd_data_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_mesh_mpi(id_vtk, ioff_gl,                    &
     &          nnod, nele, nnod_ele, ntot_comp, xx, ie,                &
     &          istack_merged_intnod, istack_merged_ele)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_ele(0:nprocs)
      integer(kind = kint), intent(in) :: nnod_ele, ntot_comp
      integer(kind = kint_gl), intent(in) :: nnod, nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint_gl) :: ie0(nnod_ele), inod_gl, iele_gl
      integer(kind = kint_gl) :: iele, inod, nt_nod, nt_ele, num
      real(kind = kreal)  :: dat_1(n_vector)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset, ilength
!
!
      nt_nod = istack_merged_intnod(nprocs)
      nt_ele = istack_merged_ele(nprocs)
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      write(textbuf,'(a,a1)')                                           &
     &      ucd_connect_head(nt_nod, nt_ele, ntot_comp), char(0)
      ilength = len(ucd_connect_head(nt_nod, nt_ele, ntot_comp))
      if(my_rank .eq. 0) then
        ioffset = int(ioff_gl)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
      end if
      ioff_gl = ioff_gl + ilength

      inod_gl = 1
      dat_1(1:n_vector) = zero
      ilength = len(ucd_each_field(inod_gl, n_vector, dat_1))
      ioffset = int(ioff_gl + ilength * istack_merged_intnod(my_rank))
      do inod = 1, num
        inod_gl =    inod + istack_merged_intnod(my_rank)
        dat_1(1:n_vector) = xx(inod,1:n_vector)
        write(textbuf,'(a,a1)')                                         &
     &      ucd_each_field(inod_gl, n_vector, dat_1), char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &      CALYPSO_CHARACTER, sta1, ierr_MPI)
        ioffset = ioffset + ilength
      end do
      ioff_gl = ioff_gl + ilength * nt_nod
!
!
      iele_gl = 1
      ie0(1:nnod_ele) = 0
      ilength = len(ucd_each_connect(iele_gl, nnod_ele, ie0))
      ioffset = int(ioff_gl + ilength * istack_merged_ele(my_rank))
      do iele = 1, nele
        iele_gl = iele + istack_merged_ele(my_rank)
        ie0(1:nnod_ele) = ie(iele,1:nnod_ele)
        write(textbuf,'(a,a1)')                                         &
     &        ucd_each_connect(iele_gl, nnod_ele,ie0), char(0)
        call MPI_FILE_SEEK(id_vtk, ioffset, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_vtk, textbuf, ilength,                   &
     &                      CALYPSO_CHARACTER, sta1, ierr_MPI)
        ioffset = ioffset + ilength
      end do
      ioff_gl = ioff_gl + ilength * nt_ele
!
      end subroutine write_ucd_mesh_mpi
!
! -----------------------------------------------------------------------
!
      end module ucd_file_MPI_IO
