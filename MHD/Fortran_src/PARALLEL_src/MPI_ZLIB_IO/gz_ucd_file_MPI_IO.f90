!>@file  gz_ucd_file_MPI_IO.f90
!!       module gz_ucd_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_ucd_data_mpi(id_vtk, ioff_gl,               &
!!     &          nnod, num_field, ntot_comp, ncomp_field,              &
!!     &          field_name, d_nod, istack_merged_intnod)
!!      subroutine gz_write_ucd_mesh_mpi(id_vtk, ioff_gl,               &
!!     &          nnod, nele, nnod_ele, ntot_comp, xx, ie,              &
!!     &          istack_merged_intnod, istack_merged_ele)
!!@endverbatim
!
      module gz_ucd_file_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use vtk_data_to_buffer
      use ucd_data_to_buffer
!
      implicit none
!
      character(len=1), allocatable :: gzip_buf(:)
!
      private :: gz_write_ucd_header_mpi
      private :: gz_write_ucd_vecotr_mpi, gz_write_ucd_connect_mpi
!
      integer(kind = kint), parameter, private :: maxline = 10000
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_data_mpi(id_vtk, ioff_gl,                 &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged_intnod)
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
!
!
      call gz_write_ucd_header_mpi(id_vtk, ioff_gl,                     &
     &    ucd_num_comps(num_field, ncomp_field))
!
      do j = 1, num_field
        call gz_write_ucd_header_mpi(id_vtk, ioff_gl,                   &
     &      ucd_field_name(field_name(j)))
      end do
!
      call gz_write_ucd_vecotr_mpi(id_vtk, ioff_gl,                     &
     &    nnod, ntot_comp, d_nod, istack_merged_intnod)
!
      end subroutine gz_write_ucd_data_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_mesh_mpi(id_vtk, ioff_gl,                 &
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
      integer(kind = kint_gl) :: nt_nod, nt_ele
!
!
      nt_nod = istack_merged_intnod(nprocs)
      nt_ele = istack_merged_ele(nprocs)
!
      call gz_write_ucd_header_mpi(id_vtk, ioff_gl,                     &
     &    ucd_connect_head(nt_nod, nt_ele, ntot_comp))
!
      call gz_write_ucd_vecotr_mpi(id_vtk, ioff_gl,                     &
     &    nnod, n_vector, xx, istack_merged_intnod)
!
      call gz_write_ucd_connect_mpi(id_vtk, ioff_gl,                    &
     &    nele, nnod_ele, ie, istack_merged_ele)
!
      end subroutine gz_write_ucd_mesh_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_header_mpi(id_vtk, ioff_gl, header_txt)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=*), intent(in) :: header_txt
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint) :: ilen_gz32, ilen_gzipped32, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ilength = len(header_txt)
        ilen_gz32 = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz32))
        call gzip_defleat_once(ilength, header_txt,                     &
     &      ilen_gz32, ilen_gzipped32, gzip_buf(1))
!
        ioffset = ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &     (id_vtk, ioffset, ilen_gzipped32, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped32, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped32
!
      end subroutine gz_write_ucd_header_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_vecotr_mpi(id_vtk, ioff_gl,               &
     &          nnod, ntot_comp, vect, istack_merged_intnod)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: vect(nnod,ntot_comp)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped
      integer(kind = kint_gl) :: num, inod_gl
      real(kind = kreal)  :: dat_1(ntot_comp)
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      inod_gl = 1
      dat_1(1:ntot_comp) = zero
      ilength = len(ucd_each_field(inod_gl, ntot_comp, dat_1))
      ilen_gz = int(real(num*ilength) * 1.01) + 24
      allocate(gzip_buf(ilen_gz))
!
      call defleate_ucd_vector                                          &
     &   (nnod, num, ntot_comp, vect, istack_merged_intnod(my_rank),    &
     &    ilength, ilen_gz, gzip_buf, ilen_gzipped)
!
      call calypso_gz_mpi_seek_write                                    &
     &   (id_vtk, ioff_gl, ilen_gzipped, gzip_buf(1))
!
      deallocate(gzip_buf)
!
      end subroutine gz_write_ucd_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_connect_mpi(id_vtk, ioff_gl,              &
     &          nele, nnod_ele, ie, istack_merged_ele)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_ele(0:nprocs)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint_gl) :: ie0(nnod_ele)
      integer(kind = kint_gl) :: iele_gl
!
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped
!
!
      iele_gl = 1
      ie0(1:nnod_ele) = 0
      ilength = len(ucd_each_connect(iele_gl, nnod_ele, ie0))
      ilen_gz = real(nele*ilength) * 1.01 + 24
      allocate(gzip_buf(ilen_gz))
!
      call defleate_ucd_connect                                         &
     &   (nele, ie, nnod_ele, istack_merged_ele(my_rank),               &
     &    ilength, ilen_gz, gzip_buf, ilen_gzipped)
!
      call calypso_gz_mpi_seek_write                                    &
     &   (id_vtk, ioff_gl, ilen_gzipped, gzip_buf(1))
      deallocate(gzip_buf)
!
      end subroutine gz_write_ucd_connect_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine defleate_ucd_vector                                    &
     &         (nnod, num, ntot_comp, vect, istack_merged_intnod,       &
     &          ilen_line, ilen_gz, gzip_buf, ilen_gzipped)
!
      integer(kind = kint_gl), intent(in) :: istack_merged_intnod
      integer(kind = kint_gl), intent(in) :: nnod, num
      integer(kind=kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: vect(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) :: ilen_line
!
      integer(kind = kint_gl), intent(in) :: ilen_gz
      character(len=1), intent(inout) :: gzip_buf(ilen_gz)
      integer(kind = kint_gl), intent(inout) :: ilen_gzipped
!
      integer(kind = kint_gl) :: inod_gl, inod, ist, nline
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_used, ilen_in
!
      real(kind = kreal)  :: dat_1(ntot_comp)
!
!
      if(num .eq. 1) then
        ilen_in = int(ilen_gz)
        inod_gl = 1 + istack_merged_intnod
        dat_1(1:ntot_comp) = vect(1,1:ntot_comp)
        call gzip_defleat_once(ilen_line,                               &
     &      ucd_each_field(inod_gl, ntot_comp, dat_1),                  &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
!
      else if(num .gt. 1) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = dble(maxline*ilen_line) * 1.01 + 24
!        if(my_rank .eq. 0) write(*,*) 'defleate_vtk_tensor start ',    &
!     &      num, ilen_line, ilen_gz, ilen_tmp
        do
          nline = min((num - ist), maxline)
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1,  ilen_in
          inod_gl = ist+1 + istack_merged_intnod
          dat_1(1:ntot_comp) = vect(ist+1,1:ntot_comp)
          call gzip_defleat_begin(ilen_line,                            &
     &        ucd_each_field(inod_gl, ntot_comp, dat_1),                &
     &        ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
!
          do inod = ist+2, ist+nline-1
            inod_gl =    inod + istack_merged_intnod
            dat_1(1:ntot_comp) = vect(inod,1:ntot_comp)
            call gzip_defleat_cont(ilen_line,                           &
     &          ucd_each_field(inod_gl, ntot_comp, dat_1),              &
     &          ilen_in, ilen_used)
          end do
          inod_gl = ist + nline + istack_merged_intnod
          dat_1(1:ntot_comp) = vect(ist+nline,1:ntot_comp)
          call gzip_defleat_last(ilen_line,                             &
     &        ucd_each_field(inod_gl, ntot_comp, dat_1),                &
     &        ilen_in, ilen_used)
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_last',           &
!     &        ilen_used, ist + nline, num
!
          ilen_gzipped = ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. num) exit
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', ilen_gzipped
      else
        ilen_gzipped = 0
      end if
!
      end subroutine defleate_ucd_vector
!
! -----------------------------------------------------------------------
!
      subroutine defleate_ucd_connect                                   &
     &         (nele, ie, nnod_ele, istack_merged_ele,                  &
     &          ilen_line, ilen_gz, gzip_buf, ilen_gzipped)
!
      integer(kind = kint_gl), intent(in) :: istack_merged_ele
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
!
      integer(kind = kint), intent(in) :: ilen_line
!
      integer(kind = kint_gl), intent(in) :: ilen_gz
      character(len=1), intent(inout) :: gzip_buf(ilen_gz)
      integer(kind = kint_gl), intent(inout) :: ilen_gzipped
!
      integer(kind = kint_gl) :: i, ist, nline, iele_gl
      integer(kind = kint_gl) :: ie0(nnod_ele)
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_used, ilen_in
!
!
      if(nele .eq. 1) then
        ilen_in = int(ilen_gz)
        iele_gl = 1
        ie0(1:nnod_ele) = ie(1,1:nnod_ele)
        call gzip_defleat_once(ilen_line,                               &
     &      ucd_each_connect(iele_gl, nnod_ele, ie0),                   &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
!
      else if(nele .gt. 1) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = dble(maxline*ilen_line) * 1.01 + 24
!        if(my_rank .eq. 0) write(*,*) 'defleate_vtk_celltype start ',  &
!     &      nele, ilen_line, ilen_gz, ilen_tmp
        do
          nline = min((nele - ist), maxline)
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1,  ilen_in
          iele_gl = ist+1 + istack_merged_ele
          ie0(1:nnod_ele) = ie(ist+1,1:nnod_ele)
          call gzip_defleat_begin(ilen_line,                            &
     &      ucd_each_connect(iele_gl, nnod_ele, ie0),                   &
     &        ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
          do i = ist+2, ist+nline-1
            iele_gl = i + istack_merged_ele
            ie0(1:nnod_ele) = ie(i,1:nnod_ele)
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_connect(nnod_ele, ie0),                        &
     &          ucd_each_connect(iele_gl, nnod_ele, ie0),               &
     &          ilen_in, ilen_used)
          end do
!
          iele_gl = ist+nline + istack_merged_ele
          ie0(1:nnod_ele) = ie(ist+nline,1:nnod_ele)
          call gzip_defleat_last(ilen_line,                             &
     &      ucd_each_connect(iele_gl, nnod_ele, ie0),                   &
     &        ilen_in, ilen_used)
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_last',           &
!     &        ilen_used, ist + nline, nele
!
          ilen_gzipped = ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nele) exit
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', ilen_gzipped
      else
        ilen_gzipped = 0
      end if
!
      end subroutine defleate_ucd_connect
!
! -----------------------------------------------------------------------
!
      end module gz_ucd_file_MPI_IO
