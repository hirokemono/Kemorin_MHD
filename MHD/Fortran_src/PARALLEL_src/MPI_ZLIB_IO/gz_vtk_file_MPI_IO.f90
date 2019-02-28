!>@file  gz_vtk_file_MPI_IO.f90
!!       module gz_vtk_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usging MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_vtk_data_mpi(id_vtk, ioff_gl,               &
!!     &          nnod, num_field, ntot_comp, ncomp_field,              &
!!     &          field_name, d_nod, istack_merged_intnod)
!!      subroutine gz_write_vtk_mesh_mpi(id_vtk, ioff_gl,               &
!!     &          nnod, nele, nnod_ele, xx, ie,                         &
!!     &          istack_merged_intnod, istack_merged_ele)
!!@endverbatim
!
      module gz_vtk_file_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use vtk_data_to_buffer
!
      implicit none
!
      character(len=1), allocatable :: gzip_buf(:)
!
      private :: gz_write_vtk_scalar_mpi
      private :: gz_write_vtk_tensor_mpi, gz_write_vtk_vecotr_mpi
      private :: gz_write_vtk_connect_mpi, gz_write_vtk_celltype_mpi
!
      integer(kind = kint), parameter, private :: maxline = 10000
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_vtk_data_mpi(id_vtk, ioff_gl,                 &
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
!
!
      nt_nod = istack_merged_intnod(nprocs)
!
!     Field header
!
      call gz_write_vtk_header_mpi                                      &
     &   (id_vtk, ioff_gl, vtk_fields_head(nt_nod))
!
!      field loop
!
      if(ntot_comp .le. 0) return
      icou = 1
      do j = 1, num_field
!
!         Scalar
        if ( ncomp_field(j) .eq. n_scalar) then
          call gz_write_vtk_header_mpi(id_vtk, ioff_gl,                 &
     &        vtk_scalar_head(field_name(j)))
!
          call gz_write_vtk_scalar_mpi(id_vtk, ioff_gl,                 &
     &        nnod, d_nod(1,icou), istack_merged_intnod)
!
        else if ( ncomp_field(j) .eq. n_vector) then
          call gz_write_vtk_header_mpi(id_vtk, ioff_gl,                 &
     &        vtk_vector_head(field_name(j)))
!
          call gz_write_vtk_vecotr_mpi(id_vtk, ioff_gl,                 &
     &       nnod, d_nod(1,icou), istack_merged_intnod)
!
        else if ( ncomp_field(j) .eq. n_sym_tensor) then
          call gz_write_vtk_header_mpi(id_vtk, ioff_gl,                 &
     &        vtk_tensor_head(field_name(j)))
!
          call gz_write_vtk_tensor_mpi(id_vtk, ioff_gl,                 &
     &          nnod, d_nod(1,icou), istack_merged_intnod)
        end if
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine gz_write_vtk_data_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_mesh_mpi(id_vtk, ioff_gl,                 &
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
      integer(kind = kint_gl) :: nt_nod, nt_ele
!
!
      nt_nod = istack_merged_intnod(nprocs)
      nt_ele = istack_merged_ele(nprocs)
!
!       Node header
!
      call gz_write_vtk_header_mpi                                      &
     &   (id_vtk, ioff_gl, vtk_node_head(nt_nod))
!
!      Node position
!
      call gz_write_vtk_vecotr_mpi(id_vtk, ioff_gl,                     &
     &    nnod, xx, istack_merged_intnod)
!
!        Element connectivity
!
      call gz_write_vtk_header_mpi                                      &
     &   (id_vtk, ioff_gl, vtk_connect_head(nt_ele, nnod_ele))
!
      call gz_write_vtk_connect_mpi                                     &
     &   (id_vtk, ioff_gl, nele, nnod_ele, ie)
!
!       Element type
!
      call gz_write_vtk_header_mpi                                      &
     &   (id_vtk, ioff_gl, vtk_cell_type_head(nt_ele))
!
      call gz_write_vtk_celltype_mpi(id_vtk, ioff_gl, nele, nnod_ele)
!
      end subroutine gz_write_vtk_mesh_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_header_mpi(id_vtk, ioff_gl, header_txt)
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
!
      if(my_rank .eq. 0) then
        ilength = len(header_txt)
        ilen_gz32 = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz32))
        call gzip_defleat_once(ilength, header_txt,                     &
     &      ilen_gz32, ilen_gzipped32, gzip_buf(1))
!
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_write_chara                               &
     &    (id_vtk, ioffset, ilen_gzipped32, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped32, ione, CALYPSO_INTEGER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped32
!
      end subroutine gz_write_vtk_header_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_scalar_mpi(id_vtk, ioff_gl,               &
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
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped
      integer(kind = kint_gl) :: num
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      ilength = len(vtk_each_scalar(zero))
      ilen_gz = int(dble(num*ilength) * 1.01 + 24,KIND(ilen_gz))
      allocate(gzip_buf(ilen_gz))
!
      call defleate_vtk_scalar(nnod, num, vect,                         &
     &   ilength, ilen_gz, gzip_buf, ilen_gzipped)
!
      call calypso_gz_mpi_seek_write                                    &
     &   (id_vtk, ioff_gl, ilen_gzipped, gzip_buf(1))
      deallocate(gzip_buf)
!
      end subroutine gz_write_vtk_scalar_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_vecotr_mpi(id_vtk, ioff_gl,               &
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
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped
      integer(kind = kint_gl) :: num
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      ilength = len(vtk_each_vector(zero, zero, zero))
      ilen_gz = int(dble(num*ilength) * 1.01 + 24,KIND(ilen_gz))
      allocate(gzip_buf(ilen_gz))
!
      call defleate_vtk_vector(nnod, num, vect,                         &
     &    ilength, ilen_gz, gzip_buf, ilen_gzipped)
!
      call calypso_gz_mpi_seek_write                                    &
     &   (id_vtk, ioff_gl, ilen_gzipped, gzip_buf(1))
      deallocate(gzip_buf)
!
      end subroutine gz_write_vtk_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_tensor_mpi(id_vtk, ioff_gl,               &
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
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) ::  num
      integer(kind = kint) :: ip, ilength
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped
      integer(kind = kint_gl) :: ilen_gzipped_list(nprocs)
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      ilength = len(vtk_each_vector(zero, zero, zero))
      ilen_gz = int(real(3*num*ilength) * 1.01) + 24
      allocate(gzip_buf(ilen_gz))
!
      call defleate_vtk_tensor(nnod, num, vect,                         &
     &    ilength, ilen_gz, gzip_buf, ilen_gzipped)
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_GLOBAL_INT,        &
     &    ilen_gzipped_list(1), ione, CALYPSO_GLOBAL_INT,               &
     &    CALYPSO_COMM, ierr_MPI)
      ioffset = int(ioff_gl)
      do ip = 1, my_rank
        ioffset = ioffset + ilen_gzipped_list(ip)
      end do
!
      if(ilen_gzipped .gt. 0) then
        call calypso_mpi_seek_long_write_gz                             &
     &    (id_vtk, ioffset, ilen_gzipped, gzip_buf(1))
      end if
      do ip = 1, nprocs
        ioff_gl = ioff_gl + ilen_gzipped_list(ip)
      end do
      deallocate(gzip_buf)
!
      end subroutine gz_write_vtk_tensor_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_connect_mpi(id_vtk, ioff_gl,              &
     &          nele, nnod_ele, ie)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint_gl) :: ie0(nnod_ele)
!
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped
!
!
      ie0(1:nnod_ele) = 0
      ilength = len(vtk_each_connect(nnod_ele, ie0))
      ilen_gz = int(real(nele*ilength) * 1.01) + 24
      allocate(gzip_buf(ilen_gz))
!
      call defleate_vtk_connect(nele, ie, nnod_ele,                     &
     &    ilength, ilen_gz, gzip_buf, ilen_gzipped)
!
      call calypso_gz_mpi_seek_write                                    &
     &   (id_vtk, ioff_gl, ilen_gzipped, gzip_buf(1))
      deallocate(gzip_buf)
!
      end subroutine gz_write_vtk_connect_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_celltype_mpi                              &
     &         (id_vtk, ioff_gl, nele, nnod_ele)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
      integer(kind = kint) :: icellid
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped
!
!
      icellid = vtk_cell_type(nnod_ele)
      ilength = len(vtk_each_cell_type(icellid))
      ilen_gz = int(dble(nele*ilength) * 1.01 + 24,KIND(ilen_gz))
      allocate(gzip_buf(ilen_gz))
!
      call defleate_vtk_celltype                                        &
     &   (nele, ilength, vtk_each_cell_type(icellid),                   &
     &   ilen_gz, gzip_buf, ilen_gzipped)
!
      call calypso_gz_mpi_seek_write                                    &
     &   (id_vtk, ioff_gl, ilen_gzipped, gzip_buf(1))
      deallocate(gzip_buf)
!
      end subroutine gz_write_vtk_celltype_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine defleate_vtk_scalar(nnod, num, vect,               &
     &          ilen_line, ilen_gz, gzip_buf, ilen_gzipped)
!
      integer(kind = kint_gl), intent(in) :: nnod, num
      real(kind = kreal), intent(in) :: vect(nnod)
!
      integer(kind = kint), intent(in) :: ilen_line
!
      integer(kind = kint_gl), intent(in) :: ilen_gz
      character(len=1), intent(inout) :: gzip_buf(ilen_gz)
      integer(kind = kint_gl), intent(inout) :: ilen_gzipped
!
      integer(kind = kint_gl) :: inod, ist, nline
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_used, ilen_in
!
!
      if(num .eq. 1) then
        ilen_in = int(ilen_gz)
        call gzip_defleat_once(ilen_line, vtk_each_scalar(vect(1)),    &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
!
      else if(num .gt. 1) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = int(dble(maxline*ilen_line)*1.01+24,KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*) 'defleate_vtk_tensor start ',    &
!     &      num, ilen_line, ilen_gz, ilen_tmp
        do
          nline = min((num - ist), maxline)
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1,  ilen_in
          call gzip_defleat_begin                                       &
     &      (ilen_line, vtk_each_scalar(vect(ist+1)),                   &
     &       ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
!
          do inod = ist+2, ist+nline-1
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_scalar(vect(inod)), ilen_in, ilen_used)
          end do
          call gzip_defleat_last(ilen_line,                             &
     &        vtk_each_scalar(vect(ist+nline)), ilen_in, ilen_used)
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
      end subroutine defleate_vtk_scalar
!
! -----------------------------------------------------------------------
!
      subroutine defleate_vtk_vector(nnod, num, vect,               &
     &          ilen_line, ilen_gz, gzip_buf, ilen_gzipped)
!
      integer(kind = kint_gl), intent(in) :: nnod, num
      real(kind = kreal), intent(in) :: vect(nnod,3)
!
      integer(kind = kint), intent(in) :: ilen_line
!
      integer(kind = kint_gl), intent(in) :: ilen_gz
      character(len=1), intent(inout) :: gzip_buf(ilen_gz)
      integer(kind = kint_gl), intent(inout) :: ilen_gzipped
!
      integer(kind = kint_gl) :: inod, ist, nline
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_used, ilen_in
!
!
      if(num .eq. 1) then
        ilen_in = int(ilen_gz)
        call gzip_defleat_once(ilen_line,                               &
     &      vtk_each_vector(vect(1,1),vect(1,2),vect(1,3)),             &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
!
      else if(num .gt. 1) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = int(dble(maxline*ilen_line)*1.01+24,KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*) 'defleate_vtk_tensor start ',    &
!     &      num, ilen_line, ilen_gz, ilen_tmp
        do
          nline = min((num - ist), maxline)
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1,  ilen_in
          call gzip_defleat_begin(ilen_line,                            &
     &      vtk_each_vector(vect(ist+1,1),vect(ist+1,2),vect(ist+1,3)), &
     &      ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
!
          do inod = ist+2, ist+nline-1
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_vector                                         &
     &                     (vect(inod,1),vect(inod,2),vect(inod,3)),    &
     &          ilen_in, ilen_used)
          end do
          call gzip_defleat_last(ilen_line,                             &
     &        vtk_each_vector                                           &
     &         (vect(ist+nline,1),vect(ist+nline,2),vect(ist+nline,3)), &
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
      end subroutine defleate_vtk_vector
!
! -----------------------------------------------------------------------
!
      subroutine defleate_vtk_tensor(nnod, num, vect,               &
     &          ilen_line, ilen_gz, gzip_buf, ilen_gzipped)
!
      integer(kind = kint_gl), intent(in) :: nnod, num
      real(kind = kreal), intent(in) :: vect(nnod,6)
!
      integer(kind = kint), intent(in) :: ilen_line
!
      integer(kind = kint_gl), intent(in) :: ilen_gz
      character(len=1), intent(inout) :: gzip_buf(ilen_gz)
      integer(kind = kint_gl), intent(inout) :: ilen_gzipped
!
      integer(kind = kint_gl) :: inod, ist, nline
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_used, ilen_in
!
!
      if(num .eq. 1) then
        ilen_in = int(ilen_gz)
        call gzip_defleat_begin(ilen_line,                              &
     &      vtk_each_vector(vect(1,1),vect(1,2),vect(1,3)),             &
     &      ilen_in, ilen_used, gzip_buf(1))
        call gzip_defleat_cont(ilen_line,                               &
     &      vtk_each_vector(vect(1,2),vect(1,4),vect(1,5)),             &
     &      ilen_in, ilen_used)
        call gzip_defleat_last(ilen_line,                               &
     &      vtk_each_vector(vect(1,3),vect(1,5),vect(1,6)),             &
     &      ilen_in, ilen_used)
        ilen_gzipped = ilen_used
!
      else if(num .gt. 1) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = int(dble(maxline*ilen_line)*1.01+24,KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*) 'defleate_vtk_tensor start ',    &
!     &      num, ilen_line, ilen_gz, ilen_tmp
        do
          nline = min((num - ist), maxline)
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1,  ilen_in
          call gzip_defleat_begin(ilen_line,                            &
     &      vtk_each_vector(vect(ist+1,1),vect(ist+1,2),vect(ist+1,3)), &
     &      ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
          call gzip_defleat_cont(ilen_line,                             &
     &      vtk_each_vector(vect(ist+1,2),vect(ist+1,4),vect(ist+1,5)), &
     &      ilen_in, ilen_used)
          call gzip_defleat_cont(ilen_line,                             &
     &      vtk_each_vector(vect(ist+1,3),vect(ist+1,5),vect(ist+1,6)), &
     &      ilen_in, ilen_used)
!
          do inod = ist+2, ist+nline-1
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_vector                                         &
     &                     (vect(inod,1),vect(inod,2),vect(inod,3)),    &
     &          ilen_in, ilen_used)
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_vector                                         &
     &                     (vect(inod,2),vect(inod,4),vect(inod,5)),    &
     &          ilen_in, ilen_used)
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_vector                                         &
     &                     (vect(inod,3),vect(inod,5),vect(inod,6)),    &
     &          ilen_in, ilen_used)
          end do
          call gzip_defleat_cont(ilen_line,                             &
     &        vtk_each_vector                                           &
     &         (vect(ist+nline,1),vect(ist+nline,2),vect(ist+nline,3)), &
     &        ilen_in, ilen_used)
          call gzip_defleat_cont(ilen_line,                             &
     &        vtk_each_vector                                           &
     &         (vect(ist+nline,2),vect(ist+nline,4),vect(ist+nline,5)), &
     &        ilen_in, ilen_used)
          call gzip_defleat_last(ilen_line,                             &
     &        vtk_each_vector                                           &
     &         (vect(ist+nline,3),vect(ist+nline,5),vect(ist+nline,6)), &
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
      end subroutine defleate_vtk_tensor
!
! -----------------------------------------------------------------------
!
      subroutine defleate_vtk_connect(nele, ie, nnod_ele,               &
     &          ilen_line, ilen_gz, gzip_buf, ilen_gzipped)
!
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
      integer(kind = kint_gl) :: i, ist, nline
      integer(kind = kint_gl) :: ie0(nnod_ele)
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_used, ilen_in
!
!
      if(nele .eq. 1) then
        ilen_in = int(ilen_gz)
        ie0(1:nnod_ele) = ie(1,1:nnod_ele) - 1
        call gzip_defleat_once(ilen_line,                               &
     &      vtk_each_connect(nnod_ele, ie0),                            &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
!
      else if(nele .gt. 1) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = int(dble(maxline*ilen_line)*1.01+24,KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*) 'defleate_vtk_celltype start ',  &
!     &      nele, ilen_line, ilen_gz, ilen_tmp
        do
          nline = min((nele - ist), maxline)
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1,  ilen_in
          ie0(1:nnod_ele) = ie(ist+1,1:nnod_ele) - 1
          call gzip_defleat_begin(ilen_line,                            &
     &        vtk_each_connect(nnod_ele, ie0),                          &
     &        ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
          do i = ist+2, ist+nline-1
            ie0(1:nnod_ele) = ie(i,1:nnod_ele) - 1
            call gzip_defleat_cont(ilen_line,                           &
     &          vtk_each_connect(nnod_ele, ie0),                        &
     &          ilen_in, ilen_used)
          end do
!
          ie0(1:nnod_ele) = ie(ist+nline,1:nnod_ele) - 1
          call gzip_defleat_last(ilen_line,                             &
     &        vtk_each_connect(nnod_ele, ie0),                          &
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
      end subroutine defleate_vtk_connect
!
! -----------------------------------------------------------------------
!
      subroutine defleate_vtk_celltype                                  &
     &         (nele, ilen_line, cell_type_name,                        &
     &          ilen_gz, gzip_buf, ilen_gzipped)
!
      integer(kind = kint_gl), intent(in) :: nele
!
      integer(kind = kint), intent(in) :: ilen_line
      character(len=ilen_line), intent(in) :: cell_type_name
!
      integer(kind = kint_gl), intent(in) :: ilen_gz
      character(len=1), intent(inout) :: gzip_buf(ilen_gz)
      integer(kind = kint_gl), intent(inout) :: ilen_gzipped
!
      integer(kind = kint_gl) :: i, ist, nline
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_used, ilen_in
!
!
      if(nele .eq. 1) then
        ilen_in = int(ilen_gz)
        call gzip_defleat_once(ilen_line, cell_type_name,               &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
!
      else if(nele .gt. 1) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = int(dble(maxline*ilen_line)*1.01+24,KIND(ilen_tmp))
!        if(my_rank .eq. 0) write(*,*) 'defleate_vtk_celltype start ',  &
!     &      nele, ilen_line, ilen_gz, ilen_tmp
        do
          nline = min((nele - ist), maxline)
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1,  ilen_in
          call gzip_defleat_begin(ilen_line, cell_type_name,            &
     &        ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
          do i = ist+2, ist+nline-1
            call gzip_defleat_cont(ilen_line, cell_type_name,           &
     &          ilen_in, ilen_used)
          end do
          call gzip_defleat_last(ilen_line, cell_type_name,             &
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
      end subroutine defleate_vtk_celltype
!
! -----------------------------------------------------------------------
!
      end module gz_vtk_file_MPI_IO
