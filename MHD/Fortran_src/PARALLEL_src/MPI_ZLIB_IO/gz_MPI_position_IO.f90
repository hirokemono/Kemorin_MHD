!>@file   gz_MPI_position_IO.f90
!!@brief  module gz_MPI_position_IO
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_mpi_read_radial_position                          &
!!     &         (IO_param, nnod, id_global, rr)
!!      subroutine gz_mpi_read_node_position                            &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!
!!      subroutine gz_mpi_write_radial_position                         &
!!     &         (IO_param, nnod, id_global, rr)
!!      subroutine gz_mpi_write_node_position                           &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!@endverbatim
!
      module gz_MPI_position_IO
!
      use m_precision
      use m_constants
!
      use t_calypso_mpi_IO_param
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use MPI_ascii_data_IO
!
      implicit none
!
      integer(kind = kint), parameter, private :: maxline = 10000
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_radial_position                            &
     &         (IO_param, nnod, id_global, rr)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint_gl), intent(inout) :: id_global(nnod)
      real(kind=kreal), intent(inout) :: rr(nnod)
!
      call gz_mpi_read_node_position                                    &
     &   (IO_param, nnod, ione, id_global, rr(1))
!
      end subroutine gz_mpi_read_radial_position
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_node_position                              &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(inout) :: id_global(nnod)
      real(kind=kreal), intent(inout) :: xx(nnod, numdir)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: ilen_gz
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      call read_int8_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param,                               &
     &      len_multi_int_textline(IO_param%nprocs_in)),                &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ilen_gz = IO_param%istack_merged(IO_param%id_rank+1)              &
     &            - IO_param%istack_merged(IO_param%id_rank)
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(ilen_gz .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
!
      allocate(gzip_buf(ilen_gz))
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,          &
     &    int(ilen_gz), gzip_buf(1))
!
      call infleate_node_position                                       &
     &   (ilen_gz, gzip_buf, nnod, numdir, id_global, xx)
!
      deallocate(gzip_buf)
!
      end subroutine gz_mpi_read_node_position
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_radial_position                           &
     &         (IO_param, nnod, id_global, rr)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: rr(nnod)
!
      call gz_mpi_write_node_position                                   &
     &         (IO_param, nnod, ione, id_global, rr(1))
!
      end subroutine gz_mpi_write_radial_position
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_node_position                             &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: ilen_gz, ilen_gzipped
      integer(kind = kint) :: ilen_line
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      call gz_mpi_write_num_of_data(IO_param, nnod)
!
      ilen_line = len_int8_and_vector_textline(numdir)
      ilen_gz = dble(nnod*ilen_line) * 1.01 + 24
      allocate(gzip_buf(ilen_gz))
!
      call defleate_node_position(nnod, numdir, id_global, xx,          &
     &    ilen_line, ilen_gz, gzip_buf, ilen_gzipped)
!
      call gz_mpi_write_stack_over_domain(IO_param, int(ilen_gzipped))
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      int(ilen_gzipped), gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_node_position
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine infleate_node_position                              &
     &         (ilen_gz, gzip_buf, nnod, numdir, id_global, xx)
!
      integer(kind = kint_gl), intent(in) :: ilen_gz
      character(len=1), intent(in) :: gzip_buf(ilen_gz)
!
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: numdir
      integer(kind=kint_gl), intent(inout) :: id_global(nnod)
      real(kind=kreal), intent(inout) :: xx(nnod, numdir)
!
      integer(kind = kint) :: i, ist
      real(kind = kreal) :: xx_tmp(numdir)
!
      integer(kind = kint_gl) :: ilen_gzipped, ilen_tmp
      integer(kind = kint) :: ilen_line, ilen_used, ilen_in
      integer(kind = kint) :: nline
!
      character(len=1), allocatable :: textbuf(:)
!
!
      ilen_line = len_int8_and_vector_textline(numdir)
      allocate(textbuf(ilen_line))
!
      if(nnod .le. 0) then
        ilen_in = int(ilen_gz)
        call gzip_infleat_once                                          &
     &    (ilen_in, gzip_buf(1), ione, textbuf(1), ilen_used)
        ilen_gzipped = ilen_used
      else if(nnod .eq. 1) then
        ilen_in = int(ilen_gz)
        call gzip_infleat_once                                          &
     &    (ilen_in, gzip_buf(1), ilen_line, textbuf(1), ilen_used)
        call read_int8_and_vector_textline                              &
     &     (textbuf(1), id_global(1), numdir, xx(1,1))
        ilen_gzipped = ilen_used
      else if(nnod .gt. 0) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = dble(maxline*ilen_line) * 1.01 + 24
!        if(my_rank .eq. 0) write(*,*) 'all start ',                    &
!     &      nnod, ilen_line, ilen_gz, ilen_tmp
!
        do
          nline = int(min((nnod - ist), maxline))
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1,  ilen_in
          call gzip_infleat_begin(ilen_in, gzip_buf(ilen_gzipped+1),    &
     &        ilen_line, textbuf(1), ilen_used)
!          if(my_rank .eq. 0) write(*,*) 'gzip_infleat_begin',          &
!     &                      ilen_used
          call read_int8_and_vector_textline                            &
     &       (textbuf(1), id_global(ist+1), numdir, xx_tmp)
          xx(ist+1,1:numdir) = xx_tmp(1:numdir)
!
          do i = ist+2, ist+nline-1
            call gzip_infleat_cont                                      &
     &         (ilen_in, ilen_line, textbuf(1), ilen_used)
            call read_int8_and_vector_textline                          &
     &         (textbuf(1), id_global(i), numdir, xx_tmp)
            xx(i,1:numdir) = xx_tmp(1:numdir)
          end do
!          if(my_rank .eq. 0) write(*,*) 'gzip_infleat_cont',           &
!     &                      ilen_used
!
          call gzip_infleat_last                                        &
     &       (ilen_in, ilen_line, textbuf(1), ilen_used)
!          if(my_rank .eq. 0) write(*,*) 'gzip_infleat_last',           &
!     &                      ilen_used
          call read_int8_and_vector_textline                            &
     &       (textbuf(1), id_global(ist+nline), numdir, xx_tmp)
          xx(ist+nline,1:numdir) = xx_tmp(1:numdir)
!
          ilen_gzipped = ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nnod) exit
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', ilen_gzipped
      end if
!
      deallocate(textbuf)
!
      end subroutine infleate_node_position
!
! -----------------------------------------------------------------------
!
      subroutine defleate_node_position(nnod, numdir, id_global, xx,    &
     &           ilen_line, ilen_gz, gzip_buf, ilen_gzipped)
!
      integer(kind=kint), intent(in) :: nnod
      integer(kind=kint), intent(in) :: numdir
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = kint), intent(in) :: ilen_line
      integer(kind = kint_gl), intent(in) :: ilen_gz
      character(len=1), intent(inout) :: gzip_buf(ilen_gz)
      integer(kind = kint_gl), intent(inout) :: ilen_gzipped
!
      integer(kind = kint_gl) ::  i, ist, nline
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = kint_gl) :: ilen_tmp
      integer(kind = kint) :: ilen_used, ilen_in
!
!
      if(nnod .le. 0) then
        ilen_in = int(ilen_gz)
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
      else if(nnod .eq. 1) then
        ilen_in = int(ilen_gz)
        call gzip_defleat_once(ilen_line,                               &
     &      int8_and_vector_textline                                    &
     &         (id_global(1), numdir, xx(1,1)),                         &
     &      ilen_in, ilen_used, gzip_buf(1))
        ilen_gzipped = ilen_used
      else if(nnod .gt. 0) then
        ist = 0
        ilen_gzipped = 0
        ilen_tmp = dble(maxline*ilen_line) * 1.01 + 24
!        if(my_rank .eq. 0) write(*,*)                                  &
!     &     'defleate_node_position start ',                            &
!     &      nnod, ilen_line, ilen_gz, ilen_tmp
        do
          nline = int(min((nnod - ist), maxline))
          ilen_in = int(min(ilen_gz-ilen_gzipped, ilen_tmp))
!
!          if(my_rank .eq. 0) write(*,*) 'start ',                      &
!     &      ist+1, ist+nline, nline, ilen_gzipped+1, ilen_in
          xx_tmp(1:numdir) = xx(ist+1,1:numdir)
          call gzip_defleat_begin(ilen_line,                            &
     &      int8_and_vector_textline(id_global(ist+1), numdir, xx_tmp), &
     &      ilen_in, ilen_used, gzip_buf(ilen_gzipped+1))
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_begin', ilen_used
!
          do i = ist+2, ist+nline-1
            xx_tmp(1:numdir) = xx(i,1:numdir)
            call gzip_defleat_cont(ilen_line,                           &
     &          int8_and_vector_textline(id_global(i), numdir, xx_tmp), &
     &          ilen_in, ilen_used)
          end do
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_cont', ilen_used
!
          xx_tmp(1:numdir) = xx(ist+nline,1:numdir)
          call gzip_defleat_last(ilen_line,                             &
     &       int8_and_vector_textline                                   &
     &          (id_global(ist+nline), numdir, xx_tmp),                 &
     &       ilen_in, ilen_used)
!          if(my_rank .eq. 0) write(*,*) 'gzip_defleat_last',           &
!     &        ilen_used, ist + nline, nnod
!
          ilen_gzipped = ilen_gzipped + ilen_used
          ist = ist + nline
          if(ist .ge. nnod) exit
        end do
!        if(my_rank .eq. 0) write(*,*) 'all done ', ilen_gzipped
      end if
!
      end subroutine defleate_node_position
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_position_IO
