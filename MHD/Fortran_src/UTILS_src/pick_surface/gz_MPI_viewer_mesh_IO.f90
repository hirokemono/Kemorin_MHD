!>@file   gz_MPI_viewer_mesh_IO.f90
!!@brief  module gz_MPI_viewer_mesh_IO
!!
!!@author H.Matsui
!!@date      Programmed in Mar., 2018
!
!>@brief  Viewer mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_mpi_write_viewer_position                         &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!
!!      subroutine gz_mpi_write_viewer_element_type                     &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine gz_mpi_write_viewer_connect                          &
!!     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!!
!!      subroutine gz_mpi_write_domain_grp_data(IO_param, view_grp)
!!      subroutine gz_mpi_write_viewer_grp_data                         &
!!     &         (IO_param, num_grp, grp_name, view_grp)
!!        type(viewer_group_data), intent(in) :: view_grp
!!      subroutine gz_mpi_write_viewer_grp_item                         &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module gz_MPI_viewer_mesh_IO
!
      use m_precision
      use m_constants
!
      use t_viewer_mesh
      use t_calypso_mpi_IO_param
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use MPI_ascii_data_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_position                           &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = kint) :: i
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
!      call gz_mpi_write_num_of_data(IO_param, nnod)
!
      ilen_line = len_int8_and_vector_textline(numdir)
      ilen_gz = int(real(nnod*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nnod .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      int8_and_vector_textline                                    &
     &         (id_global(1), numdir, xx(1,1)),                         &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .gt. 0) then
        xx_tmp(1:numdir) = xx(1,1:numdir)
        call gzip_defleat_begin(ilen_line,                              &
     &     int8_and_vector_textline(id_global(1), numdir, xx_tmp),      &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nnod - 1
          xx_tmp(1:numdir) = xx(i,1:numdir)
          call gzip_defleat_cont(ilen_line,                             &
     &     int8_and_vector_textline(id_global(i), numdir, xx_tmp),      &
     &        ilen_gz, ilen_gzipped)
        end do
        xx_tmp(1:numdir) = xx(nnod,1:numdir)
        call gzip_defleat_last(ilen_line,                               &
     &     int8_and_vector_textline                                     &
     &          (id_global(nnod), numdir, xx_tmp),                      &
     &      ilen_gz, ilen_gzipped)
      end if
!
!      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_viewer_position
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_element_type                       &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_gz = int(real(num*len_6digit_txt) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(num .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(num .le. ncolumn) then
        call gzip_defleat_once(len_multi_6digit_line(num),              &
     &      mul_6digit_int_line(num, int_dat(1)),                       &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(num .gt. 0) then
        call gzip_defleat_begin(len_multi_6digit_line(ncolumn),         &
     &      mul_6digit_int_line(ncolumn, int_dat(1)),                   &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 1, (num-1)/ncolumn - 1
          call gzip_defleat_cont(len_multi_6digit_line(ncolumn),        &
     &        mul_6digit_int_line(ncolumn, int_dat(ncolumn*i+1)),       &
     &        ilen_gz, ilen_gzipped)
        end do
        nrest = mod((num-1),ncolumn) + 1
        call gzip_defleat_last(len_multi_6digit_line(nrest),            &
     &      mul_6digit_int_line(nrest, int_dat(num-nrest+1)),           &
     &      ilen_gz, ilen_gzipped)
      end if
!
!      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_viewer_element_type
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_connect                            &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind = kint) :: i
      integer(kind = kint) :: ie_tmp(nnod_4_ele)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_line = len_int8_and_mul_int_textline(nnod_4_ele)
      ilen_gz = int(real(nele*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nele .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nele .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      int8_and_mul_int_textline                                   &
     &         (id_global(1), nnod_4_ele, ie(1,1)),                     &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nele .gt. 0) then
        ie_tmp(1:nnod_4_ele) = ie(1,1:nnod_4_ele)
        call gzip_defleat_begin(ilen_line,                              &
     &     int8_and_mul_int_textline(id_global(1), nnod_4_ele, ie_tmp), &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nele - 1
          ie_tmp(1:nnod_4_ele) = ie(i,1:nnod_4_ele)
          call gzip_defleat_cont(ilen_line,                             &
     &     int8_and_mul_int_textline(id_global(i), nnod_4_ele, ie_tmp), &
     &        ilen_gz, ilen_gzipped)
        end do
        ie_tmp(1:nnod_4_ele) = ie(nele,1:nnod_4_ele)
        call gzip_defleat_last(ilen_line,                               &
     &     int8_and_mul_int_textline                                    &
     &          (id_global(nele), nnod_4_ele, ie_tmp),                  &
     &      ilen_gz, ilen_gzipped)
      end if
!
!      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_viewer_connect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_domain_grp_data(IO_param, view_grp)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(viewer_group_data), intent(in) :: view_grp
!
      integer(kind = kint) :: total_count
!
!
      call MPI_allREDUCE(view_grp%num_item, total_count, ione,          &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(total_count))
      call gz_mpi_write_stack_over_domain                               &
     &   (IO_param, view_grp%num_item)
      call gz_mpi_write_viewer_grp_item(IO_param, ieight,               &
     &    view_grp%num_item, view_grp%item_sf)
!
      end subroutine gz_mpi_write_domain_grp_data
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_grp_data                           &
     &         (IO_param, num_grp, grp_name, view_grp)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num_grp
      character(len=kchara), intent(in) :: grp_name(num_grp)
      type(viewer_group_data), intent(in) :: view_grp
!
      integer(kind = kint) :: i, ist, num, ntot, ip
      integer(kind = kint) :: num_global(nprocs)
!
!
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(num_grp))
!
      ntot = 0
      do i = 1, num_grp
        num = view_grp%istack_sf(i) - view_grp%istack_sf(i-1)
        call MPI_Allgather(num, ione, CALYPSO_INTEGER, num_global,      &
     &      ione, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
        do ip = 2, nprocs
          num_global(ip) = num_global(ip-1) + num_global(ip)
        end do
        num = ntot + num_global(my_rank+1)
        ntot = ntot + num_global(nprocs)
        call gz_mpi_write_num_of_data(IO_param, num)
      end do
!
      do i = 1, num_grp
        call gz_mpi_write_charahead(IO_param,                           &
     &      len_one_word_textline(grp_name(i)),                         &
     &      one_word_textline(grp_name(i)))
!
        ist = view_grp%istack_sf(i-1) + 1
        num = view_grp%istack_sf(i) - view_grp%istack_sf(i-1)
        call gz_mpi_write_viewer_grp_item                               &
     &     (IO_param, ieight, num, view_grp%item_sf(ist))
      end do
!
      end subroutine gz_mpi_write_viewer_grp_data
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_grp_item                           &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_gz = int(real(num*len_int_txt) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(num .le. 0) then
!        call gzip_defleat_once(ione, char(10),                         &
!     &      ilen_gz, ilen_gzipped, gzip_buf(1))
         ilen_gzipped = 0
      else if(num .le. ncolumn) then
        call gzip_defleat_once(len_multi_int_textline(num),             &
     &      multi_int_textline(num, int_dat(1)),                        &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(num .gt. 0) then
        call gzip_defleat_begin(len_multi_int_textline(ncolumn),        &
     &      multi_int_textline(ncolumn, int_dat(1)),                    &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 1, (num-1)/ncolumn - 1
          call gzip_defleat_cont(len_multi_int_textline(ncolumn),       &
     &        multi_int_textline(ncolumn, int_dat(ncolumn*i+1)),        &
     &        ilen_gz, ilen_gzipped)
        end do
        nrest = mod((num-1),ncolumn) + 1
        call gzip_defleat_last(len_multi_int_textline(nrest),           &
     &      multi_int_textline(nrest, int_dat(num-nrest+1)),            &
     &      ilen_gz, ilen_gzipped)
      end if
!
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_viewer_grp_item
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_viewer_mesh_IO
