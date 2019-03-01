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
      use t_viewer_group
      use t_calypso_mpi_IO_param
      use t_buffer_4_gzip
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use MPI_ascii_data_IO
!
      implicit none
!
      type(buffer_4_gzip) :: zbuf
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
      use gz_MPI_position_IO
      use zlib_convert_ascii_vector
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: nnod64
!
!
!      call gz_mpi_write_num_of_data(IO_param, nnod)
!
      nnod64 = nnod
      call defleate_node_position(nnod64, numdir, id_global, xx, zbuf)
!
      call gz_mpi_write_stack_over_domain(IO_param, zbuf%ilen_gzipped)
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_long_write_gz(IO_param%id_file, ioffset,  &
     &      zbuf%ilen_gzipped, zbuf%gzip_buf(1))
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_mpi_write_viewer_position
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
      integer(kind = kint_gl) :: num_item64
!
!
      call MPI_allREDUCE(view_grp%num_item, total_count, ione,          &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(total_count))
!
      num_item64 = view_grp%num_item
      call gz_mpi_write_stack_over_domain                               &
     &   (IO_param, num_item64)
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
      use gz_MPI_domain_data_IO
      use zlib_cvt_ascii_comm_tbl
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: num64
!
!
      num64 = num
      call defleate_comm_table(ncolumn, num64, int_dat, zbuf)
!
      call gz_mpi_write_stack_over_domain(IO_param, zbuf%ilen_gzipped)
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_long_write_gz(IO_param%id_file, ioffset,  &
     &      zbuf%ilen_gzipped, zbuf%gzip_buf(1))
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_mpi_write_viewer_grp_item
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_viewer_mesh_IO
