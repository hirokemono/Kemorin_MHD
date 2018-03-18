!>@file   MPI_viewer_mesh_IO.f90
!!@brief  module MPI_viewer_mesh_IO
!!
!!@author H.Matsui
!!@date      Programmed in Mar., 2018
!
!>@brief  Viewer mesh file IO for ascii format
!!
!!@verbatim
!!      subroutine mpi_write_viewer_position                            &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!
!!      subroutine mpi_write_viewer_element_type                        &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine mpi_write_viewer_connect                             &
!!     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!!
!!      subroutine mpi_write_domain_grp_data(IO_param, view_grp)
!!      subroutine mpi_write_viewer_grp_data                            &
!!     &         (IO_param, num_grp, grp_name, view_grp)
!!        type(viewer_group_data), intent(inout) :: view_grp
!!      subroutine mpi_write_viewer_grp_item                            &
!!     &         (IO_param, ncolumn, num, int_dat)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module MPI_viewer_mesh_IO
!
      use m_precision
      use m_constants
!
      use t_viewer_mesh
      use t_calypso_mpi_IO_param
      use MPI_domain_data_IO
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
      subroutine mpi_write_viewer_position                              &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = kint) :: i, led, ilength
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = len_int8_and_vector_textline(numdir)
      led = nnod * len_int8_and_vector_textline(numdir)
      call set_istack_4_parallell_data(led, IO_param)
!      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nnod .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else
        do i = 1, nnod
          xx_tmp(1:numdir) = xx(i,1:numdir)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        int8_and_vector_textline(id_global(i), numdir, xx_tmp))
        end do
      end if
      call calypso_mpi_barrier
!
      end subroutine mpi_write_viewer_position
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_viewer_element_type                          &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, loop, led
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(num .le. 0) then
        led = ione
      else if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_6digit_line(ncolumn) * loop                     &
     &       + len_multi_6digit_line(nrest)
      end if
!
!      call mpi_write_stack_over_domain(IO_param, led)
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(num .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else if(num .gt. 0) then
        do i = 0, (num-1)/ncolumn - 1
          call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,  &
     &        len_multi_6digit_line(ncolumn),                           &
     &        mul_6digit_int_line(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_multi_6digit_line(nrest),                               &
     &      mul_6digit_int_line(nrest, int_dat(num-nrest+1)))
      end if
!
      end subroutine mpi_write_viewer_element_type
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_viewer_connect                               &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind = kint) :: i, led, ilength
      integer(kind = kint) :: ie_tmp(nnod_4_ele)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = len_int8_and_mul_int_textline(nnod_4_ele)
!
      if(nele .le. 0) then
        led = ione
      else
        led = ilength * nele
      end if
!
!      call mpi_write_stack_over_domain(IO_param, led)
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nele .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else
        do i = 1, nele
          ie_tmp(1:nnod_4_ele) = ie(i,1:nnod_4_ele)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        int8_and_mul_int_textline(id_global(i),                   &
     &                                  nnod_4_ele, ie_tmp))
        end do
      end if
!
      end subroutine mpi_write_viewer_connect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_domain_grp_data(IO_param, view_grp)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(viewer_group_data), intent(in) :: view_grp
!
      integer(kind = kint) :: total_count
!
!
      call MPI_allREDUCE(view_grp%num_item, total_count, ione,          &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(total_count))
      call mpi_write_stack_over_domain                                  &
     &   (IO_param, view_grp%num_item)
      call mpi_write_viewer_grp_item(IO_param, ieight,                  &
     &    view_grp%num_item, view_grp%item_sf)
!
      end subroutine mpi_write_domain_grp_data
!
!------------------------------------------------------------------
!
      subroutine mpi_write_viewer_grp_data                              &
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
      call mpi_write_charahead(IO_param, len_int_txt,                   &
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
        call mpi_write_num_of_data(IO_param, num)
      end do
!
      do i = 1, num_grp
        call mpi_write_charahead(IO_param,                              &
     &      len_one_word_textline(grp_name(i)),                         &
     &      one_word_textline(grp_name(i)))
!
        ist = view_grp%istack_sf(i-1) + 1
        num = view_grp%istack_sf(i) - view_grp%istack_sf(i-1)
        call mpi_write_viewer_grp_item                                  &
     &     (IO_param, ieight, num, view_grp%item_sf(ist))
      end do
!
      end subroutine mpi_write_viewer_grp_data
!
!------------------------------------------------------------------
!
      subroutine mpi_write_viewer_grp_item                              &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, loop, led
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(num .le. 0) then
!        led = ione
         led = izero
      else if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_int_textline(ncolumn) * loop                    &
     &       + len_multi_int_textline(nrest)
      end if
!
!      call mpi_write_stack_over_domain(IO_param, led)
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(num .gt. 0) then
        do i = 0, (num-1)/ncolumn - 1
          call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,  &
     &        len_multi_int_textline(ncolumn),                          &
     &        multi_int_textline(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_multi_int_textline(nrest),                              &
     &      multi_int_textline(nrest, int_dat(num-nrest+1)))
!      else
!        call calypso_mpi_seek_write_chara                              &
!     &     (IO_param%id_file, ioffset, ione, char(10))
      end if
!
      end subroutine mpi_write_viewer_grp_item
!
! -----------------------------------------------------------------------
!
      end module MPI_viewer_mesh_IO
