!>@file   MPI_write_single_mesh_data.f90
!!@brief  module MPI_write_single_mesh_data
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine mpi_write_merged_node_position                       &
!!     &         (IO_param, nnod, numdir, id_global, xx)
!!
!!      subroutine mpi_write_merged_element_type                        &
!!     &         (IO_param, ncolumn, num, int_dat)
!!      subroutine mpi_write_merged_ele_connect(IO_param,               &
!!     &          nele, nnod_4_ele, id_global, ie, istack_interele,     &
!!     &          istack_internod, nnod_local, inod_local, irank_home)
!!
!!      subroutine mpi_write_grp_item                                   &
!!     &         (IO_param, ncolumn, num, int_dat, nshift8)
!!      subroutine mpi_write_surf_grp_item(IO_param, ncolumn,           &
!!     &          ntot, ist, num, int_dat, nshift8_ele)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module MPI_write_single_mesh_data
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_phys_constants
!
      use m_calypso_mpi_IO
      use MPI_ascii_data_IO
      use data_IO_to_textline
!
      implicit none
!
      private :: mpi_write_merged_group_item
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine mpi_write_merged_node_position                         &
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
      end subroutine mpi_write_merged_node_position
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_merged_element_type                          &
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
      led = izero
      if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_6digit_line(ncolumn) * loop                     &
     &       + len_multi_6digit_line(nrest)
      end if
!
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
     &        len_multi_6digit_line(ncolumn),                           &
     &        mul_6digit_int_line(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_multi_6digit_line(nrest),                               &
     &      mul_6digit_int_line(nrest, int_dat(num-nrest+1)))
      end if
!
      end subroutine mpi_write_merged_element_type
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_merged_ele_connect(IO_param,                 &
     &          nele, nnod_4_ele, id_global, ie, istack_interele,       &
     &          istack_internod, nnod_local, inod_local, irank_home)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind=kint_gl), intent(in) :: istack_internod(0:nprocs)
      integer(kind=kint_gl), intent(in) :: istack_interele(0:nprocs)
      integer(kind=kint), intent(in) :: nnod_local
      integer(kind=kint), intent(in) :: inod_local(nnod_local)
      integer(kind=kint), intent(in) :: irank_home(nnod_local)
!
      integer(kind = kint) :: k1, inod, irank
      integer(kind = kint) :: i, led, ilength
      integer(kind = kint_gl) :: ie_tmp(0:nnod_4_ele)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = len_multi_int_textline(nnod_4_ele+1)
!
      led = izero
      if(nele .gt. 0) then
        led = ilength * nele
      end if
!
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nele .gt. 0) then
        do i = 1, nele
          ie_tmp(0) = id_global(i) + istack_interele(my_rank)
!
          do k1 = 1, nnod_4_ele
            inod = ie(i,k1)
            irank = irank_home(inod)
            ie_tmp(k1) = inod_local(inod) + istack_internod(irank)
          end do
!
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        multi_int8_textline((nnod_4_ele+1), ie_tmp))
        end do
      end if
!
      end subroutine mpi_write_merged_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_grp_item                                     &
     &         (IO_param, ncolumn, num, int_dat, nshift8)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: nshift8
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint_gl) :: int_tmp(num)
!
!
!$omp parallel workshare
       int_tmp(1:num) = int_dat(1:num) + nshift8
!$omp end parallel workshare
      call mpi_write_merged_group_item(IO_param, ncolumn, num, int_tmp)
!
      end subroutine mpi_write_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_surf_grp_item(IO_param, ncolumn,             &
     &          ntot, ist, num, int_dat, nshift8_ele)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: nshift8_ele
      integer(kind=kint), intent(in) :: ntot, ist, num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(2,ntot)
!
      integer(kind = kint_gl) :: int_tmp(num)
!
!
!$omp parallel workshare
       int_tmp(1:num) = int_dat(1,ist+1:ist+num) + nshift8_ele
!$omp end parallel workshare
      call mpi_write_merged_group_item(IO_param, ncolumn, num, int_tmp)
!
!
!$omp parallel workshare
       int_tmp(1:num) = int_dat(2,ist+1:ist+num)
!$omp end parallel workshare
      call mpi_write_merged_group_item(IO_param, ncolumn, num, int_tmp)
!
      end subroutine mpi_write_surf_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_merged_group_item                            &
     &         (IO_param, ncolumn, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint_gl), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, loop, led
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      led = izero
      if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_int_textline(ncolumn) * loop                    &
     &       + len_multi_int_textline(nrest)
      end if
!
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl + IO_param%istack_merged(my_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(nprocs)
!
      if(num .gt. 0) then
        do i = 0, (num-1)/ncolumn - 1
          call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,  &
     &        len_multi_int_textline(ncolumn),                          &
     &        multi_int8_textline(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_multi_int_textline(nrest),                              &
     &      multi_int8_textline(nrest, int_dat(num-nrest+1)))
      end if
!
      end subroutine mpi_write_merged_group_item
!
! -----------------------------------------------------------------------
!
      end module MPI_write_single_mesh_data
