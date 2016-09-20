!>@file  MPI_spherical_model_IO.f90
!!       module MPI_spherical_model_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine mpi_read_rank_4_sph(IO_param, sph_IO)
!!      subroutine mpi_read_gl_reso_sph(IO_param, sph_IO)
!!      subroutine mpi_read_gl_nodes_sph(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine mpi_write_rank_4_sph(IO_param, sph_IO)
!!      subroutine mpi_write_gl_reso_sph(IO_param, sph_IO)
!!      subroutine mpi_write_gl_nodes_sph(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!@endverbatim
!
      module MPI_spherical_model_IO
!
      use m_precision
!
      use t_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use m_sph_modes_grid_labels
      use MPI_ascii_data_IO
      use MPI_domain_data_IO
      use data_IO_to_textline
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_rank_4_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) ::  num_tmp
!
!
      call mpi_skip_read(IO_param, len(hd_segment()))
      call mpi_read_num_int(IO_param, num_tmp)
      call mpi_read_comm_table(IO_param,                                &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%sph_rank)
!
      end subroutine mpi_read_rank_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gl_reso_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) ::  num_tmp
!
!
      call mpi_skip_read(IO_param, len(hd_trunc()))
      call read_integer_textline                                        &
     &   (mpi_read_charahead(IO_param, len_int_txt), sph_IO%ltr_gl)
!
      call mpi_read_num_int(IO_param, num_tmp)
      call mpi_read_comm_table(IO_param,                                &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
!
      end subroutine mpi_read_gl_reso_sph
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gl_nodes_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_read_num_int(IO_param, sph_IO%numnod_sph)
!
      call alloc_nod_id_sph_IO(sph_IO)
      call mpi_read_ele_connect                                         &
     &   (IO_param, sph_IO%numnod_sph, sph_IO%numdir_sph,               &
     &    sph_IO%inod_gl_sph, sph_IO%idx_gl_sph)
!
      end subroutine mpi_read_gl_nodes_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_rank_4_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_segment()), hd_segment())
      call mpi_write_comm_table(IO_param,                               &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%sph_rank)
!
      end subroutine mpi_write_rank_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gl_reso_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_trunc()), hd_trunc())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(sph_IO%ltr_gl))
      call mpi_write_comm_table(IO_param,                               &
     &    sph_IO%numdir_sph, sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
!
      end subroutine mpi_write_gl_reso_sph
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gl_nodes_sph(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) ::  nvect
!
!
      call mpi_write_ele_connect(IO_param,                              &
     &    sph_IO%numnod_sph, sph_IO%numdir_sph,                         &
     &    sph_IO%inod_gl_sph, sph_IO%idx_gl_sph)
!
      call dealloc_nod_id_sph_IO(sph_IO)
!
      end subroutine mpi_write_gl_nodes_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_ele_connect                                   &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(inout) :: id_global(nele)
      integer(kind=kint), intent(inout) :: ie(nele, nnod_4_ele)
!
      integer(kind = kint) :: ie_tmp(nnod_4_ele)
      integer(kind = kint) :: i
!
      integer(kind = kint) :: ilength, n_item
!
      character(len = ((nnod_4_ele+1)*len_integer_nolf+1)),             &
     &                                allocatable :: textbuf(:)
      character(len = 1) :: tmpchara
!
!
      call mpi_skip_read                                                &
     &   (IO_param, len_multi_int_textline(IO_param%nprocs_in))
!
      IO_param%istack_merged(0) = 0
      do i = 1, IO_param%nprocs_in
        n_item = int(IO_param%istack_merged(i))
        if(n_item .le. 0) then
          ilength = ione
        else if(n_item .gt. 0) then
          ilength = len_int8_and_mul_int_textline(nnod_4_ele) * n_item
        end if
        IO_param%istack_merged(i) = IO_param%istack_merged(i-1)         &
     &                             + ilength
      end do
      ilength = int(IO_param%istack_merged(IO_param%id_rank+1)          &
     &          - IO_param%istack_merged(IO_param%id_rank))
!
      allocate(textbuf(nele))

      if(nele .eq. 0) then
        call mpi_sub_read_characters(IO_param, ilength, tmpchara)
      else
        call mpi_sub_read_characters(IO_param, ilength, textbuf(1))
      end if
!
      do i = 1, nele
        call read_int8_and_mul_int_textline                           &
     &     (textbuf(i) ,id_global(i), nnod_4_ele, ie_tmp)
        ie(i,1:nnod_4_ele) = ie_tmp(1:nnod_4_ele)
      end do
      deallocate(textbuf)
!
      end subroutine mpi_read_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_ele_connect                                  &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind = kint) :: i, lst, led
      integer(kind = kint) :: ie_tmp(nnod_4_ele)
      integer(kind = kint) :: ilen_line, ilength
!
      character(len = ((nnod_4_ele+1)*len_integer_nolf+1)),             &
     &                                allocatable :: textbuf(:)
!
!
      call set_numbers_2_head_node(nele, IO_param)
      call mpi_write_charahead(IO_param,                                &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &    IO_param%istack_merged))
!
      ilen_line = len_int8_and_mul_int_textline(nnod_4_ele)
      allocate(textbuf(nele))
!
      if(nele .le. 0) then
        ilength = ione
      else
        lst = 0
        do i = 1, nele
          lst = led
          led = lst + ilen_line
          ie_tmp(1:nnod_4_ele) = ie(i,1:nnod_4_ele)
          textbuf(i) = int8_and_mul_int_textline                        &
     &                       (id_global(i), nnod_4_ele, ie_tmp)
        end do
        ilength = led
      end if
!
      call set_istack_4_parallell_data(ilength, IO_param)
      call mpi_write_charahead(IO_param,                                &
     &    len_multi_int_textline(IO_param%nprocs_in),                   &
     &    int_stack8_textline(IO_param%nprocs_in,                       &
     &                        IO_param%istack_merged))
!
      if(nele .le. 0) then
        call mpi_write_characters(IO_param, ione, char(10))
      else
        call mpi_write_characters(IO_param, ilength, textbuf(1))
      end if
      deallocate(textbuf)
!
      end subroutine mpi_write_ele_connect
!
! -----------------------------------------------------------------------
!
      end module MPI_spherical_model_IO
