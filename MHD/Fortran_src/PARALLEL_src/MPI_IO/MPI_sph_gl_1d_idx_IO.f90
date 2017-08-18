!>@file   MPI_sph_gl_1d_idx_IO.f90
!!@brief  module MPI_sph_gl_1d_idx_IO
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine mpi_read_rtp_gl_1d_table(IO_param, sph_IO)
!!      subroutine mpi_read_rj_gl_1d_table(IO_param, sph_IO)
!!
!!      subroutine mpi_write_rtp_gl_1d_table(IO_param, sph_IO)
!!      subroutine mpi_write_rj_gl_1d_table(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!@endverbatim
!
      module MPI_sph_gl_1d_idx_IO
!
      use m_precision
      use m_constants
!
      use t_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use m_sph_modes_grid_labels
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use MPI_vectors_IO
!
      implicit none
!
      integer(kind = kint_gl), allocatable :: idx_gl_tmp(:)
      private :: idx_gl_tmp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------!
      subroutine mpi_read_rtp_gl_1d_table(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      sph_IO%numdir_sph = 3
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 1
      sph_IO%ncomp_table_1d(3) = 2
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      call mpi_skip_read(IO_param, len(hd_rgrid()))
      call mpi_read_num_of_data(IO_param, sph_IO%ist_sph(1))
      call mpi_read_num_of_data(IO_param, sph_IO%ied_sph(1))
      call mpi_read_num_of_data(IO_param, sph_IO%nidx_sph(1))
!
      call alloc_idx_sph_1d1_IO(sph_IO)
!
      allocate(idx_gl_tmp(sph_IO%nidx_sph(1)))
      call mpi_read_node_position(IO_param,                             &
     &   sph_IO%nidx_sph(1), sph_IO%ncomp_table_1d(1),                  &
     &   idx_gl_tmp, sph_IO%r_gl_1)
      sph_IO%idx_gl_1(1:sph_IO%nidx_sph(1))                             &
     &       = int(idx_gl_tmp(1:sph_IO%nidx_sph(1)))
      deallocate(idx_gl_tmp)
!
!
      call mpi_skip_read(IO_param, len(hd_tgrid()))
      call mpi_read_num_of_data(IO_param, sph_IO%ist_sph(2))
      call mpi_read_num_of_data(IO_param, sph_IO%ied_sph(2))
      call mpi_read_num_of_data(IO_param, sph_IO%nidx_sph(2))
!
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      call mpi_read_1d_gl_address(IO_param,                             &
     &   sph_IO%nidx_sph(2), sph_IO%ncomp_table_1d(2), sph_IO%idx_gl_2)
!
!
      call mpi_skip_read(IO_param, len(hd_pgrid()))
      call mpi_read_num_of_data(IO_param, sph_IO%ist_sph(3))
      call mpi_read_num_of_data(IO_param, sph_IO%ied_sph(3))
      call mpi_read_num_of_data(IO_param, sph_IO%nidx_sph(3))
!
      call alloc_idx_sph_1d3_IO(sph_IO)
!
      call mpi_read_1d_gl_address(IO_param,                             &
     &   sph_IO%nidx_sph(3), sph_IO%ncomp_table_1d(3), sph_IO%idx_gl_3)
!
      end subroutine mpi_read_rtp_gl_1d_table
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_rj_gl_1d_table(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      sph_IO%numdir_sph = 2
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 3
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      call mpi_skip_read(IO_param, len(hd_rgrid()))
      call mpi_read_num_of_data(IO_param, sph_IO%ist_sph(1))
      call mpi_read_num_of_data(IO_param, sph_IO%ied_sph(1))
      call mpi_read_num_of_data(IO_param, sph_IO%nidx_sph(1))
!
      call alloc_idx_sph_1d1_IO(sph_IO)
!
      allocate(idx_gl_tmp(sph_IO%nidx_sph(1)))
      call mpi_read_node_position(IO_param,                             &
     &   sph_IO%nidx_sph(1), sph_IO%ncomp_table_1d(1),                  &
     &   idx_gl_tmp, sph_IO%r_gl_1)
!
      sph_IO%idx_gl_1(1:sph_IO%nidx_sph(1))                             &
     &       = int(idx_gl_tmp(1:sph_IO%nidx_sph(1)))
      deallocate(idx_gl_tmp)
!
!
      call mpi_skip_read(IO_param, len(hd_jmode()))
      call mpi_read_num_of_data(IO_param, sph_IO%ist_sph(2))
      call mpi_read_num_of_data(IO_param, sph_IO%ied_sph(2))
      call mpi_read_num_of_data(IO_param, sph_IO%nidx_sph(2))
!
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      call mpi_read_1d_gl_address(IO_param,                             &
     &   sph_IO%nidx_sph(2), sph_IO%ncomp_table_1d(2), sph_IO%idx_gl_2)
!
      end subroutine mpi_read_rj_gl_1d_table
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_rtp_gl_1d_table(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_rgrid()), hd_rgrid())
!
      call mpi_write_num_of_data(IO_param, sph_IO%ist_sph(1))
      call mpi_write_num_of_data(IO_param, sph_IO%ied_sph(1))
!
      allocate(idx_gl_tmp(sph_IO%nidx_sph(1)))
      idx_gl_tmp(1:sph_IO%nidx_sph(1))                                  &
     &       =  sph_IO%idx_gl_1(1:sph_IO%nidx_sph(1))
      call mpi_write_node_position(IO_param,                            &
     &   sph_IO%nidx_sph(1), ione, idx_gl_tmp, sph_IO%r_gl_1)
      deallocate(idx_gl_tmp)
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_tgrid()), hd_tgrid())
!
      call mpi_write_num_of_data(IO_param, sph_IO%ist_sph(2))
      call mpi_write_num_of_data(IO_param, sph_IO%ied_sph(2))
!
      call mpi_write_1d_gl_address(IO_param,                            &
     &    sph_IO%nidx_sph(2), sph_IO%ncomp_table_1d(2),                 &
     &    sph_IO%idx_gl_2)
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_pgrid()), hd_pgrid())
!
      call mpi_write_num_of_data(IO_param, sph_IO%ist_sph(3))
      call mpi_write_num_of_data(IO_param, sph_IO%ied_sph(3))
!
      call mpi_write_1d_gl_address(IO_param,                            &
     &    sph_IO%nidx_sph(3), sph_IO%ncomp_table_1d(3),                 &
     &    sph_IO%idx_gl_3)
!
!
      call dealloc_num_idx_sph_IO(sph_IO)
      call dealloc_idx_sph_1d1_IO(sph_IO)
      call dealloc_idx_sph_1d2_IO(sph_IO)
      call dealloc_idx_sph_1d3_IO(sph_IO)
!
      end subroutine mpi_write_rtp_gl_1d_table
!
! ----------------------------------------------------------------------
!
      subroutine mpi_write_rj_gl_1d_table(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_rgrid()), hd_rgrid())
!
      call mpi_write_num_of_data(IO_param, sph_IO%ist_sph(1))
      call mpi_write_num_of_data(IO_param, sph_IO%ied_sph(1))
!
      allocate(idx_gl_tmp(sph_IO%nidx_sph(1)))
      idx_gl_tmp(1:sph_IO%nidx_sph(1))                                  &
     &       =  sph_IO%idx_gl_1(1:sph_IO%nidx_sph(1))
      call mpi_write_node_position(IO_param,                            &
     &    sph_IO%nidx_sph(1), sph_IO%ncomp_table_1d(1),                 &
     &    idx_gl_tmp, sph_IO%r_gl_1)
      deallocate(idx_gl_tmp)
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_jmode()), hd_jmode())
!
      call mpi_write_num_of_data(IO_param, sph_IO%ist_sph(2))
      call mpi_write_num_of_data(IO_param, sph_IO%ied_sph(2))
!
      call mpi_write_1d_gl_address(IO_param,                            &
     &    sph_IO%nidx_sph(2), sph_IO%ncomp_table_1d(2),                 &
     &    sph_IO%idx_gl_2)
!
      call dealloc_num_idx_sph_IO(sph_IO)
      call dealloc_idx_sph_1d1_IO(sph_IO)
      call dealloc_idx_sph_1d2_IO(sph_IO)
!
      end subroutine mpi_write_rj_gl_1d_table
!
! -----------------------------------------------------------------------
!
      end module MPI_sph_gl_1d_idx_IO
