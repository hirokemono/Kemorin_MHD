!>@file   MPI_sph_gl_1d_idx_IO_b.f90
!!@brief  module MPI_sph_gl_1d_idx_IO_b
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine mpi_read_rtp_gl_1d_table_b                           &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_read_rj_gl_1d_table_b                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!
!!      subroutine mpi_write_rtp_gl_1d_table_b                          &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_write_rj_gl_1d_table_b                           &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!@endverbatim
!
      module MPI_sph_gl_1d_idx_IO_b
!
      use m_precision
      use m_constants
!
      use m_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use MPI_binary_data_IO
!
      implicit none
!
      type(calypso_MPI_IO_params), private :: IO_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------!
      subroutine mpi_read_rtp_gl_1d_table_b                             &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
      integer(kind = kint) :: nvect
!
!
      ndir_sph_IO = 3
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 1
      ncomp_itbl_1d_IO(3) = 2
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_fixed_num(ndir_sph_IO, IO_param)
!
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ndir_sph_IO, nidx_sph_IO, IO_param%istack_merged)
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ndir_sph_IO, ist_sph_IO, IO_param%istack_merged)
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ndir_sph_IO, ied_sph_IO, IO_param%istack_merged)
!
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
      call set_istack_4_parallell_data(nidx_sph_IO(1), IO_param)
!
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nidx_sph_IO(1), idx_gl_1_IO, IO_param%istack_merged)
      call mpi_read_1d_vector_b                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nidx_sph_IO(1), r_gl_1_IO, IO_param%istack_merged)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call set_istack_4_parallell_data(nvect, IO_param)
!
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nvect, idx_gl_2_IO, IO_param%istack_merged)
!
      nvect = nidx_sph_IO(3) * ncomp_itbl_1d_IO(3)
      call set_istack_4_parallell_data(nvect, IO_param)
!
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nvect, idx_gl_3_IO, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      end subroutine mpi_read_rtp_gl_1d_table_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_rj_gl_1d_table_b                              &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
      integer(kind = kint) :: nvect
!
!
      ndir_sph_IO = 2
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 3
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_fixed_num(ndir_sph_IO, IO_param)
!
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ndir_sph_IO, nidx_sph_IO, IO_param%istack_merged)
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ndir_sph_IO, ist_sph_IO, IO_param%istack_merged)
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ndir_sph_IO, ied_sph_IO, IO_param%istack_merged)
!
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
      call set_istack_4_parallell_data(nidx_sph_IO(1), IO_param)
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nidx_sph_IO(1), idx_gl_1_IO, IO_param%istack_merged)
      call mpi_read_1d_vector_b                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nidx_sph_IO(1), r_gl_1_IO, IO_param%istack_merged)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call set_istack_4_parallell_data(nvect, IO_param)
!
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nvect, idx_gl_2_IO, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      end subroutine mpi_read_rj_gl_1d_table_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_rtp_gl_1d_table_b                            &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
      integer(kind = kint) :: nvect
!
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_fixed_num(ndir_sph_IO, IO_param)
!
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    ndir_sph_IO, nidx_sph_IO, IO_param%istack_merged)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    ndir_sph_IO, ist_sph_IO, IO_param%istack_merged)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    ndir_sph_IO, ied_sph_IO, IO_param%istack_merged)
!
      call set_istack_4_parallell_data(nidx_sph_IO(1), IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    nidx_sph_IO(1), idx_gl_1_IO, IO_param%istack_merged)
      call mpi_write_1d_vector_b(id_file, nprocs_in, id_rank, ioff_gl,  &
     &    nidx_sph_IO(1), r_gl_1_IO, IO_param%istack_merged)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call set_istack_4_parallell_data(nvect, IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    nvect, idx_gl_2_IO, IO_param%istack_merged)
!
      nvect = nidx_sph_IO(3) * ncomp_itbl_1d_IO(3)
      call set_istack_4_parallell_data(nvect, IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    nvect, idx_gl_3_IO, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine mpi_write_rtp_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      subroutine mpi_write_rj_gl_1d_table_b                             &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
      integer(kind = kint) :: nvect
!
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_fixed_num(ndir_sph_IO, IO_param)
!
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    ndir_sph_IO, nidx_sph_IO, IO_param%istack_merged)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    ndir_sph_IO, ist_sph_IO, IO_param%istack_merged)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    ndir_sph_IO, ied_sph_IO, IO_param%istack_merged)
!
      call set_istack_4_parallell_data(nidx_sph_IO(1), IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    nidx_sph_IO(1), idx_gl_1_IO, IO_param%istack_merged)
      call mpi_write_1d_vector_b(id_file, nprocs_in, id_rank, ioff_gl,  &
     &    nidx_sph_IO(1), r_gl_1_IO, IO_param%istack_merged)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call set_istack_4_parallell_data(nvect, IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    nvect, idx_gl_2_IO, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine mpi_write_rj_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      end module MPI_sph_gl_1d_idx_IO_b
