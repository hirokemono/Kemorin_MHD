!>@file  MPI_spherical_model_IO_b.f90
!!       module MPI_spherical_model_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine mpi_read_rank_4_sph_b                                &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_read_gl_reso_sph_b                               &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_read_gl_nodes_sph_b                              &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!
!!      subroutine mpi_write_rank_4_sph_b                               &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_write_gl_reso_sph_b(id_file, ioff_gl)
!!      subroutine mpi_write_gl_nodes_sph_b                             &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!@endverbatim
!
      module MPI_spherical_model_IO_b
!
      use m_precision
!
      use m_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      implicit none
!
      type(calypso_MPI_IO_params), private :: IO_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_rank_4_sph_b                                  &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_fixed_num(ndir_sph_IO, IO_param)
!
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ndir_sph_IO, sph_IO1%sph_rank, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      end subroutine mpi_read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gl_reso_sph_b                                 &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
!
      call mpi_read_mul_inthead_b                                       &
     &   (id_file, ioff_gl, ndir_sph_IO, sph_IO1%nidx_gl_sph)
      call mpi_read_one_inthead_b(id_file, ioff_gl, ltr_gl_IO)
!
      end subroutine mpi_read_gl_reso_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gl_nodes_sph_b                                &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
      integer(kind = kint) :: nvect
!
!
      call mpi_read_one_integer_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nnod_sph_IO)
!
      call allocate_nod_id_sph_IO
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(nnod_sph_IO, IO_param)
      call mpi_read_int8_vector_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nnod_sph_IO, inod_gl_sph_IO, IO_param%istack_merged)
!
      nvect = nnod_sph_IO * ndir_sph_IO
      call mul_istack_4_parallell_vect(ndir_sph_IO, IO_param)
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nvect, idx_gl_sph_IO,   &
     &    IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      end subroutine mpi_read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_rank_4_sph_b                                 &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_fixed_num(ndir_sph_IO, IO_param)
!
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    ndir_sph_IO, sph_IO1%sph_rank, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      end subroutine mpi_write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gl_reso_sph_b(id_file, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call mpi_write_mul_inthead_b                                      &
     &   (id_file, ioff_gl, ndir_sph_IO, sph_IO1%nidx_gl_sph)
      call mpi_write_one_inthead_b(id_file, ioff_gl, ltr_gl_IO)
!
      end subroutine mpi_write_gl_reso_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gl_nodes_sph_b                               &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
      integer(kind = kint) ::  nvect
!
!
      call mpi_write_one_integer_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nnod_sph_IO)
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(nnod_sph_IO, IO_param)
      call mpi_write_int8_vector_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nnod_sph_IO, inod_gl_sph_IO, IO_param%istack_merged)
!
      nvect = nnod_sph_IO * ndir_sph_IO
      call mul_istack_4_parallell_vect(ndir_sph_IO, IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    nvect, idx_gl_sph_IO, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      call deallocate_nod_id_sph_IO
!
      end subroutine mpi_write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module MPI_spherical_model_IO_b
