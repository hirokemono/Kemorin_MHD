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
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, sph_IO)
!!      subroutine mpi_read_gl_reso_sph_b                               &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, sph_IO)
!!      subroutine mpi_read_gl_nodes_sph_b                              &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, sph_IO)
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine mpi_write_rank_4_sph_b                               &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, sph_IO)
!!      subroutine mpi_write_gl_reso_sph_b(id_file, ioff_gl, sph_IO)
!!      subroutine mpi_write_gl_nodes_sph_b                             &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, sph_IO)
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!@endverbatim
!
      module MPI_spherical_model_IO_b
!
      use m_precision
!
      use t_node_id_spherical_IO
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
     &         (id_file, nprocs_in, id_rank, ioff_gl, sph_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_fixed_num(sph_IO%numdir_sph, IO_param)
!
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    sph_IO%numdir_sph, sph_IO%sph_rank, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      end subroutine mpi_read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gl_reso_sph_b                                 &
     &         (id_file, nprocs_in, id_rank, ioff_gl, sph_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_read_mul_inthead_b                                       &
     &   (id_file, ioff_gl, sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
      call mpi_read_one_inthead_b(id_file, ioff_gl, sph_IO%ltr_gl)
!
      end subroutine mpi_read_gl_reso_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_gl_nodes_sph_b                                &
     &         (id_file, nprocs_in, id_rank, ioff_gl, sph_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: nvect
!
!
      call mpi_read_one_integer_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, sph_IO%numnod_sph)
!
      call alloc_nod_id_sph_IO(sph_IO)
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(sph_IO%numnod_sph, IO_param)
      call mpi_read_int8_vector_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    sph_IO%numnod_sph, sph_IO%inod_gl_sph,                        &
     &    IO_param%istack_merged)
!
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call mul_istack_4_parallell_vect(sph_IO%numdir_sph, IO_param)
      call mpi_read_int_vector_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nvect,                  &
     &    sph_IO%idx_gl_sph, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      end subroutine mpi_read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_rank_4_sph_b                                 &
     &         (id_file, nprocs_in, id_rank, ioff_gl, sph_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_fixed_num(sph_IO%numdir_sph, IO_param)
!
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    sph_IO%numdir_sph, sph_IO%sph_rank, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      end subroutine mpi_write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gl_reso_sph_b(id_file, ioff_gl, sph_IO)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call mpi_write_mul_inthead_b                                      &
     &   (id_file, ioff_gl, sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
      call mpi_write_one_inthead_b(id_file, ioff_gl, sph_IO%ltr_gl)
!
      end subroutine mpi_write_gl_reso_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_gl_nodes_sph_b                               &
     &         (id_file, nprocs_in, id_rank, ioff_gl, sph_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) ::  nvect
!
!
      call mpi_write_one_integer_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, sph_IO%numnod_sph)
!
      call alloc_istack_merge(id_rank, nprocs_in, IO_param)
      call set_istack_4_parallell_data(sph_IO%numnod_sph, IO_param)
      call mpi_write_int8_vector_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    sph_IO%numnod_sph, sph_IO%inod_gl_sph,                        &
     &    IO_param%istack_merged)
!
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call mul_istack_4_parallell_vect(sph_IO%numdir_sph, IO_param)
      call mpi_write_int_vector_b(id_file, nprocs_in, id_rank, ioff_gl, &
     &    nvect, sph_IO%idx_gl_sph, IO_param%istack_merged)
      call dealloc_istack_merge(IO_param)
!
      call dealloc_nod_id_sph_IO(sph_IO)
!
      end subroutine mpi_write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module MPI_spherical_model_IO_b
