!>@file  spherical_model_IO_b.f90
!!       module spherical_model_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine read_rank_4_sph_b
!!      subroutine read_gl_resolution_sph_b
!!      subroutine read_gl_nodes_sph_b
!!
!!      subroutine write_rank_4_sph_b
!!      subroutine write_gl_resolution_sph_b
!!      subroutine write_gl_nodes_sph_b
!!@endverbatim
!
      module spherical_model_IO_b
!
      use m_precision
!
      use m_node_id_spherical_IO
      use binary_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rank_4_sph_b
!
!
      call read_mul_integer_b(ndir_sph_IO, sph_IO1%sph_rank)
!
      end subroutine read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_resolution_sph_b
!
!
      call read_mul_integer_b(ndir_sph_IO, sph_IO1%nidx_gl_sph)
      call read_one_integer_b(sph_IO1%ltr_gl)
!
      end subroutine read_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_nodes_sph_b
!
      integer(kind = kint) :: nvect
!
!
      call read_one_integer_b(nnod_sph_IO)
      call allocate_nod_id_sph_IO
!
      call read_mul_int8_b(nnod_sph_IO, inod_gl_sph_IO)
      nvect = nnod_sph_IO * ndir_sph_IO
      call read_mul_integer_b(nvect, idx_gl_sph_IO)
!
      end subroutine read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine write_rank_4_sph_b
!
!
      call write_mul_integer_b(ndir_sph_IO, sph_IO1%sph_rank)
!
      end subroutine write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_resolution_sph_b
!
!
      call write_mul_integer_b(ndir_sph_IO, sph_IO1%nidx_gl_sph)
      call write_one_integer_b(sph_IO1%ltr_gl)
!
      end subroutine write_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_nodes_sph_b
!
      integer(kind = kint) ::  nvect
!
!
      call write_one_integer_b(nnod_sph_IO)
      call write_mul_int8_b(nnod_sph_IO, inod_gl_sph_IO)
      nvect = nnod_sph_IO * ndir_sph_IO
      call write_mul_integer_b(nvect, idx_gl_sph_IO)
!
      call deallocate_nod_id_sph_IO
!
      end subroutine write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module spherical_model_IO_b
