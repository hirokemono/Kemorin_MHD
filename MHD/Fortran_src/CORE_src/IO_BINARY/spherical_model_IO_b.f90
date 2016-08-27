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
      use field_data_IO_b
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
      call read_fld_mul_inthead_b(ndir_sph_IO, sph_rank_IO)
!
      end subroutine read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_resolution_sph_b
!
!
      call read_fld_mul_inthead_b(ndir_sph_IO, nidx_gl_sph_IO)
      call read_fld_inthead_b(ltr_gl_IO)
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
      call read_fld_inthead_b(nnod_sph_IO)
      call allocate_nod_id_sph_IO
!
      call read_fld_mul_i8head_b(nnod_sph_IO, inod_gl_sph_IO)
      nvect = nnod_sph_IO * ndir_sph_IO
      call read_fld_mul_inthead_b(nvect, idx_gl_sph_IO)
!
      end subroutine read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine write_rank_4_sph_b
!
!
      call write_fld_mul_inthead_b(ndir_sph_IO, sph_rank_IO)
!
      end subroutine write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_resolution_sph_b
!
!
      call write_fld_mul_inthead_b(ndir_sph_IO, nidx_gl_sph_IO)
      call write_fld_inthead_b(ltr_gl_IO)
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
      call write_fld_inthead_b(nnod_sph_IO)
      call write_fld_mul_i8head_b(nnod_sph_IO, inod_gl_sph_IO)
      nvect = nnod_sph_IO * ndir_sph_IO
      call write_fld_mul_inthead_b(nvect, idx_gl_sph_IO)
!
      call deallocate_nod_id_sph_IO
!
      end subroutine write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module spherical_model_IO_b
