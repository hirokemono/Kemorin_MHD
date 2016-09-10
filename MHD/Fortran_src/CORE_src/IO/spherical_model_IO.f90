!>@file  spherical_model_IO.f90
!!       module spherical_model_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine read_rank_4_sph(id_file)
!!      subroutine read_gl_resolution_sph(id_file)
!!      subroutine read_gl_nodes_sph(id_file)
!!
!!      subroutine write_rank_4_sph(id_file)
!!      subroutine write_gl_resolution_sph(id_file)
!!      subroutine write_gl_nodes_sph(id_file)
!!@endverbatim
!
      module spherical_model_IO
!
      use m_precision
!
      use m_node_id_spherical_IO
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rank_4_sph(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO1%sph_rank(1:sph_IO1%numdir_sph)
!
      end subroutine read_rank_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_resolution_sph(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO1%nidx_gl_sph(1:sph_IO1%numdir_sph)
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO1%ltr_gl
!
      end subroutine read_gl_resolution_sph
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_nodes_sph(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) sph_IO1%numnod_sph
!
      call allocate_nod_id_sph_IO
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*)                                          &
     &        inod_gl_sph_IO(1), idx_gl_sph_IO(1,1:sph_IO1%numdir_sph)
      do i = 2, sph_IO1%numnod_sph
        read(id_file,*)                                                 &
     &        inod_gl_sph_IO(i), idx_gl_sph_IO(i,1:sph_IO1%numdir_sph)
      end do
!
      end subroutine read_gl_nodes_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rank_4_sph(id_file)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: id_file
!
!
      write(id_file,'(a)', advance='NO') hd_segment()
      write(id_file,'(10i16)') sph_IO1%sph_rank(1:sph_IO1%numdir_sph)
!
      end subroutine write_rank_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_resolution_sph(id_file)
!
      use m_sph_modes_grid_labels
!
      integer(kind = kint), intent(in) :: id_file
!
!
      write(id_file,'(a)', advance='NO') hd_trunc()
      write(id_file,'(3i16)') sph_IO1%nidx_gl_sph(1:sph_IO1%numdir_sph)
      write(id_file,'(i16)') sph_IO1%ltr_gl
!
      end subroutine write_gl_resolution_sph
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_nodes_sph(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
!
      write(id_file,'(i16)') sph_IO1%numnod_sph
      do i = 1, sph_IO1%numnod_sph
        write(id_file,'(20i16)')                                        &
     &         inod_gl_sph_IO(i), idx_gl_sph_IO(i,1:sph_IO1%numdir_sph)
      end do
!
      call deallocate_nod_id_sph_IO
!
      end subroutine write_gl_nodes_sph
!
! -----------------------------------------------------------------------
!
      end module spherical_model_IO
