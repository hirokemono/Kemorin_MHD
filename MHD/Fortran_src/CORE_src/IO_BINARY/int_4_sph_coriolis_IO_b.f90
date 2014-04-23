!int_4_sph_coriolis_IO_b.f90
!      module int_4_sph_coriolis_IO_b
!
!     Written by H. Matsui on March, 2010
!
!      subroutine write_int_4_sph_coriolis_b
!      subroutine read_int_4_sph_coriolis_b
!
      module int_4_sph_coriolis_IO_b
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_int_4_sph_coriolis_b
!
      use m_int_4_sph_coriolis_IO
!
      integer(kind = kint) :: j1, j2
!
!
      write(*,'(a,a)') 'Write tri-integration data file: ',             &
     &                trim(sph_cor_file_name)
      open(id_sph_cor,file=sph_cor_file_name, form='unformatted')
!
      write(id_sph_cor)  ltr_cor_IO
!
      j1 = 2
      do j2 = 1, 2
        write(id_sph_cor) jgl_kcor_IO(1:jmax_cor_IO,j2,j1)
        write(id_sph_cor) gk_cor_IO(1:jmax_cor_IO,j2,j1)
      end do
      write(id_sph_cor) jgl_lcor_IO(1:jmax_cor_IO,1,j1)
      write(id_sph_cor) el_cor_IO(1:jmax_cor_IO,1,j1)
!*
!
      do j1 = 1, 3, 2
        do j2 = 1, 4
          write(id_sph_cor) jgl_kcor_IO(1:jmax_cor_IO,j2,j1)
          write(id_sph_cor) gk_cor_IO(1:jmax_cor_IO,j2,j1)
        end do
        do j2 = 1, 2
          write(id_sph_cor) jgl_lcor_IO(1:jmax_cor_IO,j2,j1)
          write(id_sph_cor) el_cor_IO(1:jmax_cor_IO,j2,j1)
        end do
      end do
      close(id_sph_cor)
!
      call deallocate_int_sph_cor_IO
!
      end subroutine write_int_4_sph_coriolis_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_int_4_sph_coriolis_b
!
      use m_int_4_sph_coriolis_IO
      use skip_comment_f
!
      integer(kind = kint) :: j1, j2
!
!
      write(*,*) 'read integrals for coriolis: ',                       &
     &           trim(sph_cor_file_name)
      open(id_sph_cor,file=sph_cor_file_name, form='unformatted')
!
      read(id_sph_cor) ltr_cor_IO
      call allocate_int_sph_cor_IO
!
      j1 = 2
      do j2 = 1, 2
        read(id_sph_cor) jgl_kcor_IO(1:jmax_cor_IO,j2,j1)
        read(id_sph_cor) gk_cor_IO(1:jmax_cor_IO,j2,j1)
      end do
      read(id_sph_cor) jgl_lcor_IO(1:jmax_cor_IO,1,j1)
      read(id_sph_cor) el_cor_IO(1:jmax_cor_IO,1,j1)
!*
!
      do j1 = 1, 3, 2
        do j2 = 1, 4
          read(id_sph_cor) jgl_kcor_IO(1:jmax_cor_IO,j2,j1)
          read(id_sph_cor) gk_cor_IO(1:jmax_cor_IO,j2,j1)
        end do
        do j2 = 1, 2
          read(id_sph_cor) jgl_lcor_IO(1:jmax_cor_IO,j2,j1)
          read(id_sph_cor) el_cor_IO(1:jmax_cor_IO,j2,j1)
        end do
      end do
      close(id_sph_cor)
!
      end subroutine read_int_4_sph_coriolis_b
!
! -----------------------------------------------------------------------
!
      end module int_4_sph_coriolis_IO_b
