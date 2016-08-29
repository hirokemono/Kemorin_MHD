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
      use binary_IO
!
      integer(kind = kint) :: j1, j2
!
!
      write(*,'(a,a)') 'Write tri-integration data file: ',             &
     &                trim(sph_cor_file_name)
      call open_write_binary_file(sph_cor_file_name)
      call write_fld_inthead_b(ltr_cor_IO)
!
      call write_fld_mul_inthead_b                                      &
     &  ((jmax_cor_IO*itwo), jgl_kcor_IO(1,1,2))
      call write_fld_realarray2_b                                       &
     &   (jmax_cor_IO, itwo, gk_cor_IO(1,1,2))
!
      call write_fld_mul_inthead_b                                      &
     &   (jmax_cor_IO, jgl_lcor_IO(1,1,2))
      call write_fld_realarray2_b                                       &
     &   (jmax_cor_IO, ione, el_cor_IO(1,1,2))
!*
!
      do j1 = 1, 3, 2
        call write_fld_mul_inthead_b                                    &
     &    ((jmax_cor_IO*ifour), jgl_kcor_IO(1,1,j1))
        call write_fld_realarray2_b                                     &
     &     (jmax_cor_IO, ifour, gk_cor_IO(1,1,j1))
!
        call write_fld_mul_inthead_b                                    &
     &    ((jmax_cor_IO*itwo), jgl_lcor_IO(1,1,j1))
        call write_fld_realarray2_b                                     &
     &     (jmax_cor_IO, itwo, el_cor_IO(1,1,j1))
      end do
      call close_binary_file
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
      use binary_IO
      use skip_comment_f
!
      integer(kind = kint) :: j1, j2
!
!
      write(*,*) 'read integrals for coriolis: ',                       &
     &           trim(sph_cor_file_name)
      call open_read_binary_file(sph_cor_file_name, my_rank)
!
      call read_fld_inthead_b(ltr_cor_IO)
      call allocate_int_sph_cor_IO
!
      call read_fld_mul_inthead_b                                       &
     &  ((jmax_cor_IO*itwo), jgl_kcor_IO(1,1,2))
      call read_fld_realarray2_b                                        &
     &   (jmax_cor_IO, itwo, gk_cor_IO(1,1,2))
!
      call read_fld_mul_inthead_b                                       &
     &   (jmax_cor_IO, jgl_lcor_IO(1,1,2))
      call read_fld_realarray2_b                                        &
     &   (jmax_cor_IO, ione, el_cor_IO(1,1,2))
!*
!
      do j1 = 1, 3, 2
        call read_fld_mul_inthead_b                                     &
     &    ((jmax_cor_IO*ifour), jgl_kcor_IO(1,1,j1))
        call read_fld_realarray2_b                                      &
     &     (jmax_cor_IO, ifour, gk_cor_IO(1,1,j1))
!
        call read_fld_mul_inthead_b                                     &
     &    ((jmax_cor_IO*itwo), jgl_lcor_IO(1,1,j1))
        call read_fld_realarray2_b                                      &
     &     (jmax_cor_IO, itwo, el_cor_IO(1,1,j1))
      end do
      call close_binary_file
!
      end subroutine read_int_4_sph_coriolis_b
!
! -----------------------------------------------------------------------
!
      end module int_4_sph_coriolis_IO_b
