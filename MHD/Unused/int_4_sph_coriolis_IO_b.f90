!int_4_sph_coriolis_IO_b.f90
!      module int_4_sph_coriolis_IO_b
!
!     Written by H. Matsui on March, 2010
!
!!      subroutine write_int_4_sph_coriolis_b
!!      subroutine read_int_4_sph_coriolis_b(ierr)
!
      module int_4_sph_coriolis_IO_b
!
      use m_precision
      use t_binary_IO_buffer
!
      implicit none
!
      integer(kind = kint), parameter :: id_read_cor =  21
      integer(kind = kint), parameter :: id_write_cor = 22
      type(binary_IO_buffer), private :: bbuf_cor
!
      private :: bbuf_cor, id_read_cor, id_write_cor
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
      integer(kind = kint_gl) :: num64
      integer(kind = kint) :: j1, j2
!
!
      write(*,'(a,a)') 'Write tri-integration data file: ',             &
     &                trim(sph_cor_file_name)
      bbuf_cor%id_binary = id_write_cor
      call open_write_binary_file(sph_cor_file_name, bbuf_cor)
      if(bbuf_cor%ierr_bin .gt. 0) go to 99
      call write_one_integer_b(ltr_cor_IO, bbuf_cor)
      if(bbuf_cor%ierr_bin .gt. 0) go to 99
!
      num64 = jmax_cor_IO * itwo
      call write_mul_integer_b(num64, jgl_kcor_IO(1,1,2), bbuf_cor)
      if(bbuf_cor%ierr_bin .gt. 0) go to 99
!
      num64 = jmax_cor_IO
      call write_2d_vector_b                                            &
     &   (num64, itwo, gk_cor_IO(1,1,2), bbuf_cor)
      if(bbuf_cor%ierr_bin .gt. 0) go to 99
      call write_mul_integer_b(num64, jgl_lcor_IO(1,1,2), bbuf_cor)
      if(bbuf_cor%ierr_bin .gt. 0) go to 99
      call write_2d_vector_b                                            &
     &   (num64, ione, el_cor_IO(1,1,2), bbuf_cor)
      if(bbuf_cor%ierr_bin .gt. 0) go to 99
!
      do j1 = 1, 3, 2
        num64 = jmax_cor_IO * ifour
        call write_mul_integer_b                                        &
     &     (num64, jgl_kcor_IO(1,1,j1), bbuf_cor)
        if(bbuf_cor%ierr_bin .gt. 0) go to 99
!
        call write_2d_vector_b(trim_long(jmax_cor_IO), ifour,           &
     &      gk_cor_IO(1,1,j1), bbuf_cor)
        if(bbuf_cor%ierr_bin .gt. 0) go to 99
!
        num64 = jmax_cor_IO * itwo
        call write_mul_integer_b                                        &
     &     (num64, jgl_lcor_IO(1,1,j1), bbuf_cor)
        if(bbuf_cor%ierr_bin .gt. 0) go to 99
!
        call write_2d_vector_b(trim_long(jmax_cor_IO), itwo,            &
     &      el_cor_IO(1,1,j1), bbuf_cor)
        if(bbuf_cor%ierr_bin .gt. 0) go to 99
      end do
  99  contninue
      call close_binary_file(bbuf_cor)
      ierr = bbuf_cor%ierr_bin
!
      call deallocate_int_sph_cor_IO
!
      end subroutine write_int_4_sph_coriolis_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_int_4_sph_coriolis_b(ierr)
!
      use m_int_4_sph_coriolis_IO
      use binary_IO
      use skip_comment_f
!
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint) :: j1, j2
!
!
      write(*,*) 'read integrals for coriolis: ',                       &
     &           trim(sph_cor_file_name)
      bbuf_cor%id_binary = id_read_cor
      call open_read_binary_file                                        &
     &   (sph_cor_file_name, my_rank, bbuf_cor)
      if(bbuf_cor%ierr_bin .ne. 0) goto 99
!
      call read_one_integer_b(bbuf_cor, ltr_cor_IO, bbuf_cor)
      if(bbuf_cor%ierr_bin .gt. 0) goto 99
!
      call allocate_int_sph_cor_IO
!
      num64 = jmax_cor_IO*itwo
      call read_mul_integer_b(bbuf_cor, num64, jgl_kcor_IO(1,1,2))
      if(bbuf_cor%ierr_bin .gt. 0)  go to 99
!
      num64 = jmax_cor_IO
      call read_2d_vector_b                                             &
     &   (bbuf_cor, num64, itwo, gk_cor_IO(1,1,2))
      if(bbuf_cor%ierr_bin .gt. 0) go to 99
      call read_mul_integer_b(bbuf_cor, num64, jgl_lcor_IO(1,1,2))
      if(bbuf_cor%ierr_bin .gt. 0) go to 99
      call read_2d_vector_b                                             &
     &   (bbuf_cor, num64, ione, el_cor_IO(1,1,2))
      if(bbuf_cor%ierr_bin .gt. 0) goto 99
!*
!
      do j1 = 1, 3, 2
        num64 = jmax_cor_IO * ifour
        call read_mul_integer_b                                         &
     &     (bbuf_cor, num64, jgl_kcor_IO(1,1,j1))
        if(bbuf_cor%ierr_bin .gt. 0) go to 99
!
        call read_2d_vector_b(bbuf_cor,                                    &
     &      cast_long(jmax_cor_IO), ifour, gk_cor_IO(1,1,j1))
        if(bbuf_cor%ierr_bin .gt. 0) goto 99
!
        num64 = jmax_cor_IO * itwo
        call read_mul_integer_b                                         &
     &     (bbuf_cor, num64, jgl_lcor_IO(1,1,j1))
        if(bbuf_cor%ierr_bin .gt. 0) go to 99
!
        call read_2d_vector_b(bbuf_cor,                                    &
     &      cast_long(jmax_cor_IO), itwo, el_cor_IO(1,1,j1))
        if(bbuf_cor%ierr_bin .gt. 0) goto 99
      end do
!
  99  continue
      call close_binary_file(bbuf_cor)
      ierr = bbuf_cor%ierr_IO
!
      end subroutine read_int_4_sph_coriolis_b
!
! -----------------------------------------------------------------------
!
      end module int_4_sph_coriolis_IO_b
