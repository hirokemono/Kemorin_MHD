!
!
      program testlu_5band
!
      use m_precision
!
      use m_sample_matrices
      use m_ludcmp
      use m_ludcmp_5mat
      use m_ludcmp_bmat
      use m_ludcmp_band
      use lubksb_357band
!
      implicit none
!
      real(kind = kreal), allocatable :: band_a(:,:), band_lu(:,:)
      real(kind = kreal) :: d, diff, rnrm
      integer(kind = kint) :: indx(ncomp_mat)
      integer(kind = kint) :: i
!
!
      call init_5band_sample_mat
!
      call ludcmp(mat2d16_5band,ncomp_mat,ncomp_mat,indx,d)
!
      xvec16(1:16) = bvec16_5band(1:16)
      call lubksb(mat2d16_5band,ncomp_mat,ncomp_mat,indx,xvec16)
!
      rnrm = 0.0d0
      do i = 1, ncomp_mat
        rnrm = rnrm + (xvec16(i) - xvec16_solution(i))**2
      end do
!
      write(*,*) 'solve by original solver', rnrm
      write(*,*) 'i, indx(i), xvec16(i), xvec16_solution(i), diff'
      do i = 1, ncomp_mat
        diff =  xvec16(i) - xvec16_solution(i)
        write(*,'(2i5,1p3E25.15e3)') i, indx(i), xvec16(i),             &
     &                         xvec16_solution(i), diff
      end do
!
!
!
      call init_5band_sample_mat
!
      call ludcmp_5b(mat2d16_5band,ncomp_mat,ncomp_mat,indx,d)
!
      xvec16(1:16) = bvec16_5band(1:16)
      call lubksb_5b(mat2d16_5band,ncomp_mat,ncomp_mat,indx,xvec16)
!
      rnrm = 0.0d0
      do i = 1, ncomp_mat
        rnrm = rnrm + (xvec16(i) - xvec16_solution(i))**2
      end do
!
      write(*,*) 'solve by 5-band solver', rnrm
      write(*,*) 'i, indx(i), xvec16(i), xvec16_solution(i), diff'
      do i = 1, ncomp_mat
        diff =  xvec16(i) - xvec16_solution(i)
        write(*,'(2i5,1p3E25.15e3)') i, indx(i), xvec16(i),             &
     &                         xvec16_solution(i), diff
      end do
!
!
!
      call init_7band_sample_mat
!
      call ludcmp(mat2d16_7band,ncomp_mat,ncomp_mat,indx,d)
!
      xvec16(1:16) = bvec16_7band(1:16)
      call lubksb(mat2d16_7band,ncomp_mat,ncomp_mat,indx,xvec16)
!
      rnrm = 0.0d0
      do i = 1, ncomp_mat
        rnrm = rnrm + (xvec16(i) - xvec16_solution(i))**2
      end do
!
      write(*,*) 'solve by original solver', rnrm
      write(*,*) 'i, indx(i), xvec16(i), xvec16_solution(i), diff'
      do i = 1, ncomp_mat
        diff =  xvec16(i) - xvec16_solution(i)
        write(*,'(2i5,1p3E25.15e3)') i, indx(i), xvec16(i),             &
     &                         xvec16_solution(i), diff
      end do
!
!
      call init_7band_sample_mat
!
      call ludcmp_bmat(mat2d16_7band,ncomp_mat,ncomp_mat,nb7,indx,d)
!
      write(*,*) 'matrix after LU decomposit'
      do i = 1, ncomp_mat
        write(*,'(2i5,1p255e12.4)')                                     &
     &        i, indx(i), mat2d16_7band(i,1:ncomp_mat)
      end do
!
      xvec16(1:16) = bvec16_7band(1:16)
      call lubksb_bmat(mat2d16_7band,ncomp_mat,ncomp_mat,               &
     &    nb7,indx,xvec16)
!
      rnrm = 0.0d0
      do i = 1, ncomp_mat
        rnrm = rnrm + (xvec16(i) - xvec16_solution(i))**2
      end do
!
      write(*,*) 'solve by band solver', rnrm
      write(*,*) 'i, indx(i), xvec16(i), xvec16_solution(i), diff'
      do i = 1, ncomp_mat
        diff =  xvec16(i) - xvec16_solution(i)
        write(*,'(2i5,1p3E25.15e3)') i, indx(i), xvec16(i),             &
     &                         xvec16_solution(i), diff
      end do
!
!
      allocate( band_a(nb7,ncomp_mat) )
      allocate( band_lu(2*nb7-1,ncomp_mat) )
!
      call init_7band_sample_mat
      call set_mt_2_band_mat(ncomp_mat, ncomp_mat, nb7, mat2d16_7band,  &
     &    band_a)
!
      write(*,*) 'band_a(1:nb7,i)'
      do i = 1, ncomp_mat
        write(*,'(i5, 1p255e12.4)') i, band_a(1:nb7,i)
      end do
!
      call ludcmp_band(ncomp_mat, nb7, band_a, band_lu, indx,d)
!
!      write(*,*) 'band_lu(1:2*nb7-1,i)'
!      do i = 1, ncomp_mat
!        write(*,'(i5, 1p255e12.4)') i, band_lu(1:2*nb7-1,i)
!      end do
!
!      write(*,*) 'matrix after LU decomposit'
!      do i = 1, ncomp_mat
!        write(*,'(2i5,1p255e12.4)')                                     &
!     &        i, indx(i), mat2d16_7band(i,1:ncomp_mat)
!      end do
!
      xvec16(1:16) = bvec16_7band(1:16)
      call lubksb_band(ncomp_mat, nb7,band_lu, indx,xvec16)
!
      rnrm = 0.0d0
      do i = 1, ncomp_mat
        rnrm = rnrm + (xvec16(i) - xvec16_solution(i))**2
      end do
!
      write(*,*) 'solve by band format solver', rnrm
      write(*,*) 'i, indx(i), xvec16(i), xvec16_solution(i), diff'
      do i = 1, ncomp_mat
        diff =  xvec16(i) - xvec16_solution(i)
        write(*,'(2i5,1p3E25.15e3)') i, indx(i), xvec16(i),             &
     &                         xvec16_solution(i), diff
      end do
!
!
      xvec16(1:16) = bvec16_7band(1:16)
      call lubksb_7band(ncomp_mat, band_lu, indx, xvec16)
!
      rnrm = 0.0d0
      do i = 1, ncomp_mat
        rnrm = rnrm + (xvec16(i) - xvec16_solution(i))**2
      end do
!
      write(*,*) 'solve by 7 band format solver', rnrm
      write(*,*) 'i, indx(i), xvec16(i), xvec16_solution(i), diff'
      do i = 1, ncomp_mat
        diff =  xvec16(i) - xvec16_solution(i)
        write(*,'(2i5,1p3E25.15e3)') i, indx(i), xvec16(i),             &
     &                         xvec16_solution(i), diff
      end do
!
      stop
      end program testlu_5band
