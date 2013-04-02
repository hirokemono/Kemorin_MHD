!
!
      use m_precision
!
      use m_ludcmp
      use m_ludcmp_band
      use m_ludcmp_3band
      use lubksb_357band
!
      implicit none
!
!
      character(len=kchara) :: tmpchara
      real(kind = kreal) :: mat3(3,97), lu3(5,97), b1(97), b2(97)
      real(kind = kreal) :: mat5(5,97), lu5(9,97), v1(97), v2(97)
      real(kind = kreal) :: a(97,97), rr(97), d, diff(5,97)
      real(kind = kreal) :: p(97,97), diff5(9,97)
      integer(kind = kint) :: i_pivot(97), indx(97)
      integer(kind = kint) :: i_pivot5(97), indx5(97)
      integer :: k, itmp, ierr, k2
      real rtmp
!
      open(9,file='input.txt')
      read(9,*) tmpchara
      read(9,*) itmp, rr(1), rtmp, mat3(2,1), mat3(1,1+1)
      do k = 2, 96
        read(9,*) itmp, rr(k), mat3(3,k-1), mat3(2,k), mat3(1,k+1)
      end do
      read(9,*) itmp, rr(97), mat3(3,97-1), mat3(2,97), rtmp
      read(9,*) tmpchara
      do k = 1, 97
        read(9,*) b1(k)
      end do
!
      read(9,*) tmpchara
      read(9,*) itmp, rr(1), rtmp, rtmp, mat5(3,1), &
     &            mat5(2,1+1), mat5(1,1+2)
      read(9,*) itmp, rr(2), rtmp, mat5(4,2-1), mat5(3,2), &
     &            mat5(2,2+1), mat5(1,2+2)
      do k = 3, 95
        read(9,*) itmp, rr(k), mat5(5,k-2), mat5(4,k-1), mat5(3,k), &
     &            mat5(2,k+1), mat5(1,k+2)
      end do
      read(9,*) itmp, rr(96), mat5(5,96-2), mat5(4,96-1), mat5(3,96), &
     &            mat5(2,96+1), rtmp
      read(9,*) itmp, rr(97), mat5(5,97-2), mat5(4,97-1), mat5(3,97), &
     &            rtmp, rtmp
      read(9,*) tmpchara
      do k = 1, 97
        read(9,*) v1(k)
      end do
      close(9)
!
      b2 = b1
      v2 = v1
!
      a(1,1)   = mat3(2,1  )
      a(1,1+1) = mat3(1,1+1)
      do k = 2, 96
        a(k,k-1) = mat3(3,k-1)
        a(k,k)   = mat3(2,k  )
        a(k,k+1) = mat3(1,k+1)
      end do
      a(97,97-1) = mat3(3,97-1)
      a(97,97)   = mat3(2,97  )
!
        p(1,1)   = mat5(3,1  )
        p(1,1+1) = mat5(2,1+1)
        p(1,1+2) = mat5(1,1+2)
!
        p(2,2-1) = mat5(4,2-1)
        p(2,2)   = mat5(3,2  )
        p(2,2+1) = mat5(2,2+1)
        p(2,2+2) = mat5(1,2+2)
      do k = 3, 95
        p(k,k-2) = mat5(5,k-2)
        p(k,k-1) = mat5(4,k-1)
        p(k,k)   = mat5(3,k  )
        p(k,k+1) = mat5(2,k+1)
        p(k,k+2) = mat5(1,k+2)
      end do
        p(96,96-2) = mat5(5,96-2)
        p(96,96-1) = mat5(4,96-1)
        p(96,96)   = mat5(3,96  )
        p(96,96+1) = mat5(2,96+1)
!
        p(97,97-2) = mat5(5,97-2)
        p(97,97-1) = mat5(4,97-1)
        p(97,97)   = mat5(3,97  )
!
      call ludcmp(a, 97, 97, indx, d)
      call ludcmp(p, 97, 97, indx5, d)
      call ludcmp_3band(97, mat3, i_pivot, ierr, lu3, d)
      call ludcmp_band(97, 5, mat5, lu5, i_pivot5,  d)
!
      do k = 1, 97
        write(10,*) k, indx(k)
      end do
      do k = 1, 97
        do k2 = 1, 97
        write(10,*) k, k2, a(k,k2)
        end do
      end do
!
      do k = 1, 97
        write(11,*) k, indx5(k)
      end do
      do k = 1, 97
        do k2 = 1, 97
        write(11,*) k, k2, p(k,k2)
        end do
      end do
!
      do k = 1, 97
        write(8,*) k, indx5(k), i_pivot5(k), indx5(k)-i_pivot5(k)
      end do
      do k = 1, 97
        do k2 = 1, 9
          write(8,*) k, k2, lu5(-k2,k+k2-5)
        end do
      end do
!
      do k = 1, 97
        write(9,*) k, indx(k), i_pivot(k), indx(k)-i_pivot(k)
      end do
      write(9,*) 1, 0.0d0, 0.0d0, lu3(3,1), lu3(2,1+1), lu3(1,1+2)
      write(9,*) 2, 0.0d0, lu3(4,2-1), lu3(3,2), lu3(2,2+1), lu3(1,2+2)
      do k = 3, 95
        write(9,*) k,  lu3(5,k-2), lu3(4,k-1), lu3(3,k), lu3(2,k+1), lu3(1,k+2)
      end do
        write(9,*) 96, lu3(5,96-2), lu3(4,96-1), lu3(3,96), lu3(2,96+1), 0.0d0
        write(9,*) 97,   lu3(5,97-2), lu3(4,97-1), lu3(3,97), 0.0d0, 0.0d0
!
      call lubksb(a, 97, 97, indx, b1)
      call lubksb_3band(97, lu3, i_pivot, b2)
!
      call lubksb(p, 97, 97, indx5, v1)
      call lubksb_5band(97, lu5, i_pivot5, v2)
!
      write(9,*) 1, 0.0d0, 0.0d0, lu3(3,1), lu3(2,1+1), lu3(1,1+2)
      write(9,*) 2, 0.0d0, lu3(4,2-1), lu3(3,2), lu3(2,2+1), lu3(1,2+2)
      do k = 3, 95
        write(9,*) k,  lu3(5,k-2), lu3(4,k-1), lu3(3,k), lu3(2,k+1), lu3(1,k+2)
      end do
        write(9,*) 96, lu3(5,96-2), lu3(4,96-1), lu3(3,96), lu3(2,96+1), 0.0d0
        write(9,*) 97,   lu3(5,97-2), lu3(4,97-1), lu3(3,97), 0.0d0, 0.0d0
!
      do k = 1, 97
        write(*,*) k, b1(k), b2(k), b2(k)-b1(k)
      end do
      do k = 1, 97
        write(*,*) k, v1(k), v2(k), v2(k)-v1(k)
      end do
      stop
      end
