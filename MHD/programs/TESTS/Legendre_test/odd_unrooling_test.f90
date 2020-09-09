      program odd_unrooling_test

      read(*,*) n
      do k = 1, n/8
        write(*,*) k, (j,j=k*8-7,k*8)
      end do
      ist = 1+int(n/8) * 8
      write(*,*) 'ist8', ist
      do k = 1+ist/4, n/4
        write(*,*) k, (j,j=k*4-3,k*4)
      end do
      ist = 1 + int(n/4) * 4
      write(*,*) 'ist4', ist
      do k = 1+ist/2, n/2
        write(*,*) k, (j,j=k*2-1,k*2)
      end do
      ist = 1 + int(n/2) * 2
      write(*,*) 'ist2', ist
      do k = ist, n
        write(*,*) k
      end do
   
      end program odd_unrooling_test
