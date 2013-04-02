!
!     module mat_conect_SORT
!
!      Written by K. Nakajima in 2001
!        modified by H. Matsui on May. 2002
!        modified by H. Matsui on June. 2006
!
!      subroutine matconSORT (STEM, INUM, N, NN)
!
      module mat_conect_SORT
!
      use m_precision
!
      implicit none
!
      integer(kind=kint), allocatable :: ISTACK(:,:)
      private :: ISTACK
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine matconSORT (STEM, INUM, N, NN)
!
      integer(kind=kint), intent(in) :: N, NN
      integer(kind=kint), intent(inout) :: STEM(N), INUM(N)
!
      integer(kind=kint) :: i, ii, k, ik1, ik2, icon, ICONmax
!
!
      allocate (ISTACK(0:NN+1,2))
      ISTACK(0,1)= 0
      ISTACK(0,2)= 0

!CDIR NODEP
      do i= 1, N
        INUM(i)= i
        STEM(i)= STEM(i) + 1
      enddo

!CDIR NODEP
      do i= 1, NN+1
        ISTACK(i,1)= 0
      enddo

      ICONmax= -N
!CDIR NOVECTOR
      do i= 1, N
        ii= STEM(i)
        ICONmax= max(ii,ICONmax)
        ISTACK(ii,1)= ISTACK(ii,1) + 1
      enddo

!CDIR NOVECTOR
      do k= 1, ICONmax
        ISTACK(k,1)= ISTACK(k-1,1) + ISTACK(k,1)
        ISTACK(k,2)= ISTACK(k  ,1)
      enddo

      ISTACK(0,2)= ISTACK(1,2)
!CDIR NOVECTOR
      do k= 1, ICONmax
        ik1= ICONmax - k
        ik2= ik1     + 1
        ISTACK(k,1)= ISTACK(ik2,2) - ISTACK(ik1,2) + ISTACK(k-1,1)
      enddo

!CDIR NODEP
      do k= 1, ICONmax
        ISTACK(k,2)= 0
      enddo

!CDIR NOVECTOR
      do i= 1, N
        ii  = STEM(i)
        icon= ISTACK(ii,2) + 1
        ISTACK(ii,2)= icon
        INUM(ISTACK(ICONmax-ii+1-1,1)+icon)= i
      enddo

      do i= 1, N
        STEM(i)= STEM(i) - 1
      enddo

      deallocate (ISTACK)

      return
      end subroutine matconSORT
!
! ----------------------------------------------------------------------
!
      end module mat_conect_SORT
