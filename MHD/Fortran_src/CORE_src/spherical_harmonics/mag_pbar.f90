!
!      module mag_pbar
!
!      subroutine alloc_mag_lag(ni, lmax)
!      subroutine dealloc_mag_lag
!      subroutine mag_lagendre(ni, lmax)
!      subroutine ordering_mag_lag(ni, lmax)
!      subroutine norm_mag_lag(ni, lmax)
!      subroutine mag_gauss_point(l)
!      subroutine pbar(the,l,m,p)
!
      module mag_pbar
!
      use m_precision
      use m_constants
!
      implicit none
!
      real(kind = kreal), allocatable :: gauss_w(:)
      real(kind = kreal), allocatable :: colat(:)
!
      real(kind = kreal), allocatable :: p_mag(:,:)
      real(kind = kreal), allocatable :: dp_mag(:,:)
!
      real(kind = kreal), allocatable :: aleg1(:,:)
      real(kind = kreal), allocatable :: aleg2(:,:)
      real(kind = kreal), allocatable :: aleg3(:,:)
!
      private :: pbar
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_mag_lag(ni, lmax)
!
      integer(kind = kint), intent(in) :: ni, lmax
!
      allocate( colat(ni) )
      allocate( gauss_w(ni) )
      colat =   0.0d0
      gauss_w = 0.0d0
!
      allocate( aleg1(lmax*(lmax+3)/2+1,ni) )
      allocate( aleg2(lmax*(lmax+5)/2+2,ni) )
      allocate( aleg3(lmax*(lmax+3)/2+1,ni) )
!
      allocate( p_mag(0:lmax*(lmax+2),ni) )
      allocate( dp_mag(0:lmax*(lmax+2),ni) )
      aleg1 = 0.0d0
      aleg2 = 0.0d0
      aleg3 = 0.0d0
      p_mag =  0.0d0
      dp_mag = 0.0d0
!
      end subroutine alloc_mag_lag
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_mag_lag
!
      deallocate( colat, gauss_w)
      deallocate( aleg1, aleg2, aleg3, p_mag, dp_mag)
!
      end subroutine dealloc_mag_lag
!
! -----------------------------------------------------------------------
!
      subroutine mag_lagendre(ni, lmax)
!
      integer(kind = kint), intent(in) :: ni, lmax
!
      integer(kind = kint) :: ic, lm, lmp, l, m, lc, mc, mca
      real(kind = kreal) :: xm, plm
      real(kind = kreal) :: pi, sqrt2pi
      real(kind = kreal), allocatable :: bleg1(:), bleg2(:), bleg3(:)
      real(kind = kreal), allocatable :: clm(:,:)
!c
!c *** legendre functions
!c
      allocate( clm(lmax+2,lmax+1) )
      allocate( bleg1(lmax+2), bleg2(lmax+2), bleg3(lmax+1) )
      bleg1 = 0.
      bleg2 = 0.
      bleg3 = 0.
!
!
      pi = 4.0d0*atan(1.0d0)
      sqrt2pi=sqrt(2.0d0*pi)
!
      do mc=1, lmax+1
        m = mc-1
        mca = m+1
        do lc = mc,lmax+2
          l = lc-1
          clm(lc,mca) = sqrt( dble((l+m)*(l-m))/dble((2*l-1)*(2*l+1)) )
        end do
      end do
!c
!
      do ic = 1, ni
        lm =  0
        lmp = 0
        do mc = 1, lmax+1
          m =   mc-1
          mca = m+1
          xm = (-1.0d0)**m
!
          do lc = mc, lmax+2
            l=lc-1
            call pbar(colat(ic),l,m,plm)
            bleg1(lc)=xm / sqrt2pi * plm
            bleg2(lc)=xm * sqrt2pi * gauss_w(ic) * plm
          end do
!
          bleg3(mc)=dble(m)*clm(mc+1,mca)*bleg1(mc+1)
          if(mc .lt. (lmax+1)) then
            do lc=mc+1, lmax+1
               l=lc-1
               bleg3(lc)=dble(l)*clm(lc+1,mca)*bleg1(lc+1)              &
     &                  - dble(lc)*clm(lc,mca)*bleg1(lc-1)
            end do
          end if
!
          do lc=mc, lmax+1
            lm=lm+1
            lmp=lmp+1
            aleg1(lm,ic) =  bleg1(lc)
            aleg2(lmp,ic) = bleg2(lc)
            aleg3(lm,ic) =  bleg3(lc)
          end do
          lmp=lmp+1
          aleg2(lmp,ic)=bleg2(lmax+2)
        end do
      end do
!c
      deallocate( bleg1, bleg2, bleg3, clm)
!
      end subroutine mag_lagendre
!
! ----------------------------------------------------------------------
!
      subroutine ordering_mag_lag(ni, lmax)
!
      integer(kind = kint), intent(in) :: ni, lmax
!
      real(kind = kreal) :: pi, sqrt2pi, cl
      integer(kind = kint) :: ic, lm, l, m, lc, mc, j
!
      pi = 4.0d0*atan(1.0d0)
      sqrt2pi=sqrt(2.0d0*pi)
!
      lm = 0
      do mc = 1, lmax+1
        m = mc-1
        do lc=mc, lmax+1
          l = lc-1
          lm=lm+1
          j = l*(l+1) + m
          cl = 2.0d0 / sqrt(dble(2*l+1))
          do ic = 1, ni
            p_mag(j,ic) =  aleg1(lm,ni-ic+1)
            dp_mag(j,ic) = aleg3(lm,ni-ic+1)                            &
     &                    / sin( colat(ni-ic+1) )
          end do
        end do
      end do
!
      end subroutine ordering_mag_lag
!
! ----------------------------------------------------------------------
!
      subroutine norm_mag_lag(ni, lmax)
!
      integer(kind = kint), intent(in) :: ni, lmax
!
      real(kind = kreal) :: pi, sqrt2pi, cl
      integer(kind = kint) :: ic, lm, l, m, lc, mc, j
!
      pi = 4.0d0*atan(1.0d0)
      sqrt2pi=sqrt(2.0d0*pi)
!
      lm = 0
      do mc = 1, lmax+1
        m = mc-1
        do lc=mc, lmax+1
          l = lc-1
          lm=lm+1
          j = l*(l+1) + m
          cl = 2.0d0 / sqrt(dble(2*l+1))
          do ic = 1, ni
            p_mag(j,ic) =  (-1.0d0)**m * sqrt2pi*cl * aleg1(lm,ni-ic+1)
            dp_mag(j,ic) = (-1.0d0)**m * sqrt2pi*cl* aleg3(lm,ni-ic+1)  &
     &                    / sin( colat(ni-ic+1) )
          end do
        end do
      end do
!
!   adjust for m = 0
!
      m = 0
      do lc = 1, lmax+1
        l = lc-1
        j = l*(l+1)
        do ic = 1, ni
          p_mag(j,ic) = p_mag(j,ic) / sqrt(2.0d0)
          dp_mag(j,ic) = dp_mag(j,ic) / sqrt(2.0d0)
        end do
      end do
!
      end subroutine norm_mag_lag
!
! ----------------------------------------------------------------------
!
      subroutine mag_gauss_point(l)
!c
!c  mag_gauss_point (linked with pbar) finds the l roots (in theta)
!c  and gaussian weights associated with
!c  the legendre polynomial of degree l > 1
!c
!c  called in prep
!
      integer(kind = kint), intent(in) :: l
!
      integer(kind = kint) :: l1, l2, k, l3, l22, i
      real(kind = kreal) :: pi, del, p1, p2, t1, t2, theta, s, p, co
!c
      pi = 4.0d0*atan(1.0d0)
      del = pi / dble(4*l)
      l1 = l + 1
      co=dble(2*l+3)/dble(l1**2)
      p2=1.0d0
      t2=-del
      l2=l/2
      k=1
!c
      do 10 i=1,l2
   20 t1=t2
      t2=t1+del
      theta=t2
      call pbar(theta,l,izero,p)
      p1=p2
      p2=p
      if((k*p2) .gt. 0.) go to 20
      k=-k
   40 s=(t2-t1)/(p2-p1)
      t1=t2
      t2=t2-s*p2
      theta=t2
      call pbar(theta,l,izero,p)
      p1=p2
      p2=p
      if(abs(p) .le. 1.e-10) go to 30
      if(p2 .eq. p1) then
!c        write(6,*) 'sub mag_gauss_point: zero = ',p,' at i = ',i
         go to 30
      end if
      go to 40
   30 colat(i)=theta
      call pbar(theta,l1,izero,p)
      gauss_w(i)=co*(sin(theta)/p)**2
   10 continue
!c
      l22=2*l2
      if(l22 .eq. l) go to 70
      l2=l2+1
      theta=pi/2.
      colat(l2)=theta
      call pbar(theta,l1,izero,p)
      gauss_w(l2)=co/p**2
   70 continue
!c
      l3=l2+1
      do 50 i=l3,l
      colat(i)=pi-colat(l-i+1)
      gauss_w(i)=gauss_w(l-i+1)
   50 continue
!c
      return
      end subroutine mag_gauss_point
!
! -----------------------------------------------------------------------
!
      subroutine pbar(the,l,m,p)
!c
!c  pbar calculates the value of the normalized associated
!c  legendre function of the first kind, of degree l,
!c  of order m, for the real argument cos(the), and returns
!c  it in the variable p
!c  0 .le. m .le. l
!c
!c  called in mag_gauss_point and prep
!
      integer(kind = kint), intent(in) :: l, m
      real(kind = kreal), intent(in) :: the
      real(kind = kreal), intent(inout) :: p
!
      integer(kind = kint) :: m1, i, j
      real(kind = kreal) :: s, c, p1, p2
!
      s = sin(the)
      c = cos(the)
      p = 1.0d0 / sqrt(2.0d0)
      if(m .eq. 0) go to 22
      do 20 i=1,m
        p=sqrt(dble(2*i+1)/dble(2*i))*s*p
   20 continue
   22 continue
      if(l .eq. m) return
      p1 = 1.0d0
      m1 = m+1
      do 30 j = m1,l
        p2 = p1
        p1 = p
        p = 2.0d0 * sqrt((dble(j**2)-0.25d0)/dble(j**2-m**2)) * c * p1  &
     &     - sqrt(dble((2*j+1)*(j-m-1)*(j+m-1)) /                       &
     &            dble((2*j-3)*(j-m)*(j+m)))*p2
   30 continue
      return
      end subroutine pbar
!
! -----------------------------------------------------------------------
!
      end module mag_pbar
