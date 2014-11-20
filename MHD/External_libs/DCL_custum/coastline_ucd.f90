!
      program coastline_ucd
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint) :: nloop, numnod, numedge
      integer(kind = kint), allocatable :: ie_edge(:)
      real(kind = kreal), allocatable :: tp_edge(:)
      real(kind = kreal), allocatable :: xx_edge(:,:)
      real(kind = kreal), allocatable :: d_edge(:)
!
      real(kind = kreal) :: rtmp(10)
      real(kind = kreal) :: p, t, p1, p2, t1, t2
      real(kind = kreal) :: latrange(2), longrange(2)
!
      integer(kind = kint), parameter :: id_coast = 8
      integer(kind = kint), parameter :: id_file =  7
      real(kind = kreal) :: pi, ratio
!
      integer(kind = kint) :: nnod2, nedge2
      integer(kind = kint) :: i, j, jrest, jnum, jcou, inod, iedge
      integer(kind = kint) :: jnod, jele, i1, i2, iele
      integer(kind = kint) :: n_point, itype
!
!
      open(id_coast,file="coast_world.asc")
!
      nloop =   0
      nnod2 =   0
      nedge2 =  0
      do
        read(id_coast,'(2I8,4F8.3)',end=99,err=99)                      &
     &                      n_point, itype, rtmp(1:4)
!        write(*,*)  'n_point', n_point, itype, rtmp(1:4)
        jcou = 0
        jnum = 10
        jrest = mod(n_point-1,10) + 1
        do
          if( (n_point - jcou) .lt. 10) jnum = jrest
          read(id_coast,'(10F8.3)') rtmp(1:jnum)
          jcou = jcou + jnum
!          write(*,*)  'jcou', jcou, jnum
          if(jcou .ge. n_point) exit
        end do
!        write(*,'(a, 10f8.2)') 'rtmp(1:jnum)', rtmp(1:jnum)
!
        nloop =   nloop + 1
        nnod2 =  nnod2 + n_point - 2
        nedge2 = nedge2 + n_point - 2
      end do
!
  99  continue
      close(id_coast)
!
!
      numnod =  nnod2 /  2
      numedge = nedge2 / 2
      allocate(tp_edge(2*nnod2+2))
      allocate(d_edge(2*nnod2))
      allocate(xx_edge(3,2*numnod))
      allocate(ie_edge(2*nedge2))
!
      open(id_coast,file="coast_world.asc")
!
      inod =  0
      iedge = 0
      do j = 1, nloop
        read(id_coast,'(2I8,4F8.3)',end=98,err=98)                      &
     &                n_point, itype, latrange(1:2), longrange(1:2)
!        write(*,*)  'n_point', n_point,                                &
!     &      latrange(1:2), longrange(1:2), inod, iedge
        d_edge(inod+1:inod+n_point-2) = dble(itype)
!
        do i = 2, n_point-4, 2
          ie_edge(iedge+1) = (inod + i)/2
          ie_edge(iedge+2) = (inod + i)/2 + 1
          iedge = iedge + 2
        end do
        ie_edge(iedge+1) = (inod + n_point-2) / 2
        ie_edge(iedge+2) = (inod + 2) / 2
        iedge = iedge + 2
!        write(*,*) ie_edge(1:4)
!
        jcou = 0
        jnum = 10
        jrest = mod(n_point-1,10) + 1
        do
          if( (n_point - jcou) .lt. 10) jnum = jrest
          read(id_coast,'(10F8.3)') rtmp(1:jnum)
          tp_edge(inod+1:inod+jnum) = rtmp(1:jnum)
          jcou = jcou + jnum
          inod = inod + jnum
          if(jcou .ge. n_point) exit
        end do
        inod = inod - 2
      end do
!
  98  continue
      close(id_coast)
!
!
!
      nedge2 = numedge
      do iele = 1, nedge2
        i1 = ie_edge(2*iele-1)
        i2 = ie_edge(2*iele)
        t1 = tp_edge(2*i1-1)
        t2 = tp_edge(2*i2-1)
        p1 = tp_edge(2*i1  )
        p2 = tp_edge(2*i2  )
        if(     (p1.lt.180.0 .and. p2.gt.180.0)                         &
     &     .or. (p2.lt.180.0 .and. p1.gt.180.0) )  then
          write(*,*) 'cut line at 180deg.: ', iele, i1, p1,  i2, p2
!
          numnod = numnod + 1
          tp_edge(2*numnod-1) = ((p2-180.0)*t1 - (p1-180.0)*t2) / (p2-p1)
          tp_edge(2*numnod  ) = 180.0
          d_edge(2*numnod-1) = d_edge(2*i1-1)
          d_edge(2*numnod  ) = d_edge(2*i1  )
          numnod = numnod + 2
          tp_edge(2*numnod-1) = ((p2-180.0)*t1 - (p1-180.0)*t2) / (p2-p1)
          tp_edge(2*numnod  ) = 180.0
          d_edge(2*numnod-1) = d_edge(2*i2-1)
          d_edge(2*numnod  ) = d_edge(2*i2  )
!
          ie_edge(2*iele-1) = i1
          ie_edge(2*iele  ) = numnod - 1

          numedge = numedge + 1
          ie_edge(2*numedge-1) = numnod
          ie_edge(2*numedge  ) = i2

        else if(p1.eq.180.0 .and. p2.gt.180.0)  then
          write(*,*) 'move line at 180deg.: ', iele, i1, p1,  i2, p2

          numnod = numnod + 1
          tp_edge(2*numnod-1) =  t1
          tp_edge(2*numnod  ) = -p1
          d_edge(2*numnod-1) = d_edge(2*i2-1)
          d_edge(2*numnod  ) = d_edge(2*i2  )

          ie_edge(2*iele-1) = numnod
          ie_edge(2*iele  ) = i2

        else if(p2.eq.180.0 .and. p1.gt.180.0)  then
          write(*,*) 'move line at 180deg.: ', iele, i1, p1,  i2, p2

          numnod = numnod + 1
          tp_edge(2*numnod-1) =  t2
          tp_edge(2*numnod  ) = -p2
          d_edge(2*numnod-1) = d_edge(2*i1-1)
          d_edge(2*numnod  ) = d_edge(2*i1  )

          ie_edge(2*iele-1) = i1
          ie_edge(2*iele  ) = numnod
        end if
      end do
!
      do inod = 1, numnod
        if( tp_edge(2*inod) .gt. 180.0) then
          write(*,*) 'flip angle at: ', inod, tp_edge(2*inod)
          tp_edge(2*inod) = tp_edge(2*inod) - 360.0
        end if
      end do
!
!
      pi = four*atan(one)
      ratio = pi / 180.0d0
      tp_edge(1:2*numnod) = tp_edge(1:2*numnod) * ratio
!
      do inod = 1, numnod
        t = tp_edge(2*inod-1)
        p = tp_edge(2*inod  )
        xx_edge(1,inod) = cos(t) * cos(p)
        xx_edge(2,inod) = cos(t) * sin(p)
        xx_edge(3,inod) = sin(t)
      end do
!
!      tp_edge(1:2*numnod) = tp_edge(1:2*numnod) / ratio
!
      open(id_file,file="coastline.0.inp")
!
      write(id_file,*) numnod, numedge
      do i = 1, numnod
        write(id_file,'(i16,1p3e16.7)') i, xx_edge(1:3,i)
!        write(id_file,'(i16,1p3e16.7)') i, tp_edge(2*i-1:2*i)
      end do
!
      do i = 1, numedge
        write(id_file,'(2i16,a7,2i16)') i, ione,                        &
     &               '  line ', ie_edge(2*i-1:2*i)
      end do
!
      write(id_file,'(2i4)') ione, ione
      write(id_file,'(a,a1)') "coastline", ','
      do i = 1, numnod
        write(id_file,'(i16,1pe16.7)') i, d_edge(2*i)
      end do
!
      close(id_file)
!
!
      open(id_file,file="coastline_c.c")

      write(id_file,'(a)')
      write(id_file,'(a)') '/*  coastline_c.c */'
      write(id_file,'(a)')

      write(id_file,'(a)') '#include "coastline_c.h" '

      write(id_file,'(a,i6,a)') 'int nnod_coast =  ', numnod,  ';'
      write(id_file,'(a,i6,a)') 'int nedge_coast = ', numedge, ';'
      write(id_file,'(a,i6,a)') 'int iedge_coast[', numedge, '][2] = {'
      do i = 1, numedge
        write(id_file,'(a2,i6,a1,i6,a2)')                               &
     &        '	{',ie_edge(2*i-1), ',', ie_edge(2*i), '},'
      end do
      write(id_file,'(a)') '};'
!
!
      write(id_file,'(a,i6,a)')                                        &
     &         'double map_coast_r1[', numnod, '][2] = {'
      do i = 1, numnod
        write(id_file,'(a2,1pe13.5,a1,1pe13.5,a2)')                     &
     &        '	{',tp_edge(2*i-1), ',', tp_edge(2*i), '},'
      end do
      write(id_file,'(a)') '};'
!
      write(id_file,'(a,i6,a)') 'double d_coast_r1[',numnod,'] = {'
      do i = 1, numnod
        write(id_file,'(10(1pe8.2,a2))')  d_edge(2*i),', '
      end do
      write(id_file,'(a)') '};'
!
      write(id_file,'(a)') 'int get_nedge_coastline()'
      write(id_file,'(a)') '{	return nedge_coast;};'
!
      write(id_file,'(a)') 'void get_coastline'
      write(id_file,'(a)')                                              &
     &    '		(int iedge, double *tp_coast, double *lake){'
      write(id_file,'(a)') '	int k1, inod;'
      write(id_file,'(a)') '	for(k1=0;k1<2;k1++){'
      write(id_file,'(a)')                                              &
     &   '		inod = iedge_coast[iedge][k1] - 1;'
      write(id_file,'(a)')                                              &
     &   '		tp_coast[2*k1  ] = map_coast_r1[inod][0];'
      write(id_file,'(a)')                                              &
     &   '		tp_coast[2*k1+1] = map_coast_r1[inod][1];'
      write(id_file,'(a)') '		lake[k1  ] = d_coast_r1[inod];'
      write(id_file,'(a)') '	}'
      write(id_file,'(a)') '	return;'
      write(id_file,'(a)') '};'

      close(id_file)
!
!
!
      open(id_file,file="coastline_c.h")

      write(id_file,'(a)')
      write(id_file,'(a)') '/*  coastline_c.h */'
      write(id_file,'(a)')
      write(id_file,'(a)') '#ifndef COASTLINE_C_'
      write(id_file,'(a)') '#define COASTLINE_C_'
      write(id_file,'(a)')
      write(id_file,'(a)') '#ifdef __cplusplus'
      write(id_file,'(a)') 'extern "C" {'
      write(id_file,'(a)') '#endif'
      write(id_file,'(a)')
      write(id_file,'(a)') '/* prototypes */'
      write(id_file,'(a)')
      write(id_file,'(a)') 'int get_nedge_coastline();'
!
      write(id_file,'(a)') 'void get_coastline'
      write(id_file,'(a)')                                              &
     &    '		(int iedge, double *tp_coast, double *lake);'

      write(id_file,'(a)') '#ifdef __cplusplus'
      write(id_file,'(a)') '}'
      write(id_file,'(a)') '#endif'
      write(id_file,'(a)')
      write(id_file,'(a)') '#endif'

      close(id_file)
!
      stop
      end program coastline_ucd
!
!
