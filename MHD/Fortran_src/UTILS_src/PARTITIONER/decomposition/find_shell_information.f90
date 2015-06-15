!
!      module find_shell_information
!
!      Written by H. Matsui on Sep., 2007
!      subroutine s_find_shell_information(itheta, iphi, nnod,          &
!     &          radius, theta, phi,  num_bc_grp, ntot_bc_grp,          &
!     &          istack_bc_grp, item_bc_grp, name_bc_grp)
!
!
      module find_shell_information
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_find_shell_information(itheta, iphi, nnod,           &
     &          radius, theta, phi,  num_bc_grp, ntot_bc_grp,           &
     &          istack_bc_grp, item_bc_grp, name_bc_grp)

      use m_shell_surface
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: itheta, iphi
      integer(kind = kint), intent(in) :: nnod
      real(kind= kreal), intent(in) :: radius(nnod)
      real(kind= kreal), intent(in) :: theta(nnod)
      real(kind= kreal), intent(in) :: phi(nnod)
!
      integer(kind = kint), intent(in) :: num_bc_grp, ntot_bc_grp
      integer(kind = kint), intent(in) :: istack_bc_grp(0:num_bc_grp)
      integer(kind = kint), intent(in) :: item_bc_grp(ntot_bc_grp)
      character(len=kchara), intent(in) :: name_bc_grp(num_bc_grp)
!
      integer(kind = kint) :: inod_oc_start, inod_oc_end, num_free
      integer(kind = kint) :: num_ICB, numnod_horizontal
      integer(kind = kint) :: i0, ii, is, i, istart, iend, itp
      integer(kind = kint) :: inod, num1, irest1, iflag, icount, isf
      real(kind= kreal) :: delta
!
!
      itp = itheta * iphi
      inod_oc_start = nnod
      inod_oc_end   = 1
!
!
      do isf = 1, num_bc_grp
       if ( name_bc_grp(isf) .eq. 'CMB' ) then
        num_CMB = istack_bc_grp(isf)-istack_bc_grp(isf-1)
        nnod_CMB = num_CMB
        write(*,*) 'num_CMB', num_CMB
        num_layer = nnod/num_CMB
!
        allocate ( rtp_cmb(num_CMB,3) )
        allocate ( IGROUP_cmb(num_CMB) )
        allocate ( istack_sph(num_CMB) )
!
        istack_sph = 0
        rtp_cmb = 0.0d0
        IGROUP_cmb = 0
!
        do i = 1, num_CMB
         i0 = istack_bc_grp(isf-1)+i
         inod = item_bc_grp(i0)
         rtp_cmb(i,1) = radius(inod)
         rtp_cmb(i,2) = theta(inod)
         rtp_cmb(i,3) = phi(inod)
         inod_oc_end = max(inod_oc_end,inod)
        end do
       end if
!
       if ( name_bc_grp(isf) .eq. 'ICB' ) then
        num_ICB = istack_bc_grp(isf)-istack_bc_grp(isf-1)
        do i = istack_bc_grp(isf-1)+1, istack_bc_grp(isf)
         inod_oc_start = min(inod_oc_start,item_bc_grp(i))
        end do
       end if
      end do
!
      numnod_horizontal = 1+int( sqrt( real(num_ICB-2)/6.0 ) )
      num_cube = (numnod_horizontal)**3
      num_layer = (nnod-num_cube) / num_CMB
      write(*,*) 'num_cube', num_cube
      write(*,*) 'num_layer', num_layer
      allocate (inod_free(num_cube))
      allocate ( item_sph(num_layer,num_CMB) )
      item_sph = 0
!
!  conut number of surface nodes for each subdomain
!
      call cal_divide_and_rest(num1, irest1, num_CMB, itp)
      call set_number_of_segments(itp, num1, irest1, numcmb_local)
!
      inod_free = 0
      iflag = 0
      istart = nnod
      iend = num_cube
      icount = 0

      do 
      do inod = istart, iend+1, -1
       iflag= 0
       delta = 10.0
       do is = 1, num_CMB
        if (    abs(theta(inod)-rtp_cmb(is,2)) .lt. 1.0d-9              &
     &    .and. abs(phi(inod)-rtp_cmb(is,3))  .lt. 1.0d-9         &
     &    .and. istack_sph(is) .lt. num_layer) then
         icount = icount + 1
         istack_sph(is) = istack_sph(is) + 1
         item_sph(istack_sph(is),is) = inod
         iflag = 1
         exit
        end if
       end do
       if (iflag.eq.0) then
        write(*,*) inod,':where should I belong??'
        num_free = num_free + 1
        inod_free(num_cube-num_free+1) = inod
       end if
      end do
      istart = iend
      iend = iend - (num_CMB*num_layer-icount)
      write(*,*) 'rest:', (num_CMB*num_layer-icount)
      if ( icount .ge. (num_CMB*num_layer) ) exit
      end do
!
!      write(*,*) 'istart, address:', istart, num_free, num_cube
      do i = istart, 1, -1
       inod_free(i) = i
      end do
!
      ii = 0
      do is = 1, num_cmb
       ii = ii + istack_sph(is)
      end do
      write(*,*) 'num. sphere:', ii
!
      nlayer_ICB = 1
      nlayer_CMB = num_layer
!
      allocate ( IGROUP_radius(num_layer) )
      IGROUP_radius = 1
!
      end subroutine s_find_shell_information
!
!   --------------------------------------------------------------------
!
      end module find_shell_information
