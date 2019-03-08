!
!      module find_shell_information
!
!      Written by H. Matsui on Sep., 2007
!!
!!      subroutine s_find_shell_information(itheta, iphi, nnod,         &
!!     &          radius, theta, phi,  num_bc_grp, ntot_bc_grp,         &
!!     &          istack_bc_grp, item_bc_grp, name_bc_grp)
!!     &          sphere_4_part)
!!        type(shell_surface_4_part), intent(inout) :: sphere_4_part
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
     &          istack_bc_grp, item_bc_grp, name_bc_grp,                &
     &          sphere_4_part)

      use t_shell_surface_4_part
      use cal_minmax_and_stacks
      use coordinate_converter
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
      type(shell_surface_4_part), intent(inout) :: sphere_4_part
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
      num_free = 0
      num_ICB = 8
!
!
      do isf = 1, num_bc_grp
       if ( name_bc_grp(isf) .eq. 'CMB' ) then
        sphere_4_part%num_CMB = istack_bc_grp(isf)-istack_bc_grp(isf-1)
        sphere_4_part%nnod_CMB = sphere_4_part%num_CMB
        write(*,*) 'num_CMB', sphere_4_part%num_CMB
        sphere_4_part%num_layer = nnod / sphere_4_part%num_CMB
!
        call alloc_xyz_cmb_4_part(sphere_4_part)
!
        call alloc_IGROUP_CMB(sphere_4_part)
!
          do i = 1, sphere_4_part%num_CMB
           i0 = istack_bc_grp(isf-1)+i
           inod = item_bc_grp(i0)
           sphere_4_part%rtp_cmb(i,1) = radius(inod)
           sphere_4_part%rtp_cmb(i,2) = theta(inod)
           sphere_4_part%rtp_cmb(i,3) = phi(inod)
           inod_oc_end = max(inod_oc_end,inod)
         end do
!
          call position_2_xyz(sphere_4_part%num_CMB,                    &
     &        sphere_4_part%rtp_cmb(1,1), sphere_4_part%rtp_cmb(1,2),   &
     &        sphere_4_part%rtp_cmb(1,3), sphere_4_part%xx_cmb(1,1),    &
     &        sphere_4_part%xx_cmb(1,2), sphere_4_part%xx_cmb(1,3))
          call position_sph_2_cyl_radius(sphere_4_part%num_CMB,         &
     &        sphere_4_part%rtp_cmb(1,1), sphere_4_part%rtp_cmb(1,2),   &
     &        sphere_4_part%s_cmb, sphere_4_part%as_cmb,                &
     &        sphere_4_part%ar_cmb)
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
      numnod_horizontal = 1 + int( sqrt( real(num_ICB-2)/6.0 ) )
      sphere_4_part%num_cube = (numnod_horizontal)**3
      sphere_4_part%num_layer = (nnod-sphere_4_part%num_cube)           &
     &                         / sphere_4_part%num_CMB
      write(*,*) 'num_cube', sphere_4_part%num_cube
      write(*,*) 'num_layer', sphere_4_part%num_layer
!
!  conut number of surface nodes for each subdomain
!
      call alloc_item_sph_4_part(sphere_4_part)
      call cal_divide_and_rest                                          &
     &   (num1, irest1, sphere_4_part%num_CMB, itp)
      call set_number_of_segments                                       &
     &   (itp, num1, irest1, sphere_4_part%numcmb_local)
!
      iflag = 0
      istart = nnod
      iend = sphere_4_part%num_cube
      icount = 0

      do 
      do inod = istart, iend+1, -1
       iflag= 0
       delta = 10.0
       do is = 1, sphere_4_part%num_CMB
        if (  abs(theta(inod)-sphere_4_part%rtp_cmb(is,2)) .lt. 1.0d-9  &
     &   .and. abs(phi(inod)-sphere_4_part%rtp_cmb(is,3))  .lt. 1.0d-9  &
     &    .and. sphere_4_part%istack_sph(is)                            &
     &               .lt. sphere_4_part%num_layer) then
         icount = icount + 1
         sphere_4_part%istack_sph(is)                                   &
     &          = sphere_4_part%istack_sph(is) + 1
         sphere_4_part%item_sph(sphere_4_part%istack_sph(is),is) = inod
         iflag = 1
         exit
        end if
       end do
       if (iflag.eq.0) then
        write(*,*) inod,':where should I belong??'
        num_free = num_free + 1
        sphere_4_part%inod_free(sphere_4_part%num_cube-num_free+1)      &
     &           = inod
       end if
      end do
      istart = iend
      iend = iend - (sphere_4_part%num_CMB * sphere_4_part%num_layer    &
     &             - icount)
      write(*,*) 'rest:',                                               &
     &   (sphere_4_part%num_CMB * sphere_4_part%num_layer - icount)
      if(icount .ge. (sphere_4_part%num_CMB * sphere_4_part%num_layer)) &
     &   exit
      end do
!
!      write(*,*) 'istart, address:', istart, num_free,                 &
!     &           sphere_4_part%num_cube
      do i = istart, 1, -1
       sphere_4_part%inod_free(i) = i
      end do
!
      ii = 0
      do is = 1, sphere_4_part%num_cmb
       ii = ii + sphere_4_part%istack_sph(is)
      end do
      write(*,*) 'num. sphere:', ii
!
      sphere_4_part%nlayer_ICB = 1
      sphere_4_part%nlayer_CMB = sphere_4_part%num_layer
!
      call alloc_IGROUP_radius(sphere_4_part)
!
      end subroutine s_find_shell_information
!
!   --------------------------------------------------------------------
!
      end module find_shell_information
