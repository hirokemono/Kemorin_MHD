!find_boundary_4_tri_refine.f90
!      module find_boundary_4_tri_refine
!
!      Writen by H. Matsui on Oct., 2007
!
!!      subroutine s_find_boundary_4_tri_refine                         &
!!     &          (numele, numsurf, isf_4_ele, nele_tri, iele_tri,      &
!!     &           iflag_refine_ele)
!
      module find_boundary_4_tri_refine
!
      use m_precision
      use m_geometry_constants
!
      implicit none
!
      integer(kind = kint), private :: nsurf_surfdomain
      integer(kind = kint), allocatable, private :: isurf_surfdomain(:)
!
      integer(kind = kint), allocatable, private :: imark_sf(:)
!
      private :: set_triple_refine_by_surf, set_domain_bc_list
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_find_boundary_4_tri_refine                           &
     &          (numele, numsurf, isf_4_ele, nele_tri, iele_tri,        &
     &           iflag_refine_ele)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint), intent(in) :: nele_tri
      integer(kind = kint), intent(in) :: iele_tri(nele_tri)
!
      integer(kind = kint), intent(inout) :: iflag_refine_ele(numele)
!
      integer(kind = kint) :: inum, iele, isurf, icou, iflag
      integer(kind = kint) :: isf1, isf2, isf3, isf4, isf5, isf6
!
!
      allocate(imark_sf(numsurf))
      imark_sf =  0
!
!   mark boundary of domain surface
!
      call set_domain_bc_list(numele, numsurf, isf_4_ele)
!
! interior... imark_sf = 0
!
      do isurf = 1, numsurf
        imark_sf(isurf) = -imark_sf(isurf)
      end do
!
!   mark boundary of refinment surface
!
      do inum = 1, nele_tri
        iele = iele_tri(inum)
!
        isf1 = abs(isf_4_ele(iele,1))
        isf2 = abs(isf_4_ele(iele,2))
        isf3 = abs(isf_4_ele(iele,3))
        isf4 = abs(isf_4_ele(iele,4))
        isf5 = abs(isf_4_ele(iele,5))
        isf6 = abs(isf_4_ele(iele,6))
!
        imark_sf(isf1) = imark_sf(isf1) + isf_4_ele(iele,1) / isf1
        imark_sf(isf2) = imark_sf(isf2) + isf_4_ele(iele,2) / isf2
        imark_sf(isf3) = imark_sf(isf3) + isf_4_ele(iele,3) / isf3
        imark_sf(isf4) = imark_sf(isf4) + isf_4_ele(iele,4) / isf4
        imark_sf(isf5) = imark_sf(isf5) + isf_4_ele(iele,5) / isf5
        imark_sf(isf6) = imark_sf(isf6) + isf_4_ele(iele,6) / isf6
      end do
!
      icou = 0
      do
        call set_triple_refine_by_surf(numele, isf_4_ele,               &
     &      nele_tri, iele_tri, iflag_refine_ele, iflag)
        if(iflag .eq. 0) exit
        icou = icou + 1
      end do
!
!
      deallocate(imark_sf, isurf_surfdomain)
!
      end subroutine s_find_boundary_4_tri_refine
!
!  ---------------------------------------------------------------------
!
      subroutine set_triple_refine_by_surf(numele, isf_4_ele,           &
     &          nele_tri, iele_tri, iflag_refine_ele, iflag)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint), intent(in) :: nele_tri
      integer(kind = kint), intent(in) :: iele_tri(nele_tri)
!
      integer(kind = kint), intent(inout) :: iflag_refine_ele(numele)
      integer(kind = kint), intent(inout) :: iflag
!
      integer(kind = kint) :: inum, iele, isig
      integer(kind = kint) :: isf1, isf2, isf3, isf4, isf5, isf6
      integer(kind = kint) :: imk1, imk2, imk3, imk4, imk5, imk6
!
      iflag = 0
      do inum = 1, nele_tri
        iele = iele_tri(inum)
        isf1 = abs(isf_4_ele(iele,1))
        isf2 = abs(isf_4_ele(iele,2))
        isf3 = abs(isf_4_ele(iele,3))
        isf4 = abs(isf_4_ele(iele,4))
        isf5 = abs(isf_4_ele(iele,5))
        isf6 = abs(isf_4_ele(iele,6))
!
        imk1 = imark_sf(isf1)
        imk2 = imark_sf(isf2)
        imk3 = imark_sf(isf3)
        imk4 = imark_sf(isf4)
        imk5 = imark_sf(isf5)
        imk6 = imark_sf(isf6)
!
        isig = abs(imk1) + abs(imk2) + abs(imk3)                        &
     &        + abs(imk4) + abs(imk5) + abs(imk6)
!
        if(isig.eq.6) then
          iflag_refine_ele(iele) = iflag_nothing
!
        else if(isig.eq.5 .or. isig.eq.4) then
          iflag_refine_ele(iele) = iflag_nothing
!
          if(imark_sf(isf1) .eq. 0) imark_sf(isf1) = 1
          if(imark_sf(isf2) .eq. 0) imark_sf(isf2) = 1
          if(imark_sf(isf3) .eq. 0) imark_sf(isf3) = 1
          if(imark_sf(isf4) .eq. 0) imark_sf(isf4) = 1
          if(imark_sf(isf5) .eq. 0) imark_sf(isf5) = 1
          if(imark_sf(isf6) .eq. 0) imark_sf(isf6) = 1
!
          iflag = 1
!
        else if(isig.eq.3) then
          if    (imk1.eq.0 .and. imk2.ne.0 .and. imk3.eq.0              &
     &     .and. imk4.ne.0 .and. imk5.eq.0 .and. imk6.ne.0) then
            iflag_refine_ele(iele) = iflag_tri_n1
          else if (imk1.ne.0 .and. imk2.eq.0 .and. imk3.eq.0            &
     &     .and. imk4.ne.0 .and. imk5.eq.0 .and. imk6.ne.0) then
            iflag_refine_ele(iele) = iflag_tri_n2
          else if (imk1.ne.0 .and. imk2.ne.0 .and. imk3.eq.0            &
     &     .and. imk4.eq.0 .and. imk5.eq.0 .and. imk6.ne.0) then
            iflag_refine_ele(iele) = iflag_tri_n3
          else if (imk1.eq.0 .and. imk2.ne.0 .and. imk3.ne.0            &
     &     .and. imk4.eq.0 .and. imk5.eq.0 .and. imk6.ne.0) then
            iflag_refine_ele(iele) = iflag_tri_n4
          else if (imk1.eq.0 .and. imk2.ne.0 .and. imk3.eq.0            &
     &     .and. imk4.ne.0 .and. imk5.ne.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_n5
          else if (imk1.ne.0 .and. imk2.eq.0 .and. imk3.eq.0            &
     &     .and. imk4.ne.0 .and. imk5.ne.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_n6
          else if (imk1.ne.0 .and. imk2.ne.0 .and. imk3.eq.0            &
     &     .and. imk4.eq.0 .and. imk5.ne.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_n7
          else if (imk1.eq.0 .and. imk2.ne.0 .and. imk3.ne.0            &
     &     .and. imk4.eq.0 .and. imk5.ne.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_n8
          else
            iflag_refine_ele(iele) = iflag_nothing
!
            if(imark_sf(isf1) .eq. 0) imark_sf(isf1) = 1
            if(imark_sf(isf2) .eq. 0) imark_sf(isf2) = 1
            if(imark_sf(isf3) .eq. 0) imark_sf(isf3) = 1
            if(imark_sf(isf4) .eq. 0) imark_sf(isf4) = 1
            if(imark_sf(isf5) .eq. 0) imark_sf(isf5) = 1
            if(imark_sf(isf6) .eq. 0) imark_sf(isf6) = 1
            iflag = 1
          end if
!
        else if(isig.eq.2) then
          if    (imk1.eq.0 .and. imk2.eq.0 .and. imk3.eq.0              &
     &     .and. imk4.ne.0 .and. imk5.eq.0 .and. imk6.ne.0) then
            iflag_refine_ele(iele) = iflag_tri_e1
          else if (imk1.ne.0 .and. imk2.eq.0 .and. imk3.eq.0            &
     &     .and. imk4.eq.0 .and. imk5.eq.0 .and. imk6.ne.0) then
            iflag_refine_ele(iele) = iflag_tri_e2
          else if (imk1.eq.0 .and. imk2.eq.0 .and. imk3.ne.0            &
     &     .and. imk4.eq.0 .and. imk5.eq.0 .and. imk6.ne.0) then
            iflag_refine_ele(iele) = iflag_tri_e3
          else if (imk1.eq.0 .and. imk2.ne.0 .and. imk3.eq.0            &
     &     .and. imk4.eq.0 .and. imk5.eq.0 .and. imk6.ne.0) then
            iflag_refine_ele(iele) = iflag_tri_e4
          else if (imk1.eq.0 .and. imk2.eq.0 .and. imk3.eq.0            &
     &     .and. imk4.ne.0 .and. imk5.ne.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_e5
          else if (imk1.ne.0 .and. imk2.eq.0 .and. imk3.eq.0            &
     &     .and. imk4.eq.0 .and. imk5.ne.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_e6
          else if (imk1.eq.0 .and. imk2.eq.0 .and. imk3.ne.0            &
     &     .and. imk4.eq.0 .and. imk5.ne.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_e7
          else if (imk1.eq.0 .and. imk2.ne.0 .and. imk3.eq.0            &
     &     .and. imk4.eq.0 .and. imk5.ne.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_e8
          else if (imk1.eq.0 .and. imk2.ne.0 .and. imk3.eq.0            &
     &     .and. imk4.ne.0 .and. imk5.eq.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_e9
          else if (imk1.ne.0 .and. imk2.eq.0 .and. imk3.eq.0            &
     &     .and. imk4.ne.0 .and. imk5.eq.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_e10
          else if (imk1.ne.0 .and. imk2.eq.0 .and. imk3.ne.0            &
     &     .and. imk4.eq.0 .and. imk5.eq.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_e11
          else if (imk1.eq.0 .and. imk2.ne.0 .and. imk3.ne.0            &
     &     .and. imk4.eq.0 .and. imk5.eq.0 .and. imk6.eq.0) then
            iflag_refine_ele(iele) = iflag_tri_e12
          else
!
            if(imark_sf(isf1) .eq. 0) imark_sf(isf1) = 1
            if(imark_sf(isf2) .eq. 0) imark_sf(isf2) = 1
            if(imark_sf(isf3) .eq. 0) imark_sf(isf3) = 1
            if(imark_sf(isf4) .eq. 0) imark_sf(isf4) = 1
            if(imark_sf(isf5) .eq. 0) imark_sf(isf5) = 1
            if(imark_sf(isf6) .eq. 0) imark_sf(isf6) = 1
            iflag = 1
          end if
!
        else if(isig .eq. 1) then
          iflag_refine_ele(iele) = iflag_tri_full
!
        else if(isig .eq. 0) then
          iflag_refine_ele(iele) = iflag_tri_full
        end if
!
      end do
!
      end subroutine set_triple_refine_by_surf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_domain_bc_list(numele, numsurf, isf_4_ele)
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint) :: iele, isurf, icou
      integer(kind = kint) :: isf1, isf2, isf3, isf4, isf5, isf6
!
!
      imark_sf =  0
!
!   mark boundary of domain surface
!
      do iele = 1, numele
        isf1 = abs(isf_4_ele(iele,1))
        isf2 = abs(isf_4_ele(iele,2))
        isf3 = abs(isf_4_ele(iele,3))
        isf4 = abs(isf_4_ele(iele,4))
        isf5 = abs(isf_4_ele(iele,5))
        isf6 = abs(isf_4_ele(iele,6))
!
        imark_sf(isf1) = imark_sf(isf1) + isf_4_ele(iele,1) / isf1
        imark_sf(isf2) = imark_sf(isf2) + isf_4_ele(iele,2) / isf2
        imark_sf(isf3) = imark_sf(isf3) + isf_4_ele(iele,3) / isf3
        imark_sf(isf4) = imark_sf(isf4) + isf_4_ele(iele,4) / isf4
        imark_sf(isf5) = imark_sf(isf5) + isf_4_ele(iele,5) / isf5
        imark_sf(isf6) = imark_sf(isf6) + isf_4_ele(iele,6) / isf6
      end do
!
      nsurf_surfdomain = 0
      do isurf = 1, numsurf
        nsurf_surfdomain = nsurf_surfdomain + abs( imark_sf(isurf) )
      end do
!
      allocate(isurf_surfdomain(nsurf_surfdomain))
!
      icou = 0
      do isurf = 1, numsurf
        if( imark_sf(isurf) .ne. 0) then 
          icou = icou + 1
          isurf_surfdomain(icou) = isurf
        end if
      end do
!
      do icou = 1, nsurf_surfdomain
        isurf =   isurf_surfdomain(icou)
      end do
!
      end subroutine set_domain_bc_list
!
!  ---------------------------------------------------------------------
!
      end module find_boundary_4_tri_refine
