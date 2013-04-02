!ordering_pvr_sf_domain_grp.f90
!      module ordering_pvr_sf_domain_grp
!
      module ordering_pvr_sf_domain_grp
!
!        programmed by H.Matsui on Aug., 2011
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      integer(kind = kint) :: isize_array = 0
      integer(kind = kint), allocatable :: i_org(:)
      integer(kind = kint), allocatable :: itmp(:)
      real(kind = kreal), allocatable :: ztmp(:)
!
!      subroutine s_ordering_pvr_sf_domain_grp
!      subroutine dealloc_ordering_pvr_domain_grp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_ordering_pvr_sf_domain_grp(i_pvr)
!
      use m_surf_grp_4_pvr_domain
      use m_control_params_4_pvr
      use quicksort
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: inum, ist, ied, nd, k1
!
!
      if (isize_array .eq. 0) then
        allocate(itmp(ntot_pvr_surf_domain))
        allocate(ztmp(ntot_pvr_surf_domain))
        allocate(i_org(ntot_pvr_surf_domain))
        isize_array = ntot_pvr_surf_domain
      end if
!
      ist = istack_pvr_surf_domain(i_pvr-1) + 1
      ied = istack_pvr_surf_domain(i_pvr)
!
      do inum = 1, ntot_pvr_surf_domain
        ztmp(inum) = screen_posi_pvr_domain(3,inum)
        i_org(inum) = inum
      end do
!
      call quicksort_real_w_index(ntot_pvr_surf_domain, ztmp, ist,      &
     &    ied, i_org)
!
!
      call swap_int_items_sf_grp(itwo, ntot_pvr_surf_domain,            &
     &    item_pvr_surf_domain)
!
      call swap_int_items_sf_grp(itwo, ntot_pvr_surf_domain,            &
     &    isurf_xrng_pvr_domain)
      call swap_int_items_sf_grp(itwo, ntot_pvr_surf_domain,            &
     &    jsurf_yrng_pvr_domain)
!
!
      call swap_real_items_sf_grp(ithree, ntot_pvr_surf_domain,         &
     &    screen_posi_pvr_domain)
      call swap_real_items_sf_grp(ithree, ntot_pvr_surf_domain,         &
     &    screen_norm_pvr_domain)
!
      call swap_real_items_sf_grp(ione, ntot_pvr_surf_domain,           &
     &    screen_w_pvr_domain)
!
      call swap_real_items_sf_grp(itwo, ntot_pvr_surf_domain,           &
     &    screen_xrng_pvr_domain)
      call swap_real_items_sf_grp(itwo, ntot_pvr_surf_domain,           &
     &    screen_yrng_pvr_domain)
      call swap_real_items_sf_grp(itwo, ntot_pvr_surf_domain,           &
     &    screen_zrng_pvr_domain)
!
      do nd = 1, 3
          call swap_real_items_sf_grp(ifour, ntot_pvr_surf_domain,      &
     &        xx_nod_pvr_domain(1,nd) )
          call swap_real_items_sf_grp(ifour, ntot_pvr_surf_domain,      &
     &        xx_model_pvr_domain(1,nd) )
          call swap_real_items_sf_grp(ifour, ntot_pvr_surf_domain,      &
     &        xx_screen_pvr_domain(1,nd) )
      end do
!
      end subroutine s_ordering_pvr_sf_domain_grp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ordering_pvr_domain_grp
!
      deallocate(itmp, ztmp, i_org)
!
      end subroutine dealloc_ordering_pvr_domain_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine swap_int_items_sf_grp(nd, num, item)
!
      integer(kind = kint), intent(in) :: nd, num
      integer(kind = kint), intent(inout) :: item(nd,num)
!
      integer(kind = kint) :: id
!
      do id = 1, nd
        call swap_int_4_sf_grp(nd, num, id, item)
      end do
!
      end subroutine swap_int_items_sf_grp
!
! -----------------------------------------------------------------------
!
      subroutine swap_real_items_sf_grp(nd, num, ritem)
!
      integer(kind = kint), intent(in) :: nd, num
      real(kind = kreal), intent(inout) :: ritem(nd,num)
!
      integer(kind = kint) :: id
!
      do id = 1, nd
        call swap_real_4_sf_grp(nd, num, id, ritem)
      end do
!
      end subroutine swap_real_items_sf_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine swap_int_4_sf_grp(nd, num, id, item)
!
      integer(kind = kint), intent(in) :: nd, num, id
      integer(kind = kint), intent(inout) :: item(nd,num)
!
      integer(kind = kint) :: inum, i
!
!
!
      itmp(1:num) = item(id,1:num)
      do inum = 1, num
        i = i_org(inum)
        item(id,inum) = itmp(i)
      end do
!
      end subroutine swap_int_4_sf_grp
!
! -----------------------------------------------------------------------
!
      subroutine swap_real_4_sf_grp(nd, num, id, ritem)
!
      integer(kind = kint), intent(in) :: nd, num, id
      real(kind = kreal), intent(inout) :: ritem(nd,num)
!
      integer(kind = kint) :: inum, i
!
!
      ztmp(1:num) = ritem(id,1:num)
      do inum = 1, num
        i = i_org(inum)
        ritem(id,inum) = ztmp(i)
      end do
!
      end subroutine swap_real_4_sf_grp
!
! -----------------------------------------------------------------------
!
      end module ordering_pvr_sf_domain_grp
