!m_surf_grp_4_pvr_domain.f90
!      module m_surf_grp_4_pvr_domain
!
!        programmed by H.Matsui on Aug., 2011
!
!      subroutine allocate_pvr_surf_domain_num
!      subroutine allocate_pvr_surf_domain_item
!      subroutine allocate_iflag_pvr_used_ele(numele)
!
!      subroutine deallocate_pvr_surf_domain_item
!      subroutine deallocate_iflag_pvr_used_ele
!      subroutine find_item_pvr_surf_domain
!
      module m_surf_grp_4_pvr_domain
!
      use m_precision
!
      use m_constants
      use m_control_params_4_pvr
!
      implicit  none
!
      integer(kind = kint) :: ntot_pvr_surf_domain
      integer(kind = kint), allocatable :: nsurf_pvr_surf_domain(:)
      integer(kind = kint), allocatable :: istack_pvr_surf_domain(:)
      integer(kind = kint), allocatable :: item_pvr_surf_domain(:,:)
!
      real(kind = kreal), allocatable :: screen_posi_pvr_domain(:,:)
      real(kind = kreal), allocatable :: screen_norm_pvr_domain(:,:)
      real(kind = kreal), allocatable :: screen_w_pvr_domain(:)
!
      real(kind = kreal), allocatable :: xx_nod_pvr_domain(:,:)
      real(kind = kreal), allocatable :: xx_model_pvr_domain(:,:)
      real(kind = kreal), allocatable :: xx_screen_pvr_domain(:,:)
!         (4*(Surface #-1)+locel node#, direction)
!
      real(kind = kreal), allocatable :: screen_xrng_pvr_domain(:,:)
      real(kind = kreal), allocatable :: screen_yrng_pvr_domain(:,:)
      real(kind = kreal), allocatable :: screen_zrng_pvr_domain(:,:)
      integer(kind = kint), allocatable :: isurf_xrng_pvr_domain(:,:)
      integer(kind = kint), allocatable :: jsurf_yrng_pvr_domain(:,:)
!
      integer(kind = kint), allocatable :: iflag_pvr_used_ele(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pvr_surf_domain_num
!
!
      allocate(nsurf_pvr_surf_domain(num_pvr))
      allocate(istack_pvr_surf_domain(0:num_pvr))
      nsurf_pvr_surf_domain =  0
      istack_pvr_surf_domain = 0
!
      end subroutine allocate_pvr_surf_domain_num
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pvr_surf_domain_item
!
!
      ntot_pvr_surf_domain = istack_pvr_surf_domain(num_pvr)
      allocate(item_pvr_surf_domain(2,ntot_pvr_surf_domain))
      allocate(screen_norm_pvr_domain(3,ntot_pvr_surf_domain))
      allocate(screen_posi_pvr_domain(3,ntot_pvr_surf_domain))
      allocate(screen_w_pvr_domain(ntot_pvr_surf_domain))
!
      allocate(screen_xrng_pvr_domain(2,ntot_pvr_surf_domain))
      allocate(screen_yrng_pvr_domain(2,ntot_pvr_surf_domain))
      allocate(screen_zrng_pvr_domain(2,ntot_pvr_surf_domain))
      allocate(isurf_xrng_pvr_domain(2,ntot_pvr_surf_domain))
      allocate(jsurf_yrng_pvr_domain(2,ntot_pvr_surf_domain))
!
      allocate(xx_nod_pvr_domain(4*ntot_pvr_surf_domain,4))
      allocate(xx_model_pvr_domain(4*ntot_pvr_surf_domain,4))
      allocate(xx_screen_pvr_domain(4*ntot_pvr_surf_domain,4))
!
      item_pvr_surf_domain = 0
      screen_norm_pvr_domain = 0.0d0
      screen_posi_pvr_domain = 0.0d0
!
      screen_xrng_pvr_domain = 0.0d0
      screen_yrng_pvr_domain = 0.0d0
      screen_zrng_pvr_domain = 0.0d0
      isurf_xrng_pvr_domain = 0
      jsurf_yrng_pvr_domain = 0
!
      end subroutine allocate_pvr_surf_domain_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_iflag_pvr_used_ele(numele)
!
      integer(kind = kint), intent(in) :: numele
!
!
      allocate(iflag_pvr_used_ele(numele,num_pvr))
      iflag_pvr_used_ele = 0
!
      end subroutine allocate_iflag_pvr_used_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pvr_surf_domain_item
!
      deallocate(xx_nod_pvr_domain, xx_model_pvr_domain)
      deallocate(xx_screen_pvr_domain, item_pvr_surf_domain)
      deallocate(screen_posi_pvr_domain, screen_norm_pvr_domain)
      deallocate(screen_xrng_pvr_domain, screen_yrng_pvr_domain)
      deallocate(screen_zrng_pvr_domain)
      deallocate(isurf_xrng_pvr_domain,  jsurf_yrng_pvr_domain)
!
      deallocate(nsurf_pvr_surf_domain, istack_pvr_surf_domain)
!
      end subroutine deallocate_pvr_surf_domain_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_iflag_pvr_used_ele
!
      deallocate(iflag_pvr_used_ele)
!
      end subroutine deallocate_iflag_pvr_used_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_surf_grp_4_pvr_domain(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i_pvr, ist, ied
!
!
      write(50+my_rank,*) 'istack_pvr_surf_domain',                     &
     &                      istack_pvr_surf_domain
      do i_pvr = 1, num_pvr
        write(50+my_rank,*) 'nsurf_pvr_surf_domain',                    &
     &                     i_pvr, nsurf_pvr_surf_domain(i_pvr)
        ist = istack_pvr_surf_domain(i_pvr-1) + 1
        ied = istack_pvr_surf_domain(i_pvr)
        write(50+my_rank,'(8i10)') item_pvr_surf_domain(1,ist:ied)
        write(50+my_rank,'(8i10)') item_pvr_surf_domain(2,ist:ied)
      end do
!
      end subroutine check_surf_grp_4_pvr_domain
!
! -----------------------------------------------------------------------
!
      subroutine check_surf_posi_pvr_domain(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i_pvr, ist, ied, inum
!
!
      write(50+my_rank,*) 'istack_pvr_surf_domain',                     &
     &                      istack_pvr_surf_domain
      do i_pvr = 1, num_pvr
        write(50+my_rank,*) 'screen_posi_pvr_domain',                   &
     &                     i_pvr, nsurf_pvr_surf_domain(i_pvr)
        ist = istack_pvr_surf_domain(i_pvr-1) + 1
        ied = istack_pvr_surf_domain(i_pvr)
        do inum = ist, ied
          write(50+my_rank,*) inum, screen_posi_pvr_domain(1:3,inum),   &
     &                        screen_w_pvr_domain(inum)
        end do
      end do
!
      end subroutine check_surf_posi_pvr_domain
!
! -----------------------------------------------------------------------
!
      subroutine check_surf_norm_pvr_domain(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i_pvr, ist, ied, inum
!
!
      do i_pvr = 1, num_pvr
        write(50+my_rank,*) 'screen_norm_pvr_domain',                   &
     &                     i_pvr, nsurf_pvr_surf_domain(i_pvr)
        ist = istack_pvr_surf_domain(i_pvr-1) + 1
        ied = istack_pvr_surf_domain(i_pvr)
        do inum = ist, ied
          write(50+my_rank,*) inum, screen_norm_pvr_domain(1:3,inum)
        end do
      end do
!
      end subroutine check_surf_norm_pvr_domain
!
! -----------------------------------------------------------------------
!
      subroutine check_surf_rng_pvr_domain(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i_pvr, ist, ied, inum
!
!
      do i_pvr = 1, num_pvr
        write(50+my_rank,*) 'isurf_xrng_pvr_domain',                    &
     &                     i_pvr, nsurf_pvr_surf_domain(i_pvr)
        ist = istack_pvr_surf_domain(i_pvr-1) + 1
        ied = istack_pvr_surf_domain(i_pvr)
        do inum = ist, ied
          write(50+my_rank,'(i15,4i5,1p4e16.7)')                        &
     &          inum, isurf_xrng_pvr_domain(1:2,inum),                  &
     &                jsurf_yrng_pvr_domain(1:2,inum),                  &
     &                screen_xrng_pvr_domain(1:2,inum),                 &
     &                screen_yrng_pvr_domain(1:2,inum)
        end do
      end do
!
      end subroutine check_surf_rng_pvr_domain
!
! -----------------------------------------------------------------------
!
      end module m_surf_grp_4_pvr_domain
