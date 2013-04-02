!find_pvr_surf_domain.f90
!      module find_pvr_surf_domain
!
!        programmed by H.Matsui on Aug., 2011
!
!      subroutine s_find_pvr_surf_domain(numele, numsurf, e_multi,      &
!     &          isf_4_ele, iele_4_surf, num_mat, num_mat_bc,           &
!     &          mat_istack, mat_item)
!      subroutine set_pvr_domain_surface_data(i_pvr, numele, numsurf,   &
!     &          nnod_4_surf, ie_surf, isf_4_ele)
!
      module find_pvr_surf_domain
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use m_control_params_4_pvr
      use m_surf_grp_4_pvr_domain
!
      implicit  none
!
      private :: norm_on_model_pvr_domains, range_on_screen_pvr_domains
      private :: range_on_pixel_pvr_domains
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_find_pvr_surf_domain(numele, numsurf, e_multi,       &
     &          isf_4_ele, iele_4_surf, num_mat, num_mat_bc,            &
     &          mat_istack, mat_item)
!
      use m_geometry_constants
      use find_selected_domain_bd
      use set_iflag_for_used_ele
!
      integer(kind=kint), intent(in) :: numele, numsurf
      real(kind = kreal), intent(in) :: e_multi(numele)
      integer(kind=kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind=kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
!
      integer(kind = kint) :: i_pvr, jst_grp, ist
!
!
      call allocate_pvr_surf_domain_num
      call allocate_iflag_pvr_used_ele(numele)
      call allocate_imark_4_surface(numsurf)
!
!
      istack_pvr_surf_domain(0) = 0
      do i_pvr = 1, num_pvr
        jst_grp = istack_grp_area_pvr(i_pvr-1) + 1
        call s_set_iflag_for_used_ele(numele, e_multi,                  &
     &      num_mat, num_mat_bc, mat_istack, mat_item,                  &
     &      nele_grp_area_pvr(i_pvr), id_ele_grp_area_pvr(jst_grp),     &
     &      iflag_pvr_used_ele(1,i_pvr))
!
        call mark_selected_domain_bd(numele, numsurf, isf_4_ele,        &
     &      iflag_pvr_used_ele(1,i_pvr))
        call count_selected_domain_bd(numsurf,                          &
     &      nsurf_pvr_surf_domain(i_pvr))
        istack_pvr_surf_domain(i_pvr) = istack_pvr_surf_domain(i_pvr-1) &
     &                                 + nsurf_pvr_surf_domain(i_pvr)
      end do
      ntot_pvr_surf_domain = istack_pvr_surf_domain(num_pvr)
!
      call allocate_pvr_surf_domain_item
      do i_pvr = 1, num_pvr
        ist = istack_pvr_surf_domain(i_pvr-1) + 1
        call mark_selected_domain_bd(numele, numsurf, isf_4_ele,        &
     &      iflag_pvr_used_ele(1,i_pvr))
        call s_find_selected_domain_bd(numele, numsurf, iele_4_surf,    &
     &      iflag_pvr_used_ele(1,i_pvr), nsurf_pvr_surf_domain(i_pvr),  &
     &      item_pvr_surf_domain(1,ist) )
      end do
!
      call deallocate_imark_4_surface
!
      end subroutine s_find_pvr_surf_domain
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_domain_surface_data(i_pvr, numele, numsurf,    &
     &          nnod_4_surf, ie_surf, isf_4_ele)
!
      use ordering_pvr_sf_domain_grp
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint), intent(in) :: numele, numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
!
!$omp parallel
      call norm_on_model_pvr_domains(i_pvr, numele, numsurf,            &
     &          nnod_4_surf, ie_surf, isf_4_ele)
      call range_on_screen_pvr_domains(i_pvr, numele, numsurf,          &
     &          nnod_4_surf, ie_surf, isf_4_ele)
      call range_on_pixel_pvr_domains(i_pvr)
!$omp end parallel
!
      call s_ordering_pvr_sf_domain_grp(i_pvr)
!
      end subroutine set_pvr_domain_surface_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine norm_on_model_pvr_domains(i_pvr, numele, numsurf,      &
     &          nnod_4_surf, ie_surf, isf_4_ele)
!
      use m_geometries_in_pvr_screen
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint), intent(in) :: numele, numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint) :: ist, ied, inum, iele, k1, isurf
      integer(kind = kint) :: i1, i2, i3, i4
      real(kind = kreal) :: x31(3), x42(3)
!
!
        ist = istack_pvr_surf_domain(i_pvr-1) + 1
        ied = istack_pvr_surf_domain(i_pvr  )
!$omp do private (inum,iele,k1,isurf,i1,i2,i3,i4,x31,x42)
        do inum = ist, ied
          iele = item_pvr_surf_domain(1,inum)
          k1 =   item_pvr_surf_domain(2,inum)
          isurf = abs(isf_4_ele(iele,k1))
!
          i1 = ie_surf(isurf,1)
          i2 = ie_surf(isurf,2)
          i3 = ie_surf(isurf,3)
          i4 = ie_surf(isurf,4)
          x31(1:3) = x_nod_model(i3,1:3) - x_nod_model(i1,1:3)
          x42(1:3) = x_nod_model(i4,1:3) - x_nod_model(i2,1:3)
!
          screen_norm_pvr_domain(1,inum)                                &
     &                  = (x31(2)*x42(3) - x31(3)*x42(2))               &
     &                   * dble(isf_4_ele(iele,k1) /isurf)
          screen_norm_pvr_domain(2,inum)                                &
     &                  = (x31(3)*x42(1) - x31(1)*x42(3))               &
     &                   * dble(isf_4_ele(iele,k1) /isurf)
          screen_norm_pvr_domain(3,inum)                                &
     &                  = (x31(1)*x42(2) - x31(2)*x42(1))               &
     &                   * dble(isf_4_ele(iele,k1) /isurf)
        end do
!$omp end do
!
      end subroutine norm_on_model_pvr_domains
!
! -----------------------------------------------------------------------
!
      subroutine range_on_screen_pvr_domains(i_pvr, numele, numsurf,    &
     &          nnod_4_surf, ie_surf, isf_4_ele)
!
      use m_geometries_in_pvr_screen
!
      integer(kind = kint), intent(in) :: i_pvr
!
      integer(kind = kint), intent(in) :: numele, numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint) :: ist, ied, inum, iele, k1, isurf
      integer(kind = kint) :: i1, i2, i3, i4
      real(kind = kreal) :: x1(3), x2(3), x3(3), x4(3), w(4)
!
!
        ist = istack_pvr_surf_domain(i_pvr-1) + 1
        ied = istack_pvr_surf_domain(i_pvr  )
!$omp do private (inum,iele,k1,isurf,i1,i2,i3,i4,x1,x2,x3,x4,w)
        do inum = ist, ied
          iele = item_pvr_surf_domain(1,inum)
          k1 =   item_pvr_surf_domain(2,inum)
          isurf = abs(isf_4_ele(iele,k1))
!
          i1 = ie_surf(isurf,1)
          i2 = ie_surf(isurf,2)
          i3 = ie_surf(isurf,3)
          i4 = ie_surf(isurf,4)
          x1(1:3) = x_nod_screen(i1,1:3)
          x2(1:3) = x_nod_screen(i2,1:3)
          x3(1:3) = x_nod_screen(i3,1:3)
          x4(1:3) = x_nod_screen(i4,1:3)
          w(1:4) =  x_nod_screen(ie_surf(isurf,1:4),4)
!
          screen_posi_pvr_domain(1:3,inum)                              &
     &           = (x1(1:3) + x2(1:3) + x3(1:3) + x4(1:3)) / four
          screen_w_pvr_domain(inum) = (w(1)+w(2)+w(3)+w(4)) / four
!
          screen_xrng_pvr_domain(1,inum) = min(x1(1),x2(1),x3(1),x4(1))
          screen_xrng_pvr_domain(2,inum) = max(x1(1),x2(1),x3(1),x4(1))
          screen_yrng_pvr_domain(1,inum) = min(x1(2),x2(2),x3(2),x4(2))
          screen_yrng_pvr_domain(2,inum) = max(x1(2),x2(2),x3(2),x4(2))
          screen_zrng_pvr_domain(1,inum) = min(x1(3),x2(3),x3(3),x4(3))
          screen_zrng_pvr_domain(2,inum) = max(x1(3),x2(3),x3(3),x4(3))
        end do
!$omp end do
!
      end subroutine range_on_screen_pvr_domains
!
! -----------------------------------------------------------------------
!
      subroutine range_on_pixel_pvr_domains(i_pvr)
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: ist, ied, inum
!
!
        ist = istack_pvr_surf_domain(i_pvr-1) + 1
        ied = istack_pvr_surf_domain(i_pvr  )
!$omp do private (inum)
        do inum = ist, ied
          isurf_xrng_pvr_domain(1,inum)                                 &
     &          = nint( (screen_xrng_pvr_domain(1,inum) + one)          &
     &           * half * dble(n_pvr_pixel(1,i_pvr)) )
          isurf_xrng_pvr_domain(2,inum)                                 &
     &          = nint( (screen_xrng_pvr_domain(2,inum) + one)          &
     &           * half * dble(n_pvr_pixel(1,i_pvr)) )
          jsurf_yrng_pvr_domain(1,inum)                                 &
     &          = nint( (screen_yrng_pvr_domain(1,inum) + one)          &
     &           * half * dble(n_pvr_pixel(2,i_pvr)) )
          jsurf_yrng_pvr_domain(2,inum)                                 &
     &          = nint( (screen_yrng_pvr_domain(2,inum) + one)          &
     &           * half * dble(n_pvr_pixel(2,i_pvr)) )
!
          isurf_xrng_pvr_domain(1,inum)                                 &
     &       = max(isurf_xrng_pvr_domain(1,inum),ione)
          isurf_xrng_pvr_domain(2,inum)                                 &
     &       = min(isurf_xrng_pvr_domain(2,inum),n_pvr_pixel(1,i_pvr))
          jsurf_yrng_pvr_domain(1,inum)                                 &
     &       = max(jsurf_yrng_pvr_domain(1,inum),ione)
          jsurf_yrng_pvr_domain(2,inum)                                 &
     &       = min(jsurf_yrng_pvr_domain(2,inum),n_pvr_pixel(2,i_pvr))
      end do
!$omp end do
!
      end subroutine range_on_pixel_pvr_domains
!
! -----------------------------------------------------------------------
!
      end module find_pvr_surf_domain
