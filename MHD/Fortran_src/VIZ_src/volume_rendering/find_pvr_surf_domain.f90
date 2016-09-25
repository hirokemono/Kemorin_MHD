!find_pvr_surf_domain.f90
!      module find_pvr_surf_domain
!
!        programmed by H.Matsui on Aug., 2011
!
!!      subroutine s_find_pvr_surf_domain(num_pvr, numele, numsurf,     &
!!     &          interior_ele, isf_4_ele, iele_4_surf,                 &
!!     &          ele_grp, fld_params, pvr_bound, field_pvr)
!!      subroutine set_pvr_domain_surface_data                          &
!!     &       (n_pvr_pixel, numnod, numele, numsurf, nnod_4_surf,      &
!!     &        ie_surf, isf_4_ele, x_nod_screen, pvr_bound)
!!      subroutine norm_on_model_pvr_domains(numnod, numele, numsurf,   &
!!     &         nnod_4_surf, ie_surf, isf_4_ele, x_nod_model,          &
!!     &         num_pvr_surf, item_pvr_surf_domain,                    &
!!     &         screen_norm_pvr_domain)
!!      subroutine deallocate_pvr_surf_domain(num_pvr, pvr_bound)
!
      module find_pvr_surf_domain
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
!
      implicit  none
!
      private :: range_on_screen_pvr_domains
      private :: range_on_pixel_pvr_domains
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_find_pvr_surf_domain(num_pvr, numele, numsurf,       &
     &          interior_ele, isf_4_ele, iele_4_surf,                   &
     &          ele_grp, fld_params, pvr_bound, field_pvr)
!
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_geometries_in_pvr_screen
      use t_group_data
      use find_selected_domain_bd
      use set_iflag_for_used_ele
      use pvr_surface_enhancement
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: interior_ele(numele)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_field_parameter), intent(in) :: fld_params(num_pvr)
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound(num_pvr)
      type(pvr_projected_field), intent(inout) :: field_pvr(num_pvr)
!
      integer(kind = kint) :: i_pvr
!
!
      call allocate_imark_4_surface(numsurf)
!
      do i_pvr = 1, num_pvr
        call s_set_iflag_for_used_ele(numele, interior_ele,             &
     &      ele_grp%num_grp, ele_grp%num_item,                          &
     &      ele_grp%istack_grp, ele_grp%item_grp,                       &
     &      fld_params(i_pvr)%nele_grp_area_pvr,                        &
     &      fld_params(i_pvr)%id_ele_grp_area_pvr,                      &
     &      field_pvr(i_pvr)%iflag_used_ele)
!
        call mark_selected_domain_bd(numele, numsurf, isf_4_ele,        &
     &      field_pvr(i_pvr)%iflag_used_ele)
        call count_selected_domain_bd(numsurf,                          &
     &      pvr_bound(i_pvr)%num_pvr_surf)
!
        call alloc_pvr_surf_domain_item(pvr_bound(i_pvr))
!
        call mark_selected_domain_bd(numele, numsurf, isf_4_ele,        &
     &      field_pvr(i_pvr)%iflag_used_ele)
        call s_find_selected_domain_bd(numele, numsurf, iele_4_surf,    &
     &      field_pvr(i_pvr)%iflag_used_ele,                            &
     &      pvr_bound(i_pvr)%num_pvr_surf,                              &
     &      pvr_bound(i_pvr)%item_pvr_surf)
      end do
!
      call deallocate_imark_4_surface
!
      end subroutine s_find_pvr_surf_domain
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_domain_surface_data                            &
     &       (n_pvr_pixel, numnod, numele, numsurf, nnod_4_surf,        &
     &        ie_surf, isf_4_ele, x_nod_screen, pvr_bound)
!
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use ordering_pvr_sf_domain_grp
!
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numele, numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      real(kind = kreal), intent(in) :: x_nod_screen(numnod,4)
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!
!
!$omp parallel
      call range_on_screen_pvr_domains(numnod, numele, numsurf,         &
     &    nnod_4_surf, ie_surf, isf_4_ele, x_nod_screen,                &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_posi, pvr_bound%screen_w,                    &
     &    pvr_bound%screen_xrng, pvr_bound%screen_yrng,                 &
     &    pvr_bound%screen_zrng)
      call range_on_pixel_pvr_domains                                   &
     &   (n_pvr_pixel, pvr_bound%num_pvr_surf,                          &
     &    pvr_bound%screen_xrng, pvr_bound%screen_yrng,                 &
     &    pvr_bound%isurf_xrng,  pvr_bound%jsurf_yrng)
!$omp end parallel
!
      call s_ordering_pvr_sf_domain_grp(pvr_bound)
!
      end subroutine set_pvr_domain_surface_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pvr_surf_domain(num_pvr, pvr_bound)
!
      use t_surf_grp_4_pvr_domain
!
      integer(kind=kint), intent(in) :: num_pvr
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound(num_pvr)
!
      integer(kind = kint) :: i_pvr
!
!
      do i_pvr = 1, num_pvr
        call dealloc_pvr_surf_domain_item(pvr_bound(i_pvr))
      end do
!
      end subroutine deallocate_pvr_surf_domain
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine norm_on_model_pvr_domains(numnod, numele, numsurf,     &
     &         nnod_4_surf, ie_surf, isf_4_ele, x_nod_model,            &
     &         num_pvr_surf, item_pvr_surf_domain,                      &
     &         screen_norm_pvr_domain)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numele, numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      real(kind = kreal), intent(in) :: x_nod_model(numnod,4)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      integer(kind = kint), intent(in)                                  &
     &                    :: item_pvr_surf_domain(2,num_pvr_surf)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_norm_pvr_domain(3,num_pvr_surf)
!
      integer(kind = kint) :: inum, iele, k1, isurf
      integer(kind = kint) :: i1, i2, i3, i4
      real(kind = kreal) :: x31(3), x42(3)
!
!
!$omp parallel do private (inum,iele,k1,isurf,i1,i2,i3,i4,x31,x42)
        do inum = 1, num_pvr_surf
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
!$omp end parallel do
!
      end subroutine norm_on_model_pvr_domains
!
! -----------------------------------------------------------------------
!
      subroutine range_on_screen_pvr_domains(numnod, numele, numsurf,   &
     &      nnod_4_surf, ie_surf, isf_4_ele, x_nod_screen,              &
     &      num_pvr_surf, item_pvr_surf_domain,                         &
     &      screen_posi_pvr_domain, screen_w_pvr_domain,                &
     &      screen_xrng_pvr_domain, screen_yrng_pvr_domain,             &
     &      screen_zrng_pvr_domain)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numele, numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      real(kind = kreal), intent(in) :: x_nod_screen(numnod,4)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      integer(kind = kint), intent(in)                                  &
     &                    :: item_pvr_surf_domain(2,num_pvr_surf)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_posi_pvr_domain(3,num_pvr_surf)
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_w_pvr_domain(num_pvr_surf)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_xrng_pvr_domain(2,num_pvr_surf)
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_yrng_pvr_domain(2,num_pvr_surf)
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_zrng_pvr_domain(2,num_pvr_surf)
!
      integer(kind = kint) :: inum, iele, k1, isurf
      integer(kind = kint) :: i1, i2, i3, i4
      real(kind = kreal) :: x1(3), x2(3), x3(3), x4(3), w(4)
!
!
!$omp do private (inum,iele,k1,isurf,i1,i2,i3,i4,x1,x2,x3,x4,w)
        do inum = 1, num_pvr_surf
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
      subroutine range_on_pixel_pvr_domains(n_pvr_pixel, num_pvr_surf,  &
     &          screen_xrng_pvr_domain, screen_yrng_pvr_domain,         &
     &          isurf_xrng_pvr_domain, jsurf_yrng_pvr_domain)
!
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      real(kind = kreal), intent(in)                                    &
     &                    :: screen_xrng_pvr_domain(2,num_pvr_surf)
      real(kind = kreal), intent(in)                                    &
     &                    :: screen_yrng_pvr_domain(2,num_pvr_surf)
!
      integer(kind = kint), intent(inout)                               &
     &                    :: isurf_xrng_pvr_domain(2,num_pvr_surf)
      integer(kind = kint), intent(inout)                               &
     &                    :: jsurf_yrng_pvr_domain(2,num_pvr_surf)
!
      integer(kind = kint) :: inum
!
!
!$omp do private (inum)
        do inum = 1, num_pvr_surf
          isurf_xrng_pvr_domain(1,inum)                                 &
     &          = nint( (screen_xrng_pvr_domain(1,inum) + one)          &
     &           * half * dble(n_pvr_pixel(1)) )
          isurf_xrng_pvr_domain(2,inum)                                 &
     &          = nint( (screen_xrng_pvr_domain(2,inum) + one)          &
     &           * half * dble(n_pvr_pixel(1)) )
          jsurf_yrng_pvr_domain(1,inum)                                 &
     &          = nint( (screen_yrng_pvr_domain(1,inum) + one)          &
     &           * half * dble(n_pvr_pixel(2)) )
          jsurf_yrng_pvr_domain(2,inum)                                 &
     &          = nint( (screen_yrng_pvr_domain(2,inum) + one)          &
     &           * half * dble(n_pvr_pixel(2)) )
!
          isurf_xrng_pvr_domain(1,inum)                                 &
     &       = max(isurf_xrng_pvr_domain(1,inum),ione)
          isurf_xrng_pvr_domain(2,inum)                                 &
     &       = min(isurf_xrng_pvr_domain(2,inum),n_pvr_pixel(1))
          jsurf_yrng_pvr_domain(1,inum)                                 &
     &       = max(jsurf_yrng_pvr_domain(1,inum),ione)
          jsurf_yrng_pvr_domain(2,inum)                                 &
     &       = min(jsurf_yrng_pvr_domain(2,inum),n_pvr_pixel(2))
      end do
!$omp end do
!
      end subroutine range_on_pixel_pvr_domains
!
! -----------------------------------------------------------------------
!
      end module find_pvr_surf_domain
