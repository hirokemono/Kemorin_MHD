!set_pvr_ray_start_point.f90
!      module set_pvr_ray_start_point
!
!        programmed by H.Matsui on Aug., 2011
!
!!      subroutine set_each_pvr_ray_start                               &
!!     &         (numnod, numele, numsurf, nnod_4_surf,                 &
!!     &          xx, ie_surf, isf_4_ele, x_nod_screen,                 &
!!     &          npixel_x, npixel_y, pixel_point_x, pixel_point_y,     &
!!     &          num_pvr_surf, item_pvr_surf_domain,                   &
!!     &          screen_norm_pvr_domain, isurf_xrng_pvr_domain,        &
!!     &          jsurf_yrng_pvr_domain, viewpoint_vec, ray_vec,        &
!!     &          istack_pvr_ray_sf, num_pvr_ray, id_pixel_start,       &
!!     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,    &
!!     &          xx_pvr_start, xx_pvr_ray_start, pvr_ray_dir)
!!      subroutine check_pvr_ray_startpoint                             &
!!     &         (npixel_x, npixel_y, num_pvr_ray, id_pixel_start)
!!      subroutine set_pvr_ray_trace_check(npixel_x, npixel_y,          &
!!     &          num_pvr_ray, id_pixel_start, id_pixel_check)
!
      module set_pvr_ray_start_point
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_geometry_constants
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_each_pvr_ray_start                                 &
     &         (numnod, numele, numsurf, nnod_4_surf,                   &
     &          xx, ie_surf, isf_4_ele, x_nod_screen,                   &
     &          npixel_x, npixel_y, pixel_point_x, pixel_point_y,       &
     &          num_pvr_surf, item_pvr_surf_domain,                     &
     &          screen_norm_pvr_domain, viewpoint_vec, ray_vec,         &
     &          ntot_tmp_pvr_ray, istack_tmp_pvr_ray_st,                &
     &          ipix_start_tmp, iflag_start_tmp, xi_pvr_start_tmp,      &
     &          istack_pvr_ray_sf, num_pvr_ray, id_pixel_start,         &
     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,      &
     &          xx_pvr_start, xx_pvr_ray_start, pvr_ray_dir)
!
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      real(kind = kreal), intent(in)  :: xx(numnod,3)
!
      real(kind = kreal), intent(in) :: x_nod_screen(numnod,4)
!
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
      real(kind = kreal), intent(in) :: pixel_point_x(npixel_x)
      real(kind = kreal), intent(in) :: pixel_point_y(npixel_y)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      integer(kind = kint), intent(in)                                  &
     &                    :: item_pvr_surf_domain(2,num_pvr_surf)
      real(kind = kreal), intent(in)                                    &
     &                    :: screen_norm_pvr_domain(3,num_pvr_surf)
!
      real(kind = kreal), intent(in)  :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec(3)
!
      integer(kind = kint), intent(in) :: ntot_tmp_pvr_ray
      integer(kind = kint), intent(in)                                  &
     &                   :: istack_tmp_pvr_ray_st(0:num_pvr_surf)
      integer(kind = kint), intent(in)                                  &
     &                   :: iflag_start_tmp(ntot_tmp_pvr_ray)
      integer(kind = kint), intent(in)                                  &
     &                   :: ipix_start_tmp(2,ntot_tmp_pvr_ray)
      real(kind = kreal), intent(in)                                    &
     &                   :: xi_pvr_start_tmp(2,ntot_tmp_pvr_ray)
      integer(kind = kint), intent(in)                                  &
     &                    :: istack_pvr_ray_sf(0:num_pvr_surf)
!
      integer(kind = kint), intent(in) ::  num_pvr_ray
      integer(kind = kint), intent(inout)                               &
     &                    :: id_pixel_start(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                    :: icount_pvr_trace(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                    :: isf_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xi_pvr_start(2,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xx_pvr_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                   :: xx_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout) :: pvr_ray_dir(3,num_pvr_ray)
!
      integer(kind = kint) :: inum, icou, jcou, iele, k1, isurf
      integer(kind = kint) :: ist_pix, ied_pix
      integer(kind = kint) :: ipix, jpix
!
!
!      write(*,*) 'set_each_pvr_ray_start loop '
!$omp parallel do private(inum,icou,jcou,iele,k1,isurf,                 &
!$omp&                    ipix,jpix,ist_pix,ied_pix)
      do inum = 1, num_pvr_surf
        if( (screen_norm_pvr_domain(3,inum)*ray_vec(3)) .gt. zero) then
          iele = item_pvr_surf_domain(1,inum)
          k1 =   item_pvr_surf_domain(2,inum)
          isurf = abs(isf_4_ele(iele,k1))
!
          icou = istack_tmp_pvr_ray_st(inum-1)
          jcou = istack_pvr_ray_sf(inum-1)
!
          ist_pix = istack_tmp_pvr_ray_st(inum-1) + 1
          ied_pix = istack_tmp_pvr_ray_st(inum)
          do icou = ist_pix, ied_pix
            ipix = ipix_start_tmp(1,icou)
            jpix = ipix_start_tmp(2,icou)
            if(iflag_start_tmp(icou) .gt. 0) then
              jcou = jcou + 1
              if(jcou .gt. num_pvr_ray) write(*,*) 'aho', my_rank,      &
     &                         jcou, num_pvr_ray, inum, num_pvr_surf
!
              icount_pvr_trace(jcou) = 0
              id_pixel_start(jcou) = ipix + (jpix-1)*npixel_x
              isf_pvr_ray_start(1,jcou) = iele
              isf_pvr_ray_start(2,jcou) = k1
              isf_pvr_ray_start(3,jcou) = ie_surf(isurf,1)
              xi_pvr_start(1:2,jcou) =   xi_pvr_start_tmp(1:2,icou)
              xx_pvr_ray_start(1,jcou) = pixel_point_x(ipix)
              xx_pvr_ray_start(2,jcou) = pixel_point_y(jpix)
!
              call cal_field_on_surf_scalar(numnod, numsurf,            &
     &              nnod_4_surf, ie_surf, isurf, xi_pvr_start(1,jcou),  &
     &              x_nod_screen(1,3), xx_pvr_ray_start(3,jcou) )
              call cal_field_on_surf_vector(numnod, numsurf,            &
     &              nnod_4_surf, ie_surf, isurf, xi_pvr_start(1,jcou),  &
     &              xx(1,1), xx_pvr_start(1,jcou) )
!
              pvr_ray_dir(1,jcou) = viewpoint_vec(1)                    &
     &                                 - xx_pvr_start(1,jcou)
              pvr_ray_dir(2,jcou) = viewpoint_vec(2)                    &
     &                                 - xx_pvr_start(2,jcou)
              pvr_ray_dir(3,jcou) = viewpoint_vec(3)                    &
     &                                 - xx_pvr_start(3,jcou)
            end if
          end do
        end if
!
      end do
!$omp end parallel do
!       write(*,*) 'set_each_pvr_ray_start end '
!
      end subroutine set_each_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine check_pvr_ray_startpoint                               &
     &         (npixel_x, npixel_y, num_pvr_ray, id_pixel_start)
!
      use write_bmp_image
!
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
      integer(kind = kint), intent(in) ::  num_pvr_ray
      integer(kind = kint), intent(in)                                  &
     &                    :: id_pixel_start(num_pvr_ray)
!
      integer(kind = kint) :: inum, icou
!
      character(len=kchara), parameter :: img_head = 'startpoints'
      integer(kind = kint), allocatable :: iflag_pix_g(:)
      integer(kind = kint), allocatable :: iflag_pix_l(:)
      character(len = 1), allocatable :: rgb_chk(:,:)
!
!
      allocate(iflag_pix_l(npixel_x*npixel_y))
      allocate(iflag_pix_g(npixel_x*npixel_y))
      allocate(rgb_chk(3,npixel_x*npixel_y))
!
      iflag_pix_l = 0
      iflag_pix_g = 0
      rgb_chk = char(0)
      do icou = 1, num_pvr_ray
        inum = id_pixel_start(icou)
        iflag_pix_l(inum) = 1
      end do
      call MPI_allREDUCE (iflag_pix_l, iflag_pix_g,                     &
     &    npixel_x*npixel_y, CALYPSO_INTEGER,                           &
     &    MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .eq. 0) then
        do icou = 1, npixel_x*npixel_y
          rgb_chk(1,icou) = char(iflag_pix_g(icou)*255)
        end do
!
        call pixout_BMP                                                 &
     &     (img_head, npixel_x, npixel_y, rgb_chk(1,1))
!
        do icou = 1, npixel_x*npixel_y
          if(iflag_pix_g(icou) .eq. 0) write(*,*) 'missing pixel: ',    &
     &       icou, mod(icou-1,npixel_x)+1, (icou-1)/npixel_x+1
        end do
      end if
!
      deallocate(rgb_chk, iflag_pix_g, iflag_pix_l)
!
      end subroutine check_pvr_ray_startpoint
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_ray_trace_check(npixel_x, npixel_y,            &
     &          num_pvr_ray, id_pixel_start, id_pixel_check)
!
      use write_bmp_image
!
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
      integer(kind = kint), intent(in) ::  num_pvr_ray
      integer(kind = kint), intent(in)                                  &
     &                    :: id_pixel_start(num_pvr_ray)
!
      integer(kind = kint), intent(inout)                               &
     &                    :: id_pixel_check(num_pvr_ray)
!
      integer(kind = kint) :: inum, icou
      integer(kind = kint) :: ipix, jpix
!
!
      do icou = 1, num_pvr_ray
        inum = id_pixel_start(icou)
        ipix = mod(inum-1,npixel_x)+1
        jpix = (inum-1)/npixel_x+1
!
        if(ipix.eq.639 .and. jpix.eq.396) id_pixel_check(icou) = 1
        if(ipix.eq.638 .and. jpix.eq.396) id_pixel_check(icou) = 1
        if(ipix.eq.637 .and. jpix.eq.396) id_pixel_check(icou) = 1
        if(ipix.eq.636 .and. jpix.eq.396) id_pixel_check(icou) = 1
        if(ipix.eq.635 .and. jpix.eq.396) id_pixel_check(icou) = 1
        if(ipix.eq.634 .and. jpix.eq.396) id_pixel_check(icou) = 1
!
        if(id_pixel_check(icou) .gt. 0) then
          write(*,*) 'pixel check for ', my_rank, icou, ipix, jpix
        end if
      end do
!
      end subroutine set_pvr_ray_trace_check
!
!  ---------------------------------------------------------------------
!
      end module set_pvr_ray_start_point
