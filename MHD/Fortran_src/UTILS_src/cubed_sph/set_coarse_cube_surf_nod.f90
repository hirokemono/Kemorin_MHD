!set_coarse_cube_surf_nod.f90
!      module set_coarse_cube_surf_nod
!
!     Written by H. Matsui on Apr., 2003
!     Modified by H. Matsui on Oct., 2007
!
!      subroutine set_bottom_course_cube(num_h, nskip_s, nskip_fs,      &
!     &          nnod_sf, jnod, inod_2_org, inod_2_next)
!      subroutine set_side_course_cube(num_h, num_v, nskip_s, nskip_fs, &
!     &          nnod_sf, jnod, inod_2_org, inod_2_next)
!      subroutine set_top_course_cube(num_h, num_v, nskip_s, nskip_fs,  &
!     &          nnod_sf, jnod, inod_2_org, inod_2_next)
!
!      subroutine set_bottom_merged_cube(num_h, nskip_s, nskip_fs,      &
!     &          nnod_sf, nmax_merge, iele, imerge_e_sf)
!      subroutine set_side_merged_cube(num_h, num_v, nskip_s, nskip_fs, &
!     &          nnod_sf, nmax_merge, iele, imerge_e_sf)
!      subroutine set_top_course_cube(num_h, num_v, nskip_s, nskip_fs,  &
!     &          nnod_sf, jnod, inod_2_org, inod_2_next)
!
      module set_coarse_cube_surf_nod
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
      subroutine set_bottom_course_cube(num_h, nskip_s, nskip_fs,       &
     &          nnod_sf, jnod, inod_2_org, inod_2_next)
!
      integer (kind = kint), intent(in) :: num_h
      integer (kind = kint), intent(in) :: nskip_s, nskip_fs
      integer (kind = kint), intent(in) :: nnod_sf
!
      integer (kind = kint), intent(inout) :: jnod
      integer (kind = kint), intent(inout) :: inod_2_org(nnod_sf)
      integer (kind = kint), intent(inout) :: inod_2_next(nnod_sf)
!
      integer (kind = kint) :: ix, iy, jx, jy, num_hc
!
!
      num_hc = num_h / nskip_fs
!
      do iy = 1, num_h+1, nskip_s
        jy = (iy-1)/nskip_fs + 1
        do ix = 1, num_h+1, nskip_s
          jx = (ix-1)/nskip_fs + 1
          jnod = jnod + 1
          inod_2_org(jnod) =  (num_h+1)*(iy-1) +  ix
          inod_2_next(jnod) = (num_hc+1)*(jy-1) + jx
        end do
      end do
!
      end subroutine set_bottom_course_cube
!
!   --------------------------------------------------------------------
!
      subroutine set_side_course_cube(num_h, num_v, nskip_s, nskip_fs,  &
     &          nnod_sf, jnod, inod_2_org, inod_2_next)
!
      integer (kind = kint), intent(in) :: num_h, num_v
      integer (kind = kint), intent(in) :: nskip_s, nskip_fs
      integer (kind = kint), intent(in) :: nnod_sf
!
      integer (kind = kint), intent(inout) :: jnod
      integer (kind = kint), intent(inout) :: inod_2_org(nnod_sf)
      integer (kind = kint), intent(inout) :: inod_2_next(nnod_sf)
!
      integer (kind = kint) :: iz, ixy, jz, jxy, num_hc
      integer (kind = kint) :: inod_ref, knod_ref
!
!
      num_hc = num_h / nskip_fs
!
      inod_ref = (num_h+1)**2
      knod_ref = (num_hc+1)**2
      do iz = nskip_s, num_v-nskip_s, nskip_s
        jz = iz/nskip_fs
        do ixy = 1, 4*num_h+1-nskip_s, nskip_s
          jxy = (ixy-1)/nskip_fs + 1
          jnod = jnod + 1
          inod_2_org(jnod) =  inod_ref + 4*num_h*(iz-1) +  ixy
          inod_2_next(jnod) = knod_ref + 4*num_hc*(jz-1) + jxy
        end do
      end do
!
      end subroutine set_side_course_cube
!
!   --------------------------------------------------------------------
!
      subroutine set_top_course_cube(num_h, num_v, nskip_s, nskip_fs,   &
     &          nnod_sf, jnod, inod_2_org, inod_2_next)
!
      integer (kind = kint), intent(in) :: num_h, num_v
      integer (kind = kint), intent(in) :: nskip_s, nskip_fs
      integer (kind = kint), intent(in) :: nnod_sf
!
      integer (kind = kint), intent(inout) :: jnod
      integer (kind = kint), intent(inout) :: inod_2_org(nnod_sf)
      integer (kind = kint), intent(inout) :: inod_2_next(nnod_sf)
!
      integer (kind = kint) :: ix, iy, jx, jy, num_hc, num_vc
      integer (kind = kint) :: inod_ref, knod_ref
!
!
      num_hc = num_h / nskip_fs
      num_vc = num_v / nskip_fs
!
      inod_ref = (num_h+1)**2  + 4*num_h*(num_v-1)
      knod_ref = (num_hc+1)**2 + 4*num_hc*(num_vc-1)
      do iy = 1, num_h+1, nskip_s
        jy = (iy-1)/nskip_fs + 1
        do ix = 1, num_h+1, nskip_s
          jx = (ix-1)/nskip_fs + 1
          jnod = jnod + 1
          inod_2_org(jnod) =  inod_ref + (num_h+1)* (iy-1) + ix
          inod_2_next(jnod) = knod_ref + (num_hc+1)*(jy-1) + jx
        end do
      end do
!
      end subroutine set_top_course_cube
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_bottom_merged_cube(num_h, nskip_s, nskip_fs,       &
     &          nnod_sf, nmax_merge, iele, imerge_e_sf)
!
      integer (kind = kint), intent(in) :: num_h
      integer (kind = kint), intent(in) :: nskip_s, nskip_fs
      integer (kind = kint), intent(in) :: nnod_sf, nmax_merge
!
      integer (kind = kint), intent(inout) :: iele
      integer (kind = kint), intent(inout)                              &
     &                      :: imerge_e_sf(nnod_sf,nmax_merge)
!
      integer (kind = kint) :: ix, iy, jx, jy, kx, ky, jnum, num_hc
!
!
      num_hc = num_h / nskip_fs
!
      do iy = 1, num_h-nskip_s+1, nskip_s
        do ix = 1, num_h-nskip_s+1, nskip_s
          iele = iele + 1
!
          jnum = 0
          do jy = iy, iy+nskip_s-nskip_fs, nskip_fs
            ky = (jy-1)/nskip_fs + 1
            do jx = ix, ix+nskip_s-nskip_fs, nskip_fs
              kx = (jx-1)/nskip_fs + 1
              jnum = jnum + 1
              imerge_e_sf(iele,jnum) = (ky-1)*num_hc + kx
            end do
          end do
!
        end do
      end do
!
      end subroutine set_bottom_merged_cube
!
!   --------------------------------------------------------------------
!
      subroutine set_side_merged_cube(num_h, num_v, nskip_s, nskip_fs,  &
     &          nnod_sf, nmax_merge, iele, imerge_e_sf)
!
      integer (kind = kint), intent(in) :: num_h, num_v
      integer (kind = kint), intent(in) :: nskip_s, nskip_fs
      integer (kind = kint), intent(in) :: nnod_sf, nmax_merge
!
      integer (kind = kint), intent(inout) :: iele
      integer (kind = kint), intent(inout)                              &
     &                      :: imerge_e_sf(nnod_sf,nmax_merge)
!
      integer (kind = kint) :: ixy, iz, jxy, jz, kxy, kz, jnum, num_hc
!
!
      num_hc = num_h / nskip_fs
!
      do iz = 0, num_v-nskip_s, nskip_s
        do ixy = 1, 4*num_h+1 - nskip_s, nskip_s
          iele = iele + 1
!
          jnum = 0
          do jz = iz, iz+nskip_s-nskip_fs, nskip_fs
            kz = jz / nskip_fs
            do jxy = ixy, ixy+nskip_s-nskip_fs, nskip_fs
              kxy = (jxy-1)/nskip_fs + 1
              jnum = jnum + 1
              imerge_e_sf(iele,jnum) = num_hc**2 + 4*num_hc*kz + kxy
            end do
          end do
        end do
      end do
!
      end subroutine set_side_merged_cube
!
!   --------------------------------------------------------------------
!
      subroutine set_top_merged_cube(num_h, num_v, nskip_s, nskip_fs,   &
     &          nnod_sf, nmax_merge, iele, imerge_e_sf)
!
      integer (kind = kint), intent(in) :: num_h, num_v
      integer (kind = kint), intent(in) :: nskip_s, nskip_fs
      integer (kind = kint), intent(in) :: nnod_sf, nmax_merge
!
      integer (kind = kint), intent(inout) :: iele
      integer (kind = kint), intent(inout)                              &
     &                      :: imerge_e_sf(nnod_sf,nmax_merge)
!
      integer (kind = kint) :: ix, iy, jx, jy, kx, ky
      integer (kind = kint) :: jnum, num_hc, num_vc, jele_st
!
!
      num_hc = num_h / nskip_fs
      num_vc = num_v / nskip_fs
!
      jele_st = num_hc**2 + 4*num_hc*num_vc
      do iy = 1, num_h-nskip_s+1, nskip_s
        do ix = 1, num_h-nskip_s+1, nskip_s
          iele = iele + 1
!
          jnum = 0
          do jy = iy, iy+nskip_s-nskip_fs, nskip_fs
            ky = (jy-1)/nskip_fs + 1
            do jx = ix, ix+nskip_s-nskip_fs, nskip_fs
              kx = (jx-1)/nskip_fs + 1
              jnum = jnum + 1
              imerge_e_sf(iele,jnum) = jele_st + (ky-1)*num_hc + kx
            end do
          end do
        end do
      end do
!
      end subroutine set_top_merged_cube
!
!   --------------------------------------------------------------------
!
      end module set_coarse_cube_surf_nod
